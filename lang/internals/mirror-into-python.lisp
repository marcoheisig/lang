(in-package #:lang.internals)

(defvar *mirror-into-python-table*
  (make-hash-table :synchronized t)
  "A hash table mapping from Lisp objects to their mirror object in the Python
world.

Each mirror object must have a finalizer attached that deletes its entry in
this table so that the garbage collector may eventually clean up the Lisp
object.")

(defmethod mirror-into-python :around ((python-object python-object))
  (let ((pyobject (python-object-pyobject python-object)))
    (with-global-interpreter-lock-held
      (pyobject-incref pyobject))
    ;; Prevent the mirror object from being cleaned up before the PyObject
    ;; refcount has been incremented.
    (touch python-object)
    pyobject))

(defmethod mirror-into-python :around ((object t))
  (multiple-value-bind (pyobject presentp)
      (gethash object *mirror-into-python-table*)
    (if presentp
        (with-global-interpreter-lock-held
          (pyobject-incref pyobject)
          pyobject)
        (let ((pyobject (call-next-method)))
          (setf (gethash object *mirror-into-python-table*)
                pyobject)
          (setf (gethash (pyobject-address pyobject) *mirror-into-lisp-table*)
                object)
          pyobject))))

(defmethod mirror-into-python ((object t))
  (with-global-interpreter-lock-held
    (pyobject-call-no-args
     (mirror-into-python
      (class-of object)))))

(cffi:defcallback __finalize__ :void
    ((pyobject pyobject))
  (let* ((address (pyobject-address pyobject))
         (lisp-object (gethash address *mirror-into-lisp-table*)))
    #+(or)
    (format *trace-output* "~&(Python) Finalizing ~S.~%"
            lisp-object)
    (remhash address *mirror-into-lisp-table*)
    (remhash lisp-object *mirror-into-python-table*)))

(cffi:defcallback __call__ pyobject
    ((callable pyobject)
     (args pyobject)
     (kwargs pyobject))
  (with-global-interpreter-lock-held
    (let ((fn (mirror-into-lisp callable))
          (nargs (pytuple-size args))
          (nkwargs
            (if (cffi:null-pointer-p kwargs)
                0
                (pydict-size kwargs))))
      (assert (functionp fn))
      (flet ((arg (index)
               (mirror-into-lisp
                (pytuple-getitem args index))))
        (mirror-into-python
         (if (zerop nkwargs)
             (case nargs
               (0 (funcall fn))
               (1 (funcall fn (arg 0)))
               (2 (funcall fn (arg 0) (arg 1)))
               (3 (funcall fn (arg 0) (arg 1) (arg 2)))
               (4 (funcall fn (arg 0) (arg 1) (arg 2) (arg 3)))
               (5 (funcall fn (arg 0) (arg 1) (arg 2) (arg 3) (arg 4)))
               (6 (funcall fn (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5)))
               (7 (funcall fn (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5) (arg 6)))
               (otherwise
                (apply fn (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5) (arg 6)
                       (loop for index from 7 below nargs collect (arg index)))))
             (error "TODO")))))))

(cffi:defcallback __fastcall__ pyobject
    ((self pyobject)
     (args :pointer)
     (nargs :size)
     (kwnames pyobject))
  (assert (cffi:null-pointer-p kwnames))
  (let ((fn (mirror-into-lisp self)))
    (flet ((arg (index)
             (mirror-into-lisp
              (cffi:mem-aref args :pointer index))))
      (mirror-into-python
       (case nargs
         (0 (funcall fn (arg 0)))
         (1 (funcall fn (arg 0) (arg 1)))
         (2 (funcall fn (arg 0) (arg 1) (arg 2)))
         (3 (funcall fn (arg 0) (arg 1) (arg 2)))
         (4 (funcall fn (arg 0) (arg 1) (arg 2) (arg 3)))
         (5 (funcall fn (arg 0) (arg 1) (arg 2) (arg 3) (arg 4)))
         (6 (funcall fn (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5)))
         (7 (funcall fn (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5) (arg 6)))
         (otherwise
          (apply fn (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5) (arg 6)
                 (loop for index from 7 below nargs collect (arg index)))))))))



(defclass lisp-object ()
  ())

(defclass lisp-type ()
  ())

(defmethod mirror-into-python ((class (eql (find-class 'lisp-object))))
  (make-pytype "lang.lisp.lisp_object"
               (+ +pyobject-header-size+ +pointer-size+)
               0
               '(:default :basetype)
               :tp-base *object-pyobject*
               :tp-doc (cffi:null-pointer) ;; TODO
               :tp-finalize (cffi:callback __finalize__)
))

(defmethod mirror-into-python ((class (eql (find-class 'lisp-type))))
  (apply
   #'make-pytype
   "lang.lisp.lisp_type"
   (+ +pyobject-type-size+ +pointer-size+)
   0
   '(:default :basetype :type-subclass)
   :tp-base *type-pyobject*
   :tp-doc (cffi:null-pointer) ;; TODO
   :tp-finalize (cffi:callback __finalize__)
   (pytype-function-attributes class)))

(defmethod mirror-into-python ((class class))
  (let* ((name (class-name class))
         (metaclass (class-of class))
         (classp (subtypep class 'class))
         (supers (substitute
                  (if classp
                      (find-class 'lisp-type)
                      (find-class 'lisp-object))
                  (find-class 't)
                  (class-direct-superclasses class)))
         (type-name
           (format nil "lang.lisp.~A.~A"
                   (string-downcase (package-name (symbol-package name)))
                   (string-downcase (symbol-name name)))))
    (declare (ignore metaclass)) ;; TODO
    (with-pyobjects ((bases (move-into-lisp (pytuple-new (length supers)))))
      (loop for super in supers for index from 0 do
        (pytuple-setitem bases index (mirror-into-python super)))
      (apply
       #'make-pytype
       type-name
       (cond (classp
              (+ +pyobject-type-size+ +pointer-size+))
             ((subtypep class 'function)
              (+ +pyobject-header-size+ +pointer-size+ +pointer-size+))
             (t
              (+ +pyobject-header-size+ +pointer-size+)))
       0
       '(:default :basetype)
       :tp-bases bases
       :tp-doc (cffi:null-pointer)
       (pytype-function-attributes class)))))

(defmethod mirror-into-python ((class (eql (find-class 't))))
  *object-pyobject*)

(defmethod mirror-into-python ((class (eql (find-class 'function))))
  (with-pyobjects ((base (find-class 'lisp-object)))
    (apply
     #'make-pytype
     "lang.lisp.function"
     (+ +pyobject-header-size+ +pointer-size+ +pointer-size+)
     0
     '(:default :basetype)
     :tp-base base
     :tp-descr-get (pytype-getslot *pyfunction-pyobject* :tp-descr-get)
     :tp-doc (cffi:null-pointer)
     :tp-call (cffi:callback __call__)
     (pytype-function-attributes class))))

(defparameter *pytype-function-attributes*
  '((:tp-repr __repr__)
    (:tp-str __str__)
    (:tp-hash __hash__)
    (:tp-richcompare __richcmp__)
    ;; Number Functions
    (:nb-absolute __abs__)
    (:nb-add __add__)
    (:nb-and __and__)
    (:nb-bool __bool__)
    (:nb-divmod __divmod__)
    (:nb-float __float__)
    (:nb-floor-divide __floordiv__)
    (:nb-index __index__)
    (:nb-inplace-add __iadd__)
    (:nb-inplace-and __iand__)
    (:nb-inplace-floor-divide __ifloordiv__)
    (:nb-inplace-lshift __ilshift__)
    (:nb-inplace-multiply __imul__)
    (:nb-inplace-or __ior__)
    (:nb-inplace-power __ipow__)
    (:nb-inplace-remainder __imod__)
    (:nb-inplace-rshift __irshift__)
    (:nb-inplace-subtract __isub__)
    (:nb-inplace-true-divide __itruediv__)
    (:nb-inplace-xor __ixor__)
    (:nb-int __int__)
    (:nb-invert __invert__)
    (:nb-lshift __lshift__)
    (:nb-multiply __mul__)
    (:nb-negative __neg__)
    (:nb-or __or__)
    (:nb-positive __pos__)
    (:nb-power __pow__)
    (:nb-remainder __mod__)
    (:nb-rshift __rshift__)
    (:nb-subtract __sub__)
    (:nb-true-divide __truediv__)
    (:nb-xor __xor__)
    ;; Mapping Functions
    (:mp-length __len__)
    (:mp-subscript __getitem__)
    (:mp-ass-subscript __setitem__)
    ;; Sequence Functions
    (:sq-length __len__)
    (:sq-concat __add__)
    (:sq-repeat __mul__)
    (:sq-ass-item __sq_setitem__)
    (:sq-item __sq_getitem__)
    (:sq-contains __contains__)
    (:sq-inplace-concat __iadd__)
    (:sq-inplace-repeat __imul__)))

(defmethod pytype-function-attributes ((class class))
  (loop for (slot function-name) in *pytype-function-attributes*
        collect slot
        collect (pytype-function-attribute class function-name)))

(defun pytype-function-attribute (class function-name)
  (let ((fn (fdefinition function-name)))
    (etypecase fn
      (generic-function
       (let* ((lambda-list (generic-function-lambda-list fn))
              (nargs (length lambda-list)))
         (assert (null (intersection lambda-list lambda-list-keywords)))
         (if (compute-applicable-methods-using-classes
              fn
              (list* class (make-list (1- nargs) :initial-element (find-class 't))))
             (cffi-sys:%callback function-name)
             (cffi:null-pointer))))
      (function (cffi-sys:%callback function-name)))))
