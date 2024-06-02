(in-package #:ouroboros.internals)

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
  (pyobject-call-no-args
   (mirror-into-python
    (class-of object))))

(cffi:defcallback __finalize__ :void
    ((pyobject pyobject))
  #+(or)
  (let ((lisp-object (gethash pyobject *mirror-into-python-table*)))
    (format *trace-output* "~&(Python) Finalizing ~S.~%"
            lisp-object))
  (remhash pyobject *mirror-into-python-table*))

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
  (make-pytype "ouroboros.common_lisp.lisp_object"
               (+ +pyobject-header-size+ +pointer-size+)
               0
               '(:default :basetype)
               :tp-base *object-pyobject*
               :tp-doc (cffi:null-pointer) ;; TODO
               :tp-finalize (cffi:callback __finalize__)
               :tp-repr (cffi:callback __repr__)
               :tp-str (cffi:callback __str__)
               :tp-hash (cffi:callback __hash__)
               :tp-richcompare (cffi:callback __richcmp__)
               ;; Number Functions
               :nb-absolute (cffi:callback __abs__)
               :nb-add (cffi:callback __add__)
               :nb-and (cffi:callback __and__)
               :nb-bool (cffi:callback __bool__)
               :nb-divmod (cffi:callback __divmod__)
               :nb-float (cffi:callback __float__)
               :nb-floor-divide (cffi:callback __floordiv__)
               :nb-index (cffi:callback __index__)
               :nb-inplace-add (cffi:callback __iadd__)
               :nb-inplace-and (cffi:callback __iand__)
               :nb-inplace-floor-divide (cffi:callback __ifloordiv__)
               :nb-inplace-lshift (cffi:callback __ilshift__)
               :nb-inplace-multiply (cffi:callback __imul__)
               :nb-inplace-or (cffi:callback __ior__)
               :nb-inplace-power (cffi:callback __ipow__)
               :nb-inplace-remainder (cffi:callback __imod__)
               :nb-inplace-rshift (cffi:callback __irshift__)
               :nb-inplace-subtract (cffi:callback __isub__)
               :nb-inplace-true-divide (cffi:callback __itruediv__)
               :nb-inplace-xor (cffi:callback __ixor__)
               :nb-int (cffi:callback __int__)
               :nb-invert (cffi:callback __invert__)
               :nb-lshift (cffi:callback __lshift__)
               :nb-multiply (cffi:callback __mul__)
               :nb-negative (cffi:callback __neg__)
               :nb-or (cffi:callback __or__)
               :nb-positive (cffi:callback __pos__)
               :nb-power (cffi:callback __pow__)
               :nb-remainder (cffi:callback __mod__)
               :nb-rshift (cffi:callback __rshift__)
               :nb-subtract (cffi:callback __sub__)
               :nb-true-divide (cffi:callback __truediv__)
               :nb-xor (cffi:callback __xor__)
               ;; Mapping Functions
               :mp-length (cffi:callback __len__)
               :mp-subscript (cffi:callback __getitem__)
               :mp-ass-subscript (cffi:callback __setitem__)
               ;; Sequence Functions
               :sq-length (cffi:callback __len__)
               :sq-concat (cffi:callback __add__)
               :sq-repeat (cffi:callback __mul__)
               :sq-ass-item (cffi:callback __sq_setitem__)
               :sq-item (cffi:callback __sq_getitem__)
               :sq-contains (cffi:callback __contains__)
               :sq-inplace-concat (cffi:callback __iadd__)
               :sq-inplace-repeat (cffi:callback __imul__)))

(defmethod mirror-into-python ((class (eql (find-class 'lisp-type))))
  (make-pytype "ouroboros.common_lisp.lisp_type"
               (+ +pyobject-type-size+ +pointer-size+)
               0
               '(:default :basetype :type-subclass)
               :tp-base *type-pyobject*
               :tp-doc (cffi:null-pointer) ;; TODO
               :tp-finalize (cffi:callback __finalize__)
               :tp-repr (cffi:callback __repr__)
               :tp-str (cffi:callback __str__)
               :tp-hash (cffi:callback __hash__)
               :tp-richcompare (cffi:callback __richcmp__)))

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
           (format nil "ouroboros.~A.~A"
                   (string-downcase (package-name (symbol-package name)))
                   (string-downcase (symbol-name name)))))
    (declare (ignore metaclass)) ;; TODO
    (with-pyobjects ((bases
                      (move-into-lisp
                       (apply #'pytuple (mapcar #'mirror-into-python supers)))))
      (make-pytype
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
       :tp-doc (cffi:null-pointer)))))

(defmethod mirror-into-python ((class (eql (find-class 't))))
  *object-pyobject*)

(defmethod mirror-into-python ((class (eql (find-class 'function))))
  (with-pyobjects ((base (find-class 'lisp-object)))
    (make-pytype
     "ouroboros.common_lisp.function"
     (+ +pyobject-header-size+ +pointer-size+ +pointer-size+)
     0
     '(:default :basetype)
     :tp-base base
     :tp-call (cffi:callback __call__)
     :tp-descr-get (pytype-getslot *pyfunction-pyobject* :tp-descr-get)
     :tp-doc (cffi:null-pointer))))
