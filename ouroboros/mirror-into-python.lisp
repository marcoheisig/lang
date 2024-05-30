(in-package #:ouroboros.internals)

(defparameter *mirror-into-python-table*
  (make-hash-table)
  "A hash table mapping from Lisp objects to their mirror object in the Python
world.

Each mirror object must have a finalizer attached that deletes its entry in
this table so that the garbage collector may eventually clean up the Lisp
object.")

(defmethod mirror-into-python :around
    ((object t))
  (or (gethash object *mirror-into-python-table*)
      (let ((pyobject (call-next-method)))
        (setf (gethash object *mirror-into-python-table*)
              pyobject)
        (setf (gethash (pyobject-address pyobject) *mirror-into-lisp-table*)
              object)
        pyobject)))

(defmethod mirror-into-python ((object t))
  (with-global-interpreter-lock-held
    (pyobject-call-no-args
     (mirror-into-python
      (class-of object)))))

(cffi:defcallback __finalize__ :void
    ((pyobject pyobject))
  (remhash (mirror-into-lisp pyobject) *mirror-into-python-table*))

(cffi:defcallback __repr__ pyobject
    ((pyobject pyobject))
  (mirror-into-python
   (repr
    (mirror-into-lisp pyobject))))

(cffi:defcallback __str__ pyobject
    ((pyobject pyobject))
  (mirror-into-python
   (str
    (mirror-into-lisp pyobject))))

(cffi:defcallback __bool__ :int
    ((pyobject pyobject))
  (if (bool (mirror-into-lisp pyobject))
      1
      0))

(cffi:defcallback __int__ pyobject
    ((pyobject pyobject))
  (pylong-from-long
   (mirror-into-lisp pyobject)))

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

(defmethod mirror-into-python ((class class))
  (let* ((name (class-name class))
         (metaclass (class-of class))
         (supers (class-direct-superclasses class)))
    (declare (ignore metaclass)) ;; TODO
    (make-pytype
     (format nil "ouroboros.~A.~A"
             (string-downcase (package-name (symbol-package name)))
             (string-downcase (symbol-name name)))
     +pyobject-type-size+
     0
     '(:default :basetype :type-subclass)
     :tp-bases (apply #'pytuple (mapcar #'mirror-into-python supers))
     :tp-doc (cffi:null-pointer)
     :tp-repr (cffi:callback __repr__)
     :tp-str (cffi:callback __str__)
     :tp-finalize (cffi:callback __finalize__)
     :nb-bool (cffi:callback __bool__))))

(defmethod mirror-into-python ((class (eql (find-class 'standard-class))))
  (make-pytype "ouroboros.common_lisp.standard_class"
               +pyobject-type-size+
               0
               '(:default :basetype :type-subclass)
               :tp-bases (pytuple *type-pyobject*)
               :tp-doc (cffi:null-pointer)
               :tp-repr (cffi:callback __repr__)
               :tp-str (cffi:callback __str__)
               :tp-finalize (cffi:callback __finalize__)
               :nb-bool (cffi:callback __bool__)))

(defmethod mirror-into-python ((class (eql (find-class 't))))
  (make-pytype "ouroboros.common_lisp.t"
               +pyobject-header-size+
               0
               '(:default :basetype)
               :tp-bases (pytuple *object-pyobject*)
               :tp-doc (cffi:null-pointer)
               :tp-repr (cffi:callback __repr__)
               :tp-str (cffi:callback __str__)
               :tp-finalize (cffi:callback __finalize__)
               :nb-bool (cffi:callback __bool__)
               :nb-int (cffi:callback __int__)))

(defmethod mirror-into-python ((class (eql (find-class 'function))))
  (make-pytype "ouroboros.common_lisp.function"
               (+ +pyobject-header-size+ +pointer-size+)
               0
               '(:default :basetype)
               :tp-bases (pytuple (mirror-into-python (find-class 't)))
               :tp-call (cffi:callback __call__)
               :tp-descr_get (pytype-getslot *pyfunction-pyobject* :tp-descr-get)
               :tp-doc (cffi:null-pointer)
               :tp-repr (cffi:callback __repr__)
               :tp-str (cffi:callback __str__)
               :tp-finalize (cffi:callback __finalize__)
               :nb-bool (cffi:callback __bool__)))
