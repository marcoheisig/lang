(in-package #:ouroboros)

(defparameter *python-object-table*
  (make-hash-table :weakness :value)
  "A hash table mapping from integers that are PyObject addresses to the
corresponding Lisp objects.

Each Lisp object should have a finalizer that decreases the reference count of
its corresponding PyObject.

Use weak references for the values, because we can recreate the Python object
at any time if necessary.")

(defclass python-object ()
  ((%pyobject
    :initarg :pyobject
    :initform (alexandria:required-argument :pyobject)
    :type pyobject
    :reader python-object-pyobject))
  (:documentation
   "An object of the Python programming language."))

(defmethod print-object ((python-object python-object) stream)
  (format stream "#<~A ~A>"
          (class-name (class-of python-object))
          (let ((pyrepr (pyobject-repr (python-object-pyobject python-object))))
            (unwind-protect (string-from-pyobject pyrepr)
              (pyobject-decref pyrepr)))))

(defmethod shared-initialize :after
    ((python-object python-object)
     (slot-names t)
     &key pyobject &allow-other-keys)
  ;; Register the object in a hash table.
  (alexandria:ensure-gethash
   (pyobject-address pyobject)
   *python-object-table*
   (register-python-object-finalizer pyobject python-object)))

(defun register-python-object-finalizer (pyobject python-object)
  (pyobject-incref pyobject)
  (trivial-garbage:finalize
   python-object
   (lambda ()
     ;; TODO ensure we are in the main thread.
     (pyobject-decref pyobject))))

(defclass python-class (python-object standard-class)
  ())

(defmethod validate-superclass
    ((class python-class)
     (superclass standard-class))
  t)

(defclass python:|type| (python-class)
  ()
  (:metaclass python-class)
  (:pyobject . #.*type-pyobject*))

(defmethod validate-superclass
    ((class python:|type|)
     (superclass standard-class))
  t)

(defclass python:|object| (python-object)
  ()
  (:metaclass python:|type|)
  (:pyobject . #.*object-pyobject*))

(defclass python:|None| (python:|object|)
  ()
  (:metaclass python:|type|)
  (:pyobject . #.*none-pyobject*))

;; Treat null pointers as None.
(setf (gethash 0 *python-object-table*)
      (find-class 'python:|None|))

(defparameter python:|type| (find-class 'python:|type|))

(defparameter python:|object| (find-class 'python:|object|))

(defparameter python:|None| (find-class 'python:|None|))

(defun mirror-into-lisp (pyobject)
  "Return the Lisp object corresponding to the supplied PyObject pointer."
  (declare (pyobject pyobject))
  (or (gethash (pyobject-address pyobject) *python-object-table*)
      (let* ((pytype (pyobject-pytype pyobject))
             (class (mirror-into-lisp pytype)))
        (if (pytype-subtypep pytype *type-pyobject*)
            ;; Create a type.
            (let* ((name (pytype-name-symbol pyobject))
                   (direct-superclasses (pytype-direct-superclasses pyobject)))
              (make-instance class
                :name name
                :direct-superclasses direct-superclasses
                :pyobject pyobject))
            ;; Create an instance.
            (make-instance class
              :pyobject pyobject)))))

(defun pytype-name-symbol (pytype)
  (let ((pyname (pytype-qualified-name pytype)))
    (if (cffi:null-pointer-p pyname)
        (intern
         (format nil "UNNAMED-TYPE-~X" (random most-positive-fixnum))
         "PYTHON")
        (multiple-value-bind (lisp-name visibility)
            (string-from-pyobject pyname)
          (let* ((pymodule-name (pyobject-getattr-string pytype "__module__"))
                 (module-name
                   (if (cffi:null-pointer-p pymodule-name)
                       nil
                       (string-from-pyobject pymodule-name)))
                 (symbol-name
                   (if (or (not module-name)
                           (string= module-name "builtins"))
                       lisp-name
                       (concatenate 'string module-name "." lisp-name)))
                 (lisp-symbol (intern symbol-name "PYTHON")))
            (when (eq visibility :external)
              (export lisp-symbol "PYTHON"))
            lisp-symbol)))))

(defun pytype-direct-superclasses (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (let ((bases (pyobject-getattr-string pyobject "__bases__")))
    (when (cffi:null-pointer-p bases)
      (error "Couldn't determine the bases of ~S." pyobject))
    (loop for position below (pytuple-size bases)
          collect
          (mirror-into-lisp
           (pytuple-getitem bases position)))))
