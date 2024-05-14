(in-package #:sbclmodule)

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
                :pointer pyobject))
            ;; Create an instance.
            (make-instance class
              :pointer pyobject)))))

(defun pytype-direct-superclasses (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (let ((bases (pyobject-getattr-string pyobject "__bases__")))
    (if (cffi:null-pointer-p bases)
        (list (find-class 'python:object))
         (loop for position below (pytuple-size bases)
               collect
               (mirror-into-lisp
                (pytuple-getitem bases position))))))

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
                           (string= module-name '#:builtins))
                       lisp-name
                       (concatenate 'string module-name "." lisp-name)))
                 (lisp-symbol (intern symbol-name "PYTHON")))
            (when (eq visibility :external)
              (export lisp-symbol "PYTHON"))
            lisp-symbol)))))
