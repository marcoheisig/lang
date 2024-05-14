(in-package #:sbclmodule)

;;; Mirroring Python Objects into Lisp

(defparameter *python-object-table*
  (make-hash-table :weakness :value)
  "A hash table mapping from integers that are PyObject addresses to the
corresponding Python objects.

Each Python object must have a finalizer that decreases the reference count of
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

;;; Mirroring Lisp Objects into Python

(defparameter *lispobj-table*
  (make-hash-table)
  "A hash table mapping from Lisp objects to their corresponding lispobj.

Each lispobj must have a finalizer attached that deletes its entry in this
table so that the garbage collector may eventually clean up the Lisp object.")

(defparameter *lispobj-lisp-objects*
  (make-hash-table)
  "A hash table mapping from lispobj addresses to their corresponding Lisp
objects.

This table is necessary because we cannot store references to Lisp objects on
the Python heap.")

(defun make-class-lispobj (class metaclass-lispobj superclass-lispobjs)
  (let* ((symbol (class-name class))
         (name (pyobject-from-string
                (format nil "~A:~A"
                        (package-name (symbol-package symbol))
                        (symbol-name symbol))))
         (bases (apply #'pytuple superclass-lispobjs))
         (dict (pydict-new))
         (lispobj (pycall metaclass-lispobj name bases dict)))
    (when (cffi:null-pointer-p lispobj)
      (error "Failed to mirror the class ~S into Python." class))
    (setf (gethash class *lispobj-table*)
          lispobj)
    (setf (gethash lispobj *lispobj-lisp-objects*)
          class)
    (pyobject-decref dict)
    (pyobject-decref bases)
    (pyobject-decref name)
    lispobj))

(defun make-instance-lispobj (object class-lispobj)
  (let ((lispobj (pycall class-lispobj)))
    (when (cffi:null-pointer-p lispobj)
      (error "Failed to mirror the object ~S into Python." object))
    (setf (gethash object *lispobj-table*)
          lispobj)
    (setf (gethash lispobj *lispobj-lisp-objects*)
          object)
    lispobj))

(defclass proto-standard-class (standard-class)
  ())

(defparameter *proto-standard-class-pyobject*
  (make-class-lispobj
   (find-class 'proto-standard-class)
   *type-pyobject*
   (list *type-pyobject*)))

(defparameter *t-pyobject*
  (make-class-lispobj
   (find-class 't)
   *type-pyobject*
   (list *object-pyobject*)))

(defgeneric mirror-into-python (object))

(defmethod mirror-into-python :around
    ((object t))
  (or (gethash object *lispobj-table*)
      (call-next-method)))

(defmethod mirror-into-python ((class class))
  ;; In Python, the metaclass of a derived class must be a (non-strict)
  ;; subclass of the metaclasses of all its bases.  This is not always the
  ;; case in the Common Lisp world, so we have to filter out bases that would
  ;; violate this rule.
  (let* ((metaclass (class-of class))
         (supers
           (if (eq class (find-class 'standard-object))
               (list (find-class 't))
               (class-direct-superclasses class))))
    (make-class-lispobj
     class
     (if (eq metaclass (find-class 'standard-class))
         *proto-standard-class-pyobject*
         (mirror-into-python metaclass))
     (append (mapcar #'mirror-into-python supers)
             (if (subtypep class 'class)
                 (list *type-pyobject*)
                 (list *object-pyobject*))))))

(defmethod mirror-into-python ((object t))
  (make-instance-lispobj
   object
   (mirror-into-python (class-of object))))

;;; TODO change-class -> update-dependents -> update PyObject type
