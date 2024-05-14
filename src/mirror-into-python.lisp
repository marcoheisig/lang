(in-package #:sbclmodule)

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
  (let* ((name (class-lispobj-name class))
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

(defun class-lispobj-name (class)
  (let* ((symbol (class-name class))
         (pname (package-name (symbol-package symbol)))
         (sname (symbol-name symbol))
         (internalp (eql (nth-value 1 (find-symbol sname pname)) :internal))
         (separator (if internalp "::" ":")))
    (pyobject-from-string
     (concatenate 'string pname separator sname))))

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

;;; TODO finalizer

;;; TODO __repr__

;;; TODO __str__

;;; TODO __call__

;;; TODO Make CL accessible
