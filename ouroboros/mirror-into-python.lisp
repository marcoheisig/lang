(in-package #:ouroboros)

(defparameter *lispobj-table*
  (make-hash-table)
  "A hash table mapping from Lisp objects to their corresponding lispobj.

Each lispobj must have a finalizer attached that deletes its entry in this
table so that the garbage collector may eventually clean up the Lisp object.")

(defun make-class-lispobj (class metaclass-lispobj superclass-lispobjs)
  (let* ((name (class-lispobj-name class))
         (bases (apply #'pytuple superclass-lispobjs))
         (dict (class-lispobj-dict class))
         (args (pytuple name bases dict))
         (lispobj (pyobject-call-object metaclass-lispobj args)))
    (when (cffi:null-pointer-p lispobj)
      (error "Failed to mirror the class ~S into Python." class))
    (setf (gethash class *lispobj-table*)
          lispobj)
    (setf (gethash (pyobject-address lispobj) *python-object-table*)
          class)
    (pyobject-decref args)
    (pyobject-decref dict)
    (pyobject-decref bases)
    (pyobject-decref name)
    lispobj))

(defun class-lispobj-name (class)
  (let* ((symbol (class-name class))
         (pname (package-name (symbol-package symbol)))
         (sname (symbol-name symbol)))
    (pyobject-from-string
     (if (string= pname "PYTHON")
         sname
         (let ((separator
                 (if (eql (nth-value 1 (find-symbol sname pname)) :internal)
                     "::"
                     ":")))
           (concatenate 'string pname separator sname))))))

(defun class-lispobj-dict (class)
  "Returns a Python dictionary that describes all the methods and attributes of
the supplied class."
  (let ((pydict (pydict-new)))
    ;; __repr__
    ;; __str__
    ;; __doc__
    pydict))

(defun make-instance-lispobj (object class-lispobj)
  (let ((lispobj (pyobject-call-no-args class-lispobj)))
    (when (cffi:null-pointer-p lispobj)
      (error "Failed to mirror the object ~S into Python." object))
    (setf (gethash object *lispobj-table*)
          lispobj)
    (setf (gethash (cffi:pointer-address lispobj) *python-object-table*)
          object)
    lispobj))

(defclass proto-standard-class (standard-class)
  ())

(defparameter *proto-standard-class-pyobject*
  (with-global-interpreter-lock-held
    (make-class-lispobj
     (find-class 'proto-standard-class)
     *type-pyobject*
     (list *type-pyobject*))))

(defparameter *t-pyobject*
  (with-global-interpreter-lock-held
    (make-class-lispobj
     (find-class 't)
     *type-pyobject*
     (list *object-pyobject*))))

(defgeneric mirror-into-python (object))

(defmethod mirror-into-python :around
    ((object t))
  (or (gethash object *lispobj-table*)
      (call-next-method)))

(defmethod mirror-into-python
    ((python-object python-object))
  (python-object-pyobject python-object))

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
    (with-global-interpreter-lock-held
      (make-class-lispobj
       class
       (if (eq metaclass (find-class 'standard-class))
           *proto-standard-class-pyobject*
           (mirror-into-python metaclass))
       (append (mapcar #'mirror-into-python supers)
               (if (subtypep class 'class)
                   (list *type-pyobject*)
                   (list *object-pyobject*)))))))

(defmethod mirror-into-python ((object t))
  (with-global-interpreter-lock-held
    (make-instance-lispobj
     object
     (mirror-into-python (class-of object)))))

(defmacro with-pyobjects (bindings &body body)
  "Retrieve the PyObject of each supplied Lisp object, increment its reference
count, execute BODY, and then decrement the reference count.

Example:
 (with-pyobjects ((pyobject object))
   (foo pyobject))"
  (let* ((obvars (loop repeat (length bindings) collect (gensym)))
         (pyvars (mapcar #'first bindings))
         (forms (mapcar #'second bindings))
         (obvar-bindings (mapcar #'list obvars forms))
         (pyvar-bindings
           (loop for pyvar in pyvars
                 for obvar in obvars
                 collect
                 `(,pyvar (mirror-into-python ,obvar)))))
    `(let (,@obvar-bindings)
       (let (,@pyvar-bindings)
         ;; Increment the refcount of each PyObject, and then touch the
         ;; corresponding Lisp object so that it doesn't get garbage collected
         ;; before the refcount is increased.
         ,@(loop for pyvar in pyvars
                 for obvar in obvars
                 collect
                 `(progn (pyobject-incref ,pyvar)
                         (touch ,obvar)))
         (unwind-protect (progn ,@body)
           ;;Decrement the refcount of each PyObject.
           ,@(loop for pyvar in pyvars
                   collect
                   `(pyobject-decref ,pyvar)))))))

(declaim (notinline touch))

(defun touch (x) x)

;;; TODO change-class -> update-dependents -> update PyObject type

;;; TODO finalizer

;;; TODO __repr__

;;; TODO __str__

;;; TODO __call__

;;; TODO Make CL accessible
