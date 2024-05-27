(in-package #:ouroboros.internals)

(defparameter *lispobj-table*
  (make-hash-table)
  "A hash table mapping from Lisp objects to their corresponding lispobj.

Each lispobj must have a finalizer attached that deletes its entry in this
table so that the garbage collector may eventually clean up the Lisp object.")

(defun make-class-lispobj (class metaclass-lispobj superclass-lispobjs)
  (with-global-interpreter-lock-held
    (let* ((name (class-lispobj-name class))
           (bases (apply #'pytuple superclass-lispobjs))
           (dict (class-lispobj-dict class))
           (args (pytuple name bases dict))
           (lispobj (pyobject-call-object metaclass-lispobj args)))
      (when (subtypep class 'function)
        (setf (cffi:mem-ref lispobj :pointer +pytype-call-offset+)
              (cffi:callback call-into-lisp)))
      (setf (gethash class *lispobj-table*)
            lispobj)
      (setf (gethash (pyobject-address lispobj) *python-object-table*)
            class)
      (pyobject-decref args)
      (pyobject-decref dict)
      (pyobject-decref bases)
      (pyobject-decref name)
      lispobj)))

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
  (declare (ignore class)) ;; TODO
  (with-global-interpreter-lock-held
    (let ((pydict (pydict-new)))
      ;; __repr__
      ;; __str__
      ;; __doc__
      pydict)))

(defun make-instance-lispobj (object class-lispobj)
  (with-global-interpreter-lock-held
    (let ((lispobj (pyobject-call-no-args class-lispobj)))
      (when (cffi:null-pointer-p lispobj)
        (error "Failed to mirror the object ~S into Python." object))
      (setf (gethash object *lispobj-table*)
            lispobj)
      (setf (gethash (cffi:pointer-address lispobj) *python-object-table*)
            object)
      lispobj)))

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
    `(with-global-interpreter-lock-held
       (let (,@obvar-bindings)
         (let (,@pyvar-bindings)
           (unwind-protect (progn ,@body)
             ;; Touch the object to keep it alive, and thereby to keep the
             ;; reference count of the corresponding pyobject above zero.
             ,@(loop for obvar in obvars
                     collect
                     `(touch ,obvar))))))))

(cffi:defcallback call-into-lisp :pointer
    ((callable :pointer)
     (args :pointer)
     (kwargs :pointer))
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
               (otherwise
                (apply fn (arg 0) (arg 1) (arg 2) (arg 3) (arg 4) (arg 5)
                       (loop for index from 6 below nargs collect (arg index)))))
             (error "TODO")))))))

;;; TODO change-class -> update-dependents -> update PyObject type

;;; TODO finalizer

;;; TODO __repr__

;;; TODO __str__

;;; TODO __call__

;;; Steps for making the instances of some class vectorcallable:
;;;
;;; 1. Set Py_TPFLAGS_HAVE_VECTORCALL of the tp_flags slots.
;;;
;;; 2. Set the tp_vectorcall_offset to the byte offset of the tp_vectorcall
;;; slot.
;;;
;;; 3. Set the tp_vectorcall slot to a suitable function.
;;;
;;; 4. Set the tp_call slot to PyVectorcall_Call for backward compatibility.

#+(or)
(cffi:defcallback vectorcall-into-lisp :pointer
    ((callable :pointer)
     (args :pointer)
     (nargsf :size)
     (kwnames :pointer))
  (flet ((argref (index)
           (mirror-into-lisp (cffi:mem-aref args :pointer index))))
    (let ((nargs (logandc2 nargsf +python-vectorcall-arguments-offset+))
          (function (mirror-into-lisp callable)))
      (mirror-into-python
       (if (or (cffi:null-pointer-p kwnames)
               (zerop (pytuple-size kwnames)))
           ;; Call without keyword arguments.
           (case nargs
             (0 (funcall function))
             (1 (funcall function (argref 0)))
             (2 (funcall function (argref 0) (argref 1)))
             (3 (funcall function (argref 0) (argref 1) (argref 2)))
             (4 (funcall function (argref 0) (argref 1) (argref 2) (argref 3)))
             (otherwise
              (apply function (loop for index below nargs collect (argref index)))))
           ;; Call with keyword arguments.
           (let ((args '()))
             (error "TODO")))))))
