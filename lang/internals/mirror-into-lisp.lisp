(in-package #:lang.internals)

(defvar *mirror-into-lisp-table*
  (make-hash-table :weakness :value :synchronized t)
  "A hash table mapping from integers that are PyObject addresses to the
corresponding Lisp objects.

Each Lisp object must have a finalizer that decreases the reference count of
its corresponding PyObject.

Use weak references for the values, because we can recreate them
at any time if necessary.")

(defmethod move-into-lisp (pyobject)
  (multiple-value-bind (value presentp)
      (gethash (pyobject-address pyobject) *mirror-into-lisp-table*)
    (if presentp
        (prog1 value
          (with-global-interpreter-lock-held
            (pyobject-decref pyobject)))
        (make-mirror-object :strong-reference pyobject))))

(defmethod mirror-into-lisp (pyobject)
  (multiple-value-bind (value presentp)
      (gethash (pyobject-address pyobject) *mirror-into-lisp-table*)
    (if presentp
        value
        (make-mirror-object :borrowed-reference pyobject))))

(defmethod shared-initialize :after
    ((python-object python-object)
     (slot-names t)
     &key pyobject &allow-other-keys)
  "For a supplied Python object, set its funcallable instance function, define a
finalizer for it, and register it in the mirror-into-lisp table."
  (set-funcallable-instance-function
   python-object
   (lambda (&rest args)
     (pyapply pyobject args)))
  (alexandria:ensure-gethash
   (pyobject-address pyobject)
   *mirror-into-lisp-table*
   (prog1 python-object
     (register-python-object-finalizer pyobject python-object))))

(defun register-python-object-finalizer (pyobject python-object)
  (trivial-garbage:finalize
   python-object
   (lambda ()
     (with-global-interpreter-lock-held
       #+(or)
       (format *trace-output* "~&(Lisp) Finalizing ~S.~%"
               (let ((pyrepr (pyobject-repr pyobject)))
                 (unwind-protect (string-from-pyobject pyrepr)
                   (pyobject-decref pyrepr))))
       (pyobject-decref pyobject)))))

(defun pyapply (pycallable args)
  (declare (pyobject pycallable))
  (multiple-value-bind (nargs nkwargs kwstart)
      (labels ((scan-positional (args nargs)
                 (if (null args)
                     (values nargs 0 '())
                     (scan-positional (rest args) (1+ nargs))
                     #+(or) ;; TODO
                     (if (keywordp (first args))
                         (scan-keyword args nargs 0 args)
                         (scan-positional (rest args) (1+ nargs)))))
               (scan-keyword (args nargs nkwargs kwstart)
                 (if (null args)
                     (values nargs nkwargs kwstart)
                     (let ((rest (rest args)))
                       (if (null rest)
                           (error "Odd number of keyword arguments in ~S." args)
                           (scan-keyword (rest rest) nargs (1+ nkwargs) kwstart))))))
        (scan-positional args 0))
    (with-global-interpreter-lock-held
      (let (;; Stack-allocate a vector of addresses that is large enough to
            ;; hold one pointer per argument plus one extra element for
            ;; Python's vectorcall calling convention.
            (argv (make-array (+ nargs nkwargs 1) :element-type '(unsigned-byte 64)))
            ;; If there are keyword arguments, allocate a Python tuple for
            ;; holding the keyword strings.
            (kwnames (if (zerop nkwargs)
                         (cffi:null-pointer)
                         (pytuple-new nkwargs))))
        (declare (dynamic-extent argv))
        ;; Mirror all positional arguments to Python.
        (loop for index below nargs
              for arg in args
              do (setf (aref argv (1+ index))
                       (argument-pyobject arg)))
        ;; Mirror all keyword arguments to Python.
        (loop for index below nkwargs
              for (keyword argument) on kwstart
              do (setf (pytuple-getitem kwnames index)
                       (pyobject-from-string
                        (string-downcase
                         (symbol-name keyword))))
              do (setf (aref argv (+ 1 nargs index))
                       (argument-pyobject argument)))
        ;; Perform the actual call and mirror the result into Lisp.
        (move-into-lisp
         (prog1 (pyobject-vectorcall
                 pycallable
                 (cffi:mem-aptr (sb-sys:vector-sap argv) :pointer 1)
                 (+ nargs nkwargs +python-vectorcall-arguments-offset+)
                 kwnames)
           ;; Ensure that arguments aren't collected before this point.
           (touch args)
           (unless (cffi:null-pointer-p kwnames)
             (pyobject-decref kwnames))))))))

(defmacro with-pyobjects (bindings &body body)
  "Bind each variable to a borrowed reference to the PyObject corresponding to each
supplied Lisp object, and execute BODY in an environment that holds the global
interpreter lock.

Example:
 (with-pyobjects ((pyobject object))
   (foo pyobject))"
  `(with-global-interpreter-lock-held
     (let* ,(loop for (pyvar form) in bindings
                 collect
                 `(,pyvar (mirror-into-python ,form)))
       (unwind-protect (progn ,@body)
         ,@(loop for (pyvar form) in bindings
                 collect
                 `(pyobject-decref ,pyvar))))))

(defstruct (literal-keyword
            (:constructor literal-keyword (keyword))
            (:copier nil)
            (:predicate literal-keyword-p))
  "A wrapper for keywords that should be passed to Python as objects instead of
triggering the start of the keyword argument portion."
  (keyword (alexandria:required-argument :keyword)
   :type keyword
   :read-only t))

(defun positional-argument (argument)
  (if (keywordp argument)
      (literal-keyword argument)
      argument))

(defun argument-pyobject (argument)
  (pyobject-address
   (mirror-into-python
    (if (literal-keyword-p argument)
        (literal-keyword-keyword argument)
        argument))))

(ensure-class
 'python:type
 :metaclass (find-class 'python-class)
 :direct-superclasses (list (find-class 'python-class))
 :pyobject *type-pyobject*)

(ensure-class
 'python:object
 :metaclass (find-class 'python:type)
 :direct-superclasses (list (find-class 'python-object))
 :pyobject *object-pyobject*)

(ensure-class
 'python:none
 :metaclass (find-class 'python:type)
 :direct-superclasses (list (find-class 'python:type))
 :pyobject *none-pyobject*)

;; Add a :PACKAGE slot to all mirrored Python modules.
(ensure-class
 'python:module
 :metaclass (find-class 'python:type)
 :direct-superclasses (list (find-class 'python:object))
 :pyobject *module-pyobject*
 :direct-slots
 `((:name %package
    :initfunction ,(lambda () (error "No package provided"))
    :readers (module-package)
    :initargs (:package)
    :initform (error "No package provided."))))

;; Treat null pointers as python:none.
(setf (gethash 0 *mirror-into-lisp-table*)
      (find-class 'python:none))

(defun make-mirror-object (reference-type pyobject)
  (with-global-interpreter-lock-held
    ;; Acquire a strong reference that is released by the finalizer of the
    ;; resulting mirror object.
    (ecase reference-type
      (:strong-reference)
      (:borrowed-reference
       (pyobject-incref pyobject)))
    (let* ((pytype (pyobject-type pyobject))
           (class (if (pyobject-eq pytype pyobject)
                      (error "Encountered an object that is its own type.")
                      (move-into-lisp pytype))))
      (cond ((pytype-subtypep pytype *type-pyobject*)
             ;; Create a type.
             (let* ((class-name (pytype-class-name pyobject))
                    (direct-superclasses (pytype-direct-superclasses pyobject)))
               (ensure-class
                class-name
                :metaclass class
                :direct-superclasses direct-superclasses
                :pyobject pyobject)))
            ((pytype-subtypep pytype *module-pyobject*)
             ;; Create a module
             (make-instance class
               :pyobject pyobject
               :package
               (pymodule-package pyobject)))
            (t
             ;; Create an instance.
             (make-instance class
               :pyobject pyobject))))))

(defun ensure-package (name)
  (declare (string name))
  (or (find-package name)
      (make-package name)))

(defun pytype-class-name (pytype)
  (with-global-interpreter-lock-held
    (let* ((pyname (ignore-errors (pytype-name pytype))))
      (intern
       (if (or (null pyname)
               (cffi:null-pointer-p pyname))
           (format nil "UNNAMED-PYTHON-TYPE-~X" (random most-positive-fixnum))
           (lisp-style-name (string-from-pyobject pyname)))
       (pymodule-package
        (pyobject-getattr-string pytype "__module__"))))))

(defun pymodule-package (pymodule)
  (declare (pyobject pymodule))
  (let ((address (cffi:pointer-address pymodule)))
    (multiple-value-bind (module presentp)
        (gethash address *mirror-into-lisp-table*)
      (if (not presentp)
          (ensure-package
           (concatenate
            'string
            "LANG.PYTHON."
            (string-upcase
             (with-global-interpreter-lock-held
               (or (pymodule-name pymodule)
                   (string-from-pyobject (pyobject-str pymodule)))))))
          (module-package module)))))

(defun pytype-direct-superclasses (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (with-global-interpreter-lock-held
    (let ((bases (pyobject-getattr-string pyobject "__bases__")))
      (loop for position below (pytuple-size bases)
            collect
            (mirror-into-lisp
             (pytuple-getitem bases position))))))

;;; Define mirror objects for all of Python's built-in types.

(macrolet ((def (name var)
             `(defparameter ,name (mirror-into-lisp ,var))))
  (def python:bool *bool-pyobject*)
  (def python:bytearray *bytearray-pyobject*)
  (def python:bytes *bytes-pyobject*)
  (def python:builtin-function-or-method *pycfunction-pyobject*)
  (def python:complex *complex-pyobject*)
  (def python:dict *dict-pyobject*)
  (def python:float *float-pyobject*)
  (def python:frozenset *frozenset-pyobject*)
  (def python:int *long-pyobject*)
  (def python:list *list-pyobject*)
  (def python:module *module-pyobject*)
  (def python:none *none-pyobject*)
  (def python:object *object-pyobject*)
  (def python:range *range-pyobject*)
  (def python:set *set-pyobject*)
  (def python:slice *slice-pyobject*)
  (def python:str *unicode-pyobject*)
  (def python:tuple *tuple-pyobject*)
  (def python:type *type-pyobject*))
