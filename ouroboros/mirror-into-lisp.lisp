(in-package #:ouroboros)

(defparameter *python-object-table*
  (make-hash-table :weakness :value)
  "A hash table mapping from integers that are PyObject addresses to the
corresponding Lisp objects.

Each Lisp object should have a finalizer that decreases the reference count of
its corresponding PyObject.

Use weak references for the values, because we can recreate the Python object
at any time if necessary.")

(defclass python-object (funcallable-standard-object)
  ((%pyobject
    :initarg :pyobject
    :initform (alexandria:required-argument :pyobject)
    :type pyobject
    :reader python-object-pyobject))
  (:metaclass funcallable-standard-class)
  (:documentation
   "An object of the Python programming language."))

(defmethod print-object ((python-object python-object) stream)
  (format stream "#<~A ~16R>"
          (class-name (class-of python-object))
          (python-object-pyobject python-object)))

(defmethod shared-initialize :after
    ((python-object python-object)
     (slot-names t)
     &key pyobject &allow-other-keys)
  "Register the Python object in the global Python object table, define a
finalizer for it, and set its funcallable instance function."
  (set-funcallable-instance-function
   python-object
   (lambda (&rest args)
     (pyapply pyobject args)))
  (alexandria:ensure-gethash
   (pyobject-address pyobject)
   *python-object-table*
   (register-python-object-finalizer pyobject python-object)))

(defun pyapply (pycallable args)
  (declare (pyobject pycallable))
  (multiple-value-bind (nargs nkwargs kwstart)
      (labels ((scan-positional (args nargs)
                 (if (null args)
                     (values nargs 0 '())
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
                       (pyobject-from-string (symbol-name keyword)))
              do (setf (aref argv (+ 1 nargs index))
                       (argument-pyobject argument)))
        ;; Perform the actual call and mirror the result into Lisp.
        (mirror-into-lisp
         (prog1 (pyobject-vectorcall
                 pycallable
                 (cffi:mem-aptr (sb-sys:vector-sap argv) :pointer 1)
                 (+ nargs nkwargs +python-vectorcall-arguments-offset+)
                 kwnames)
           ;; Ensure that arguments aren't collected before this point.
           (touch args)
           (unless (cffi:null-pointer-p kwnames)
             (pyobject-decref kwnames))))))))

(defun register-python-object-finalizer (pyobject python-object)
  (with-global-interpreter-lock-held
    (pyobject-foreign-incref pyobject))
  (trivial-garbage:finalize
   python-object
   (lambda ()
     (with-global-interpreter-lock-held
       (pyobject-decref pyobject)))))

(declaim (ftype (function (t)) mirror-into-python))

(defstruct (literal-keyword
            (:constructor literal-keyword)
            (:copier nil)
            (:predicate literal-keyword-p))
  "A wrapper for keywords that should be passed to Python as objects instead of
triggering the start of the keyword argument portion."
  (keyword (alexandria:required-argument :keyword)
   :type keyword
   :read-only t))

(defun argument-pyobject (argument)
  (pyobject-address
   (mirror-into-python
    (if (literal-keyword-p argument)
        (literal-keyword-keyword argument)
        argument))))

(defclass python-class (python-object funcallable-standard-class)
  ()
  (:metaclass funcallable-standard-class))

(defmethod print-object ((python-class python-class) stream)
  (format stream "#<~A>"
          (class-name python-class)))

(defmethod validate-superclass
    ((python-class python-class)
     (superclass funcallable-standard-class))
  t)

(defmethod print-object ((python-class python-class) stream)
  (format stream "#<~A>" (class-name python-class)))

(defclass python:|type| (python-class)
  ()
  (:metaclass python-class)
  (:pyobject . #.*type-pyobject*))

(defclass python:|object| (python-object)
  ()
  (:metaclass python:|type|)
  (:pyobject . #.*object-pyobject*))

(defclass python:|None| (python:|type|)
  ()
  (:metaclass python:|type|)
  (:pyobject . #.*none-pyobject*))

;; Treat null pointers as None.
(setf (gethash 0 *python-object-table*)
      (find-class 'python:|None|))

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
              (ensure-class
               name
               :metaclass class
               :direct-superclasses direct-superclasses
               :pyobject pyobject))
            ;; Create an instance.
            (make-instance class
              :pyobject pyobject)))))

(defun pytype-name-symbol (pytype)
  (with-global-interpreter-lock-held
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
              lisp-symbol))))))

(defun pytype-direct-superclasses (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (with-global-interpreter-lock-held
    (let ((bases (pyobject-getattr-string pyobject "__bases__")))
      (when (cffi:null-pointer-p bases)
        (error "Couldn't determine the bases of ~S." pyobject))
      (loop for position below (pytuple-size bases)
            collect
            (mirror-into-lisp
             (pytuple-getitem bases position))))))

;;; Define all the built-in types.

(defparameter python:|type|      (mirror-into-lisp *type-pyobject*))

(defparameter python:|object|    (mirror-into-lisp *object-pyobject*))

(defparameter python:|None|      (mirror-into-lisp *none-pyobject*))

(defparameter python:|bytearray| (mirror-into-lisp *bytearray-pyobject*))

(defparameter python:|bytes|     (mirror-into-lisp *bytes-pyobject*))

(defparameter python:|complex|   (mirror-into-lisp *complex-pyobject*))

(defparameter python:|dict|      (mirror-into-lisp *dict-pyobject*))

(defparameter python:|float|     (mirror-into-lisp *float-pyobject*))

(defparameter python:|frozenset| (mirror-into-lisp *frozenset-pyobject*))

(defparameter python:|int|       (mirror-into-lisp *long-pyobject*))

(defparameter python:|list|      (mirror-into-lisp *list-pyobject*))

(defparameter python:|range|     (mirror-into-lisp *range-pyobject*))

(defparameter python:|set|       (mirror-into-lisp *set-pyobject*))

(defparameter python:|slice|     (mirror-into-lisp *slice-pyobject*))

(defparameter python:|str|       (mirror-into-lisp *unicode-pyobject*))

(defparameter python:|tuple|     (mirror-into-lisp *tuple-pyobject*))
