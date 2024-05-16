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
  (format stream "#<~A ~A>"
          (class-name (class-of python-object))
          (let ((pyrepr (pyobject-repr (python-object-pyobject python-object))))
            (unwind-protect (string-from-pyobject pyrepr)
              (pyobject-decref pyrepr)))))

(defmethod shared-initialize :after
    ((python-object python-object)
     (slot-names t)
     &key pyobject &allow-other-keys)
  "Register the Python object in the global Python object table, define a
finalizer for it, and set its funcallable instance function."
  (alexandria:ensure-gethash
   (pyobject-address pyobject)
   *python-object-table*
   (register-python-object-finalizer pyobject python-object))
  (set-funcallable-instance-function
   python-object
   (lambda (&rest args)
     (pyapply pyobject args))))

(defun register-python-object-finalizer (pyobject python-object)
  (pyobject-incref pyobject)
  (trivial-garbage:finalize
   python-object
   (lambda ()
     ;; TODO ensure we are in the main thread.
     (pyobject-decref pyobject))))

(defun pyapply (pycallable args)
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
      ;; Call
      (mirror-into-lisp
       (prog1 (pyobject-vectorcall
               pycallable
               (cffi:mem-aptr (sb-sys:vector-sap argv) :pointer 1)
               (+ nargs nkwargs +python-vectorcall-arguments-offset+)
               kwnames)
         ;; Ensure that arguments aren't collected before this point.
         (touch args)
         (unless (cffi:null-pointer-p kwnames)
           (pyobject-decref kwnames)))))))

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
