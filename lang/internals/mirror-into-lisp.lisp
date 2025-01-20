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
   (create-funcallable-instance-function pyobject))
  (alexandria:ensure-gethash
   (pyobject-address pyobject)
   *mirror-into-lisp-table*
   (prog1 python-object
     (register-python-object-finalizer pyobject python-object))))

(defun create-funcallable-instance-function (pycallable)
  #+(or)
  (multiple-value-bind (posonlyargs)
      (lambda (pycallable)
        (lambda (,@posonlyargs ,@args-sans-defaults &rest ,rest)
          (pyapply pycallable ,@posonlyargs ,@args-sans-defaults ,rest))))
  (lambda (&rest args)
    (pyapply pycallable args)))

(defun pycallable-lambda-list (pycallable)
  ;; Option 1: Use the inspect module.
  ;; Option 2: Use typeshed_client.
  ;; Option 3: Use (&rest rest) as a reasonable default.
  '(&rest rest))

(defmethod shared-initialize :after
    ((python-exception python-exception)
     (slot-names t)
     &key pyobject &allow-other-keys)
  (alexandria:ensure-gethash
   (pyobject-address pyobject)
   *mirror-into-lisp-table*
   (prog1 python-exception
     (register-python-object-finalizer pyobject python-exception))))

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

(defstruct (kwarg
            (:constructor kwarg (key value &aux (key (pythonize-string key))))
            (:copier nil)
            (:predicate kwargp))
  (key (alexandria:required-argument :key)
   :type python-object
   :read-only t)
  (value (alexandria:required-argument :value)
   :type t
   :read-only t))

(defmethod print-object ((kwarg kwarg) stream)
  (format stream (if *print-readably* "#.(kwarg ~S ~S)" "~A=~A")
          (lisp-string-from-python-string (kwarg-key kwarg))
          (kwarg-value kwarg)))

(defun pyapply (pycallable args)
  (declare (pyobject pycallable))
  ;; Count the number of positional arguments and keyword arguments and
  ;; determine the cons holding the first keyword argument.
  (multiple-value-bind (nargs nkwargs kwstart)
      (labels ((scan-positional (args nargs)
                 (if (null args)
                     (values nargs 0 '())
                     (let ((arg (first args))
                           (rest (rest args)))
                       (if (kwargp arg)
                           (scan-keyword rest nargs 1 args)
                           (scan-positional rest (1+ nargs))))))
               (scan-keyword (args nargs nkwargs kwstart)
                 (if (null args)
                     (values nargs nkwargs kwstart)
                     (let ((arg (first args))
                           (rest (rest args)))
                       (if (kwargp arg)
                           (scan-keyword rest nargs (1+ nkwargs) kwstart)
                           (error "Positional argument following keyword argument: ~A"
                                  arg))))))
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
        (declare (type (simple-array (unsigned-byte 64) (*)) argv)
                 (type cffi:foreign-pointer kwnames)
                 (dynamic-extent argv))
        ;; Mirror all positional arguments to Python.
        (loop for index below nargs
              for arg in args
              do (setf (aref argv (+ 1 index))
                       ;; TODO mirror-into-python returns a strong reference,
                       ;; but we should pass a borrowed reference instead.
                       (mirror-into-python arg)))
        ;; Mirror all keyword arguments to Python.
        (loop for index below nkwargs
              for kwarg in kwstart
              do (setf (pytuple-getitem kwnames index)
                       (python-object-pyobject (kwarg-key kwarg)))
              do (setf (aref argv (+ 1 nargs index))
                       ;; TODO mirror-into-python returns a strong reference,
                       ;; but we should pass a borrowed reference instead.
                       (mirror-into-python (kwarg-value kwarg))))
        ;; Perform the actual call and mirror the result into Lisp.
        (let ((pyobject (pyobject-vectorcall
                         pycallable
                         (cffi:mem-aptr (sb-sys:vector-sap argv) :pointer 1)
                         (+ nargs nkwargs +python-vectorcall-arguments-offset+)
                         kwnames)))
          ;; Ensure that arguments aren't collected before this point.
          (touch args)
          ;; Release the Python list of keywords.
          (unless (cffi:null-pointer-p kwnames)
            (pyobject-decref kwnames))
          ;; Mirror the result into Lisp.
          (move-into-lisp pyobject))))))

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

(defun lisp-integer-from-python-integer (python-integer)
  (declare (python-object python-integer))
  (with-pyobjects ((pylong python-integer))
    ;; TODO support bignums
    (pylong-as-long pylong)))

(defun lisp-float-from-python-float (python-float)
  (declare (python-object python-float))
  (with-pyobjects ((pyfloat python-float))
    (pyfloat-as-double pyfloat)))

(defun lisp-complex-from-python-complex (python-complex)
  (declare (python-object python-complex))
  (with-pyobjects ((pycomplex python-complex))
    (complex (pycomplex-real-as-double pycomplex)
             (pycomplex-imag-as-double pycomplex))))

(defun lisp-string-from-python-string (python-string)
  (declare (python-object python-string))
  (with-pyobjects ((pyobject python-string))
    (string-from-pyobject pyobject)))

(defun python-integer-from-lisp-integer (lisp-integer)
  (declare (integer lisp-integer))
  (with-global-interpreter-lock-held
    ;; TODO support bignums
    (move-into-lisp (pylong-from-long lisp-integer))))

(defun python-float-from-lisp-float (lisp-float)
  (declare (float lisp-float))
  (with-global-interpreter-lock-held
    (move-into-lisp
     (pyfloat-from-double
      (coerce lisp-float 'double-float)))))

(defun python-complex-from-lisp-complex (lisp-complex)
  (declare (complex lisp-complex))
  (with-global-interpreter-lock-held
    (move-into-lisp
     (pycomplex-from-doubles
      (coerce (realpart lisp-complex) 'double-float)
      (coerce (imagpart lisp-complex) 'double-float)))))

(defun python-string-from-lisp-string (lisp-string)
  (declare (string lisp-string))
  (with-global-interpreter-lock-held
    (move-into-lisp
     (pyobject-from-string lisp-string))))

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

(define-condition python:base-exception (python-exception)
  ())

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
      (cond
        ;; Types
        ((pytype-subtypep pytype *type-pyobject*)
         (let ((class-name (pytype-class-name pyobject))
               (direct-superclasses (pytype-direct-superclasses pyobject)))
           ;; Create either a condition or a regular Python type.
           (if (or (eql class-name 'python:base-exception)
                   (every (lambda (class)
                            (subtypep (class-name class) 'python-exception))
                          direct-superclasses))
               (or (find-class class-name nil)
                   (find-class
                    (eval `(define-condition ,class-name
                               ,(mapcar #'class-name direct-superclasses)
                             ()))))
               (ensure-class
                class-name
                :metaclass class
                :direct-superclasses direct-superclasses
                :pyobject pyobject))))
        ;; Modules
        ((pytype-subtypep pytype *module-pyobject*)
         (make-instance class
           :pyobject pyobject
           :package
           (pymodule-package pyobject)))
        ;; Instances
        (t
         (make-instance class
           :pyobject pyobject))))))

(defun ensure-package (name)
  (declare (string name))
  (or (find-package name)
      (make-package name)))

(defun pytype-class-name (pytype)
  (with-global-interpreter-lock-held
    (let* ((pyname (ignore-errors (pytype-name pytype)))
           (name (if (or (null pyname)
                         (cffi:null-pointer-p pyname))
                     (format nil "UNNAMED-PYTHON-TYPE-~X" (random most-positive-fixnum))
                     (lisp-style-name (string-from-pyobject pyname))))
           (package
             (let* ((pymodule-name (pyobject-getattr-string pytype "__module__"))
                    (pymodule (pyimport-import pymodule-name)))
               (pymodule-package pymodule))))
      (intern name package))))

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
                   (string-from-pyobject
                    (pyobject-str pymodule)))))))
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
