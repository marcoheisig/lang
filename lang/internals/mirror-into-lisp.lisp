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
   (lisp-function-from-pycallable pyobject))
  (alexandria:ensure-gethash
   (pyobject-address pyobject)
   *mirror-into-lisp-table*
   (prog1 python-object
     (register-python-object-finalizer pyobject python-object))))

(define-condition unable-to-derive-pycallable-lambda-list (serious-condition)
  ())

(defvar *pycallable-lambda-list-derivers* '()
  "A list of function designators that are called one after the other on a pycallable to derive a
suitable lambda list.  Each function may throw :failure to transfer control to
the next one.")

(defun pycallable-lambda-list (pycallable)
  "Returns a lambda list for the pycallable that consists purely of required
arguments and possibly a rest argument."
  (the (values list &optional)
       (loop for fn in *pycallable-lambda-list-derivers* do
         (handler-case (return (funcall fn pycallable))
           (unable-to-derive-pycallable-lambda-list () ()))
             finally (return '(&rest rest)))))

;; We'll later redefine lisp-function-from-pycallable to generate more detailed
;; lambda lists, so we declare it as notinline.
(declaim (notinline lisp-function-from-pycallable))
(defun lisp-function-from-pycallable (pycallable)
  (let* ((lambda-list (pycallable-lambda-list pycallable))
         (restp (position '&rest lambda-list)))
    (funcall
     (compile
      nil
      `(lambda (pycallable)
         (lambda ,lambda-list
           (let ((args ,(if (not restp)
                            `(list ,@lambda-list)
                            `(list* ,@(subseq lambda-list 0 restp)
                                    ,(elt lambda-list (1+ restp))))))
             (declare (dynamic-extent args))
             (pyapply pycallable args)))))
     pycallable)))

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
  (declare (dynamic-extent args))
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
    (declare (unsigned-byte nargs nkwargs)
             (list kwstart))
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
                       (pyobject-address
                        (mirror-into-python arg))))
        ;; Mirror all keyword arguments to Python.
        (loop for index below nkwargs
              for kwarg in kwstart
              do (setf (pytuple-getitem kwnames index)
                       (python-object-pyobject (kwarg-key kwarg)))
              do (setf (aref argv (+ 1 nargs index))
                       ;; TODO mirror-into-python returns a strong reference,
                       ;; but we should pass a borrowed reference instead.
                       (pyobject-address
                        (mirror-into-python
                         (kwarg-value kwarg)))))
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

(defun find-module (module-name)
  (with-global-interpreter-lock-held
    (move-into-lisp
     (etypecase module-name
       (string
        (pyimport-import-module module-name))
       (python-object
        (with-pyobjects ((pyname module-name))
          (pyimport-import pyname)))))))

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

;; Define the module class and (f)bind python:module.

(ensure-class
 'python:module
 :metaclass (find-class 'python:type)
 :direct-superclasses (list (find-class 'python:object))
 :pyobject *module-pyobject*
 :direct-slots
 `((:name %package
    :initfunction ,(lambda () #1=(error "No package provided"))
    :readers (module-package)
    :initargs (:package)
    :initform #1#)
   (:name %symbol-table
    :initfunction ,(lambda () #2=(error "No symbol table provided"))
    :readers (module-symbol-table)
    :initargs (:symbol-table)
    :initform #2#)))

(setf (symbol-function 'python:module)
      (find-class 'python:module))

(defparameter python:module
  (find-class 'python:module))

;; For reasons I haven't fully understood yet, these modules have to be
;; imported once before doing anything else, or they won't be mirrored as a
;; module but as builting_function_or_method.  It probably has something to do
;; with some CPython hack to improve startup times.
(defparameter *weird-modules*
  (with-global-interpreter-lock-held
    (list (pyimport-import-module "ast")
          (pyimport-import-module "collections.abc")
          (pyimport-import-module "inspect")
          (pyimport-import-module "weakref"))))

(defun make-mirror-object (reference-type pyobject)
  (with-global-interpreter-lock-held
    ;; Acquire a strong reference that is released by the finalizer of the
    ;; resulting mirror object.
    (ecase reference-type
      (:strong-reference)
      (:borrowed-reference
       (pyobject-incref pyobject)))
    ;; Determine the object's Python type and Lisp class.
    (let* ((pytype (pyobject-type pyobject))
           (class (if (pyobject-eq pytype pyobject)
                      (error "Encountered an object that is its own type.")
                      (move-into-lisp pytype))))
      (cond
        ;; Modules
        ((or (cffi:pointer-eq pytype *module-pyobject*)
             (pyobject-typep pyobject *module-pyobject*))
         (multiple-value-bind (package symbol-table)
             (pymodule-package-and-symbol-table pyobject)
           (make-instance class
             :pyobject pyobject
             :package package
             :symbol-table symbol-table)))
        ;; Types
        ((pyobject-typep pyobject *type-pyobject*)
         (let* ((direct-superclasses (pytype-direct-superclasses pyobject))
                (python-name
                  (with-pyobjects ((pyname (move-into-lisp (pytype-qualified-name pyobject))))
                    (string-from-pyobject pyname)))
                (pymodule (pytype-defining-pymodule pyobject))
                (lisp-name
                  (if (null pymodule)
                      (gensym python-name)
                      (multiple-value-bind (package symbol-table)
                          (pymodule-package-and-symbol-table pymodule)
                        (or (bijection-value symbol-table python-name)
                            ;; TODO gensym?
                            (intern (lisp-style-name python-name)
                                    package))))))
           (if (or (eql lisp-name 'python:base-exception)
                   (some
                    (lambda (superclass)
                      (subtypep (class-name superclass) 'python-exception))
                    direct-superclasses))
               ;; Find or create a condition.
               (or (find-class lisp-name nil)
                   (find-class
                    (eval `(define-condition ,lisp-name
                               ,(mapcar #'class-name direct-superclasses)
                             ()))))
               ;; Find or create a class.
               (if (find-class lisp-name nil)
                   (error "Attempt to overwrite the mirror class ~S." lisp-name)
                   (ensure-class
                    lisp-name
                    :metaclass class
                    :direct-superclasses direct-superclasses
                    :pyobject pyobject)))))
        ;; Instances
        (t
         (make-instance class
           :pyobject pyobject))))))

(defun pytype-defining-pymodule (pytype)
  "Returns the PyModule that defines the supplied PyType.  Returns
NIL if the type has no module or is not a toplevel definition of its module."
  (with-global-interpreter-lock-held
    (when (pyobject-hasattr-string pytype "__module__")
      (with-pyobjects
          ((pytype-name (move-into-lisp (pyobject-getattr-string pytype "__qualname__")))
           (pymodule-name (move-into-lisp (pyobject-getattr-string pytype "__module__")))
           (pymodule (move-into-lisp (pyimport-import pymodule-name)))
           (pyvalue (move-into-lisp
                     (if (pyobject-hasattr pymodule pytype-name)
                         (pyobject-getattr pymodule pytype-name)
                         (pylong-from-long 0)))))
        (when (and (pyobject-typep pymodule *module-pyobject*)
                   (pyobject-hasattr pymodule pytype-name)
                   (cffi:pointer-eq pytype pyvalue))
          pymodule)))))

(defun pymodule-package-and-symbol-table (pymodule)
  (declare (pyobject pymodule))
  ;; If the pymodule already has a corresponding mirror object, return the
  ;; package of that mirror object.
  (multiple-value-bind (module presentp)
      (gethash (cffi:pointer-address pymodule) *mirror-into-lisp-table*)
    (when presentp
      (return-from pymodule-package-and-symbol-table
        (values
         (module-package module)
         (module-symbol-table module)))))
  ;; If there is no mirror object yet, create or lookup the right package and
  ;; populate it with symbols.
  (with-global-interpreter-lock-held
    (let* ((dict (pymodule-dict pymodule))
           (keys (pydict-keys dict))
           (size (pydict-size dict)))
      (python-to-lisp-naming
       ;; Determine the module name.
       (or (pymodule-name pymodule)
           (format nil "unnamed-module-~X" (random most-positive-fixnum)))
       ;; Determine the module contents.
       (loop for index below size
             collect
             (string-from-pyobject
              (pylist-getitem keys index)))))))

(defun pytype-direct-superclasses (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (with-global-interpreter-lock-held
    (let ((bases (pyobject-getattr-string pyobject "__bases__")))
      (loop for position below (pytuple-size bases)
            collect
            (mirror-into-lisp
             (pytuple-getitem bases position))))))
