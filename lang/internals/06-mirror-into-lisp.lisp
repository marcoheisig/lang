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
            (|%Py_DecRef| pyobject)))
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
               (let ((pyrepr (|%PyObject_Repr| pyobject)))
                 (unwind-protect (string-from-pyobject pyrepr)
                   (|%Py_DecRef| pyrepr))))
       (|%Py_DecRef| pyobject)))))

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
                         (|%PyTuple_New| nkwargs))))
        (declare (type (simple-array (unsigned-byte 64) (*)) argv)
                 (type cffi:foreign-pointer kwnames)
                 (dynamic-extent argv))
        ;; Zero the argument vector, so that the cleanup below can tell
        ;; which arguments have already been mirrored.
        (fill argv 0)
        (unwind-protect
             (progn
               ;; Mirror all positional arguments to Python.  Each entry of
               ;; ARGV is a strong reference that is released below.
               (loop for index below nargs
                     for arg in args
                     do (setf (aref argv (+ 1 index))
                              (pyobject-address
                               (mirror-into-python arg))))
               ;; Mirror all keyword arguments to Python.
               (loop for index below nkwargs
                     for kwarg in kwstart
                     ;; PyTuple_SetItem steals a reference, so we pass it a
                     ;; strong reference to the keyword string.
                     do (|%PyTuple_SetItem| kwnames index
                                            (mirror-into-python (kwarg-key kwarg)))
                     do (setf (aref argv (+ 1 nargs index))
                              (pyobject-address
                               (mirror-into-python
                                (kwarg-value kwarg)))))
               ;; Perform the actual call and mirror the result into Lisp.
               (move-into-lisp
                (|%PyObject_Vectorcall|
                 pycallable
                 (cffi:mem-aptr (sb-sys:vector-sap argv) :pointer 1)
                 ;; Vectorcall expects only the number of positional
                 ;; arguments here and derives the number of keyword
                 ;; arguments from KWNAMES.
                 (+ nargs +python-vectorcall-arguments-offset+)
                 kwnames)))
          ;; Ensure that arguments aren't collected before this point.
          (touch args)
          ;; Release the strong references to all arguments.
          (loop for index from 1 below (length argv)
                for address = (aref argv index)
                unless (zerop address)
                  do (|%Py_DecRef| (cffi:make-pointer address)))
          ;; Release the Python tuple of keywords.
          (unless (cffi:null-pointer-p kwnames)
            (|%Py_DecRef| kwnames)))))))

(defmacro with-pyobjects (bindings &body body)
  "Bind each variable to a borrowed reference to the PyObject corresponding to each
supplied Lisp object, and execute BODY in an environment that holds the global
interpreter lock.

Example:
 (with-pyobjects ((pyobject object))
   (foo pyobject))"
  `(with-global-interpreter-lock-held
     ;; Each variable holds a strong reference for the extent of BODY, which
     ;; is released even if mirroring one of the later objects fails.
     (with-new-pyobjects
         ,(loop for (pyvar form) in bindings
                collect
                `(,pyvar (mirror-into-python ,form)))
       ,@body)))

(defun lisp-integer-from-python-integer (python-integer)
  (declare (python-object python-integer))
  (with-pyobjects ((pylong python-integer))
    (cffi:with-foreign-object (overflow :int)
      (let ((value (|%PyLong_AsLongLongAndOverflow| pylong overflow)))
        (cond ((not (zerop (cffi:mem-ref overflow :int)))
               ;; The integer doesn't fit into a long long, so we take a detour
               ;; via its hexadecimal string representation "[-]0x...".
               (let* ((string
                        (with-pyobjects ((pystring (move-into-lisp (|%PyNumber_ToBase| pylong 16))))
                          (string-from-pyobject pystring)))
                      (negativep (char= (schar string 0) #\-))
                      (start (if negativep 3 2))
                      (magnitude (parse-integer string :start start :radix 16)))
                 (if negativep (- magnitude) magnitude)))
              (t value))))))

(defun lisp-float-from-python-float (python-float)
  (declare (python-object python-float))
  (with-pyobjects ((pyfloat python-float))
    (|%PyFloat_AsDouble| pyfloat)))

(defun lisp-complex-from-python-complex (python-complex)
  (declare (python-object python-complex))
  (with-pyobjects ((pycomplex python-complex))
    (complex (|%PyComplex_RealAsDouble| pycomplex)
             (|%PyComplex_ImagAsDouble| pycomplex))))

(defun lisp-string-from-python-string (python-string)
  (declare (python-object python-string))
  (with-pyobjects ((pyobject python-string))
    (string-from-pyobject pyobject)))

(defun python-integer-from-lisp-integer (lisp-integer)
  (declare (integer lisp-integer))
  (with-global-interpreter-lock-held
    (move-into-lisp
     (if (typep lisp-integer '(signed-byte 64))
         (|%PyLong_FromLongLong| lisp-integer)
         (|%PyLong_FromString| (format nil "~X" lisp-integer) (cffi:null-pointer) 16)))))

(defun python-float-from-lisp-float (lisp-float)
  (declare (float lisp-float))
  (with-global-interpreter-lock-held
    (move-into-lisp
     (|%PyFloat_FromDouble|
      (coerce lisp-float 'double-float)))))

(defun python-complex-from-lisp-complex (lisp-complex)
  (declare (complex lisp-complex))
  (with-global-interpreter-lock-held
    (move-into-lisp
     (|%PyComplex_FromDoubles|
      (coerce (realpart lisp-complex) 'double-float)
      (coerce (imagpart lisp-complex) 'double-float)))))

(defun python-string-from-lisp-string (lisp-string)
  (declare (string lisp-string))
  (with-global-interpreter-lock-held
    (move-into-lisp
     (pyobject-from-string lisp-string))))

(defun python-hasattr (python-object python-string)
  "An implementation of hasattr that we use to load all built-in Python
functions (including hasattr)."
  (with-pyobjects ((pyobject python-object)
                   (pystring python-string))
    (|%PyObject_HasAttrWithError| pyobject pystring)))

(defun python-getattr (python-object python-string)
  "An implementation of getattr that we use to load all built-in Python
functions (including getattr)."
  (with-pyobjects ((pyobject python-object)
                   (pystring python-string))
    (move-into-lisp
     (|%PyObject_GetAttr| pyobject pystring))))

(defun python-getitem (python-object key)
  (with-pyobjects ((pyobject python-object)
                   (pykey key))
    (move-into-lisp
     (|%PyObject_GetItem| pyobject pykey))))

(defun (setf python-getattr) (python-value python-object python-string)
  (with-pyobjects ((pyvalue python-value)
                   (pyobject python-object)
                   (pystring python-string))
    (|%PyObject_SetAttr| pyobject pystring pyvalue)
    python-value))

(defun (setf python-getitem) (python-value python-object python-key)
  (with-pyobjects ((pyvalue python-value)
                   (pyobject python-object)
                   (pykey python-key))
    (|%PyObject_SetItem| pyobject pykey pyvalue)
    python-value))

(defun find-module (module-name)
  (move-into-lisp
   (etypecase module-name
     (string
      (with-global-interpreter-lock-held
        (|%PyImport_ImportModule| module-name)))
     (symbol
      (with-global-interpreter-lock-held
        (|%PyImport_ImportModule| (string-downcase (symbol-name module-name)))))
     (python-object
      (with-pyobjects ((pyname module-name))
        (|%PyImport_Import| pyname))))))

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

(defgeneric module-package (module))

(defgeneric module-symbol-table (module))

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

;;; Ensure that LD_LIBRARY_PATH points to the location of the currently loaded
;;; libpython.
(with-global-interpreter-lock-held
  (with-new-pyobjects ((os (|%PyImport_ImportModule| "os"))
                       (environ (|%PyObject_GetAttrString| os "environ"))
                       (pykey (pyobject-from-string "LD_LIBRARY_PATH"))
                       (pyvalue
                        (pyobject-from-string
                         (directory-namestring
                          (cffi:foreign-library-pathname
                           (cffi::get-foreign-library 'libpython))))))
    (|%PyObject_SetItem| environ pykey pyvalue)))

;;; CPython uses several hacks to speed up its load times, in a way that some
;;; built-in modules are in an inconsistent state the first time they are
;;; accessed.  This is usually not a problem, since the first access is
;;; typically just an import statement.  It is a problem for lang, though,
;;; because it immediately descends deep into each module and its children.
;;; The solution is to load all these modules right away to hide these
;;; shenanigans from the user:

(with-global-interpreter-lock-held
  (|%Py_DecRef| (|%PyImport_ImportModule| "ast"))
  (|%Py_DecRef| (|%PyImport_ImportModule| "collections.abc"))
  (|%Py_DecRef| (|%PyImport_ImportModule| "inspect"))
  (|%Py_DecRef| (|%PyImport_ImportModule| "re"))
  (|%Py_DecRef| (|%PyImport_ImportModule| "weakref")))

(defun reset-mirror-into-lisp-machinery ()
  "Cursed function - don't use."
  (let ((keep (list
               (find-class 'python:type)
               (find-class 'python:object)
               (find-class 'python:none)
               (find-class 'python:module))))
    (loop for (key . value) in (alexandria:hash-table-alist *mirror-into-lisp-table*) do
      (unless (member value keep)
        (typecase value
          (python:type
           (reinitialize-instance value :direct-superclasses '())
           (setf (find-class (class-name value))
                 nil))
          (python:module
           (delete-package (module-package value))))
        ;; Drop the table entry.
        (remhash key *mirror-into-lisp-table*)))))

(defun make-mirror-object (reference-type pyobject)
  (with-global-interpreter-lock-held
    ;; Acquire a strong reference that is released by the finalizer of the
    ;; resulting mirror object.
    (ecase reference-type
      (:strong-reference)
      (:borrowed-reference
       (|%Py_IncRef| pyobject)))
    ;; Determine the object's Python type and Lisp class.
    (let* ((pytype (|%PyObject_Type| pyobject))
           (class (if (pyobject-eq pytype pyobject)
                      (error "Encountered an object that is its own type.")
                      (move-into-lisp pytype))))
      (cond
        ;; Modules
        ((or (cffi:pointer-eq pytype *module-pyobject*)
             (|%PyObject_IsInstance| pyobject *module-pyobject*))
         (multiple-value-bind (package symbol-table)
             (pymodule-package-and-symbol-table pyobject)
           (make-instance class
             :pyobject pyobject
             :package package
             :symbol-table symbol-table)))
        ;; Types
        ((|%PyObject_IsInstance| pyobject *type-pyobject*)
         (let* ((direct-superclasses (pytype-direct-superclasses pyobject))
                (python-name
                  (with-pyobjects ((pyname (move-into-lisp (|%PyType_GetQualName| pyobject))))
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
               ;; Find or create a condition.  Conditions can't be Python
               ;; objects, so we register the mapping between the condition
               ;; class and its PyType manually.  The strong reference to the
               ;; PyType is never released, just like the condition class.
               (register-condition-class
                (or (find-class lisp-name nil)
                    (find-class
                     (eval `(define-condition ,lisp-name
                                ,(mapcar #'class-name direct-superclasses)
                              ()))))
                pyobject)
               ;; Find or create a class.
               (if (find-class lisp-name nil)
                   (error "Attempt to overwrite a mirror class: ~S"
                          lisp-name)
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
NIL if the type has no module or is not a toplevel definition of its module.

The result is a borrowed reference that is kept alive by sys.modules."
  (with-global-interpreter-lock-held
    (when (and (|%PyObject_HasAttrStringWithError| pytype "__qualname__")
               (|%PyObject_HasAttrStringWithError| pytype "__module__"))
      (with-new-pyobjects ((pytype-name (|%PyObject_GetAttrString| pytype "__qualname__"))
                           (pymodule-name (|%PyObject_GetAttrString| pytype "__module__")))
        (when (|%PyObject_IsInstance| pymodule-name *unicode-pyobject*)
          ;; PyImport_GetModule returns NULL without setting an exception
          ;; when the module isn't in sys.modules.
          (with-new-pyobjects ((pymodule (|%PyImport_GetModule| pymodule-name)))
            (cond ((not (cffi:null-pointer-p pymodule))
                   (when (and (|%PyObject_IsInstance| pymodule *module-pyobject*)
                              (|%PyObject_HasAttrWithError| pymodule pytype-name))
                     (with-new-pyobjects ((pyvalue (|%PyObject_GetAttr| pymodule pytype-name)))
                       (when (cffi:pointer-eq pytype pyvalue)
                         pymodule))))
                  ((pyerr-occurred-p)
                   (python-error-handler))
                  (t nil))))))))

(defun pymodule-name-string (pymodule)
  "Returns the name of the supplied PyModule as a Lisp string, or a made up
name if the module has no name."
  (block nil
    ;; PyModule_GetNameObject signals a SystemError if the module has no name.
    ;; We check the Python exception directly, because this function is used
    ;; while mirroring the very first modules, so the Lisp condition class
    ;; PYTHON:SYSTEM-ERROR may not exist yet.
    (handler-bind
        ((python-exception
           (lambda (condition)
             (when (with-global-interpreter-lock-held
                     (|%PyObject_IsInstance| (python-object-pyobject condition)
                                             *system-error-pyobject*))
               (return
                 (format nil "unnamed-module-~X" (random most-positive-fixnum)))))))
      (with-new-pyobjects ((pyname (|%PyModule_GetNameObject| pymodule)))
        (string-from-pyobject pyname)))))

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
    ;; PyModule_GetDict returns a borrowed reference.
    (with-new-pyobjects ((keys (|%PyDict_Keys| (|%PyModule_GetDict| pymodule))))
      (python-to-lisp-naming
       (pymodule-name-string pymodule)
       (loop for index below (|%PyList_Size| keys)
             collect
             (with-new-pyobjects ((pykey (|%PyList_GetItemRef| keys index)))
               (string-from-pyobject pykey)))))))

(defun pytype-direct-superclasses (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (with-global-interpreter-lock-held
    (let ((bases (|%PyObject_GetAttrString| pyobject "__bases__")))
      (unwind-protect
           (loop for position below (|%PyTuple_Size| bases)
                 collect
                 (mirror-into-lisp
                  (|%PyTuple_GetItem| bases position)))
        (|%Py_DecRef| bases)))))
