(in-package #:lang.internals)

(declaim (ftype function python-error-handler))

(deftype pyobject ()
  "A raw reference to a Python object."
  'cffi:foreign-pointer)

(declaim (inline pyobject-address))
(defun pyobject-address (pyobject)
  (declare (pyobject pyobject))
  (cffi:pointer-address pyobject))

(declaim (inline pyobject-eq))
(defun pyobject-eq (pyobject1 pyobject2)
  (declare (pyobject pyobject1 pyobject2))
  (cffi:pointer-eq pyobject1 pyobject2))

;;; CFFI Integration

(cffi:define-foreign-type pyobject-type ()
  ()
  (:actual-type :pointer))

(cffi:define-parse-method pyobject ()
  (make-instance 'pyobject-type))

(defmethod cffi:expand-to-foreign (pyobject (pyobject-type pyobject-type))
  pyobject)

(defmethod cffi:expand-from-foreign (pyobject (pyobject-type pyobject-type))
  (alexandria:once-only (pyobject)
    `(progn (when (cffi:null-pointer-p ,pyobject)
              (python-error-handler))
            ,pyobject)))

;;; The following subtypes of PYOBJECT describe the ownership of PyObject
;;; references that cross the boundary between Lisp and Python.  They don't
;;; change how pointers are passed, but they document each function's
;;; reference semantics and let us derive the right reference counting
;;; operations automatically.
;;;
;;; - PYOBJECT is an argument that the function borrows.
;;;
;;; - STOLEN-PYOBJECT is an argument whose reference the function takes over,
;;;   even when it fails.
;;;
;;; - NEW-PYOBJECT is a result that the caller has to release eventually.
;;;
;;; - BORROWED-PYOBJECT is a result that is kept alive by another object.
;;;
;;; Returning NULL from a function with any of these result types signals the
;;; current Python exception.

(cffi:define-foreign-type stolen-pyobject-type (pyobject-type) ())

(cffi:define-parse-method stolen-pyobject ()
  (make-instance 'stolen-pyobject-type))

(cffi:define-foreign-type new-pyobject-type (pyobject-type) ())

(cffi:define-parse-method new-pyobject ()
  (make-instance 'new-pyobject-type))

(cffi:define-foreign-type borrowed-pyobject-type (pyobject-type) ())

(cffi:define-parse-method borrowed-pyobject ()
  (make-instance 'borrowed-pyobject-type))

;;; Some arguments and results may also be NULL without signifying an error.
;;; For results of these types, the caller has to check for errors itself,
;;; e.g., by calling PYERR-OCCURRED-P.

(cffi:define-foreign-type pyobject-or-null-type ()
  ()
  (:actual-type :pointer))

(defmethod cffi:expand-to-foreign (pyobject (type pyobject-or-null-type))
  pyobject)

(defmethod cffi:expand-from-foreign (pyobject (type pyobject-or-null-type))
  pyobject)

(cffi:define-parse-method pyobject-or-null ()
  (make-instance 'pyobject-or-null-type))

(cffi:define-foreign-type new-pyobject-or-null-type (pyobject-or-null-type) ())

(cffi:define-parse-method new-pyobject-or-null ()
  (make-instance 'new-pyobject-or-null-type))

;;; A pointer to memory that is not a Python object, such as the UTF-8 buffer
;;; of a Python string, where NULL signifies an error.

(cffi:define-foreign-type checked-pointer-type ()
  ()
  (:actual-type :pointer))

(cffi:define-parse-method checked-pointer ()
  (make-instance 'checked-pointer-type))

(defmethod cffi:expand-from-foreign (pointer (type checked-pointer-type))
  (alexandria:once-only (pointer)
    `(progn (when (cffi:null-pointer-p ,pointer)
              (python-error-handler))
            ,pointer)))

(defmacro define-pyobject (lisp-name c-name)
  `(progn
     (declaim (pyobject ,lisp-name))
     (defparameter ,lisp-name
       (cffi:foreign-symbol-pointer ,c-name))))

(cffi:define-foreign-type pystatus-type ()
  ()
  (:actual-type :int))

(cffi:define-parse-method pystatus ()
  (make-instance 'pystatus-type))

(defmethod cffi:expand-to-foreign (pystatus (pystatus-type pystatus-type))
  pystatus)

(defmethod cffi:expand-from-foreign (pystatus (pystatus-type pystatus-type))
  (alexandria:once-only (pystatus)
    `(progn (when (= ,pystatus -1)
              (python-error-handler))
            ,pystatus)))

;;; A C int that is either 0 (false), 1 (true), or -1 (error), such as the
;;; result of PyObject_IsTrue.

(cffi:define-foreign-type pybool-type ()
  ()
  (:actual-type :int))

(cffi:define-parse-method pybool ()
  (make-instance 'pybool-type))

(defmethod cffi:expand-from-foreign (pybool (pybool-type pybool-type))
  (alexandria:once-only (pybool)
    `(progn (when (= ,pybool -1)
              (python-error-handler))
            (= ,pybool 1))))

;;; A Py_ssize_t that signals an error when it is -1, such as the result of
;;; PyObject_Size or PyObject_Hash.

(cffi:define-foreign-type pyssize-type ()
  ()
  (:actual-type :ssize))

(cffi:define-parse-method pyssize ()
  (make-instance 'pyssize-type))

(defmethod cffi:expand-to-foreign (pyssize (pyssize-type pyssize-type))
  pyssize)

(defmethod cffi:expand-from-foreign (pyssize (pyssize-type pyssize-type))
  (alexandria:once-only (pyssize)
    `(progn (when (= ,pyssize -1)
              (python-error-handler))
            ,pyssize)))

;;; Numbers where -1 signifies an error only if a Python exception is set,
;;; such as the result of PyLong_AsLong.

(declaim (ftype (function () (values boolean &optional)) pyerr-occurred-p))

(defmacro define-checked-number-type (name actual-type minus-one)
  (let ((class (alexandria:symbolicate name '#:-type)))
    `(progn
       (cffi:define-foreign-type ,class ()
         ()
         (:actual-type ,actual-type))
       (cffi:define-parse-method ,name ()
         (make-instance ',class))
       (defmethod cffi:expand-from-foreign (value (type ,class))
         (alexandria:once-only (value)
           `(progn (when (and (= ,value ,',minus-one) (pyerr-occurred-p))
                     (python-error-handler))
                   ,value))))))

(define-checked-number-type pylong :long -1)

(define-checked-number-type pylong-long :long-long -1)

(define-checked-number-type pydouble :double -1d0)

;;; The Python C API

;; These helpers are needed at macroexpansion time.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *python-c-api-package*
    (find-package "LANG.PYTHON.C-API"))

  (defun c-api-symbol (c-name &key raw)
    "Returns the symbol that names the Lisp wrapper of the Python C API function
with the supplied name.  If RAW is true, return the name of the function that
operates on raw PyObject pointers instead."
    (intern (if raw (concatenate 'string "%" c-name) c-name)
            *python-c-api-package*))

  (defun c-api-lambda-list (argument-types)
    "Returns a list of distinct argument names derived from the supplied foreign
types."
    (let ((names (loop for type in argument-types
                       collect (string (if (consp type) (first type) type)))))
      (loop for name in names
            for index from 1
            collect (intern
                     (if (= 1 (count name names :test #'string=))
                         name
                         (format nil "~A~D" name index)))))))

(defmacro define-raw-c-api-function (c-name argument-types result-type)
  "Define a Lisp function that calls the Python C API function C-NAME, with
arguments and result converted according to the supplied foreign types.  The
name of the Lisp function is C-NAME prefixed with a percent sign, and it must
only be called while holding the global interpreter lock."
  (let ((name (c-api-symbol c-name :raw t))
        (lambda-list (c-api-lambda-list argument-types)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',name *python-c-api-package*))
       (cffi:defcfun (,c-name ,name) ,result-type
         ,@(mapcar #'list lambda-list argument-types)))))

(defmacro define-raw-c-api-functions (table)
  "Define a raw Lisp function for each row of the Python C API table, which
must be a symbol whose value is a list of entries of the form

 (C-NAME ARGUMENT-TYPES RESULT-TYPE)"
  `(progn
     ,@(loop for row in (symbol-value table)
             collect `(define-raw-c-api-function ,@row))))
