(in-package #:lang.internals)

;;; For each raw C API function |%PyFoo| that operates on PyObjects, we define
;;; a function |PyFoo| that can be called from anywhere.  It acquires the
;;; global interpreter lock if necessary, mirrors each PyObject argument into
;;; Python, and returns mirror objects instead of PyObject references.
;;;
;;; The conversion is derived from the argument and result types in the
;;; *PYTHON-C-API* table:
;;;
;;; - A PYOBJECT or STOLEN-PYOBJECT argument may be any Lisp object, which is
;;;   mirrored into Python for the duration of the call.
;;;
;;; - A PYOBJECT-OR-NULL argument may be any Lisp object, or NIL for NULL.
;;;
;;; - A NEW-PYOBJECT or BORROWED-PYOBJECT result is returned as a mirror.
;;;
;;; - A NEW-PYOBJECT-OR-NULL result is returned as two values: the mirror and
;;;   T, or NIL and NIL if the result is NULL.  We need the second value,
;;;   because the mirror of the Lisp object NIL is NIL.
;;;
;;; - All other arguments and results are passed through unchanged.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *c-api-functions-without-wrapper*
    '(;; Reference counts of mirrored objects are managed by their mirrors.
      "Py_IncRef" "Py_DecRef" "Py_NewRef"
      ;; The error indicator only means something while Python code is
      ;; running.  Python errors are signaled as Lisp conditions instead.
      "PyErr_GetRaisedException" "PyErr_SetRaisedException"
      "PyErr_SetNone" "PyErr_SetString" "PyErr_SetObject"
      "PyErr_WriteUnraisable"
      ;; The arguments are an array of raw PyObject pointers.  Use PYAPPLY.
      "PyObject_Vectorcall"
      ;; These create tuples and lists whose items are NULL, which are only
      ;; safe to use until each item has been set.  PyTuple_SetItem fails on
      ;; any tuple that is referenced by a mirror.
      "PyTuple_New" "PyTuple_SetItem" "PyList_New"
      ;; These return a pointer into the memory of their argument, which
      ;; isn't valid any longer once the global interpreter lock is released.
      "PyUnicode_AsUTF8AndSize" "PyBytes_AsString" "PyByteArray_AsString"
      ;; These store a new reference in memory supplied by the caller, so
      ;; their wrappers are defined by hand below.
      "PyDict_GetItemRef" "PyIter_Send")
    "The names of C API functions on PyObjects that have no mechanically derived
wrapper.")

  (defun pyobject-type-p (type)
    (member type '(pyobject stolen-pyobject new-pyobject borrowed-pyobject
                   pyobject-or-null new-pyobject-or-null))))

(defmacro define-c-api-function (c-name argument-types result-type)
  "Define the function |C-NAME| that calls the raw C API function |%C-NAME|
while holding the global interpreter lock, and converts PyObject arguments and
results to and from mirror objects."
  (let* ((name (c-api-symbol c-name))
         (raw-name (c-api-symbol c-name :raw t))
         (lambda-list (c-api-lambda-list argument-types))
         (stolen (loop for variable in lambda-list
                       for type in argument-types
                       when (eq type 'stolen-pyobject)
                         collect variable))
         (call `(,raw-name ,@lambda-list)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',name *python-c-api-package*))
       (defun ,name ,lambda-list
         ,(format nil "Call the Python C API function ~A on mirror objects." c-name)
         (with-global-interpreter-lock-held
           (with-new-pyobjects
               ,(loop for variable in lambda-list
                      for type in argument-types
                      when (member type '(pyobject stolen-pyobject))
                        collect `(,variable (mirror-into-python ,variable))
                      when (eq type 'pyobject-or-null)
                        collect `(,variable (if (null ,variable)
                                                (cffi:null-pointer)
                                                (mirror-into-python ,variable))))
             ;; Each stolen argument consumes a reference of its own.
             ,@(loop for variable in stolen
                     collect `(|%Py_IncRef| ,variable))
             ,(case result-type
                (new-pyobject
                 `(move-into-lisp ,call))
                (borrowed-pyobject
                 `(mirror-into-lisp ,call))
                (new-pyobject-or-null
                 `(let ((result ,call))
                    (cond ((not (cffi:null-pointer-p result))
                           (values (move-into-lisp result) t))
                          ((pyerr-occurred-p)
                           (python-error-handler))
                          (t (values nil nil)))))
                (otherwise
                 (when (pyobject-type-p result-type)
                   (error "Don't know how to wrap a C API function returning ~S."
                          result-type))
                 call))))))))

(defmacro define-c-api-functions (table)
  "Define a wrapper for each row of the Python C API table that mentions a
PyObject in its argument or result types, except for the functions listed in
*C-API-FUNCTIONS-WITHOUT-WRAPPER*."
  `(progn
     ,@(loop for (c-name argument-types result-type) in (symbol-value table)
             when (and (or (pyobject-type-p result-type)
                           (some #'pyobject-type-p argument-types))
                       (not (member c-name *c-api-functions-without-wrapper*
                                    :test #'string=)))
               collect `(define-c-api-function ,c-name ,argument-types ,result-type))))

(define-c-api-functions *python-c-api*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export (c-api-symbol "PyDict_GetItemRef") *python-c-api-package*))

(defun |PyDict_GetItemRef| (dict key)
  "Returns the value of KEY in DICT and T, or NIL and NIL if there is no such
key."
  (with-pyobjects ((pydict dict)
                   (pykey key))
    (cffi:with-foreign-object (result :pointer)
      (if (|%PyDict_GetItemRef| pydict pykey result)
          (values (move-into-lisp (cffi:mem-ref result :pointer)) t)
          (values nil nil)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export (c-api-symbol "PyIter_Send") *python-c-api-package*))

(defun |PyIter_Send| (iterator argument)
  "Send ARGUMENT to ITERATOR.  Returns the yielded value and :NEXT, or the
returned value and :RETURN."
  (with-pyobjects ((pyiterator iterator)
                   (pyargument argument))
    (cffi:with-foreign-object (result :pointer)
      (ecase (|%PyIter_Send| pyiterator pyargument result)
        (-1 (python-error-handler))
        (0 (values (move-into-lisp (cffi:mem-ref result :pointer)) :return))
        (1 (values (move-into-lisp (cffi:mem-ref result :pointer)) :next))))))
