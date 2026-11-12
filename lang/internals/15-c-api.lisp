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
;;;
;;; A compiler macro merges nested calls, so that the result of an inner call
;;; is passed to the outer call as a raw PyObject instead of a mirror object.
;;; E.g., (|PyNumber_Add| (|PyNumber_Multiply| a b) c) calls
;;; |%PyNumber_Multiply| and |%PyNumber_Add| and mirrors only the final result
;;; into Lisp.  All argument forms are still evaluated in order, and each raw
;;; call is still made exactly once, at the same point.  The global
;;; interpreter lock is only held during each raw call, because holding a
;;; reference to a PyObject doesn't require it.

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
                   pyobject-or-null new-pyobject-or-null)))

  (defun pyobject-argument-type-p (type)
    (member type '(pyobject stolen-pyobject pyobject-or-null)))

  (defun mirror-argument-form (variable type)
    "Returns a form that mirrors the value of VARIABLE into Python as an
argument of the supplied type."
    (if (eq type 'pyobject-or-null)
        `(if (null ,variable)
             (cffi:null-pointer)
             (mirror-into-python ,variable))
        `(mirror-into-python ,variable)))

  (defun c-api-result-form (result-type call)
    "Returns a form that converts the result of the raw CALL for its wrapper."
    (case result-type
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
       call)))

  (defvar *mergeable-c-api-functions* (make-hash-table :test #'eq)
    "A hash table mapping the name of each wrapper whose result is a new or
borrowed PyObject to its C name, argument types, and result type.")

  (defun mergeable-call-p (form env)
    "Returns true if FORM is a call to a wrapper whose raw result can be passed
to another raw C API function directly."
    (and (consp form)
         (symbolp (first form))
         (let ((row (gethash (first form) *mergeable-c-api-functions*)))
           (and row
                (= (length (rest form)) (length (second row)))
                (multiple-value-bind (kind localp declarations)
                    (sb-cltl2:function-information (first form) env)
                  (and (eq kind :function)
                       (not localp)
                       (not (eq (cdr (assoc 'inline declarations)) 'notinline))))))))

  (defun merge-c-api-calls (c-name argument-types result-type arguments env)
    "Returns a form that calls the wrapper of C-NAME on ARGUMENTS, where each
argument that is a mergeable call is replaced by the corresponding raw call."
    (let ((lisp-variables '())
          (pyobject-variables '())
          (steps '()))
      (labels ((release (variables)
                 (loop for variable in variables
                       collect `(|%Py_DecRef| ,variable)
                       collect `(setq ,variable (cffi:null-pointer))))
               ;; Push the steps that evaluate the supplied arguments, and
               ;; return the forms that mirror them, the raw call, and the
               ;; PyObject variables that it consumes.  The mirroring forms
               ;; and the call must be evaluated while holding the lock.
               (merge-call (c-name argument-types arguments)
                 (let ((call-arguments '())
                       (mirrors '())
                       (consumed '())
                       (stolen '()))
                   (loop for argument in arguments
                         for type in argument-types
                         do (cond
                              ((and (pyobject-argument-type-p type)
                                    (mergeable-call-p argument env))
                               (let ((variable (gensym "PYOBJECT")))
                                 (push variable pyobject-variables)
                                 (destructuring-bind (c-name argument-types result-type)
                                     (gethash (first argument) *mergeable-c-api-functions*)
                                   (multiple-value-bind (mirroring call consumed)
                                       (merge-call c-name argument-types (rest argument))
                                     ;; Own each result, so that it stays
                                     ;; valid without the lock.
                                     (push `(with-global-interpreter-lock-held
                                              ,@mirroring
                                              (setq ,variable
                                                    ,(if (eq result-type 'borrowed-pyobject)
                                                         `(|%Py_NewRef| ,call)
                                                         call))
                                              ,@(release consumed))
                                           steps)))
                                 (push variable call-arguments)
                                 (push variable consumed)
                                 (when (eq type 'stolen-pyobject)
                                   (push variable stolen))))
                              (t
                               (let ((variable (gensym "ARGUMENT")))
                                 (push variable lisp-variables)
                                 (push `(setq ,variable ,argument) steps)
                                 (if (not (pyobject-argument-type-p type))
                                     (push variable call-arguments)
                                     (let ((pyobject (gensym "PYOBJECT")))
                                       (push pyobject pyobject-variables)
                                       (push (list pyobject variable type) mirrors)
                                       (push pyobject call-arguments)
                                       (push pyobject consumed)
                                       (when (eq type 'stolen-pyobject)
                                         (push pyobject stolen))))))))
                   (let ((call `(,(c-api-symbol c-name :raw t) ,@(reverse call-arguments))))
                     (values
                      ;; Like the wrapper, mirror the arguments only after
                      ;; all of them have been evaluated.
                      (loop for (pyobject variable type) in (reverse mirrors)
                            collect `(setq ,pyobject ,(mirror-argument-form variable type)))
                      (if (null stolen)
                          call
                          `(progn
                             ;; Each stolen argument consumes a reference of
                             ;; its own.
                             ,@(loop for variable in stolen
                                     collect `(|%Py_IncRef| ,variable))
                             ,call))
                      consumed)))))
        (multiple-value-bind (mirroring call consumed)
            (merge-call c-name argument-types arguments)
          `(let (,@lisp-variables
                 ,@(loop for variable in pyobject-variables
                         collect `(,variable (cffi:null-pointer))))
             (unwind-protect
                  (progn
                    ,@(reverse steps)
                    (with-global-interpreter-lock-held
                      ,@mirroring
                      (multiple-value-prog1 ,(c-api-result-form result-type call)
                        ,@(release consumed))))
               ;; Release the PyObjects that weren't consumed due to a
               ;; non-local exit.
               (unless (and ,@(loop for variable in pyobject-variables
                                    collect `(cffi:null-pointer-p ,variable)))
                 (with-global-interpreter-lock-held
                   ,@(loop for variable in pyobject-variables
                           collect `(|%Py_DecRef| ,variable)))))))))))

(defmacro define-c-api-function (c-name argument-types result-type)
  "Define the function |C-NAME| that calls the raw C API function |%C-NAME|
while holding the global interpreter lock, and converts PyObject arguments and
results to and from mirror objects.  Also define a compiler macro that merges
nested calls."
  (let* ((name (c-api-symbol c-name))
         (raw-name (c-api-symbol c-name :raw t))
         (lambda-list (c-api-lambda-list argument-types))
         (stolen (loop for variable in lambda-list
                       for type in argument-types
                       when (eq type 'stolen-pyobject)
                         collect variable)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',name *python-c-api-package*)
         ,@(when (member result-type '(new-pyobject borrowed-pyobject))
             `((setf (gethash ',name *mergeable-c-api-functions*)
                     '(,c-name ,argument-types ,result-type)))))
       (defun ,name ,lambda-list
         ,(format nil "Call the Python C API function ~A on mirror objects." c-name)
         (with-global-interpreter-lock-held
           (with-new-pyobjects
               ,(loop for variable in lambda-list
                      for type in argument-types
                      when (pyobject-argument-type-p type)
                        collect `(,variable ,(mirror-argument-form variable type)))
             ;; Each stolen argument consumes a reference of its own.
             ,@(loop for variable in stolen
                     collect `(|%Py_IncRef| ,variable))
             ,(c-api-result-form result-type `(,raw-name ,@lambda-list)))))
       (define-compiler-macro ,name (&whole whole &rest arguments &environment env)
         (if (and (= (length arguments) ,(length argument-types))
                  (some (lambda (argument type)
                          (and (pyobject-argument-type-p type)
                               (mergeable-call-p argument env)))
                        arguments
                        ',argument-types))
             (merge-c-api-calls ,c-name ',argument-types ',result-type arguments env)
             whole)))))

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
