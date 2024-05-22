(in-package #:ouroboros)

;;;  The Python Interpreter

(cffi:defcfun ("Py_Initialize" python-initialize) :void)

(cffi:defcfun ("Py_InitializeEx" python-initialize-ex) :void
  (initsigs :bool))

(cffi:defcfun ("Py_IsInitialized" python-initializedp) :bool)

(cffi:defcfun ("PyGILState_Check" python-gil-check) :bool)

(cffi:defcfun ("PyGILState_Ensure" python-gil-ensure) :int)

(cffi:defcfun ("PyGILState_Release" python-gil-release) :void
  (gilstate :int))

(cffi:defcfun ("PyEval_SaveThread" python-save-thread) :pointer)

(cffi:defcfun ("PyEval_RestoreThread" python-restore-thread) :void
  (thread :pointer))

(cffi:defcfun ("PyThreadState_Get" python-current-thread) :pointer)

;;; PyObject

(cffi:defcfun ("Py_IncRef" pyobject-foreign-incref) :void
  (pyobject pyobject))

(cffi:defcfun ("Py_DecRef" pyobject-foreign-decref) :void
  (pyobject pyobject))

(cffi:defcfun ("Py_ReprEnter" pyobject-repr-enter) :bool
  (pyobject pyobject))

(cffi:defcfun ("Py_ReprLeave" pyobject-repr-leave) :void
  (pyobject pyobject))

(cffi:defcfun ("PyObject_GetAttr" pyobject-getattr) pyobject
  (pyobject pyobject)
  (pystring pyobject))

(cffi:defcfun ("PyObject_HasAttr" pyobject-hasattr) :bool
  (pyobject pyobject)
  (pystring pyobject))

(cffi:defcfun ("PyObject_GetAttrString" pyobject-getattr-string) pyobject
  (pyobject pyobject)
  (string :string))

(cffi:defcfun ("PyObject_HasAttrString" pyobject-hasattr-string) :bool
  (pyobject pyobject)
  (string :string))

(cffi:defcfun ("PyObject_SetAttr" pyobject-setattr) :int
  (pyobject pyobject)
  (pystring pyobject)
  (pyvalue pyobject))

(cffi:defcfun ("PyObject_SetAttrString" pyobject-setattr-string) :int
  (pyobject pyobject)
  (string :string)
  (pyvalue pyobject))

(cffi:defcfun ("PyObject_Repr" pyobject-repr) pyobject
  (pyobject pyobject))

(cffi:defcfun ("PyObject_Str" pyobject-str) pyobject
  (pyobject pyobject))

(cffi:defcfun ("PyObject_ASCII" pyobject-ascii) pyobject
  (pyobject pyobject))

(cffi:defcfun ("PyObject_Bytes" pyobject-bytes) pyobject
  (pyobject pyobject))

(cffi:defcfun ("PyObject_IsTrue" pyobject-truep) :bool
  (pyobject pyobject))

(cffi:defcfun ("PyObject_Not" pyobject-not) :bool
  (pyobject pyobject))

(cffi:defcfun ("PyObject_Dir" pyobject-dir) pyobject
  (pyobject pyobject))

(cffi:defcfun ("PyObject_GetIter" pyobject-iterator) pyobject
  (pyobject pyobject))

(cffi:defcfun ("PyObject_GetAIter" pyobject-asynchronous-iterator) pyobject
  (pyobject pyobject))

;;; PyErr

(cffi:defcfun ("PyErr_Print" pyerr-print) :void)

(cffi:defcfun ("PyErr_WriteUnraisable" pyerr-write-unraisable) :void
  (pyobject pyobject))

(cffi:defcfun ("PyErr_SetNone" pyerr-set-none) :void
  (pyobject pyobject))

(cffi:defcfun ("PyErr_SetObject" pyerr-set-object) :void
  (pyobject pyobject)
  (pyvalue pyobject))

(cffi:defcfun ("PyErr_Occurred" pyerr-occurred) :pointer)

(cffi:defcfun ("PyErr_Clear" pyerr-clear) :void)

(cffi:defcfun ("PyErr_Fetch"  pyerr-fetch) :void
  (pytype (:pointer pyobject))
  (pyvalue (:pointer pyobject))
  (pytraceback (:pointer pyobject)))

(cffi:defcfun ("PyErr_Restore"  pyerr-restore) :void
  (pytype pyobject)
  (pyvalue pyobject)
  (pytraceback pyobject))

(cffi:defcfun ("PyErr_CheckSignals" pyerr-check-signals) :void)

(define-condition python-error (serious-condition)
  ((%type
    :initform (alexandria:required-argument :type)
    :initarg :type
    :reader python-error-type)
   (%value
    :initform (alexandria:required-argument :value)
    :initarg :value
    :reader python-error-value))
  (:report
   (lambda (python-error stream)
     (format stream "Received a Python exception of type ~A:~%~S"
             (string (class-name (python-error-type python-error)))
             (python-error-value python-error)))))

(defun python-error-handler ()
  (unless (cffi:null-pointer-p (pyerr-occurred))
    (cffi:with-foreign-objects ((pytype :pointer)
                                (pyvalue :pointer)
                                (pytraceback :pointer))
      (pyerr-fetch pytype pyvalue pytraceback)
      (error 'python-error
             :type (mirror-into-lisp (cffi:mem-ref pytype :pointer))
             :value (mirror-into-lisp (cffi:mem-ref pyvalue :pointer))))))

;;; PyCallable

(cffi:defcfun ("PyCallable_Check" pycallablep) :bool
  (pyobject pyobject))

(cffi:defcfun ("PyObject_CallNoArgs" pyobject-call-no-args) pyobject
  (pycallable pyobject))

(cffi:defcfun ("PyObject_CallOneArg" pyobject-call-one-arg) pyobject
  (pycallable pyobject)
  (arg pyobject))

(cffi:defcfun ("PyObject_Call" pyobject-call) pyobject
  (pycallable pyobject)
  (args pyobject)
  (kwargs pyobject))

(cffi:defcfun ("PyObject_CallObject" pyobject-call-object) pyobject
  (pycallable pyobject)
  (args pyobject))

(cffi:defcfun ("PyObject_Vectorcall" pyobject-vectorcall) pyobject
  (pycallable pyobject)
  (argvector :pointer)
  (nargsf :size)
  (kwnames pyobject))

;;; PyType

(cffi:defcfun ("PyType_FromMetaclass" pytype-from-spec) pyobject
  (pytype-spec :pointer))

(cffi:defcfun ("PyType_GetSlot" pytype-slotref) :pointer
  (pytype pyobject)
  (slotid :int))

(cffi:defcfun ("PyType_GetName" pytype-name) pyobject
  (pytype pyobject))

(cffi:defcfun ("PyType_GetQualName" pytype-qualified-name) pyobject
  (pytype pyobject))

(cffi:defcfun ("PyType_GetModule" pytype-module) pyobject
  (pytype pyobject))

(cffi:defcfun ("PyType_GetModuleName" pytype-module-name) pyobject
  (pytype pyobject))

(cffi:defcfun ("PyType_IsSubtype" pytype-subtypep) :bool
  (pytype1 pyobject)
  (pytype2 pyobject))

;;; PyModule

(cffi:defcfun ("PyModule_New" pymodule-new) pyobject
  (name :string))

;;; PyLong

(cffi:defcfun ("PyLong_FromLong" pylong-from-long) pyobject
  (long :long))

(cffi:defcfun ("PyLong_FromDouble" pylong-from-double-float) pyobject
  (double :double))

(cffi:defcfun ("PyLong_AsLong" pylong-as-long) :long
  (pyobject pyobject))

;;; PyIter

(cffi:defcfun ("PyIter_Check" pyiterp) :bool
  (pyobject pyobject))

(cffi:defcfun ("PyIter_Next" pyiter-next) pyobject
  (iterator pyobject))

(cffi:defcfun ("PyIter_Send" pyiter-send) :int
  (iterator pyobject)
  (argument pyobject)
  (result* (:pointer pyobject)))

;;; PyNumber

(cffi:defcfun ("PyNumber_Check" pynumberp) :bool
  (pyobject pyobject))

;;; PyUnicode

(cffi:defcfun ("PyUnicode_AsUTF8AndSize" pyunicode-as-utf8-string) pyobject
  (pyobject pyobject)
  (size-ptr (:pointer :size)))

(cffi:defcfun ("PyUnicode_DecodeUTF8" pyunicode-decode-utf8) pyobject
  (char-pointer :pointer)
  (size :size)
  (errors (:pointer :char)))

;;; PyTuple

(cffi:defcfun ("PyTuple_New" pytuple-new) pyobject
  (size :size))

(cffi:defcfun ("PyTuple_Size" pytuple-size) :size
  (pytuple pyobject))

(cffi:defcfun ("PyTuple_GetItem" pytuple-getitem) pyobject
  (pytuple pyobject)
  (position :size))

(cffi:defcfun ("PyTuple_SetItem" pytuple-setitem) :int
  (pytuple pyobject)
  (position :size)
  (pyvalue pyobject))

(cffi:defcfun ("PyTuple_Pack" pytuple-pack) pyobject
  (size :size) &rest)

(defun (setf pytuple-getitem) (pyvalue pytuple position)
  (pytuple-setitem pytuple position pyvalue))

;;; PyList

(cffi:defcfun ("PyList_New" pylist-new) pyobject
  (size :size))

(cffi:defcfun ("PyList_Size" pylist-size) :size
  (pylist pyobject))

(cffi:defcfun ("PyList_GetItem" pylist-getitem) pyobject
  (pylist pyobject)
  (position :size))

(cffi:defcfun ("PyList_SetItem" pylist-setitem) :int
  (pylist pyobject)
  (position :size)
  (pyvalue pyobject))

;;; PyDict

(cffi:defcfun ("PyDict_New" pydict-new) pyobject)

(cffi:defcfun ("PyDict_GetItem" pydict-getitem) pyobject
  (pydict pyobject)
  (key pyobject))

(cffi:defcfun ("PyDict_SetItem" pydict-setitem) :int
  (pydict pyobject)
  (key pyobject)
  (value pyobject))

(cffi:defcfun ("PyDict_DelItem" pydict-delitem) :int
  (pydict pyobject)
  (key pyobject))

(cffi:defcfun ("PyDict_Clear" pydict-clear) :void
  (pydict pyobject))

(cffi:defcfun ("PyDict_Keys" pydict-keys) pyobject
  (pydict pyobject))

(cffi:defcfun ("PyDict_Values" pydict-values) pyobject
  (pydict pyobject))

(cffi:defcfun ("PyDict_Items" pydict-items) pyobject
  (pydict pyobject))

(cffi:defcfun ("PyDict_Size" pydict-size) :size
  (pydict pyobject))

(cffi:defcfun ("PyDict_Copy" pydict-copy) pyobject
  (pydict pyobject))

(cffi:defcfun ("PyDict_Contains" pydict-contains) :bool
  (pydict pyobject)
  (key pyobject))

;;; PyImport

(cffi:defcfun ("PyImport_GetModule" pyimport-getmodule) pyobject
  (pyobject pyobject))

;;; Initialization

(cffi:define-foreign-library libpython
  (:unix (:or "libpython3.12.so"
              "libpython3.11.so.1"
              "libpython3.11.so"))
  (t (:or (:default "libpython3.11")
          (:default "libpython3.11"))))

;; Load Python as a shared library if its foreign symbols aren't available yet.
(unless (cffi:foreign-symbol-pointer "Py_IsInitialized")
  (cffi:use-foreign-library libpython))

;; Initialize Python if it isn't already initialized.
(unless (python-initializedp)
  (python-initialize-ex nil))

;; Determine the Python thread associated with this Lisp thread.
(defvar *python-thread*
  (if (python-gil-check)
      (python-save-thread)
      (let ((gil-state (python-gil-ensure)))
        (unwind-protect (python-current-thread)
          (python-gil-release gil-state)))))

(when (python-gil-check)
  (error "~%Failed to release the global interpreter lock."))

(defvar *global-interpreter-lock-held* nil)

(defun call-with-global-interpreter-lock-held (thunk)
  (cond
    ;; Case 1 - Lisp already holds the lock.
    (*global-interpreter-lock-held*
     (funcall thunk))
    ;; Case 2 - Lisp is called from Python code that holds the lock.
    ((python-gil-check)
     (let ((*global-interpreter-lock-held* t))
       (funcall thunk)))
    ;; Case 3 - Acquire the lock and release it when the thunk returns.
    (t
     (let ((*global-interpreter-lock-held* t))
       (python-restore-thread *python-thread*)
       (unwind-protect (funcall thunk)
         (let ((thread (python-save-thread)))
           ;; Not sure how to deal with multiple Python threads.  For now, we
           ;; assume there is only one Python thread.
           (assert (cffi:pointer-eq thread *python-thread*))))))))

(defmacro with-global-interpreter-lock-held (&body body)
  `(call-with-global-interpreter-lock-held (lambda () ,@body)))

;;; Constants (Copied from include/python3.11/cpython/*.h)
;;;
;;; Admittedly, I could use CFFI's groveler to extract this information, but
;;; I'd rather not taint this project with a C compiler dependency.

(defmacro define-tpflags (&body clauses)
  `(progn ,@(loop for (bit name) in clauses
                  collect
                  `(defconstant ,name (ash 1 ,bit)))
          (defun extract-tpflags (flags)
            (declare (type unsigned-byte flags))
            (append
             ,@(loop for (bit name) in clauses
                     collect
                     `(when (logbitp ,bit flags)
                        (list ',name)))))))

(define-tpflags
  (05 +tpflags-sequence+)
  (06 +tpflags-mapping+)
  (07 +tpflags-diallow-instantiation+)
  (08 +tpflags-immutabletype+)
  (09 +tpflags-heaptype+)
  (10 +tpflags-basetype+)
  (11 +tpflags-have-vectorcall+)
  (12 +tpflags-ready+)
  (13 +tpflags-readying+)
  (14 +tpflags-have-gc+)
  (17 +tpflags-method-descriptor+)
  (19 +tpflags-valid-version-tag+)
  (20 +tpflags-is-abstract+)
  (24 +tpflags-long-subclass+)
  (25 +tpflags-list-subclass+)
  (26 +tpflags-tuple-subclass+)
  (27 +tpflags-bytes-subclass+)
  (28 +tpflags-unicode-subclass+)
  (29 +tpflags-dict-subclass+)
  (30 +tpflags-base-exc-subclass+)
  (31 +tpflags-type-subclass+))

(defmacro define-typeslots (&body clauses)
  `(progn ,@(loop for (slotid name) in clauses
                  collect
                  `(defconstant ,name ,slotid))))

(define-typeslots
  (01 +bf-getbuffer+)
  (02 +bf-releasebuffer+)
  (03 +mp-ass-subscript+)
  (04 +mp-length+)
  (05 +mp-subscript+)
  (06 +nb-absolute+)
  (07 +nb-add+)
  (08 +nb-and+)
  (09 +nb-bool+)
  (10 +nb-divmod+)
  (11 +nb-float+)
  (12 +nb-floor-divide+)
  (13 +nb-index+)
  (14 +nb-inplace-add+)
  (15 +nb-inplace-and+)
  (16 +nb-inplace-floor-divide+)
  (17 +nb-inplace-lshift+)
  (18 +nb-inplace-multiply+)
  (19 +nb-inplace-or+)
  (20 +nb-inplace-power+)
  (21 +nb-inplace-remainder+)
  (22 +nb-inplace-rshift+)
  (23 +nb-inplace-subtract+)
  (24 +nb-inplace-true-divide+)
  (25 +nb-inplace-xor+)
  (26 +nb-int+)
  (27 +nb-invert+)
  (28 +nb-lshift+)
  (29 +nb-multiply+)
  (30 +nb-negative+)
  (31 +nb-or+)
  (32 +nb-positive+)
  (33 +nb-power+)
  (34 +nb-remainder+)
  (35 +nb-rshift+)
  (36 +nb-subtract+)
  (37 +nb-true-divide+)
  (38 +nb-xor+)
  (39 +sq-ass-item+)
  (40 +sq-concat+)
  (41 +sq-contains+)
  (42 +sq-inplace-concat+)
  (43 +sq-inplace-repeat+)
  (44 +sq-item+)
  (45 +sq-length+)
  (46 +sq-repeat+)
  (47 +tp-alloc+)
  (48 +tp-base+)
  (49 +tp-bases+)
  (50 +tp-call+)
  (51 +tp-clear+)
  (52 +tp-dealloc+)
  (53 +tp-del+)
  (54 +tp-descr-get+)
  (55 +tp-descr-set+)
  (56 +tp-doc+)
  (57 +tp-getattr+)
  (58 +tp-getattro+)
  (59 +tp-hash+)
  (60 +tp-init+)
  (61 +tp-is-gc+)
  (62 +tp-iter+)
  (63 +tp-iternext+)
  (64 +tp-methods+)
  (65 +tp-new+)
  (66 +tp-repr+)
  (67 +tp-richcompare+)
  (68 +tp-setattr+)
  (69 +tp-setattro+)
  (70 +tp-str+)
  (71 +tp-traverse+)
  (72 +tp-members+)
  (73 +tp-getset+)
  (74 +tp-free+)
  (75 +nb-matrix-multiply+)
  (76 +nb-inplace-matrix-multiply+)
  (77 +am-await+)
  (78 +am-aiter+)
  (79 +am-anext+)
  (80 +tp-finalize+)
  (81 +am-send+))

;;; Pointers

(declaim (pyobject *type-pyobject* *object-pyobject*))

(defparameter *type-pyobject*
  (cffi:foreign-symbol-pointer "PyType_Type"))

(defparameter *object-pyobject*
  (cffi:foreign-symbol-pointer "PyBaseObject_Type"))

(defparameter *none-pyobject*
  (cffi:foreign-symbol-pointer "_Py_NoneStruct"))

(defparameter *unicode-pyobject*
  (cffi:foreign-symbol-pointer "PyUnicode_Type"))
