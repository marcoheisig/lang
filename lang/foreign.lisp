(in-package #:lang-internals)

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

;;; PyBool

(define-pyobject *bool-pyobject* "PyBool_Type")

(cffi:defcfun ("PyBool_FromLong" pybool-from-long) pyobject
  (long :long))

(cffi:defcfun ("PyLong_AsLong" pybool-truep) :bool
  (pybool pyobject))

;;; PyBytearray

(define-pyobject *bytearray-pyobject* "PyByteArray_Type")

(cffi:defcfun ("PyByteArray_FromObject" pybytearray-from-object) pyobject
  (pyobject pyobject))

(cffi:defcfun ("PyByteArray_Concat" pybytearray-concat) pyobject
  (pybytearray-1 pyobject)
  (pybytearray-2 pyobject))

(cffi:defcfun ("PyByteArray_FromStringAndSize" pybytearray-from-string-and-size) pyobject
  (string :string)
  (size :size))

(cffi:defcfun ("PyByteArray_Size" pybytearray-size) :size
  (pybytearray pyobject))

(cffi:defcfun ("PyByteArray_AsString" pybytearray-as-string) :string
  (pybytearray pyobject))

(cffi:defcfun ("PyByteArray_Resize" pybytearray-resize) pystatus
  (pybytearray pyobject)
  (size :size))

;;; PyBytes

(define-pyobject *bytes-pyobject* "PyBytes_Type")

(cffi:defcfun ("PyBytes_FromStringAndSize" pybytes-from-string-and-size) pyobject
  (string :string)
  (size :size))

(cffi:defcfun ("PyBytes_FromString" pybytes-from-string) pyobject
  (string :string))

(cffi:defcfun ("PyBytes_FromObject" pybytes-from-object) pyobject
  (pyobject pyobject))

(cffi:defcfun ("PyBytes_Size" pybytes-size) :size
  (pybytes pyobject))

(cffi:defcfun ("PyBytes_AsString" pybytes-as-string) :string
  (pybytes pyobject))

(cffi:defcfun ("PyBytes_Concat" pybytes-concat) pyobject
  (pybytes-1 pyobject)
  (pybytes-2 pyobject))

;;; PyCallable

(define-pyobject *pyfunction-pyobject* "PyFunction_Type")

(define-pyobject *pycfunction-pyobject* "PyCFunction_Type")

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

(cffi:defcfun ("PyObject_CallMethod" pyobject-call-method) pyobject
  (pyobject pyobject)
  (name :string)
  (format :string)
  &rest)

(cffi:defcfun ("PyObject_Vectorcall" pyobject-vectorcall) pyobject
  (pycallable pyobject)
  (argvector :pointer)
  (nargsf :size)
  (kwnames pyobject))

(cffi:defcfun ("PyCMethod_New" pycmethod-new) pyobject
  (pymethoddef :pointer)
  (pyself pyobject)
  (pymodule pyobject)
  (pyclass pyobject))

(defconstant +pointer-size+
  (cffi:foreign-type-size :pointer))

(defconstant +python-vectorcall-arguments-offset+
  (ash 1 (1- (* 8 +pointer-size+))))

(defconstant +meth-varargs+ #x0001)

(defconstant +meth-keywords+ #x0002)

(defconstant +meth-noargs+ #x0004)

(defconstant +meth-o+ #x0008)

(defconstant +meth-class+ #x0010)

(defconstant +meth-static+ #x0020)

(defconstant +meth-coexist+ #x0040)

(defconstant +meth-fastcall+ #x0080)

(defconstant +meth-method+ #x0200)

;;; PyComplex

(define-pyobject *complex-pyobject* "PyComplex_Type")

(cffi:defcfun ("PyComplex_FromDoubles" pycomplex-from-doubles) pyobject
  (real :double)
  (imag :double))

(cffi:defcfun ("PyComplex_RealAsDouble" pycomplex-real-as-double) :double
  (pycomplex pyobject))

(cffi:defcfun ("PyComplex_ImagAsDouble" pycomplex-imag-as-double) :double
  (pycomplex pyobject))

;;; PyDict

(define-pyobject *dict-pyobject* "PyDict_Type")

(cffi:defcfun ("PyDict_New" pydict-new) pyobject)

(cffi:defcfun ("PyDict_GetItem" pydict-getitem) pyobject
  (pydict pyobject)
  (key pyobject))

(cffi:defcfun ("PyDict_SetItem" pydict-setitem) pystatus
  (pydict pyobject)
  (key pyobject)
  (value pyobject))

(cffi:defcfun ("PyDict_DelItem" pydict-delitem) pystatus
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

;;; PyErr

(cffi:defcfun ("PyErr_Print" pyerr-print) :void)

(cffi:defcfun ("PyErr_WriteUnraisable" pyerr-write-unraisable) :void
  (pyobject pyobject))

(cffi:defcfun ("PyErr_SetNone" pyerr-set-none) :void
  (pyobject pyobject))

(cffi:defcfun ("PyErr_SetString" pyerr-set-string) :void
  (pytype pyobject)
  (string :string))

(cffi:defcfun ("PyErr_SetObject" pyerr-set-object) :void
  (pytype pyobject)
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

;;; PyFLoat

(define-pyobject *float-pyobject* "PyFloat_Type")

(cffi:defcfun ("PyFloat_GetMax" pyfloat-max) :double)

(cffi:defcfun ("PyFloat_GetMin" pyfloat-min) :double)

(cffi:defcfun ("PyFloat_GetInfo" pyfloat-info) pyobject)

(cffi:defcfun ("PyFloat_FromDouble" pyfloat-from-double) pyobject
  (double :double))

(cffi:defcfun ("PyFloat_AsDouble" pyfloat-as-double) :double
  (pyfloat pyobject))

;;; PyFrozenset

(define-pyobject *frozenset-pyobject* "PyFrozenSet_Type")

;;; PyIndex

(cffi:defcfun ("PyIndex_Check" pyindexp) :bool
  (pyobject pyobject))

;;; PyIter

(cffi:defcfun ("PyIter_Check" pyiterp) :bool
  (pyobject pyobject))

(cffi:defcfun ("PyIter_Next" pyiter-next) :pointer
  (iterator pyobject))

(cffi:defcfun ("PyIter_Send" pyiter-send) :int
  (iterator pyobject)
  (argument pyobject)
  (result* (:pointer pyobject)))

;;; PyLong

(define-pyobject *long-pyobject* "PyLong_Type")

(cffi:defcfun ("PyLong_FromLong" pylong-from-long) pyobject
  (long :long))

(cffi:defcfun ("PyLong_AsLong" pylong-as-long) :long
  (pyobject pyobject))

(cffi:defcfun ("PyLong_GetInfo" pylong-info) pyobject)

;;; PyList

(define-pyobject *list-pyobject* "PyList_Type")

(cffi:defcfun ("PyList_New" pylist-new) pyobject
  (size :size))

(cffi:defcfun ("PyList_Size" pylist-size) :size
  (pylist pyobject))

(cffi:defcfun ("PyList_GetItem" pylist-getitem) pyobject
  (pylist pyobject)
  (position :size))

(cffi:defcfun ("PyList_SetItem" pylist-setitem) pystatus
  (pylist pyobject)
  (position :size)
  (pyvalue pyobject))

;;; PyMapping

(cffi:defcfun ("PyMapping_Size" pymapping-size) :size
  (pymapping pyobject))

(cffi:defcfun ("PyMapping_HasKeyString" pymapping-has-key-string) :bool
  (pymapping pyobject)
  (key :string))

(cffi:defcfun ("PyMapping_HasKey" pymapping-has-key) :bool
  (pymapping pyobject)
  (pykey pyobject))

(cffi:defcfun ("PyMapping_Keys" pymapping-keys) pyobject
  (pymapping pyobject))

(cffi:defcfun ("PyMapping_Values" pymapping-values) pyobject
  (pymapping pyobject))

(cffi:defcfun ("PyMapping_Items" pymapping-items) pyobject
  (pymapping pyobject))

;;; PyModule

(define-pyobject *module-pyobject* "PyModule_Type")

(cffi:defcfun ("PyModule_New" pymodule-new) pyobject
  (name :string))

(cffi:defcfun ("PyModule_GetDict" pymodule-dict) pyobject
  (pymodule pyobject))

(cffi:defcfun ("PyModule_GetName" pymodule-name) :string
  (pymodule pyobject))

(cffi:defcfun ("PyModule_GetFilename" pymodule-filename) :string
  (pymodule pyobject))

(cffi:defcfun ("PyModule_GetFilenameObject" pymodule-pyfilename) pyobject
  (pymodule pyobject))

(cffi:defcfun ("PyImport_Import" pyimport-import) pyobject
  (name :string))

;;; PyNone

(define-pyobject *none-pyobject* "_Py_NoneStruct")

;;; PyNumber

(cffi:defcfun ("PyNumber_Check" pynumberp) :bool
  (pyobject pyobject))

(cffi:defcfun ("PyNumber_Add" pynumber-add) pyobject
  (o1 pyobject)
  (o2 pyobject))

(cffi:defcfun ("PyNumber_Subtract" pynumber-subtract) pyobject
  (o1 pyobject)
  (o2 pyobject))

(cffi:defcfun ("PyNumber_Multiply" pynumber-multiply) pyobject
  (o1 pyobject)
  (o2 pyobject))

(cffi:defcfun ("PyNumber_MatrixMultiply" pynumber-matrix-multiply) pyobject
  (o1 pyobject)
  (o2 pyobject))

(cffi:defcfun ("PyNumber_FloorDivide" pynumber-floor-divide) pyobject
  (o1 pyobject)
  (o2 pyobject))

(cffi:defcfun ("PyNumber_TrueDivide" pynumber-true-divide) pyobject
  (o1 pyobject)
  (o2 pyobject))

(cffi:defcfun ("PyNumber_Remainder" pynumber-remainder) pyobject
  (o1 pyobject)
  (o2 pyobject))

(cffi:defcfun ("PyNumber_Divmod" pynumber-divmod) pyobject
  (o1 pyobject)
  (o2 pyobject))

(cffi:defcfun ("PyNumber_Power" pynumber-power) pyobject
  (o1 pyobject)
  (o2 pyobject))

(cffi:defcfun ("PyNumber_Negative" pynumber-negative) pyobject
  (o pyobject))

(cffi:defcfun ("PyNumber_Positive" pynumber-positive) pyobject
  (o pyobject))

(cffi:defcfun ("PyNumber_Absolute" pynumber-absolute) pyobject
  (o pyobject))

(cffi:defcfun ("PyNumber_Invert" pynumber-invert) pyobject
  (o pyobject))

(cffi:defcfun ("PyNumber_Lshift" pynumber-lshift) pyobject
  (o1 pyobject)
  (o2 pyobject))

(cffi:defcfun ("PyNumber_Rshift" pynumber-rshift) pyobject
  (o1 pyobject)
  (o2 pyobject))

(cffi:defcfun ("PyNumber_And" pynumber-and) pyobject
  (o1 pyobject)
  (o2 pyobject))

(cffi:defcfun ("PyNumber_Xor" pynumber-xor) pyobject
  (o1 pyobject)
  (o2 pyobject))

(cffi:defcfun ("PyNumber_Or" pynumber-or) pyobject
  (o1 pyobject)
  (o2 pyobject))

(cffi:defcfun ("PyNumber_Index" pynumber-index) pyobject
  (pynumber pyobject))

(cffi:defcfun ("PyNumber_Long" pynumber-long) pyobject
  (pynumber pyobject))

(cffi:defcfun ("PyNumber_Float" pynumber-float) pyobject
  (pynumber pyobject))

;;; PyObject

(define-pyobject *object-pyobject* "PyBaseObject_Type")

(cffi:defcfun ("Py_IncRef" pyobject-incref) :void
  (pyobject pyobject))

(cffi:defcfun ("Py_DecRef" pyobject-decref) :void
  (pyobject pyobject))

(cffi:defcfun ("Py_NewRef" pyobject-newref) pyobject
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

(cffi:defcfun ("PyObject_SetAttr" pyobject-setattr) pystatus
  (pyobject pyobject)
  (pystring pyobject)
  (pyvalue pyobject))

(declaim (inline (setf pyobject-getattr)))
(defun (setf pyobject-getattr) (pyvalue pyobject pystring)
  (pyobject-setattr pyobject pystring pyvalue)
  pyvalue)

(cffi:defcfun ("PyObject_SetAttrString" pyobject-setattr-string) pystatus
  (pyobject pyobject)
  (string :string)
  (pyvalue pyobject))

(declaim (inline (setf pyobject-getattr-string)))
(defun (setf pyobject-getattr-string) (pyvalue pyobject string)
  (pyobject-setattr-string pyobject string pyvalue)
  pyvalue)

(cffi:defcfun ("PyObject_Repr" pyobject-repr) pyobject
  (pyobject pyobject))

(cffi:defcfun ("PyObject_Str" pyobject-str) pyobject
  (pyobject pyobject))

(cffi:defcfun ("PyObject_ASCII" pyobject-ascii) pyobject
  (pyobject pyobject))

(cffi:defcfun ("PyObject_Bytes" pyobject-bytes) pyobject
  (pyobject pyobject))

(cffi:defcenum pycmp
  (:lt 0)
  (:le 1)
  (:eq 2)
  (:ne 3)
  (:gt 4)
  (:ge 5))

(cffi:defcfun ("PyObject_RichCompare" pyobject-richcompare) pyobject
  (pyleft pyobject)
  (pyright pyobject)
  (cmp pycmp))

(cffi:defcfun ("PyObject_Hash" pyobject-hash) :uint
  (pyobject pyobject))

(cffi:defcfun ("PyObject_IsTrue" pyobject-truep) :bool
  (pyobject pyobject))

(cffi:defcfun ("Py_IsFalse" pyobject-falsep) :bool
  (pyobject pyobject))

(cffi:defcfun ("PyObject_Not" pyobject-not) :bool
  (pyobject pyobject))

(cffi:defcfun ("PyObject_Dir" pyobject-dir) pyobject
  (pyobject pyobject))

(cffi:defcfun ("PyObject_Type" pyobject-type) :pointer
  (pyobject pyobject))

(declaim (inline pyobject-typep))
(defun pyobject-typep (pyobject pytype)
  (pytype-subtypep (pyobject-type pyobject) pytype))

(cffi:defcfun ("PyObject_Size" pyobject-size) :size
  (pyobject pyobject))

(cffi:defcfun ("_PySys_GetSizeOf" pyobject-sizeof) :size
  (pyobject pyobject))

(cffi:defcfun ("PyObject_GetItem" pyobject-getitem) pyobject
  (pyobject pyobject)
  (pykey pyobject))

(cffi:defcfun ("PyObject_SetItem" pyobject-setitem) pystatus
  (pyobject pyobject)
  (pykey pyobject)
  (pyvalue pyobject))

(defun (setf pyobject-getitem) (pyvalue pyobject pykey)
  (declare (pyobject pyvalue pyobject pykey))
  (pyobject-setitem pyobject pykey pyvalue))

(cffi:defcfun ("PyObject_DelItemString" pyobject-del-item-string) pystatus
  (pyobject pyobject)
  (key :string))

(cffi:defcfun ("PyObject_DelItem" pyobject-del-item) pystatus
  (pyobject pyobject)
  (pykey pyobject))

(cffi:defcfun ("PyObject_Format" pyobject-format) pyobject
  (pyobject pyobject)
  (format-spec pyobject))

(cffi:defcfun ("PyObject_GetIter" pyobject-iterator) pyobject
  (pyobject pyobject))

(cffi:defcfun ("PyObject_GetAIter" pyobject-asynchronous-iterator) pyobject
  (pyobject pyobject))

;;; PyRange

(define-pyobject *range-pyobject* "PyRange_Type")

;;; PySequence

(cffi:defcfun ("PySequence_Check" pysequencep) :bool
  (pyobject pyobject))

(cffi:defcfun ("PySequence_Size" pysequence-size) :size
  (pyobject pyobject))

(cffi:defcfun ("PySequence_Concat" pysequence-concat) pyobject
  (o1 pyobject)
  (o2 pyobject))

(cffi:defcfun ("PySequence_Repeat" pysequence-repeat) pyobject
  (pysequence pyobject)
  (count :size))

(cffi:defcfun ("PySequence_GetItem" pysequence-getitem) pyobject
  (pysequence pyobject)
  (index :size))

(cffi:defcfun ("PySequence_GetSlice" pysequence-getslice) pyobject
  (pysequence pyobject)
  (start :size)
  (end :size))

(cffi:defcfun ("PySequence_SetItem" pysequence-setitem) pystatus
  (pysequence pyobject)
  (index :size)
  (pyvalue pyobject))

(cffi:defcfun ("PySequence_DelItem" pysequence-delitem) pyobject
  (pysequence pyobject)
  (index :size))

(cffi:defcfun ("PySequence_SetSlice" pysequence-setslice) pystatus
  (pytarget pyobject)
  (start :size)
  (end :size)
  (pysource pyobject))

(cffi:defcfun ("PySequence_DelSlice" pysequence-delslice) pyobject
  (pytarget pyobject)
  (start :size)
  (end :size))

(cffi:defcfun ("PySequence_Tuple" pysequence-tuple) pyobject
  (pysequence pyobject))

(cffi:defcfun ("PySequence_List" pysequence-list) pyobject
  (pysequence pyobject))

(cffi:defcfun ("PySequence_Fast" pysequence-fast) pyobject
  (pysequence pyobject))

(cffi:defcfun ("PySequence_Contains" pysequence-contains) pyobject
  (pysequence pyobject)
  (pyobject pyobject))

(cffi:defcfun ("PySequence_Index" pysequence-index) :size
  (pysequence pyobject)
  (pyobject pyobject))

;;; PySet

(define-pyobject *set-pyobject* "PySet_Type")

;;; PySlice

(define-pyobject *slice-pyobject* "PySlice_Type")

;;; PyUnicode

(define-pyobject *unicode-pyobject* "PyUnicode_Type")

(cffi:defcfun ("PyUnicode_AsUTF8AndSize" pyunicode-as-utf8-string) pyobject
  (pyobject pyobject)
  (size-ptr (:pointer :size)))

(cffi:defcfun ("PyUnicode_DecodeUTF8" pyunicode-decode-utf8) pyobject
  (char-pointer :pointer)
  (size :size)
  (errors (:pointer :char)))

;;; PyTuple

(define-pyobject *tuple-pyobject* "PyTuple_Type")

(cffi:defcfun ("PyTuple_New" pytuple-new) pyobject
  (size :size))

(cffi:defcfun ("PyTuple_Size" pytuple-size) :size
  (pytuple pyobject))

(cffi:defcfun ("PyTuple_GetItem" pytuple-getitem) pyobject
  (pytuple pyobject)
  (position :size))

(cffi:defcfun ("PyTuple_SetItem" pytuple-setitem) pystatus
  (pytuple pyobject)
  (position :size)
  (pyvalue pyobject))

(cffi:defcfun ("PyTuple_Pack" pytuple-pack) pyobject
  (size :size) &rest)

(defun (setf pytuple-getitem) (pyvalue pytuple position)
  (pytuple-setitem pytuple position pyvalue))

;;; PyType

;;; The following constants are copied verbatim from cpython/typeslots.h.  We
;;; could also use CFFI's groveler to extract this information, but I'd rather
;;; not taint this project with a C compiler dependency, and those constants
;;; are part of Python's stable API.

(cffi:defcenum typeslot
  (:bf-getbuffer 01)
  (:bf-releasebuffer 02)
  (:mp-ass-subscript 03)
  (:mp-length 04)
  (:mp-subscript 05)
  (:nb-absolute 06)
  (:nb-add 07)
  (:nb-and 08)
  (:nb-bool 09)
  (:nb-divmod 10)
  (:nb-float 11)
  (:nb-floor-divide 12)
  (:nb-index 13)
  (:nb-inplace-add 14)
  (:nb-inplace-and 15)
  (:nb-inplace-floor-divide 16)
  (:nb-inplace-lshift 17)
  (:nb-inplace-multiply 18)
  (:nb-inplace-or 19)
  (:nb-inplace-power 20)
  (:nb-inplace-remainder 21)
  (:nb-inplace-rshift 22)
  (:nb-inplace-subtract 23)
  (:nb-inplace-true-divide 24)
  (:nb-inplace-xor 25)
  (:nb-int 26)
  (:nb-invert 27)
  (:nb-lshift 28)
  (:nb-multiply 29)
  (:nb-negative 30)
  (:nb-or 31)
  (:nb-positive 32)
  (:nb-power 33)
  (:nb-remainder 34)
  (:nb-rshift 35)
  (:nb-subtract 36)
  (:nb-true-divide 37)
  (:nb-xor 38)
  (:sq-ass-item 39)
  (:sq-concat 40)
  (:sq-contains 41)
  (:sq-inplace-concat 42)
  (:sq-inplace-repeat 43)
  (:sq-item 44)
  (:sq-length 45)
  (:sq-repeat 46)
  (:tp-alloc 47)
  (:tp-base 48)
  (:tp-bases 49)
  (:tp-call 50)
  (:tp-clear 51)
  (:tp-dealloc 52)
  (:tp-del 53)
  (:tp-descr-get 54)
  (:tp-descr-set 55)
  (:tp-doc 56)
  (:tp-getattr 57)
  (:tp-getattro 58)
  (:tp-hash 59)
  (:tp-init 60)
  (:tp-is-gc 61)
  (:tp-iter 62)
  (:tp-iternext 63)
  (:tp-methods 64)
  (:tp-new 65)
  (:tp-repr 66)
  (:tp-richcompare 67)
  (:tp-setattr 68)
  (:tp-setattro 69)
  (:tp-str 70)
  (:tp-traverse 71)
  (:tp-members 72)
  (:tp-getset 73)
  (:tp-free 74)
  (:nb-matrix-multiply 75)
  (:nb-inplace-matrix-multiply 76)
  (:am-await 77)
  (:am-aiter 78)
  (:am-anext 79)
  (:tp-finalize 80)
  (:am-send 81))

(cffi:defcenum tpflags
  :default
  (:managed-dict          #.(ash 1 04))
  (:sequence              #.(ash 1 05))
  (:mapping               #.(ash 1 06))
  (:diallow-instantiation #.(ash 1 07))
  (:immutabletype         #.(ash 1 08))
  (:heaptype              #.(ash 1 09))
  (:basetype              #.(ash 1 10))
  (:have-vectorcall       #.(ash 1 11))
  (:ready                 #.(ash 1 12))
  (:readying              #.(ash 1 13))
  (:have-gc               #.(ash 1 14))
  (:method-descriptor     #.(ash 1 17))
  (:valid-version-tag     #.(ash 1 19))
  (:is-abstract           #.(ash 1 20))
  (:match-self            #.(ash 1 22))
  (:long-subclass         #.(ash 1 24))
  (:list-subclass         #.(ash 1 25))
  (:tuple-subclass        #.(ash 1 26))
  (:bytes-subclass        #.(ash 1 27))
  (:unicode-subclass      #.(ash 1 28))
  (:dict-subclass         #.(ash 1 29))
  (:base-exc-subclass     #.(ash 1 30))
  (:type-subclass         #.(ash 1 31)))

(cffi:defcstruct pytype-slot
  (id :int)
  (value :pointer))

(cffi:defcstruct pytype-spec
  (name (:pointer :char))
  (basicsize :int)
  (itemsize :int)
  (flags :uint)
  (slots (:pointer (:struct pytype-slot))))

(define-pyobject *type-pyobject* "PyType_Type")

(cffi:defcfun ("PyType_GetSlot" pytype-getslot) :pointer
  (pytype pyobject)
  (typeslot typeslot))

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

(cffi:defcfun ("PyType_FromSpec" pytype-from-spec) pyobject
  (pytype-spec :pointer))

(cffi:defcfun ("PyType_FromSpecWithBases" pytype-from-spec-with-bases) pyobject
  (pytype-spec :pointer)
  (pybases pyobject))

(cffi:defcfun ("PyType_FromModuleAndSpec" pytype-from-module-and-spec) pyobject
  (pymodule pyobject)
  (pytype-spec :pointer)
  (pybases pyobject))

(cffi:defcfun ("PyType_FromMetaclass" pytype-from-metaclass) pyobject
  (pymetaclass pyobject)
  (pymodule pyobject)
  (pytype-spec :pointer)
  (pybases pyobject))

(defun make-pytype
    (name basicsize itemsize flags
     &rest slots &key &allow-other-keys)
  (declare (string name)
           (type (signed-byte 32) basicsize itemsize)
           (list flags))
  (let ((id-value-alist '())
        (flags
          (apply #'logior
                   (loop for keyword in flags
                         collect
                         (cffi:foreign-enum-value 'tpflags keyword)))))
    (loop for (keyword value) on slots by #'cddr do
      (unless (cffi:pointerp value)
        (error "Type slot values must be pointers or objects converted to pointers, got ~S."
               value))
      (push (cons (cffi:foreign-enum-value 'typeslot keyword) value)
            id-value-alist))
    (cffi:with-foreign-string (nameptr name)
      ;; Convert all the slot specifiers.
      (cffi:with-foreign-object (slots '(:struct pytype-slot) (1+ (length id-value-alist)))
        (loop for (id . value) in id-value-alist for index from 0 do
          (let ((slot (cffi:mem-aptr slots '(:struct pytype-slot) index)))
            (setf (cffi:foreign-slot-value slot '(:struct pytype-slot) 'id)
                  id)
            (setf (cffi:foreign-slot-value slot '(:struct pytype-slot) 'value)
                  value)))
        ;; Zero-terminate the array of slots.
        (let ((slot (cffi:mem-aptr slots '(:struct pytype-slot) (length id-value-alist))))
          (setf (cffi:foreign-slot-value slot '(:struct pytype-slot) 'id)
                0)
          (setf (cffi:foreign-slot-value slot '(:struct pytype-slot) 'value)
                (cffi:null-pointer)))
        ;; Populate the pytype-spec structure.
        (cffi:with-foreign-object (spec '(:struct pytype-spec))
          (setf (cffi:foreign-slot-value spec '(:struct pytype-spec) 'name)
                nameptr)
          (setf (cffi:foreign-slot-value spec '(:struct pytype-spec) 'basicsize)
                basicsize)
          (setf (cffi:foreign-slot-value spec '(:struct pytype-spec) 'itemsize)
                itemsize)
          (setf (cffi:foreign-slot-value spec '(:struct pytype-spec) 'flags)
                flags)
          (setf (cffi:foreign-slot-value spec '(:struct pytype-spec) 'slots)
                slots)
          ;; Create a new Python class.
          (with-global-interpreter-lock-held
            (pytype-from-spec spec)))))))
