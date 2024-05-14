(in-package #:ouroboros)

;;; Py

(cffi:defcfun ("Py_Initialize" python-initialize) :void)

(cffi:defcfun ("Py_InitializeEx" python-initialize-ex) :void
  (initsigs :bool))

(cffi:defcfun ("Py_IsInitialized" python-initializedp) :bool)

(cffi:defcfun ("Py_IncRef" pyobject-incref) :void
  (pyobject :pointer))

(cffi:defcfun ("Py_DecRef" pyobject-decref) :void
  (pyobject :pointer))

(cffi:defcfun ("Py_ReprEnter" pyobject-repr-enter) :bool
  (pyobject :pointer))

(cffi:defcfun ("Py_ReprLeave" pyobject-repr-leave) :void
  (pyobject :pointer))

;;; PyObject

(cffi:defcfun ("PyObject_GetAttr" pyobject-getattr) :pointer
  (pyobject :pointer)
  (pystring :pointer))

(cffi:defcfun ("PyObject_HasAttr" pyobject-hasattr) :bool
  (pyobject :pointer)
  (pystring :pointer))

(cffi:defcfun ("PyObject_GetAttrString" pyobject-getattr-string) :pointer
  (pyobject :pointer)
  (string :string))

(cffi:defcfun ("PyObject_HasAttrString" pyobject-hasattr-string) :bool
  (pyobject :pointer)
  (string :string))

(cffi:defcfun ("PyObject_SetAttr" pyobject-setattr) :int
  (pyobject :pointer)
  (pystring :pointer)
  (pyvalue :pointer))

(cffi:defcfun ("PyObject_SetAttrString" pyobject-setattr-string) :int
  (pyobject :pointer)
  (string :string)
  (pyvalue :pointer))

(cffi:defcfun ("PyObject_Repr" pyobject-repr) :pointer
  (pyobject :pointer))

(cffi:defcfun ("PyObject_Str" pyobject-str) :pointer
  (pyobject :pointer))

(cffi:defcfun ("PyObject_ASCII" pyobject-ascii) :pointer
  (pyobject :pointer))

(cffi:defcfun ("PyObject_Bytes" pyobject-bytes) :pointer
  (pyobject :pointer))

(cffi:defcfun ("PyObject_IsTrue" pyobject-truep) :bool
  (pyobject :pointer))

(cffi:defcfun ("PyObject_Dir" pyobject-dir) :pointer
  (pyobject :pointer))

(cffi:defcfun ("PyObject_GetIter" pyobject-iterator) :pointer
  (pyobject :pointer))

(cffi:defcfun ("PyObject_GetAIter" pyobject-asynchronous-iterator) :pointer
  (pyobject :pointer))

;;; PyErr

(cffi:defcfun ("PyErr_Print" pyerr-print) :void)

(cffi:defcfun ("PyErr_WriteUnraisable" pyerr-write-unraisable) :void
  (pyobject :pointer))

(cffi:defcfun ("PyErr_SetNone" pyerr-set-none) :void
  (pyobject :pointer))

(cffi:defcfun ("PyErr_SetObject" pyerr-set-object) :void
  (pyobject :pointer)
  (pyvalue :pointer))

(cffi:defcfun ("PyErr_Occurred" pyerr-occurred) :pointer)

(cffi:defcfun ("PyErr_Clear" pyerr-clear) :void)

(cffi:defcfun ("PyErr_Fetch"  pyerr-fetch) :void
  (aptr :pointer)
  (bptr :pointer)
  (cptr :pointer))

(cffi:defcfun ("PyErr_Restore"  pyerr-restore) :void
  (a :pointer)
  (b :pointer)
  (c :pointer))

(cffi:defcfun ("PyErr_CheckSignals" pyerr-check-signals) :void)

;;; PyCallable

(cffi:defcfun ("PyCallable_Check" pycallablep) :bool
  (pyobject :pointer))

(cffi:defcfun ("PyObject_CallNoArgs" pyobject-call-no-args) :pointer
  (pycallable :pointer))

(cffi:defcfun ("PyObject_CallOneArg" pyobject-call-one-arg) :pointer
  (pycallable :pointer)
  (arg :pointer))

(cffi:defcfun ("PyObject_Call" pyobject-call) :pointer
  (pycallable :pointer)
  (args :pointer)
  (kwargs :pointer))

(cffi:defcfun ("PyObject_CallObject" pyobject-call-object) :pointer
  (pycallable :pointer)
  (args :pointer))

(cffi:defcfun ("PyObject_Vectorcall" pyobject-vectorcall) :pointer
  (pycallable :pointer)
  (argvector :pointer)
  (nargsf :size)
  (kwnames :pointer))

;;; PyType

(cffi:defcfun ("PyType_GetName" pytype-name) :pointer
  (pytype :pointer))

(cffi:defcfun ("PyType_GetQualName" pytype-qualified-name) :pointer
  (pytype :pointer))

(cffi:defcfun ("PyType_GetModuleName" pytype-module-name) :pointer
  (pytype :pointer))

(cffi:defcfun ("PyType_IsSubtype" pytype-subtypep) :bool
  (pytype1 :pointer)
  (pytype2 :pointer))

;;; PyLong

(cffi:defcfun ("PyLong_FromLong" pylong-from-long) :pointer
  (long :long))

(cffi:defcfun ("PyLong_FromDouble" pylong-from-double-float) :pointer
  (double :double))

;;; PyIter

(cffi:defcfun ("PyIter_Check" pyiterp) :bool
  (pyobject :pointer))

(cffi:defcfun ("PyIter_Next" pyiter-next) :pointer
  (iterator :pointer))

(cffi:defcfun ("PyIter_Send" pyiter-send) :int
  (iterator :pointer)
  (argument :pointer)
  (result* :pointer))

;;; PyNumber

(cffi:defcfun ("PyNumber_Check" pynumberp) :bool
  (pyobject :pointer))

;;; PyUnicode

(cffi:defcfun ("PyUnicode_AsUTF8AndSize" pyunicode-as-utf8-string) :pointer
  (pyobject :pointer)
  (size-ptr (:pointer :size)))

(cffi:defcfun ("PyUnicode_DecodeUTF8" pyunicode-decode-utf8) :pointer
  (char-pointer :pointer)
  (size :size)
  (errors :pointer))

;;; PyTuple

(cffi:defcfun ("PyTuple_New" pytuple-new) :pointer
  (size :size))

(cffi:defcfun ("PyTuple_Size" pytuple-size) :size
  (pytuple :pointer))

(cffi:defcfun ("PyTuple_GetItem" pytuple-getitem) :pointer
  (pytuple :pointer)
  (position :size))

(cffi:defcfun ("PyTuple_SetItem" pytuple-setitem) :pointer
  (pytuple :pointer)
  (position :size)
  (pyvalue :pointer))

(cffi:defcfun ("PyTuple_Pack" pytuple-pack) :pointer
  (size :size) &rest)

;;; PyDict

(cffi:defcfun ("PyDict_New" pydict-new) :pointer)

(cffi:defcfun ("PyDict_GetItem" pydict-getitem) :pointer
  (pydict :pointer)
  (key :pointer))

(cffi:defcfun ("PyDict_SetItem" pydict-setitem) :pointer
  (pydict :pointer)
  (key :pointer)
  (value :pointer))

(cffi:defcfun ("PyDict_DelItem" pydict-delitem) :pointer
  (pydict :pointer)
  (key :pointer))

(cffi:defcfun ("PyDict_Clear" pydict-clear) :void
  (pydict :pointer))

(cffi:defcfun ("PyDict_Keys" pydict-keys) :pointer
  (pydict :pointer))

(cffi:defcfun ("PyDict_Values" pydict-values) :pointer
  (pydict :pointer))

(cffi:defcfun ("PyDict_Values" pydict-items) :pointer
  (pydict :pointer))

(cffi:defcfun ("PyDict_Size" pydict-size) :size
  (pydict :pointer))

(cffi:defcfun ("PyDict_Copy" pydict-copy) :pointer
  (pydict :pointer))

(cffi:defcfun ("PyDict_Contains" pydict-contains) :bool
  (pydict :pointer)
  (key :pointer))

;;; PyImport

(cffi:defcfun ("PyImport_GetModule" pyimport-getmodule) :pointer
  (pyobject :pointer))

;;; Initialization

(cffi:define-foreign-library libpython
  (:unix (:or "libpython3.12.so"
              "libpython3.11.so.1"
              "libpython3.11.so"))
  (t (:or (:default "libpython3.11")
          (:default "libpython3.11"))))

(unless (cffi:foreign-symbol-pointer "Py_IsInitialized")
  (cffi:use-foreign-library libpython))

(unless (python-initializedp)
  (python-initialize-ex nil))

;;; Pointers and Slots

(deftype pyobject ()
  "A raw reference to a Python object."
  'cffi:foreign-pointer)

(defun pyobject-address (pyobject)
  (cffi:pointer-address pyobject))

(declaim (pyobject *type-pyobject* *object-pyobject*))

(defparameter *type-pyobject*
  (cffi:foreign-symbol-pointer "PyType_Type"))

(defparameter *object-pyobject*
  (cffi:foreign-symbol-pointer "PyBaseObject_Type"))

(defparameter *unicode-pyobject*
  (cffi:foreign-symbol-pointer "PyUnicode_Type"))

(declaim (type (integer 0 256) *pyobject-type-offset* *pyobject-refcount-offset*))

(defparameter *pyobject-type-offset*
  ;; We know that Python's type object is its own type, so we can determine the
  ;; offset by linear search.
  (loop for offset from 0 to 256 do
    (when (cffi:pointer-eq
           (cffi:mem-ref *type-pyobject* :pointer offset)
           *type-pyobject*)
      (return offset)))
  "The byte offset from the start of a Python object to the slot holding its type.")

(defun pyobject-pytype (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (cffi:mem-ref pyobject :pointer *pyobject-type-offset*))

(unless (cffi:pointer-eq (pyobject-pytype *object-pyobject*) *type-pyobject*)
  (error "Failed to determine the type offset of Python objects."))

(defparameter *pyobject-refcount-offset*
  (- *pyobject-type-offset* 8)
  "The byte offset from the start of a Python object to its reference count.")

(defun pyobject-refcount (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (cffi:mem-ref pyobject :size *pyobject-refcount-offset*))
