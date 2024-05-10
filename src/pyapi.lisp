(in-package #:sbclmodule)

(cffi:defcfun ("Py_Initialize" python-initialize) :void)

(cffi:defcfun ("Py_IsInitialized" python-initializedp) :bool)

(cffi:defcfun ("Py_IncRef" pyobject-incref) :void
  (pyobject :pointer))

(cffi:defcfun ("Py_DecRef" pyobject-decref) :void
  (pyobject :pointer))

(cffi:defcfun ("PyObject_GetAttr" pyobject-getattr) :pointer
  (pyobject :pointer)
  (pystring :pointer))

(cffi:defcfun ("PyObject_GetAttrString" pyobject-getattr-string) :pointer
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

(cffi:defcfun ("PyObject_Repr" pyobject-representation) :pointer
  (pyobject :pointer))

(cffi:defcfun ("PyObject_Str" pyobject-string) :pointer
  (pyobject :pointer))

(cffi:defcfun ("PyType_IsSubtype" pyobject-subtypep) :bool
  (pyobject :pointer)
  (pytype :pointer))

(cffi:defcfun ("PyUnicode_AsUTF8AndSize" pyunicode-as-utf8-string) :pointer
  (pyobject :pointer)
  (size-ptr (:pointer :size)))

(cffi:defcfun ("PyUnicode_DecodeUTF8" pyunicode-decode-utf8) :pointer
  (char-pointer :pointer)
  (size :size)
  (errors :pointer))

(cffi:defcfun ("PyTuple_Size" pytuple-size) :size
  (pytuple :pointer))

(cffi:defcfun ("PyTuple_GetItem" pytuple-getitem) :pointer
  (pytuple :pointer)
  (position :size))

(cffi:defcfun ("PyImport_GetModule" pyimport-getmodule) :pointer
  (pyobject :pointer))

(defun initialize-python ()
  (cffi:load-foreign-library "libpython3.10.so")
  (unless (python-initializedp)
    (python-initialize)))

(initialize-python)

(declaim (cffi:foreign-pointer *type-pyobject* *object-pyobject*))

(defparameter *type-pyobject*
  (cffi:foreign-symbol-pointer "PyType_Type"))

(defparameter *object-pyobject*
  (cffi:foreign-symbol-pointer "PyBaseObject_Type"))
