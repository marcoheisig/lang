(in-package #:lang.internals)

;;; Foreign Types

(cffi:defcenum pycmp
  (:lt 0)
  (:le 1)
  (:eq 2)
  (:ne 3)
  (:gt 4)
  (:ge 5))

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

(cffi:defcstruct pymethoddef
  (name (:pointer :char))
  (meth :pointer)
  (flags :int)
  (doc (:pointer :char)))

(cffi:defcstruct pygetsetdef
  (name (:pointer :char))
  (getter :pointer)
  (setter :pointer)
  (doc (:pointer :char))
  (closure :pointer))

(cffi:defcstruct pytype-slot
  (id :int)
  (value :pointer))

(cffi:defcstruct pytype-spec
  (name (:pointer :char))
  (basicsize :int)
  (itemsize :int)
  (flags :uint)
  (slots (:pointer (:struct pytype-slot))))


;;; The Python C API
;;;
;;; Each entry describes one C API function that we use, in the form
;;;
;;;  (C-NAME ARGUMENT-TYPES RESULT-TYPE)
;;;
;;; The argument and result types describe the ownership of each PyObject
;;; reference, see pyobject.lisp.  We only list functions with simple and
;;; thread-safe semantics, e.g., PyDict_GetItemRef instead of PyDict_GetItem.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *python-c-api*
    '(;; Interpreter and Threads
      ("Py_Initialize" () :void)
      ("Py_InitializeEx" (:bool) :void)
      ("Py_IsInitialized" () :bool)
      ("PyGILState_Check" () :bool)
      ("PyGILState_Ensure" () :int)
      ("PyGILState_Release" (:int) :void)
      ("PyEval_SaveThread" () :pointer)
      ("PyEval_RestoreThread" (:pointer) :void)
      ("PyThreadState_Get" () :pointer)
      ("PyEval_EvalCode" (pyobject pyobject pyobject) new-pyobject)
      ("Py_CompileString" (:string :string :int) new-pyobject)
      ;; Reference Counting
      ("Py_IncRef" (pyobject) :void)
      ("Py_DecRef" (pyobject-or-null) :void)
      ("Py_NewRef" (pyobject) new-pyobject)
      ;; Errors and Exceptions
      ("PyErr_Clear" () :void)
      ("PyErr_Print" () :void)
      ("PyErr_GetRaisedException" () new-pyobject-or-null)
      ("PyErr_SetRaisedException" (stolen-pyobject) :void)
      ("PyErr_DisplayException" (pyobject) :void)
      ("PyErr_WriteUnraisable" (pyobject-or-null) :void)
      ("PyErr_SetNone" (pyobject) :void)
      ("PyErr_SetString" (pyobject :string) :void)
      ("PyErr_SetObject" (pyobject pyobject) :void)
      ("PyErr_CheckSignals" () pystatus)
      ("PyException_GetArgs" (pyobject) new-pyobject)
      ("PyException_GetTraceback" (pyobject) new-pyobject-or-null)
      ;; Objects
      ("PyObject_Type" (pyobject) new-pyobject)
      ("PyObject_IsInstance" (pyobject pyobject) pybool)
      ("PyObject_Repr" (pyobject) new-pyobject)
      ("PyObject_Str" (pyobject) new-pyobject)
      ("PyObject_ASCII" (pyobject) new-pyobject)
      ("PyObject_Bytes" (pyobject) new-pyobject)
      ("PyObject_Format" (pyobject pyobject-or-null) new-pyobject)
      ("PyObject_Dir" (pyobject-or-null) new-pyobject)
      ("PyObject_Hash" (pyobject) pyssize)
      ("PyObject_IsTrue" (pyobject) pybool)
      ("PyObject_Not" (pyobject) pybool)
      ("Py_IsFalse" (pyobject) :bool)
      ("PyObject_RichCompare" (pyobject pyobject pycmp) new-pyobject)
      ("PyObject_GetAttr" (pyobject pyobject) new-pyobject)
      ("PyObject_GetAttrString" (pyobject :string) new-pyobject)
      ("PyObject_HasAttrWithError" (pyobject pyobject) pybool)
      ("PyObject_HasAttrStringWithError" (pyobject :string) pybool)
      ("PyObject_SetAttr" (pyobject pyobject pyobject-or-null) pystatus)
      ("PyObject_SetAttrString" (pyobject :string pyobject-or-null) pystatus)
      ("PyObject_Size" (pyobject) pyssize)
      ("PyObject_GetItem" (pyobject pyobject) new-pyobject)
      ("PyObject_SetItem" (pyobject pyobject pyobject) pystatus)
      ("PyObject_DelItem" (pyobject pyobject) pystatus)
      ("PyObject_DelItemString" (pyobject :string) pystatus)
      ("PyObject_GetIter" (pyobject) new-pyobject)
      ("PyObject_GetAIter" (pyobject) new-pyobject)
      ("Py_ReprEnter" (pyobject) pybool)
      ("Py_ReprLeave" (pyobject) :void)
      ;; Calls
      ("PyCallable_Check" (pyobject) :bool)
      ("PyObject_CallNoArgs" (pyobject) new-pyobject)
      ("PyObject_CallOneArg" (pyobject pyobject) new-pyobject)
      ("PyObject_CallObject" (pyobject pyobject-or-null) new-pyobject)
      ("PyObject_Call" (pyobject pyobject pyobject-or-null) new-pyobject)
      ("PyObject_Vectorcall" (pyobject :pointer :size pyobject-or-null) new-pyobject)
      ("PyCMethod_New" (:pointer pyobject-or-null pyobject-or-null pyobject-or-null) new-pyobject)
      ;; Types
      ("PyType_IsSubtype" (pyobject pyobject) :bool)
      ("PyType_GetSlot" (pyobject typeslot) :pointer)
      ("PyType_GetName" (pyobject) new-pyobject)
      ("PyType_GetQualName" (pyobject) new-pyobject)
      ("PyType_GetModuleName" (pyobject) new-pyobject)
      ("PyType_GetModule" (pyobject) borrowed-pyobject)
      ("PyType_FromSpec" (:pointer) new-pyobject)
      ("PyType_FromSpecWithBases" (:pointer pyobject-or-null) new-pyobject)
      ("PyType_FromModuleAndSpec" (pyobject-or-null :pointer pyobject-or-null) new-pyobject)
      ("PyType_FromMetaclass" (pyobject-or-null pyobject-or-null :pointer pyobject-or-null) new-pyobject)
      ;; Numbers
      ("PyNumber_Check" (pyobject) :bool)
      ("PyNumber_Add" (pyobject pyobject) new-pyobject)
      ("PyNumber_Subtract" (pyobject pyobject) new-pyobject)
      ("PyNumber_Multiply" (pyobject pyobject) new-pyobject)
      ("PyNumber_MatrixMultiply" (pyobject pyobject) new-pyobject)
      ("PyNumber_FloorDivide" (pyobject pyobject) new-pyobject)
      ("PyNumber_TrueDivide" (pyobject pyobject) new-pyobject)
      ("PyNumber_Remainder" (pyobject pyobject) new-pyobject)
      ("PyNumber_Divmod" (pyobject pyobject) new-pyobject)
      ("PyNumber_Power" (pyobject pyobject pyobject) new-pyobject)
      ("PyNumber_Negative" (pyobject) new-pyobject)
      ("PyNumber_Positive" (pyobject) new-pyobject)
      ("PyNumber_Absolute" (pyobject) new-pyobject)
      ("PyNumber_Invert" (pyobject) new-pyobject)
      ("PyNumber_Lshift" (pyobject pyobject) new-pyobject)
      ("PyNumber_Rshift" (pyobject pyobject) new-pyobject)
      ("PyNumber_And" (pyobject pyobject) new-pyobject)
      ("PyNumber_Xor" (pyobject pyobject) new-pyobject)
      ("PyNumber_Or" (pyobject pyobject) new-pyobject)
      ("PyNumber_Index" (pyobject) new-pyobject)
      ("PyNumber_Long" (pyobject) new-pyobject)
      ("PyNumber_Float" (pyobject) new-pyobject)
      ("PyNumber_ToBase" (pyobject :int) new-pyobject)
      ("PyIndex_Check" (pyobject) :bool)
      ;; Booleans, Integers, Floats, and Complex Numbers
      ("PyBool_FromLong" (:long) new-pyobject)
      ("PyLong_FromLong" (:long) new-pyobject)
      ("PyLong_FromLongLong" (:long-long) new-pyobject)
      ("PyLong_FromString" (:string :pointer :int) new-pyobject)
      ("PyLong_AsLong" (pyobject) pylong)
      ("PyLong_AsLongLongAndOverflow" (pyobject (:pointer :int)) pylong-long)
      ("PyLong_GetInfo" () new-pyobject)
      ("PyFloat_FromDouble" (:double) new-pyobject)
      ("PyFloat_AsDouble" (pyobject) pydouble)
      ("PyFloat_GetMax" () :double)
      ("PyFloat_GetMin" () :double)
      ("PyFloat_GetInfo" () new-pyobject)
      ("PyComplex_FromDoubles" (:double :double) new-pyobject)
      ("PyComplex_RealAsDouble" (pyobject) pydouble)
      ("PyComplex_ImagAsDouble" (pyobject) pydouble)
      ;; Strings, Bytes, and Byte Arrays
      ("PyUnicode_DecodeUTF8" (:pointer :ssize :pointer) new-pyobject)
      ("PyUnicode_AsUTF8AndSize" (pyobject (:pointer :ssize)) checked-pointer)
      ("PyBytes_FromStringAndSize" (:pointer :ssize) new-pyobject)
      ("PyBytes_FromObject" (pyobject) new-pyobject)
      ("PyBytes_Size" (pyobject) pyssize)
      ("PyBytes_AsString" (pyobject) checked-pointer)
      ("PyByteArray_FromStringAndSize" (:pointer :ssize) new-pyobject)
      ("PyByteArray_FromObject" (pyobject) new-pyobject)
      ("PyByteArray_Concat" (pyobject pyobject) new-pyobject)
      ("PyByteArray_Size" (pyobject) pyssize)
      ("PyByteArray_AsString" (pyobject) checked-pointer)
      ("PyByteArray_Resize" (pyobject :ssize) pystatus)
      ;; Sequences
      ("PySequence_Check" (pyobject) :bool)
      ("PySequence_Size" (pyobject) pyssize)
      ("PySequence_Concat" (pyobject pyobject) new-pyobject)
      ("PySequence_Repeat" (pyobject :ssize) new-pyobject)
      ("PySequence_GetItem" (pyobject :ssize) new-pyobject)
      ("PySequence_GetSlice" (pyobject :ssize :ssize) new-pyobject)
      ("PySequence_SetItem" (pyobject :ssize pyobject) pystatus)
      ("PySequence_DelItem" (pyobject :ssize) pystatus)
      ("PySequence_SetSlice" (pyobject :ssize :ssize pyobject) pystatus)
      ("PySequence_DelSlice" (pyobject :ssize :ssize) pystatus)
      ("PySequence_Tuple" (pyobject) new-pyobject)
      ("PySequence_List" (pyobject) new-pyobject)
      ("PySequence_Contains" (pyobject pyobject) pybool)
      ("PySequence_Index" (pyobject pyobject) pyssize)
      ;; Tuples
      ("PyTuple_New" (:ssize) new-pyobject)
      ("PyTuple_Size" (pyobject) pyssize)
      ("PyTuple_GetItem" (pyobject :ssize) borrowed-pyobject)
      ("PyTuple_SetItem" (pyobject :ssize stolen-pyobject) pystatus)
      ;; Lists
      ("PyList_New" (:ssize) new-pyobject)
      ("PyList_Size" (pyobject) pyssize)
      ("PyList_GetItemRef" (pyobject :ssize) new-pyobject)
      ("PyList_SetItem" (pyobject :ssize stolen-pyobject) pystatus)
      ;; Dicts and Mappings
      ("PyDict_New" () new-pyobject)
      ("PyDict_GetItemRef" (pyobject pyobject (:pointer :pointer)) pybool)
      ("PyDict_SetItem" (pyobject pyobject pyobject) pystatus)
      ("PyDict_DelItem" (pyobject pyobject) pystatus)
      ("PyDict_Contains" (pyobject pyobject) pybool)
      ("PyDict_Clear" (pyobject) :void)
      ("PyDict_Size" (pyobject) pyssize)
      ("PyDict_Copy" (pyobject) new-pyobject)
      ("PyDict_Keys" (pyobject) new-pyobject)
      ("PyDict_Values" (pyobject) new-pyobject)
      ("PyDict_Items" (pyobject) new-pyobject)
      ("PyMapping_Size" (pyobject) pyssize)
      ("PyMapping_HasKeyWithError" (pyobject pyobject) pybool)
      ("PyMapping_HasKeyStringWithError" (pyobject :string) pybool)
      ("PyMapping_Keys" (pyobject) new-pyobject)
      ("PyMapping_Values" (pyobject) new-pyobject)
      ("PyMapping_Items" (pyobject) new-pyobject)
      ;; Iterators
      ("PyIter_Check" (pyobject) :bool)
      ;; Returns NULL without an exception when the iterator is exhausted.
      ("PyIter_Next" (pyobject) new-pyobject-or-null)
      ("PyIter_Send" (pyobject pyobject (:pointer :pointer)) :int)
      ("PySeqIter_New" (pyobject) new-pyobject)
      ;; Modules
      ("PyModule_New" (:string) new-pyobject)
      ("PyModule_GetDict" (pyobject) borrowed-pyobject)
      ("PyModule_GetNameObject" (pyobject) new-pyobject)
      ("PyModule_GetFilenameObject" (pyobject) new-pyobject)
      ("PyImport_Import" (pyobject) new-pyobject)
      ("PyImport_ImportModule" (:string) new-pyobject)
      ;; Returns NULL without an exception when the module isn't imported.
      ("PyImport_GetModule" (pyobject) new-pyobject-or-null))))

(define-raw-c-api-functions *python-c-api*)

;; PyErr_Occurred isn't part of the table, because it returns a borrowed
;; reference to the type of the current exception, or NULL if there is none,
;; which is never an error.  We only use it to check for errors.
(define-raw-c-api-function "PyErr_Occurred" () :pointer)

(defun pyerr-occurred-p ()
  (not (cffi:null-pointer-p (|%PyErr_Occurred|))))

(defconstant +py-single-input+ 256)
(defconstant +py-file-input+ 257)
(defconstant +py-eval-input+ 258)

(cffi:define-foreign-library libpython
  (:unix (:or "libpython3.14.so"
              "libpython3.13.so"))
  (t (:or (:default "libpython3.14")
          (:default "libpython3.13"))))

;; Load Python as a shared library if its foreign symbols aren't available yet.
(unless (cffi:foreign-symbol-pointer "Py_IsInitialized")
  (cffi:use-foreign-library libpython))

(let ((hexversion (cffi:mem-ref (cffi:foreign-symbol-pointer "Py_Version") :unsigned-long)))
  (when (< hexversion #x030D0000)
    (error "Lang requires Python 3.13 or newer, but this is Python ~D.~D."
           (ldb (byte 8 24) hexversion)
           (ldb (byte 8 16) hexversion))))

;; Initialize Python if it isn't already initialized.
(unless (|%Py_IsInitialized|)
  (|%Py_InitializeEx| nil))

;; If this thread holds the global interpreter lock, e.g., because it just
;; initialized Python, release it so that other threads can acquire it.  We
;; never use the returned thread state directly: The PyGILState API below
;; finds it again whenever this thread needs the lock.
(when (|%PyGILState_Check|)
  (|%PyEval_SaveThread|))

(when (|%PyGILState_Check|)
  (error "~%Failed to release the global interpreter lock."))

(defvar *global-interpreter-lock-held* nil)

(defun call-with-global-interpreter-lock-held (thunk)
  (cond
    ;; Case 1 - Lisp already holds the lock.
    (*global-interpreter-lock-held*
     (funcall thunk))
    ;; Case 2 - Lisp is called from Python code that holds the lock.
    ((|%PyGILState_Check|)
     (let ((*global-interpreter-lock-held* t))
       (funcall thunk)))
    ;; Case 3 - Acquire the lock and release it when the thunk returns.  The
    ;; PyGILState API uses a separate Python thread state for each OS thread,
    ;; creating one if necessary.  This matters because SBCL runs finalizers
    ;; in a thread of their own, and because SLIME evaluates in worker threads.
    (t
     (let ((*global-interpreter-lock-held* t))
       ;; Mask floating point traps while interacting with Python code.
       (sb-int:with-float-traps-masked (:underflow :overflow :inexact :invalid :divide-by-zero)
         (let ((gil-state (|%PyGILState_Ensure|)))
           (unwind-protect (funcall thunk)
             (|%PyGILState_Release| gil-state))))))))

(defmacro with-global-interpreter-lock-held (&body body)
  `(call-with-global-interpreter-lock-held (lambda () ,@body)))

;;; Static Python Objects

(define-pyobject *none-pyobject* "_Py_NoneStruct")
(define-pyobject *object-pyobject* "PyBaseObject_Type")
(define-pyobject *type-pyobject* "PyType_Type")
(define-pyobject *bool-pyobject* "PyBool_Type")
(define-pyobject *long-pyobject* "PyLong_Type")
(define-pyobject *float-pyobject* "PyFloat_Type")
(define-pyobject *complex-pyobject* "PyComplex_Type")
(define-pyobject *unicode-pyobject* "PyUnicode_Type")
(define-pyobject *bytes-pyobject* "PyBytes_Type")
(define-pyobject *bytearray-pyobject* "PyByteArray_Type")
(define-pyobject *tuple-pyobject* "PyTuple_Type")
(define-pyobject *list-pyobject* "PyList_Type")
(define-pyobject *dict-pyobject* "PyDict_Type")
(define-pyobject *set-pyobject* "PySet_Type")
(define-pyobject *frozenset-pyobject* "PyFrozenSet_Type")
(define-pyobject *range-pyobject* "PyRange_Type")
(define-pyobject *slice-pyobject* "PySlice_Type")
(define-pyobject *module-pyobject* "PyModule_Type")
(define-pyobject *pyfunction-pyobject* "PyFunction_Type")
(define-pyobject *pycfunction-pyobject* "PyCFunction_Type")

;; Unlike PyBaseObject_Type and friends, which are the type structs
;; themselves, PyExc_BaseException is a variable holding a pointer to the type.
(declaim (pyobject *base-exception-pyobject*))
(defparameter *base-exception-pyobject*
  (cffi:mem-ref (cffi:foreign-symbol-pointer "PyExc_BaseException") :pointer))

(declaim (pyobject *system-error-pyobject*))
(defparameter *system-error-pyobject*
  (cffi:mem-ref (cffi:foreign-symbol-pointer "PyExc_SystemError") :pointer))

;;; Auxiliary Functions

(defmacro with-new-pyobjects (bindings &body body)
  "Bind each variable to the new PyObject reference returned by its form, like
LET*, and execute BODY.  Release each reference whose form has returned when
the body exits, normally or by a non-local exit.  BODY must not return any of
these references, unless it takes a reference of its own first."
  (if (null bindings)
      `(progn ,@body)
      (destructuring-bind ((variable form) &rest more-bindings) bindings
        ;; Evaluate FORM before VARIABLE is bound, so that FORM may refer to
        ;; an outer variable of the same name.  Py_DecRef ignores NULL.
        `(let ((,variable ,form))
           (unwind-protect (with-new-pyobjects ,more-bindings ,@body)
             (|%Py_DecRef| ,variable))))))

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

;;; Python Types

(defun make-pytype
    (name basicsize itemsize flags
     &rest slots &key metaclass &allow-other-keys)
  "Create a new Python type from the supplied type slots.  If METACLASS is
supplied, it is a borrowed reference to the type of the new type."
  (declare (string name)
           (type (signed-byte 32) basicsize itemsize)
           (list flags))
  (let ((slots (alexandria:remove-from-plist slots :metaclass))
        (id-value-alist '())
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
            (if metaclass
                (|%PyType_FromMetaclass| metaclass (cffi:null-pointer) spec (cffi:null-pointer))
                (|%PyType_FromSpec| spec))))))))
