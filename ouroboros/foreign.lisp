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
  (aptr :pointer)
  (bptr :pointer)
  (cptr :pointer))

(cffi:defcfun ("PyErr_Restore"  pyerr-restore) :void
  (a :pointer)
  (b :pointer)
  (c :pointer))

(cffi:defcfun ("PyErr_CheckSignals" pyerr-check-signals) :void)

(define-condition python-error (serious-condition)
  ((%pyerror
    :initform (alexandria:required-argument :pyerror)
    :initarg :pyerror
    :reader python-error-pyerror))
  (:report
   (lambda (python-error stream)
     (format stream "The Python error ~S occurred!"
             (python-error-pyerror python-error)))))

(defun python-error-handler ()
  (let ((pyerror (pyerr-occurred)))
    (unless (cffi:null-pointer-p pyerror)
      (pyerr-clear)
      (error 'python-error :pyerror pyerror))))

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

(cffi:defcfun ("PyType_GetSlot" pytype-slotref) pyobject
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

;; Release the global interpreter lock if we are holding it right now.  Store
;; the Python thread in a special variable so that we can restore it later.
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
  (if *global-interpreter-lock-held*
      (funcall thunk)
      (let ((*global-interpreter-lock-held* t))
        (python-restore-thread *python-thread*)
        (unwind-protect (funcall thunk)
          (let ((thread (python-save-thread)))
            (assert (cffi:pointer-eq thread *python-thread*)))))))

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

(defconstant +python-vectorcall-arguments-offset+
  (ash 1 (1- (* 8 (cffi:foreign-type-size :size)))))

;;; Pointers and Slots

(declaim (pyobject *type-pyobject* *object-pyobject*))

(defparameter *type-pyobject*
  (cffi:foreign-symbol-pointer "PyType_Type"))

(defparameter *object-pyobject*
  (cffi:foreign-symbol-pointer "PyBaseObject_Type"))

(defparameter *none-pyobject*
  (cffi:foreign-symbol-pointer "_Py_NoneStruct"))

(defparameter *unicode-pyobject*
  (cffi:foreign-symbol-pointer "PyUnicode_Type"))

(declaim (type (integer 0 1024)
               *pyobject-type-offset*
               *pyobject-refcount-offset*
               *pytype-flags-offset*
               *pytype-call-offset*
               *pytype-vectorcall-offset-offset*
               *pytype-vectorcall-offset*))

(defparameter *pyobject-type-offset*
  ;; We know that Python's type object is its own type, so we can determine the
  ;; offset by linear search.
  (loop for offset to 1024 do
    (when (cffi:pointer-eq
           (cffi:mem-ref *type-pyobject* :pointer offset)
           *type-pyobject*)
      (return offset))
        finally
        (error "Failed to determine the type offset of Python objects."))
  "The byte offset from the start of a Python object to the slot holding its type.")

(defun pyobject-pytype (pyobject)
  "Returns the type PyObject of the supplied PyObject."
  (declare (cffi:foreign-pointer pyobject))
  (cffi:mem-ref pyobject :pointer *pyobject-type-offset*))

(unless (pyobject-eq (pyobject-pytype *object-pyobject*) *type-pyobject*)
  (error "Failed to determine the type offset of Python objects."))

(defparameter *pyobject-refcount-offset*
  ;; Acquire the GIL, because we are going to bump reference counts.
  (with-global-interpreter-lock-held
    ;; Locate the byte offset to the reference count by temporarily bumping the
    ;; reference count and checking whether it affects the current region.
    (loop for offset to 1024 do
      (let* ((increment 7)
             (pyobject (pylist-new 0))
             (before (cffi:mem-ref pyobject :size offset)))
        (loop repeat increment do (pyobject-foreign-incref pyobject))
        (let ((after (cffi:mem-ref pyobject :size offset)))
          (loop repeat (1+ increment) do (pyobject-foreign-decref pyobject))
          (when (= (+ before increment) after)
            (return offset))))
          finally
             (error "Failed to determine the refcount offset of Python objects.")))
  "The byte offset from the start of a Python object to its reference count.")

(defparameter *pytype-flags-offset*
  (with-global-interpreter-lock-held
    ;; We know some of the flag bits of certain types, and we can use this to
    ;; find the offset to where the flag bits are stored.
    (let* ((long (pylong-from-long 42))
           (tuple (pytuple-new 1))
           (list (pylist-new 1))
           (dict (pydict-new))
           (long-type (pyobject-pytype long))
           (tuple-type (pyobject-pytype tuple))
           (list-type (pyobject-pytype list))
           (dict-type (pyobject-pytype dict))
           (builtin (logior +tpflags-immutabletype+ +tpflags-basetype+))
           (primitives
             (logior +tpflags-long-subclass+
                     +tpflags-list-subclass+
                     +tpflags-tuple-subclass+
                     +tpflags-bytes-subclass+
                     +tpflags-unicode-subclass+
                     +tpflags-dict-subclass+
                     +tpflags-type-subclass+))
           (offset
             (loop for offset to 1024 do
               (flet ((probe (pytype positive negative)
                        (let ((bits (cffi:mem-ref pytype :ulong offset)))
                          (and (zerop (logandc1 bits positive))
                               (zerop (logand bits negative))))))
                 (when (and (probe *object-pyobject* builtin 0)
                            (probe *type-pyobject*
                                   (logior builtin +tpflags-type-subclass+)
                                   (logandc2 primitives +tpflags-type-subclass+))
                            (probe long-type
                                   (logior builtin +tpflags-long-subclass+)
                                   (logandc2 primitives +tpflags-long-subclass+))
                            (probe tuple-type
                                   (logior builtin +tpflags-tuple-subclass+)
                                   (logandc2 primitives +tpflags-tuple-subclass+))
                            (probe list-type
                                   (logior builtin +tpflags-list-subclass+ +tpflags-sequence+)
                                   (logandc2 primitives +tpflags-list-subclass+))
                            (probe dict-type
                                   (logior builtin +tpflags-dict-subclass+)
                                   (logandc2 primitives +tpflags-dict-subclass+)))
                   (return offset)))
                   finally
                      (error "Failed to determine the flags offset of Python objects."))))
      (pyobject-foreign-decref long)
      (pyobject-foreign-decref dict)
      (pyobject-foreign-decref tuple)
      offset))
  "The byte offset from the start of a Python object to the slot holding its
flag bits.")

(defun pytype-flags (pytype)
  (extract-tpflags
   (cffi:mem-ref pytype :ulong *pytype-flags-offset*)))

#+(or)
(defparameter *pytype-call-offset*
  (loop for offset to 1024 do
    (error "TODO"))
  "The byte offset from the start of a Python object to the slot holding its
call function, if there is any.")

#+(or)
(defparameter *pytype-vectorcall-offset-offset*
  (loop for offset to 1024 do
    (error "TODO"))
  "The byte offset from the start of a Python object to the slot holding its
vectorcall offset.")

#+(or)
(defparameter *pytype-vectorcall-offset*
  (loop for offset to 1024 do
    (error "TODO"))
  "The byte offset from the start of a Python object to the slot holding its
vectorcall function, if there is any.")

;;; Steps for making the instances of some class vectorcallable:
;;;
;;; 1. Set Py_TPFLAGS_HAVE_VECTORCALL of the tp_flags slots.
;;;
;;; 2. Set the tp_vectorcall_offset to the byte offset of the tp_vectorcall
;;; slot.
;;;
;;; 3. Set the tp_vectorcall slot to a suitable function.
;;;
;;; 4. Set the tp_call slot to PyVectorcall_Call for backward compatibility.

#+(or)
(cffi:defcallback vectorcall-into-lisp :pointer
    ((callable :pointer)
     (args :pointer)
     (nargsf :size)
     (kwnames :pointer))
  (flet ((argref (index)
           (mirror-into-lisp (cffi:mem-aref args :pointer index))))
    (let ((nargs (logandc2 nargsf +python-vectorcall-arguments-offset+))
          (function (mirror-into-lisp callable)))
      (mirror-into-python
       (if (or (cffi:null-pointer-p kwnames)
               (zerop (pytuple-size kwnames)))
           ;; Call without keyword arguments.
           (case nargs
             (0 (funcall function))
             (1 (funcall function (argref 0)))
             (2 (funcall function (argref 0) (argref 1)))
             (3 (funcall function (argref 0) (argref 1) (argref 2)))
             (4 (funcall function (argref 0) (argref 1) (argref 2) (argref 3)))
             (otherwise
              (apply function (loop for index below nargs collect (argref index)))))
           ;; Call with keyword arguments.
           (let ((args '()))
             (error "TODO")))))))
