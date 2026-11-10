(in-package #:lang.internals)

(defclass python-object (funcallable-standard-object)
  ((%pyobject
    :initarg :pyobject
    :initform (alexandria:required-argument :pyobject)
    :type pyobject
    :reader python-object-pyobject))
  (:metaclass funcallable-standard-class)
  (:documentation
   "An object of the Python programming language."))

(defclass python-class (python-object funcallable-standard-class)
  ()
  (:metaclass funcallable-standard-class)
  (:documentation
   "A class of the Python programming language."))

;; Unfortunately, CLOS doesn't allow condition metaclasses or conditions that
;; inherit from non-conditions.  We still translate Python exceptions to Lisp
;; conditions for convenience, but they are now an exception (pun intended) in
;; terms of how they are mirrored from Python to Lisp.
(define-condition python-exception (serious-condition)
  ((%pyobject
    :initarg :pyobject
    :initform (alexandria:required-argument :pyobject)
    :type pyobject
    :reader python-object-pyobject))
  (:documentation
   "An exception of the Python programming language."))

(defmethod validate-superclass
    ((class python-class)
     (superclass funcallable-standard-class))
  t)

;;; Lispifying Generic Functions

(declaim (ftype (function (t &optional t) (values t &optional)) lispify))

(defclass lispifying-generic-function (standard-generic-function)
  ()
  (:metaclass funcallable-standard-class)
  (:documentation
   "A generic function class that ensures that whenever it is invoked with a mix of
Python objects and Lisp objects, the Python objects are first converted to Lisp."))

(defmethod initialize-instance :after
    ((lgf lispifying-generic-function)
     &key lambda-list
     &allow-other-keys)
  (let* ((method-class (generic-function-method-class lgf))
         (nfixed
           (or (position-if (lambda (x) (member x lambda-list-keywords)) lambda-list)
               (length lambda-list)))
         (other-args (subseq lambda-list nfixed)))
    (when other-args
      (error "Lispifying generic functions must have required arguments only."))
    (apply
     #'alexandria:map-product
     (lambda (&rest specializers)
       ;; Skip the case where all arguments are Python objects, and the case
       ;; where no arguments are Python objects.
       (unless (= 1 (length (remove-duplicates specializers)))
         (multiple-value-bind (method-lambda extra-arguments)
             (make-method-lambda
              lgf
              (class-prototype method-class)
              `(lambda ,lambda-list
                 (funcall
                  (function ,(generic-function-name lgf))
                  ,@(loop for arg in lambda-list
                          for specializer in specializers
                          collect
                          (if (eql specializer 't)
                              arg
                              `(lispify ,arg)))))
              nil)
           (add-method
            lgf
            (apply #'make-instance method-class
                   :lambda-list lambda-list
                   :function (compile nil method-lambda)
                   :specializers (mapcar #'find-class specializers)
                   :qualifiers '()
                   :documentation "Convert Python arguments into Lisp."
                   extra-arguments)))))
     (make-list nfixed :initial-element '(t python-object)))))

;;; Constants

;;; We use the __basicsize__ of each type, because that is the size its
;;; subtypes have to extend.  The size of the object itself, as returned by
;;; sys.getsizeof, differs for types, because Python's builtin type is a
;;; static type that is smaller than any heap type.

(defconstant +pyobject-header-size+
  (with-global-interpreter-lock-held
    (let ((pysize (|%PyObject_GetAttrString| *object-pyobject* "__basicsize__")))
      (prog1 (|%PyLong_AsLong| pysize)
        (|%Py_DecRef| pysize))))
  "The size of instances of Python's object type in bytes.")

(defconstant +pyobject-type-size+
  (with-global-interpreter-lock-held
    (let ((pysize (|%PyObject_GetAttrString| *type-pyobject* "__basicsize__")))
      (prog1 (|%PyLong_AsLong| pysize)
        (|%Py_DecRef| pysize))))
  "The size of heap types, the instances of Python's type, in bytes.")

;;; Touch

(declaim (notinline touch))

(defun touch (x)
  "Does nothing, but is declared notinline to keep its argument alive.

We invoke this function at the end of each block that extracts a PyObject
pointer from its Lisp wrapper, to keep the wrapper alive while manipulating the
pointer.  We know that the reference count is always at least one while the
wrapper is alive, and keeping the wrapper alive is cheaper than bumping
the PyObject's refcount."
  (declare (ignore x))
  nil)

;;; Reference Counts

(defconstant +pyobject-refcount-offset+
  ;; Acquire the GIL, because we are going to bump reference counts.
  (with-global-interpreter-lock-held
    ;; Locate the byte offset to the reference count by temporarily bumping the
    ;; reference count and checking whether it affects the current region.
    (loop for offset to 1024 do
      (let* ((increment 7)
             (pyobject (|%PyList_New| 0))
             (before (cffi:mem-ref pyobject :size offset)))
        (loop repeat increment do (|%Py_IncRef| pyobject))
        (let ((after (cffi:mem-ref pyobject :size offset)))
          (loop repeat (1+ increment) do (|%Py_DecRef| pyobject))
          (when (= (+ before increment) after)
            (return offset))))
          finally
             (error "Failed to determine the refcount offset of Python objects.")))
  "The byte offset from the start of a Python object to its reference count.")

(defconstant +pyobject-refcount-immortal+
  (ecase (cffi:foreign-type-size :pointer)
    (8 #xffffffff)
    (4 #x3fffffff)))

(declaim (inline pyobject-refcount pyobject-fast-incref pyobject-fast-decref))

(defun pyobject-refcount (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (cffi:mem-ref pyobject :size +pyobject-refcount-offset+))

(defun pyobject-fast-incref (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (symbol-macrolet ((refcount (cffi:mem-ref pyobject :size +pyobject-refcount-offset+)))
    (setf refcount (logand +pyobject-refcount-immortal+ (1+ refcount)))))

(defun pyobject-fast-decref (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (let ((value (logand (cffi:mem-ref pyobject :size +pyobject-refcount-offset+)
                       +pyobject-refcount-immortal+)))
    (if (<= value 1)
        (|%Py_DecRef| pyobject)
        (setf (cffi:mem-ref pyobject :size +pyobject-refcount-offset+)
              (1- value)))))

;;; Tuple Initialization

(defconstant +pytuple-items-offset+
  ;; Locate the byte offset to the first item of a tuple by storing a known
  ;; object in a fresh tuple and searching for it.
  (with-global-interpreter-lock-held
    (let ((pytuple (|%PyTuple_New| 1)))
      (|%Py_IncRef| *none-pyobject*)
      (|%PyTuple_SetItem| pytuple 0 *none-pyobject*)
      (prog1 (loop for offset from 0 to 64 by +pointer-size+
                   when (cffi:pointer-eq (cffi:mem-ref pytuple :pointer offset)
                                         *none-pyobject*)
                     return offset
                   finally
                      (error "Failed to determine the item offset of Python tuples."))
        (|%Py_DecRef| pytuple))))
  "The byte offset from the start of a Python tuple to its first item.")

(defun pytuple-initialize-item (pytuple position pyvalue)
  "Store PYVALUE in the still uninitialized slot POSITION of PYTUPLE, stealing the
reference.  Unlike PyTuple_SetItem, this works even when the tuple's reference
count is larger than one, which happens when initializing tuples that are
already referenced elsewhere.  Equivalent to the PyTuple_SET_ITEM macro."
  (declare (pyobject pytuple pyvalue))
  (assert (cffi:null-pointer-p
           (cffi:mem-aref (cffi:inc-pointer pytuple +pytuple-items-offset+) :pointer position)))
  (setf (cffi:mem-aref (cffi:inc-pointer pytuple +pytuple-items-offset+) :pointer position)
        pyvalue))

;;; String Conversion

(defun string-from-pyobject (pyobject)
  "Returns a Lisp string with the same content as the supplied PyObject."
  (declare (pyobject pyobject))
  (cffi:with-foreign-object (size-pointer :ssize)
    (let* ((char-pointer
             ;; The UTF-8 buffer is owned by PYOBJECT, which outlives this
             ;; function.
             (with-global-interpreter-lock-held
               (|%PyUnicode_AsUTF8AndSize| pyobject size-pointer)))
           (nbytes (cffi:mem-ref size-pointer :ssize))
           (octets (make-array nbytes :element-type '(unsigned-byte 8))))
      (loop for index below nbytes do
        (setf (aref octets index)
              (cffi:mem-aref char-pointer :uchar index)))
      (sb-ext:octets-to-string octets :external-format :utf-8))))


(defun pyobject-from-string (string)
  "Returns a PyObject with the same content as the supplied Lisp string."
  (declare (alexandria:string-designator string))
  (let* ((string (string string))
         (octets (sb-ext:string-to-octets string :external-format :utf-8))
         (nbytes (length octets)))
    (cffi:with-foreign-object (char-pointer :uchar nbytes)
      (loop for index below nbytes do
        (setf (cffi:mem-ref char-pointer :uchar index)
              (aref octets index)))
      ;; Passing NULL as the error handler name selects 'strict'.
      (with-global-interpreter-lock-held
        (|%PyUnicode_DecodeUTF8| char-pointer nbytes (cffi:null-pointer))))))

;;; Eval

(defun python-eval (string)
  "Evaluate STRING as Python code and return a new reference to the result."
  (with-global-interpreter-lock-held
    (with-new-pyobjects ((main (|%PyImport_ImportModule| "__main__"))
                         (globals (|%PyObject_GetAttrString| main "__dict__"))
                         (locals (|%PyDict_New|))
                         (code (|%Py_CompileString| string "bootstrap.lisp" +py-single-input+)))
      (|%PyEval_EvalCode| code globals locals))))
