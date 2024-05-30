(in-package #:ouroboros.internals)

(defconstant +pointer-size+
  (cffi:foreign-type-size :pointer))

(defconstant +pyobject-header-size+
  (with-global-interpreter-lock-held (pyobject-sizeof *none-pyobject*)))

(defconstant +pyobject-type-size+
  (with-global-interpreter-lock-held (pyobject-sizeof *type-pyobject*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +pyobject-refcount-immortal+
    (ecase (cffi:foreign-type-size :pointer)
      (8 #xffffffff)
      (4 #x3fffffff)))

  (defconstant +pyobject-type-offset+
    ;; We know that the type of Python's type object is itself, so we can
    ;; determine the offset by linear search.
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
    (cffi:mem-ref pyobject :pointer +pyobject-type-offset+))

  (unless (pyobject-eq (pyobject-pytype *object-pyobject*) *type-pyobject*)
    (error "Failed to determine the type offset of Python objects."))

  (defconstant +pyobject-refcount-offset+
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
    "The byte offset from the start of a Python object to its reference count."))

(defconstant +python-vectorcall-arguments-offset+
  (ash 1 (1- (* 8 (cffi:foreign-type-size :size)))))

(declaim (inline pyobject-refcount pyobject-incref pyobject-decref))

(defun pyobject-refcount (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (logand
   (cffi:mem-ref pyobject :size +pyobject-refcount-offset+)
   +pyobject-refcount-immortal+))

(defun pyobject-incref (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (symbol-macrolet ((refcount (cffi:mem-ref pyobject :size +pyobject-refcount-offset+)))
    (setf refcount (logand +pyobject-refcount-immortal+ (1+ refcount)))))

(defun pyobject-decref (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (let ((value (logand (cffi:mem-ref pyobject :size +pyobject-refcount-offset+)
                       +pyobject-refcount-immortal+)))
    (if (<= value 1)
        (pyobject-foreign-decref pyobject)
        (setf (cffi:mem-ref pyobject :size +pyobject-refcount-offset+)
              (1- value)))))

(defun pyobject-typep (pyobject pytype)
  (pytype-subtypep (pyobject-pytype pyobject) pytype))

(defun pytuple (&rest pyobjects)
  "Creates a tuple PyObject from the supplied element PyObjects."
  (let* ((size (length pyobjects))
         (tuple (pytuple-new size)))
    (loop for position below size
          for pyobject in pyobjects
          do (pyobject-incref pyobject)
          do (pytuple-setitem tuple position pyobject))
    tuple))

(defun string-from-pyobject (pyobject)
  "Returns a Lisp string with the same content as the supplied PyObject."
  (declare (pyobject pyobject))
  (cffi:with-foreign-object (size-pointer :size)
    (let* ((char-pointer
             (with-global-interpreter-lock-held
               (pyunicode-as-utf8-string pyobject size-pointer)))
           (nbytes (if (cffi:null-pointer-p char-pointer)
                       (error "Failed to convert string from Python to Lisp.")
                       (cffi:mem-ref size-pointer :size)))
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
    (cffi:with-foreign-object (errors :pointer)
      (cffi:with-foreign-object (char-pointer :uchar nbytes)
        (loop for index below nbytes do
          (setf (cffi:mem-ref char-pointer :uchar index)
                (aref octets index)))
        (with-global-interpreter-lock-held
          (let ((pyobject
                  (pyunicode-decode-utf8 char-pointer nbytes errors)))
            (unless (pyobject-typep pyobject *unicode-pyobject*)
              (error "Not a PyUnicode object: ~S" pyobject))
            pyobject))))))

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
