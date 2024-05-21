(in-package #:ouroboros)

(declaim (inline pyobject-refcount pyobject-incref pyobject-decref))

(defun pyobject-refcount (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (cffi:mem-ref pyobject :size *pyobject-refcount-offset*))

(defun pyobject-incref (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (with-global-interpreter-lock-held
    (incf (cffi:mem-ref pyobject :size *pyobject-refcount-offset*))))

(defun pyobject-decref (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (with-global-interpreter-lock-held
    (let ((value (cffi:mem-ref pyobject :size *pyobject-refcount-offset*)))
      (if (= 1 value)
          (pyobject-foreign-decref pyobject)
          (setf (cffi:mem-ref pyobject :size *pyobject-refcount-offset*)
                (1- value))))))

(defun pyobject-typep (pyobject pytype)
  (with-global-interpreter-lock-held
    (pytype-subtypep (pyobject-pytype pyobject) pytype)))

(defun pytuple (&rest pyobjects)
  "Creates a tuple PyObject from the supplied element PyObjects."
  (with-global-interpreter-lock-held
    (let* ((size (length pyobjects))
           (tuple (pytuple-new size)))
      (loop for position below size
            for pyobject in pyobjects
            do (pyobject-incref pyobject)
            do (pytuple-setitem tuple position pyobject))
      tuple)))

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

(defun pyprint (pyobject &optional (stream t))
  "Print the string representation of the supplied PyObject."
  (with-global-interpreter-lock-held
    (let ((repr (pyobject-repr pyobject)))
      (princ (string-from-pyobject repr) stream)
      (pyobject-decref repr)))
  pyobject)

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
