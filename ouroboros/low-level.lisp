(in-package #:ouroboros)

(defun call-with-python-error-handling (thunk)
  (unwind-protect (funcall thunk)
    (pyerr-check-signals)
    (let ((err (pyerr-occurred)))
      (unless (cffi:null-pointer-p err)
        (pyerr-write-unraisable err)))))

(defmacro with-python-error-handling (&body body)
  `(call-with-python-error-handling (lambda () ,@body)))

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
  (unless (pyobject-typep pyobject *unicode-pyobject*)
    (error "Not a PyUnicode object: ~A." pyobject))
  (cffi:with-foreign-object (size-pointer :size)
    (let* ((char-pointer
             (with-python-error-handling
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
        (let ((pyobject
                (with-python-error-handling
                  (pyunicode-decode-utf8 char-pointer nbytes errors))))
          (when (cffi:null-pointer-p pyobject)
            (error "Failed to turn ~S into a PyUnicode object." string))
          (unless (pyobject-typep pyobject *unicode-pyobject*)
            (error "Not a PyUnicode object: ~S" pyobject))
          pyobject)))))

(defun pyprint (pyobject &optional (stream t))
  "Print the string representation of the supplied PyObject."
  (let ((repr (with-python-error-handling (pyobject-repr pyobject))))
    (princ (string-from-pyobject repr) stream))
  pyobject)
