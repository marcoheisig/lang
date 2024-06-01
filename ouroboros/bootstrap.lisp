(in-package #:ouroboros.internals)

(defgeneric mirror-into-lisp (object))

(defgeneric mirror-into-python (object))

(defclass python-object (funcallable-standard-object)
    ((%pyobject
      :initarg :pyobject
      :initform (alexandria:required-argument :pyobject)
      :type pyobject
      :reader python-object-pyobject
      :reader mirror-into-python))
    (:metaclass funcallable-standard-class)
    (:documentation
     "An object of the Python programming language."))

;;; Early Object Conversion

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
          (pyunicode-decode-utf8 char-pointer nbytes errors))))))

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

(defconstant +pyobject-header-size+
  (with-global-interpreter-lock-held (pyobject-sizeof *none-pyobject*)))

(defconstant +pyobject-type-size+
  (with-global-interpreter-lock-held (pyobject-sizeof *type-pyobject*)))

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
