(in-package #:ouroboros)

(declaim (ftype function python-error-handler))

(deftype pyobject ()
  "A raw reference to a Python object."
  'cffi:foreign-pointer)

(declaim (inline pyobject-address))
(defun pyobject-address (pyobject)
  (declare (pyobject pyobject))
  (cffi:pointer-address pyobject))

(declaim (inline pyobject-eq))
(defun pyobject-eq (pyobject1 pyobject2)
  (declare (pyobject pyobject1 pyobject2))
  (cffi:pointer-eq pyobject1 pyobject2))

(cffi:define-foreign-type pyobject-type ()
  ()
  (:actual-type :pointer))

(cffi:define-parse-method pyobject ()
  (make-instance 'pyobject-type))

(defmethod cffi:expand-to-foreign (pyobject (pyobject-type pyobject-type))
  pyobject)

(defmethod cffi:expand-from-foreign (pyobject (pyobject-type pyobject-type))
  (alexandria:once-only (pyobject)
    `(progn (when (cffi:null-pointer-p ,pyobject)
              (python-error-handler))
            ,pyobject)))

