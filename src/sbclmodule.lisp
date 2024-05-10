(in-package #:sbclmodule)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric python-object-pointer (python-object))

(defgeneric python-object-id (python-object))

(defgeneric python-object-refcount (python-object))

(defgeneric python-object-attribute (python-object string))

(defgeneric (setf python-object-attribute) (value python-object string))

(defgeneric python-object-representation (python-object))

(defgeneric python-object-string (python-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Special Variables

(declaim (bordeaux-threads-2:thread *python-thread*))
(defvar *python-thread* (bordeaux-threads-2:current-thread)
  "The one Lisp thread that is allowed to mutate Python objects.")

(declaim (type (unsigned-byte 16) *pyobject-type-offset* *pyobject-refcount-offset*))
(defparameter *pyobject-type-offset*
  ;; We know that Python's type object is its own type, so we can determine the
  ;; offset by linear search.
  (loop for offset from 0 to 256 do
    (when (cffi:pointer-eq
           (cffi:mem-ref *type-pyobject* :pointer offset)
           *type-pyobject*)
      (return offset)))
  "The byte offset from the start of a Python object to the slot holding its type.")

(defparameter *pyobject-refcount-offset*
  (- *pyobject-type-offset* 8))

(declaim (bordeaux-threads:lock *lisp-from-python-lock* *python-from-lisp-lock*)) ;; TODO

(declaim (hash-table *lisp-from-python-table* *python-from-lisp-table*))

(defparameter *lisp-from-python-table*
  (make-hash-table)
  "A hash table whose keys are PyObject pointers, and whose values are the
corresponding Lisp wrappers.")

(defparameter *python-from-lisp-table*
  (make-hash-table :weakness :key)
  "A hast table whose keys are Lisp objects, and whose values are the
corresponding PyObject wrappers.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Foreign Functions

(defun pyobject-id (pyobject)
  (cffi:pointer-address pyobject))

(defun pyobject-pytype (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (cffi:mem-ref pyobject :pointer *pyobject-type-offset*))

(defun pyobject-refcount (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (cffi:mem-ref pyobject :size *pyobject-refcount-offset*))

(defun string-from-pyobject (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (cffi:with-foreign-object (size-pointer :size)
    (let* ((char-pointer (pyunicode-as-utf8-string pyobject size-pointer))
           (nbytes (if (cffi:null-pointer-p char-pointer)
                     (error "Not a Python string: ~A." (lisp-from-python pyobject))
                     (cffi:mem-ref size-pointer :size)))
           (octets (make-array nbytes :element-type '(unsigned-byte 8))))
      (loop for index below nbytes do
        (setf (aref octets index)
              (cffi:mem-aref char-pointer :uchar index)))
      (sb-ext:octets-to-string octets :external-format :utf-8))))

(defun pyobject-from-string (string)
  (declare (string string))
  (let* ((octets (sb-ext:string-to-octets string :external-format :utf-8))
         (nbytes (length octets)))
    (cffi:with-foreign-object (errors :pointer)
      (cffi:with-foreign-object (char-pointer :uchar nbytes)
        (loop for index below nbytes do
          (setf (cffi:mem-ref char-pointer :uchar index)
                (aref octets index)))
        (pyunicode-decode-utf8 char-pointer nbytes errors)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass pyobject-mixin ()
  ((%pointer
    :initarg :pointer
    :initform (alexandria:required-argument :pointer)
    :type cffi:foreign-pointer
    :reader python-object-pointer)))

(defmethod shared-initialize :after
    ((instance pyobject-mixin)
     (slot-names t)
     &key pointer &allow-other-keys)
  (alexandria:ensure-gethash
   (cffi:pointer-address pointer)
   *lisp-from-python-table*
   (register-pyobject-finalizer pointer instance)))

(defun register-pyobject-finalizer (pyobject object)
  (pyobject-incref pyobject)
  (trivial-garbage:finalize
   object
   (lambda ()
     ;; TODO ensure we are in the main thread.
     (pyobject-decref pyobject))))

(defclass python-class (pyobject-mixin standard-class)
  ())

(defmethod validate-superclass
    ((class python-class)
     (superclass standard-class))
  t)

(defclass python:type (python-class)
  ()
  (:metaclass python-class)
  (:pointer . #.*type-pyobject*))

(defmethod validate-superclass
    ((class python:type)
     (superclass standard-class))
  t)

(defclass python:object (pyobject-mixin)
  ()
  (:metaclass python:type)
  (:pointer . #.*object-pyobject*))

(defclass python:none (python:object)
  ()
  (:metaclass python:type)
  (:pointer . #.(cffi:null-pointer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod python-object-pointer ((object t))
  "Return a pointer to a Python object that mirrors the supplied Lisp object."
  (python-from-lisp object))

(defmethod python-object-id ((object t))
  "Return a pointer to a Python object that mirrors the supplied Lisp object."
  (pyobject-id
   (python-object-pointer object)))

(defmethod python-object-refcount ((object t))
  (pyobject-refcount
   (python-object-pointer object)))

(defmethod python-object-attribute (python-object string)
  (let ((pyobject (python-object-pointer python-object)))
    (lisp-from-python
     (if (stringp string)
         (pyobject-getattr-string pyobject string)
         (pyobject-getattr pyobject (python-object-pointer string))))))

(defmethod (setf python-object-attribute) (value python-object string)
  (let ((pyobject (python-object-pointer python-object))
        (pyvalue (python-object-pointer value)))
    (if (stringp string)
        (pyobject-setattr-string pyobject string pyvalue)
        (pyobject-setattr pyobject (python-object-pointer string) pyvalue))
    (values value)))

(defmethod python-object-representation ((object t))
  (string-from-pyobject
   (pyobject-representation
    (python-object-pointer object))))

(defmethod python-object-string ((object t))
  (string-from-pyobject
   (pyobject-representation
    (python-object-pointer object))))

(defmethod print-object ((python-object python:object) stream)
  (print-unreadable-object (python-object stream :type t)
    (format stream "~A {~X}"
            (python-object-string python-object)
            (python-object-id python-object))))

(defmethod print-object ((python-type python:type) stream)
  (print-unreadable-object (python-type stream :type t)
    (format stream "~A {~X}"
            (class-name python-type)
            (python-object-id python-type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mirror Object Machinery

(declaim (ftype (function (cffi:foreign-pointer)
                          (values t &optional))
                lisp-from-python))

(defun lisp-from-python (pyobject)
  "Return the Lisp object corresponding to the supplied PyObject pointer."
  (declare (cffi:foreign-pointer pyobject))
  (multiple-value-bind (value presentp)
      (gethash (cffi:pointer-address pyobject) *lisp-from-python-table*)
    (if presentp
        value
        (let* ((pytype (pyobject-pytype pyobject))
               (class (lisp-from-python pytype)))
          (if (pyobject-subtypep pytype *type-pyobject*)
              ;; Create a type.
              (let* ((name (pytype-name pyobject))
                     (direct-superclasses (pytype-direct-superclasses pyobject)))
                (make-instance class
                  :name name
                  :direct-superclasses direct-superclasses
                  :pointer pyobject))
              ;; Create an instance.
              (make-instance class
                :pointer pyobject))))))

(defun pytype-name (pytype)
  (let ((pyname (pyobject-getattr-string pytype "__name__")))
    (intern
     (if (cffi:null-pointer-p pyname)
         (format nil "UNNAMED-TYPE-~X" (random most-positive-fixnum))
         (string-upcase (string-from-pyobject pyname)))
     ;; TODO find the correct module
     "PYTHON")))

(defun pytype-direct-superclasses (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (let ((bases (pyobject-getattr-string pyobject "__bases__")))
    (if (cffi:null-pointer-p bases)
        (list (find-class 'python:object))
         (loop for position below (pytuple-size bases)
               collect
               (lisp-from-python (pytuple-getitem bases position))))))

(declaim (ftype (function (t)
                          (values cffi:foreign-pointer &optional))
                python-from-lisp))

(defun python-from-lisp (object)
  "Return the PyObject pointer corresponding to the supplied Lisp object."
  ;; Allocate
  (break "TODO")
  ;; Add finalizer
  (break "TODO")
  *type-pyobject*)
