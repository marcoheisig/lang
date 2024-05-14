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
;;; Low-level Functions

(defun pystr (string-designator)
  (etypecase string-designator
    (python:object
     (python-object-pointer string-designator))
    (string
     (pyobject-from-string string-designator))
    (symbol
     (pyobject-from-string (string-downcase (symbol-name string-designator))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass python-class (pyobject-mixin funcallable-standard-class)
  ()
  (:metaclass funcallable-standard-class))

(defmethod validate-superclass
    ((class python-class)
     (superclass funcallable-standard-class))
  t)

(defclass python:type (python-class)
  ()
  (:metaclass python-class)
  (:pointer . #.*type-pyobject*))

(defmethod validate-superclass
    ((class python:type)
     (superclass funcallable-standard-class))
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
  (error "Not a Python object: ~S" object))

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

(defun lispify-name (name)
  "Turn Python identifiers like FooBar_baz into FOO-BAR-BAZ.  As a second value,
returns one of the keywords :external or :internal to describe the intended
visibility of that Lisp symbol."
  (declare (string name))
  (let* ((ntotal (length name))
         (nleading (or (position #\_ name :test-not #'char=) ntotal)))
    (values
     (with-output-to-string (stream)
       (labels ((emit (char) (write-char char stream)))
         (let ((position 0))
           (loop while (< position ntotal) do
             (let ((char (schar name position)))
               (cond ((upper-case-p char)
                      (unless (= position nleading)
                        (emit #\-))
                      (emit char)
                      (incf position)
                      (loop while (< position ntotal) do
                        (let ((char (schar name position)))
                          (if (upper-case-p char)
                              (progn (emit char) (incf position))
                              (loop-finish)))))
                     ((char= char #\_)
                      (emit #\-)
                      (incf position))
                     (t
                      (emit (char-upcase char))
                      (incf position))))))))
     (if (zerop nleading)
         :external
         :internal))))

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
  (when (typep object 'python:object)
    (return-from python-from-lisp
      (python-object-pointer object)))
  (when (typep object 'integer)
    (return-from python-from-lisp
      (pylong-from-long object)))
  ;; Allocate
  (break "TODO")
  ;; Add finalizer
  (break "TODO")
  *type-pyobject*)
