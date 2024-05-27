(in-package #:ouroboros.internals)

;;; PyBool

;;; PyByteArray

;;; PyBytes

;;; PyCallable

;;; PyComplex

;;; PyDict

;;; PyErr

;;; PyFloat

;;; PyFrozenset

;;; PyIter

;;; PyLong

;;; PyList

;;; PyModule

;;; PyNone

;;; PyObject

(defmethod print-object ((python-object python-object) stream)
  (format stream "#<~S ~A>"
          (class-name (class-of python-object))
          (let ((repr (with-pyobjects ((pyobject python-object))
                        (pyobject-repr pyobject))))
            (unwind-protect (string-from-pyobject repr)
              (pyobject-decref repr)))))

;;; PyRange

;;; PySet

;;; PySlice

;;; PyUnicode

;;; PyTuple

;;; PyType

(defmethod print-object ((type python-class) stream)
  (print-unreadable-object (type stream)
    (format stream "~S ~S"
            (class-name (class-of type))
            (class-name type))))
