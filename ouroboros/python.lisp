(in-package #:ouroboros)

;;; Define a Python package that works

(defvar *python-package-modules* (make-hash-table :test #'eq)
  "A hash table that maps python packages to their modules.")

(defmacro package-module (package)
  `(values
    (gethash ,package *python-package-modules*)))

(defun python:import (module-name)
  (declare (string module-name))
  (lisp-from-python
   (pyimport-getmodule
    (pyobject-from-string module-name))))

(defparameter *builtins*
  (python:import "builtins"))

#+(or)
(defun python:call (callable &rest arguments)
  (let* ((pycallable (python-object-id callable))
         (pyfunction (pyobject-getattr-string pyobject "__call__")))
    (when (cffi:null-pointer-p pyfunction)
      (error "The object ~S is not callable." callable))
    (cffi:foreign-funcall-pointer pyfunction )))
