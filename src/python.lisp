(in-package #:sbclmodule)

(defun python:import (module-name &optional (package-name (string-upcase module-name)))
  (declare (string module-name package-name))
  ;; TODO Also define corresponding Lisp package.
  (lisp-from-python
   (pyimport-getmodule
    (pyobject-from-string module-name))))

(defparameter *builtin*
  (python:import "__builtin__"))

#+(or)
(defun python:call (callable &rest arguments)
  (let* ((pycallable (python-object-id callable))
         (pyfunction (pyobject-getattr-string pyobject "__call__")))
    (when (cffi:null-pointer-p pyfunction)
      (error "The object ~S is not callable." callable))
    (cffi:foreign-funcall-pointer pyfunction )))
