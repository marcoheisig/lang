(in-package #:lang.internals)

;;; Conversion to Lisp

(defmethod lispify-number ((python-integer python:int))
  (lisp-integer-from-python-integer python-integer))

(defun lisp-integer-from-python-integer (python-integer)
  (declare (python:int python-integer))
  (with-pyobjects ((pylong python-integer))
    ;; TODO support bignums
    (pylong-as-long pylong)))

(defmethod lispify-string ((string string))
  string)

(defmethod lispify-string ((python-string python:str))
  (lisp-string-from-python-string python-string))

(defun lisp-string-from-python-string (python-string)
  (declare (python:str python-string))
  (with-pyobjects ((pyobject python-string))
    (string-from-pyobject pyobject)))

(defmethod pythonize-string ((string python:str))
  string)

;;; Conversion to Python

(defmethod pythonize-number ((integer integer))
  (python-integer-from-lisp-integer integer))

(defun python-integer-from-lisp-integer (lisp-integer)
  (declare (integer lisp-integer))
  (with-global-interpreter-lock-held
    ;; TODO support bignums
    (move-into-lisp (pylong-from-long lisp-integer))))

(defmethod pythonize-string ((string string))
  (python-string-from-lisp-string string))

(defun python-string-from-lisp-string (lisp-string)
  (declare (string lisp-string))
  (with-global-interpreter-lock-held
    (move-into-lisp
     (pyobject-from-string lisp-string))))
