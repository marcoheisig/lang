(in-package #:lang.internals)

(defclass pythonize ()
  ())

(defclass pythonize-once (pythonize convert-once)
  ())

(defclass pythonize-tree (pythonize convert-tree)
  ())

(defclass pythonize-graph (pythonize convert-graph)
  ())

(defun pythonize (object &optional (strategy 'pythonize-graph))
  (convert object strategy))

(defmethod convert-object
    ((strategy pythonize)
     (integer integer))
  (python-integer-from-lisp-integer integer))

(defmethod convert-object
    ((strategy pythonize)
     (float float))
  (python-float-from-lisp-float float))

(defmethod convert-object
    ((strategy pythonize)
     (string string))
  (python-string-from-lisp-string string))

(defmethod (setf slot-ref) (value (list python:list) (position integer))
  (with-pyobjects ((pylist list) (pyvalue value))
    (pylist-setitem pylist position pyvalue)
    value))

#+(or)
(defmethod (setf slot-ref) ((list python:dict) (key t) value)
  (setf (python- list position)
        value))

(defmethod convert-object
    ((strategy pythonize)
     (vector vector))
  (let* ((length (length vector))
         (pylist (with-global-interpreter-lock-held (pylist-new length))))
    (loop for position below length
          for value = (convert-slot strategy vector position (elt vector position))
          do (with-pyobjects ((pyvalue value))
               (pylist-setitem pylist position pyvalue)))
    (mirror-into-lisp pylist)))

(defmethod convert-object
    ((strategy pythonize)
     (list list))
  (let* ((length (length list))
         (pytuple (with-global-interpreter-lock-held (pytuple-new length))))
    (loop for position below length
          for value in list
          do (with-global-interpreter-lock-held
               (pytuple-setitem pytuple position (mirror-into-python value))))
    (mirror-into-lisp pytuple)))

#+(or)
(defmethod finalize-conversion
    ((strategy pythonize-graph)
     (simple-vector simple-vector)
     (python-list python:list)
     slots)
  (loop for (value index) in slots do
    (setf (python-list-elt python-list index)
          value)))
