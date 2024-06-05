(in-package #:lang-internals)

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

#+(or)
(defmethod convert-object
    ((strategy pythonize)
     (simple-vector simple-vector))
  (let ((length (length simple-vector))
        (list (make-python-list length)))
    (loop for position below length do
      (setf (python-lisp-element list position)
            (convert-slot strategy simple-vector position (svref simple-vector position)))))
  (make-python-list
   (ensure-converted-slot (car cons) (pycons value) (python:rplaca tuple value))
   (convert-slot strategy cons '(car cons) car)
   (convert-slot strategy cons '(cdr cons) cdr)))

#+(or)
(defmethod finalize-conversion
    ((strategy pythonize-graph)
     (simple-vector simple-vector)
     (python-list python:list)
     slots)
  (loop for (value index) in slots do
    (setf (python-list-elt python-list index)
          value)))
