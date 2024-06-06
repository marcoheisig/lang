(in-package #:lang.internals)

(defclass lispify ()
  ())

(defclass lispify-once (lispify convert-once)
  ())

(defclass lispify-tree (lispify convert-tree)
  ())

(defclass lispify-graph (lispify convert-graph)
  ())

(defun lispify (object &optional (strategy 'lispify-graph))
  (convert object strategy))

(defmethod convert-object
    ((strategy lispify)
     object)
  object)

(defmethod convert-object
    ((strategy lispify)
     (object python:object))
  (error "Don't know how to convert ~S to a Lisp object."
         object))

(defmethod convert-object
    ((strategy lispify)
     (int python:int))
  (lisp-integer-from-python-integer int))

(defmethod convert-object
    ((strategy lispify)
     (float python:float))
  (lisp-float-from-python-float float))

(defmethod convert-object
    ((strategy lispify)
     (str python:str))
  (lisp-string-from-python-string str))

(defmethod convert-object
    ((strategy lispify)
     (list python:list))
  (with-pyobjects ((pylist list))
    (let* ((size (pylist-size pylist))
           (vector (make-array size :adjustable t)))
      (register-converted-object strategy list vector)
      (loop for position below size
            for value = (pylist-getitem pylist position)
            do (setf (aref vector position)
                     (convert-slot strategy list position value)))
      vector)))

(defmethod convert-object
    ((strategy lispify)
     (tuple python:tuple))
  (with-pyobjects ((pytuple tuple))
    (let* ((size (pytuple-size pytuple))
           (list (make-list size)))
      (register-converted-object strategy tuple list)
      (loop for position below size
            for (head) on list
            for value = (pytuple-getitem pytuple position)
            do (setf (first list)
                     (convert-slot strategy tuple position value)))
      list)))

(defmethod convert-object
    ((strategy lispify)
     (bytes python:bytes))
  (break "TODO"))

(defmethod convert-object
    ((strategy lispify)
     (dict python:dict))
  (break "TODO"))

;; TODO function

;; TODO module

;; TODO float

;; TODO number
