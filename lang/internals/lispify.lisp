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
  (convert strategy object))

(defmethod convert-object
    ((strategy lispify)
     object)
  "Ensures that objects lispify to themselves by default."
  object)

(defmethod convert-object
    ((strategy lispify)
     (object python:object))
  "Signals an error when encountering Python objects without a more specific method."
  (error "Don't know how to convert ~S to a Lisp object."
         object))

(defmethod convert-object
    ((strategy lispify)
     (int python:int))
  "Converts Python integers to Lisp integers."
  (lisp-integer-from-python-integer int))

(defmethod convert-object
    ((strategy lispify)
     (float python:float))
  "Converts Python floats to Lisp floats."
  (lisp-float-from-python-float float))

(defmethod convert-object
    ((strategy lispify)
     (complex python:complex))
  "Converts Python complex numbers to Lisp complex numbers."
  (lisp-complex-from-python-complex complex))

(defmethod convert-object
    ((strategy lispify)
     (str python:str))
  "Converts Python strings to Lisp strings."
  (lisp-string-from-python-string str))

(defmethod convert-object
    ((strategy lispify)
     (list python:list))
  "Converts Python lists to Lisp vectors that are adjustable and have a fill
pointer."
  (let ((vector
          (make-array
           (with-pyobjects ((pylist list))
             (pylist-size pylist))
           :adjustable t
           :fill-pointer t)))
    (values
     vector
     (lambda ()
       (with-pyobjects ((pylist list))
         (loop for position below (length vector)
               for value = (move-into-lisp (pylist-getitem pylist position))
               do (setf (aref vector position)
                        (convert-object strategy value))))))))

(defmethod convert-object
    ((strategy lispify)
     (tuple python:tuple))
  "Converts Python tuples to Lisp lists."
  (let ((list (make-list
               (with-pyobjects ((pytuple tuple))
                 (pytuple-size pytuple)))))
    (values
     list
     (lambda ()
       (with-pyobjects ((pytuple tuple))
         (loop for (cons) on list
               for position from 0
               for value = (mirror-into-lisp (pytuple-getitem pytuple position))
               do (setf (car cons)
                        (convert-object strategy value))))))))

(defmethod convert-object
    ((strategy lispify)
     (dict python:dict))
  "Converts Python dicts to Lisp EQUAL hash tables."
  (let ((hash-table (make-hash-table :test #'equal)))
    (values
     hash-table
     (lambda ()
       (with-pyobjects ((pydict dict))
         (let ((pyiter (pyiter-new (pydict-items pydict))))
           (assert (pyiterp pyiter))
           (loop for pynext = (pyiter-next pyiter)
                 until (cffi:null-pointer-p pynext)
                 for key = (mirror-into-lisp (pytuple-getitem pynext 0))
                 for value = (mirror-into-lisp (pytuple-getitem pynext 1))
                 do (setf (gethash (convert-object strategy key) hash-table)
                          (convert-object strategy value)))))))))

(defmethod convert-object
    ((strategy lispify)
     (bytes python:bytes))
  (break "TODO"))

;; TODO function

;; TODO module

;; TODO number
