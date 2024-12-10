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
  (convert strategy object))

(defmethod convert-object
    ((strategy pythonize)
     (integer integer))
  "Converts Lisp integers to Python integers."
  (python-integer-from-lisp-integer integer))

(defmethod convert-object
    ((strategy pythonize)
     (float float))
  "Converts Lisp floats to Python floats."
  (python-float-from-lisp-float float))

(defmethod convert-object
    ((strategy pythonize)
     (string string))
  "Converts Lisp strings to Python strings."
  (python-string-from-lisp-string string))

(defmethod convert-object
    ((strategy pythonize)
     (vector vector))
  "Converts Lisp vectors to Python lists."
  (let* ((length (length vector))
         (list
           (move-into-lisp
            (with-global-interpreter-lock-held
                (pylist-new length)))))
    (values
     list
     (lambda ()
       (with-pyobjects ((pylist list))
         (loop for position below length
               for value = (convert-object strategy (elt vector position))
               do (with-pyobjects ((pyvalue value))
                    (pylist-setitem pylist position pyvalue))))))))

(defmethod convert-object
    ((strategy pythonize)
     (list list))
  "Converts Lisp lists to Python tuples."
  (let* ((length (length list))
         (tuple
           (move-into-lisp
            (with-global-interpreter-lock-held
                (pytuple-new length)))))
    (values
     tuple
     (lambda ()
       (with-pyobjects ((pytuple tuple))
         (loop for position below length
               for value in list
               do (with-pyobjects ((pyvalue value))
                    (pytuple-setitem pytuple position pyvalue))))))))

(defmethod convert-object
    ((strategy pythonize)
     (hash-table hash-table))
  "Converts Lisp hash tables to Python dicts."
  (let* ((dict
           (move-into-lisp
            (with-global-interpreter-lock-held
              (pydict-new)))))
    (values
     dict
     (lambda ()
       (with-pyobjects ((pydict dict))
         (maphash
          (lambda (key value)
            (with-pyobjects ((pykey (convert-object strategy key))
                             (pyvalue (convert-object strategy value)))
              (pydict-setitem pydict pykey pyvalue)
              (values)))
          hash-table))))))
