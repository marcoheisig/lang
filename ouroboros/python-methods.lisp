(in-package #:ouroboros)

(defmethod print-object ((int python:|int|) stream)
  (print-unreadable-object (int stream)
    (format stream "~A ~D"
            (class-name (class-of int))
            (with-pyobjects ((pyobject int))
              (pylong-as-long pyobject)))))

(defmethod print-object ((str python:|str|) stream)
  (print-unreadable-object (str stream)
    (format stream "~A ~S"
            (class-name (class-of str))
            (with-pyobjects ((pyobject str))
              (string-from-pyobject pyobject)))))

(defmethod print-object ((tuple python:|tuple|) stream)
  (print-unreadable-object (tuple stream)
    (format stream "~A~{ ~A~}"
            (class-name (class-of tuple))
            (with-pyobjects ((pytuple tuple))
              (loop for index below (pytuple-size pytuple)
                    collect
                    (mirror-into-lisp
                     (pytuple-getitem pytuple index)))))))

(defmethod print-object ((list python:|list|) stream)
  (print-unreadable-object (list stream)
    (format stream "~A~{ ~A~}"
            (class-name (class-of list))
            (with-pyobjects ((pylist list))
              (loop for index below (pylist-size pylist)
                    collect
                    (mirror-into-lisp
                     (pylist-getitem pylist index)))))))
