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
