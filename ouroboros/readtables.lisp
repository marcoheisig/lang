(in-package #:ouroboros)

(named-readtables:defreadtable python:syntax
  (:merge :standard)
  (:case :preserve)
  #+(or)
  (:macro-char #\[ #'(lambda (stream char)
                       (make-python-list
                        (read-delimited-list #\] stream)))))
