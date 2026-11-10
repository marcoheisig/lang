(in-package #:lang.python.test)

;;; A round trip through the most basic parts of the bridge: Lisp objects are
;;; mirrored into Python, Python builds a list of them, and the list's repr is
;;; converted back into a Lisp string.

(deftest smoke
  (is (string= (internals:lispify (python:repr (python:list #(1 2.0 "x"))))
               "[1, 2.0, \"x\"]")))

(deftest python-exceptions-become-conditions
  (signals python:index-error
    (python:getitem (python:list #()) 0)))
