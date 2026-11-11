(in-package #:cl-user)

(defpackage #:lang.python.test
  (:use #:common-lisp)
  (:local-nicknames
   (#:python #:lang.python)
   (#:internals #:lang.internals)
   (#:c-api #:lang.python.c-api))
  (:export
   #:deftest
   #:is
   #:signals
   #:run-tests))
