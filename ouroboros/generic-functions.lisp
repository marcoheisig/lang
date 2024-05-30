(in-package #:ouroboros.internals)

(defgeneric repr (object))

(defgeneric str (object))

(defgeneric bool (object))

(defgeneric mirror-into-lisp (object))

(defgeneric mirror-into-python (object))
