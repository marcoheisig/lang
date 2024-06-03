(in-package #:lang-internals)

;; Locate or define the lang package and add some attributes to it.

#+(or)
(with-global-interpreter-lock-held
  (let ((module (or (find-module "lang.lisp")
                    (make-module "lang.lisp"))))
    (setf (python:getattr module "print")
          #'cl:print)))
