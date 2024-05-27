(in-package #:ouroboros.internals)

;; Locate or define the ouroboros.lisp package and add some attributes to it.

#+(or)
(with-global-interpreter-lock-held
  (let ((module (or (find-module "ouroboros.lisp")
                    (make-module "ouroboros.lisp"))))
    (setf (python:getattr module "print")
          #'cl:print)))
