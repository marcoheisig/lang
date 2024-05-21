(in-package #:ouroboros)

;; Locate or define the Python package named Ouroboros and add some attributes
;; to it.

(with-global-interpreter-lock-held
  (let ((module (or (find-module "ouroboros")
                    (make-module "ouroboros"))))
    (setf (python:|getattr| module "print")
          #'print)))
