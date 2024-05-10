(in-package #:cl-user)

(load "~/quicklisp/setup.lisp")

(load "./sbclmodule.lisp")

(sb-ext:save-lisp-and-die "sbclmodule.core" :callable-exports `(("marcofn" ,(find-symbol "MARCOFN" "SBCLMODULE"))))
