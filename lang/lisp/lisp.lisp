(in-package #:cl-user)

(defpackage #:lang.lisp
  (:use #:common-lisp))

(in-package #:lang.lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lisp Packages with 'lang.lisp' Prefix
;;;
;;; The goal of this file is to make Lisp packages available both with a
;;; 'lang.lisp' prefix and without.

(defun lisp-searcher (system-name)
  (multiple-value-bind (lang module-name)
      (lang:parse-system-name system-name)
    (assert (string= lang "lisp"))
    (lang:ensure-asdf-wrapper system-name `(,module-name))))

(lang:register-lang-searcher "lisp" 'lisp-searcher)
