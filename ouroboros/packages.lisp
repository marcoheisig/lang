(cl:in-package #:cl-user)

(defpackage #:python
  (:use)
  (:export
   #:type
   #:object
   .
   #.(let ((package (find-package '#:python)))
       (when package
         (loop for symbol being each external-symbol of package
               collect (symbol-name symbol))))))

(defpackage #:ouroboros
  (:use #:closer-common-lisp)
  (:export))
