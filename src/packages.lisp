(cl:in-package #:cl-user)

(defpackage #:python
  (:use)
  (:export
   #:type
   #:object
   #:none
   #:import
   #:kwargs
   .
   #.(let ((package (find-package '#:python)))
       (when package
         (loop for symbol being each external-symbol of package
               collect (symbol-name symbol))))))

(defpackage #:sbclmodule
  (:use #:closer-common-lisp)
  (:export
   #:python-object-pointer
   #:python-object-refcount
   #:python-object-attribute
   #:python-object-representation
   #:python-object-string))
