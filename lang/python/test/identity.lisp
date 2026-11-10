(in-package #:lang.python.test)

;;; Each object has at most one live mirror in the other language, so that
;;; passing an object to the other language and back preserves its identity.

(deftest module-mirrors-are-canonical
  (is (eq (internals::find-module "sys")
          (internals::find-module "sys"))))

(deftest lisp-object-round-trip
  (let ((object (make-hash-table)))
    (is (eq (python:getitem (python:list (vector object)) 0)
            object))))

(deftest python-object-round-trip
  (let ((object (python:list #(1 2))))
    (is (eq (python:getitem (python:list (vector object)) 0)
            object))))
