(in-package #:lang.python.test)

;;; Mirrors of objects that can never become callable are plain objects,
;;; because those are much cheaper to allocate than funcallable ones.  All
;;; other mirrors are funcallable.

(deftest immutable-non-callable-objects-are-plain
  (dolist (object (list (internals:pythonize 42)
                        (internals:pythonize 1.5d0)
                        (internals:pythonize "abc")
                        (python:list #(1 2))
                        (python:tuple #(1 2))))
    (is (typep object 'internals::python-object))
    (is (not (functionp object)))))

(deftest callable-objects-are-funcallable
  (let ((split (internals::python-getattr (internals:pythonize "a-b")
                                          (internals:pythonize "split"))))
    (is (functionp split))
    (is (functionp #'python:len))
    (is (functionp (internals::find-module "sys")))
    (is (= (internals:lispify (funcall (find-class 'python:int)
                                       (internals:pythonize "42")))
           42))))

(deftest python-classes-are-mirrored-as-python-types
  (dolist (class (list (find-class 'python:int)
                       (find-class 'python:object)
                       (class-of #'python:len)
                       (class-of (internals::find-module "sys"))))
    (is (typep class 'python:type))
    (is (functionp class))))

;;; Instances of mutable types are funcallable, because __call__ can be added
;;; to their type later.

(deftest instances-of-mutable-types-are-funcallable
  (let* ((pair (internals::expr "(lambda C: (C, C()))(type('C', (), {}))"))
         (class (python:getitem pair 0))
         (instance (python:getitem pair 1)))
    (is (functionp instance))
    (signals python:type-error (funcall instance))
    (setf (internals::python-getattr class (internals:pythonize "__call__"))
          (internals::expr "lambda self: 42"))
    (is (= (internals:lispify (funcall instance)) 42))))

(deftest subclasses-of-plain-types-are-funcallable
  (let ((my-int (internals::expr "type('MyInt', (int,), {})(3)")))
    (is (functionp my-int))
    (is (typep my-int 'python:int))))
