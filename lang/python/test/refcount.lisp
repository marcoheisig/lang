(in-package #:lang.python.test)

;;; Repeating an operation many times must not change the reference count of
;;; the Python objects involved, once all garbage has been collected.  We use
;;; a fresh list as the probe, because many builtin objects are immortal and
;;; their reference counts never change.

(defun collect-garbage ()
  "Run a full GC and release the references of the mirrors it collected."
  (loop repeat 3 do
    (sb-ext:gc :full t)
    (internals::release-dead-references)))

(defun refcount (object)
  (internals::pyobject-refcount (internals::python-object-pyobject object)))

(defmacro does-not-leak ((variable) &body body)
  "Signal a test failure if running BODY 1000 times, with VARIABLE bound to a
fresh Python list, changes that list's reference count.

The reference count is first measured after running BODY once, because the GC
sometimes keeps the last temporary object of a loop alive, e.g., a list that
contains VARIABLE.  This adds one reference regardless of the number of
iterations, whereas a leak adds one per iteration."
  `(let ((,variable (python:list #(1 2))))
     (flet ((run (n)
              (loop repeat n do (progn ,@body))
              (collect-garbage)
              (refcount ,variable)))
       (let ((before (run 1)))
         (is (= (run 1000) before))))))

(deftest calls-do-not-leak
  (does-not-leak (list)
    (python:len list)
    (python:getitem list 0)))

(deftest mirrors-do-not-leak
  (does-not-leak (list)
    (python:list (vector list))))

(deftest lispify-does-not-leak
  (does-not-leak (list)
    (internals:lispify (python:list (vector list)))))
