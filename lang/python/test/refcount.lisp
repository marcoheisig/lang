(in-package #:lang.python.test)

;;; Repeating an operation many times must not change the reference count of
;;; the Python objects involved, once all garbage has been collected.  We use
;;; a fresh list as the probe, because many builtin objects are immortal and
;;; their reference counts never change.

(defun collect-garbage ()
  "Run a full GC and the finalizers it scheduled, which release references."
  (loop repeat 3 do
    (sb-ext:gc :full t)
    (sb-impl::run-pending-finalizers)))

(defun refcount (object)
  (internals::pyobject-refcount (internals::python-object-pyobject object)))

(defmacro does-not-leak ((variable) &body body)
  "Signal a test failure if running BODY 1000 times, with VARIABLE bound to a
fresh Python list, changes that list's reference count."
  `(let* ((,variable (python:list #(1 2)))
          (before (refcount ,variable)))
     (loop repeat 1000 do (progn ,@body))
     (collect-garbage)
     (is (= (refcount ,variable) before))))

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
