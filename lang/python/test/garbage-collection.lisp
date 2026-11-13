(in-package #:lang.python.test)

;;; INTERNALS:COLLECT-GARBAGE reclaims cycles that cross the language
;;; boundary, which an ordinary GC can't collect.
;;;
;;; SBCL scans the control stack conservatively, so a stale pointer in a dead
;;; stack slot can keep an object alive.  To make the tests deterministic,
;;; each test creates and inspects its objects in threads of their own, whose
;;; stacks are gone once they have finished, and keeps only weak pointers.

(defvar *root* nil
  "A Lisp root for tests that need one.")

(defun in-new-thread (function)
  "Call FUNCTION in a new thread and return its values."
  (sb-thread:join-thread (sb-thread:make-thread function)))

(defun alivep (weak-pointer)
  (nth-value 1 (sb-ext:weak-pointer-value weak-pointer)))

(defun make-cross-heap-cycle (&optional (function #'identity) python-object)
  "Make a cycle of a Lisp object and a Python list that contains the Lisp
object and PYTHON-OBJECT (if supplied), call FUNCTION on the Python list, and
return a weak pointer to the Lisp object."
  (in-new-thread
   (lambda ()
     (let* ((object (make-hash-table))
            (list (python:list (if python-object
                                   (vector object python-object)
                                   (vector object)))))
       (setf (gethash :list object) list)
       (funcall function list)
       (sb-ext:make-weak-pointer object)))))

(deftest cross-heap-cycles-are-collected
  (let* ((probe (python:list #()))
         (weak-pointer (make-cross-heap-cycle #'identity probe)))
    (collect-garbage)
    (is (alivep weak-pointer))
    (is (= (refcount probe) 2))
    (internals:collect-garbage)
    (is (not (alivep weak-pointer)))
    ;; The Python list of the cycle has been freed, too.
    (is (= (refcount probe) 1))))

(deftest many-cross-heap-cycles-are-collected
  (let ((weak-pointers
          (in-new-thread
           (lambda ()
             (loop repeat 1000 collect (make-cross-heap-cycle))))))
    (internals:collect-garbage)
    (is (notany #'alivep weak-pointers))
    (is (zerop (hash-table-count internals::*zombie-mirror-table*)))))

(deftest python-roots-keep-cycles-alive
  (let* ((sys (internals::find-module "sys"))
         (name (internals:pythonize "_lang_test_root"))
         (weak-pointer
           (make-cross-heap-cycle
            (lambda (list) (python:setattr sys name list)))))
    (internals:collect-garbage)
    (is (alivep weak-pointer))
    ;; The Python mirror still stands for the same Lisp object.
    (is (in-new-thread
         (lambda ()
           (eq (python:getitem (python:getattr sys name) 0)
               (sb-ext:weak-pointer-value weak-pointer)))))
    (python:delattr sys name)
    (internals:collect-garbage)
    (is (not (alivep weak-pointer)))))

(deftest lisp-roots-keep-cycles-alive
  (let* ((object (make-hash-table))
         (list (python:list (vector object))))
    (setf (gethash :list object) list)
    (let ((address (internals::pyobject-address
                    (gethash object internals::*mirror-into-python-table*))))
      (internals:collect-garbage)
      ;; The Python mirror survived and is still registered.
      (is (= (internals::pyobject-address
              (gethash object internals::*mirror-into-python-table*))
             address))
      (is (eq (python:getitem list 0) object)))))

(deftest chains-through-python-are-collected-as-a-whole
  ;; Each object references the next one only through a Python list, and the
  ;; last one references the first one.
  (let ((weak-pointers
          (in-new-thread
           (lambda ()
             (let ((objects (loop repeat 3 collect (make-hash-table))))
               (loop for (object next) on (append objects (list (first objects)))
                     while next
                     do (setf (gethash :next object) (python:list (vector next))))
               (setf *root* (first objects))
               (mapcar #'sb-ext:make-weak-pointer objects))))))
    (internals:collect-garbage)
    (is (every #'alivep weak-pointers))
    (setf *root* nil)
    (internals:collect-garbage)
    (is (notany #'alivep weak-pointers))))

;;; A zombie is the Python mirror of a Lisp object that has been collected.
;;; It exists until Python frees it, e.g., because it is part of a Python
;;; cycle that only Python's collector can free.

(deftest zombies-signal-errors
  (let* ((list-address nil)
         (weak-pointer
           (make-cross-heap-cycle
            (lambda (list)
              ;; The list contains itself, so it outlives its Lisp mirror.
              (funcall (python:getattr list (internals:pythonize "append")) list)
              (setf list-address (internals::pyobject-address
                                  (internals::python-object-pyobject list))))))
         (item-address nil))
    (internals::with-global-interpreter-lock-held
      (internals::%collect-garbage)
      (is (not (alivep weak-pointer)))
      (let ((item (c-api:|%PySequence_GetItem| (cffi:make-pointer list-address) 0)))
        (setf item-address (internals::pyobject-address item))
        (is (gethash item-address internals::*zombie-mirror-table*))
        (signals internals:zombie-mirror-error
          (internals::move-into-lisp item))))
    ;; Freeing the zombie removes its entry.
    (internals:collect-garbage)
    (is (not (gethash item-address internals::*zombie-mirror-table*)))))

;;; A weak reference to a Python object of a collected cycle must be dead
;;; once COLLECT-GARBAGE returns, so that Python code can't reach a zombie
;;; through it.  The object is part of a Python cycle, so it survives the
;;; release of its Lisp mirror's reference until Python's collector runs.

(deftest weak-references-to-collected-cycles-are-dead
  (multiple-value-bind (weak-pointer weak-reference)
      (in-new-thread
       (lambda ()
         (let* ((object (make-hash-table))
                (box (internals::expr "type('Box', (), {})()"))
                (name (internals:pythonize "item")))
           (python:setattr box name object)
           (python:setattr box (internals:pythonize "self") box)
           (setf (gethash :box object) box)
           (values (sb-ext:make-weak-pointer object)
                   (funcall (internals::expr "__import__('weakref').ref") box)))))
    (internals:collect-garbage)
    (is (not (alivep weak-pointer)))
    (is (internals:lispify
         (funcall (internals::expr "lambda r: r() is None") weak-reference)))))

;;; Since Python 3.14, a suspended generator can hold a borrowed reference to
;;; one of its local variables on its evaluation stack, which traversal
;;; reports although it isn't counted.  If we subtracted both references,
;;; the mirror below would look unrooted, and its Lisp object would be
;;; collected although the mirror is still referenced from outside.

(deftest borrowed-stack-references-are-not-subtracted
  ;; The mirror of the Lisp object is referenced by a local variable of a
  ;; suspended generator, by a borrowed reference on that generator's stack
  ;; (on Python 3.14 and later), and by a reference that Python can't see.
  ;; The generator is garbage.
  (multiple-value-bind (weak-pointer address)
      (in-new-thread
       (lambda ()
         (let* ((object (make-hash-table))
                (generator (funcall (internals::expr "lambda a: [a, (yield)]")
                                    object)))
           (python:next generator)
           (values (sb-ext:make-weak-pointer object)
                   (internals::pyobject-address
                    (internals::mirror-into-python object))))))
    (internals:collect-garbage)
    (is (alivep weak-pointer))
    ;; The first collection has freed the generator, so the hidden reference
    ;; is the last one.
    (internals::with-global-interpreter-lock-held
      (c-api:|%Py_DecRef| (cffi:make-pointer address)))
    (internals:collect-garbage)
    (is (not (alivep weak-pointer)))))
