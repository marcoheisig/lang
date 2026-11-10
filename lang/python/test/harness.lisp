(in-package #:lang.python.test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A Minimal Test Harness
;;;
;;; We deliberately don't depend on a test framework.  A test is a function
;;; without arguments that signals an error when it fails.

(defvar *tests* '()
  "The names of all tests, in the order of their definition.")

(defmacro deftest (name &body body)
  "Define a test named NAME that runs BODY."
  `(progn
     (defun ,name () ,@body (values))
     (setf *tests* (append (remove ',name *tests*) (list ',name)))
     ',name))

(define-condition test-failure (error)
  ((%form :initarg :form :reader test-failure-form))
  (:report
   (lambda (condition stream)
     (format stream "~S is false." (test-failure-form condition)))))

(defmacro is (form)
  "Signal a test failure unless FORM evaluates to true."
  `(unless ,form
     (error 'test-failure :form ',form)))

(defmacro signals (condition-type &body body)
  "Signal a test failure unless BODY signals a condition of CONDITION-TYPE."
  `(is (handler-case (progn ,@body nil)
         (,condition-type () t))))

(defun run-tests (&key (tests *tests*) (stream *standard-output*))
  "Run TESTS, report each failure on STREAM, and return whether all passed."
  (let ((failures 0))
    (dolist (test tests)
      (handler-case (funcall test)
        (error (condition)
          (incf failures)
          (format stream "~&FAIL ~S: ~A~%" test condition))))
    (format stream "~&~D of ~D tests passed.~%"
            (- (length tests) failures)
            (length tests))
    (zerop failures)))
