(in-package #:lang.python.test)

;;; Converting a Lisp object to Python and back yields an equivalent object.

(defun round-trip (object)
  (internals:lispify (internals:pythonize object)))

(deftest integer-round-trip
  (dolist (integer (list 0 1 -1
                         most-positive-fixnum (1+ most-positive-fixnum)
                         most-negative-fixnum (1- most-negative-fixnum)
                         (expt 2 64) (- (expt 2 64))
                         (expt 7 200) (- (expt 7 200))))
    (is (eql (round-trip integer) integer))))

(deftest float-round-trip
  (dolist (float (list 0d0 1.5d0 -2.25d0
                       most-positive-double-float
                       least-positive-double-float))
    (is (eql (round-trip float) float)))
  ;; Python has only one float type, so single floats come back as doubles.
  (is (eql (round-trip 1.5f0) 1.5d0)))

(deftest complex-round-trip
  (is (eql (round-trip #c(3d0 -4d0)) #c(3d0 -4d0))))

(deftest string-round-trip
  (dolist (string (list "" "hello" "héllo ∀ 𝄞"))
    (is (string= (round-trip string) string))))

(deftest vector-round-trip
  (is (equalp (round-trip #(1 2 3)) #(1 2 3)))
  (is (equalp (round-trip #()) #())))

(deftest list-round-trip
  (is (equal (round-trip '()) '()))
  (is (equal (round-trip '(1 "a" 2d0)) '(1 "a" 2d0))))

(deftest hash-table-round-trip
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "a" table) 1)
    (setf (gethash "b" table) "c")
    (let ((result (round-trip table)))
      (is (= (hash-table-count result) 2))
      (is (eql (gethash "a" result) 1))
      (is (string= (gethash "b" result) "c")))))
