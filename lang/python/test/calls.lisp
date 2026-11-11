(in-package #:lang.python.test)

;;; Calling the mirror of a Python callable passes keyword arguments as
;;; instances of internals::kwarg, which must follow all positional ones.

(deftest calls-with-positional-and-keyword-arguments
  (let ((split (internals::python-getattr (internals:pythonize "a-b-c")
                                          (internals:pythonize "split")))
        (sep (internals:pythonize "-")))
    (is (equalp (internals:lispify (funcall split sep)) #("a" "b" "c")))
    (is (equalp (internals:lispify (funcall split sep (internals::kwarg "maxsplit" 1)))
                #("a" "b-c")))
    (is (equalp (internals:lispify (funcall split (internals::kwarg "sep" sep)))
                #("a" "b" "c")))))

;;; Lambda list derivers run when a callable is mirrored, after its mirror has
;;; been registered.  Mirrors that share a lambda list share compiled code.
;;; The tests use bound methods, because Python creates a fresh one on each
;;; attribute access, whereas mirroring a module already mirrors its functions.

(defun derived-mirror-function (mirror)
  (sb-kernel:%fun-fun (sb-kernel:%funcallable-instance-fun mirror)))

(deftest lambda-list-derivers-run-on-mirroring
  (let* ((derived-for '())
         (internals::*pycallable-lambda-list-derivers*
           (list (lambda (pycallable)
                   (push (internals::mirror-into-lisp pycallable) derived-for)
                   '(x y))))
         (string (internals:pythonize "abcabc"))
         (count (internals::python-getattr string (internals:pythonize "count")))
         (find (internals::python-getattr string (internals:pythonize "find")))
         (c (internals:pythonize "c")))
    (is (equal derived-for (list find count)))
    (is (equal (sb-kernel:%fun-lambda-list count) '(x y)))
    (is (eq (derived-mirror-function count) (derived-mirror-function find)))
    (is (= (internals:lispify (funcall find c 3)) 5))
    (signals program-error (funcall find c 3 4))))

(deftest lambda-list-derivers-skip-non-callables
  (let* ((derived-for '())
         (internals::*pycallable-lambda-list-derivers*
           (list (lambda (pycallable)
                   (push pycallable derived-for)
                   '(x)))))
    (internals:pythonize (expt 10 40))
    (is (null derived-for))))
