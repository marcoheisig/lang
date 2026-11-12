(in-package #:lang.python.test)

;;; The functions |PyFoo| take and return mirror objects and can be called
;;; without holding the global interpreter lock.

(deftest c-api-calls
  (let ((list (python:list #(1 2 3)))
        (one (internals:pythonize 1))
        (two (internals:pythonize 2)))
    (is (= (c-api:|PyObject_Size| list) 3))
    (is (eql (c-api:|PyList_GetItemRef| list 1) 2))
    (is (eql (c-api:|PyTuple_GetItem| (python:tuple list) 0) 1))
    (is (= (internals:lispify (c-api:|PyNumber_Add| one two)) 3))
    (is (c-api:|PyObject_IsTrue| list))
    (is (string= (internals:lispify (c-api:|PyObject_Repr| one)) "1"))))

(deftest c-api-errors
  (signals python:attribute-error
    (c-api:|PyObject_GetAttrString| (python:list #()) "no_such_attribute")))

(deftest c-api-nil-is-null
  ;; PyObject_SetAttr deletes the attribute when the value is NULL.
  (let ((module (c-api:|PyModule_New| "c_api_test")))
    (c-api:|PyObject_SetAttrString| module "x" 42)
    (is (c-api:|PyObject_HasAttrStringWithError| module "x"))
    (c-api:|PyObject_SetAttrString| module "x" nil)
    (is (not (c-api:|PyObject_HasAttrStringWithError| module "x")))))

(deftest c-api-null-results
  ;; The second value distinguishes NULL from the mirror of NIL.
  (is (equal (multiple-value-list
              (c-api:|PyIter_Next| (c-api:|PyObject_GetIter| (python:list #()))))
             '(nil nil)))
  (is (equal (multiple-value-list
              (c-api:|PyIter_Next| (c-api:|PyObject_GetIter| (python:list #(nil)))))
             '(nil t))))

(deftest c-api-out-parameters
  (let ((dict (c-api:|PyDict_New|))
        (one (internals:pythonize 1))
        (two (internals:pythonize 2)))
    (c-api:|PyDict_SetItem| dict one :value)
    (is (equal (multiple-value-list (c-api:|PyDict_GetItemRef| dict one))
               '(:value t)))
    (is (equal (multiple-value-list (c-api:|PyDict_GetItemRef| dict two))
               '(nil nil)))))

(deftest c-api-does-not-leak
  ;; A stolen argument.
  (does-not-leak (list)
    (c-api:|PyList_SetItem| (python:list #(0)) 0 list))
  ;; A borrowed result.
  (does-not-leak (list)
    (c-api:|PyTuple_GetItem| (python:tuple (vector list)) 0)))

;;; Nested calls are merged into calls of the raw C API functions.

(defmacro merged-p (form &environment env)
  "Returns true if the compiler macro of FORM merges it in the current lexical
environment."
  (not (eq form (funcall (compiler-macro-function (first form)) form env))))

(deftest c-api-merging
  (is (merged-p (c-api:|PyNumber_Add| (c-api:|PyNumber_Multiply| a b) c)))
  (is (merged-p (c-api:|PyObject_Repr| (c-api:|PyTuple_GetItem| tuple 0))))
  ;; A call is only merged into an argument that is a PyObject.
  (is (not (merged-p (c-api:|PyNumber_Add| a b))))
  (is (not (merged-p (c-api:|PySequence_GetItem| a (c-api:|PyObject_Size| b)))))
  ;; Calls with a NULL result can't be merged.
  (is (not (merged-p (c-api:|PyObject_Repr| (c-api:|PyIter_Next| a)))))
  ;; Local functions and NOTINLINE declarations prevent merging.
  (flet ((c-api:|PyNumber_Multiply| (a b) (* a b)))
    (is (not (merged-p (c-api:|PyNumber_Add| (c-api:|PyNumber_Multiply| a b) c)))))
  (locally (declare (notinline c-api:|PyNumber_Multiply|))
    (is (not (merged-p (c-api:|PyNumber_Add| (c-api:|PyNumber_Multiply| a b) c))))))

(deftest c-api-merged-calls
  (let ((two (internals:pythonize 2))
        (three (internals:pythonize 3))
        (four (internals:pythonize 4))
        (log '()))
    (is (= (internals:lispify
            (c-api:|PyNumber_Add|
             (c-api:|PyNumber_Multiply| (progn (push 1 log) two)
                                        (progn (push 2 log) three))
             (progn (push 3 log) four)))
           10))
    ;; Arguments are evaluated from left to right.
    (is (equal log '(3 2 1)))
    (is (string= (internals:lispify
                  (c-api:|PyObject_Repr|
                   (c-api:|PyTuple_GetItem| (python:tuple (vector two)) 0)))
                 "2"))
    (signals python:type-error
      (c-api:|PyNumber_Add| (c-api:|PyNumber_Multiply| two three) (python:list #())))))

(deftest c-api-merged-calls-do-not-leak
  ;; A borrowed result.
  (does-not-leak (list)
    (c-api:|PyObject_Repr| (c-api:|PyTuple_GetItem| (python:tuple (vector list)) 0)))
  ;; A borrowed result as a stolen argument.
  (does-not-leak (list)
    (c-api:|PyList_SetItem| (c-api:|PySequence_List| (python:tuple #(0))) 0
                            (c-api:|PyTuple_GetItem| (python:tuple (vector list)) 0)))
  ;; A Python exception in the outer call.
  (does-not-leak (list)
    (handler-case
        (c-api:|PyObject_GetAttrString|
         (c-api:|PyTuple_GetItem| (python:tuple (vector list)) 0)
         "no_such_attribute")
      (python:attribute-error () nil))))
