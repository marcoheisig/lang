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
