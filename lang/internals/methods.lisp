(in-package #:lang.internals)

(defmethod __repr__ ((object t))
  (python-string-from-lisp-string
   (with-output-to-string (stream)
     (format stream "~S" object))))

(defmethod __str__ ((object t))
  (python-string-from-lisp-string
   (with-output-to-string (stream)
     (format stream "~A" object))))

(defmethod __lt__ ((a real) (b real))
  (< a b))

(defmethod __lt__ ((a python-object) (b real))
  (__lt__ a (pythonize b)))

(defmethod __lt__ ((a python-object) (b python-object))
  (with-pyobjects ((a a) (b b))
    (pyobject-richcompare a b :lt)))

(defmethod __le__ ((a real) (b real))
  (<= a b))

(defmethod __le__ ((a python-object) (b real))
  (__le__ a (pythonize b)))

(defmethod __le__ ((a python-object) (b python-object))
  (with-pyobjects ((a a) (b b))
    (pyobject-richcompare a b :le)))

(defmethod __eq__ ((a number) (b number))
  (= a b))

(defmethod __eq__ ((a python-object) (b number))
  (__eq__ a (pythonize b)))

(defmethod __eq__ ((a python-object) (b python-object))
  (with-pyobjects ((a a) (b b))
    (pyobject-richcompare a b :eq)))

(defmethod __ne__ ((a number) (b number))
  (/= a b))

(defmethod __ne__ ((a python-object) (b number))
  (__ne__ a (pythonize b)))

(defmethod __ne__ ((a python-object) (b python-object))
  (with-pyobjects ((a a) (b b))
    (pyobject-richcompare a b :ne)))

(defmethod __gt__ ((a real) (b real))
  (> a b))

(defmethod __gt__ ((a python-object) (b real))
  (__gt__ a (pythonize b)))

(defmethod __gt__ ((a python-object) (b python-object))
  (with-pyobjects ((a a) (b b))
    (pyobject-richcompare a b :gt)))

(defmethod __ge__ ((a real) (b real))
  (>= a b))

(defmethod __ge__ ((a python-object) (b real))
  (__ge__ a (pythonize b)))

(defmethod __ge__ ((a python-object) (b python-object))
  (with-pyobjects ((a a) (b b))
    (pyobject-richcompare a b :ge)))

(defmethod __hash__ ((object t))
  (let* ((hash (sxhash object))
         (lo (ldb (byte 32 0) hash))
         (hi (ldb (byte #.(ceiling (log most-positive-fixnum 2)) 32) hash)))
    (python-integer-from-lisp-integer
     (logxor lo hi))))

(defmethod __hash__ ((python-object python-object))
  (with-pyobjects ((pyobject python-object))
    (python-integer-from-lisp-integer
     (pyobject-hash pyobject))))

(defmethod __len__ ((sequence sequence))
  (length sequence))

(defmethod __len__ ((python-object python-object))
  (with-pyobjects ((pyobject python-object))
    (pyobject-size pyobject)))

(defmethod __getitem__ ((sequence sequence) index)
  (elt sequence index))

(defmethod __getitem__ ((python-object python-object) (index integer))
  (with-pyobjects ((pyobject python-object))
    (move-into-lisp
     (pysequence-getitem pyobject index))))

(defmethod __getitem__ ((python-object python-object) (index string))
  (with-pyobjects ((pyobject python-object)
                   (pystr (python-string-from-lisp-string index)))
    (move-into-lisp
     (pyobject-getitem pyobject pystr))))

(defmethod __getitem__ ((python-object python-object) (index python-object))
  (with-pyobjects ((pyobject python-object)
                   (pyindex index))
    (move-into-lisp
     (pyobject-getitem pyobject pyindex))))

(defmethod __getitem__ ((hash-table hash-table) key)
  (gethash key hash-table))

#+(or)
(defmethod __getitem__ ((array array) index)
  )

(defmethod __setitem__ ((sequence sequence) index value)
  (setf (elt sequence index) value))

(defmethod __setitem__ ((python-object python-object) (index integer) value)
  (with-pyobjects ((pyobject python-object)
                   (pyvalue value))
    (pysequence-setitem pyobject index value)))

(defmethod __setitem__ ((python-object python-object) (index string) value)
  (with-pyobjects ((pyobject python-object)
                   (pystr (python-string-from-lisp-string index))
                   (pyvalue value))
    (pysequence-setitem pyobject pystr pyvalue)))

(remove-method
 (function  __setitem__)
 (find-method
  (function __setitem__)
  '()
  (mapcar #'find-class '(python-object python-object t))))

(defmethod __setitem__ ((python-object python-object) (index python-object) value)
  (with-pyobjects ((pyobject python-object)
                   (pyindex index)
                   (pyvalue value))
    (pyobject-setitem pyobject pyindex pyvalue)))

(defmethod __setitem__ ((hash-table hash-table) key value)
  (setf (gethash key hash-table) value))

#+(or)
(defmethod __setitem__ ((array array) index value)
  )

(defmethod __contains__ ((sequence sequence) value)
  (find value sequence))

(defmethod __contains__ ((hash-table hash-table) value)
  (nth-value 1 (gethash value hash-table)))

(defmethod __contains__ ((python-object python-object) value)
  (with-pyobjects ((pyobject python-object)
                   (pyvalue value))
    (move-into-lisp
     (pymapping-has-key pyobject value))))

(defmethod __abs__ ((number number))
  (abs number))

(defmethod __abs__ ((python-object python-object))
  (with-pyobjects ((pyobject python-object))
    (move-into-lisp
     (pynumber-absolute pyobject))))

(defmethod __add__ ((a number) (b number))
  (+ a b))

(defmethod __add__ ((a python-object) (b number))
  (__add__ a (pythonize b)))

(defmethod __add__ ((a python-object) (b python-object))
  (with-pyobjects ((pya a) (pyb b))
    (move-into-lisp
     (pynumber-add pya pyb))))

(defmethod __and__ ((a number) (b number))
  (logand a b))

(defmethod __and__ ((a python-object) (b number))
  (__and__ a (pythonize b)))

(defmethod __and__ ((a python-object) (b python-object))
  (with-pyobjects ((pya a) (pyb b))
    (move-into-lisp
     (pynumber-and pya pyb))))

(defmethod __bool__ (object)
  object)

(defmethod __bool__ ((python-object python-object))
  (with-pyobjects ((pyobject python-object))
    (if (pyobject-truep pyobject)
        python:true
        python:false)))

#+(or)
(defmethod __divmod__ ((a number) (b number))
  )

(defmethod __float__ ((a float))
  (python-float-from-lisp-float a))

(defmethod __float__ ((a rational))
  (python-float-from-lisp-float (coerce a 'double-float)))

(defmethod __float__ ((python-object python-object))
  (with-pyobjects ((pyobject python-object))
    (pynumber-float pyobject)))

(defmethod __floordiv__ ((a real) (b real))
  (floor a b))

(defmethod __floordiv__ ((a python-object) (b real))
  (__floordiv__ a (pythonize b)))

(defmethod __floordiv__ ((a python-object) (b python-object))
  (with-pyobjects ((pya a) (pyb b))
    (move-into-lisp
     (pynumber-floor-divide pya pyb))))

(defmethod __index__ ((integer integer))
  (__int__ integer))

(defmethod __index__ ((python-object python-object))
  (with-pyobjects ((pyobject python-object))
    (move-into-lisp
     (pynumber-index pyobject))))

#+(or)
(defmethod __iadd__ (a b)
  )

#+(or)
(defmethod __iand__ (a b)
  )

#+(or)
(defmethod __ifloordiv__ (a b)
  )

#+(or)
(defmethod __ilshift__ (a b)
  )

#+(or)
(defmethod __imul__ (a b)
  )

#+(or)
(defmethod __ior__ (a b)
  )

#+(or)
(defmethod __ipow__ (a b c)
  )

#+(or)
(defmethod __imod__ (a b)
  )

#+(or)
(defmethod __irshift__ (a b)
  )

#+(or)
(defmethod __isub__ (a b)
  )

#+(or)
(defmethod __itruediv__ (a b)
  )

#+(or)
(defmethod __ixor__ (a b)
  )

(defmethod __int__ ((integer integer))
  (move-into-lisp
   (pylong-from-long integer)))

(defmethod __int__ ((python-object python-object))
  (with-pyobjects ((pyobject python-object))
    (move-into-lisp
     (pynumber-long pyobject))))

(defmethod __invert__ ((number number))
  (/ number))

(defmethod __invert__ ((python-object python-object))
  (with-pyobjects ((pyobject python-object))
    (move-into-lisp
     (pynumber-invert pyobject))))

(defmethod __lshift__ ((integer integer) (count integer))
  (ash integer count))

(defmethod __lshift__ ((a python-object) (b integer))
  (__lshift__ a (python-integer-from-lisp-integer b)))

(defmethod __lshift__ ((a python-object) (b python-object))
  (with-pyobjects ((pya a) (pyb b))
    (move-into-lisp
     (pynumber-lshift pya pyb))))

(defmethod __mul__ ((a number) (b number))
  (* a b))

(defmethod __mul__ ((a python-object) (b number))
  (__mul__ a (pythonize b)))

(defmethod __mul__ ((a python-object) (b python-object))
  (with-pyobjects ((pya a) (pyb b))
    (move-into-lisp
     (pynumber-multiply pya pyb))))

(defmethod __neg__ ((number number))
  (- number))

(defmethod __neg__ ((python-object python-object))
  (with-pyobjects ((pyobject python-object))
    (move-into-lisp
     (pynumber-negative pyobject))))

(defmethod __or__ ((a integer) (b integer))
  (logior a b))

(defmethod __or__ ((a python-object) (b integer))
  (__or__ a (python-integer-from-lisp-integer b)))

(defmethod __or__ ((a python-object) (b python-object))
  (with-pyobjects ((pya a) (pyb b))
    (move-into-lisp
     (pynumber-or pya pyb))))

(defmethod __pos__ ((number number))
  (+ number))

(defmethod __pos__ ((python-object python-object))
  (with-pyobjects ((pyobject python-object))
    (move-into-lisp
     (pynumber-positive pyobject))))

#+(or)
(defmethod __pow__ ((a number) (b number) c)
  )

(defmethod __mod__ ((a real) (b real))
  (mod a b))

(defmethod __mod__ ((a python-object) (b real))
  (__mod__ a (pythonize b)))

(defmethod __mod__ ((a python-object) (b python-object))
  (with-pyobjects ((pya a) (pyb b))
    (move-into-lisp
     (pynumber-remainder pya pyb))))

(defmethod __rshift__ ((integer integer) (count integer))
  (ash integer (- count)))

(defmethod __rshift__ ((a python-object) (b integer))
  (__rshift__ a (python-integer-from-lisp-integer b)))

(defmethod __rshift__ ((a python-object) (b python-object))
  (with-pyobjects ((pya a) (pyb b))
    (move-into-lisp
     (pynumber-rshift pya pyb))))

(defmethod __sub__ ((a number) (b number))
  (- a b))

(defmethod __sub__ ((a python-object) (b number))
  (__sub__ a (pythonize b)))

(defmethod __sub__ ((a python-object) (b python-object))
  (with-pyobjects ((pya a) (pyb b))
    (move-into-lisp
     (pynumber-subtract pya pyb))))

(defmethod __truediv__ ((a number) (b number))
  (/ a b))

(defmethod __truediv__ ((a python-object) (b number))
  (__truediv__ a (pythonize b)))

(defmethod __truediv__ ((a python-object) (b python-object))
  (with-pyobjects ((pya a) (pyb b))
    (move-into-lisp
     (pynumber-true-divide pya pyb))))

(defmethod __xor__ ((a integer) (b integer))
  (logxor a b))

(defmethod __xor__ ((a python-object) (b integer))
  (__xor__ a (python-integer-from-lisp-integer b)))

(defmethod __xor__ ((a python-object) (b python-object))
  (with-pyobjects ((pya a) (pyb b))
    (move-into-lisp
     (pynumber-xor pya pyb))))

(defmethod print-object ((python-object python-object) stream)
  (with-pyobjects ((pyobject python-object))
    (let ((repr (pyobject-repr pyobject)))
      (unwind-protect (write-string (string-from-pyobject repr) stream)
        (pyobject-decref repr)))))

(defmethod print-object ((type python-class) stream)
  (print-unreadable-object (type stream)
    (format stream "~S ~S"
            (class-name (class-of type))
            (class-name type))))

(defparameter *pytype-function-attributes*
  '((:tp-repr __repr__)
    (:tp-str __str__)
    (:tp-hash __hash__)
    (:tp-richcompare __richcmp__)
    ;; Number Functions
    (:nb-absolute __abs__)
    (:nb-add __add__)
    (:nb-and __and__)
    (:nb-bool __bool__)
    (:nb-divmod __divmod__)
    (:nb-float __float__)
    (:nb-floor-divide __floordiv__)
    (:nb-index __index__)
    (:nb-inplace-add __iadd__)
    (:nb-inplace-and __iand__)
    (:nb-inplace-floor-divide __ifloordiv__)
    (:nb-inplace-lshift __ilshift__)
    (:nb-inplace-multiply __imul__)
    (:nb-inplace-or __ior__)
    (:nb-inplace-power __ipow__)
    (:nb-inplace-remainder __imod__)
    (:nb-inplace-rshift __irshift__)
    (:nb-inplace-subtract __isub__)
    (:nb-inplace-true-divide __itruediv__)
    (:nb-inplace-xor __ixor__)
    (:nb-int __int__)
    (:nb-invert __invert__)
    (:nb-lshift __lshift__)
    (:nb-multiply __mul__)
    (:nb-negative __neg__)
    (:nb-or __or__)
    (:nb-positive __pos__)
    (:nb-power __pow__)
    (:nb-remainder __mod__)
    (:nb-rshift __rshift__)
    (:nb-subtract __sub__)
    (:nb-true-divide __truediv__)
    (:nb-xor __xor__)
    ;; Mapping Functions
    (:mp-length __len__)
    (:mp-subscript __getitem__)
    (:mp-ass-subscript __setitem__)
    ;; Sequence Functions
    (:sq-length __len__)
    (:sq-concat __add__)
    (:sq-repeat __mul__)
    (:sq-ass-item __sq_setitem__)
    (:sq-item __sq_getitem__)
    (:sq-contains __contains__)
    (:sq-inplace-concat __iadd__)
    (:sq-inplace-repeat __imul__)))

(defmethod pytype-function-attributes ((class class))
  (loop for (slot function-name) in *pytype-function-attributes*
        collect slot
        collect (pytype-function-attribute class function-name)))

(defun pytype-function-attribute (class function-name)
  (let ((fn (fdefinition function-name)))
    (etypecase fn
      (generic-function
       (let* ((lambda-list (generic-function-lambda-list fn))
              (nargs (length lambda-list)))
         (assert (null (intersection lambda-list lambda-list-keywords)))
         (if (compute-applicable-methods-using-classes
              fn
              (list* class (make-list (1- nargs) :initial-element (find-class 't))))
             (cffi-sys:%callback function-name)
             (cffi:null-pointer))))
      (function (cffi-sys:%callback function-name)))))
