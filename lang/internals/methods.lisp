(in-package #:lang.internals)

;;; Conversion to Lisp

(defmethod lispify-number ((python-float python:float))
  (lisp-float-from-python-float python-float))

(defun lisp-float-from-python-float (python-float)
  (declare (python:float python-float))
  (with-pyobjects ((pyfloat python-float))
    (pyfloat-as-double pyfloat)))

(defmethod lispify-number ((python-complex python:complex))
  (lisp-complex-from-python-complex python-complex))

(defun lisp-complex-from-python-complex (python-complex)
  (declare (python:complex python-complex))
  (with-pyobjects ((pycomplex python-complex))
    (complex (pycomplex-real-as-double pycomplex)
             (pycomplex-imag-as-double pycomplex))))

(defmethod lispify-number ((python-object python:object))
  (with-pyobjects ((pyobject python-object))
    (cond ((pyobject-hasattr-string pyobject "__int__")
           (move-into-lisp (pynumber-long pyobject)))
          ((pyobject-hasattr-string pyobject "__float__")
           (move-into-lisp (pynumber-float pyobject)))
          (t
           (call-next-method)))))

;;; Conversion to Python

(defmethod pythonize-number ((float float))
  (python-float-from-lisp-float float))

(defun python-float-from-lisp-float (lisp-float)
  (declare (float lisp-float))
  (with-global-interpreter-lock-held
    (move-into-lisp
     (pyfloat-from-double
      (coerce lisp-float 'double-float)))))

(defmethod pythonize-number ((complex complex))
  (python-complex-from-lisp-complex complex))

(defun python-complex-from-lisp-complex (lisp-complex)
  (declare (complex lisp-complex))
  (with-global-interpreter-lock-held
    (move-into-lisp
     (pycomplex-from-doubles
      (coerce (realpart lisp-complex) 'double-float)
      (coerce (imagpart lisp-complex) 'double-float)))))

;;; Methods of Callback Functions

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
    (logxor lo hi)))

(defmethod __hash__ ((python-object python-object))
  (with-pyobjects ((pyobject python-object))
    (pyobject-hash pyobject)))

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

(remove-method
 (function  __contains__)
 (find-method
  (function __contains__)
  '()
  (mapcar #'find-class '(python-object t))))

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

(defmacro with-place-rebound ((place value) &body body &environment env)
  (multiple-value-bind (vars values storevars writer-form reader-form)
      (get-setf-expansion place env)
    (alexandria:with-gensyms (original-value)
      `(let* (,@(mapcar #'list vars values)
              (,original-value ,reader-form)
              (,(first storevars) ,value))
         ,writer-form
         (unwind-protect (progn ,@body)
           (setf ,(first storevars) ,original-value)
           ,writer-form)))))

(defmethod print-object ((exception python-exception) stream)
  (with-global-interpreter-lock-held
    (with-place-rebound ((getattr *sys-module* (python-string-from-lisp-string "stderr"))
                         stream)
      (pyerr-display-exception
       (python-object-pyobject exception)))))
