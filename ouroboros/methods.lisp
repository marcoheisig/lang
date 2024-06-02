(in-package #:ouroboros.internals)

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

(defmethod __lt__ ((a python-object) (b python-object))
  (with-pyobjects ((a a) (b b))
    (pyobject-richcompare a b :lt)))

(defmethod __le__ ((a real) (b real))
  (<= a b))

(defmethod __le__ ((a python-object) (b python-object))
  (with-pyobjects ((a a) (b b))
    (pyobject-richcompare a b :le)))

(defmethod __eq__ ((a number) (b number))
  (= a b))

(defmethod __eq__ ((a python-object) (b python-object))
  (with-pyobjects ((a a) (b b))
    (pyobject-richcompare a b :eq)))

(defmethod __ne__ ((a number) (b number))
  (/= a b))

(defmethod __ne__ ((a python-object) (b python-object))
  (with-pyobjects ((a a) (b b))
    (pyobject-richcompare a b :ne)))

(defmethod __gt__ ((a real) (b real))
  (> a b))

(defmethod __gt__ ((a python-object) (b python-object))
  (with-pyobjects ((a a) (b b))
    (pyobject-richcompare a b :gt)))

(defmethod __ge__ ((a real) (b real))
  (>= a b))

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

(defmethod __abs__ ((number number))
  (abs number))

(defmethod __add__ ((a number) (b number))
  (+ a b))

(defmethod __add__ ((a python-object) (b python-object))
  (with-pyobjects ((pya a) (pyb b))
    (move-into-lisp
     (pynumber-add pya pyb))))

(defmethod __and__ ((a number) (b number))
  (logand a b))

(defmethod __bool__ (object)
  object)

#+(or)
(defmethod __divmod__ ((a number) (b number))
  )

(defmethod __float__ ((a float))
  (python-float-from-lisp-float a))

(defmethod __float__ ((a rational))
  (python-float-from-lisp-float (coerce a 'double-float)))

(defmethod __floordiv__ ((a real) (b real))
  (floor a b))

(defmethod __index__ ((integer integer))
  (__int__ integer))

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

(defmethod __invert__ ((number number))
  (/ number))

(defmethod __lshift__ ((integer integer) (count integer))
  (ash integer count))

(defmethod __mul__ ((a number) (b number))
  (* a b))

(defmethod __neg__ ((number number))
  (- number))

(defmethod __or__ ((a integer) (b integer))
  (logior a b))

(defmethod __pos__ ((number number))
  (+ number))

#+(or)
(defmethod __pow__ ((a number) (b number) c)
  )

(defmethod __mod__ ((a real) (b real))
  (mod a b))

(defmethod __rshift__ ((integer integer) (count integer))
  (ash integer (- count)))

(defmethod __sub__ ((a number) (b number))
  (- a b))

(defmethod __truediv__ ((a number) (b number))
  (/ a b))

(defmethod __xor__ ((a integer) (b integer))
  (logxor a b))

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
