(in-package #:ouroboros.internals)

(defmethod __repr__ (object)
  (python-string-from-lisp-string
   (with-output-to-string (stream)
     (print object stream))))

(defmethod __str__ (object)
  (python-string-from-lisp-string
   (with-output-to-string (stream)
     (princ object stream))))

(defmethod __lt__ ((object-1 real) (object-2 real))
  (< object-1 object-2))

(defmethod __le__ ((object-1 real) (object-2 real))
  (<= object-1 object-2))

(defmethod __eq__ ((object-1 number) (object-2 number))
  (= object-1 object-2))

(defmethod __ne__ ((object-1 number) (object-2 number))
  (/= object-1 object-2))

(defmethod __gt__ ((object-1 real) (object-2 real))
  (> object-1 object-2))

(defmethod __ge__ ((object-1 real) (object-2 real))
  (>= object-1 object-2))

(defmethod __hash__ (object)
  (let* ((hash (sxhash object))
         (lo (ldb (byte 32 0) hash))
         (hi (ldb (byte #.(ceiling (log most-positive-fixnum 2)) 32) hash)))
    (logxor lo hi)))

(defmethod __len__ ((sequence sequence))
  (length sequence))

(defmethod __setitem__ ((sequence sequence) index value)
  (setf (elt sequence index) value))

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
  (mirror-into-lisp
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

;;; PyBool

;;; PyByteArray

;;; PyBytes

;;; PyCallable

;;; PyComplex

;;; PyDict

;;; PyErr

;;; PyFloat

;;; PyFrozenset

;;; PyIter

;;; PyLong

;;; PyList

;;; PyModule

;;; PyNone

;;; PyObject

(defmethod print-object ((python-object python-object) stream)
  (with-pyobjects ((pyobject python-object))
    (let ((repr (pyobject-repr pyobject)))
      (unwind-protect (write-string (string-from-pyobject repr) stream)
        (pyobject-decref repr)))))

;;; PyRange

;;; PySet

;;; PySlice

;;; PyUnicode

;;; PyTuple

;;; PyType

(defmethod print-object ((type python-class) stream)
  (print-unreadable-object (type stream)
    (format stream "~S ~S"
            (class-name (class-of type))
            (class-name type))))
