(in-package #:lang.internals)

(defgeneric mirror-into-lisp (pyobject)
  (:documentation
   "Returns a Lisp object that encapsulates the supplied borrowed reference to a
Python object.  Adds a finalizer to the Lisp object that decrements the
reference count of the Python object by one once the Lisp object is garbage
collected."))

(defgeneric move-into-lisp (pyobject)
  (:documentation
   "Returns a Lisp object that encapsulates the supplied strong reference to a
Python object."))

(defgeneric mirror-into-python (lisp-object)
  (:documentation
   "Returns a strong reference to a PyObject that encapsulates the supplied Lisp
object."))

(define-compiler-macro move-into-lisp (&whole whole form)
  "Rewrite (move-into-lisp (mirror-into-python X)) to X."
  (if (and (listp form)
           (= 2 (length form))
           (eql (first form) 'mirror-into-python))
      (second form)
      whole))

(define-compiler-macro mirror-into-python (&whole whole form)
  "Rewrite (mirror-into-python (move-into-lisp X)) to X."
  (if (and (listp form)
           (= 2 (length form))
           (eql (first form) 'move-into-lisp))
      (second form)
      whole))

(define-condition python-error (serious-condition)
  ((%type
    :initform (alexandria:required-argument :type)
    :initarg :type
    :reader python-error-type)
   (%value
    :initform (alexandria:required-argument :value)
    :initarg :value
    :reader python-error-value)
   (%traceback
    :initform (alexandria:required-argument :traceback)
    :initarg :traceback
    :reader python-error-traceback))
  (:report
   (lambda (python-error stream)
     (format stream "Received a Python exception of type ~A:~%~A"
             (string (class-name (python-error-type python-error)))
             (python-error-value python-error)))))

(defun python-error-handler ()
  (unless (cffi:null-pointer-p (pyerr-occurred))
    (cffi:with-foreign-objects ((pytype :pointer)
                                (pyvalue :pointer)
                                (pytraceback :pointer))
      (pyerr-fetch pytype pyvalue pytraceback)
      (error 'python-error
             :type (move-into-lisp (cffi:mem-ref pytype :pointer))
             :value (move-into-lisp (cffi:mem-ref pyvalue :pointer))
             :traceback (move-into-lisp (cffi:mem-ref pytraceback :pointer))))))

(defmacro define-pycallable (name lambda-list)
  `(progn
     (defgeneric ,name ,lambda-list
       (:generic-function-class lispifying-generic-function))
     (cffi:defcallback ,name pyobject
         ,(loop for item in lambda-list
                collect
                `(,item pyobject))
       (mirror-into-python
        (,name
         ,@(loop for item in lambda-list
                 collect
                 `(mirror-into-lisp ,item)))))))

(define-pycallable __repr__ (object))

(define-pycallable __str__ (object))

;;; Comparisons

(defgeneric __lt__ (object-1 object-2)
  (:generic-function-class lispifying-generic-function))

(defgeneric __le__ (object-1 object-2)
  (:generic-function-class lispifying-generic-function))

(defgeneric __eq__ (object-1 object-2)
  (:generic-function-class lispifying-generic-function))

(defgeneric __ne__ (object-1 object-2)
  (:generic-function-class lispifying-generic-function))

(defgeneric __gt__ (object-1 object-2)
  (:generic-function-class lispifying-generic-function))

(defgeneric __ge__ (object-1 object-2)
  (:generic-function-class lispifying-generic-function))

(defgeneric __hash__ (object)
  (:generic-function-class lispifying-generic-function))

(defun __richcmp__ (o1 o2 cmp)
  (let ((a (mirror-into-lisp o1))
        (b (mirror-into-lisp o2)))
    (ecase cmp
      (0 (__lt__ a b))
      (1 (__le__ a b))
      (2 (__eq__ a b))
      (3 (__ne__ a b))
      (4 (__gt__ a b))
      (5 (__ge__ a b)))))

(cffi:defcallback __hash__ :uint
    ((object pyobject))
  (__hash__ (mirror-into-lisp object)))

(cffi:defcallback __richcmp__ pyobject
    ((o1 pyobject)
     (o2 pyobject)
     (cmp :int))
  (__richcmp__ o1 o2 cmp))

;;; Sequence and Mapping Methods

(defgeneric __len__ (object)
  (:generic-function-class lispifying-generic-function))

(cffi:defcallback __len__ :ssize
    ((pyobject pyobject))
  (__len__ (mirror-into-lisp pyobject)))

(define-pycallable __getitem__ (object key))

(defun __sq_getitem__ (pyobject index)
  (handler-case
      (mirror-into-python
       (__getitem__ (mirror-into-lisp pyobject) index))
    ((or #-sbcl type-error
         #+sbcl sb-kernel:index-too-large-error
         sb-int:invalid-array-index-error)
      ()
      (with-global-interpreter-lock-held
        (pyerr-set-none (mirror-into-python (find-class 'python:index-error)))
        (cffi:null-pointer)))))

(cffi:defcallback __sq_getitem__ :pointer
    ((pyobject pyobject)
     (index :ssize))
  (__sq_getitem__ pyobject index))

(define-pycallable __setitem__ (object index value))

(cffi:defcallback __setitem__ :int
    ((pyobject pyobject)
     (pyindex pyobject)
     (pyvalue pyobject))
  (__setitem__ (mirror-into-lisp pyobject)
               (mirror-into-lisp pyindex)
               (mirror-into-lisp pyvalue))
  (values 0))

(defun __sq_setitem__ (pyobject index pyvalue)
  (__setitem__ (mirror-into-lisp pyobject)
               index
               (mirror-into-lisp pyvalue))
  (values 0))

(cffi:defcallback __sq_setitem__ :int
    ((pyobject pyobject)
     (index :ssize)
     (pyvalue pyobject))
  (__sq_setitem__ pyobject index pyvalue))

(defgeneric __contains__ (object value)
  (:generic-function-class lispifying-generic-function))

(cffi:defcallback __contains__ :bool
    ((pyobject pyobject)
     (pyvalue pyobject))
  (__contains__ (mirror-into-lisp pyobject)
                (mirror-into-lisp pyvalue)))

;;; Number Methods

(define-pycallable __abs__ (object))

(define-pycallable __add__ (object-1 object-2))

(define-pycallable __and__ (object-1 object-2))

(defgeneric __bool__ (object)
  (:generic-function-class lispifying-generic-function))

(cffi:defcallback __bool__ :bool
    ((object pyobject))
  (__bool__ (mirror-into-lisp object)))

(define-pycallable __divmod__ (object-1 object-2))

(define-pycallable __float__ (object))

(define-pycallable __floordiv__ (object-1 object-2))

(define-pycallable __index__ (object))

(define-pycallable __iadd__ (object-1 object-2))

(define-pycallable __iand__ (object-1 object-2))

(define-pycallable __ifloordiv__ (object-1 object-2))

(define-pycallable __ilshift__ (object-1 object-2))

(define-pycallable __imul__ (object-1 object-2))

(define-pycallable __ior__ (object-1 object-2))

(define-pycallable __ipow__ (object-1 object-2 object-3))

(define-pycallable __imod__ (object-1 object-2))

(define-pycallable __irshift__ (object-1 object-2))

(define-pycallable __isub__ (object-1 object-2))

(define-pycallable __itruediv__ (object-1 object-2))

(define-pycallable __ixor__ (object-1 object-2))

(define-pycallable __int__ (object))

(define-pycallable __invert__ (object))

(define-pycallable __lshift__ (object-1 object-2))

(define-pycallable __mul__ (object-1 object-2))

(define-pycallable __neg__ (object))

(define-pycallable __or__ (object-1 object-2))

(define-pycallable __pos__ (object))

(define-pycallable __pow__ (object-1 object-2 object-3))

(define-pycallable __mod__ (object-1 object-2))

(define-pycallable __rshift__ (object-1 object-2))

(define-pycallable __sub__ (object-1 object-2))

(define-pycallable __truediv__ (object-1 object-2))

(define-pycallable __xor__ (object-1 object-2))

(defgeneric pytype-function-attributes (class)
  (:documentation
   "Returns a list of alternating slot keywords and callables that describe the
function attributes of the Python type corresponding to the supplied class."))
