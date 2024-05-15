(in-package #:ouroboros)

;;; Define a package that mimics the behavior of the Python programming
;;; language, but using Lisp S-expressions as syntax.

(defun truep (python-object)
  (pyobject-truep (mirror-into-python ,expression)))

(defmacro python:|and| (&rest clauses)
  `(and ,@()))

(defmacro python:|assert| (expression)
  `(unless (pyobject-truep (mirror-into-python ,expression))
     (error "Expression ~S evaluated to False."
            ',expression)))

(defmacro python:|await| (&rest rest)
  (declare (ignore rest))
  (error "Not yet implemented."))

(defmacro python:|break| ()
  (error "Encountered break statement outside of a loop."))

(defmacro python:|case| (&body clauses)
  (declare (ignore clauses))
  (error "Not yet implemented."))

(defmacro python:|class| (direct-superclasses &body body)
  (declare (ignore direct-superclasses body))
  (error "Not yet implemented."))

(defmacro python:|continue| ()
  (error "Encountered continue statement outside of a loop."))

(defmacro python:|def| (name lambda-list &body body)
  (declare (ignore name lambda-list body))
  (error "Not yet implemented."))

(defmacro python:|del| (variable)
  (declare (ignore variable))
  (error "Not yet implemented."))

(defmacro python:|for| (variable iterable &body body)
  (alexandria:with-gensyms (iterator nextp loop-start loop-end)
    `(let ((,iterator (make-iterator ,iterable)))
       (tagbody ,loop-start
          (multiple-value-bind (,variable ,nextp)
              (iterator-next ,iterator)
            (when (not ,nextp)
              (go ,loop-end))
            (macrolet ((python:|continue| ()
                         `(go ,',loop-start))
                       (python:|break| ()
                         `(go ,',loop-end)))
              ,@body))
          (go ,loop-start)
          ,loop-end))))

(defun make-iterator (iterable)
  (with-pyobjects ((pyobject iterable))
    (mirror-into-lisp (pyobject-iterator pyobject))))

(defun iterator-next (iterator)
  (with-pyobjects ((pyiter iterator))
    (let* ((pynext (pyiter-next pyiter)))
      (if (cffi:null-pointer-p pynext)
          (values nil nil)
          (values (mirror-into-lisp pynext) t)))))

(defun (setf python:|getattr|) (value object attribute)
  (python:|setattr| object attribute value))

(defmacro python:|import| (module-name &optional (variable module-name))
  `(defparameter ,variable (find-module ',module-name)))

(defmacro python:|import-from| (module-name &rest variables)
  (alexandria:with-gensyms (module)
    `(let ((,module (find-module ',module-name)))
       ,@(loop for variable in variables
               collect
               `(defparameter ,variable
                  (python:|getattr| ,module ',variable))))))

(defun find-module (module-name)
  (let ((pymodulename (pyobject-from-string module-name)))
    (mirror-into-lisp
     (with-python-error-handling
       (prog1 (pyimport-getmodule pymodulename)
         (pyobject-decref pymodulename))))))

(defun python:|pass| ()
  (find-class 'python:|None|))

(defmacro python:|break| ()
  (error "Not yet implemented."))

(defun python:|getattr| (object attribute)
  (let ((pyobject (mirror-into-python object))
        (pyattribute
          (pyobject-from-string attribute)))
    (unwind-protect (mirror-into-lisp (pyobject-getattr pyobject pyattribute))
      (pyobject-decref pyattribute))))

(defun python:|setattr| (object attribute value)
  (let ((pyobject (mirror-into-python object))
        (pyattribute
          (pyobject-from-string attribute))
        (pyvalue (mirror-into-python value)))
    (prog1 value
      (unwind-protect (pyobject-setattr pyobject pyattribute pyvalue)
        (pyobject-decref pyattribute)))))

(in-package #:python)

(named-readtables:in-readtable python:syntax)

(import-from
 builtins
 False
 True
 None
 NotImplemented
 Ellipsis
 __debug__
 abs
 aiter
 all
 anext
 any
 ascii
 bin
 bool
 breakpoint
 bytearray
 bytes
 callable
 chr
 classmethod
 compile
 complex
 delattr
 dict
 dir
 divmod
 enumerate
 eval
 exec
 filter
 float
 format
 frozenset
 getattr
 globals
 hasattr
 hash
 help
 hex
 id
 input
 int
 isinstance
 issubclass
 iter
 len
 list
 locals
 map
 max
 memoryview
 min
 next
 object
 oct
 open
 ord
 pow
 print
 property
 range
 repr
 reversed
 round
 set
 setattr
 slice
 sorted
 staticmethod
 str
 sum
 super
 tuple
 type
 vars
 zip
 __import__
 )
