(in-package #:ouroboros)

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

(defun (setf python:|getattr|) (value object attribute)
  (python:|setattr| object attribute value))

(defmacro python:|import| (module-name &optional (variable module-name))
  `(with-global-interpreter-lock-held
     (defparameter ,variable (find-module ',module-name))))

(defmacro python:|import-from| (module-name &rest variables)
  (alexandria:with-gensyms (module value)
    `(with-global-interpreter-lock-held
       (let ((,module (find-module ',module-name)))
         ,@(loop for variable in variables
                 collect
                 `(let ((,value (python:|getattr| ,module ',variable)))
                    (defparameter ,variable ,value)
                    (defun ,variable (&rest arguments)
                      (with-pyobjects ((fn ,value))
                        (pyapply fn arguments)))))))))

(defun make-module (module-name)
  (with-global-interpreter-lock-held
    (let ((module (pymodule-new module-name)))
      (unwind-protect (mirror-into-lisp module)
        (pyobject-decref module)))))

(defun find-module (module-name)
  (with-global-interpreter-lock-held
    (let* ((pymodulename (pyobject-from-string module-name))
           (pymodule
             (with-python-error-handling
               (prog1 (pyimport-getmodule pymodulename)
                 (pyobject-decref pymodulename)))))
      (if (cffi:null-pointer-p pymodule)
          nil
          (mirror-into-lisp pymodule)))))

(in-package #:python)

(named-readtables:in-readtable python:syntax)

(import-from
 builtins
 ;; Constants
 False
 True
 None
 Ellipsis
 NotImplemented
 ;; Functions
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
 ;; getattr
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
 ;; setattr
 slice
 sorted
 staticmethod
 str
 sum
 super
 tuple type
 vars
 zip
 ;; Exceptions
 BaseException
 BaseExceptionGroup
 Exception
 GeneratorExit
 KeyboardInterrupt
 SystemExit
 ArithmeticError
 AssertionError
 AttributeError
 BufferError
 EOFError
 ImportError
 LookupError
 MemoryError
 NameError
 OSError
 ReferenceError
 RuntimeError
 StopAsyncIteration
 StopIteration
 SyntaxError
 SystemError
 TypeError
 ValueError
 Warning
 FloatingPointError
 OverflowError
 ZeroDivisionError
 BytesWarning
 DeprecationWarning
 EncodingWarning
 FutureWarning
 ImportWarning
 PendingDeprecationWarning
 ResourceWarning
 RuntimeWarning
 SyntaxWarning
 UnicodeWarning
 UserWarning
 BlockingIOError
 ChildProcessError
 ConnectionError
 FileExistsError
 FileNotFoundError
 InterruptedError
 IsADirectoryError
 NotADirectoryError
 PermissionError
 ProcessLookupError
 TimeoutError
 IndentationError
 IndexError
 KeyError
 ModuleNotFoundError
 NotImplementedError
 RecursionError
 UnboundLocalError
 UnicodeError
 BrokenPipeError
 ConnectionAbortedError
 ConnectionRefusedError
 ConnectionResetError
 TabError
 UnicodeDecodeError
 UnicodeEncodeError
 UnicodeTranslateError
 ExceptionGroup
 EnvironmentError
 IOError
 )
