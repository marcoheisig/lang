(in-package #:ouroboros)

(defstruct (literal-keyword
            (:constructor literal-keyword)
            (:copier nil)
            (:predicate literal-keyword-p))
  "A wrapper for keywords that should be passed to Python as objects instead of
triggering the start of the keyword argument portion."
  (keyword (alexandria:required-argument :keyword)
   :type keyword
   :read-only t))

(declaim (inline argument-pyobject))
(defun argument-pyobject (argument)
  (pyobject-address
   (mirror-into-python
    (if (literal-keyword-p argument)
        (literal-keyword-keyword argument)
        argument))))

(defun call (function &rest arguments)
  "Invoke a Lisp function or Python callable on the supplied arguments."
  (etypecase function
    ((or symbol function)
     (apply function arguments))
    (python-object
     ;; Count the positional arguments and keyword arguments, and keep a
     ;; reference to the keyword portion of the arguments.
     (multiple-value-bind (nargs nkwargs kwstart)
         (labels ((scan-positional (args nargs)
                    (if (null args)
                        (values nargs 0 '())
                        (if (keywordp (first args))
                            (scan-keyword args nargs 0 args)
                            (scan-positional (rest args) (1+ nargs)))))
                  (scan-keyword (args nargs nkwargs kwstart)
                    (if (null args)
                        (values nargs nkwargs kwstart)
                        (let ((rest (rest args)))
                          (if (null rest)
                              (error "Odd number of keyword arguments in ~S." arguments)
                              (scan-keyword (rest rest) nargs (1+ nkwargs) kwstart))))))
           (scan-positional arguments 0))
       (let (;; Stack-allocate a vector of addresses that is large enough to
             ;; hold one pointer per argument plus one extra element for
             ;; Python's vectorcall calling convention.
             (argv (make-array (+ nargs nkwargs 1) :element-type '(unsigned-byte 64)))
             ;; If there are keyword arguments, allocate a Python tuple for
             ;; holding the keyword strings.
             (kwnames (if (null nkwargs)
                          (cffi:null-pointer)
                          (pytuple-new nkwargs))))
         (declare (dynamic-extent argv))
         ;; Mirror all positional arguments to Python.
         (loop for index below nargs
               for argument in arguments
               do (setf (aref argv (1+ index))
                        (argument-pyobject argument)))
         ;; Mirror all keyword arguments to Python.
         (loop for index below nkwargs
               for (keyword argument) on kwstart
               do (setf (pytuple-getitem kwnames index)
                        (pyobject-from-string (symbol-name keyword)))
               do (setf (aref argv (+ 1 nargs index))
                        (argument-pyobject argument)))
         ;; Call
         (mirror-into-lisp
          (prog1 (pyobject-vectorcall
                  (python-object-pyobject function)
                  (cffi:mem-aptr (sb-sys:vector-sap argv) :pointer 1)
                  (+ nargs nkwargs +python-vectorcall-arguments-offset+)
                  kwnames)
            ;; Ensure that arguments aren't collected before this point.
            (touch function)
            (touch arguments)
            (pyobject-decref kwnames))))))))

(define-compiler-macro call (&whole whole function &rest arguments
                             &environment environment)
  "(call f a b :c d :e 42)
=> (let ((#:f f) (#:a a) (#:b b) (#:d d))
     (etypecase #:f
       ((or symbol function)
        (funcall #:f #:a #:b :c #:d :e 42))
       (python-object
        (if (or (keywordp a)
                (keywordp b))
            (locally (declare (notinline call))
              (call #:f #:a #:b :c #:d :e 42))
            (funcall (optimized-pycaller 5)
                     #:f #:a #:b #:d 42
                     (pycall-kwnames :c :e))))))"
  (let* ((function-value (gensym "FUNCTION"))
         (constantp-list
           (loop for argument in arguments
                 collect (constantp argument environment)))
         (argvalues
           (loop for argument in arguments
                 for constantp in constantp-list
                 collect
                 (if constantp argument (gensym "ARGUMENT"))))
         (bindings
           (loop for argument in arguments
                 for argvalue in argvalues
                 for constantp in constantp-list
                 unless constantp
                   collect `(,argvalue ,argument)))
         (n-positional-arguments
           (or (position-if #'keywordp argvalues)
               (length argvalues)))
         (keyword-arguments
           (subseq argvalues n-positional-arguments))
         (n-keyword-arguments (length keyword-arguments))
         (n-keywords
           (if (evenp n-keyword-arguments)
               (/ n-keyword-arguments 2)
               (return-from call whole)))
         (keyword-names
           (loop for (key value) on keyword-arguments by #'cddr
                 collect key))
         (keyword-values
           (loop for (key value) on keyword-arguments by #'cddr
                 collect value)))
    (unless (every #'keywordp keyword-names)
      (return-from call whole))
    `(let ((,function-value ,function) ,@bindings)
       (etypecase ,function-value
         ((or symbol function)
          (funcall ,function-value ,@argvalues))
         (python-object
          (if (or ,@(loop for argument in (subseq argvalues 0 n-positional-arguments)
                          for constantp in constantp-list
                          unless constantp
                            collect `(keywordp ,argument)))
              (locally (declare (notinline call))
                (call ,function-value ,@argvalues))
              (funcall (optimized-pycaller ,(+ n-positional-arguments n-keywords))
                       ,function-value
                       ,@(subseq argvalues 0 n-positional-arguments)
                       ,@keyword-values
                       ,(if (null keyword-names)
                            `(cffi:null-pointer)
                            `(load-time-value
                              ;; TODO this is a permanent memory leak.
                              (pytuple-pack
                               (length keyword-names)
                               ,@(loop for keyword-name in keyword-names
                                       collect `(pyobject-from-string keyword-name))))))))))))

(defun argument-symbol (n)
  (intern (format nil"ARG~D" n) #.*package*))

(let ((pycallers (make-hash-table)))
  (defun optimized-pycaller (n-arguments)
    (values
     (alexandria:ensure-gethash
      n-arguments
      pycallers
      (compile
       nil
       (let ((arguments (loop for n below n-arguments collect (argument-symbol n))))
         (print
          `(lambda (function ,@arguments kwnames)
             (let ((argv (make-array ,(1+ n-arguments) :element-type '(unsigned-byte 64))))
               (declare (dynamic-extent argv))
               ,@(loop for position below n-arguments
                       for argument in arguments
                       collect
                       `(setf (aref argv ,(1+ position))
                              (argument-pyobject ,argument)))
               (prog1 (mirror-into-lisp
                       (pyobject-vectorcall
                        (python-object-pyobject function)
                        (cffi:mem-aptr (sb-sys:vector-sap argv) :pointer 1)
                        ,(+ n-arguments +python-vectorcall-arguments-offset+)
                        kwnames))
                 (touch function)))))))))))

(define-compiler-macro optimized-pycaller (&whole whole n-arguments)
  (if (constantp n-arguments)
      `(load-time-value
        (locally (declare (notinline optimized-pycaller))
          (optimized-pycaller ,n-arguments)))
      whole))
