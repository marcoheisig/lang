(in-package #:lang.internals)

(defvar *mirror-into-lisp-table*
  (make-hash-table :weakness :value :synchronized t)
  "A hash table mapping from integers that are PyObject addresses to the
corresponding Lisp objects.

Each Lisp object that is not a mirror object should have a finalizer that
decreases the reference count of its corresponding PyObject.

Use weak references for the values, because we can recreate the Python object
at any time if necessary.")

(defmethod move-into-lisp (pyobject)
  (multiple-value-bind (value presentp)
      (gethash (pyobject-address pyobject) *mirror-into-lisp-table*)
    (if presentp
        (prog1 value
          (with-global-interpreter-lock-held
            (pyobject-decref pyobject)))
        (make-mirror-object :strong-reference pyobject))))

(defmethod mirror-into-lisp (pyobject)
  (multiple-value-bind (value presentp)
      (gethash (pyobject-address pyobject) *mirror-into-lisp-table*)
    (if presentp
        value
        (make-mirror-object :borrowed-reference pyobject))))

(defmethod shared-initialize :after
    ((python-object python-object)
     (slot-names t)
     &key pyobject &allow-other-keys)
  "For a supplied Python object, set its funcallable instance function, define a
finalizer for it, and register it in the mirror-into-lisp table."
  (set-funcallable-instance-function
   python-object
   (lambda (&rest args)
     (pyapply pyobject args)))
  (alexandria:ensure-gethash
   (pyobject-address pyobject)
   *mirror-into-lisp-table*
   (prog1 python-object
     (register-python-object-finalizer pyobject python-object))))

(defun register-python-object-finalizer (pyobject python-object)
  (trivial-garbage:finalize
   python-object
   (lambda ()
     (with-global-interpreter-lock-held
       #+(or)
       (format *trace-output* "~&(Lisp) Finalizing ~S.~%"
               (let ((pyrepr (pyobject-repr pyobject)))
                 (unwind-protect (string-from-pyobject pyrepr)
                   (pyobject-decref pyrepr))))
       (pyobject-decref pyobject)))))

(defun pyapply (pycallable args)
  (declare (pyobject pycallable))
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
                           (error "Odd number of keyword arguments in ~S." args)
                           (scan-keyword (rest rest) nargs (1+ nkwargs) kwstart))))))
        (scan-positional args 0))
    (with-global-interpreter-lock-held
      (let (;; Stack-allocate a vector of addresses that is large enough to
            ;; hold one pointer per argument plus one extra element for
            ;; Python's vectorcall calling convention.
            (argv (make-array (+ nargs nkwargs 1) :element-type '(unsigned-byte 64)))
            ;; If there are keyword arguments, allocate a Python tuple for
            ;; holding the keyword strings.
            (kwnames (if (zerop nkwargs)
                         (cffi:null-pointer)
                         (pytuple-new nkwargs))))
        (declare (dynamic-extent argv))
        ;; Mirror all positional arguments to Python.
        (loop for index below nargs
              for arg in args
              do (setf (aref argv (1+ index))
                       (argument-pyobject arg)))
        ;; Mirror all keyword arguments to Python.
        (loop for index below nkwargs
              for (keyword argument) on kwstart
              do (setf (pytuple-getitem kwnames index)
                       (pyobject-from-string (symbol-name keyword)))
              do (setf (aref argv (+ 1 nargs index))
                       (argument-pyobject argument)))
        ;; Perform the actual call and mirror the result into Lisp.
        (move-into-lisp
         (prog1 (pyobject-vectorcall
                 pycallable
                 (cffi:mem-aptr (sb-sys:vector-sap argv) :pointer 1)
                 (+ nargs nkwargs +python-vectorcall-arguments-offset+)
                 kwnames)
           ;; Ensure that arguments aren't collected before this point.
           (touch args)
           (unless (cffi:null-pointer-p kwnames)
             (pyobject-decref kwnames))))))))

(defmacro with-pyobjects (bindings &body body)
  "Bind each variable to a borrowed reference to the PyObject corresponding to each
supplied Lisp object, and execute BODY in an environment that holds the global
interpreter lock.

Example:
 (with-pyobjects ((pyobject object))
   (foo pyobject))"
  `(with-global-interpreter-lock-held
     (let* ,(loop for (pyvar form) in bindings
                 collect
                 `(,pyvar (mirror-into-python ,form)))
       (unwind-protect (progn ,@body)
         ,@(loop for (pyvar form) in bindings
                 collect
                 `(pyobject-decref ,pyvar))))))

(defstruct (literal-keyword
            (:constructor literal-keyword)
            (:copier nil)
            (:predicate literal-keyword-p))
  "A wrapper for keywords that should be passed to Python as objects instead of
triggering the start of the keyword argument portion."
  (keyword (alexandria:required-argument :keyword)
   :type keyword
   :read-only t))

(defun argument-pyobject (argument)
  (pyobject-address
   (mirror-into-python
    (if (literal-keyword-p argument)
        (literal-keyword-keyword argument)
        argument))))

(ensure-class
 'python:type
 :metaclass (find-class 'python-class)
 :direct-superclasses (list (find-class 'python-class))
 :pyobject *type-pyobject*)

(ensure-class
 'python:object
 :metaclass (find-class 'python:type)
 :direct-superclasses (list (find-class 'python-object))
 :pyobject *object-pyobject*)

(ensure-class
 'python:none
 :metaclass (find-class 'python:type)
 :direct-superclasses (list (find-class 'python:type))
 :pyobject *none-pyobject*)

;; Treat null pointers as none.
(setf (gethash 0 *mirror-into-lisp-table*)
      (find-class 'python:none))

(defun make-mirror-object (reference-type pyobject)
  (with-global-interpreter-lock-held
    ;; Acquire a strong reference that is released by the finalizer of the
    ;; resulting mirror object.
    (ecase reference-type
      (:strong-reference)
      (:borrowed-reference
       (pyobject-incref pyobject)))
    (let* ((pytype (pyobject-type pyobject))
           (class (if (pyobject-eq pytype pyobject)
                      (error "bug")
                      (move-into-lisp pytype))))
      (if (pytype-subtypep pytype *type-pyobject*)
          ;; Create a type.
          (let* ((class-name (pytype-class-name pyobject))
                 (direct-superclasses (pytype-direct-superclasses pyobject)))
            (ensure-class
             class-name
             :metaclass class
             :direct-superclasses direct-superclasses
             :pyobject pyobject))
          ;; Create an instance.
          (make-instance class
            :pyobject pyobject)))))

(defun pytype-class-name (pytype)
  (with-global-interpreter-lock-held
    (let* ((pyname (ignore-errors (pytype-name pytype)))
           (symbol-name (if (or (null pyname) (cffi:null-pointer-p pyname))
                            (format nil "UNNAMED-TYPE-~X" (random most-positive-fixnum))
                            (lispify-pydentifier pyname)))
           (pymodule-name (pyobject-getattr-string pytype "__module__"))
           (package-name
             (if (cffi:null-pointer-p pymodule-name)
                 nil
                 (lispify-pydentifier pymodule-name)))
           (package (or (find-package package-name)
                        (make-package package-name))))
      (intern symbol-name package)
      (intern symbol-name "LANG.PYTHON"))))

(defun pytype-direct-superclasses (pyobject)
  (declare (cffi:foreign-pointer pyobject))
  (with-global-interpreter-lock-held
    (let ((bases (pyobject-getattr-string pyobject "__bases__")))
      (loop for position below (pytuple-size bases)
            collect
            (mirror-into-lisp
             (pytuple-getitem bases position))))))

(defun lispify-pydentifier (pyunicode)
  "Converts the Python identifier into a suitable Lisp symbol.

Applies the following rules:

1. Replace underscores by hyphens (foo_bar -> foo-bar), except when they appear
   at the beginning or the end of the identifier (__foo__ -> __foo__).

2. Convert camel case to hyphenated words (BlockingIOError -> blocking-io-error)

3. Convert all characters to the current print case."
  (let* ((original (string-from-pyobject pyunicode))
         (prefix
           (or (position #\_ original :test-not #'char=)
               (length original)))
         (suffix
           (1+
            (or (position #\_ original :test-not #'char= :start prefix :from-end t)
                (- (length original) prefix 1))))
         ;; Collect the locations of camel case words as (start . end) pairs.
         (camel-case-words
           (let ((capitals
                   (loop for position from prefix below suffix
                         when (upper-case-p (schar original position))
                           collect position)))
             (loop for start in capitals
                   collect
                   (cons
                    start
                    (or (position-if-not #'lower-case-p original :start (1+ start))
                        suffix))))))
    (with-output-to-string (stream)
      (flet ((copy-verbatim (start end)
               (write-sequence original stream :start start :end end))
             (convert (start end)
               (let ((converter
                       (ecase (readtable-case *readtable*)
                         (:upcase #'char-upcase)
                         (:preserve #'identity)
                         (:downcase #'char-downcase))))
                 (loop for position from start below end do
                   (let ((char (schar original position)))
                     (write-char
                      (cond ((char= char #\_) #\-)
                            ((alpha-char-p char)
                             (funcall converter char))
                            (t char))
                      stream))))))
        (copy-verbatim 0 prefix)
        (let ((position prefix))
          (loop for ((start . end) . rest) on camel-case-words do
            (convert position start)
            (convert start end)
            (when (and
                   ;; Must not be the end of the original.
                   (< end suffix)
                   ;; Must be followed by an alpha char.
                   (alpha-char-p (schar original end))
                   ;; If it has size one, must not be followed by another camel
                   ;; case word of size larger than one.
                   (or (> (- end start) 1)
                       (and rest
                            (destructuring-bind (next-start . next-end) (first rest)
                              (> (- next-end next-start) 1)))))
              (write-char #\- stream))
            (setf position end))
          (convert position suffix))
        (copy-verbatim suffix (length original))))))

;;; Define all the built-in types.

(defparameter python:bool (mirror-into-lisp *bool-pyobject*))

(defparameter python:bytearray (mirror-into-lisp *bytearray-pyobject*))

(defparameter python:bytes (mirror-into-lisp *bytes-pyobject*))

(defparameter python::|builtin_function_or_method| (mirror-into-lisp *pycfunction-pyobject*))

(defparameter python:complex (mirror-into-lisp *complex-pyobject*))

(defparameter python:dict (mirror-into-lisp *dict-pyobject*))

(defparameter python:float (mirror-into-lisp *float-pyobject*))

(defparameter python:frozenset (mirror-into-lisp *frozenset-pyobject*))

(defparameter python:int (mirror-into-lisp *long-pyobject*))

(defparameter python:list (mirror-into-lisp *list-pyobject*))

(defparameter python:module (mirror-into-lisp *module-pyobject*))

(defparameter python:None (mirror-into-lisp *none-pyobject*))

(defparameter python:object (mirror-into-lisp *object-pyobject*))

(defparameter python:range (mirror-into-lisp *range-pyobject*))

(defparameter python:set (mirror-into-lisp *set-pyobject*))

(defparameter python:slice (mirror-into-lisp *slice-pyobject*))

(defparameter python:str (mirror-into-lisp *unicode-pyobject*))

(defparameter python:tuple (mirror-into-lisp *tuple-pyobject*))

(defparameter python:type (mirror-into-lisp *type-pyobject*))
