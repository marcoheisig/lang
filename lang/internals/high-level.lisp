(in-package #:lang.internals)

;;; Conversion from Lisp to Python

(defun lisp-integer-from-python-integer (python-integer)
  (with-pyobjects ((pylong python-integer))
    (pylong-as-long pylong)))

(defun lisp-float-from-python-float (python-float)
  (declare (python:object python-float))
  (with-pyobjects ((pyfloat python-float))
    (pyfloat-as-double pyfloat)))

(defun lisp-complex-from-python-complex (python-complex)
  (declare (python:object python-complex))
  (with-pyobjects ((pycomplex python-complex))
    (complex (pycomplex-real-as-double pycomplex)
             (pycomplex-imag-as-double pycomplex))))

(defun lisp-real-from-python-real (python-real)
  (declare (python:object python-real))
  (with-pyobjects ((pyreal python-real))
    (cond ((pyobject-typep pyreal *long-pyobject*)
           (lisp-integer-from-python-integer python-real))
          ((pyobject-typep pyreal *float-pyobject*)
           (lisp-float-from-python-float python-real))
          ((pyobject-hasattr-string pyreal "__int__")
           (move-into-lisp (pynumber-long pyreal)))
          ((pyobject-hasattr-string pyreal "__float__")
           (move-into-lisp (pynumber-float pyreal)))
          (t
           (error "Not a Python real: ~S."
                  python-real)))))

(defun lisp-number-from-python-number (python-number)
  (declare (python:object python-number))
  (with-pyobjects ((pynumber python-number))
    (cond ((pyobject-typep pynumber *long-pyobject*)
           (lisp-integer-from-python-integer python-number))
          ((pyobject-typep pynumber *float-pyobject*)
           (lisp-float-from-python-float python-number))
          ((pyobject-typep pynumber *complex-pyobject*)
           (lisp-complex-from-python-complex python-number))
          ((pyobject-hasattr-string pynumber "__int__")
           (move-into-lisp (pynumber-long pynumber)))
          ((pyobject-hasattr-string pynumber "__float__")
           (move-into-lisp (pynumber-float pynumber)))
          (t
           (error "Not a Python number: ~S."
                  python-number)))))

(defun lisp-string-from-python-string (python-string)
  (declare (python:object python-string))
  (with-pyobjects ((pyobject python-string))
    (string-from-pyobject pyobject)))

(defun lisp-package-from-python-module (package-name module-name)
  (declare (string package-name module-name))
  (let* ((package (or (find-package package-name)
                      (make-package package-name)))
         (module (find-module module-name))
         (symbol-table (make-hash-table :test #'eq)))
    ;; Populate the symbol table with mappings from Lisp symbols to Python
    ;; identifiers.  Normally, two Lisp symbols are associated with each Python
    ;; identifier - one using the literal spelling, and one using a more Lisp-y
    ;; translation.  The exception is when two or more Lisp-y translations
    ;; collide, in which case these Python identifiers are only associated with
    ;; their literal spelling.
    (with-pyobjects ((pymodule module)
                     (pylist (move-into-lisp (pyobject-dir pymodule))))
      (let ((size (pylist-size pylist)))
        (dotimes (position size)
          (let* ((python-string
                   ;; pylist-getitem returns a borrowed reference.
                   (let ((pystr (pylist-getitem pylist position)))
                     (mirror-into-lisp pystr)))
                 (python-name
                   (with-pyobjects ((pystring python-string))
                     (string-from-pyobject pystring)))
                 (python-symbol (intern python-name package))
                 (lisp-name (lispify-python-identifier python-string))
                 (lisp-symbol (intern lisp-name package)))
            (export (list python-symbol lisp-symbol) package)
            (if (nth-value 1 (gethash lisp-symbol symbol-table))
                ;; Collision of two lisp names.
                (setf (gethash lisp-symbol symbol-table)
                      '.collision.)
                ;; No collision.
                (setf (gethash lisp-symbol symbol-table)
                      python-string))
            (setf (gethash python-symbol symbol-table)
                  python-string)))))
    ;; Now traverse the symbol table and associate each symbol with their
    ;; values in that module.
    (maphash
     (lambda (symbol python-string)
       (unless (eq python-string '.collision.)
         (let ((value (getattr module python-string)))
           (proclaim `(special ,symbol))
           (setf (symbol-value symbol)
                 value)
           (setf (symbol-function symbol)
                 (lambda (&rest args)
                   (apply value args))))))
     symbol-table)))

(defun lispify-python-identifier (python-string)
  (with-pyobjects ((pydentifier python-string))
    (lispify-pydentifier pydentifier)))

(defun find-module (module-name)
  (with-global-interpreter-lock-held
    (with-pyobjects ((pystring (move-into-lisp (pyobject-from-string module-name))))
      (move-into-lisp (pyimport-import pystring)))))

(defun getattr (python-object python-string)
  "An implementation of getattr that we use to load all built-in Python
functions (including getattr)."
  (with-pyobjects ((pyobject python-object)
                   (pystring python-string))
    (move-into-lisp
     (pyobject-getattr pyobject pystring))))

(defun (setf getattr) (python-value python-object python-string)
  (with-pyobjects ((pyvalue python-value)
                   (pyobject python-object)
                   (pystring python-string))
    (pyobject-setattr pyobject pystring pyvalue)
    python-value))

;;; Conversion from Python to Lisp

(defun python-integer-from-lisp-integer (lisp-integer)
  (declare (integer lisp-integer))
  (with-global-interpreter-lock-held
    (move-into-lisp (pylong-from-long lisp-integer))))

(defun python-float-from-lisp-float (lisp-float)
  (declare (float lisp-float))
  (with-global-interpreter-lock-held
    (move-into-lisp
     (pyfloat-from-double (coerce lisp-float 'double-float)))))

(defun python-complex-from-lisp-complex (lisp-complex)
  (declare (complex lisp-complex))
  (with-global-interpreter-lock-held
    (move-into-lisp
     (pycomplex-from-doubles
      (coerce (realpart lisp-complex) 'double-float)
      (coerce (imagpart lisp-complex) 'double-float)))))

(defun python-string-from-lisp-string (lisp-string)
  (declare (string lisp-string))
  (with-global-interpreter-lock-held
    (move-into-lisp
     (pyobject-from-string lisp-string))))

;;; ... and now for the magic command that sets up all the rest.

(lisp-package-from-python-module "LANG.PYTHON.BUILTINS" "builtins")
