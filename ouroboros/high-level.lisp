(in-package #:ouroboros.internals)

;;; Conversion from Lisp to Python

(defun lisp-integer-from-python-integer (python-integer)
  (declare (python:object python-integer))
  (with-pyobjects ((pyobject python-integer))
    (let ((pylong (pynumber-long pyobject)))
      (unwind-protect (pylong-as-long pylong)
        (pyobject-decref pylong)))))

(defun lisp-float-from-python-float (python-float)
  (declare (python:object python-float))
  (with-pyobjects ((pyobject python-float))
    (let ((pyfloat (pynumber-float pyobject)))
      (unwind-protect (pyfloat-as-double pyfloat)
        (pyobject-decref pyfloat)))))

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
    (with-pyobjects ((pymodule module))
      (let* ((pylist (pyobject-dir pymodule))
             (size (pylist-size pylist)))
        (dotimes (position size)
          (let* ((python-string
                   (let ((pystr (pylist-getitem pylist position)))
                     (unwind-protect (mirror-into-lisp pystr)
                       (pyobject-decref pystr))))
                 (python-name
                   (with-pyobjects ((pystring python-string))
                     (unwind-protect (string-from-pyobject pystring)
                       (pyobject-decref pystring))))
                 (python-symbol (intern python-name package))
                 (lisp-name (lispify-python-identifier python-string))
                 (lisp-symbol (intern lisp-name package)))
            (if (nth-value 1 (gethash lisp-symbol symbol-table))
                ;; Collision of two lisp names.
                (setf (gethash lisp-symbol symbol-table)
                      '.collision.)
                ;; No collision.
                (setf (gethash lisp-symbol symbol-table)
                      python-string))
            (setf (gethash python-symbol symbol-table)
                  python-string)))
        (pyobject-decref pylist)))
    ;; Now traverse the symbol table and associate each symbol with their
    ;; values in that module.
    (maphash
     (lambda (symbol python-string)
       (unless (eq python-string '.collision.)
         (let ((value (bootstrap-getattr module python-string)))
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
    (with-pyobjects ((pystring (python-string-from-lisp-string module-name)))
      (let ((pymodule (pyimport-import pystring)))
        (if (cffi:null-pointer-p pymodule)
            (error "Found no module named ~S." module-name)
            (unwind-protect (mirror-into-lisp pymodule)
              (pyobject-decref pymodule)))))))

(defun bootstrap-getattr (python-object python-string)
  "An implementation of getattr that we use to load all built-in Python
functions (including getattr)."
  (with-pyobjects ((pyobject python-object)
                   (pystring python-string))
    (let ((pyattr (pyobject-getattr pyobject pystring)))
      (unwind-protect (mirror-into-lisp pyattr)
        (pyobject-decref pyattr)))))

;;; Conversion from Python to Lisp

(defun python-integer-from-lisp-integer (lisp-integer)
  (declare (integer lisp-integer))
  (with-global-interpreter-lock-held
    (let ((pylong (pylong-from-long lisp-integer)))
      (unwind-protect (mirror-into-lisp pylong)
        (pyobject-decref pylong)))))

(defun python-float-from-lisp-float (lisp-float)
  (declare (float lisp-float))
  (with-global-interpreter-lock-held
    (let ((pyfloat (pyfloat-from-double (coerce lisp-float 'double-float))))
      (unwind-protect (mirror-into-lisp pyfloat)
        (pyobject-decref pyfloat)))))

(defun python-string-from-lisp-string (lisp-string)
  (declare (string lisp-string))
  (with-global-interpreter-lock-held
    (let ((pyobject (pyobject-from-string lisp-string)))
      (unwind-protect (mirror-into-lisp pyobject)
        (pyobject-decref pyobject)))))

;;; ... and now for the magic command that sets up all the rest.

(lisp-package-from-python-module "OUROBOROS.PYTHON.BUILTINS" "builtins")
