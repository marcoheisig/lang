(in-package #:lang.internals)

(defclass descriptor ()
  ())

(defclass symbol-value-descriptor ()
  ((%symbol
    :initarg :symbol
    :reader symbol-value-descriptor-symbol
    :type symbol))
  (:documentation
   "A Python Descriptor for getting and setting the value of a symbol."))

(defmethod __get__
    ((descriptor symbol-value-descriptor)
     (instance t)
     (owner t))
  (symbol-value (symbol-value-descriptor-symbol descriptor)))

(defmethod __set__
    ((descriptor symbol-value-descriptor)
     (instance t)
     (value t))
  (setf (symbol-value (symbol-value-descriptor-symbol descriptor))
        value))

(defmethod __set__
    ((descriptor symbol-value-descriptor)
     (instance t)
     (value null))
  (makunbound (symbol-value-descriptor-symbol descriptor)))

(defun module-from-package (package)
  (let* ((module-name (format nil "lang.lisp.~A" (python-style-name (package-name package))))
         (module (funcall python:module (python-string-from-lisp-string module-name))))
    (with-pyobjects ((pymodule module))
      (loop for symbol being the external-symbols of package do
        (when (boundp symbol)
          (cffi:with-foreign-string (nameptr (python-style-name symbol))
            (pyobject-setattr-string
             pymodule
             nameptr
             (mirror-into-python
              (make-instance 'symbol-value-descriptor :symbol symbol)))))))
    module))

(defun package-from-module (package-name module-name)
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
                 (lisp-name (lisp-style-name python-name))
                 (lisp-symbol (intern lisp-name package)))
            (export (list python-symbol lisp-symbol) package)
            (setf (gethash python-symbol symbol-table)
                  python-string)
            (if (nth-value 1 (gethash lisp-symbol symbol-table))
                ;; Collision of two lisp names.
                (setf (gethash lisp-symbol symbol-table)
                      '.collision.)
                ;; No collision.
                (setf (gethash lisp-symbol symbol-table)
                      python-string))))))
    ;; Now traverse the symbol table and associate each symbol with their
    ;; values in that module.
    (maphash
     (lambda (symbol python-string)
       (unless (eq python-string '.collision.)
         (let ((value (getattr module python-string)))
           (proclaim `(special ,symbol))
           (setf (symbol-value symbol)
                 value)
           ;; TODO proper lambda lists.
           (setf (symbol-function symbol)
                 (lambda (&rest args)
                   (apply value args))))))
     symbol-table)
    ;; Done.
    package))

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

(defun dot (object &rest strings)
  (reduce #'getattr strings
          :initial-value object
          :key #'pythonize-string))

(defparameter *main-module*
  (find-module "__main__"))

(defparameter *globals*
  (dot (find-module "__main__") "__dict__"))

(defparameter *ast-module*
  (find-module "ast"))

(defparameter *sys-module*
  (find-module "sys"))

(setf (getattr *sys-module* (python-string-from-lisp-string "stdout"))
      *standard-output*)

(setf (getattr *sys-module* (python-string-from-lisp-string "stderr"))
      *error-output*)

;;; ... and now for the magic command that sets up all the rest.

(package-from-module "LANG.PYTHON.BUILTINS" "builtins")
