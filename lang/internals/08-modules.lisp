(in-package #:lang.internals)

(defun module-value (module attribute-name)
  (with-pyobjects ((pymodule module))
    (move-into-lisp
     (|%PyObject_GetAttrString| pymodule attribute-name))))

(defun (setf module-value) (value module attribute-name)
  (with-pyobjects ((pyvalue value)
                   (pymodule module))
    (|%PyObject_SetAttrString| pymodule attribute-name pyvalue)
    value))

(defun module-name (module)
  (declare (python:module module))
  (lisp-string-from-python-string
   (module-value module "__name__")))

;;; Initialize all module mirror objects right after creation.

(defmethod initialize-instance :after ((module python:module) &key &allow-other-keys)
  "Populate the package corresponding to each Python module."
  (with-pyobjects ((pymodule module)
                   ;; PyModule_GetDict returns a borrowed reference.
                   (pydict (mirror-into-lisp (|%PyModule_GetDict| pymodule))))
    (let ((symbol-table (module-symbol-table module))
          (package (module-package module)))
      ;; Mirror all Python bindings to Lisp.
      (maphash
       (lambda (python-name lisp-name)
         #+(or)
         (format *trace-output* "Mirroring ~A to ~A.~%"
                 python-name lisp-name)
         (let ((value (with-new-pyobjects ((pykey (pyobject-from-string python-name)))
                        (cffi:with-foreign-object (pyvalue-pointer :pointer)
                          ;; PyDict_GetItemRef stores a new reference.
                          (if (|%PyDict_GetItemRef| pydict pykey pyvalue-pointer)
                              (move-into-lisp (cffi:mem-ref pyvalue-pointer :pointer))
                              (error "Missing module attribute: ~A" python-name))))))
           (cond ((boundp lisp-name)
                  (unless (eq value (symbol-value lisp-name))
                    (warn "Multiple definitions of ~S." lisp-name)))
                 ((fboundp lisp-name)
                  (unless (eq value (symbol-function lisp-name))
                    (warn "Multiple definitions of #'~S." lisp-name)))
                 (t
                  #+(or)
                  (eval `(define-symbol-macro ,lisp-name
                             (module-value
                              (load-time-value
                               (find-module
                                 ',(module-name module)))
                              ',python-name)))
                  #+(or)
                  (when (alexandria:starts-with #\_ python-name)
                    (print python-name))
                  (proclaim `(special ,lisp-name))
                  (setf (symbol-value lisp-name)
                        value)
                  (when (functionp value)
                    (setf (symbol-function lisp-name)
                          value))))))
       (bijection-a2b symbol-table))
      ;; Determine which symbols to export.
      (if (|%PyObject_HasAttrStringWithError| pymodule "__all__")
          ;; If the module has an __all__ attribute, export the Lisp symbols
          ;; corresponding to the Python names therein.
          (with-pyobjects
              ((pylist (move-into-lisp (|%PyObject_GetAttrString| pymodule "__all__"))))
            (when (|%PyObject_IsInstance| pylist *list-pyobject*)
              (let ((size (|%PyList_Size| pylist)))
                (loop for index below size do
                  (with-new-pyobjects ((pyitem (|%PyList_GetItemRef| pylist index)))
                    (when (|%PyObject_IsInstance| pyitem *unicode-pyobject*)
                      (let* ((python-name (string-from-pyobject pyitem))
                             (lisp-name (bijection-value symbol-table python-name)))
                        (export lisp-name package))))))))
          ;; If the module has no __all__ attribute, export the Lisp symbols
          ;; corresponding to Python names that don't start with an underscore.
          (maphash
           (lambda (python-name lisp-name)
             (unless (and (>= (length python-name) 1)
                          (char= (schar python-name 0) #\_))
               (export lisp-name package)))
           (bijection-a2b symbol-table))))))

;;; Import the most important Python modules.

(defparameter *builtins*
  (find-module "builtins"))

(defparameter *sys*
  (find-module "sys"))

(defparameter *ast*
  (find-module "ast"))

(defparameter *os*
  (find-module "os"))

(defparameter *main*
  (find-module "__main__"))

;;; Redirect all IO to Lisp.

(with-pyobjects ((pysys *sys*)
                 (pystream *standard-output*))
  (|%PyObject_SetAttrString| pysys "stdout" pystream))

(with-pyobjects ((pysys *sys*)
                 (pystream *error-output*))
  (|%PyObject_SetAttrString| pysys "stderr" pystream))

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
