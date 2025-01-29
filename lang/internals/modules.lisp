(in-package #:lang.internals)

(defun module-value (module attribute-name)
  (with-pyobjects ((pymodule module))
    (move-into-lisp
     (pyobject-getattr-string pymodule attribute-name))))

(defun (setf module-value) (value module attribute-name)
  (with-pyobjects ((pyvalue value)
                   (pymodule module))
    (move-into-lisp
     (pyobject-setattr-string pymodule attribute-name pyvalue))))

(defun module-name (module)
  (declare (python:module module))
  (lisp-string-from-python-string
   (module-value module "__name__")))

;;; Initialize all module mirror objects right after creation.

(defmethod initialize-instance :after ((module python:module) &key &allow-other-keys)
  "Populate the package corresponding to each Python module."
  (with-pyobjects ((pymodule module)
                   ;; Fancy trick to later decrement the refcount:
                   (pydict (move-into-lisp (pymodule-dict pymodule))))
    (maphash
     (lambda (python-name lisp-name)
       (let* ((pykey (pyobject-from-string python-name))
              (value (move-into-lisp (pydict-getitem pydict pykey))))
         (cond ((boundp lisp-name)
                (unless (__eq__ value (symbol-value lisp-name))
                  (warn "Multiple definitions of ~S." lisp-name)))
               ((fboundp lisp-name)
                (unless (__eq__ value (symbol-function lisp-name))
                  (warn "Multiple definitions of #'~S." lisp-name)))
               (t
                #+(or)
                (eval `(define-symbol-macro ,lisp-name
                           (module-value
                            (load-time-value
                             (find-module
                              ',(module-name module)))
                            ',python-name)))
                (proclaim `(special ,lisp-name))
                (setf (symbol-value lisp-name)
                      value)
                (when (functionp value)
                  (setf (symbol-function lisp-name)
                        value))))))
     (bijection-a2b (module-symbol-table module)))))

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
  (pyobject-setattr-string pysys "stdout" pystream))

(with-pyobjects ((pysys *sys*)
                 (pystream *error-output*))
  (pyobject-setattr-string pysys "stderr" pystream))

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

;;; Ensure that LD_LIBRARY_PATH points to the location of the currently loaded
;;; libpython.
#+(or)
(setf (getitem (getattr *os* "environ") "LD_LIBRARY_PATH")
      (python-string-from-lisp-string
       (directory-namestring
        (cffi:foreign-library-pathname
         (cffi::get-foreign-library 'libpython)))))
