(in-package #:ouroboros.internals)

(defun (setf python:getattr) (value object attribute)
  (python:setattr object attribute value))

(defmacro python:import (module-name &optional (variable module-name))
  `(with-global-interpreter-lock-held
     (defparameter ,variable (find-module ',module-name))))

(defmacro python:from (module-name &body variables)
  (when (eq (first variables) 'python:import)
    (pop variables))
  `(import-from ',module-name ',variables)
  (alexandria:with-gensyms (module symbol-table value)
    `(with-global-interpreter-lock-held
       (let* ((,module (find-module ',module-name))
              (,symbol-table (module-symbol-table ,module)))
         ,@(loop for variable in variables
                 collect
                 `(let ((,value (module-symbol-table-lookup ,module ,symbol-table)))
                    (defparameter ,variable ,value)
                    (defun ,variable (&rest arguments)
                      (with-pyobjects ((fn ,value))
                        (pyapply fn arguments)))))))))

(defun module-symbol-table-lookup (module symbol-table symbol)
  (multiple-value-bind (value presentp)
      (gethash symbol symbol-table)
    (when (not presentp)
      (error "The symbol ~S is not present in the module ~S."
             symbol module))
    value))

(defun make-module (module-name)
  (with-global-interpreter-lock-held
    (let ((module (pymodule-new module-name)))
      (unwind-protect (mirror-into-lisp module)
        (pyobject-decref module)))))
