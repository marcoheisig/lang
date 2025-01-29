(in-package #:lang.internals)

(defun inspect-lambda-list-deriver (pycallable)
  (error 'unable-to-derive-pycallable-lambda-list))

(defparameter *inspect*
  (find-module "inspect"))

(defparameter *typeshed-client-resolver*
  (handler-case (funcall (getattr (find-module "typeshed_client") "Resolver"))
    (python:module-not-found-error () nil)))

(defun typeshed-lambda-list-deriver (pycallable)
  ;; Abort if there is no resolver.
  (when (not *typeshed-client-resolver*)
    (error 'unable-to-derive-pycallable-lambda-list))
  (let ((module
          (handler-case (find-module (getattr pycallable "__module__"))
            (python:module-not-found-error ()
              (error 'unable-to-derive-pycallable-lambda-list))))
        (nameinfo
          (funcall (getattr *typeshed-client-resolver* "get_fully_qualified_name")
                   (pycallable-fully-qualified-name pycallable))))
    (when (eql nameinfo 'python:none)
      (error 'unable-to-derive-pycallable-lambda-list))
    ;; Remove any ImportedName indirection introduced by typeshed_client.
    (loop repeat 10 while (hasattr nameinfo "info") do
      (setf nameinfo (getattr nameinfo "info")))
    (let* ((package (module-package module))
           (ast (getattr nameinfo "ast"))
           (args (getattr ast "args"))
           (posargs (getattr args "posonlyargs"))
           (anyargs (getattr args "args"))
           (defaults (getattr args "defaults"))
           (kwargs (getattr args "kwonlyargs"))
           (vararg (getattr args "vararg"))
           (kwarg (getattr args "kwarg")))
      (with-pyobjects ((pyposargs posargs)
                       (pyanyargs anyargs)
                       (pydefaults defaults)
                       (pykwargs kwargs))
        (let* ((nposargs (pylist-size pyposargs))
               (nanyargs (pylist-size pyanyargs))
               (ndefaults (pylist-size pydefaults))
               (nkwargs (pylist-size pykwargs))
               (nrequired (- (+ nposargs nanyargs) ndefaults))
               ;; Group all argument names into those that are strictly required
               ;; and those that go into the rest portion of the lambda list.
               (required '())
               (optional '()))
          (flet ((argname (pyarg)
                   (string-from-pyobject
                    (pyobject-getattr-string pyarg "arg"))))
            (unless (eql kwarg python:none)
              (push (argname kwarg) optional))
            (unless (eql vararg python:none)
              (push (argname vararg) optional))
            (loop for index from (- nkwargs 1) downto 0 do
              (push (argname (pylist-getitem pykwargs index))
                    optional))
            (loop for index from (- nanyargs 1) downto 0 do
              (let ((argname (argname (pylist-getitem pyanyargs index))))
                (if (< index (- nrequired nposargs))
                    (push argname required)
                    (push argname optional))))
            (loop for index from (- nposargs 1) downto 0 do
              (let ((argname (argname (pylist-getitem pyposargs index))))
                (if (< index nrequired)
                    (push argname required)
                    (push argname optional)))))
          (let ((required-args
                  (loop for name in required collect (intern name package))))
            (if (null optional)
                `(,@required-args)
                `(,@required-args
                  &rest
                  ,(intern
                    (format nil "~{~A~^ ~}" optional)
                    package)))))))))

(defun pycallable-fully-qualified-name (pycallable)
  (let* ((modulename (getattr pycallable "__module__"))
         (qualname (getattr pycallable "__qualname__")))
    (python-string-from-lisp-string
     (format nil "~A.~A"
             (lisp-string-from-python-string modulename)
             (lisp-string-from-python-string qualname)))))

