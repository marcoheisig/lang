(in-package #:lang.internals)

(defclass pythonize ()
  ())

(defclass pythonize-once (pythonize convert-once)
  ())

(defclass pythonize-tree (pythonize convert-tree)
  ())

(defclass pythonize-graph (pythonize convert-graph)
  ())

(defun pythonize (object &optional (strategy 'pythonize-graph))
  (convert strategy object))

(defmethod convert-object
    ((strategy pythonize)
     (integer integer))
  "Converts Lisp integers to Python integers."
  (python-integer-from-lisp-integer integer))

(defmethod convert-object
    ((strategy pythonize)
     (float float))
  "Converts Lisp floats to Python floats."
  (python-float-from-lisp-float float))

(defmethod convert-object
    ((strategy pythonize)
     (complex complex))
  "Converts Lisp complex numbers to Python complex numbers."
  (python-complex-from-lisp-complex complex))

(defmethod convert-object
    ((strategy pythonize)
     (string string))
  "Converts Lisp strings to Python strings."
  (python-string-from-lisp-string string))

(defmethod convert-object
    ((strategy pythonize)
     (vector vector))
  "Converts Lisp vectors to Python lists."
  (let* ((length (length vector))
         (list
           (move-into-lisp
            (with-global-interpreter-lock-held
                (pylist-new length)))))
    (values
     list
     (lambda ()
       (with-pyobjects ((pylist list))
         (loop for position below length
               for value = (convert-object strategy (elt vector position))
               do (with-pyobjects ((pyvalue value))
                    (pylist-setitem pylist position pyvalue))))))))

(defmethod convert-object
    ((strategy pythonize)
     (list list))
  "Converts Lisp lists to Python tuples."
  (let* ((length (length list))
         (tuple
           (move-into-lisp
            (with-global-interpreter-lock-held
                (pytuple-new length)))))
    (values
     tuple
     (lambda ()
       (with-pyobjects ((pytuple tuple))
         (loop for position below length
               for value in list
               do (with-pyobjects ((pyvalue value))
                    (pytuple-setitem pytuple position pyvalue))))))))

(defmethod convert-object
    ((strategy pythonize)
     (hash-table hash-table))
  "Converts Lisp hash tables to Python dicts."
  (let* ((dict
           (move-into-lisp
            (with-global-interpreter-lock-held
              (pydict-new)))))
    (values
     dict
     (lambda ()
       (with-pyobjects ((pydict dict))
         (maphash
          (lambda (key value)
            (with-pyobjects ((pykey (convert-object strategy key))
                             (pyvalue (convert-object strategy value)))
              (pydict-setitem pydict pykey pyvalue)
              (values)))
          hash-table))))))

(defmethod convert-object
    ((strategy pythonize)
     (package package))
  "Convert Lisp packages to Python modules."
  (let* ((name (format nil "lang.lisp.~(~A~)" (package-name package)))
         (fname (format nil "~A.__functions__" name))
         (vname (format nil "~A.__variables__" name))
         (module (funcall python:module (python-string-from-lisp-string name)))
         (fmodule (funcall python:module (python-string-from-lisp-string fname)))
         (vmodule (funcall python:module (python-string-from-lisp-string vname))))
    (python:setattr module (pythonize "__functions__") fmodule)
    (python:setattr module (pythonize "__variables__") vmodule)
    (values
     module
     (lambda ()
       ;; Initialize the fmodule and vmodule.
       (loop for s being the symbols of package do
         (let* ((lisp-name (symbol-name s))
                (python-name
                  (python-string-from-lisp-string
                   (string-downcase lisp-name))))
           (when (and (fboundp s)
                      (not (macro-function s))
                      (not (special-operator-p s))
                      (functionp (symbol-function s)))
             (setf (python:getattr fmodule python-name)
                   (symbol-function s)))
           ;; TODO bind to a property, not the value.
           (when (and (boundp s))
             (setf (python:getattr vmodule python-name)
                   (symbol-value s)))))
       ;; Copy definitions from fmodule and vmodule to the actual module.

       ;; Create the m.functions and m.variables alias unless those symbols
       ;; already have an attached definition.
       ))))

(cffi:defcallback get-symbol-function pyobject
    ((pyobject pyobject)
     (pysymbol pyobject))
  (declare (ignore pyobject))
  (let ((symbol (mirror-into-lisp pysymbol)))
    (assert (symbolp symbol))
    (mirror-into-python (fdefinition symbol))))

(cffi:defcallback set-symbol-function pystatus
    ((pyobject pyobject)
     (pyvalue pyobject)
     (pysymbol pyobject))
  (declare (ignore pyobject))
  (let ((symbol (mirror-into-lisp pysymbol))
        (value (mirror-into-lisp pyvalue)))
    (assert (symbolp symbol))
    (assert (functionp value))
    (setf (symbol-function symbol)
          value)
    (values 0)))

(cffi:defcallback get-symbol-value pyobject
    ((pyobject pyobject)
     (pysymbol pyobject))
  (declare (ignore pyobject))
  (let ((symbol (mirror-into-lisp pysymbol)))
    (assert (symbolp symbol))
    (mirror-into-python (symbol-value symbol))))

(cffi:defcallback set-symbol-value pystatus
    ((pyobject pyobject)
     (pyvalue pyobject)
     (pysymbol pyobject))
  (declare (ignore pyobject))
  (let ((symbol (mirror-into-lisp pysymbol))
        (value (mirror-into-lisp pyvalue)))
    (assert (symbolp symbol))
    (setf (symbol-value symbol)
          value)
    (values 0)))

#+(or)
(defvar symbol-function-descriptor
  (mirror-into-lisp
   (make-pytype
    "lang.lisp.SymbolFunctionDescriptor"
    +pyobject-type-size+
    0
    '(:default)
    :tp-getset TODO
    )
   ))
