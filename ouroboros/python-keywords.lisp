(cl:in-package #:ouroboros)

(defmacro python:|let| (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      (destructuring-bind (var form) (first bindings)
        `(let ((,var ,form))
           (flet ((,var (&rest args)
                    (apply ,var args)))
             (python:|let| ,(rest bindings) ,@body))))))

(defun truep (object)
  (with-pyobjects ((pyobject object))
    (pyobject-truep pyobject)))

(defmacro python:|and| (&rest clauses)
  `(if (and ,@(loop for clause in clauses collect `(truep ,clause)))
       python:|True|
       python:|False|))

(defmacro python:|assert| (expression)
  (alexandria:with-gensyms (value)
    `(let ((,value ,expression))
       (unless (truep ,value)
         (error "Expression ~S evaluated to False."
                ',expression))
       ,value)))

(defmacro python:|await| (&rest rest)
  (declare (ignore rest))
  (error "Not yet implemented."))

(defmacro python:|break| ()
  (error "Encountered break statement outside of a loop."))

(defmacro python:|case| (&body clauses)
  (declare (ignore clauses))
  (error "Not yet implemented."))

(defmacro python:|class| (direct-superclasses &body body)
  (declare (ignore direct-superclasses body))
  (error "Not yet implemented."))

(defmacro python:|continue| ()
  (error "Encountered continue statement outside of a loop."))

(defmacro python:|def| (name lambda-list &body body)
  `(defun ,name ,lambda-list ,@body))

(defmacro python:|del| (variable)
  (declare (ignore variable))
  (error "Not yet implemented."))

(defmacro python:|for| (variable iterable &body body)
  (when (eql iterable 'python:|in|)
    (setf iterable (first body)))
  (alexandria:with-gensyms (iterator nextp loop-start loop-end)
    `(let ((,iterator (make-iterator ,iterable)))
       (tagbody ,loop-start
          (multiple-value-bind (,variable ,nextp)
              (iterator-next ,iterator)
            (when (not ,nextp)
              (go ,loop-end))
            (macrolet ((python:|continue| ()
                         `(go ,',loop-start))
                       (python:|break| ()
                         `(go ,',loop-end)))
              ,@body))
          (go ,loop-start)
          ,loop-end)
       python:|None|)))

(defun make-iterator (iterable)
  (with-pyobjects ((pyobject iterable))
    (mirror-into-lisp (pyobject-iterator pyobject))))

(defun iterator-next (iterator)
  (with-pyobjects ((pyiter iterator))
    (let* ((pynext (pyiter-next pyiter)))
      (if (cffi:null-pointer-p pynext)
          (values python:|None| nil)
          (values (mirror-into-lisp pynext) t)))))

(defmacro python:|if| (test then &optional (else python:|None|))
  `(if (truep ,test)
       ,then
       ,else))

(defun python:|is| (&rest objects)
  (if (loop for (object . rest) on objects
            until (null rest)
            always (eq object (first rest)))
      python:|True|
      python:|False|))

(defmacro python:|lambda| (lambda-list &body body)
  `(lambda ,lambda-list ,@body))

(defmacro python:|match| (object &body patterns)
  (declare (ignore object patterns))
  (error "Not yet implemented."))

(defun python:|not| (object)
  (with-pyobjects ((pyobject object))
    (if (pyobject-not pyobject)
        python:|True|
        python:|False|)))

(defmacro python:|or| (&rest clauses)
  `(if (or ,@(loop for clause in clauses collect `(truep ,clause)))
       python:|True|
       python:|False|))

(defun python:|pass| ()
  python:|None|)
