(in-package #:lang.internals)

;;; Keywords

(defmacro python:and (&rest clauses)
  `(if (and ,@(loop for clause in clauses collect `(truep ,clause)))
       python:true
       python:false))

(defun truep (object)
  (with-pyobjects ((pyobject object))
    (pyobject-truep pyobject)))

(defmacro python:as (&rest rest)
  (declare (ignore rest))
  (error "The ~S keyword must only appear in ~S forms."
         'python:as
         'python:import))

(defmacro python:assert (&rest expressions)
  `(progn
     ,@(loop for expression in expressions
             collect
             (alexandria:with-gensyms (value)
               `(let ((,value ,expression))
                  (unless (truep ,value)
                    (error "The assertion ~S evaluated to False."
                           ',expression)))))))

(defmacro python:await (&rest rest)
  (declare (ignore rest))
  (error "Not yet implemented."))

(defmacro python:break (&optional (tag nil))
  (declare (ignore tag))
  (error "The ~S keyword must only appear in loops."
         'python:break))

(defmacro python:class (direct-superclasses &body body)
  (declare (ignore direct-superclasses body))
  (error "Not yet implemented."))

(defmacro python:continue (&optional (tag nil))
  (declare (ignore tag))
  (error "The ~S keyword must only appear in loops."
         'python:continue))

(defmacro python:def (name lambda-list &body body)
  `(defun ,name ,lambda-list ,@body))

(defmacro python:del (&rest targets)
  (declare (ignore targets))
  (error "Not yet implemented."))

(defmacro python:for (variable iterable &body body)
  (when (eql iterable 'python:in)
    (setf iterable (pop body)))
  (alexandria:with-gensyms (iterator nextp loop-start loop-end)
    `(let ((,iterator (make-iterator ,iterable)))
       (tagbody ,loop-start
          (multiple-value-bind (,variable ,nextp)
              (iterator-next ,iterator)
            (when (not ,nextp)
              (go ,loop-end))
            (macrolet ((python:continue ()
                         `(go ,',loop-start))
                       (python:break ()
                         `(go ,',loop-end)))
              ,@body))
          (go ,loop-start)
          ,loop-end)
       python:none)))

(defun make-iterator (iterable)
  (with-pyobjects ((pyobject iterable))
    (move-into-lisp (pyobject-iterator pyobject))))

(defun iterator-next (iterator)
  (with-pyobjects ((pyiter iterator))
    (let ((pynext (pyiter-next pyiter)))
      (if (cffi:null-pointer-p pynext)
          (values python:none nil)
          (values (move-into-lisp pynext) t)))))

#+(or)
(defmacro python:if (&whole whole test then &body more-clauses)
  (let ((clauses '()))
    (labels ((emit-clause (tokens)
               (if (null tokens)
                   (error "Encountered empty clause in ~D" whole)
                   (let ((test (first tokens))
                         (body ))
                     ))))
      (when more-clauses
        (case (first more-clauses)
          (python:elif (emit-clause (rest more-clauses)))
          (python:else (emit-clause `(python:true ,@(rest more-clauses)))))))
    )
  `(cond ((truep ,test) ,then)
         )
  )

(defun python:is (&rest objects)
  (if (loop for (object . rest) on objects
            until (null rest)
            always (eq object (first rest)))
      python:true
      python:false))

(defmacro python:lambda (lambda-list &body body)
  `(lambda ,lambda-list ,@body))

(defmacro python:match (object &body patterns)
  (declare (ignore object patterns))
  (error "Not yet implemented."))

(defun python:not (object)
  (with-pyobjects ((pyobject object))
    (if (pyobject-not pyobject)
        python:True
        python:False)))

(defmacro python:or (&rest clauses)
  `(if (or ,@(loop for clause in clauses collect `(truep ,clause)))
       python:True
       python:False))

(defun python:pass ()
  python:none)

;;; Miscellaneous

(defmacro python:case (&body clauses)
  (declare (ignore clauses))
  (error "Not yet implemented."))

;;; Operators

(defun python:+ (&rest numbers)
  (if (null numbers)
      0
      (reduce #'__add__ numbers)))

(defun python:- (number &rest more-numbers)
  (if (null more-numbers)
      (__neg__ number)
      (reduce #'__sub__ more-numbers :initial-value number)))

(defun python:* (&rest numbers)
  (if (null numbers)
      1
      (reduce #'__mul__ numbers)))

(defun python:/ (number &rest more-numbers)
  (if (null more-numbers)
      (__invert__ number)
      (reduce #'__truediv__ more-numbers :initial-value number)))

(defun python:// (number &rest more-numbers)
  (if (null more-numbers)
      (__floordiv__ number 1)
      (reduce #'__floordiv__ more-numbers :initial-value number)))

;;; Functions

(defun python:getitem (object index)
  (__getitem__ object index))

(defun (setf python:getitem) (value object index)
  (__setitem__ object index value))

(defun (setf python:getattr) (value object key)
  (python:setattr object key value))

;;; Ensure lang is in the Python path.
#+(or)
(let* ((op (find-module "operator"))
       (sys (find-module "sys"))
       (sys.path (python:getattr sys (pythonize-string "path")))
       (set (python:set sys.path))
       (dir (pythonize-string
             (namestring
              (asdf:system-source-directory (asdf:find-system "lang" t))))))
  (unless (truep (funcall (python:getattr op (pythonize-string "contains")) set dir))
    (funcall (python:getattr sys.path (pythonize-string "append")) dir)))

#+(or)
(let ((lang.python (find-module "lang.python")))
  (python:setattr lang.python (pythonize-string "cl")
                  (module-from-package (find-package "CL"))))

(defun stmt (string)
  (python:exec
   (pythonize-string string)
   *globals*
   *globals*)
  (values))

(defun expr (string)
  (python:eval
   (pythonize-string string)
   *globals*
   *globals*))

(defun python (&rest strings)
  (loop for (string . rest) on strings do
    (if (null rest)
        (return (expr string))
        (stmt string))))
