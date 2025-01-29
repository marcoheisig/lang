(in-package #:lang.internals)

;;; Mapping names from Python to Lisp and vice versa has to take the following
;;; rules into account:
;;;
;;; - Python only has one namespace per module, but the value corresponding to
;;;   each name can be a type, a function, and a value at the same time.
;;;
;;; - Lisp has separate namespaces for functions, variables, and types.
;;;
;;; - A Python name must obey certain rules (no hyphens, doesn't start with a
;;;   digit), and should follow certain conventions (capitalized class names,
;;;   lowercase function names).
;;;
;;; - A Lisp name can be an arbitrary sequence of characters, but typically it
;;;   consists only of uppercase characters and hyphens.
;;;
;;; We solve the problem of mapping multiple namespaces into a single Python
;;; module by giving each module three sub-modules named f, v, and t, holding
;;; the functions, variables, and types, respectively.
;;;
;;; We solve the problems of collisions within one Python or Lisp namespace by
;;; appending an increasing counter to each subsequent entry that would
;;; otherwise have the same name.
;;;
;;; Both Python modules and Lisp pacakges can be modified at runtime, but we
;;; disregard this possibility when creating symbol tables because it is
;;; considered bad practice anyway.  Also, incremental changes would result in
;;; names that depend on the order of those changes, which is undesirable.  For
;;; those reasons, all symbol tables are immutable and represent the state of a
;;; package or symbol at a particular point in time.

(defparameter *python-style-suffix-substitutions*
  '(("1+" . "inc")
    ("1-" . "dec")
    ("<=" . "le")
    (">=" . "ge")
    ("/=" . "ne")
    ("~" . "tilde")
    ("+" . "add")
    ("*" . "mul")
    ("-" . "sub")
    ("/" . "div")
    ("=" . "eq")
    ("<" . "lt")
    (">" . "gt")
    ("?" . "p")
    ("!" . "f")))

(defun closing-earmuff (char)
  (declare (character char))
  (case char
    (#\+ #\+)
    (#\- #\-)
    (#\* #\*)
    (#\. #\.)
    (#\% #\%)
    (#\~ #\~)
    (#\< #\>)
    (#\[ #\])
    (#\{ #\})
    (otherwise nil)))

(defun python-style-name (string)
  "Applies the following rules:

1. Strip any earmuffs.

2. Apply the first matching substitution in *python-style-suffix-translations*,
   if there is any.

3. Replace non-alphanumeric characters by underscores.

3. Change the case of each character to lowercase, or to uppercase if the
   earmuffs that were stripped in the first step were plus signs."
  (declare (string string))
  (let* ((end (length string))
         ;; Strip earmuffs.
         (prefix
           (loop for pos below (floor end 2)
                 while (eql (schar string pos)
                              (closing-earmuff
                               (schar string (- end 1 pos))))
                 count 1))
         ;; Detect whether we are dealing with a constant, i.e., a string whose
         ;; earmuffs are all plus signs.
         (constantp
           (and (plusp prefix)
                (loop for k below prefix
                      always (char= #\+ (schar string k)))))
         (suffix prefix)
         (suffix-substitute nil))
    ;; Check whether any suffix substitution applies.
    (loop for (pattern . substitute) in *python-style-suffix-substitutions* do
      (let ((n (length pattern)))
        (when (and (<= n (- end prefix suffix))
                   (string= string pattern :start1 (- end suffix n) :start2 0 :end2 n))
          (incf suffix n)
          (setf suffix-substitute substitute)
          (loop-finish))))
    (with-output-to-string (stream)
      ;; Replace hyphens with underscores and change the case of the resulting
      ;; characters.
      (loop for index from prefix below (- end suffix) do
        (let ((char (schar string index)))
          (write-char
           (cond ((alpha-char-p char)
                  (if constantp
                      (char-upcase char)
                      (char-downcase char)))
                 ((digit-char-p char) char)
                 (t #\_))
           stream)))
      ;; Emit the suffix substitute if there is any.
      (when suffix-substitute
        (write-string suffix-substitute stream)))))

(defun python-style-class-name (string)
  "First convert the supplied string to a Python-style name, then capitalize the
underscore-delimited substrings and remove all the underscores."
  (declare (string string))
  (with-output-to-string (stream)
    (let ((end (length string))
          (pos 0))
      (loop while (< pos end)
            for next = (or (position #\_ string :start pos :test #'char=) end)
            do (cond ((= pos next)
                      (incf pos))
                     ((< pos next)
                      (format stream "~:(~A~)" (subseq string pos next))
                      (setf pos next)))))))

(defun lisp-style-name (string &key earmuffs)
  "Applies the following rules:

1. Replace underscores by hyphens (foo_bar -> foo-bar), except when they appear
   at the beginning or the end of the identifier (__foo__ -> __foo__).

2. Convert camel case to hyphenated words (BlockingIOError -> blocking-io-error)

3. Convert all characters to the current print case.

4. If the earmuffs keyword argument is supplied, add it to the beginning and
   the end of the resulting string."
  (declare (string string))
  (let* ((earmuffs (and earmuffs (string earmuffs)))
         (prefix
           (or (position #\_ string :test-not #'char=)
               (length string)))
         (suffix
           (1+
            (or (position #\_ string :test-not #'char= :start prefix :from-end t)
                (- (length string) prefix 1))))
         ;; Collect the locations of camel case words as (start . end) pairs.
         (camel-case-words
           (let ((capitals
                   (loop for position from prefix below suffix
                         when (upper-case-p (schar string position))
                           collect position)))
             (loop for start in capitals
                   collect
                   (cons
                    start
                    (or (position-if-not #'lower-case-p string :start (1+ start))
                        suffix))))))
    (with-output-to-string (stream)
      (flet ((copy-verbatim (start end)
               (write-sequence string stream :start start :end end))
             (convert (start end)
               (let ((converter
                       (ecase (readtable-case *readtable*)
                         (:upcase #'char-upcase)
                         (:preserve #'identity)
                         (:downcase #'char-downcase))))
                 (loop for position from start below end do
                   (let ((char (schar string position)))
                     (write-char
                      (cond ((char= char #\_) #\-)
                            ((alpha-char-p char)
                             (funcall converter char))
                            (t char))
                      stream))))))
        (when earmuffs (write-sequence earmuffs stream))
        (copy-verbatim 0 prefix)
        (let ((position prefix))
          (loop for ((start . end) . rest) on camel-case-words do
            (convert position start)
            (convert start end)
            (when (and
                   ;; Must not be the end of the string.
                   (< end suffix)
                   ;; Must be followed by an alpha char.
                   (alpha-char-p (schar string end))
                   ;; If it has size one, must not be followed by another camel
                   ;; case word of size larger than one.
                   (or (> (- end start) 1)
                       (and rest
                            (destructuring-bind (next-start . next-end) (first rest)
                              (> (- next-end next-start) 1)))))
              (write-char #\- stream))
            (setf position end))
          (convert position suffix))
        (copy-verbatim suffix (length string))
        (when earmuffs (write-sequence earmuffs stream))))))

(defstruct (bijection
            (:constructor %make-bijection (a2b b2a))
            (:predicate bijectionp)
            (:copier nil))
  "A bijection describes a pairs of objects of some sets A and B, and
features efficient lookup of the B object corresponding to a supplied A object
or the A object corresponding to supplied B object."
  (a2b nil
   :type hash-table
   :read-only t)
  (b2a nil
   :type hash-table
   :read-only t))

(defun make-bijection (&key (domain-equality 'eql) (codomain-equality 'eql))
  (%make-bijection
   (make-hash-table :test domain-equality)
   (make-hash-table :test codomain-equality)))

(defun bijection-value (bijection key)
  (declare (bijection bijection))
  (gethash key (bijection-a2b bijection)))

(defun bijection-key (bijection value)
  (declare (bijection bijection))
  (gethash value (bijection-b2a bijection)))

(defun bijection-add (bijection a b-generator)
  (let ((a2b (bijection-a2b bijection))
        (b2a (bijection-b2a bijection)))
    ;; Ensure that there is no collision in A.
    (when (nth-value 1 (gethash a a2b))
      (error "The key ~A exists already."
             a))
    ;; Ensure that there is no collision in B.
    (loop for b = (funcall b-generator) do
      (unless (nth-value 1 (gethash b b2a))
        ;; Add a new entry.
        (setf (gethash a (bijection-a2b bijection)) b)
        (setf (gethash b (bijection-b2a bijection)) a)
        (loop-finish)))))

(defun sequential-generator (fn)
  (let ((n -1))
    (lambda ()
      (incf n)
      (funcall fn n))))

(defun lisp-name-generator (python-name package)
  (let ((lisp-name (lisp-style-name python-name)))
    (sequential-generator
     (lambda (n)
       (intern
        (if (zerop n)
            lisp-name
            (format nil "~A-~D" lisp-name n))
        package)))))

(defun python-name-generator (lisp-name)
  (let ((python-name (python-style-name lisp-name)))
    (sequential-generator
     (lambda (n)
       (if (zerop n)
           python-name
           (format nil "~A_~D" python-name n))))))

(defun python-class-name-generator (lisp-name)
  (let ((python-name (python-style-class-name lisp-name)))
    (sequential-generator
     (lambda (n)
       (if (zerop n)
           python-name
           (format nil "~A_~D" python-name n))))))

(defun ensure-package (name)
  (declare (string name))
  (or (find-package name)
      (make-package name)))

(defun python-to-lisp-naming (module-name module-contents)
  "Turn a Python module name and list of strings describing the bindings therein
into a bijection from each such name to the corresponding Lisp symbol."
  (let ((package (ensure-package (format nil "LANG.PYTHON.~:@(~A~)" module-name)))
        (bijection (make-bijection :domain-equality 'equal :codomain-equality 'eq)))
    (mapc
     (lambda (python-name)
       (bijection-add bijection python-name (lisp-name-generator python-name package)))
     (sort module-contents #'string<))
    (values package bijection)))

(defun lisp-to-python-naming (package)
  "Turn a Lisp package into three bijections describing the functions, variables,
and types of the corresponding Python module."
  (declare (package package))
  (let ((symbols (loop for symbol being the symbols of package collect symbol))
        (f-names (make-bijection :domain-equality 'eql :codomain-equality 'equal))
        (v-names (make-bijection :domain-equality 'eql :codomain-equality 'equal))
        (t-names (make-bijection :domain-equality 'eql :codomain-equality 'equal)))
    ;; Populate the collision-table.
    (loop for symbol in symbols for lisp-name = (symbol-name symbol) do
      (when (fboundp symbol)
        (bijection-add f-names symbol (python-name-generator lisp-name)))
      (when (boundp symbol)
        (bijection-add v-names symbol (python-name-generator lisp-name)))
      (when (find-class symbol nil)
        (bijection-add t-names symbol (python-class-name-generator lisp-name))))
    (values f-names v-names t-names)))
