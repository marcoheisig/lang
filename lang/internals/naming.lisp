(in-package #:lang.internals)

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
