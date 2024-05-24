(in-package #:ouroboros)

(declaim (ftype function read-python-number))

(named-readtables:defreadtable python:syntax
  (:merge :standard)
  (:case :preserve)
  (:macro-char #\0 'read-python-number t)
  (:macro-char #\1 'read-python-number t)
  (:macro-char #\2 'read-python-number t)
  (:macro-char #\3 'read-python-number t)
  (:macro-char #\4 'read-python-number t)
  (:macro-char #\5 'read-python-number t)
  (:macro-char #\6 'read-python-number t)
  (:macro-char #\7 'read-python-number t)
  (:macro-char #\8 'read-python-number t)
  (:macro-char #\9 'read-python-number t))

;;; The number grammar:
;;;
;;; number        ::=  integer | floatnumber | imagnumber
;;; integer       ::=  decinteger | bininteger | octinteger | hexinteger
;;; decinteger    ::=  nonzerodigit (["_"] digit)* | "0"+ (["_"] "0")*
;;; bininteger    ::=  "0" ("b" | "B") (["_"] bindigit)+
;;; octinteger    ::=  "0" ("o" | "O") (["_"] octdigit)+
;;; hexinteger    ::=  "0" ("x" | "X") (["_"] hexdigit)+
;;; nonzerodigit  ::=  "1"..."9"
;;; digit         ::=  "0"..."9"
;;; bindigit      ::=  "0" | "1"
;;; octdigit      ::=  "0"..."7"
;;; hexdigit      ::=  digit | "a"..."f" | "A"..."F"
;;; floatnumber   ::=  pointfloat | exponentfloat
;;; pointfloat    ::=  [digitpart] fraction | digitpart "."
;;; exponentfloat ::=  (digitpart | pointfloat) exponent
;;; digitpart     ::=  digit (["_"] digit)*
;;; fraction      ::=  "." digitpart
;;; exponent      ::=  ("e" | "E") ["+" | "-"] digitpart
;;; imagnumber    ::=  (floatnumber | digitpart) ("j" | "J")

(defparameter *python-number-scanner*
  (flet ((underscore-repetition (min max thing)
           `(:greedy-repetition ,min ,max (:sequence (:greedy-repetition 0 1 #\_) ,thing))))
    (let* ((nonzerodigit
             `(:char-class (:range #\1 #\9)))
           (digit
             `(:char-class (:range #\0 #\9)))
           (hexdigit
             `(:char-class (:range #\0 #\9) (:range #\a #\f) (:range #\A #\F)))
           (octdigit
             `(:char-class (:range #\0 #\7)))
           (bindigit
             `(:char-class #\0 #\1))
           (digitpart
             `(:sequence ,digit ,(underscore-repetition 0 nil digit)))
           (exponent
             `(:sequence
               (:alternation #\e #\E)
               (:greedy-repetition 0 1 (:alternation #\+ #\-))
               ,digitpart))
           (fraction
             `(:sequence #\. ,digitpart))
           (pointfloat
             `(:alternation
               (:sequence (:greedy-repetition 0 1 ,digitpart) ,fraction)
               (:sequence ,digitpart #\.)))
           (exponentfloat
             `(:sequence
               (:alternation ,digitpart ,pointfloat)
               ,exponent))
           (floatnumber
             `(:alternation ,pointfloat ,exponentfloat))
           (imagnumber
             `(:sequence
               (:alternation ,floatnumber ,digitpart)
               (:alternation #\j #\J)))
           (hexinteger
             `(:sequence
               #\0
               (:alternation #\x #\X)
               ,(underscore-repetition 1 nil hexdigit)))
           (octinteger
             `(:sequence
               #\0
               (:alternation #\o #\O)
               ,(underscore-repetition 1 nil octdigit)))
           (bininteger
             `(:sequence
               #\0
               (:alternation #\b #\B)
               ,(underscore-repetition 1 nil bindigit)))
           (decinteger
             `(:alternation
               (:sequence ,nonzerodigit ,(underscore-repetition 0 nil digit))
               (:sequence #\0 ,(underscore-repetition 0 nil #\0))))
           (number
             `(:alternation
               (:register ,imagnumber)
               (:register ,floatnumber)
               (:register ,bininteger)
               (:register ,octinteger)
               (:register ,hexinteger)
               (:register ,decinteger))))
      (cl-ppcre:create-scanner number))))

(defun read-python-number (stream char)
  (let ((buffer (make-array 0 :fill-pointer 0 :adjustable t :element-type 'character)))
    (vector-push-extend char buffer)
    ;; Read characters until we encounter whitespace, EOF, or a terminating
    ;; macro character.
    (loop for char = (peek-char nil stream nil #\space t) do
      (if (not (or (alphanumericp char) (char= char #\_)))
          (loop-finish)
          (vector-push-extend (read-char stream t nil t) buffer)))
    ;; Parse the number
    (multiple-value-bind (start end starts ends)
        (cl-ppcre:scan *python-number-scanner* buffer)
      (unless (and (eql start 0)
                   (eql end (length buffer)))
        (error "Not a Python number: ~A" buffer))
      (flet ((match (k)
               (let ((start (aref starts k))
                     (end (aref ends k)))
                 (and start end
                      (with-output-to-string (stream)
                        (loop for position from start below end
                              for char = (aref buffer position)
                              unless (char= char #\_)
                                do (write-char char stream)))))))
        (let ((imagnumber (match 0))
              (floatnumber (match 1))
              (bininteger (match 2))
              (octinteger (match 3))
              (hexinteger (match 4))
              (decinteger (match 5)))
          (with-global-interpreter-lock-held
            (mirror-into-lisp
             (cond
               (imagnumber
                (break "TODO parse complex"))
               (floatnumber
                (break "TODO parse float"))
               (bininteger
                (pylong-from-long
                 (parse-integer bininteger :start 2 :radix 2)))
               (octinteger
                (pylong-from-long
                 (parse-integer octinteger :start 2 :radix 8)))
               (hexinteger
                (pylong-from-long
                 (parse-integer hexinteger :start 2 :radix 16)))
               (decinteger
                (pylong-from-long
                 (parse-integer decinteger :start 0 :radix 10)))))))))))
