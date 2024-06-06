(in-package #:lang)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Python Modules as a Lisp Packages
;;;
;;; This file makes Python modules available as ASDF systems and Lisp packages.

(defun python-project-searcher (system-name)
  (multiple-value-bind (lang module-name)
      (lang:parse-system-name system-name)
    (when (and (equal lang "python")
               (stringp module-name))
      (lang:ensure-asdf-wrapper
       system-name
       '()
       `((lang.internals::lisp-package-from-python-module
          ,(string-upcase system-name)
          ,module-name))))))

(lang:register-lang-searcher "python" 'python-project-searcher)
