(in-package #:cl-user)

(asdf:load-systems
 :alexandria
 :cffi
 :closer-mop)

;; This is a hack - we define an alien callable named posix-argv only so that
;; we can supply it as callable export to save-lisp-and-die.  We need to supply
;; at least one callable export to prevent SBCL from starting a REPL.
(sb-alien:define-alien-callable ("posix_argv" %posix-argv) sb-alien:void ())

(defun load-lang ()
  (handler-case (asdf:load-system "lang.internals" :force t :verbose t)
    (error (e)
      (format t "Caught error ~A." e)
      (finish-output))))

(pushnew 'load-lang sb-ext:*init-hooks*)

(sb-ext:save-lisp-and-die *lang-core* :callable-exports '(("posix_argv" %posix-argv)))

