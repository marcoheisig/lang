(in-package #:cl-user)

(defpackage #:lang.python
  (:use #:closer-common-lisp))

(in-package #:lang.python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Python Modules as a Lisp Packages
;;;
;;; The goal of this file is to make Python modules available as ASDF systems
;;; and Lisp packages.  This is achieved by looking up all the installed Python
;;; libraries via pkg_resources.working_set, turning them into stub ASD files
;;; and Lisp files that load the Python library and make its symbols accessible
;;; as a Lisp package, placing all these files in a directory, and adding that
;;; directory to ASDF's central registry.
