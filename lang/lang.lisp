(in-package #:cl-user)

(defpackage #:lang
  (:use #:common-lisp)
  (:export
   #:ensure-asdf-wrapper
   #:emit-asdf-wrapper
   #:parse-system-name
   #:register-lang-searcher))

(in-package #:lang)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load Other Language's Libraries With Lisp
;;;
;;; This file defines the infrastructure for wrapping other languages'
;;; libraries as ASDF systems.

(defparameter *lang-searchers*
  (make-hash-table :test #'equal))

(defparameter *lang-systems-directory*
  (uiop:merge-pathnames*
   "lang/"
   (uiop:xdg-cache-home)))

(defun lang-searcher (name)
  (multiple-value-bind (lang module-name)
      (parse-system-name name)
    (when (and lang module-name)
      (multiple-value-bind (lang-searcher presentp)
          (gethash lang *lang-searchers*)
        (when presentp
          (funcall lang-searcher name))))))

(unless (member 'lang-searcher asdf:*system-definition-search-functions*)
  (setf asdf:*system-definition-search-functions*
        (append asdf:*system-definition-search-functions*
                (list 'lang-searcher))))

(defun register-lang-searcher (lang searcher)
  (declare (string lang) (symbol searcher))
  (setf (gethash lang *lang-searchers*)
        searcher))

(defun parse-system-name (system-name)
  (let* ((prefix "lang.")
         (prefixlength (length prefix)))
    (if (string-starts-with system-name prefix)
        (let ((dotpos (position #\. system-name :start prefixlength)))
          (if (or (not dotpos)
                  (= dotpos (1- (length system-name))))
              (values nil nil)
              (values (subseq system-name prefixlength dotpos)
                      (subseq system-name (1+ dotpos)))))
        (values nil nil))))

(defun string-starts-with (string prefix)
  (declare (string string prefix))
  (let ((mismatch (string/= string prefix)))
    (or (not mismatch)
        (= mismatch (length prefix)))))

(defun ensure-asdf-wrapper (system-name dependencies &optional (forms '()))
  (let* ((filename (format nil "~A.asd" system-name))
         (pathname (merge-pathnames filename *lang-systems-directory*)))
    (if (uiop:file-exists-p pathname)
        pathname
        (emit-asdf-wrapper system-name dependencies forms))))

(defun emit-asdf-wrapper (system-name dependencies &optional (forms '()))
  (let ((dir *lang-systems-directory*))
    (ensure-directories-exist dir)
    (multiple-value-bind (lang module-name)
        (parse-system-name system-name)
      (when forms
        (let ((lisp-file (merge-pathnames (format nil "~A.lisp" system-name) dir)))
          (with-open-file (stream lisp-file :direction :output :if-exists :supersede)
            (let ((*package* (find-package "CL-USER")))
              (loop for form in forms do
                (print form stream))))))
      (let ((asd-file (merge-pathnames (format nil "~A.asd" system-name) dir)))
        (with-open-file (stream asd-file :direction :output :if-exists :supersede)
          (let ((*package* (find-package "CL-USER")))
            (print
             `(asdf:defsystem ,system-name
                :description
                ,(format nil "Lang-generated ASDF system for the ~A library ~A."
                         (string-capitalize lang)
                         module-name)
                :depends-on (,(format nil "lang.~A" lang) ,@dependencies)
                ,@(when forms
                    `(:components ((:file ,system-name)))))
             stream)))
        asd-file))))
