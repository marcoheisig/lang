(defsystem :ouroboros
  :description "Bringing SBCL to Python and vice versa."
  :author "Marco Heisig <marco@heisig.xyz"
  :license "MIT"
  :depends-on
  ("alexandria"
   "bordeaux-threads"
   "cffi"
   "cl-ppcre"
   "closer-mop"
   "named-readtables"
   "trivial-garbage")

  :components
  ((:static-file "gencore")
   (:file "packages")
   (:file "pyobject")
   (:file "foreign")
   (:file "low-level")
   (:file "mirror-into-lisp")
   (:file "mirror-into-python")
   (:file "python-syntax")
   (:file "python-builtins")
   (:file "convert")
   (:file "lispify")
   (:file "pythonize")
   (:file "python-keywords")
   (:file "python-methods")
   (:file "ouroboros")))
