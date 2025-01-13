(defsystem :lang.internals
  :description "Merge multiple programming languages into one."
  :author "Marco Heisig <marco@heisig.xyz"
  :license "MIT"
  :depends-on
  ("alexandria"
   "cffi"
   "closer-mop"
   "lang"
   "trivial-garbage")

  :serial t
  :components
  ((:static-file "gencore")
   (:file "packages")
   (:file "naming")
   (:file "pyobject")
   (:file "foreign")
   (:file "bootstrap")
   (:file "generic-functions")
   (:file "mirror-into-lisp")
   (:file "mirror-into-python")
   (:file "early-methods")
   (:file "modules")
   (:file "methods")
   (:file "python")
   (:file "convert")
   (:file "lispify")
   (:file "pythonize")))
