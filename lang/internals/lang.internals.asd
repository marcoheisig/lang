(defsystem :lang.internals
  :description "Merge multiple programming languages into one."
  :author "Marco Heisig <marco@heisig.xyz"
  :license "MIT"
  :depends-on
  ("alexandria"
   "cffi"
   "closer-mop"
   "lang"
   "trivial-package-local-nicknames"
   "trivial-garbage")

  :serial t
  :components
  ((:static-file "gencore")
   (:file "00-packages")
   (:file "01-naming")
   (:file "02-pyobject")
   (:file "03-foreign")
   (:file "04-bootstrap")
   (:file "05-generic-functions")
   (:file "06-mirror-into-lisp")
   (:file "07-mirror-into-python")
   (:file "08-modules")
   (:file "09-methods")
   #+(or)
   (:file "10-lambda-lists")
   (:file "11-python")
   (:file "12-convert")
   (:file "13-lispify")
   (:file "14-pythonize")))
