(defsystem :sbclmodule
  :description "Bringing SBCL to Python and vice versa."
  :author "Marco Heisig <marco@heisig.xyz"
  :license "MIT"
  :depends-on
  ("bordeaux-threads"
   "cffi"
   "closer-mop"
   "alexandria"
   "trivial-garbage")

  :components
  ((:file "packages")
   (:file "foreign")
   (:file "low-level")
   (:file "mirror-into-lisp")
   (:file "mirror-into-python")
   #+(or)
   (:file "sbclmodule")
   #+(or)
   (:file "lispify")
   #+(or)
   (:file "pythonize")
   #+(or)
   (:file "python")))
