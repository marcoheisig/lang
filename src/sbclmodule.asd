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
   (:file "pyapi")
   (:file "sbclmodule")
   (:file "python")))
