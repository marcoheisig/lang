(defsystem :lang.python
  :description "Bring Python to Lisp."
  :author "Marco Heisig <marco@heisig.xyz"
  :license "MIT"
  :depends-on ("asdf" "lang" "lang.internals")

  :serial t
  :components
  ((:file "python")))
