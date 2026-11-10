(defsystem :lang.python
  :description "Bring Python to Lisp."
  :author "Marco Heisig <marco@heisig.xyz"
  :license "MIT"
  :depends-on ("asdf" "lang" "lang.internals")

  :serial t
  :components
  ((:file "python"))
  :in-order-to ((test-op (test-op "lang.python/test"))))

(defsystem :lang.python/test
  :description "Tests for lang.python."
  :author "Marco Heisig <marco@heisig.xyz"
  :license "MIT"
  :depends-on ("lang.python")

  :pathname "test/"
  :serial t
  :components
  ((:file "packages")
   (:file "harness")
   (:file "smoke")
   (:file "identity")
   (:file "conversion")
   (:file "refcount"))
  :perform (test-op (operation component)
             (unless (uiop:symbol-call '#:lang.python.test '#:run-tests)
               (error "Some tests of lang.python failed."))))
