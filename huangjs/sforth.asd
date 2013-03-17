;;;; sforth.asd

(asdf:defsystem #:sforth
  :serial t
  :description "Simple Forth Interpreter"
  :author "Jianshi Huang"
  :license "BSD"
  :depends-on (:alexandria
               :iterate
               :stefil)
  :components ((:file "package")
               (:file "sforth")
               (:file "words")
               (:file "test")))

