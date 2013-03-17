;;;; leeforth.asd

(asdf:defsystem #:leeforth
  :serial t
  :description "Little forth"
  :author "albertlee <hanzhupeng@gmail.com>"
  :license "MIT"
  :components ((:file "package")
               (:file "leeforth")))

