;;;; april-lib.dfns.string.asd

(asdf:defsystem #:april-lib.dfns.string
  :description "April library implementing Dyalog string functions."
  :author "Andrew Sengul"
  :license "Apache-2.0"
  :version "0.0.1"
  :serial t
  :depends-on ("april")
  :components ((:file "package")
               (:file "setup")
               (:file "demo")))
