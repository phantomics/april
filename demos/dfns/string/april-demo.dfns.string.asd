;;;; april-demo.dfns.string.asd

(asdf:defsystem #:april-demo.dfns.string
  :description "Demo of April used to implement Dyalog string functions"
  :author "Andrew Sengul"
  :license  "Apache-2.0"
  :version "0.0.1"
  :serial t
  :depends-on ("april")
  :components ((:file "package")
               (:file "setup")
               (:file "demo")))
