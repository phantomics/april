;;;; april-lib.dfns.graph.asd

(asdf:defsystem #:april-lib.dfns.graph
  :description "April library containing graph tools from Dyalog dfns."
  :author "Andrew Sengul"
  :license "Apache-2.0"
  :version "0.0.1"
  :serial t
  :depends-on ("april")
  :components ((:file "package")
               (:file "setup")
               (:file "demo")))
