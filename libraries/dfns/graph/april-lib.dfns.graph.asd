;;;; april-lib.dfns.graph.asd

(asdf:defsystem #:april-lib.dfns.graph
  :description "April library implementing Dyalog graph function."
  :author "Andrew Sengul"
  :license "Apache-2.0"
  :serial t
  :depends-on ("april" "april-lib.dfns.array")
  :components ((:file "package")
               (:file "setup")
               (:file "demo")))
