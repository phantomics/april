;;;; april-lib.dfns.tree.asd

(asdf:defsystem #:april-lib.dfns.tree
  :description "April library implementing Dyalog tree functions."
  :author "Andrew Sengul"
  :license "Apache-2.0"
  :serial t
  :depends-on ("april" "april-lib.dfns.array" "april-lib.dfns.power")
  :components ((:file "package")
               (:file "setup")
               (:file "demo")))
