;;;; april-lib.dfns.numeric.asd

(asdf:defsystem #:april-lib.dfns.numeric
  :description "April library implementing Dyalog numeric functions."
  :author "Andrew Sengul"
  :license "Apache-2.0"
  :serial t
  :depends-on ("april" "april-lib.dfns.graph")
  :components ((:file "package")
               (:file "setup")
               (:file "demo")))
