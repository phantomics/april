;;;; april-lib.dfns.string.asd

(asdf:defsystem #:april-lib.dfns.string
  :description "April library implementing Dyalog string functions."
  :author "Andrew Sengul"
  :license "Apache-2.0"
  :serial t
  :depends-on ("april" "april-lib.dfns.array")
  :components ((:file "package")
               (:file "setup")
               (:file "demo")))
