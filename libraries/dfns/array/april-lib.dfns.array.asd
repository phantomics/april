;;;; april-lib.dfns.array.asd

(asdf:defsystem #:april-lib.dfns.array
  :description "April library implementing Dyalog array functions."
  :author "Andrew Sengul"
  :license "Apache-2.0"
  :serial t
  :depends-on ("april")
  :components ((:file "package")
               (:file "setup")
               (:file "demo")))
