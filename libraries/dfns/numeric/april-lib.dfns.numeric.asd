;;;; april-lib.dfns.numeric.asd

(asdf:defsystem #:april-lib.dfns.numeric
  :description "Demo of April used to implement Dyalog numeric functions."
  :author "Andrew Sengul"
  :license  "Apache-2.0"
  :version "0.0.1"
  :serial t
  :depends-on ("april")
  :components ((:file "package")
               (:file "setup")
               (:file "demo")))
