;;;; april-lib.dfns.power.asd

(asdf:defsystem #:april-lib.dfns.power
  :description "April library implementing Dyalog power operators."
  :author "Andrew Sengul"
  :license "Apache-2.0"
  :version "0.0.1"
  :serial t
  :depends-on ("april")
  :components ((:file "package")
               (:file "setup")
               (:file "demo")))
