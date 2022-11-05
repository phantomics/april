;;;; varray.asd

(asdf:defsystem #:varray
  :description "Describe varray here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("aplesque" "serapeum")
  :components ((:file "package")
               (:file "setup")
               (:file "combinatorics")
               (:file "varray")))
