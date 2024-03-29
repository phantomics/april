;;;; vex.asd

(asdf:defsystem "vex"
  :description "A set of templates for implementing a vector programming language that compiles to Common Lisp."
  :version "1.0.0"
  :author "Andrew Sengul"
  :license "Apache-2.0"
  :serial t
  :depends-on ("alexandria" "array-operations" "maxpc-apache" "cl-ppcre" "symbol-munger" "prove")
  :components ((:file "package")
               (:file "vex")))
