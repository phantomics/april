;;;; vex.asd

(asdf:defsystem "vex"
  :description "A set of templates for implementing your own vector programming language that compiles to Common Lisp."
  :version "0.8.5"
  :author "Andrew Sengul"
  :license "Apache-2.0"
  :serial t
  :depends-on ("alexandria" "array-operations" "maxpc" "cl-ppcre" "symbol-munger" "prove")
  :components ((:file "package")
               (:file "vex")))

