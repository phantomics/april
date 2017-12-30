;;;; vex.asd

(asdf:defsystem #:vex
  :description "A set of templates for implementing your own vector programming language that compiles to Common Lisp."
  :author "Andrew Sengul"
  :license "Apache-2.0"
  :serial t
  :components ((:file "package")
               (:file "vex")))

