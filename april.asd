;;;; april.asd

(asdf:defsystem "april"
  :description "April is a subset of the APL programming language that compiles to Common Lisp."
  :version "1.0.0"
  :author "Andrew Sengul"
  :license "Apache-2.0"
  :serial t
  :depends-on ("vex" "aplesque" "array-operations" "alexandria" "cl-ppcre"
	       "parse-number" "symbol-munger" "prove")
  :components 
  ((:file "package")
   (:file "utilities")
   (:file "grammar")
   (:file "patterns")
   (:file "spec")))
