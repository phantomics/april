;;;; april.asd

(asdf:defsystem "april"
  :description "April is a subset of the APL programming language that compiles to Common Lisp."
  :version "0.9.3"
  :author "Andrew Sengul"
  :license "Apache-2.0"
  :serial t
  :depends-on ("vex" "aplesque" "array-operations" "lparallel" "alexandria" "cl-ppcre" "random-state"
	             "decimals" "parse-number" "symbol-munger" "prove" "simple-date-time" "trivia"
                     "cl-unicode")
  :components ((:file "package")
               (:file "utilities")
               (:file "functions")
               (:file "patterns")
               (:file "grammar")
               (:file "spec")
               (:file "game-of-life")))
