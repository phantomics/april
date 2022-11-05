;;;; april.asd

(asdf:defsystem "april"
  :description "April is a subset of the APL programming language that compiles to Common Lisp."
  :version "1.0.0"
  :author "Andrew Sengul"
  :license "Apache-2.0"
  :serial t
  :depends-on ("vex" "aplesque" "array-operations" "lparallel" "alexandria"
                     "cl-ppcre" "random-state" "parse-number" "symbol-munger"
                     "prove" "simple-date-time" "trivia" "cl-unicode" "varray")
  :components ((:file "package")
               (:file "utilities")
               (:file "functions")
               (:file "patterns")
               (:file "grammar")
               (:file "spec")
               (:file "game-of-life")))
