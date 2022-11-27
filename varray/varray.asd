;;;; varray.asd

(asdf:defsystem #:varray
  :description "Varray implements virtual arrays - objects representing deferred array transformations."
  :author "Andrew Sengul"
  :license "Apache-2.0"
  :version "1.0.0"
  :serial t
  :depends-on ("aplesque" "serapeum" "lparallel" "random-state")
  :components ((:file "package")
               (:file "macros")
               (:file "core")
               (:file "index")
               (:file "primal")
               (:file "derived")
               ;; (:file "combinatorics")
               (:file "composed")))
