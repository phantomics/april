;;;; varray.asd

(asdf:defsystem #:varray
  :description "Varray implements virtual arrays - objects representing deferred array transformations."
  :author "Andrew Sengul"
  :license "Apache-2.0"
  :version "1.0.0"
  :serial t
  :depends-on ("aplesque" ;; #+(not clasp) "serapeum" ;; there's a better way to do this
                          (:feature (:not :clasp) "serapeum")
                          "lparallel" "random-state")
  :components ((:file "package")
               (:file "macros")
               (:file "base")
               (:file "index")
               (:file "primal")
               (:file "derived")
               (:file "calculate")
               (:file "select")
               (:file "logic")
               (:file "composed")
               (:file "effectors/x86" :if-feature (:and :sbcl :x86-64))))
