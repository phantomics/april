;;;; april-demo.fnn.asd

(asdf:defsystem #:april-demo.fnn
  :description "Demo of April used to forward-feeding neural network."
  :author "Andrew Sengul"
  :license  "Apache-2.0"
  :serial t
  :depends-on ("april" "lisp-binary" "april-lib.dfns.array")
  :components ((:file "package")
               (:file "setup")
               (:file "demo")))
