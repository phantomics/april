;;;; aplesque.asd

(asdf:defsystem "aplesque"
  :description "A collection of array manipulation functions patterned after functions from the APL language."
  :version "0.9.3"
  :author "Andrew Sengul"
  :license "Apache-2.0"
  :serial t
  :depends-on ("cl-ppcre" "alexandria" "array-operations" "parse-number" "symbol-munger" "lparallel")
  :components ((:file "package")
               (:file "aplesque")))
