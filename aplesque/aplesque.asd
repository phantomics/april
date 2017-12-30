;;;; aplesque.asd

(asdf:defsystem #:aplesque
  :description "A collection of array manipulation functions patterned after functions from the APL language."
  :author "Andrew Sengul"
  :license "Apache-2.0"
  :serial t
  :depends-on (:alexandria :array-operations :parse-number :symbol-munger)
  :components ((:file "package")
               (:file "aplesque")))

