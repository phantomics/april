;;;; april.asd

(asdf:defsystem #:april
  :description "April is a subset of the APL programming language that compiles to Common Lisp."
  :author "Andrew Sengul"
  :license "Apache-2.0"
  :serial t
  :depends-on (:aplesque :vex :alexandria :array-operations
	       :cl-ppcre :parse-number :symbol-munger :prove)
  :components 
  ((:file "package")
   (:file "utilities")
   (:file "grammar")
   (:file "spec")))
