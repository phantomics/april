;;;; apex.asd

(asdf:defsystem #:apex
  :description "Apex is a subset of the APL programming language that compiles to Common Lisp."
  :author "Andrew Sengul"
  :license "GPL-3.0"
  :serial t
  :depends-on (:alexandria :array-operations :maxpc :cl-slice
	       :cl-ppcre :parse-number :symbol-munger :prove)
  :components 
  ((:file "package")
   (:file "apex")
   (:file "tests")))
