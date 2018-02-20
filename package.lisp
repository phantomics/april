;;;; package.lisp

(defpackage #:april
  (:export #:april)
  (:use #:cl #:aplesque #:vex #:alexandria #:array-operations
	#:cl-ppcre #:parse-number #:symbol-munger #:prove)
  (:shadowing-import-from #:array-operations #:split #:flatten))
