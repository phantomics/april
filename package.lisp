;;;; package.lisp

(defpackage #:apex
  (:export #:apex)
  (:use #:cl #:aplesque #:vex #:alexandria #:array-operations #:maxpc #:cl-slice
	#:cl-ppcre #:parse-number #:symbol-munger #:prove)
  (:shadowing-import-from #:array-operations #:split #:flatten))
