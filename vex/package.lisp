;;;; package.lisp

(defpackage #:vex
  (:export #:vex-spec #:vex-program #:ambivalent #:monadic #:dyadic
	   #:boolean-op #:args #:of-environment)
  (:use #:cl #:alexandria #:array-operations #:maxpc #:cl-ppcre #:symbol-munger #:prove)
  (:shadowing-import-from #:array-operations #:split #:flatten))

