;;;; package.lisp

(defpackage #:vex
  (:export #:vex-spec #:vex-program #:ambivalent #:monadic #:dyadic
	   #:boolean-op #:args #:of-state #:of-functions #:of-operators #:of-overloaded?)
  (:use #:cl #:alexandria #:maxpc #:cl-ppcre #:symbol-munger #:prove))

