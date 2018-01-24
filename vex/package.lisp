;;;; package.lisp

(defpackage #:vex
  (:export #:vex-spec #:vex-program #:ambivalent #:monadic #:dyadic #:reverse-op #:boolean-op
	   #:reverse-boolean-op #:args #:of-state #:of-functions #:of-operators #:of-overloaded?)
  (:use #:cl #:alexandria #:maxpc #:cl-ppcre #:symbol-munger #:prove))

