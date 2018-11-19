;;;; package.lisp

(defpackage #:vex
  (:export #:local-idiom #:vex-spec #:vex-program #:ambivalent #:monadic #:dyadic #:reverse-op #:boolean-op
	   #:reverse-boolean-op #:args #:of-state #:of-functions #:of-operators #:of-overloaded?
	   #:set-composer-primitives)
  (:use #:cl #:alexandria #:maxpc #:cl-ppcre #:symbol-munger #:prove))

