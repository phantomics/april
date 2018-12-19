;;;; package.lisp

(defpackage #:vex
  (:export #:local-idiom #:vex-spec #:vex-program #:ambivalent #:monadic #:dyadic #:reverse-op #:boolean-op
	   #:reverse-boolean-op #:args #:of-functions #:of-operators #:of-overloaded?
	   #:composer #:set-composer-elements #:set-composer-patterns #:of-system)
  (:use #:cl #:alexandria #:maxpc #:cl-ppcre #:symbol-munger #:prove))

