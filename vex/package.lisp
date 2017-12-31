;;;; package.lisp

(defpackage #:vex
  (:export #:vex-spec #:ambivalent #:monadic #:dyadic #:boolean-op #:args)
  (:use #:cl #:alexandria #:array-operations #:maxpc #:cl-ppcre #:symbol-munger #:prove)
  (:shadowing-import-from #:array-operations #:split #:flatten))

