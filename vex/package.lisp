;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Vex -*-
;;;; package.lisp

(defpackage #:vex
  (:export #:local-idiom #:vex-program #:ambivalent #:monadic #:dyadic #:reverse-op #:boolean-op
	   #:reverse-boolean-op #:args #:of-functions #:of-operators #:of-overloaded? #:composer
	   #:set-composer-elements #:set-composer-patterns #:get-system-meta #:set-system-meta
	   #:specify-vex-idiom #:extend-vex-idiom #:of-lexicon)
  (:use #:cl #:alexandria #:maxpc #:cl-ppcre #:symbol-munger #:prove))

