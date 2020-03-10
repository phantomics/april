;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; package.lisp

(defpackage #:april
  (:export #:april #:april-f #:april-p #:april-c #:april-load #:with-april-context #:array-to-nested-vector)
  (:use #:cl #:aplesque #:vex #:alexandria #:array-operations #:decimals
	#:cl-ppcre #:parse-number #:symbol-munger #:prove #:simple-date-time)
  (:shadowing-import-from #:array-operations #:split #:flatten))
