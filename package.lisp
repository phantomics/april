;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; package.lisp

(defpackage #:april
  (:export #:april #:april-f #:april-p #:april-c #:april-load #:with-april-context
	   #:array-to-nested-vector #:apl-format-array #:april-create-workspace #:april-clear-workspace)
  (:use #:cl #:aplesque #:vex #:alexandria #:array-operations #:lparallel #:decimals #:cl-ppcre #:parse-number
	#:cl-cpus #:symbol-munger #:prove #:simple-date-time #:trivia)
  (:import-from #:uiop #:operating-system #:run-program)
  (:shadowing-import-from #:array-operations #:split #:flatten))
