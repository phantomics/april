;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; package.lisp

(defpackage #:april
  (:export #:april #:april-f #:april-p #:april-c #:april-load #:with-april-context #:array-to-nested-vector)
  (:use #:cl #:aplesque #:vex #:alexandria #:array-operations #:lparallel #:decimals #:cl-ppcre #:parse-number
	#:symbol-munger #:prove #:simple-date-time)
  (:import-from #:uiop #:os-genera-p #:os-macosx-p 	#:os-unix-p #:os-windows-p #:run-program)
  (:shadowing-import-from #:array-operations #:split #:flatten))
