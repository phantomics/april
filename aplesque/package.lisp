;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Aplesque -*-
;;;; package.lisp

(defpackage #:aplesque
  (:export #:array-promote #:array-match #:array-depth #:section #:array-to-list #:apply-marginal
	   #:expand-array #:enlist #:array-inner-product #:index-of #:grade #:array-grade
	   #:alpha-compare #:array-compare #:find-array #:ravel #:across #:disclose #:re-enclose
	   #:reshape-to-fit #:sprfact #:binomial #:scale-array #:mix-arrays #:array-inner-product
	   #:is-unitary #:choose #:catenate #:laminate #:enclose #:partitioned-enclose #:split-array
	   #:invert-matrix #:interval-index #:turn #:partition-array #:merge-arrays #:stencil
	   #:array-impress #:matrix-print #:disclose-unitary-array #:each-boolean #:each-scalar
	   #:assign-element-type #:type-in-common #:initialize-for-environment)
  (:use #:cl #:alexandria #:array-operations #:parse-number #:symbol-munger)
  (:shadowing-import-from #:array-operations #:flatten))
