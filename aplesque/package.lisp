;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Aplesque -*-
;;;; package.lisp

(defpackage #:aplesque
  (:export #:varef #:array-promote #:array-match #:array-depth #:section #:array-to-list #:apply-marginal
	   #:expand #:enlist #:array-inner-product #:index-of #:grade #:array-grade #:nest #:enclose
	   #:alpha-compare #:array-compare #:find-array #:ravel #:across #:disclose #:re-enclose
	   #:reshape-to-fit #:sprfact #:binomial #:scale-array #:mix-arrays #:array-inner-product
	   #:is-unitary #:choose #:catenate #:laminate #:enclose-atom #:partitioned-enclose #:split-array
	   #:invert-matrix #:interval-index #:turn #:partition-array #:merge-arrays #:stencil
	   #:array-impress #:matrix-print #:disclose-unitary-array #:apply-scalar #:each-boolean
	   #:get-first-or-disclose #:assign-element-type #:type-in-common #:initialize-for-environment
	   #:array-outer-product #:inverse-outer-product #:copy-nested-array #:disclose2)
  (:use #:cl #:cl-ppcre #:alexandria #:array-operations #:parse-number #:symbol-munger #:lparallel #:cl-cpus)
  (:shadowing-import-from #:array-operations #:flatten)
  (:shadowing-import-from #:cl-ppcre #:split))
