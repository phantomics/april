;;;; package.lisp

(defpackage #:aplesque
  (:export #:array-promote #:array-match #:array-depth #:section #:scan-back #:make-back-scanner
	   #:array-to-list #:apply-marginal #:expand-array #:enlist #:array-inner-product #:index-of
	   #:grade #:array-grade #:alpha-compare #:array-compare #:find-array #:ravel #:across #:disclose
	   #:reshape-array-fitting #:sprfact #:binomial #:make-rotator #:scale-array #:mix-arrays #:split-array
	   #:is-unitary #:choose #:subprocess #:rotate-left #:rotate-right #:catenate #:laminate #:enclose
	   #:partitioned-enclose #:re-enclose #:invert-matrix #:interval-index #:disclose-unitary-array
	   #:partition-array #:enclose-atom #:merge-arrays #:stencil #:array-impress #:matrix-print
	   #:each-boolean #:each-scalar #:assign-element-type #:type-in-common)
  (:use #:cl #:alexandria #:array-operations #:parse-number #:symbol-munger)
  (:shadowing-import-from #:array-operations #:flatten))
