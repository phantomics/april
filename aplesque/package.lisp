;;;; package.lisp

(defpackage #:aplesque
  (:export #:array-match #:array-depth #:multidim-slice #:scan-back #:make-back-scanner
	   #:apply-marginal #:expand-array #:enlist #:array-inner-product #:index-of
	   #:grade #:array-grade #:alpha-compare #:find-array #:run-dim #:invert-matrix
	   #:reshape-array-fitting #:sprfact #:binomial #:make-rotator
	   #:rotate-left #:rotate-right)
  (:use #:cl #:alexandria #:array-operations #:parse-number #:symbol-munger)
  (:shadowing-import-from #:array-operations #:flatten))

