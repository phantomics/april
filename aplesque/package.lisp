;;;; package.lisp

(defpackage #:aplesque
  (:export #:array-promote #:array-match #:array-depth #:multidim-slice #:scan-back #:make-back-scanner
	   #:array-to-list #:apply-marginal #:expand-array #:enlist #:array-inner-product #:index-of
	   #:grade #:array-grade #:alpha-compare #:array-compare #:find-array #:ravel #:across #:disclose
	   #:reshape-array-fitting #:sprfact #:binomial #:make-rotator #:scale-array #:mix-arrays
	   #:is-unitary #:aref-eliding #:subprocess #:rotate-left #:rotate-right #:catenate #:laminate
	   #:partitioned-enclose #:re-enclose #:invert-matrix #:interval-index #:disclose-unitary-array
	   #:matrix-render #:matrix-print #:combine-arrays)
  (:use #:cl #:alexandria #:array-operations #:parse-number #:symbol-munger)
  (:shadowing-import-from #:array-operations #:flatten))
