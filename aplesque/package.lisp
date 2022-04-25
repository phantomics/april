;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Aplesque -*-
;;;; package.lisp

(defpackage #:aplesque
  (:export #:varef #:array-promote #:array-match #:array-depth #:section #:array-to-list #:apply-marginal
           #:expand #:enlist #:array-inner-product #:index-of #:grade #:array-grade #:nest #:enclose
           #:alpha-compare #:array-compare #:find-array #:ravel #:across #:re-enclose #:duplicate
           #:reshape-to-fit #:sprfact #:binomial #:scale-array #:mix-arrays #:array-inner-product
           #:is-unitary #:choose #:catenate #:laminate #:enclose-atom #:partitioned-enclose #:split-array
           #:invert-matrix #:interval-index #:turn #:partition-array #:stencil #:count-segments
           #:array-impress #:matrix-print #:disclose-unitary #:apply-scalar #:reduce-array #:permute-axes
           #:get-first-or-disclose #:assign-element-type #:type-in-common #:initialize-for-environment
           #:array-outer-product #:inverse-outer-product #:copy-nested-array #:disclose #:xdotimes
           #:ydotimes #:get-dimensional-factors #:is-integer-array

           #:apl-array-prototype #:make-empty-array
           ;; IPV-TODO: do these last 2 need to be exported?
           )
  (:use #:cl)
  (:shadowing-import-from #:alexandria #:iota #:copy-array #:rotate)
  (:shadowing-import-from #:array-operations #:flatten #:dims #:size #:rank #:element-type #:split)
  (:shadowing-import-from #:parse-number #:parse-number)
  (:shadowing-import-from #:array-operations #:flatten #:dims #:size #:rank #:element-type)
  (:shadowing-import-from #:lparallel #:pdotimes)
  (:shadowing-import-from #:cl-ppcre #:split))
