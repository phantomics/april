;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Varray -*-
;;;; package.lisp

(defpackage #:varray
  (:use #:cl)
  (:export #:varray #:varrayp #:etype-of #:shape-of #:size-of #:rank-of #:generator-of #:strides-of
           #:assign-rank #:vrender #:vapri-apro-vector #:vapri-coordinate-identity #:vapri-onehot-vector
           #:vader-calculate #:vader-select #:vader-random #:vader-deal #:vader-without
           #:vader-umask #:vader-index #:vader-shape #:vader-reshape #:vader-depth #:vader-first-dim
           #:vader-compare #:vader-enlist #:vader-membership #:vader-find #:vader-where
           #:vader-inverse-where #:vader-interval-index #:vader-pare #:vader-catenate
           #:vader-mix #:vader-split #:vader-section #:vader-enclose #:vader-partition
           #:vader-pick #:vader-intersection #:vader-unique #:vader-union #:vader-turn
           #:vader-permute #:vader-expand #:vader-grade #:vader-matrix-inverse
           #:vader-matrix-divide #:vader-encode #:vader-decode #:vader-identity #:vader-subarray
           #:vader-subarray-displaced #:vader-composing #:op-compose #:vacomp-reduce
           #:vacomp-scan #:vacomp-each #:vacomp-produce #:vacomp-stencil
           #:inverse-count-to #:varray-compare)
  (:shadowing-import-from #:alexandria #:iota)
  #+(not clasp) (:shadowing-import-from #:serapeum #:count-cpus)
  (:shadowing-import-from #:random-state #:make-generator #:random-int #:random-float)
  (:shadowing-import-from #:lparallel #:pdotimes #:promise #:fulfill #:force #:fulfilledp)
  (:shadowing-import-from #:aplesque #:varef #:sprfact #:enclose #:disclose
                          #:disclose-unitary #:assign-element-type #:type-in-common
                          #:apply-scalar #:is-unitary #:enclose-atom
                          #:array-compare #:index-of #:alpha-compare #:permute-axes
                          #:grade #:array-grade #:vector-grade #:invert-matrix
                          #:left-invert-matrix #:array-inner-product)
  #+sbcl (:shadowing-import-from #:sb-c #:define-vop)
  #+sbcl (:shadowing-import-from #:sb-assem #:inst)
  #+sbcl (:shadowing-import-from #:sb-vm #:unsigned-num #:unsigned-reg)
  #+(and sbcl x86-64) (:shadowing-import-from #:sb-x86-64-asm #:ea)
  (:shadowing-import-from #:aplesque.forms #:indexer-split #:indexer-expand))
