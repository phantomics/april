;;;; package.lisp

(defpackage #:varray
  (:use #:cl)
  (:export #:varray #:varrayp #:etype-of #:shape-of #:rank-of #:indexer-of #:render
           #:vvector-integer-progression
           #:vader-shape #:vader-reshape #:vader-catenate
           #:vader-section #:vader-turn #:vader-permute #:vader-expand)
  (:shadowing-import-from #:aplesque #:enclose #:disclose #:disclose-unitary #:get-dimensional-factors
                          #:assign-element-type #:type-in-common #:apl-array-prototype)
  (:shadowing-import-from #:aplesque.forms #:indexer-section #:indexer-expand #:indexer-turn #:indexer-permute))
