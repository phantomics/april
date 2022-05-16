;;;; package.lisp

(defpackage #:varray
  (:use #:cl)
  (:export #:varray #:varrayp #:etype-of #:shape-of #:rank-of #:indexer-of #:render
           #:vvector-integer-progression #:vader-operate
           #:vader-shape #:vader-reshape #:vader-pare #:vader-catenate
           #:vader-mix #:vader-split #:vader-section #:vader-enclose #:vader-partition
           #:vader-turn #:vader-permute #:vader-expand)
  (:shadowing-import-from #:aplesque #:enclose #:disclose #:disclose-unitary #:get-dimensional-factors
                          #:assign-element-type #:type-in-common #:apl-array-prototype
                          #:apply-scalar #:is-unitary #:enclose-atom)
  (:shadowing-import-from #:aplesque.forms #:indexer-split #:indexer-section
                          #:indexer-expand #:indexer-turn #:indexer-permute))
