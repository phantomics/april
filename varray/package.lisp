;;;; package.lisp

(defpackage #:varray
  (:use #:cl)
  (:export #:varray #:varrayp #:etype-of #:shape-of #:rank-of #:indexer-of #:render
           #:vvector-integer-progression
           #:vader-shape #:vader-reshape #:vader-section #:vader-turn)
  (:shadowing-import-from #:aplesque #:enclose #:disclose #:assign-element-type #:type-in-common
                          #:apl-array-prototype)
  (:shadowing-import-from #:aplesque.forms #:indexer-section #:indexer-expand #:indexer-turn #:indexer-permute))
