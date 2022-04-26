;;;; package.lisp

(defpackage #:varray
  (:use #:cl)
  (:export #:varray #:varrayp #:etype-of #:shape-of #:indexer-of #:render
           #:vvector-integer-progression
           #:vader-section #:vader-turn)
  (:shadowing-import-from #:aplesque #:disclose #:assign-element-type #:apl-array-prototype)
  (:shadowing-import-from #:aplesque.forms #:indexer-section #:indexer-expand #:indexer-turn #:indexer-permute))
