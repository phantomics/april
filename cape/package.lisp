;;;; package.lisp

(defpackage #:cape
  (:use #:cl)
  (:export #:attach #:express #:en-value #:en-function #:ex-value #:ex-function #:en-statement
           #:base-idiom #:base-space #:base-expr #:ent-meta #:ent-data #:ent-axes
           #:exp-scope #:exp-assigned  #:enfun-lexicon #:exval-object #:exval-function
           #:exval-predicate #:exfun-primary #:exfun-operator #:exfun-composed #:enstm-clauses))
