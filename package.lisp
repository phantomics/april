;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; package.lisp

(defpackage #:april
  (:export #:april #:april-f #:april-c #:april-load #:with-april-context
           #:april-create-workspace #:april-clear-workspace
           #:april-print-progress-bar)
  (:use #:cl #:aplesque #:vex #:varray)
  (:shadowing-import-from #:aplesque.forms #:indexer-section #:indexer-expand)
  (:shadowing-import-from #:array-operations #:flatten #:dims #:size #:rank #:element-type)
  (:shadowing-import-from #:cl-ppcre #:split #:regex-replace-all)
  (:shadowing-import-from #:prove #:plan #:is #:finalize)
  (:shadowing-import-from #:alexandria #:iota #:copy-array)
  (:shadowing-import-from #:lparallel #:pdotimes)
  (:shadowing-import-from #:parse-number #:parse-number)
  (:shadowing-import-from #:symbol-munger #:lisp->camel-case)
  (:shadowing-import-from #:simple-date-time #:now #:year-of #:month-of #:day-of #:hour-of
                          #:minute-of #:second-of #:millisecond-of)
  (:shadowing-import-from #:trivia #:match #:guard)
  (:shadowing-import-from #:random-state #:make-generator #:random-int #:random-float)
  (:shadowing-import-from #:cl-unicode #:general-category))
