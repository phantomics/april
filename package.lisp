;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; package.lisp

(defpackage #:april
  (:export #:april #:april-f #:april-p #:april-c #:april-load
           #:with-april-context #:april-create-workspace #:april-clear-workspace)
  (:use #:cl #:aplesque #:vex #:alexandria #:array-operations #:lparallel #:decimals #:cl-ppcre
        #:parse-number #:symbol-munger #:prove #:simple-date-time #:trivia #:random-state)
  (:shadowing-import-from #:array-operations #:flatten)
  (:shadowing-import-from #:cl-ppcre #:split))
