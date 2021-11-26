;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; patterns.lisp

(in-package #:april)

"A set of optimization patterns for April; these patterns are matched before more basic language structures are recognized by the compiler. Optimized code for common APL language idioms is implemented in this way."

(defun match-function-patterns (tokens axes space params)
  (match tokens
    ((list* (guard index (equalp index '(:fn #\⍳)))
            (guard reduce (equalp reduce '(:op :lateral #\/)))
            (guard plus (equalp plus '(:fn #\+)))
            rest)
     (let ((arg (gensym)) (var (gensym)))
       (values `(lambda (⍵ &optional ⍺)
                  (if ⍺ ,(build-value `(⍵ ,@(subseq tokens 0 3) ⍺)
                                      :axes axes :space space
                                      :params (append (list :ignore-patterns t) params))
                        (funcall (lambda (,arg)
                                   (if (< ,arg 10000000) (iota-sum ,arg)
                                       (loop :for ,var :from 0 :to (disclose ,arg) :summing ,var)))
                                 ⍵)))
               rest)))
    ((list* (guard ravel (equalp ravel '(:fn #\,)))
            (guard shape (equalp shape '(:fn #\⍴)))
            rest)
     (if axes (values nil tokens)
         (values `(lambda (⍵ &optional ⍺)
                    (if ⍺ ,(build-value `(⍵ ,@(subseq tokens 0 2) ⍺)
                                        :axes axes :space space
                                        :params (append (list :ignore-patterns t) params))
                          (size ⍵)))
                 rest)))
    ((list* (guard ravel (equalp ravel '(:fn #\,)))
            (guard rotate (or (equalp rotate '(:fn #\⌽))
                              (equalp rotate '(:fn #\⊖))))
            (guard disclose (equalp disclose '(:fn #\⊃)))
            rest)
     (values `(lambda (⍵ &optional ⍺)
                (if ⍺ ,(build-value `(⍵ ,@(subseq tokens 0 3) ⍺)
                                    :axes axes :space space
                                    :params (append (list :ignore-patterns t) params))
                      (get-last-row-major ⍵)))
             rest))
    ((list* (guard shape (equalp shape '(:fn #\⍴)))
            (guard shape2 (equalp shape2 '(:fn #\⍴)))
            rest)
     (values `(lambda (⍵ &optional ⍺)
                (if ⍺ ,(build-value `(⍵ ,@(subseq tokens 0 2) ⍺)
                                    :axes axes :space space
                                    :params (append (list :ignore-patterns t) params))
                      (get-rank ⍵)))
             rest))
    ((list* (guard ravel (equalp ravel '(:fn #\,))) ;; TODO: problem with this and display function
            (guard unique (equalp unique '(:fn #\∪)))
            rest)
     (values `(lambda (⍵ &optional ⍺)
                (if ⍺ ,(build-value `(⍵ ,@(subseq tokens 0 2) ⍺)
                                    :axes axes :space space
                                    :params (append (list :ignore-patterns t) params))
                      (n-rank-uniques ⍵)))
             rest))))
