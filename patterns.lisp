 ;;;; grammar.lisp

(in-package #:april)

(set-composer-patterns
 composer-optimized-patterns-common
 (with :idiom-symbol idiom :space-symbol workspace :process-symbol process
       :properties-symbol properties :precedent-symbol precedent)
 (sum-until-pattern
  ;; match the assignment of a function result to a value, like a+←5
  ((:with-preceding-type :array)
   (index-function :element (function :glyph ⍳))
   (reduce-operator :element (operator :glyph /))
   (add-function :element (function :glyph +)))
  (let ((var (gensym)))
    `(loop :for ,var :from 0 :to (disclose ,precedent) :summing ,var))
  (list :type (list :array :evaluated :via-sum-until-pattern)))
 (rank-or-reshape-rank-pattern
  ((:with-preceding-type :array)
   (shape-function-1 :element (function :glyph ⍴))
   (shape-function-2 :element (function :glyph ⍴))
   (value :element (array :cancel-if :pivotal-composition) :optional t :times :any))
  (if value `(reshape-array-fitting (vector (rank ,precedent)) ,(if (numberp value)
								    `(list ,value) `(enclose ,value)))
      `(rank ,precedent))))
