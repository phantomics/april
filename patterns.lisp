 ;;;; patterns.lisp

(in-package #:april)

(set-composer-patterns
 composer-optimized-patterns-common
 ;; optimized code for common APL idioms
 (with :idiom-symbol idiom :space-symbol workspace :process-symbol process
       :properties-symbol properties :precedent-symbol precedent)
 (sum-until-pattern
  ;; optimize the pattern +/⍳Y to sum until Y
  ((:with-preceding-type :array)
   (index-function :element (function :glyph ⍳))
   (reduce-operator :element (operator :glyph /))
   (add-function :element (function :glyph +)))
  (let ((var (gensym)))
    `(avector (loop :for ,var :from 0 :to (disclose ,precedent) :summing ,var)))
  (list :type (list :array :evaluated :via-sum-until-pattern)))
 (rank-pattern
  ;; optimize the pattern ⍴⍴Y to get the rank of an array
  ((:with-preceding-type :array)
   (shape-functions :element (function :glyph ⍴) :times 2)
   (value :element (array :cancel-if :pivotal-composition) :optional t :times :any))
  (if (not value) `(avector (aops:rank ,precedent)))
  (list :type (list :array :evaluated :via-rank-pattern))))
