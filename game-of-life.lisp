;;;; game-of-life.lisp

(in-package #:april)

#|
Just for fun, an implementation of an old APL standby - Conway's Game of Life.

Usage:

To create a new playfield of a given -width- and -height-, evaluate:

(life -width- -height-)

The new field will contain a random arrangement of cells.

For example, (life 64 32) creates a field 64 wide by 32 tall.

If a single number is passed as the argument, it will be both the length and width of the field.

Therefore, evaluating (life 50) will create a 50x50 playfield.

To calculate the next generation, just evaluate:

(life)

If no playfield exists, evaluating (life) will create a new 16x16 playfield.
|#

;; (defparameter *life-default-dimension* 16)

;; (let ((life-array nil)
;;       (life-generation -1))
;;   (defun life (&optional new-width new-height)
;;     "Create or update a playfield for Conway's Game of Life."
;;     (setq life-array (if (or new-width (not life-array))
;; 			 (let* ((new-width (if new-width new-width *life-default-dimension*))
;; 				(new-height (if new-height new-height new-width)))
;; 			   (setq life-generation -1)
;; 			   (april (with (:state :index-origin 0 :in ((-w new-width) (-h new-height))))
;; 				  "?H W⍴2"))
;; 			 (april (with (:state :in ((-l life-array))))
;; 				"⊃1 L∨.∧3 4=+/,1 0 ¯1∘.⊖1 0 ¯1⌽¨⊂L")))
;;     (incf life-generation)
;;     (princ (april (with (:state :index-origin 0 :output-printed :only :in ((-l life-array))))
;; 		  "' ⍬_║▐▀'[(0,(1+⊃⌽⍴L)/2)⍪(3,L,4)⍪5]"))
;;     (list :generation life-generation)))
