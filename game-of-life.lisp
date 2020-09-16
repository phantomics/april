;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
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

(defparameter *life-default-dimension* 16)

;; (let ((life-array nil)
;;       (life-generation -1))
;;   (defun life (&optional new-width new-height)
;;     "Create or update a playfield for Conway's Game of Life."
;;     (setq life-array (if (or new-width (not life-array))
;; 			 (progn (setq life-generation -1)
;; 				(april-c (with (:state :index-origin 0)) "{?⍺ ⍵⍴2}"
;; 					 (or new-width *life-default-dimension*)
;; 					 (or new-height new-width *life-default-dimension*)))
;; 			 (april-c "{⊃1 ⍵∨.∧3 4=+/,1 0 ¯1∘.⊖1 0 ¯1⌽¨⊂⍵}" life-array)))
;;     (incf life-generation)
;;     (princ (april-c (with (:state :index-origin 0 :output-printed :only))
;; 		    "{' ⍬_║▐▀'[(0,(1+⊢/⍴⍵)⍴2)⍪(3,⍵,4)⍪5]}" life-array))
;;     (list :generation life-generation)))
