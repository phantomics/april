;;;; game-of-life.lisp

(in-package #:april)

#|
Just for fun, an implementation of an old APL standby - Conway's Game of Life.

Usage: 

To create a new playfield of a given size, evaluate:

(life *width* *height*)

For example, (life 64 32) creates a field 64 wide by 32 tall.

To calculate the next generation, just evaluate:

(life)

If no playfield exists, evaluating (life) will create a 16x16 playfield.
|#

(let ((life-array nil)
      (life-generation -1))
  (defun life (&optional new-width new-height)
    "Create or update a playfield for Conway's Game of Life."
    (setq life-array (if (or new-width (not life-array))
                      (let* ((new-width (if new-width new-width 16))
                             (new-height (if new-height new-height new-width)))
			(setq life-generation -1)
                        (april (set (:state :index-origin 0 :print-output nil
					    :in ((-w new-width) (-h new-height))))
                               "?H W⍴2"))
                      (april (set (:state :print-output nil :in ((-l life-array))))
                             "⊃1 L∨.∧3 4=+/,1 0 ¯1∘.⊖1 0 ¯1⌽¨⊂L"))
	  life-generation (1+ life-generation))
    (princ (april (set (:state :index-origin 0 :print-to-string :only :print-output nil
			       :in ((-l life-array))))
		  "' ⍬_║▐▀'[(0,(1+1⌷⍴L)/2)⍪(3,L,4)⍪5]"))
    (list :generation life-generation)))
