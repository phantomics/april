;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; novelties.lisp

(in-package #:april)

"A collection of fun odds and ends demonstrating April APL features."

#| Conway's Game of Life

An implementation of an old APL standby.

Usage:

To create a new playfield of a given -width- and -height-, evaluate:

(life :width -width- :height -height-)

The new field will contain a random arrangement of cells.

For example, (life :width 64 :height 32) creates a field 64 wide by 32 tall.

If only a width value is passed, it will be both the length and width of the field.

Therefore, evaluating (life :width 50) will create a 50x50 playfield.

To calculate the next generation, just evaluate:

(life)

If no playfield exists, evaluating (life) will create a new 16x16 playfield.

If you'd like to start with a specific playfield, you can do so by passing a third argument containing a binary matrix. For example:

(life :width -10 :height -10 :seed (april "(3 3⍴⍳9)∊1 2 3 4 8"))

This creates a 10x10 playfield with a glider in the lower right corner; that is, a ¯10 ¯10 take of the glider matrix. Passing 10 and 10 instead of -10 and -10 will result in a 10 10 take with the glider shape in the upper left corner. If a seed is passed with no specified width or height the playfield will be of the same dimensions as the seed, i.e. the seed will form the entire playfield.
|#

(let ((life-array)
      (life-generation -1)
      (default-dimension 16))
  (defun life (&key width height seed return)
    "Create or update a playfield for Conway's Game of Life."
    (setf life-array (if (or width seed (not life-array))
                         (progn (setf life-generation -1)
                                (if seed (if (not (or height width))
                                             seed (destructuring-bind (seed-height seed-width)
                                                      (array-dimensions seed)
                                                    (april-c "↑" seed
                                                             (vector (or height seed-height)
                                                                     (or width seed-width)))))
                                    (april-c "{⎕IO-⍨?2⍴⍨|⍺ ⍵}" (or width default-dimension)
                                             (or height width default-dimension))))
                         (april-c "{⊃1 ⍵∨.∧3 4=+/,1 0 ¯1∘.⊖1 0 ¯1⌽¨⊂⍵}" life-array)))
    (incf life-generation)
    ;; the ≢⍉ is efficient when lazily evaluated, but wouldn't
    ;; be otherwise without a specially recognized idiom
    (if return (values life-array (list :generation life-generation))
        (progn (april-c "{⎕←' ⍬_║▐▀'[⎕IO+(0,(1+≢⍉⍵)⍴2)⍪(3,⍵,4)⍪5]}" life-array)
               (list :generation life-generation)))))

(let ((banners
        (april-c
         "↑¨" (vector #("  Welcome to"
                        "┌─────┬─────┬─────┬─────┬─────┬─┐ "
                        "├──── │ ─── │ ┌───┤ ─── │ ─── │ │ "
                        "│ ─── │ ┌───┤ │   │ ────┤ ┌───┤ └┐"
                        "└─────┴─┘   └─┘   └─────┴─┘   └──┘"
                        "              the April APL REPL")))))
  (defun display-banner (&key width height)
    (april-c (with (:state :output-printed :only))
             "{B←⊃⍵ ⋄ W←⊃⌽⍺ ⋄ H BW←⍴B ⋄ (-⌊2÷⍨0⌈W-BW)⌽H W↑B}"
             banners (vector (or height 0) (or width 0)))))
