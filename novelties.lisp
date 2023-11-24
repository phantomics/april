;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; novelties.lisp

(in-package #:april)

"A collection of fun odds and ends demonstrating April APL features."

#|
-- Conway's Game of Life --

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

#|
-- Progress Bar Printer --

This function prints a progress bar to monitor the advance of a process. It's designed to be used with processes that take multiple steps; it returns a function that should be called upon the completion of each step in the process.

To test the progress bar, evaluate the following:

(let ((advancer (april-print-progress-bar 100)))
  (loop :for i :below 100 :do (funcall advancer) (sleep 0.05)))

This demonstrates the basic use case of the progress bar. You can also customize it by passing optional arguments:


(let ((advancer (april-print-progress-bar 100 :increments (april "1 2÷3")
                                              :width 80 :glyphs "⌸⍠╵⊥ ┑┝┥═╤╞╡")))
  (loop :for i :below 100 :do (funcall advancer) (sleep 0.05)))

This will invoke a progress bar with different demarcations (at each third rather than each quarter) printed with a different set of characters. Try printing progress bars with your own character sets - the possibilities are endless. This function is exported from the April package so you can easily use it in applications that depend on April.
|#

(defun april-print-progress-bar (count &key (increments (april "1 2 3÷4"))
                                         (width 64) (glyphs "⋄⌺∘○ ╷╓╖─┼╟╢"))
  "Print a progress bar whose growth illustrates the advance of a process."
  (let ((total 0) (printed 0) (interval-index 0) (current-interval 0)
        (breadth (* width count))
        (marked-intervals (april-c (with (:state :in ((glyphs glyphs))))
                                   "{
  ⎕IO ← 0
  mrk ← 1+⍸ind ← ¯1⌽(⊢<1∘⌽)(⍵×int ← ⍺[⍋⍺])⍸⍳⍵-2
      ⍝ locations of marked intervals, with width minus 2 for enclosing chars
      ⍝ intervals are indexed grading up so they can be specified in any order
  msk ← (2-⍨/mrk,⍵-2)>≢¨lab ← ('%',⍨⍕)¨⌊100×int↑⍨≢mrk
      ⍝ a mask used to skip printing increment labels when there isn't enough space
      ⍝ intervals are omitted from the mask when there isn't room to print the mark
  ⎕   ← {(⊃,/⍵)@(⊃,/1+(msk/mrk)+⍳∘≢¨⍵)⊢(4↓8↑glyphs)[2,ind,3]} msk/lab
      ⍝ characters on line 1: denoted increments
  ⎕   ← (8↓glyphs)[2,ind,3]
      ⍝ characters on line 2: span with increment marks

  mrk ⍝ return indices of marked increments
}"
                                   width increments)))
    ;; the returned advance function should be called upon each iteration of the process
    (lambda ()
      (when (< total breadth)
        (incf total          width)
        (incf interval-index width)
        (loop :while (< count interval-index)
              :do (if (zerop printed)
                      (princ (aref glyphs 0))
                      (when (or (/= total breadth)
                                (< count interval-index))
                        (if (and (< current-interval (length marked-intervals))
                                 (= printed (aref marked-intervals current-interval)))
                            (progn (incf current-interval)
                                   (princ (aref glyphs 3)))
                            (princ (aref glyphs 2)))))
                  (incf printed)
                  (decf interval-index count))
        (when (= total breadth)
          (princ (aref glyphs 1))
          (princ #\Newline))))))

#| 
-- April Banners --

Code to print April-related text banners. Currently used to implement welcome text for ApREPL.
|#

(let ((banners
        (april-c
         "↑¨" (vector #("  Welcome to"
                        "┌─────┬─────┬─────┬─────┬─────┬─┐"
                        "│ ─── │ ─── │ ─── │ ────┤ ─── │ │"
                        "│ ┌─┐ │ ┌───┤ ┌─┐ ┤ ────┤ ┌───┤ └─┐"
                        "└─┘ └─┴─┘   └─┘ └─┴─────┴─┘   └───┘"
                        "              the April APL REPL")
                      #("  Welcome to"
                        "┌─────┬─────┬─────┬─────┬─────┬─┐"
                        "├──── │ ─── │ ┌───┤ ─── │ ─── │ │"
                        "│ ─── │ ┌───┤ │   │ ────┤ ┌───┤ └┐"
                        "└─────┴─┘   └─┘   └─────┴─┘   └──┘"
                        "              the April APL REPL")
                      ))))
  (defun display-banner (&key width height)
    (april-c (with (:state :output-printed :only))
             "{B←⊃⍵ ⋄ W←⊃⌽⍺ ⋄ H BW←⍴B ⋄ (-⌊2÷⍨0⌈W-BW)⌽H W↑B}"
             banners (vector (or height 0) (or width 0)))))
