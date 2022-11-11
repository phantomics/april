;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:AprilDemo.Ncurses -*-
;;;; ncurses.lisp

(in-package #:april-demo.ncurses)

"Demo ncurses application using April. Start this application by loading the loader.lisp file in this directory from the command line. Example for SBCL: sbcl --load loader.lisp"

(defvar *glyphs* nil)
(defvar *bg-colors* nil)
(defvar *bg-color-forms* (vector '(:number 232) '(:number 234) '(:number 236)
                                 '(:number 238) '(:number 240) '(:number 242)))

#|
Use the variable ncurses:colors instead, see croatoan example t18 for other available
ncurses information.
|#

(april (with (:space ncurses-demo-space))
       "
random ← {⎕IO-⍨?2⍴⍨|⍺ ⍵}
       ⍝ create a randomized binary matrix
life   ← {⊃1 ⍵∨.∧3 4=+/,1 0 ¯1∘.⊖1 0 ¯1⌽¨⊂⍵}
       ⍝ the classic Conway's Game of Life function
trace  ← {⍺[;1]⌷⍨⊂⍺[;2]{1⌈⍺{⍵×⍵≤⍴⍺}⍺⍳⍵}{2⊥,⍵}⌺3 3⊢⍵}
       ⍝ use [⌺ stencil] to draw boxes around cells according to matrix decoding maps

F  ← ,⊂'[G]enerate [R]estart [Q]uit'
F ,←  ⊂'Next [G]eneration ⋄ [R]estart ⋄ [Q]uit'
F ,←  ⊂'Press [G] for the next generation, [R] to restart or [Q] to quit.'
F ,←  ⊂'Press [G] to see the next generation, [R] to restart with a random matrix or [Q] to quit.'
   ⍝ variable footer width depending on content length

GC ← 'Generation:' ⍝ generation caption

F  ← {↑(⊂⍵),⊂GC}¨F ⍝ append generation line to each footer

GI ← 0      ⍝ generation index
FB ← '━'    ⍝ footer boundary character
FW ← ⊢/∘⍴¨F ⍝ vector of footer widths

charSets  ← ,⊂'─  ─   │   │   ┌  ┌   └   └   ┐  ┐   ┘   ┘  '
charSets ,←  ⊂'═  ═   │   │   ╒  ╒   ╘   ╘   ╕  ╕   ╛   ╛  '
charSets ,←  ⊂'─  ─   ║   ║   ╓  ╓   ╙   ╙   ╖  ╖   ╜   ╜  '
encInts   ←    48 384 144 288 16 416 128 304 32 400 256 176
xChars    ← '┼╪╫'
xDecInts  ← 68 69 257 261 321 324
csIndex   ← 2 ⍝ the set of box-drawing characters to use
decodings ← encInts⍪⍨{⍵⍴⍨1,⍴⍵}' '~⍨csIndex⊃charSets
GCL       ← 1+≢GC ⍝ width of generation label field

G ← ' ' {(⍺,0)⍪⍵[1;] {(⍪⍸×⍵),⍨⍪⍺⌷⍨⊂⍵~0} ⍵[2;] {⍺{⍵×⍵≤⍴⍺}⍺⍳⍵} {2⊥,(2 2⍴0)⍷3 3⍴(9⍴2)⊤⍵}¨⍳2*9} decodings
  ⍝ map of binary decodings of stencil matrices to box characters
G ⍪← (csIndex⊃xChars),⍪xDecInts
M ← ⍬ ⍝ the character matrix
")

(defun build-screen (height width)
  (setf *glyphs* (make-array (* height width)
                             :element-type 'character :initial-element #\ )
        *bg-colors* (when (>= ncurses:colors 256)
                      (make-array (* height width)
                                  :element-type '(unsigned-byte 8) :initial-element 0)))
  :success)

(defun render (height width &optional randomize)
  (let ((rebuilding (or (not *glyphs*)
                        (/= (* height width) (array-total-size *glyphs*)))))
    (when rebuilding (build-screen height width))
    (multiple-value-bind (glyphs colors)
        (april (with (:space ncurses-demo-space)
                     (:state :in ((randomize (if randomize 1 0))
                                  (-b-c (if *bg-colors* 1 0))
                                  (-h height) (-w width))
                             :out (-m -b)))
               "
$[(M≡⍬)∨randomize∨⍲/H W=⍴M ;
    ⍝ if the program has been started or reset or the window dimensions have changed, then...
  M←H W⍴' ' ⋄ L←(H-3) random W ⋄ M[(H-3)+⍳3;]←FB⍪2 W↑F⊃⍨1⌈FW⍸W ⋄ B←(H-3) W⍴0 ⋄ I GI←⍬ 0 ;
    ⍝ initialize the matrix, generation index and history and create a random starting matrix;
  L←life L ⋄ GI+←1]
    ⍝ otherwise, produce the next-generation binary matrix according to the life function

$[BC;B←0⌈1-⍨5⌊B+2×L] ⍝ sum of last 5 life iterations if background colors are available
M[⍳H-3;]←G trace L   ⍝ assign main display area containing box-traces of cells
M[H;GCL+⍳12]←12↑⍕GI  ⍝ print generation number; supports up to 12 digits
")
      (pdotimes (i (array-total-size glyphs))
        ;; assign new vector contents in parallel, not assigning the footer content
        ;; unless the dimensions were just changed
        (setf (aref *glyphs* i) (row-major-aref glyphs i))
        ;; the underlying ncurses binding provides environment variables "colors" and "color-pairs"
        ;; use TERM=xterm-265color sbcl --load ... to make xterm support 256 colors.
        (when (and *bg-colors* (not rebuilding)
                   (< i (- (array-total-size glyphs) (* width 3))))
          (setf (aref *bg-colors* i) (row-major-aref colors i)))))))

;; since nothing is supposed to happen between key presses (during the
;; "nil" event), set input-blocking to t, instead of a timeout of 100 ms.

(defun main ()
  (croatoan:with-screen (screen :input-blocking t :bind-debugger-hook nil :cursor-visible nil)
    (let ((render-screen
            (if *bg-colors*
                (lambda (&optional restarting)
                  (render (height screen) (width screen) restarting)
                  (move screen 0 0)
                  (loop :for char :across *glyphs* :do (croatoan:add-wide-char screen char)))
                (lambda (&optional restarting)
                  (render (height screen) (width screen) restarting)
                  (move screen 0 0)
                  (loop :for char :across *glyphs* :for color :across *bg-colors*
                        :do (croatoan:add-wide-char
                             screen char :fgcolor '(:number 253)
                             :bgcolor (aref *bg-color-forms* color)))))))
      ;; (R)estart with a random matrix
      (croatoan:bind screen #\r (lambda () (funcall render-screen t)))
      ;; display the next (G)eneration
      (croatoan:bind screen #\g (lambda () (funcall render-screen)))
      ;; (Q)uit the program
      (croatoan:bind screen #\q #'croatoan:exit-event-loop)

      ;; render initial state
      (funcall render-screen t)
      (croatoan:run-event-loop screen))))
