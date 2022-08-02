;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:AprilDemo.Ncurses -*-
;;;; ncurses.lisp

(in-package #:april-demo.ncurses)

"Demo ncurses application using April. Start this application by loading the loader.lisp file in this directory from the command line. Example for SBCL: sbcl --load loader.lisp"

;; The main screen, set as a global so it may be accessed from Slime
;;(defparameter *screen* nil)

(defvar *glyphs* nil)
(defvar *bg-colors* nil)

#|
Use the variable ncurses:colors instead, see croatoan example t18 for other available
ncurses information.

(defvar *color-depth*)

(with-open-stream (cmd-out (make-string-output-stream))
  (uiop:run-program "tput colors" :output cmd-out :ignore-error-status t)
  (let ((count-string (read-from-string (get-output-stream-string cmd-out))))
    (setq *color-depth* (if (integerp count-string) count-string 0))))
|#

(april (with (:space ncurses-demo-space))
       "
random ← {⎕IO-⍨?2⍴⍨|⍺ ⍵}
       ⍝ create a randomized binary matrix
life   ← {⊃1 ⍵∨.∧3 4=+/,1 0 ¯1∘.⊖1 0 ¯1⌽¨⊂⍵}
       ⍝ the classic Conway's Game of Life function
trace  ← {⍺[;1]⌷⍨⊂⍺[;2]{1⌈⍺{⍵×⍵≤⍴⍺}⍺⍳⍵}{2⊥,⍵}⌺3 3⊢⍵}
       ⍝ use [⌺ stencil] to draw boxes around cells according to matrix decoding maps

F  ← ,⊂↑'[G]enerate [R]estart [Q]uit' 'Generation:'
F ,←  ⊂↑'Next [G]eneration ⋄ [R]estart ⋄ [Q]uit.' 'Generation:'
F ,←  ⊂↑'Press [G] for the next generation, [R] to restart or [Q] to quit.' 'Generation:'
F ,←  ⊂↑'Press [G] to see the next generation, [R] to restart with a random matrix or [Q] to quit.' 'Generation:'
   ⍝ variable footer width depending on content length

GI ← 0      ⍝ generation index
FB ← '━'    ⍝ footer boundary character
FW ← ⊢/∘⍴¨F ⍝ vector of footer widths

charSets  ←  ,⊂'─  ─   │   │   ┌  ┌   └   └   ┐  ┐   ┘   ┘  '
charSets ,←   ⊂'═  ═   │   │   ╒  ╒   ╘   ╘   ╕  ╕   ╛   ╛  '
charSets ,←   ⊂'─  ─   ║   ║   ╓  ╓   ╙   ╙   ╖  ╖   ╜   ╜  '
encInts   ←      48 384 144 288 16 416 128 304 32 400 256 176
xChars    ← '┼╪╫'
xDecInts  ←  68 69 257 261 321 324
csIndex   ← 2 ⍝ the set of box-drawing characters to use
decodings ← encInts⍪⍨{⍵⍴⍨1,⍴⍵} (csIndex⊃charSets)~' '

G ← ' ' {(⍺,0)⍪⍵[1;] {(⍪⍸×⍵),⍨⍪⍺⌷⍨⊂⍵~0} ⍵[2;] {⍺{⍵×⍵≤⍴⍺}⍺⍳⍵} {2⊥,(2 2⍴0)⍷3 3⍴(9⍴2)⊤⍵}¨⍳2*9} decodings
  ⍝ map of binary decodings of stencil matrices to box characters
G ⍪← (csIndex⊃xChars),⍪xDecInts
M ← ⍬ ⍝ the character matrix
I ← ⍬ ⍝ the 5-iteration state history
")

(defun build-screen (height width)
  (setf *glyphs* (make-array (* height width)
                             :element-type 'character :initial-element #\ )
        *bg-colors* (make-array (* height width)
                                :element-type '(unsigned-byte 8) :initial-element 232))
  :success)

(defun render (height width &optional randomize)
  (let ((rebuilding (or (not *glyphs*)
                        (not (and (= height (first (array-dimensions *glyphs*)))
                                  (= width (second (array-dimensions *glyphs*))))))))
    (if rebuilding (build-screen height width))
    (multiple-value-bind (glyphs colors)
        (april (with (:space ncurses-demo-space)
                     (:state :in ((randomize (if randomize 1 0))
                                  (-h height) (-w width))
                             :out (-m -b)))
               "
$[(M≡⍬)∨randomize∨⍲/H W=⍴M ;
    ⍝ if the program has been started or reset or the window dimensions have changed, then...
  M←H W⍴' ' ⋄ L←(H-3) random W ⋄ M[(H-3)+⍳3;]←FB⍪2 W↑⊃F[1⌈FW⍸W] ⋄ B←H W⍴232 ⋄ I GI←⍬ 0 ;
    ⍝ initialize the matrix, generation index and history and create a random starting matrix;
  L←life L ⋄ GI+←1]
    ⍝ otherwise, produce the next-generation binary matrix according to the life function

I←(5⌊1+≢I)↑I,⍨⊂L    ⍝ add latest iteration to history
B[⍳H-3;]←232+2×⊃+/I ⍝ sum iterations in history; 232 is the base terminal background color
M[⍳H-3;]←G trace L  ⍝ assign main display area containing box-traces of cells
M[H;12+⍳9]←9↑⍕GI    ⍝ print generation number; supports up to 9 digits
")
      (pdotimes (i (- (array-total-size glyphs) (* width 3 (if rebuilding 0 1))))
        ;; assign new vector contents in parallel, not assigning the footer content
        ;; unless the dimensions were just changed
        (setf (aref *glyphs* i) (row-major-aref glyphs i))

        ;; the underlying ncurses binding provides environment variables "colors" and "color-pairs"
        ;; use TERM=xterm-265color sbcl --load ... to make xterm support 256 colors.
        (when (>= ncurses:colors 256)
          (setf (aref *bg-colors* i) (row-major-aref colors i)))))))


;; since nothing is supposed to happen between key presses (during the
;; "nil" event), set input-blocking to t, instead of a timeout of 100 ms.

(defun main ()
  (croatoan:with-screen (screen :input-blocking t :bind-debugger-hook nil :cursor-visible nil)
    (flet ((render-screen (&optional restarting)
             (render (height screen) (width screen) restarting)
             (move screen 0 0)
             (loop :for char :across *glyphs* :for color :across *bg-colors*
                   :do (if (< ncurses:colors 256)
                           (croatoan:add-wide-char screen char)
                           (croatoan:add-wide-char screen char :fgcolor '(:number 253)
                                                               :bgcolor (list :number color))))))
      ;; (R)estart with a random matrix
      ;; lambda keywords win and event do not have to be explicitely passed and then declared ignored.
      (croatoan:bind screen #\r (lambda () (render-screen t)))
      ;; display the next (G)eneration
      (croatoan:bind screen #\g (lambda () (render-screen)))
      ;; exit event loop is meant to be bound directly, it doesnt have to be passed in a lambda
      (croatoan:bind screen #\q #'exit-event-loop)

      ;; render initial state
      (render-screen t)
      (croatoan:run-event-loop screen))))
