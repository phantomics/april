;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:AprilDemo.Ncurses -*-
;;;; ncurses.lisp

(in-package #:april-demo.ncurses)

"Demo ncurses application using April. Start this application by loading the loader.lisp file in this directory from the command line. Example for SBCL: sbcl --load loader.lisp"

;; The main screen, set as a global so it may be accessed from Slime
(defparameter *screen* nil)

(defvar *glyphs* nil)
(defvar *bg-colors* nil)
(defvar *color-depth*)

(with-open-stream (cmd-out (make-string-output-stream))
  (uiop:run-program "tput colors" :output cmd-out :ignore-error-status t)
  (let ((count-string (read-from-string (get-output-stream-string cmd-out))))
    (setq *color-depth* (if (integerp count-string) count-string 0))))

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

GC ←  'Generation:'  ⍝ generation caption

F  ←  {↑(⊂⍵),⊂GC}¨F  ⍝ append generation line to each footer

GI ← 0      ⍝ generation index
FB ← '━'    ⍝ footer boundary character
FW ← ⊢/∘⍴¨F ⍝ vector of footer widths

charSets  ←  ,⊂'─  ─   │   │   ┌  ┌   └   └   ┐  ┐   ┘   ┘  '
charSets ,←   ⊂'═  ═   │   │   ╒  ╒   ╘   ╘   ╕  ╕   ╛   ╛  '
charSets ,←   ⊂'─  ─   ║   ║   ╓  ╓   ╙   ╙   ╖  ╖   ╜   ╜  '
encInts   ←     48 384 144 288 16 416 128 304 32 400 256 176
xChars    ← '┼╪╫'
xDecInts  ← 68 69 257 261 321 324
csIndex   ← 2 ⍝ the set of box-drawing characters to use
decodings ← encInts⍪⍨{⍵⍴⍨1,⍴⍵} (csIndex⊃charSets)~' '
GCL       ← 1+≢GC ⍝ width of generation label field

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
⍝ ⎕←M
I←(5⌊1+≢I)↑I,⍨⊂L    ⍝ add latest iteration to history
B[⍳H-3;]←232+2×⊃+/I ⍝ sum iterations in history; 232 is the base terminal background color
M[⍳H-3;]←G trace L  ⍝ assign main display area containing box-traces of cells
M[H;GCL+⍳9]←9↑⍕GI   ⍝ print generation number; supports up to 9 digits
")
      (pdotimes (i (- (array-total-size glyphs) (* width 3 (if rebuilding 0 1))))
        ;; assign new vector contents in parallel, not assigning the footer content
        ;; unless the dimensions were just changed
        (setf (aref *glyphs* i) (row-major-aref glyphs i))
        (if (>= *color-depth* 256)
            (setf (aref *bg-colors* i) (row-major-aref colors i)))))))

(defun initialize-screen (screen)
  (flet ((render-screen (&optional restarting)
           (render (height screen) (width screen) restarting)
           (move screen 0 0)
           (loop :for char :across *glyphs* :for color :across *bg-colors*
                 :do (if (< *color-depth* 256)
                         (croatoan:add-wide-char screen char)
                         (croatoan:add-wide-char screen char :fgcolor '(:number 253)
                                                             :bgcolor (list :number color))))))
    
    (croatoan:submit (croatoan:bind screen #\r (lambda (win event)
                                                 (declare (ignore win event))
                                                 (render-screen t))))

    (croatoan:submit (croatoan:bind screen #\g (lambda (win event)
                                                 (declare (ignore win event))
                                                 (render-screen))))

    (croatoan:submit (croatoan:bind screen #\q (lambda (win event)
                                                 (croatoan:exit-event-loop win event))))

    (render-screen t) ;; render initial state

    ;; Set *screen* to the initilized screen so that we can access it form
    ;; the swank thread and then enter the event-loop.
    (croatoan:run-event-loop (setf *screen* screen))
    (cl-user::quit)))
