;;;; ncurses.lisp

(in-package #:april-demo.ncurses)

"Demo ncurses application using April. Start this application by loading the loader.lisp file in this directory from the command line. Example for SBCL: sbcl --load loader.lisp"

;; The main screen, we need a global so that we can access it from slime
(defparameter *screen* nil)

(defvar *glyphs* nil)
(defvar *fg-colors* nil)
(defvar *bg-colors* nil)

#| Alternatives for the box character encoding

#2A((#\─ #\─ #\│ #\│ #\┌ #\┌ #\└ #\└ #\┐ #\┐ #\┘ #\┘)
    (48  384 144 288 16  416 128 304 32  400 256 176))

#2A((#\═ #\─ #\║ #\│ #\┌ #\┌ #\╘ #\╘ #\╖ #\╖ #\┘ #\┘)
    (48  384 144 288 16  416 128 304 32  400 256 176)))

|#

(april (with (:space ncurses-demo-space)
             (:state :in ((encodings #2A((#\═ #\═ #\│ #\│ #\╒ #\╒ #\╘ #\╘ #\╕ #\╕ #\╛ #\╛)
                                         (48  384 144 288 16  416 128 304 32  400 256 176))))))
       "
random ← {⎕IO-⍨?2⍴⍨|⍺ ⍵}
       ⍝ create a randomized binary matrix
life   ← {⊃1 ⍵∨.∧3 4=+/,1 0 ¯1∘.⊖1 0 ¯1⌽¨⊂⍵}
       ⍝ the classic Conway's Game of Life function
trace  ← {⍺[;1]⌷⍨⊂⍺[;2]{1⌈⍺{⍵×⍵≤⍴⍺}⍺⍳⍵}{2⊥,⍵}⌺3 3⊢⍵}
       ⍝ use [⌺ stencil] to draw boxes around cells

GI ← 0   ⍝ generation index
FB ← '━' ⍝ footer boundary character
lF ← ↑'Press [G] to see the next generation, [R] to restart with a random matrix or [Q] to quit.' 'Generation:'
   ⍝ long footer content
rF ← ↑'Press [G] for the next generation, [R] to restart or [Q] to quit.' 'Generation:'
   ⍝ regular footer content
sF ← ↑'Next [G]eneration ⋄ [R]estart ⋄ [Q]uit.' 'Generation:'
   ⍝ smaller footer content
tF ← ↑'[G]enerate [R]estart [Q]uit' 'Generation:'
   ⍝ tiny footer content

G ← ' ' {(⍺,0)⍪⍵[1;] {(⍪⍸×⍵),⍨⍪⍺⌷⍨⊂⍵~0} ⍵[2;] {⍺{⍵×⍵≤⍴⍺}⍺⍳⍵} {2⊥,(2 2⍴0)⍷3 3⍴(9⍴2)⊤⍵}¨⍳2*9} encodings
  ⍝ map of binary decodings of stencil matrices to box characters
M ← ⍬ ⍝ this variable holds the character matrix
I ← ⍬ ⍝ this variable holds the 5-iteration state history
")

(defun build-screen (height width)
  (setf *glyphs* (make-array (* height width)
                             :element-type 'character :initial-element #\ )
        *fg-colors* (make-array (* height width)
                                :element-type '(unsigned-byte 8) :initial-element 15)
        *bg-colors* (make-array (* height width)
                                :element-type '(unsigned-byte 8) :initial-element 232)))

(defun render (height width &optional randomize)
  (if (or (not *glyphs*)
          (not (and (= height (first (array-dimensions *glyphs*)))
                    (= width (second (array-dimensions *glyphs*))))))
      (build-screen height width))
  (multiple-value-bind (glyphs colors)
      (april (with (:space ncurses-demo-space)
                   (:state :in ((randomize (if randomize 1 0))
                                (-h height) (-w width))
                           :out (-m -b)))
                       "
$[(M≡⍬)∨randomize∨~∧/H W=⍴M ;
    ⍝ if the program has been started or reset or the window dimensions have changed, then...
  M←H W⍴' ' ⋄ L←(H-3) random W ⋄ M[(H-3)+⍳3;]←FB⍪2 W↑$[38>W;tF;64>W;sF;W>89;lF;rF] ⋄ B←H W⍴232 ⋄ GI←0 ⋄ I←⍬ ;
    ⍝ initialize the matrix, generation index and history and create a random starting matrix;
  L←life L ⋄ GI+←1]
    ⍝ otherwise, produce the next-generation binary matrix according to the life function

I←(5⌊1+≢I)↑I,⍨⊂L    ⍝ add latest iteration to history
B[⍳H-3;]←232+2×⊃+/I ⍝ sum iterations in history; 232 is the base terminal background color
M[⍳H-3;]←G trace L  ⍝ assign main display area containing box-traces of cells
M[H;12+⍳9]←9↑⍕GI    ⍝ print generation number; supports up to 9 digits
")
    (pdotimes (i (array-total-size glyphs))
      (setf (aref *glyphs* i) (row-major-aref glyphs i)
            (aref *bg-colors* i) (row-major-aref colors i)))))

(defun initialize-screen (screen)
  (flet ((render-screen (&optional restarting)
           (render (height screen) (width screen) restarting)
           (move screen 0 0)
           (loop :for char :across *glyphs* :for color :across *bg-colors*
                 :do (croatoan:add-wide-char screen char :fgcolor '(:number 253)
                                                         :bgcolor (list :number color)))))
    
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

;; (april-c "2∘⊥∘," #2A((1 0 0)
;;                      (1 0 0)
;;                      (0 0 0)))

;; (april-c "{3 3⍴(9⍴2)⊤⍵}" 256)
