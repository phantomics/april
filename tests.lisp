;;;; tests.lisp

(in-package #:apex)

(defun test-idiom ()
  ;;(princ (format nil "~%Tests for seed.generate:~%"))
  (is (apex "1 0 1 0 1/⍳5")
      #(1 3 5))

  (is (apex "1 ¯2 3 ¯4 5/⍳5")
      #(1 0 0 3 3 3 0 0 0 0 5 5 5 5 5))

  (is (apex "0 1 0 1 ^ 0 0 1 1")
      #(0 0 0 1))

  (is (apex "1 1.2 1.4 1.6 1.8 2!5")
      #(5 6.105689248 7.219424686 8.281104786 9.227916704 10))
  
  ;;(ok (getf (seed.generate::branch-meta (first branches)) :stable))
  
  (princ #\Newline)
  nil)
