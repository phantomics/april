;;;; vex.lisp

(in-package #:vex)

(defun handle-argument (operation omega &optional alpha)
  (if (and (symbolp (first operation))
	   (macro-function (first operation))
	   (not (eql 'lambda (first operation))))
      (cond ((eql 'args (first operation))
	     (macroexpand (append (cons 'args (last operation))
				  (cons (second operation)
					(append (if (keywordp (third operation))
						    (list (third operation))
						    ;; the placeholder is added in case of (args :scalar ...)
						    ;; and other macros which have two arguments but only one
						    ;; structure-specifying parameter
						    (if alpha (list :placeholder)))
						(if (keywordp (fourth operation))
						    (list (fourth operation))))))))
	    (t (macroexpand (append operation (cons omega (if alpha (list alpha)))))))
      `(quote ,operation)))

(defun derive-opglyphs (glyph-list &optional output)
  (if (not glyph-list)
      output (derive-opglyphs (rest glyph-list)
			      (let ((glyph (first glyph-list)))
				(if (characterp glyph)
				    (cons glyph output)
				    (if (stringp glyph)
					(append output (loop for char from 0 to (1- (length glyph))
							  collect (aref glyph char)))))))))
