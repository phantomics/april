;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; utilities.lisp

(in-package #:april)

"Utility functions for April. It's important to understand the difference between the functions and macros provided here and the ones that come from the aplesque package. The faculties provided by aplesque reflect features of APL, but they have uses other than implementing APL. The material here is specifically for use in implementing APL, with uses aside from an APL implementation not accounted for in its design. The functions here are used to implement language mechanics as opposed to functions in the language's standard library; the latter are implemented in library.lisp."

(define-symbol-macro this-idiom (local-idiom april))
(define-symbol-macro atomic-vector (get-system-meta this-idiom :atomic-vector))
(define-symbol-macro *apl-timestamp* (apl-timestamp))
(define-symbol-macro *first-axis* (if (not axes) 0 (- (first axes) index-origin)))
(define-symbol-macro *last-axis* (- (if axes (first axes) (max 1 (rank omega)))
				    index-origin))
(define-symbol-macro *first-axis-or-nil* (if axes (- (first axes) index-origin)))
(define-symbol-macro *branches* (symbol-value (intern "*BRANCHES*" space)))

;; TODO: figure out why these dynamic variables are needed; some tests fail despite being compiled
;; within (let) forms that specify these values
(defvar index-origin 1)
(defvar print-precision 10)

;; the names of library functions that curry functions having axes with index-origin, needed for the λχ macro
(defparameter *io-currying-function-symbols-monadic* '(ravel-arrays))
(defparameter *io-currying-function-symbols-dyadic* '(catenate-arrays catenate-on-first section-array))

(defmacro with-april-workspace (name body)
  "Reader macro that interns symbols in the current workspace; works in tandem with 𝕊 reader macro."
  (labels ((replace-symbols (form)
	     (loop :for item :in form :for ix :from 0
		:do (if (listp item)
			(if (and (second item) (not (third item))
				 (symbolp (second item)) (eql 'inws (first item)))
			    (setf (nth ix form)
				  (intern (string (second item))
					  (concatenate 'string "APRIL-WORKSPACE-" (string-upcase name))))
			    (replace-symbols item))))))
    (replace-symbols body)
    body))

;; this reader macro expands to (inws symbol) for reader-friendly printing of compiled code
(set-macro-character #\𝕊 (lambda (stream character)
			   (declare (ignore character))
			   (list 'inws (read stream t nil t))))

;; printer extension to use the 𝕊 reader macro
(set-pprint-dispatch '(cons (member inws)) 
		     #'(lambda (s list)
			 (if (and (symbolp (second list)) (not (third list)))
			     (funcall (formatter "𝕊~W") s (second list)) 
			     (pprint-fill s list))))

(defmacro insym (symbol)
  "Macro used in grammar.lisp to intern value-referencing symbols in appropriate workspace package."
  `(if (not (symbolp ,symbol)) ,symbol (intern (string ,symbol) space)))

(defun dummy-nargument-function (first &rest rest)
  "Placeholder function to be assigned to newly initialized function symbols."
  (declare (ignorable rest))
  first)

;; keep legacy april-p macro in place and usable in place of april-f
(defmacro april-p (&rest args)
  (cons 'april-f args))

;; these macros are shorthand for lambda definitions used in the spec; they make April's compiled code
;; more compact and comfortable to read
(defmacro λω (&rest body)
  `(lambda (omega) ,@body))

(defmacro λωα (&rest body)
  `(lambda (omega alpha) ,@body))

(defmacro λωαχ (&rest body)
  `(lambda (omega alpha &optional axes) ,@body))

(defmacro λωχ (&rest body)
  `(lambda (omega &optional axes) ,@body))

(defmacro λχ (body axes)
  "Curry a function with axes for use with an operator."
  (if (member (first body) (cons 'λωαχ *io-currying-function-symbols-dyadic*))
      `(λωα (funcall ,body omega alpha ,(cons 'list axes)))
      (if (member (first body) (cons 'λωχ *io-currying-function-symbols-monadic*))
	  `(λω (funcall ,body omega ,(cons 'list axes)))
	  body)))

(defmacro is-workspace-value (item)
  "Checks if a variable is present in the current workspace as a value."
  `(and (boundp (intern (string ,item) space))
	(not (fboundp (intern (string ,item) space)))))

(defmacro is-workspace-function (item)
  "Checks if a variable is present in the current workspace as a function."
  `(fboundp (intern (string ,item) space)))

(defun set-workspace-item-meta (symbol item &rest data)
  "Sets one or more metadata for an item in a workspace."
  (if (not (boundp symbol))
      (set symbol (make-hash-table :test #'eq)))
  (loop :for (key value) :on data :by #'cddr
     :do (setf (getf (gethash item (symbol-value symbol)) key) value)))

(defun get-workspace-item-meta (symbol item &rest keys)
  "Gets one or more metadata for an item in a workspace."
  (if (boundp symbol)
      (let ((data (gethash item (symbol-value symbol))))
	(apply #'values (loop :for key :in keys :collect (getf data key))))))

(defun build-populator (metadata-symbol input)
  "Generate a function that will populate array elements with an empty array prototype." 
  (if (and (= 0 (size input))
	   (get-workspace-item-meta metadata-symbol input :eaprototype))
      (lambda () (copy-array (get-workspace-item-meta metadata-symbol input :eaprototype)))))

(defun make-prototype-of (array)
  "Make a prototype version of an array; all values in the array will be blank spaces for character arrays or zeroes for other types of arrays."
  (if (not (eq t (element-type array)))
      (make-array (dims array) :element-type (element-type array)
		  :initial-element (if (member (element-type array) '(base-char character)) #\  0))
      (let ((output (make-array (dims array))))
	(dotimes (i (size output)) (setf (row-major-aref output i)
					 (if (not (arrayp (row-major-aref array i)))
					     0 (make-prototype-of (row-major-aref array i)))))
	output)))

(defmacro print-and-run (form)
  "Print a formatted code string and then run the code; used in april's arbitrary evaluation tests."
  `(let ((*print-case* :downcase))
     (princ (indent-code (write-to-string (quote ,form))))
     ,form))

(defun indent-code (string)
  "Indent a code string produced by (print-and-run) as appropriate for April's test output."
  (concatenate 'string "  * " (regex-replace-all "[\\n]" string (format nil "~%    "))))

(defun disclose-atom (item)
  "If the argument is a non-nested array with only one member, disclose it, otherwise do nothing."
  (if (not (and (not (stringp item))
		(arrayp item)
		(is-unitary item)
		(not (arrayp (row-major-aref item 0)))))
      item (row-major-aref item 0)))

(defmacro apl-assign (symbol value)
  "This is macro is used to build variable assignment forms and includes logic for stranded assignments."
  (if (or (not (listp symbol))
	  (eql 'inws (first symbol)))
      `(set ',symbol ,value)
      (let ((values (gensym "A")))
	`(let ((,values ,value))
	   (if (and (arrayp ,values)
		    (/= 1 (size ,values)) (/= ,(1- (length symbol)) (length ,values)))
	       (error "Mismatched number of symbols and values for string assignment."))
	   ,@(loop :for s :in (rest symbol) :for sx :from 0
		:collect (let ((set-to `(disclose (if (or (not (arrayp ,values)) (= 1 (size ,values)))
						      ,values (aref ,values ,sx)))))
			   (if (member s *idiom-native-symbols*) `(setq ,s ,set-to)
			       `(set ',s ,set-to))))
	   ,values))))

(defmacro apl-output (form &rest options)
  "Generate code to output the result of APL evaluation, with options to print an APL-formatted text string expressing said result and/or return the text string as a result."
  (let ((result (gensym)) (printout (gensym)))
    `(let* ((,result ,form)
	    (,printout ,(if (and (or (getf options :print-to)
				     (getf options :output-printed)))
			    ;; don't print the results of assignment unless the :print-assignment option is set,
			    ;; as done when compiling a ⎕← expression
			    (if (and (listp form)
				     (eql 'apl-assign (first form))
				     (not (getf options :print-assignment)))
				"" `(matrix-print ,result :append #\Newline
						  :segment (lambda (n &optional s)
							     (aplesque::count-segments
							      n ,(getf options :print-precision) s))
						  :format (lambda (n &optional s r)
							    (print-apl-number-string
							     n s ,(getf options :print-precision) nil r)))))))
       (declare (ignorable ,result ,printout))
       ;; TODO: add printing rules for functions like {⍵+1}
       ,(if (getf options :print-to)
	    (let ((string-output `(write-string ,printout ,(getf options :print-to))))
	      `(if (arrayp ,result)
		   ,string-output (concatenate 'string ,string-output (list #\Newline)))))
       ,(if (getf options :output-printed)
	    (if (eq :only (getf options :output-printed))
		printout `(values ,result ,printout))
	    result))))

(defun array-to-nested-vector (array)
  "Convert an array to a nested vector. Useful for applications such as JSON conversion where multidimensional arrays must be converted to nested vectors."
  (aops:each (lambda (member) (if (not (and (arrayp member) (< 1 (rank member))))
				  member (array-to-nested-vector member)))
	     (aops:split array 1)))

(defmacro avector (&rest items)
  "This macro returns an APL vector, disclosing data within that are meant to be individual atoms."
  (let ((type))
    (loop :for item :in items :while (not (eq t type))
       :do (setq type (type-in-common type (assign-element-type item))))
    `(make-array (list ,(length items))
		 :element-type (quote ,type)
		 ;; enclose each array included in an APL vector
		 :initial-contents (mapcar (lambda (item) (if (or (not (arrayp item))
								  (= 0 (rank item)))
							      item (make-array nil :initial-contents item)))
					   (list ,@items)))))

(defun numeric-string-p (string)
  "Checks whether the argument is a numeric string."
  (ignore-errors (parse-apl-number-string string)))

(defun parse-apl-number-string (number-string &optional component-of)
  "Parse an APL numeric string into a Lisp value, handling high minus signs, J-notation for complex numbers and R-notation for rational numbers."
  (let ((nstring (string-upcase number-string)))
    (if (and (not (eql 'complex component-of))
	     (find #\J nstring))
	(let ((halves (cl-ppcre:split "J" nstring)))
	  (if (and (= 2 (length halves))
		   (< 0 (length (first halves)))
		   (< 0 (length (second halves))))
	      (complex (parse-apl-number-string (first halves) 'complex)
		       (parse-apl-number-string (second halves) 'complex))))
	(if (and (not (eql 'rational component-of))
		 (find #\R nstring))
	    (let ((halves (cl-ppcre:split "R" nstring)))
	      (/ (parse-apl-number-string (first halves) 'rational)
		 (parse-apl-number-string (second halves) 'rational)))
	    ;; the macron character is converted to the minus sign
	    (parse-number:parse-number (regex-replace-all "[¯]" nstring "-"))))))

(defun print-apl-number-string (number &optional segments precision decimals realpart-multisegment)
  "Format a number as appropriate for APL, using high minus signs and J-notation for complex numbers, optionally at a given precision and post-decimal length for floats."
  (cond ((complexp number)
	 (format nil "~aJ~a" (print-apl-number-string (realpart number)
						      (list (first segments)
							    (if (or realpart-multisegment
								    (not (integerp (realpart number))))
								(if (not (third segments))
								    0 (- (second segments)))))
						      precision)
		 (print-apl-number-string (imagpart number) (if (third segments)
								(list (- (third segments))
								      (or (fourth segments) 0))
								(list (second segments) 0))
					  precision)))
	((integerp number)
	 (let ((output (format nil (format nil "~~~d,'~ad~a" (abs (first segments))
					   ;; for negative values, empty space to the left must initially
					   ;; be filled with ¯ characters; the reasoning is explained below
					   (if (> 0 number) #\¯ (if (> 0 (first segments)) #\0 #\ ))
					   (if (not (and (second segments)
							 (or (> 0 (first segments))
							     (> 0 (second segments)))))
					       "" (make-array (1+ (abs (second segments)))
							      :element-type 'base-char :initial-element #\ )))
			       (abs number)))
	       (number-found))
	   (if (> 0 number)
	       ;; replace ¯ padding with zeroes or spaces as appropriate; this strange system
	       ;; of initially padding with ¯ is needed because ¯ is an extended unicode character
	       ;; and strings of numeric characters are rendered as base-char arrays by (format),
	       ;; making it impossible to assign ¯ to their elements; unicode characters must be generated
	       ;; by (format) so that the output string is of type 'character; this is also done for floats
	       (loop :for i :from 1 :to (1- (length output)) :while (not number-found)
		  :do (if (alphanumericp (aref output i))
			  (setq number-found t)
			  (setf (aref output (1- i)) #\ ))))
	   output))
	((rationalp number)
	 (format nil "~ar~a" (print-apl-number-string (numerator number) (list (first segments)) precision)
		 (print-apl-number-string (denominator number) (list (- (abs (second segments)))) precision)))
	(t (let* ((number-string (write-to-string number))
		  (number-sections (cl-ppcre:split "[.]" number-string))
		  (right-padding (if (not (and (second segments) (< 0 (second segments))))
				     0 (max 0 (- (second segments) (length (second number-sections))))))
		  ;; space to left of decimal is expressed by the first segment
		  (left-space (abs (first segments)))
		  ;; space to right of decimal can be explicitly specified by decimal argument
		  ;; or expressed by second segment length, whose length may be expanded if the digits
		  ;; on the left don't fill out the precision, as with ⎕pp←6 ⋄ ⍪3005 0.125
		  (right-space (or decimals (max 1 (- (max (abs (second segments))
							   (min (length (second number-sections))
								(+ (abs (second segments))
								   (- (first segments)
								      (length (first number-sections))))))
						      right-padding))))
		  ;; total number length is left space + right space + one for decimal dot
		  (total-length (+ 1 left-space right-space))
		  (output (format nil (format nil "~~~d,~d,,,'~af~a" total-length right-space
					      (if (> 0 number)
						  #\¯ (if (< 0 (first segments)) #\  #\0))
					      (if (not (and right-padding (< 0 right-padding)))
						  "" (make-array right-padding :element-type 'base-char
								 :initial-element #\ )))
				  (abs number))))
	     (if (> 0 number)
		 (let ((start-at (if (< 0 (first segments)) 0 1)))
		   (loop :for i :from start-at :while (char= #\¯ (aref output i))
		      :when (or (= 1 start-at) (char= #\¯ (aref output (1+ i))))
		      :do (setf (aref output i) (aref " 0" start-at)))))
	     output))))

(defun format-value (idiom-name symbols element)
  "Convert a token string into an APL value, paying heed to APL's native ⍺, ⍵ and ⍬ variables."
  (cond ((string= element "⍬")
	 ;; APL's "zilde" character yields a keyword the compiler translates to an empty vector
	 :empty-array)
	((or (and (char= #\" (aref element 0))
		  (char= #\" (aref element (1- (length element)))))
	     (and (char= #\' (aref element 0))
		  (char= #\' (aref element (1- (length element))))))
	 ;; strings are converted to Lisp strings and passed through,
	 ;; unless they're one element in which case the character is disclosed
	 (if (= 3 (length element))
	     (aref element 1) (subseq element 1 (1- (length element)))))
	((member element '("⍺" "⍵") :test #'string=)
	 ;; alpha and omega characters are directly changed to symbols in the April package
	 (intern element idiom-name))
	((numeric-string-p element)
	 (parse-apl-number-string element))
	(t (or (and (char= #\⎕ (aref element 0))
		    (or (getf (rest (assoc :variable symbols))
			      (intern (string-upcase element) "APRIL"))
			(getf (rest (assoc :constant symbols))
			      (intern (string-upcase element) "APRIL"))))
	       (intern element)))))

(defun apl-timestamp ()
  "Generate an APL timestamp, a vector of the current year, month, day, hour, minute, second and millisecond."
  (let ((now (now)))
    (make-array '(7) :element-type '(integer 0 16384)
		:initial-contents (list (year-of now) (month-of now) (day-of now) (hour-of now)
					(minute-of now) (second-of now) (millisecond-of now)))))

(defun process-output-vector (items)
  "Process items in a vector to be generated by the compiler, wrapping any array references in aplSymbol so that they are disclosed. This does not apply if the output vector is unitary (length 1)."
  (loop :for item :in items :collect (if (and (< 1 (length items))
					      (listp item) (eql 'choose (first item)))
					 (list 'disclose item)
					 item)))

(defmacro with-operand-derived (operand-specs &rest body)
  "Derive references to glyphs and functions from a set of operands passed to an operator so they can be used by the macro implementing the operator."
  (let* ((first-op (gensym)) (first-axes (gensym)) (second-op (gensym)) (second-axes (gensym)))
    `(lambda (,first-op ,first-axes &optional ,second-op ,second-axes)
       (declare (ignorable ,second-op ,second-axes))
       (let ,(loop :for symbol :in operand-specs
		:collect (list symbol (case symbol (left-glyph (list 'or-functional-character first-op :fn))
					    (left-function-monadic
					     (list 'resolve-function :monadic first-op first-axes))
					    (left-function-dyadic
					     (list 'resolve-function :dyadic first-op first-axes))
					    (left-function-symbolic
					     (list 'resolve-function :symbolic first-op first-axes))
					    (right-glyph (list 'or-functional-character second-op :fn))
					    (right-function-monadic
					     (list 'resolve-function :monadic second-op second-axes))
					    (right-function-dyadic
					     (list 'resolve-function :dyadic second-op second-axes))
					    (right-function-symbolic
					     (list 'resolve-function :symbolic second-op second-axes)))))
	 ,@body))))

(defun resolve-function (mode reference &optional axes)
  "Retrieve the function corresponding to a given character or symbol in a given mode (monadic or dyadic)."
  (if (characterp reference)
      (if axes `(λχ ,(of-functions this-idiom reference mode) ,axes)
	  (of-functions this-idiom reference mode))
      (if (and (symbolp reference) (fboundp reference))
	  `(function ,reference)
	  (if (and (listp reference)
		   (or (eql 'lambda (first reference))
		       (and (macro-function (first reference))
			    (not (or (eql 'avector (first reference))
				     (eql 'apl-call (first reference)))))))
	      reference))))

(defmacro resolve-operator (mode reference)
  "Retrieve an operator's composing function."
  `(of-operators this-idiom ,reference ,mode))

(defun extract-axes (process tokens &optional axes)
  "Given a list of tokens starting with axis specifications, build the code for the axis specifications to be applied to the subsequent function or value."
  (labels ((process-axis (axis)
	     (multiple-value-bind (item item-props remaining)
		 (funcall process axis)
	       (declare (ignore remaining))
	       ;; allow either a null item (representing an elided axis) or an array
	       (if (or (not item) (eq :array (first (getf item-props :type))))
		   item (error "Invalid axis.")))))
    (if (and (listp (first tokens))
	     (eq :axes (caar tokens)))
	(extract-axes process (rest tokens)
		      (cons (loop :for axis :in (cdar tokens)
			       :collect (if (= 1 (length axis))
					    (process-axis axis)
					    (cons 'progn (mapcar #'process-axis axis))))
			    axes))
	(values axes (first tokens)
		(rest tokens)))))

(defun adjust-axes-for-index-origin (io axis-list)
  "Adjust axes passed to a function to account for the given index origin."
  (if (integerp (first axis-list))
      (- (first axis-list) io)
      (if (vectorp (first axis-list))
	  (let ((output (make-array (list (length (first axis-list)))))
		(ix 0))
	    (loop :for i :across (first axis-list) :do (setf (aref output ix) (- i io)
							     ix (1+ ix)))
	    output))))

(defmacro apl-call (symbol function &rest arguments)
  "Call an APL function with one or two arguments. Compose successive scalar functions into bigger functions for more efficiency."
  (declare (ignore symbol))
  (let ((arg (gensym "A")))
    (flet ((is-scalar (form) (and (listp form) (eql 'scalar-function (first form))))
	   (is-boolean (form) (and (listp form) (listp (second form))
				   (eql 'boolean-op (caadr form))))
	   (expand-monadic (fn argument)
	     (let ((arg-expanded (macroexpand argument)))
	       (if (and (listp arg-expanded)
			(eql 'apply-scalar (first arg-expanded))
			(not (fourth arg-expanded)))
		   (let ((innerfn (second arg-expanded)))
		     (list (if (not (eql 'lambda (first innerfn)))
			       `(lambda (,arg) (funcall ,fn (funcall ,innerfn ,arg)))
			       (list (first innerfn) (second innerfn)
				     `(funcall ,fn ,(third innerfn))))
			   (third arg-expanded)))
		   (list fn argument))))
	   (expand-dyadic (fn is-first arg1 arg2)
	     (let* ((arg-expanded (macroexpand (if is-first arg1 arg2))))
	       (if (and (listp arg-expanded)
			(eql 'apply-scalar (first arg-expanded))
			;; extract the sub-arguments within the expanded argument to the function; if one
			;; is a scalar value, the function may be merged into the containing closure
			(let ((sub-arg1 (if (and (listp (second arg-expanded))
						 (eql 'lambda (caadr arg-expanded)))
					    (third (third (second arg-expanded)))
					    (third arg-expanded)))
			      (sub-arg2 (if (and (listp (second arg-expanded))
						 (eql 'lambda (caadr arg-expanded)))
					    (fourth (third (second arg-expanded)))
					    (fourth arg-expanded))))
			  ;; one of the sub-arguments must be a number - or if there is no second argument,
			  ;; the inner function is monadic and the decomposition can proceed
			  (or (numberp sub-arg1)
			      (not sub-arg2)
			      (numberp sub-arg2))))
		   (let ((innerfn (second arg-expanded)))
		     (list (if (not (eql 'lambda (first innerfn)))
			       `(lambda (,arg) (funcall ,fn ,@(if (not is-first) (list arg1))
							(funcall ,innerfn ,arg
								 ;; include the inner function's
								 ;; second argument if present
								 ,@(if (fourth arg-expanded)
								       (list (fourth arg-expanded))))
							,@(if is-first (list arg2))))
			       (list (first innerfn) (second innerfn)
				     `(funcall ,fn ,@(if (not is-first) (list arg1))
					       ,(third innerfn) ,@(if is-first (list arg2)))))
			   (third arg-expanded)))))))
      (let* ((scalar-fn (is-scalar function))
	     (fn-body (cond ((and scalar-fn (not (second arguments)))
			     ;; compose monadic functions if the argument is the output of another scalar function
			     (expand-monadic function (first arguments)))
			    ((and scalar-fn (second arguments)
				  (numberp (first arguments)))
			     ;; compose dyadic functions if the first argument is a scalar numeric value
			     ;; and the other argument is the output of a scalar function
			     (let ((expanded (expand-dyadic function nil (first arguments) (second arguments))))
			       (or expanded `((lambda (,arg) (funcall ,function ,(first arguments) ,arg))
					      ,(macroexpand (second arguments))
					      nil))))
			    ((and scalar-fn (second arguments)
				  (numberp (second arguments)))
			     ;; same as above if the numeric argument is reversed
			     (let ((expanded (expand-dyadic function t (first arguments) (second arguments))))
			       (or expanded `((lambda (,arg) (funcall ,function ,arg ,(second arguments)))
					      ,(macroexpand (first arguments))
					      nil))))
			    ;; otherwise, just list the function and its arguments
			    (t (cons function arguments)))))
	(append (list (if scalar-fn 'apply-scalar 'funcall))
		(if (and scalar-fn (= 4 (length fn-body)))
		    ;; if the function is scalar and an axis argument is present,
		    ;; adjust the numerical axis values according to the index origin
		    (append (butlast fn-body 1)
			    `((adjust-axes-for-index-origin index-origin ,(fourth fn-body))))
		    fn-body)
		(if (and scalar-fn (= 2 (length fn-body)))
		    '(nil))
		(if (and scalar-fn (is-boolean function))
		    '(nil t)))))))

#|
This is a minimalistic implementation of (apl-call) that doesn't perform any function composition.
It remains here as a standard against which to compare methods for composing APL functions.

(defmacro apl-call (symbol function &rest arguments)
  (declare (ignore symbol))
  `(,(if (and (listp function)
	      (eql 'scalar-function (first function)))
	 'apply-scalar 'funcall)
     ,function  ,@arguments))
|#

(defmacro apl-compose (symbol &rest body)
  "A wrapper macro for macros that implement April's operators; functionally this macro does nothing but it improves the readability of April's compiled code."
  (declare (ignore symbol))
  (macroexpand body))

(defmacro scalar-function (function)
  "Wrap a scalar function. This is a passthrough macro used by the scalar composition system in (apl-call)."
  (if (symbolp function)
      `(function ,function)
      function))

(defun validate-arg-unitary (value)
  "Verify that a form like (vector 5) represents a unitary value."
  (or (symbolp value)
      (numberp value)
      (and (listp value)
	   (or (not (eql 'vector (first value)))
	       (not (third value))))))

(defmacro or-functional-character (reference symbol)
  "Return a symbol representing a functional character or, if the passed value is not a character, an arbitrary fallback symbol. Used to derive the initial symbol argument for (apl-call)."
  `(if (not (characterp ,reference))
       ,symbol (intern (string-upcase ,reference))))

(defun enclose-axes (body axis-sets &key (set nil))
  "Apply axes to an array, with the ability to handle multiple sets of axes as in (6 8 5⍴⍳9)[1 4;;2 1][1;2 4 5;]."
  (let ((axes (first axis-sets)))
    (if (not axis-sets)
	body (enclose-axes
	      (if set `(multiple-value-bind (assignment-output assigned-array)
			   (choose ,body (mapcar (lambda (array) (if array (apply-scalar #'- array index-origin)))
						 (list ,@axes))
				   :set ,set)
			 (if assigned-array (setf ,body assigned-array)
			     assignment-output))
		  `(choose ,body (mapcar (lambda (array) (if array (apply-scalar #'- array index-origin)))
					 (list ,@axes))))
	      (rest axis-sets)))))

(defun coerce-type (array type-index)
  (let ((type (case type-index (0 t) (1 'bit) (2 '(unsigned-byte 2)) (3 '(unsigned-byte 4))
		    (4 '(unsigned-byte 8)) (5 '(unsigned-byte 16)) (6 '(unsigned-byte 32))
		    (7 '(unsigned-byte 64)) (8 'fixnum) (9 'float) (10 'double)
		    (11 'character))))
    (if (or (not (arrayp array))
	    (equalp type (element-type array)))
	array (let ((output (make-array (dims array) :element-type type)))
		(dotimes (i (size array)) (setf (row-major-aref output i)
						(row-major-aref array i)))
		output))))

(defun output-value (space form &optional properties)
  "Express an APL value in the form of an explicit array specification or a symbol representing an array, supporting axis arguments."
  (labels ((enclose-symbol (item)
	     (if (or (not (symbolp item))
		     (member item *idiom-native-symbols*))
		 item `(inws ,item)))
	   (apply-props (item form-props)
	     (let ((form-props (if (not (listp (first form-props)))
				   form-props (first form-props))))
	       (if (getf form-props :axes)
		   (enclose-axes (enclose-symbol item)
				 (getf form-props :axes))
		   (enclose-symbol item)))))
    (let ((properties (reverse properties)))
      (if form (if (listp form)
		   (if (eql 'avector (first form))
		       form (if (not (or (numberp (first form))
					 (listp (first form))
					 (stringp (first form))
					 (characterp (first form))
					 (and (arrayp (first form))
					      (= 0 (size (first form))))
					 (eql '⍺ (first form))
					 (eql '⍵ (first form))
					 (and (symbolp (first form))
					      (and (boundp (intern (string (first form)) space))
						   (not (fboundp (intern (string (first form)) space)))))))
				(if (= 1 (length properties))
				    (apply-props form (first properties))
				    (mapcar #'apply-props form properties))
				(if (getf (first properties) :vector-axes)
				    (enclose-axes `(avector ,@(mapcar #'apply-props form properties))
						  (getf (first properties) :vector-axes))
				    `(avector ,@(mapcar #'apply-props form properties)))))
		   (if (not (numberp form))
		       (apply-props form properties)
		       form))))))

(defun output-function (form &optional arguments)
  "Express an APL inline function like {⍵+5}."
  (let ((arguments (if arguments (mapcar (lambda (item) `(inws ,item)) arguments))))
    `(lambda ,(if arguments arguments `(⍵ &optional ⍺))
       (declare (ignorable ,@(if arguments arguments '(⍵ ⍺))))
       ,@form)))

(defun do-over (input function axis &key reduce in-reverse)
  "Apply a dyadic function over elements of an array, inserting the results into an array of the same or similar shape (possibly less one or more dimensions). Used to implement reduce and scan operations."
  (let* ((dimensions (dims input))
	 (output (make-array (if reduce (loop :for dim :in dimensions :for dx :from 0
					   :when (/= dx axis) :collect dim)
				 dimensions)))
	 (rcoords (if reduce (loop :for i :below (1- (rank input)) :collect 0)))
	 (icoords (loop :for i :below (rank input) :collect 0)))
    (across input (lambda (elem coords)
		    (declare (dynamic-extent elem coords))
		    (if rcoords (let ((decrement 0))
				  (loop :for c :in coords :for cx :from 0
				     :when (= axis cx) :do (incf decrement)
				     :when (/= axis cx) :do (setf (nth (- cx decrement) rcoords) c))))
		    (loop :for c :in coords :for cx :from 0 :do (setf (nth cx icoords)
								      (if (/= axis cx) c (if (not reduce)
											     (1- c)))))
		    (let ((elem (disclose elem)))
		      (setf (apply #'aref output (if reduce rcoords coords))
			    (if (= (if (not in-reverse) 0 (1- (nth axis dimensions)))
				   (nth axis coords))
				elem (funcall function elem (apply #'aref output
								   (if reduce rcoords
								       (or icoords (list 0)))))))))
	    :ranges (loop :for d :below (rank input) :collect (if (and in-reverse (= axis d))
								  (list (nth d dimensions)))))
    output))

(defun build-variable-declarations (input-vars space)
  "Create the set of variable declarations that begins April's compiled code."
  (loop :for var-entry :in input-vars :collect (list (intern (lisp->camel-case (first var-entry)) space)
						     (second var-entry))))

(defun build-compiled-code (exps options system-vars vars-declared space)
  "Return a set of compiled April expressions within the proper context."
  (let* ((branch-index (gensym "A")) (branches-sym (intern "*BRANCHES*" space))
	 (tags-found (loop :for exp :in exps :when (symbolp exp) :collect exp))
	 (tags-matching (loop :for tag :in (symbol-value branches-sym)
			   :when (or (and (listp tag) (member (second tag) tags-found))) :collect tag)))
    (flet ((process-tags (form)
	     (loop :for sub-form :in form
		:collect (if (not (and (listp sub-form) (eql 'go (first sub-form))
				       (not (symbolp (second sub-form)))))
			     sub-form (if (integerp (second sub-form))
					  (if (assoc (second sub-form) tags-matching)
					      (list 'go (second (assoc (second sub-form) tags-matching))))
					  (if (third sub-form)
					      `(let ((,branch-index ,(third sub-form)))
						 (cond ,@(loop :for tag :in (second sub-form)
							    :counting tag :into tix
							    :collect `((= ,branch-index ,tix)
								       (go ,tag)))))
					      `(let ((,branch-index ,(second sub-form)))
						 (cond ,@(loop :for tag :in tags-matching
							    :when (and (listp tag)
								       (member (second tag) tags-found))
							    :collect `((= ,branch-index ,(first tag))
								       (go ,(second tag))))))))))))
      (funcall (lambda (code) (if (not (assoc :compile-only options))
				  code `(quote ,code)))
	       (if (or system-vars vars-declared)
		   `(with-april-workspace ,(or (second (assoc :space options)) 'common)
		      (let* (,@system-vars ,@vars-declared)
			(declare (ignorable ,@(mapcar #'first system-vars)))
			,@(if (or (not tags-found) (not (boundp branches-sym)))
			      exps `((tagbody ,@(butlast (process-tags exps) 1))
				     ,(first (last exps))))))
		   (if (< 1 (length exps))
		       (cons 'progn exps) (first exps)))))))

(defun april-function-glyph-processor (type glyph spec)
  "Convert a Vex function specification for April into a set of lexicon elements, forms and functions that will make up part of the April idiom object used to compile the language."
  (let ((type (intern (string-upcase type) "KEYWORD"))
	(function-type (intern (string-upcase (first spec)) "KEYWORD"))
	(spec-body (rest spec)))
    (cond ((eq :symbolic function-type)
	   `(,glyph :lexicons (:functions :symbolic-functions)
		    :functions (:symbolic ,(first spec-body))))
	  ((keywordp (first spec-body))
	   ;; if this is a simple scalar declaration passing through another function
	   `(,glyph :lexicons (:functions :scalar-functions :monadic-functions :scalar-monadic-functions
					  ,@(if (not (eq :monadic function-type))
						(list :dyadic-functions :scalar-dyadic-functions)))
		    :functions ,(append (if (or (eq :ambivalent function-type)
						(eq :monadic function-type))
					    (list :monadic `(scalar-function ,(second spec-body))))
					(if (or (eq :ambivalent function-type)
						(eq :dyadic function-type))
					    (list :dyadic `(scalar-function ,(first (last spec-body))))))))
	  (t `(,glyph :lexicons ,(cond ((eq :functions type)
					`(:functions ,@(if (eq :ambivalent function-type)
							   '(:monadic-functions :dyadic-functions)
							   (list (intern (string-upcase
									  (concatenate 'string
										       (string function-type)
										       "-" (string type)))
									 "KEYWORD")))
						     ,@(if (and (or (eq :ambivalent function-type)
								    (eq :monadic function-type))
								(eql 'scalar-function (caar spec-body)))
							   '(:scalar-functions :scalar-monadic-functions))
						     ,@(if (or (and (eq :dyadic function-type)
								    (eql 'scalar-function (caar spec-body)))
							       (and (eq :ambivalent function-type)
								    (eql 'scalar-function (caadr spec-body))))
							   '(:scalar-functions :scalar-dyadic-functions))))
				       ((eq :operators type)
					`(:operators ,(if (eq :lateral function-type)
							  :lateral-operators
							  (if (eq :pivotal function-type)
							      :pivotal-operators :unitary-operators)))))
		      ,@(cond ((eq :functions type)
			       `(:functions ,(append (if (or (eq :ambivalent function-type)
							     (eq :monadic function-type))
							 (list :monadic (first spec-body)))
						     (if (eq :ambivalent function-type)
							 (list :dyadic (second spec-body))
							 (if (eq :dyadic function-type)
							     (list :dyadic (first spec-body)))))))
			      ((eq :operators type)
			       `(:operators ,(first spec-body)))))))))

;; a secondary package containing a set of tools for the extension of April idioms
(defpackage #:april.idiom-extension-tools
  (:import-from :april #:extend-vex-idiom #:april-function-glyph-processor #:scalar-function)
  (:export #:extend-vex-idiom #:april-function-glyph-processor #:scalar-function
	   #:λω #:λωα #:λωχ #:λωαχ))
