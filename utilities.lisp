;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; utilities.lisp

(in-package #:april)

"Utility functions for April. It's important to understand the difference between the functions and macros provided here and the ones that come from the aplesque package. The faculties provided by aplesque reflect features of APL, but they have uses other than implementing APL. The material here is specifically for use in implementing APL, with uses aside from an APL implementation not accounted for in its design. The functions here are used to implement language mechanics as opposed to functions in the language's standard library; the latter are implemented in library.lisp."

(define-symbol-macro this-idiom (local-idiom april))
(define-symbol-macro atomic-vector (get-system-meta this-idiom :atomic-vector))
(define-symbol-macro *apl-timestamp* (apl-timestamp))
(define-symbol-macro *first-axis* (if axes (- (first axes) index-origin)
				      0))
(define-symbol-macro *last-axis* (- (if axes (first axes)
					(max 1 (rank omega)))
				    index-origin))
(define-symbol-macro *first-axis-or-nil* (if axes (- (first axes) index-origin)))

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
  (if (eql 'λωαχ (first body))
      `(λωα (funcall ,body omega alpha ,(cons 'list axes)))
      (if (eql 'λωχ (first body))
	  `(λω (funcall ,body omega ,(cons 'list axes)))
	  body)))

(defmacro print-and-run (form)
  "Print a formatted code string and then run the code; used in April's arbitrary evaluation tests."
  `(progn (princ (indent-code (string-downcase (write-to-string (quote ,form)))))
	  ,form))

(defun indent-code (string)
  "Indent a code string produced by (print-and-run) as appropriate for April's test output."
  (concatenate 'string "  * " (regex-replace-all "[\\n]" string (format nil "~%    "))))

(defun disclose-atom (item)
  "If the argument is a non-nested array with only one member, disclose it, otherwise do nothing."
  (if (and (not (stringp item))
	   (arrayp item)
	   (is-unitary item)
	   (not (arrayp (row-major-aref item 0))))
      (row-major-aref item 0)
      item))

(defmacro in-apl-workspace (workspace-symbol body)
  "This macro encloses a body of compiled April code specifying a workspace in use for the code, and extends any assignment so as to update the workspace's stored values."
  (labels ((process (form)
	     (loop :for item :in form
		:collect (if (and (listp item) (eql 'apl-assign (first item)))
			     (cond ((or (eql 'index-origin (second item))
					(eql 'print-precision (second item)))
				    ;; if it's a system variable, assign the corresponding value
				    ;; in the workspace as well as in the lexical environment
				    `(progn ,(macroexpand item)
					    (setf (getf (getf (gethash :system ,workspace-symbol) :state)
							,(intern (string-upcase (second item)) "KEYWORD"))
						  ,(second (third item)))))
				   ;; if it's a regular variable, set the workspace value and process the
				   ;; assigned item as well to allow for chained assignment like x←y←5
				   (t (list 'setq (second item)
					    `(setf (gethash ',(second item)
							    (gethash :values ,workspace-symbol))
						   ,(if (listp (third item))
							(first (process (list (third item))))
							(third item))))))
			     (if (listp item)
				 (process item)
				 item)))))
    (process body)))

(defmacro apl-assign (symbol value)
  "This is a simple passthrough macro that is used by (in-apl-workspace)."
  `(setq ,symbol ,value))

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
						  :format (lambda (n) (print-apl-number-string
								       n t ,(getf options :print-precision))))))))
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
		 :initial-contents (list ,@items))))

(defun apply-scalar (function alpha &optional omega axes is-boolean)
  "Apply a scalar function across objects as appropriate for APL. Handles scalars as well as nested and multidimensional arrays."
  (macrolet ((for-each (function &rest body)
	       `(funcall (if (and is-boolean (not (eq t (element-type alpha)))
				  (or (not omega) (not (eq t (element-type omega)))))
			     #'each-boolean #'each-scalar)
			 (lambda (elem coords)
			   (declare (ignorable coords) (dynamic-extent elem))
			   ,(if (second body)
				`(funcall ,function elem (apply #'aref (cons ,(second body) coords)))
				`(funcall ,function elem)))
			 ,(first body))))
    (if (not omega)
	(let ((omega alpha))
	  (if (arrayp omega)
	      (labels ((apply-fn (arg)
			 (if (arrayp arg) (apply-scalar #'apply-fn arg nil axes is-boolean)
			     (funcall function arg))))
		(for-each #'apply-fn omega))
	      (funcall function omega)))
	(let* ((alpha-scalar? (not (arrayp alpha)))
	       (omega-scalar? (not (arrayp omega)))
	       (alpha-unitary? (or alpha-scalar? (is-unitary alpha)))
	       (omega-unitary? (or omega-scalar? (is-unitary omega))))
	  (cond ((and alpha-scalar? omega-scalar?)
		 (funcall function alpha omega))
		((and alpha-scalar? omega-unitary?)
		 (for-each (lambda (a o) (apply-scalar function a o axes is-boolean))
			   (vector alpha) omega))
		((and alpha-unitary? omega-scalar?)
		 (for-each (lambda (a o) (apply-scalar function a o axes is-boolean))
			   alpha (vector omega)))
		((and alpha-unitary? omega-unitary?)
		 (for-each (lambda (a o) (apply-scalar function a o axes is-boolean))
			   alpha omega))
		((not (or alpha-unitary? omega-unitary? alpha-scalar? omega-scalar?))
		 (if (or (not (or (dims alpha)
		 		  (dims omega)))
		 	 (and (= 0 (size alpha))
		 	      (= 0 (size omega))))
		     omega (if (loop for a in (dims alpha) and o in (dims omega) :always (= a o))
			       (for-each (lambda (alpha omega) (apply-scalar function alpha omega axes is-boolean))
					 alpha omega)
			       (if axes (destructuring-bind (lowrank-array highrank-array omega-lower)
					    (if (< (rank alpha) (rank omega))
						(list alpha omega nil) (list omega alpha t))
					  (setq axes (enclose axes))
					  (if (loop :for a :across axes :counting a :into ax
						 :always (= (nth (1- ax) (dims lowrank-array))
							    (nth a (dims highrank-array))))
					      (let ((output (make-array (dims highrank-array)))
						    (lr-coords (loop :for i :below (rank lowrank-array)
								  :collect 0)))
						(across highrank-array
							(lambda (elem coords)
							  (declare (dynamic-extent elem coords))
							  (loop :for a :across axes :counting a :into ax
							     :do (setf (nth (1- ax) lr-coords) (nth a coords)))
							  (setf (apply #'aref output coords)
								(if omega-lower
								    (funcall function elem
									     (apply #'aref lowrank-array lr-coords))
								    (funcall function (apply #'aref lowrank-array
											     lr-coords)
									     elem)))))
						output)
					      (error "Axis error.")))
				   (error "Array size mismatch.")))))
		(t (labels ((scan-over (element)
			      (if (arrayp element)
				  (for-each #'scan-over element)
				  (multiple-value-bind (left right)
				      (cond (alpha-scalar? (values alpha element))
					    (alpha-unitary? (values (disclose alpha) element))
					    (omega-scalar? (values element omega))
					    (omega-unitary? (values element (disclose omega))))
				    (apply-scalar function left right axes is-boolean)))))
		     (for-each #'scan-over (if (or alpha-scalar? alpha-unitary?)
					       omega alpha)))))))))

(defun numeric-string-p (string)
  "Checks whether the argument is a numeric string."
  (ignore-errors (parse-apl-number-string string)))

(defun parse-apl-number-string (number-string &optional imaginary-component)
  "Parse an APL numeric string into a Lisp value, handling high minus signs and the J-notation for complex numbers."
  (let ((nstring (string-upcase number-string)))
    (if (and (not imaginary-component)
	     (find #\J nstring))
	(let ((halves (cl-ppcre:split "J" nstring)))
	  (if (and (= 2 (length halves))
		   (< 0 (length (first halves)))
		   (< 0 (length (second halves))))
	      (complex (parse-apl-number-string (first halves) t)
		       (parse-apl-number-string (second halves) t))))
	;; the macron character is converted to the minus sign
	(parse-number:parse-number (regex-replace-all "[¯]" nstring "-")))))

(defun print-apl-number-string (number &optional coerce-rational precision decimals)
  "Format a number as appropriate for APL, using high minus signs and J-notation for complex numbers, optionally at a given precision and post-decimal length for floats."
  (cond ((complexp number)
	 (format nil "~aJ~a" (print-apl-number-string (realpart number) coerce-rational precision)
		 (print-apl-number-string (imagpart number) coerce-rational precision)))
	((> 0 number)
	 (format nil "¯~a" (print-apl-number-string (abs number) coerce-rational precision)))
	((integerp number)
	 (format nil "~D" number))
	((zerop number) "0")
	;; zero-value numbers are printed as zeroes with no decimal; this clause catches
	;; zeroes before they can cause divide-by-zero errors in the (log)s afterward
	((and coerce-rational (rationalp number))
	 (let ((before-decimal (max 1 (1+ (floor (log number 10))))))
	   (format-decimal-number number :round-magnitude (min 0 (- (- precision before-decimal))))))
	((rationalp number)
	 (write-to-string number))
	(t (if (not precision)
	       (format nil "~F" number)
	       (let ((printed (if (not decimals)
				  (loop :for digit :across (write-to-string number) :when (digit-char-p digit)
				     :counting digit :into digits :finally (return digits))))
		     (before-decimal (max 1 (1+ (floor (log number 10))))))
		 (format nil (format nil "~~~D,~D,F" (if decimals (+ 1 before-decimal decimals)
							 (min (1+ printed) (1+ precision)))
				     (if decimals decimals (- (min printed precision) before-decimal)))
			 number))))))

(defun format-value (idiom-name meta symbols element)
  "Convert a token string into an APL value, paying heed to APL's native ⍺, ⍵ and ⍬ variables."
  (cond ((string= element "⍬")
	 ;; APL's "zilde" character yields a keyword the compiler translates to an empty vector
	 :empty-array)
	((or (and (char= #\" (aref element 0))
		  (char= #\" (aref element (1- (length element)))))
	     (and (char= #\' (aref element 0))
		  (char= #\' (aref element (1- (length element))))))
	 ;; strings are converted to Lisp strings and passed through
	 (subseq element 1 (1- (length element))))	       
	((or (string= element "⍺")
	     (string= element "⍵"))
	 ;; alpha and omega characters are directly changed to symbols
	 (intern element idiom-name))
	((numeric-string-p element)
	 (parse-apl-number-string element))
	(t (let ((vars-table (gethash :variables meta))
		 (elem-keyword (intern element "KEYWORD")))
	     (or (and (char= #\⎕ (aref element 0))
		      (or (getf (rest (assoc :variable symbols))
				(intern (string-upcase element) "APRIL"))
			  (getf (rest (assoc :constant symbols))
				(intern (string-upcase element) "APRIL"))))
		 (if (not vars-table)
		     (setf vars-table (make-hash-table :test #'eq)))
		 (let ((variable-found (gethash elem-keyword vars-table)))
		   (if variable-found variable-found
		       ;; create a new variable if no variable is found matching the string
		       (setf (gethash elem-keyword vars-table)
			     (gensym "A")))))))))

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
  (let* ((workspace (gensym)) (first-op (gensym)) (first-axes (gensym))
	 (second-op (gensym)) (second-axes (gensym)))
    `(lambda (,workspace ,first-op ,first-axes &optional ,second-op ,second-axes)
       (declare (ignorable ,second-op ,second-axes))
       (let ,(loop :for symbol :in operand-specs
		:collect (list symbol (cond ((eq symbol 'left-glyph)
					     (list 'or-functional-character first-op :fn))
					    ((eq symbol 'left-function-monadic)
					     (list 'resolve-function workspace :monadic first-op first-axes))
					    ((eq symbol 'left-function-dyadic)
					     (list 'resolve-function workspace :dyadic first-op first-axes))
					    ((eq symbol 'left-function-symbolic)
					     (list 'resolve-function workspace :symbolic first-op first-axes))
					    ((eq symbol 'right-glyph)
					     (list 'or-functional-character second-op :fn))
					    ((eq symbol 'right-function-monadic)
					     (list 'resolve-function workspace :monadic second-op second-axes))
					    ((eq symbol 'right-function-dyadic)
					     (list 'resolve-function workspace :dyadic second-op second-axes))
					    ((eq symbol 'right-function-symbolic)
					     (list 'resolve-function workspace :symbolic second-op second-axes)))))
	 ,@body))))

(defun resolve-function (workspace mode reference &optional axes)
  (if (characterp reference)
      (if axes `(λχ ,(of-functions this-idiom reference mode) ,axes)
	  (of-functions this-idiom reference mode))
      (if (or (and (symbolp reference)
		   (gethash reference (gethash :functions workspace)))
	      (and (listp reference)
		   (or (eql 'lambda (first reference))
		       (and (macro-function (first reference))
			    (not (or (eql 'avector (first reference))
				     (eql 'apl-call (first reference))))))))
	  reference)))

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
		    '(t)))))))

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

(defun output-value (space form &optional properties)
  "Express an APL value in the form of an explicit array specification or a symbol representing an array, supporting axis arguments."
  (flet ((apply-props (item form-props)
	   (let ((form-props (if (listp (first form-props))
				 (first form-props)
				 form-props)))
	     (if (getf form-props :axes)
		 (enclose-axes item (getf form-props :axes))
		 item))))
    (let ((properties (reverse properties)))
      (if form (if (listp form)
		   (if (not (or (numberp (first form))
				(listp (first form))
				(stringp (first form))
				(eql '⍺ (first form))
				(eql '⍵ (first form))
				(and (symbolp (first form))
				     (or (gethash (string (first form))
						  (gethash :values space))
					 (not (loop :for key :being :the :hash-keys :of (gethash :variables space)
						 :never (eql (first form)
							     (gethash key (gethash :variables space)))))))))
		       (if (= 1 (length properties))
			   (apply-props form (first properties))
			   (mapcar #'apply-props form properties))
		       `(avector ,@(mapcar #'apply-props form properties)))
		   (if (not (numberp form))
		       (apply-props form properties)
		       form))))))

(defun output-function (form &optional arguments)
  "Express an APL inline function like {⍵+5}."
  `(lambda ,(if arguments arguments `(⍵ &optional ⍺))
     (declare (ignorable ,@(if arguments arguments '(⍵ ⍺))))
     ,@form))

(defun do-over (input function axis &key reduce in-reverse)
  "Apply a dyadic function over elements of an array, inserting the results into an array of the same or similar shape (possibly less one or more dimensions). Used to implement reduce and scan operations."
  (let* ((dimensions (dims input))
	 (output (make-array (if reduce (or (loop :for dim :in dimensions :counting dim :into dx
					       :when (/= dx (1+ axis)) :collect dim)
					    '(1))
				 dimensions)))
	 (reduce-coords (if reduce (or (loop :for i :below (1- (rank input)) :collect 0) '(0))))
	 (ref-coords (loop :for i :below (rank input) :collect 0)))
    (across input (lambda (elem coords)
		    (declare (dynamic-extent elem coords))
		    (if reduce-coords (let ((decrement 0))
					(loop :for c :in coords :counting c :into cx
					   :when (= axis (1- cx)) :do (incf decrement)
					   :when (/= axis (1- cx))
					   :do (setf (nth (- cx 1 decrement) reduce-coords) c))))
		    (loop :for c :in coords :counting c :into cx :do (setf (nth (1- cx) ref-coords)
									   (if (/= axis (1- cx))
									       c (if (not reduce)
										     (1- c)))))
		    (setf (apply #'aref output (if reduce reduce-coords coords))
			  (if (= (if (not in-reverse) 0 (1- (nth axis dimensions)))
				 (nth axis coords))
			      elem (funcall function elem
					    (apply #'aref output (or reduce-coords ref-coords (list 0)))))))
	    :reverse-axes (if in-reverse (list axis)))
    (each-scalar t output)))

(defun build-variable-declarations (options input-vars preexisting-vars var-symbols meta)
  "Create the set of variable declarations that begins April's compiled code."
  (let* ((workspace (second (assoc :space options)))
	 (declarations (loop :for key-symbol :in var-symbols
			  :when (not (member (string (gethash (first key-symbol) (gethash :variables meta)))
					     (mapcar #'first input-vars)))
			  :collect (let* ((sym (second key-symbol))
					  (fun-ref (gethash sym (gethash :functions meta)))
					  (val-ref (if workspace `(gethash (quote ,sym)
									   (gethash :values ,workspace))
						       (gethash sym (gethash :values meta)))))
				     (list sym (if (member sym preexisting-vars)
						   ;; (if val-ref val-ref (if fun-ref fun-ref))
						   (if fun-ref fun-ref (if val-ref val-ref))
						   :undefined))))))
    ;; update the variable records in the meta object if input variables are present
    (if input-vars (loop :for var-entry :in input-vars
		      :do (symbol-macrolet ((vdata (gethash (intern (lisp->camel-case (first var-entry)) "KEYWORD")
							    (gethash :variables meta))))
			    (if vdata (rplacd (assoc vdata declarations)
					      (list (second var-entry)))
				(setq declarations (append declarations (list (list (setf vdata (gensym))
										    (second var-entry)))))))))
    declarations))

(defun build-compiled-code (exps options system-vars vars-declared var-symbols meta)
  "Return a set of compiled April expressions within the proper context."
  (let ((tb-output (gensym "A"))
	(branch-index (gensym "A")))
    (flet ((process-tags (form tags)
	     (loop :for sub-form :in form
		:collect (if (not (and (listp sub-form) (eql 'go (first sub-form))
				       (not (symbolp (second sub-form)))))
			     sub-form (if (integerp (second sub-form))
					  (if (assoc (second sub-form) tags)
					      (list 'go (second (assoc (second sub-form) tags))))
					  (if (third sub-form)
					      `(let ((,branch-index ,(third sub-form)))
						 (cond ,@(loop :for tag :in (second sub-form)
							    :counting tag :into tix
							    :collect `((= ,branch-index ,tix)
								       (go ,tag)))))
					      `(let ((,branch-index ,(second sub-form)))
						 (cond ,@(loop :for tag :in tags
							    :collect `((= ,branch-index ,(first tag))
								       (go ,(second tag))))))))))))
      (funcall (lambda (code) (if (not (assoc :compile-only options))
				  code `(quote ,code)))
	       (if (or system-vars vars-declared)
		   (funcall (lambda (workspace form)
			      (funcall (if (not workspace)
					   #'identity (lambda (form) `(in-apl-workspace ,workspace ,form)))
				       form))
			    (second (assoc :space options))
			    `(let* (,@system-vars ,@vars-declared)
			       (declare (ignorable ,@(mapcar #'first system-vars)
						   ,@(mapcar #'second var-symbols)))
			       ,@(if (not (gethash :branches meta))
				     exps `((let ((,tb-output))
					      (tagbody ,@(funcall
							  (lambda (list)
							    (append (butlast list 1)
								    `((setq ,tb-output
									    ,(first (last list))))))
							  (process-tags exps (gethash :branches meta))))
					      ,tb-output)))))
		   (if (< 1 (length exps))
		       `(progn ,@exps)
		       (first exps)))))))

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
							     (list :dyadic (first spec-body))))
						     (if (eq :symbolic function-type)
							 (list :symbolic (first spec-body))))))
			      ((eq :operators type)
			       `(:operators ,(first spec-body)))))))))

;; a secondary package containing a set of tools for the extension of April idioms
(defpackage #:april.idiom-extension-tools
  (:import-from :april #:extend-vex-idiom #:april-function-glyph-processor #:scalar-function)
  (:export #:extend-vex-idiom #:april-function-glyph-processor #:scalar-function
	   #:λω #:λωα #:λωχ #:λωαχ))
