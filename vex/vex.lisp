;;;; vex.lisp

(in-package #:vex)

(defvar *vex-idiom*)

(defclass idiom ()
  ((name :accessor idiom-name
	 :initarg :name)
   (environment :accessor idiom-environment
		:initarg :environment)
   (parsers :accessor idiom-parsers
	    :initform ""
	    :initarg :function-matcher)
   (functions :accessor idiom-functions
	      :initform nil
	      :initarg :functions)
   (operators :accessor idiom-operators
	      :initform nil
	      :initarg :operators)
   (operational-glyphs :accessor idiom-opglyphs
		       :initform nil
		       :initarg :operational-glyphs)
   (operator-index :accessor idiom-opindex
		   :initform nil
		   :initarg :operator-index)
   (overloaded-lexicon :accessor idiom-overloaded-lexicon
		       :initform nil
		       :initarg :overloaded-lexicon)))

(defgeneric of-environment (idiom property))
(defmethod of-environment ((idiom idiom) property)
  (getf (idiom-environment idiom) property))

(defun handle-argument (operation omega &optional alpha)
  (if (and (symbolp (first operation))
	   (macro-function (first operation))
	   (not (eql 'lambda (first operation))))
      (cond ((eql 'args (first operation))
	     (macroexpand (append (cons 'args (cons '*vex-idiom* (last operation)))
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

(defmacro ambivalent (operation second-operation &optional third-input)
  `((if alpha
	,(handle-argument (if (eq :symmetric-scalar operation)
			      (list 'args :scalar second-operation)
			      (if (eq :asymmetric-scalar operation)
				  (list 'args :scalar third-input)
				  second-operation))
			  'omega 'alpha)
	,(handle-argument (if (eq :symmetric-scalar operation)
			      (list 'args :scalar second-operation)
			      (if (eq :asymmetric-scalar operation)
				  (list 'args :scalar second-operation)
				  operation))
			  'omega))))

(defmacro monadic (operation)
  `((if alpha
	`(progn (error "Valence error - monadic operation."))
	,(handle-argument operation 'omega))))

(defmacro dyadic (operation)
  `((if alpha
	,(handle-argument operation 'omega 'alpha)
	`(error "Valence error - dyadic operation."))))

(defmacro args (idiom operation omega &optional alpha axes)
  "Moderate arguments to a Vex function at compile time."
  (let ((fn (if (and (listp operation)
		     (macro-function (first operation))
		     (not (eql 'lambda (first operation))))
		(macroexpand (append operation (if alpha (list 'omega 'alpha)
						   (list 'omega))))
		``(function ,',operation)))
	(uses-idiom (and (listp operation)
			 (eql 'lambda (first operation))
			 (string= "IDIOM" (string-upcase (first (second operation)))))))
    (if (eq omega :scalar)
	`(if alpha
	     `(apply-scalar-function ,,fn ,@(if ,uses-idiom (list ,idiom))
				     ,(macroexpand omega)
				     ,(macroexpand alpha))
	     `(aops:each ,(if ,uses-idiom
			      `(funcall ,,fn ,,idiom)
			      ,fn)
			 ,(macroexpand omega)))
	``(if (and ,(if (eq :any ,omega)
			t (cond ((eq :one ,omega)
				 `(is-singleton ,(macroexpand omega)))
				((eq :sym ,omega)
				 (if (eql 'lambda (first alpha))
				     (setf (gethash :functions meta)
				 	   (cons (macroexpand omega)
						 (gethash :functions meta))))
				 `(symbolp (quote ,(if (listp (macroexpand omega))
						       (second (getf (macroexpand omega)
								     :initial-contents))
						       (macroexpand omega)))))))
		   ,@(if (and ,alpha (not (eq :any ,alpha)))
			 (if (eq :one ,alpha)
			     (list `(is-singleton ,(macroexpand alpha))))))
	      ;; if the arguments are scalar (:one), remove them from their arrays for evaluation
	      (funcall ,,fn ,@(if ,uses-idiom (list ,idiom))
		       ,(cond ((eq :one ,omega)
				    `(if (arrayp ,(macroexpand omega))
					 (aref ,(macroexpand omega) 0)
					 ,(macroexpand omega)))
				   ((eq :sym ,omega)
				    `(quote ,(if (listp (macroexpand omega))
						 (second (getf (macroexpand omega)
							       :initial-contents))
						 (macroexpand omega))))
				   (t (macroexpand omega)))
		       ,@(if ,alpha (list (cond ((eq :one ,alpha)
						 `(aref ,(macroexpand alpha) 0))
						((eq :axes ,alpha)
						 (cons 'list (macroexpand axes)))
						;; alpha is equal to :axes when
						;; axes are used for a monadic function
						(t (macroexpand alpha)))))
		       ,@(if ,axes (list (cons 'list (macroexpand axes)))))))))

(defmacro boolean-op (operation omega &optional alpha)
  "Converts output of a boolean operation from t/nil to 1/0."
  `(lambda ,(if alpha (list omega alpha)
  		(list omega))
     (if (funcall (function ,operation)
  		  ,@(if alpha (list omega alpha)
  			(list omega)))
  	 1 0)))

(defmacro vex-spec (symbol &rest subspecs)
  "Process the specification for a vector language and build functions that generate the code tree."
  (labels ((process-function-definition (is-dyadic is-scalar function-spec)
	     (let ((discrete-function (if (and (listp (first function-spec))
					       (macro-function (caar function-spec))
					       (not (eql 'lambda (caar function-spec))))
					  (macroexpand (append (first function-spec)
							       (cons 'omega (if is-dyadic (list 'alpha)))))
					  (cons 'function function-spec))))
	       (if (not is-scalar)
		   discrete-function
		   `(lambda ,(if is-dyadic (list 'alpha 'omega)
				 (list 'omega))
		      ,(if is-dyadic `(apply-scalar-function ,discrete-function alpha omega)
			   `(if (arrayp omega)
				(aops:each ,discrete-function omega)
				(funcall ,discrete-function omega)))))))

	   (assign-discrete-functions (entry)
	     ;; return a list containing the function, or both functions if ambivalent
	     (let ((spec (third entry)))
	       (if (eql 'monadic (first spec))
		   (list (process-function-definition nil (eq :scalar (cadadr spec))
						      (last (first (last spec)))))
		   (if (eql 'dyadic (first spec))
		       (list (process-function-definition t (eq :scalar (cadadr spec))
							  (last (first (last spec)))))
		       (list (if (listp (second spec))
				 (process-function-definition nil (or (eq :symmetric-scalar (second spec))
								      (eq :scalar (cadadr spec)))
							      (last (second spec)))
				 (process-function-definition
				  nil t
				  (if (eq :symmetric-scalar (second spec))
				      (last spec)
				      (list (third spec)))))
			     (if (listp (second spec))
				 (process-function-definition t (eq :scalar (second (third spec)))
							      (last (third spec)))
				 (process-function-definition t (or (eq :symmetric-scalar
									(second spec))
								    (eq :asymmetric-scalar
									(second spec)))
							      (last spec))))))))

	   (process-pairs (table-symbol pairs &optional output)
	     (if pairs
		 (process-pairs table-symbol (rest pairs)
				(let* ((glyph-char (character (caar pairs)))
				       (accumulator (third output)))
				  (if (and (eql 'op-specs table-symbol))
				      (setf (getf accumulator (intern (string-upcase (first (third (first pairs))))
								      "KEYWORD"))
					    (cons 'list (cons glyph-char (rest (getf accumulator
										     (intern (string-upcase
											      (first (third (first
													     pairs))))
											     "KEYWORD")))))))
				  (list (cons glyph-char (first output))
					(append (second output)
						(cond ((and (eql 'fn-specs table-symbol)
						      	    (eq :symbolic
								(intern (string-upcase (first (third (first pairs))))
									"KEYWORD")))
						       ;; assign symbolic functions as just keywords in the table
						       `((gethash ,glyph-char ,table-symbol)
						      	 ,(second (third (first pairs)))))
						      ;; assign functions in hash table
						      ((eql 'fn-specs table-symbol)
						       `((gethash ,glyph-char ,table-symbol)
							 (list (lambda (meta axes omega &optional alpha)
								 (declare (ignorable meta axes alpha))
								 ,@(macroexpand (third (first pairs))))
							       ,@(assign-discrete-functions (first pairs)))))
						      ;; assign operators in hash table
						      ((eql 'op-specs table-symbol)
						       `((gethash ,glyph-char ,table-symbol)
						      	 ,(if (eq :macro
								  (intern (string-upcase
									   (first (second (third (first pairs)))))
									  "KEYWORD"))
						      	      (macroexpand (second (second (third (first pairs)))))
						      	      `(lambda (meta axes functions operand
						      			&optional right-operand)
						      		 (declare (ignorable meta axes right-operand))
								 ;;(print (list functions operand))
						      		 `(funcall ,',(second (third (first pairs)))
						      			   ,(cons 'list axes)
						      			   ,(if (listp (first functions))
						      				(cons 'list
						      				      (mapcar (lambda (f)
						      						(if (listp f)
						      						    (cons 'list
						      							  (rest f))
						      						    f))
						      					      functions))
						      			   	(cons 'list (cdar functions)))
						      			   ,operand
						      			   ,@(if right-operand
						      				 (list right-operand)))))))))
					accumulator)))
		 output))

	   (process-tests (specs &optional output)
	     (let* ((tests (rest (assoc (intern "TESTS" (package-name *package*))
					(rest (first specs)))))
		    (props (rest (assoc (intern "HAS" (package-name *package*))
					(rest (first specs)))))
		    (heading (format nil "[~a] ~a~a"
				     (caar specs)
				     (if (getf props :title)
					 (getf props :title)
					 (if (getf props :titles)
					     (first (getf props :titles))))
				     (if (getf props :titles)
					 (concatenate 'string " / " (second (getf props :titles)))
					 ""))))
	       (labels ((for-tests (tests &optional output)
			  (if tests
			      (for-tests (rest tests)
					 (append output (list (cond ((eql 'is (caar tests))
								     `(is (,(intern (string-upcase symbol)
										    (package-name *package*))
									    ,(cadar tests))
									  ,(third (first tests))))))))
			      output)))
		 
		 (if specs
		     (process-tests (rest specs)
				    (if (assoc (intern "TESTS" (package-name *package*))
					       (rest (first specs)))
					(append output (list `(princ ,heading))
						(for-tests tests)
						(list `(princ (format nil "~%~%")) nil))
					output))
		     output)))))
    (let* ((function-specs (process-pairs 'fn-specs (rest (assoc (intern "FUNCTIONS" (package-name *package*))
								 subspecs))))
	   (operator-specs (process-pairs 'op-specs (rest (assoc (intern "OPERATORS" (package-name *package*))
								 subspecs))))
	   (function-tests (process-tests (rest (assoc (intern "FUNCTIONS" (package-name *package*))
						       subspecs))))
	   (operator-tests (process-tests (rest (assoc (intern "OPERATORS" (package-name *package*))
						       subspecs)))))
      `(progn (let ((fn-specs (make-hash-table))
		    (op-specs (make-hash-table)))
		(setf ,@(second function-specs)
		      ,@(second operator-specs))
		(setq *vex-idiom* (make-instance 'idiom
						 :name ,(intern (string-upcase symbol) "KEYWORD")
						 :environment ,(cons 'list
								     (rest (assoc (intern "ENVIRONMENT"
											  (package-name *package*))
										  subspecs)))
						 :operational-glyphs (list ,@(derive-opglyphs
									      (append (first function-specs)
										      (first operator-specs))))
						 :functions fn-specs
						 :operators op-specs
						 :overloaded-lexicon (list ,@(intersection (first function-specs)
											   (first operator-specs)))
						 :operator-index (list ,@(third operator-specs))))

		(defmacro ,(intern (string-upcase symbol)
				   (package-name *package*))
		    (options &optional input-string)
		  (if (eq :test options)
		      (cons 'progn ',(append operator-tests function-tests))
		      (vex-program *vex-idiom*
				   (if (or input-string (and options (listp options)))
				       options)
				   (if input-string input-string options)))))))))
  
(defun derive-opglyphs (glyph-list &optional output)
  (if (not glyph-list)
      output (derive-opglyphs (rest glyph-list)
			      (let ((glyph (first glyph-list)))
				(if (characterp glyph)
				    (cons glyph output)
				    (if (stringp glyph)
					(append output (loop for char from 0 to (1- (length glyph))
							  collect (aref glyph char)))))))))

(defun numeric-string-p (string)
  (handler-case (progn (parse-apl-number-string string) t)
    (condition () nil)))

(defun parse-apl-number-string (number-string &optional imaginary-component)
  (let ((nstring (string-upcase number-string)))
    (if (and (not imaginary-component)
	     (find #\J nstring))
	(let ((halves (cl-ppcre:split "J" nstring)))
	  (if (and (= 2 (length halves))
		   (< 0 (length (first halves)))
		   (< 0 (length (second halves))))
	      (complex (parse-apl-number-string (first halves) t)
		       (parse-apl-number-string (second halves) t))))
	;; either the macron or combining_macron character may be used as the high minus sign
	(parse-number:parse-number (regex-replace-all "[¯̄]" nstring "-")))))

(defun format-array (values)
  (if (or (stringp (first values))
	  (symbolp (first values))
	  (and (not (second values))
	       (or (listp (first values))
		   (functionp (first values)))))
      ;; if the first item is a list (i.e. code to generate an array of some kind),
      ;; pass it through with no changes. Also pass through strings, which are already arrays,
      ;; any symbols
      (first values)
      `(make-array (list ,(length values))
		   :initial-contents (list ,@values))))

(defun format-function (idiom content)
  (let ((⍺ (intern "⍺" (string-upcase (idiom-name idiom))))
	(⍵ (intern "⍵" (string-upcase (idiom-name idiom)))))
    `(lambda (,⍺ &optional ,⍵)
       ;; kludge to handle reversing the variable order
       ;; the latter variable is always the optional one in Lisp, but in APL
       ;; the ⍺ is the optional argument
       (if ,⍵ (funcall (lambda (,⍺ ,⍵) ,content) ,⍵ ,⍺)
	     (funcall (lambda (,⍵) ,content) ,⍺)))))

(defun format-value (element)
  (cond ((and (vectorp element)
	      (string= element "⍬")) ;; APL's "zilde" character translates to an empty vector
 	 (make-array (list 0)))
	((numeric-string-p element)
	 (parse-apl-number-string element))
	((or (and (char= #\" (aref element 0))
		  (char= #\" (aref element (1- (length element)))))
	     (and (char= #\' (aref element 0))
		  (char= #\' (aref element (1- (length element))))))
	 (subseq element 1 (1- (length element))))
	((stringp element)
	 (intern (string-upcase (symbol-munger:camel-case->lisp-name element))))
	(t element)))

(defun is-singleton (value)
  (let ((adims (dims value)))
    (and (= 1 (first adims))
	 (= 1 (length adims)))))

(defun scale-array (singleton to-match)
  (make-array (dims to-match)
	      :initial-element (aref singleton 0)))

(defun apply-scalar-function (function alpha omega)
  (let* ((alpha-scalar? (not (arrayp alpha)))
	 (omega-scalar? (not (arrayp omega)))
	 (alpha-unitary? (and (not alpha-scalar?)
			      (vectorp alpha)
			      (= 1 (length alpha))))
	 (omega-unitary? (and (not omega-scalar?)
			      (vectorp omega)
			      (= 1 (length omega)))))
    (cond ((and alpha-scalar? omega-scalar?)
	   (funcall function alpha omega))
	  ((and alpha-unitary? omega-unitary?)
	   (aops:each (lambda (alpha omega) (apply-scalar-function function alpha omega))
		      alpha omega))
	  ((and (not alpha-unitary?)
		(not omega-unitary?)
		(not alpha-scalar?)
		(not omega-scalar?))
	   (if (loop for dimension in (funcall (lambda (a o) (mapcar #'= a o))
					       (dims alpha)
					       (dims omega))
		  always dimension)
	       (aops:each (lambda (alpha omega) (apply-scalar-function function alpha omega))
			  alpha omega)
	       (error "Array size mismatch.")))
	  (t (labels ((scan-over (element)
			(if (arrayp element)
			    (aops:each #'scan-over element)
			    (apply (lambda (left right) (apply-scalar-function function left right))
				   (cond (alpha-scalar? (list alpha element))
					 (alpha-unitary? (list (aref alpha 0)
							       element))
					 (omega-scalar? (list element omega))
					 (omega-unitary? (list element (aref omega 0))))))))
	       (aops:each #'scan-over (if (or alpha-scalar? alpha-unitary?)
					  omega alpha)))))))

(defun process-reverse (function input &optional output)
  (if input
      (process-reverse function (rest input)
		       (cons (funcall function (first input))
			     output))
      output))

(defun =vex-operation (idiom meta at-start?)
  "Parse an operation belonging to a Vex expression, returning the operation string and tokens extracted along with the remainder of the expression string."
  (labels ((?blank-character ()
	     (?satisfies (lambda (c) (member c (list #\  #\tab)))))
	   (?token-character ()
	     (%or (?satisfies 'alphanumericp)
		  ;; the ¯ character must be expressed as #\macron to be correctly processed
		  ;; the ̄ (combining_macron) character can be denoted normally, however
		  (?satisfies (lambda (c) (member c (list #\macron #\̄ #\. #\⍺ #\⍵ #\⍬))))))
	   (=string (&rest delimiters)
	     (let ((lastc nil)
		   (delimiter nil))
	       (=destructure (_ content)
		   (=list (?satisfies (lambda (c) (if (member c delimiters)
						      (setq delimiter c))))
			  ;; note: nested quotes must be checked backwards; to determine whether a delimiter
			  ;; indicates the end of the quote, look at previous character to see whether it is a
			  ;; delimiter, then check whether the current character is an escape character #\\
			  (=subseq (%any (?satisfies (lambda (c) (if (or (not lastc)
									 (not (char= lastc delimiter))
									 (char= c #\\))
								     (setq lastc c)))))))
		 (format nil "~a~a" delimiter content))))
	   (=vex-opglyphs ()
	     (let ((ops nil))
	       (flet ((glyph-finder (glyph)
			(cond ((and (not (getf ops :op))
				    (gethash glyph (idiom-operators idiom)))
			       (if (and (not ops)
					(not (getf ops :fn))
					(member glyph (getf (idiom-opindex idiom) :right)))
				   (setf (getf ops :op) glyph)
				   (if (and (getf ops :fn)
					    (member glyph (getf (idiom-opindex idiom) :center)))
				       (setf (getf ops :op) glyph))))
			      ((gethash glyph (idiom-functions idiom))
			       (if (not (getf ops :fn))
				   (setf (getf ops :fn) glyph)
				   (if (and (getf ops :op)
					    (not (getf ops :afn)))
				       (setf (getf ops :afn) glyph))))
			      (t nil))))
		 (=destructure (_ glyph-group)
		     (=list (%any (?blank-character))
			    (=subseq (%any (?satisfies #'glyph-finder))))
		   (declare (ignore glyph-group))
		   ;; if only an operator was found, check whether the glyph is a member of the
		   ;; overloaded lexicon. If so, it will be reassigned as a function glyph, if not an
		   ;; error will occur
		   (if (and (getf ops :op)
			    (not (getf ops :fn)))
		       (if (member (getf ops :op)
				   (idiom-overloaded-lexicon idiom))
			   (setf (getf ops :fn) (getf ops :op)
				 (getf ops :op) nil)))
		   ops))))
	   (=vex-tokens (&optional first-token?) ;; recursive parser for tokens and closures
	     (=destructure (_ axis _ token _ last)
		 (=list (%any (?blank-character))
			(%maybe (=vex-closure "[]" #'handle-axes))
			(%any (?blank-character))
			(if (and first-token? at-start?)
			    ;; only process a function as a value if it's the first token in the expression
			    (%or (=vex-closure "{}" #'handle-function-as-data)
				 (=subseq (%some (?token-character)))
				 (=string #\' #\")
				 (=vex-closure "()"))
			    (%or (=subseq (%some (?token-character)))
				 (=string #\' #\") (=vex-closure "()")))
			(%any (?blank-character))
			(=subseq (%any (?satisfies 'characterp))))
	       (let ((token (if (not axis)
				token (list :axis axis token)))
		     (next (parse last (=vex-tokens))))
		 (if (or (not (stringp token))
			 (not (member (format-value (reverse token))
				      (gethash :functions meta))))
		     (if next (list (cons token (first next))
				    (second next))
			 (list (list token) last))))))
	   (=vex-closure (boundary-chars &optional transform-by)
	     (let ((balance 1))
	       (=destructure (_ enclosed _)
		   (=list (?eq (aref boundary-chars 1))
			  (=transform (=subseq (%some (?satisfies (lambda (char)
								    (if (char= char (aref boundary-chars 1))
									(incf balance 1))
								    (if (char= char (aref boundary-chars 0))
									(incf balance -1))
								    (< 0 balance)))))
				      (if transform-by transform-by
					  (lambda (string-content)
					    (vex-expression idiom meta string-content))))
			  (?eq (aref boundary-chars 0)))
		 enclosed)))
	   (=vex-axes () ;; handle axes, separated by semicolons
	     (=destructure (element _ next last)
		 (=list (=subseq (%some (?satisfies (lambda (c) (not (char= c #\;))))))
			(?eq #\;)
			(%maybe '=vex-axes-parser)
			(=subseq (%any (?satisfies (lambda (c) (not (char= c #\;)))))))
	       (if next (cons element next)
		   (list element last))))
	   (handle-axes (input-string)
	     (let ((axes (parse input-string (=vex-axes))))
	       ;; reverse the order of axes, since we're parsing backwards
	       (process-reverse (lambda (string-content) (vex-expression idiom meta string-content))
				(if axes axes (list input-string)))))
	   (handle-function (input-string)
	     (let ((formatted-function (format-function idiom (vex-expression idiom meta input-string))))
	       (lambda (meta axes omega &optional alpha)
		 (declare (ignorable meta axes))
		 `(funcall ,formatted-function
			   ,@(if alpha (list (macroexpand alpha)))
			   ,(macroexpand omega)))))
	   (handle-function-as-data (input-string)
	     (format-function idiom (vex-expression idiom meta input-string))))
    
    (setf (fdefinition '=vex-tokens-parser) (=vex-tokens)
	  (fdefinition '=vex-axes-parser) (=vex-axes))
    
    (=destructure (_ hd tl last)
	(if at-start? ;; handle the initial value in the expression at the start
	    (=list (%any (?blank-character))
		   (%maybe (=vex-tokens t))
		   (=subseq (%any (?satisfies 'characterp)))
		   (=subseq (%any (?satisfies 'characterp))))
	    ;; then handle each operation (function and sometimes operator) and the following tokens
	    (=list (%any (?blank-character))
		   (=list (%maybe (=vex-closure "[]" #'handle-axes))
		   	  (%or (=vex-closure "{}" #'handle-function)
			       (=subseq (%some (?token-character)))
			       (=vex-opglyphs)))
		   (%maybe (=vex-tokens))
		   (=subseq (%any (?satisfies 'characterp)))))
      (if (and (not at-start?)
      	       (stringp (second hd)))
      	  (let ((function-string (second hd)))
	    (setq hd (list (first hd)
			   (function (lambda (meta axes omega &optional alpha)
			     (declare (ignorable meta axes))
			     (if alpha
				 `(aref (apply-scalar-function ,(format-value (reverse function-string))
							       ,(macroexpand alpha)
							       ,(macroexpand omega))
					0)
				 `(aref (aops:each ,(format-value (reverse function-string))
						   ,(macroexpand omega))
					0))))))))
      ;; if the head of the operation is a string (thus a symbol-referenced function),
      ;; format it and compose the operation spec accordingly
      (if at-start? (setq tl hd hd nil))
      (list hd (if tl
		   (list (process-reverse (lambda (value) (cond ((stringp value)
								 (format-value (reverse value)))
								((and (listp value)
								      (eq :axis (first value)))
								 `(apply #'aref
									 (cons ,(if (stringp (third value))
										    (format-value
										     (reverse (third value)))
										    (third value))
									       (mapcar (lambda (i) (1- (aref i 0)))
										       (list ,@(second value))))))
								(t value)))
					  (first tl))
			 (second tl))
		   (list nil last))))))

(defun vex-expression (idiom meta string &optional precedent)
  "Convert an expression into Lisp code, tranforming the text through the parser and invoking the corresponding spec-defined functions accordingly."
  (if (= 0 (length string))
      precedent
      (let* ((next-operation (parse string (=vex-operation idiom meta (not precedent))))
	     (operation (cadar next-operation))
	     (operation-axes (caar next-operation))
	     (value-results (second next-operation))
	     (operator (if (not (or (functionp operation)
				    (eql 'lambda (first operation))))
			   (gethash (getf operation :op)
				    (idiom-operators idiom))))
	     (function (if (or (functionp operation)
			       (eql 'lambda (first operation)))
			   (list operation)
			   (gethash (getf operation :fn)
				    (idiom-functions idiom))))
	     (alpha-function (if (not (or (functionp operation)
					  (eql 'lambda (first operation))))
				 (gethash (getf operation :afn)
					  (idiom-functions idiom)))))
	(vex-expression idiom meta (second value-results)
			(apply (cond (operator operator)
				     (function (first function))
				     (t (lambda (&rest items) (third items))))
			       (append (list meta operation-axes)
				       (if operator
					   (list (cons function (if alpha-function (list alpha-function)))))
				       (if (first value-results)
					   (list (format-array (first value-results))))
				       (if precedent (list precedent))))))))

(defun vex-program (idiom options &optional string meta)
  (let ((meta (if meta meta (make-hash-table :test #'eq))))
    (labels ((assign-from (source dest)
	       (if source
		   (progn (setf (getf dest (first source))
				(second source))
			  (assign-from (cddr source)
				       dest))
		   dest)))
      (setf (gethash :functions meta)
	    nil)
      (if (getf options :env)
	  (setf (idiom-environment idiom)
		(assign-from (getf options :env)
			     (idiom-environment idiom))))
      (if string
	  `(progn
	     ,@(loop for exp in (cl-ppcre:split "[◊\\r\\n]\\s{0,}" ;; whitespace after diamonds is removed
						(regex-replace-all (concatenate 'string "^\\s{0,}⍝(.*)[\\r\\n]"
										"|(?<=[\\r\\n])\\s{0,}⍝(.*)[\\r\\n]"
										"|(?<=[^\\r\\n])\\s{0,}⍝(.*)(?=[\\r\\n])")
								   ;; remove comments
								   string ""))
		  collect (vex-expression idiom meta (reverse exp))))))))
