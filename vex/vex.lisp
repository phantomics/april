;;;; vex.lisp

(in-package #:vex)

(defmacro local-idiom (symbol)
  (intern (format nil "*~a-IDIOM*" (string-upcase symbol))))

;; The idiom object defines a vector language instance with a persistent state.
(defclass idiom ()
  ((name :accessor idiom-name
    	 :initarg :name)
   (state :accessor idiom-state
	  :initarg :state)
   (base-state :accessor idiom-base-state
	       :initarg :state)
   (default-state :accessor idiom-default-state
                  :initarg :state)
   (utilities :accessor idiom-utilities
	      :initarg :utilities)
   (utilities-spec :accessor idiom-utilities-spec
		   :initarg :utilities-spec)
   (functions :accessor idiom-functions
	      :initform nil
	      :initarg :functions)
   (functions-spec :accessor idiom-functions-spec
		   :initform nil
		   :initarg :functions-spec)
   (operators :accessor idiom-operators
	      :initform nil
	      :initarg :operators)
   (operators-spec :accessor idiom-operators-spec
		   :initform nil
		   :initarg :operators-spec)
   (operational-glyphs :accessor idiom-opglyphs
		       :initform nil
		       :initarg :operational-glyphs)
   (operator-index :accessor idiom-opindex
		   :initform nil
		   :initarg :operator-index)
   (overloaded-lexicon :accessor idiom-overloaded-lexicon
		       :initform nil
		       :initarg :overloaded-lexicon)))

(defgeneric of-state (idiom property))
(defmethod of-state ((idiom idiom) property)
  "Retrieve a property of the idiom state."
  (getf (idiom-state idiom) property))

(defgeneric of-utilities (idiom utility))
(defmethod of-utilities ((idiom idiom) utility)
  "Retrieve one of the idiom's utilities used for parsing and language processing."
  (getf (idiom-utilities idiom) utility))

(defgeneric of-functions (idiom key))
(defmethod of-functions ((idiom idiom) key)
  "Retrive one of the idiom's functions."
  (gethash key (idiom-functions idiom)))

(defgeneric of-operators (idiom key))
(defmethod of-operators ((idiom idiom) key)
  "Retrive one of the idiom's operators."
  (gethash key (idiom-operators idiom)))

(defgeneric of-overloaded? (idiom key))
(defmethod of-overloaded? ((idiom idiom) key)
  "Check whether the argument is part of the idiom's overloaded lexicon (glyphs that may be functions or operators)."
  (member key (idiom-overloaded-lexicon idiom)))

(defmethod make-load-form ((idiom idiom) &optional environment)
  (declare (ignore environment))
  ;; Note that this definition only works because X and Y do not
  ;; contain information which refers back to the object itself.
  ;; For a more general solution to this problem, see revised example below.
  `(make-instance ',(class-of idiom)
		  :name ',(idiom-name idiom)
		  :state ',(idiom-state idiom)
		  :utilities ,(cons 'list (idiom-utilities-spec idiom))
		  :functions ,(idiom-functions-spec idiom)
		  :operators ,(idiom-operators-spec idiom)
		  :operational-glyphs ',(idiom-opglyphs idiom)
		  :operator-index ',(idiom-opindex idiom)
		  :overloaded-lexicon ',(idiom-overloaded-lexicon idiom)))

(defmacro boolean-op (operation omega &optional alpha)
  "Converts output of a boolean operation from t/nil to 1/0."
  `(lambda ,(if alpha (list omega alpha)
  		(list omega))
     (if (funcall (function ,operation)
  		  ,@(if alpha (list omega alpha)
  			(list omega)))
  	 1 0)))

(defmacro reverse-op (operation omega &optional alpha)
  (declare (ignore omega))
  (if alpha `(lambda (omega alpha) (funcall (function ,operation)
					    alpha omega))
      `(function, operation)))

(defmacro reverse-boolean-op (operation omega &optional alpha)
  "Converts output of a boolean operation from t/nil to 1/0."
  `(lambda ,(if alpha (list omega alpha)
  		(list omega))
     (if (funcall (function ,operation)
  		  ,@(if alpha (list alpha omega)
  			(list omega)))
  	 1 0)))

(defmacro vex-spec (symbol &rest subspecs)
  "Process the specification for a vector language and build functions that generate the code tree."
  (let ((idiom-symbol (intern (format nil "*~a-IDIOM*" (string-upcase symbol))
			      (package-name *package*))))
    (labels ((process-pairs (table-symbol pairs &optional output)
	       (if pairs
		   (process-pairs table-symbol (rest pairs)
				  (let* ((glyph-char (character (caar pairs)))
					 (accumulator (third output))
					 ;; name of macro to process operation specs
					 (oprocess (getf (rest (assoc (intern "UTILITIES" (package-name *package*))
								      subspecs))
							 :mediate-operation-macro)))
				    (if (and (eql 'op-specs table-symbol))
					(setf (getf accumulator
						    (intern (string-upcase (first (third (first pairs))))
							    "KEYWORD"))
					      (cons 'list (cons glyph-char (rest (getf accumulator
										       (intern (string-upcase
												(first
												 (third (first
													 pairs))))
											       "KEYWORD")))))))
				    (list (cons glyph-char (first output))
					  (append (second output)
						  (cond ((and (eql 'fn-specs table-symbol)
							      (eq :symbolic
								  (intern (string-upcase
									   (first (third (first pairs))))
									  "KEYWORD")))
							 ;; assign symbolic functions as just keywords in the table
							 `((gethash ,glyph-char ,table-symbol)
							   ,(second (third (first pairs)))))
							;; assign functions in hash table
							((eql 'fn-specs table-symbol)
							 `((gethash ,glyph-char ,table-symbol)
							   ,(if (and (listp (second (third (first pairs))))
								     (eq :macro
									 (intern (string-upcase
										  (first (second (third
												  (first pairs)))))
										 "KEYWORD")))
								(macroexpand
								 (second (second (third (first pairs)))))
								(macroexpand
								 (cons (second oprocess)
								       (list (third (first pairs))))))))
							;; assign operators in hash table
							((eql 'op-specs table-symbol)
							 `((gethash ,glyph-char ,table-symbol)
							   ,(macroexpand (second (third (first pairs))))))))
					  accumulator)))
		   output))

	     (process-optests (specs &optional output)
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
									    ,(third (first tests))
									    :test #'equalp))))))
				output)))
		   
		   (if specs
		       (process-optests (rest specs)
					(if (assoc (intern "TESTS" (package-name *package*))
						   (rest (first specs)))
					    (append output (list `(princ ,heading))
						    (for-tests tests)
						    (list `(princ (format nil "~%~%"))))
					    output))
		       output))))

	     (process-gentests (specs &optional output)
	       (if specs
		   (let ((this-spec (cdar specs)))
		     (process-gentests (rest specs)
				       (append output `((princ ,(getf this-spec :title))
							(is (,(intern (string-upcase symbol)
								      (package-name *package*))
							      ,@(getf this-spec :in))
							    ,(getf this-spec :ex)
							    :test #'equalp)))))
		   output)))
      (let* ((function-specs (process-pairs 'fn-specs (rest (assoc (intern "FUNCTIONS" (package-name *package*))
								   subspecs))))
	     (operator-specs (process-pairs 'op-specs (rest (assoc (intern "OPERATORS" (package-name *package*))
								   subspecs))))
	     (function-tests (process-optests (rest (assoc (intern "FUNCTIONS" (package-name *package*))
							   subspecs))))
	     (operator-tests (process-optests (rest (assoc (intern "OPERATORS" (package-name *package*))
							   subspecs))))
	     (general-tests (process-gentests (rest (assoc (intern "GENERAL-TESTS" (package-name *package*))
							   subspecs))))
	     (idiom-definition `(make-instance 'idiom
					       :name ,(intern (string-upcase symbol) "KEYWORD")
					       :state
					       ,(cons 'list (rest (assoc (intern "STATE" (package-name *package*))
									 subspecs)))
					       :utilities
					       ,(cons 'list
						      (rest (assoc (intern "UTILITIES" (package-name *package*))
								   subspecs)))
					       :utilities-spec
					       (quote ,(rest (assoc (intern "UTILITIES"
									    (package-name *package*))
								    subspecs)))
					       :functions (let ((fn-specs (make-hash-table)))
							     (setf ,@(second function-specs))
							     fn-specs)
					       :functions-spec `(let ((fn-specs (make-hash-table)))
								  (setf ,@',(second function-specs))
								  fn-specs)
					       :operators (let ((op-specs (make-hash-table)))
							    (setf ,@(second operator-specs))
							    op-specs)
					       :operators-spec `(let ((op-specs (make-hash-table)))
								  (setf ,@',(second operator-specs))
								  op-specs)
					       :operational-glyphs (list ,@(derive-opglyphs
									    (append (first function-specs)
										    (first operator-specs))))
					       :overloaded-lexicon (list ,@(intersection (first function-specs)
											 (first operator-specs)))
					       :operator-index (list ,@(third operator-specs)))))
	`(progn (defvar ,idiom-symbol)
		(setf ,idiom-symbol ,idiom-definition)
		(defmacro ,(intern (string-upcase symbol)
				   (package-name *package*))
		    (options &optional input-string)
		  ;; this macro is the point of contact between users and the language, used to
		  ;; evaluate expressions and control properties of the language instance
		  `(progn (defvar ,(intern ,(format nil "*~a-IDIOM*" (string-upcase symbol))))
			  ,(if (not (boundp (intern ,(format nil "*~a-IDIOM*" (string-upcase symbol)))))
			       `(setq ,(intern ,(format nil "*~a-IDIOM*" (string-upcase symbol)))
				      ,',idiom-definition))
			  ,(cond ((and options (listp options)
				       (string= "TEST" (string (first options))))
				  (let ((all-tests ',(append function-tests operator-tests general-tests)))
				    `(progn
				       (setq prove:*enable-colors* nil)
				       (plan ,(loop for exp in all-tests counting (eql 'is (first exp))))
				       ,@all-tests (finalize)
				       (setq prove:*enable-colors* t))))
				 ;; the (test) setting is used to run tests
				 ((and options (listp options)
				       (string= "RESTORE-DEFAULTS" (string (first options))))
				  `(setf (idiom-state ,,idiom-symbol)
					 (copy-alist (idiom-base-state ,,idiom-symbol))))
				 ;; the (restore-defaults) setting is used to restore the workspace settings
				 ;; to the defaults from the spec
				 (t `(progn ,@(if (and (listp options)
						       (string= "SET" (string (first options)))
						       (assoc :space (rest options))
						       (not (boundp (second (assoc :space (rest options))))))
						  `((defvar ,(second (assoc :space (rest options)))
						      (make-hash-table :test #'eq))))
					    (eval (vex-program ,(intern ,(format nil "*~a-IDIOM*"
					    					 (string-upcase symbol)))
					    		       (quote
					    			,(if input-string
					    			     (if (string= "SET" (string (first options)))
					    				 (rest options)
					    				 (error "Incorrect option syntax."))))
					    		       ,(if input-string input-string
					    			    options)))
					    ;; ,(vex-program (eval (intern ,(format nil "*~a-IDIOM*"
					    ;; 					 (string-upcase symbol))))
					    ;; 		  (if input-string
					    ;; 		      (if (string= "SET" (string (first options)))
					    ;; 			  (rest options)
					    ;; 			  (error "Incorrect option syntax.")))
					    ;; 		  (if input-string input-string options))
					    ))))))))))
  
(defun derive-opglyphs (glyph-list &optional output)
  "Extract a list of function/operator glyphs from part of a Vex language specification."
  (if (not glyph-list)
      output (derive-opglyphs (rest glyph-list)
			      (let ((glyph (first glyph-list)))
				(if (characterp glyph)
				    (cons glyph output)
				    (if (stringp glyph)
					(append output (loop for char from 0 to (1- (length glyph))
							  collect (aref glyph char)))))))))

(defun =vex-string (idiom meta &optional output)
  "Parse a string of text, converting its contents into nested lists of Vex tokens."
  (labels ((?blank-character () (?satisfies (of-utilities idiom :match-blank-character)))
	   (?token-character () (?satisfies (of-utilities idiom :match-token-character)))

	   (=string (&rest delimiters)
	     (let ((lastc nil)
		   (delimiter nil))
	       (=destructure (_ content _)
		   (=list (?satisfies (lambda (c) (if (member c delimiters)
						      (setq delimiter c))))
			  ;; note: nested quotes must be checked backwards; to determine whether a delimiter
			  ;; indicates the end of the quote, look at previous character to see whether it is a
			  ;; delimiter, then check whether the current character is an escape character #\\
			  (=subseq (%any (?satisfies (lambda (char)
						       (if (or (not lastc)
							       (not (char= char delimiter))
							       (char= lastc #\\))
							   (setq lastc char))))))
			  (?satisfies (lambda (c) (char= c delimiter))))
		 content)))

	   (=vex-closure (boundary-chars &optional transform-by)
	     (let ((balance 1)
		   (char-index 0))
	       (=destructure (_ enclosed _)
		   (=list (?eq (aref boundary-chars 0))
			  ;; for some reason, the first character in the string is iterated over twice here,
			  ;; so the character index is checked and nothing is done for the first character
			  ;; TODO: fix this
			  (=transform (=subseq (%some (?satisfies (lambda (char)
								    (if (and (char= char (aref boundary-chars 0))
									     (< 0 char-index))
									(incf balance 1))
								    (if (and (char= char (aref boundary-chars 1))
									     (< 0 char-index))
									(incf balance -1))
								    (incf char-index 1)
								    (< 0 balance)))))
				      (if transform-by transform-by
					  (lambda (string-content)
					    (parse string-content (=vex-string idiom meta)))))
			  (?eq (aref boundary-chars 1)))
		 enclosed)))
	   
	   (handle-axes (input-string)
	     (let ((each-axis (funcall (of-utilities idiom :process-axis-string)
				       input-string)))
	       (cons :axes (mapcar (lambda (string) (first (parse string (=vex-string idiom meta))))
				   each-axis))))

	   (handle-function (input-string)
	     (list :fn (funcall (of-utilities idiom :format-function)
				(string-upcase (idiom-name idiom))
				(vex-program idiom nil input-string meta t)))))

    (=destructure (_ item _ rest)
	(=list (%any (?blank-character))
	       (%or (=vex-closure "()")
		    (=vex-closure "[]" #'handle-axes)
		    (=vex-closure "{}" #'handle-function)
		    (=string #\' #\")
		    (=transform (=subseq (%some (?satisfies (let ((ix 0))
							      (lambda (char)
								(and (not (< 2 (incf ix 1)))
								     (member char (idiom-opglyphs idiom))))))))
				(lambda (string)
				  (let ((char (character string)))
				    `(,(cond ((gethash char (idiom-operators idiom))
					      :op)
					     ((gethash char (idiom-functions idiom))
					      :fn))
				       ,@(if (gethash char (idiom-operators idiom))
					     (list (let ((result nil))
						     (loop for tix from 0 to (/ (length (idiom-opindex idiom))
										2)
							when (member char (nth (1+ (* 2 tix))
									       (idiom-opindex idiom)))
							do (setq result (nth (* 2 tix)
									     (idiom-opindex idiom))))
						     result)))
				       ,char))))
		    (=transform (=subseq (%some (?token-character)))
				(lambda (string)
				  (funcall (of-utilities idiom :format-value)
					   (string-upcase (idiom-name idiom))
					   meta string))))
	       (%any (?blank-character))
	       (=subseq (%any (?satisfies 'characterp))))
      (if (< 0 (length rest))
	  (parse rest (=vex-string idiom meta (if output (cons (cons item (first output))
							       (rest output))
						  (list (list item)))))
	  (cons (cons item (first output))
		(rest output))))))

(defun =vex-lines (idiom meta)
  (labels ((?blank-character () (?satisfies (of-utilities idiom :match-blank-character)))
	   (?newline-character () (?satisfies (of-utilities idiom :match-newline-character)))
	   (?but-newline-character ()
	     (?satisfies (lambda (char) (not (funcall (of-utilities idiom :match-newline-character)
						      char))))))
    (=destructure (_ content _ nextlines)
	(=list (%any (?blank-character))
	       (=subseq (%any (?but-newline-character)))
	       (%any (?newline-character))
	       (=subseq (%any (?satisfies 'characterp))))
      (list (parse content (=vex-string idiom meta))
	    nextlines))))

(defun expression (idiom meta exp &optional precedent)
  "Convert a list of Vex tokens into Lisp code, composing objects and invoking the corresponding spec-defined functions accordingly."
  (if (not exp)
      precedent
      (if (not precedent)
	  (multiple-value-bind (right-value from-value)
	      (funcall (of-utilities idiom :assemble-value)
		       idiom meta #'expression precedent exp)
	    (expression idiom meta from-value right-value))
	  (multiple-value-bind (operation from-operation)
	      (funcall (of-utilities idiom :assemble-operation)
		       idiom meta #'expression precedent exp)
	    (multiple-value-bind (right-value from-value)
		(funcall (of-utilities idiom :assemble-value)
			 idiom meta #'expression precedent from-operation nil nil exp)
	      (expression idiom meta from-value
			  (apply operation (append (list meta nil precedent)
						   (if right-value (list right-value))))))))))

(defun vex-program (idiom options &optional string meta internal)
  "Compile a set of expressions, optionally drawing external variables into the program and setting configuration parameters for the system."
  (let* ((state (rest (assoc :state options)))
	 (meta (if meta meta (if (assoc :space options)
				 (symbol-value (second (assoc :space options)))
				 (make-hash-table :test #'eq))))
	 (state-persistent (rest (assoc :state-persistent options)))
	 (state-to-use nil)
	 (preexisting-vars nil))
    (labels ((assign-from (source dest)
	       (if source
		   (progn (setf (getf dest (first source))
				(second source))
			  (assign-from (cddr source)
				       dest))
		   dest))
	     (process-lines (lines &optional output)
	       (if (= 0 (length lines))
		   output
		   (destructuring-bind (out remaining)
		       (parse lines (=vex-lines idiom meta))
		     (process-lines remaining
				    (append output (list (expression idiom meta (list out)))))))))

      (if (not (gethash :variables meta))
	  (setf (gethash :variables meta) (make-hash-table :test #'eq))
	  (setq preexisting-vars (loop for vk being the hash-values of (gethash :variables meta)
				    collect vk)))

      (if (not (gethash :values meta))
	  (setf (gethash :values meta) (make-hash-table :test #'eq)))

      (if (not (gethash :functions meta))
	  (setf (gethash :functions meta) (make-hash-table :test #'eq)))

      (setf state-to-use
	    (assign-from state (assign-from state-persistent (assign-from (gethash :state meta)
									  (idiom-base-state idiom)))))

      ;; (if state (setf (idiom-state idiom)
      ;; 		      (assign-from state (copy-alist (idiom-base-state idiom)))))

      (if state-persistent (setf (idiom-state idiom)
      				 (assign-from state-persistent (idiom-base-state idiom))
				 (gethash :state meta)
				 (assign-from state-persistent (gethash :state meta))))
      
      (if string
	  (let* ((input-vars (getf state-to-use :in))
		 (output-vars (getf state-to-use :out))
		 (compiled-expressions (process-lines (funcall (of-utilities idiom :prep-code-string)
							       string)))
		 (var-symbols (loop for key being the hash-keys of (gethash :variables meta)
				 when (not (member (string (gethash key (gethash :variables meta)))
						   (mapcar #'first input-vars)))
				 collect (list key (gethash key (gethash :variables meta)))))
		 (vars-declared (loop for key-symbol in var-symbols
				   when (not (member (string (gethash (first key-symbol)
								      (gethash :variables meta)))
						     (mapcar #'first input-vars)))
				   collect (let* ((key (first key-symbol))
						  (sym (second key-symbol))
						  (fun-ref (gethash sym (gethash :functions meta)))
						  (val-ref (gethash sym (gethash :values meta))))
					     (list sym (if (member sym preexisting-vars)
							   (if val-ref val-ref (if fun-ref fun-ref))
							   :undefined))))))
	    (if input-vars
		(loop for var-entry in input-vars
		   ;; TODO: move these APL-specific checks into spec
		   do (if (gethash (intern (lisp->camel-case (first var-entry))
					   "KEYWORD")
				   (gethash :variables meta))
			  (rplacd (assoc (gethash (intern (lisp->camel-case (first var-entry))
							  "KEYWORD")
						  (gethash :variables meta))
					 vars-declared)
				  (list (second var-entry)))
			  (setq vars-declared (append vars-declared
						      (list (list (setf (gethash (intern (lisp->camel-case
											  (first var-entry))
											 "KEYWORD")
										 (gethash :variables meta))
									(gensym))
								  (second var-entry))))))))
	    (let ((code `(,@(if (and vars-declared (not internal))
				`(let* ,vars-declared
				   (declare (ignorable ,@(mapcar #'second var-symbols))))
				'(progn))
			    ,@(funcall (if output-vars #'values (of-utilities idiom :postprocess-compiled))
				       compiled-expressions)
			    ,@(if output-vars
				  (list (cons 'values
					      (mapcar (lambda (return-var)
							(funcall (of-utilities idiom :postprocess-value)
								 (gethash (intern (lisp->camel-case return-var)
										  "KEYWORD")
									  (gethash :variables meta))))
						      output-vars)))))))
	      (if (assoc :compile-only options)
		  `(quote ,code)
		  code)))))))
