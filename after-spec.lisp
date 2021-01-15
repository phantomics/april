;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; after-spec.lisp

(in-package #:april)

(defun value-assignment-by-function-result
    (tokens space process &optional precedent properties preceding-properties)
  (declare (ignorable precedent properties preceding-properties))
  (let ((asop) (asop-props) (fn-element) (fnel-specs) (function-axes) (symbol) (symbol-props) (symbol-axes)
	(items tokens) (item (first tokens)) (rest-items (rest tokens))
	(preceding-type (getf (first preceding-properties) :type))
	(preceding-special-props (getf (first preceding-properties) :special)))
    (if (not (member :value-assignment-by-function-result (getf preceding-special-props :omit)))
	(assign-element asop asop-props process-function process '(:glyph ←) space item items rest-items))
    (if asop (assign-axes function-axes process item items rest-items))
    (if asop (assign-subprocessed fn-element fnel-specs process
				  (list :special '(:omit (:value-assignment :function-assignment)))
				  item items rest-items))
    (if fn-element (assign-axes symbol-axes process item items rest-items))
    (if fn-element (assign-element symbol symbol-props process-value process '(:symbol-overriding t)
				   space item items rest-items))
    (if (and fn-element (is-workspace-value symbol))
	(let ((fn-content (resolve-function :dyadic fn-element))
	      (fn-sym (or-functional-character fn-element :fn)))
	  (values (if (not symbol-axes)
		      `(setq (inws ,symbol)
			     (apl-call ,fn-sym ,fn-content (inws ,symbol) ,precedent
				       ,@(if function-axes `((list ,@(first function-axes))))))
		      (enclose-axes `(inws, symbol)
				    symbol-axes :set-by
				    `(lambda (item item2) (apl-call ,fn-sym ,fn-content item item2))
				    :set precedent))
		  '(:type (:array :assigned :by-result-assignment-operator))
		  items))
	(values nil nil tokens))))

(setf (getf (nth 0 (vex::idiom-composer-following-patterns *april-idiom*)) :function)
      #'value-assignment-by-function-result)

(defun value-assignment-by-selection (tokens space process &optional precedent properties preceding-properties)
  (declare (ignorable precedent properties preceding-properties))
  (let ((asop) (asop-props) (selection-form) (sform-specs)
	(items tokens) (item (first tokens)) (rest-items (rest tokens))
	(preceding-type (getf (first preceding-properties) :type))
	(preceding-special-props (getf (first preceding-properties) :special)))
    (if (not (member :value-assignment-by-selection (getf preceding-special-props :omit)))
	(assign-element asop asop-props process-function process '(:glyph ←) space item items rest-items))
    (if asop (assign-subprocessed selection-form sform-specs process
				  (list :special '(:omit (:value-assignment :function-assignment)))
				  item items rest-items))
    (or (and selection-form (listp selection-form) (eql 'apl-call (first selection-form))
	     (multiple-value-bind (sel-form sel-item placeholder set-form)
		 (generate-selection-form selection-form space)
	       ;; (print (list :sel sel-form selection-form))
	       (if sel-form
		   ;; generate an array whose each cell is its row-major index, perform the
		   ;; subtractive function on it and then use assign-selected to assign new values
		   ;; to the cells at the remaining indices of the original array
		   (values (if sel-item
			       (let ((item (gensym)) (indices (gensym)))
				 (print (list))
				 (if (or (symbolp sel-item)
					 (and (listp sel-item)
					      (eql 'inws (first sel-item))
					      (symbolp (second sel-item))))
				     `(apl-assign
				       ,sel-item
				       (let* ((,item ,sel-item)
					      (,placeholder (generate-index-array ,item))
					      (,indices (enclose ,sel-form))
					      ,@(if set-form
						    `((,placeholder
						       (make-array nil :initial-element
								   (assign-selected (disclose2 ,item)
										    ,indices ,precedent))))))
					 ,(or set-form `(assign-selected ,sel-item ,indices ,precedent))))
				     `(let* ((,item ,sel-item)
					     (,placeholder (generate-index-array ,item))
					     (,indices (enclose ,sel-form))
					     ,@(if set-form
						   `((,placeholder
						      (make-array nil :initial-element
								  (assign-selected (disclose2 ,item)
										   ,indices ,precedent))))))
					,(or set-form `(assign-selected ,sel-item ,indices ,precedent)))))
			       `(let ((,placeholder ,precedent))
				  (setf ,set-form ,sel-form)))
			   '(:type (:array :assigned))
			   items))))
	(values nil nil tokens))))

(setf (getf (nth 1 (vex::idiom-composer-following-patterns *april-idiom*)) :function)
      #'value-assignment-by-selection)

(defun value-assignment-standard (tokens space process &optional precedent properties preceding-properties)
  (declare (ignorable precedent properties))
  (let ((asop) (asop-props) (axes) (symbol) (symbol-props)
	(items tokens) (item (first tokens)) (rest-items (rest tokens))
	(preceding-type (getf (first preceding-properties) :type))
	(preceding-special-props (getf (first preceding-properties) :special)))
    
    (if (and (eq :array (first preceding-type))
	     (not (member :value-assignment (getf preceding-special-props :omit))))
	(assign-element asop asop-props process-function process '(:glyph ←) space item items rest-items))
    (if asop (assign-axes axes process item items rest-items))
    (if asop (assign-element symbol symbol-props process-value process '(:symbol-overriding t)
			     space item items rest-items))
    ;; (if asop (print (list :pr precedent preceding-properties (getf (first preceding-properties) :type))))
    (let ((output
	   ;; ensure symbol(s) are not bound to function values in the workspace, and
	   ;; define them as dynamic variables if they're unbound there
	   (if symbol
	       (progn
		 ;; remove symbols from (inws) unless they're bare and thus idiom-native
		 (loop :for symbol :in (if (not (listp symbol)) (list symbol)
					   (mapcar (lambda (s) (if (not (listp s)) s (second s))) symbol))
		    :do (if (is-workspace-function symbol)
			    (fmakunbound (intern (string symbol) space)))
		      (if (not (boundp (intern (string symbol) space)))
			  (progn (proclaim (list 'special (intern (string symbol) space)))
				 (set (intern (string symbol) space) nil))))
		 (cond ((eql 'to-output symbol)
			;; a special case to handle ⎕← quad output
			`(apl-output ,precedent :print-precision print-precision
				     :print-to output-stream :print-assignment t))
		       ((eql 'output-stream symbol)
			;; a special case to handle ⎕ost← setting the output stream; the provided string
			;; is interned in the current working package
			(if (stringp precedent)
			    ;; setq is used instead of apl-assign because output-stream is a lexical variable
			    `(setq output-stream ,(intern precedent (package-name *package*)))
			    (if (listp precedent)
				(destructuring-bind (vector-symbol package-string symbol-string) precedent
				  (if (and (eql 'avector vector-symbol)
					   (stringp package-string)
					   (stringp symbol-string))
				      ;; if the argument is a vector of two strings like ('APRIL' 'OUT-STR'),
				      ;; intern the symbol like (intern "OUT-STR" "APRIL")
				      `(setq output-stream ,(intern symbol-string package-string))
				      (error "Invalid assignment to ⎕OST.")))
				(error "Invalid assignment to ⎕OST."))))
		       (t (if (symbolp symbol)
			      (set-workspace-alias space symbol nil))
			  (let ((symbol (if (or (listp symbol) (member symbol *idiom-native-symbols*))
					    symbol (list 'inws symbol))))
			    (if axes (enclose-axes symbol axes :set precedent)
				;; enclose the symbol in (inws) so the (with-april-workspace) macro
				;; will correctly intern it, unless it's one of the system variables
				`(apl-assign ,symbol ,precedent)))))))))
      (if output (values output '(:type (:array :assigned)) items)
	  (values nil nil tokens)))))

(setf (getf (nth 2 (vex::idiom-composer-following-patterns *april-idiom*)) :function)
       #'value-assignment-standard)

(defun function-assignment
    (tokens space process &optional precedent properties preceding-properties)
  (declare (ignorable precedent properties))
  (let ((asop) (asop-props) (symbol) (symbol-props)
	(items tokens) (item (first tokens)) (rest-items (rest tokens))
	(preceding-type (getf (first preceding-properties) :type))
	(preceding-special-props (getf (first preceding-properties) :special)))
    (if (and (eq :function (first preceding-type))
	     (not (member :function-assignment (getf preceding-special-props :omit))))
	(assign-element asop asop-props process-function process '(:glyph ←) space item items rest-items))
    (if asop (assign-element symbol symbol-props process-value process '(:symbol-overriding t)
			     space item items rest-items))
    (let ((output
	   (if asop (let* ((inverted (if (listp precedent) (invert-function precedent)))
			   (inverted-symbol (if inverted (intern (concatenate 'string "𝕚∇" (string symbol))))))
		      (if (is-workspace-value symbol)
			  (makunbound (intern (string symbol) space)))
		      (setf (symbol-function (intern (string symbol) space)) #'dummy-nargument-function)
		      (if inverted (progn (if (is-workspace-value inverted-symbol)
					      (makunbound (intern (string inverted-symbol) space)))
					  (setf (symbol-function (intern (string inverted-symbol) space))
						#'dummy-nargument-function)))
		      (if (characterp precedent)
			  (if (or (resolve-function :monadic precedent)
				  (resolve-function :dyadic precedent))
			      (progn (set-workspace-alias space symbol precedent)
				     (format nil "~a aliases ~a" symbol precedent)))
			  (progn (set-workspace-alias space symbol nil)
				 `(setf (symbol-function (quote (inws ,symbol))) ,precedent
					,@(if inverted `((symbol-function (quote (inws ,inverted-symbol)))
							 ,inverted)))))))))
      (if output (values output '(:type (:function :assigned)) items)
	  (values nil nil tokens)))))

(setf (getf (nth 3 (vex::idiom-composer-following-patterns *april-idiom*)) :function)
      #'function-assignment)

(defun operator-assignment (tokens space process &optional precedent properties pre-properties)
  (declare (ignorable precedent properties))
  (let* ((asop) (asop-props) (symbol) (symbol-props)
	 (items tokens) (item (first tokens)) (rest-items (rest tokens))
	 (preceding-type (getf (first pre-properties) :type))
	 (preceding-special-props (getf (first pre-properties) :special)))
    (if (and (eq :operator (first preceding-type))
	     (not (member :operator-assignment (getf preceding-special-props :omit))))
	(assign-element asop asop-props process-function process '(:glyph ←) space item items rest-items))
    (if asop (assign-element symbol symbol-props process-value process '(:symbol-overriding t)
			     space item items rest-items))
    (let* ((operator-symbol (intern (concatenate 'string (if (member :pivotal preceding-type) "𝕆ℙ∇" "𝕆𝕃∇")
						 (string symbol))))
	   (output (if asop (progn (if (is-workspace-value symbol)
				       (makunbound (intern (string symbol) space)))
				   (setf (symbol-function (intern (string operator-symbol) space))
					 #'dummy-nargument-function)
				   (if (characterp precedent)
				       (if (or (resolve-operator :lateral precedent)
					       (resolve-operator :pivotal precedent))
					   (progn (set-workspace-alias space symbol precedent)
						  (format nil "~a aliases ~a" symbol precedent)))
				       (progn (set-workspace-alias space symbol nil)
					      `(setf (symbol-function (quote (inws ,operator-symbol)))
						     ,precedent)))))))
      (if output (values output '(:type (:operator :assigned)) items)
	  (values nil nil tokens)))))

(setf (getf (nth 4 (vex::idiom-composer-following-patterns *april-idiom*)) :function)
      #'operator-assignment)

(defun branch (tokens space process &optional precedent properties pre-properties)
  (declare (ignorable precedent properties))
  (let* ((asop) (asop-props) (branch-from) (from-props) ;; (determinant) (determinant-props)
	 (items tokens) (item (first tokens)) (rest-items (rest tokens))
	 (preceding-type (getf (first pre-properties) :type))
	 (preceding-special-props (getf (first pre-properties) :special)))
    (if (and (eq :array (first preceding-type))
	     (not (member :branch (getf preceding-special-props :omit))))
	(assign-element asop asop-props process-function process '(:glyph →) space item items rest-items))
    (if asop (assign-element branch-from from-props process-value process
			     '(:cancel-if :pivotal-composition) space item items rest-items))
    ;; TODO: figure out need for determinant function
    ;; (assign-element determinant determinant-props process-function process
    ;;	       nil space item items rest-items)

    (let ((output
	   (if asop
	       (progn
		 (if (listp precedent)
		     (if (loop :for item :in precedent :always (and (listp item) (eql 'inws (first item))))
			 (setq precedent (mapcar #'second precedent))
			 (if (eql 'inws (first precedent))
			     (setq precedent (second precedent)))))
		 (if (and branch-from (eql 'to-output precedent)) ;;(string= "TO-OUTPUT" (string precedent)))
		     ;; if this is a branch point statement like X→⎕, do the following:
		     (if (integerp branch-from)
			 ;; if the branch is designated by an integer like 5→⎕
			 (let ((branch-symbol (gensym "AB"))) ;; AB for APL Branch
			   (setf *branches* (cons (list branch-from branch-symbol) *branches*))
			   branch-symbol)
			 ;; if the branch is designated by a symbol like doSomething→⎕
			 (if (symbolp branch-from)
			     (progn (setf *branches* (cons branch-from *branches*))
				    branch-from)
			     (error "Invalid left argument to →; must be a single integer value or a symbol.")))
		     ;; otherwise, this is a branch-to statement like →5 or →doSomething
		     (if (or (integerp precedent)
			     (symbolp precedent))
			 ;; if the target is an explicit symbol as in →mySymbol, or explicit index
			 ;; as in →3, just pass the symbol through
			 (list 'go precedent)
			 (if (loop :for item :in (rest precedent) :always (symbolp item))
			     ;; if the target is one of an array of possible destination symbols...
			     (if (integerp branch-from)
				 ;; if there is an explicit index to the left of the arrow, grab the corresponding
				 ;; symbol unless the index is outside the array's scope, in which case a (list) is returned
				 ;; so nothing is done
				 (if (< 0 branch-from (length (rest precedent)))
				     (list 'go (second (nth (1- branch-from) (rest precedent))))
				     (list 'list))
				 ;; otherwise, there must be an expression to the left of the arrow, as with
				 ;; (3-2)→tagOne tagTwo, so pass it through for the postprocessor
				 (list 'go precedent branch-from))
			     (list 'go precedent))))))))
      (if output (values output '(:type (:branch)) items)
	  (values nil nil tokens)))))

(setf (getf (nth 5 (vex::idiom-composer-following-patterns *april-idiom*)) :function)
      #'branch)

(defun train-composition (tokens space process &optional precedent properties pre-properties)
  (declare (ignorable precedent properties))
  (let ((center) (center-props) (is-center-function) (left) (left-props)
	(items tokens) (item (first tokens)) (rest-items (rest tokens))
	(preceding-type (getf (first pre-properties) :type))
	(preceding-special-props (getf (first pre-properties) :special)))
    (if (and (eq :function (first preceding-type))
	     (not (member :function-assignment (getf preceding-special-props :omit))))
	(progn (assign-subprocessed center center-props process
				    (list :special '(:omit (:value-assignment :function-assignment)))
				    item items rest-items)
	       (setq is-center-function (eq :function (first (getf center-props :type))))
	       (if is-center-function
		   (assign-subprocessed left left-props process
					(list :special '(:omit (:value-assignment :function-assignment)))
					item items rest-items))))
    (let ((output (if (and left is-center-function)
		      (destructuring-bind (right omega alpha center)
			  (list precedent (gensym) (gensym)
 				(if (listp center)
 				    center (resolve-function
					    :dyadic (if (not (symbolp center))
 							center (intern (string center) space)))))
			;; train composition is only valid when there is only one function in the precedent
			;; or when continuing a train composition as for (×,-,÷)5
			(if (and center (or (= 1 (length pre-properties))
 					    (and (member :train-composition (getf (first pre-properties) :type))
 						 (not (member :closed (getf (first pre-properties) :type))))))
 			    ;; functions are resolved here, failure to resolve indicates a value in the train
 			    (let ((right-fn-monadic (if (and (listp right) (eql 'function (first right)))
 							right (resolve-function :monadic right)))
 				  (right-fn-dyadic (if (and (listp right) (eql 'function (first right)))
 						       right (resolve-function :dyadic right)))
 				  (left-fn-monadic (if (and (listp left) (eql 'function (first left)))
 						       left (resolve-function :monadic left)))
 				  (left-fn-dyadic (if (and (listp left) (eql 'function (first left)))
 						      left (resolve-function :dyadic left))))
 			      `(lambda (,omega &optional ,alpha)
 				 (if ,alpha (apl-call ,(or-functional-character center :fn) ,center
 						      (apl-call ,(or-functional-character right :fn)
 								,right-fn-dyadic ,omega ,alpha)
 						      ,(if (not left-fn-dyadic)
 							   left `(apl-call ,(or-functional-character left :fn)
 									   ,left-fn-dyadic ,omega ,alpha)))
 				     (apl-call ,(or-functional-character center :fn) ,center
 					       (apl-call ,(or-functional-character right :fn)
							 ,right-fn-monadic ,omega)
 					       ,(if (not left-fn-monadic)
 						    left `(apl-call ,(or-functional-character left :fn)
 								    ,left-fn-monadic ,omega)))))))))))
      (if output (values output (list :type (list :function :train-composition
						  (if (resolve-function :monadic left) :open :closed)))
			 items)
	  (values nil nil tokens)))))

(setf (getf (nth 6 (vex::idiom-composer-following-patterns *april-idiom*)) :function)
      #'train-composition)

(defun pivotal-or-lateral-inline-composition
    (tokens space process &optional precedent properties pre-properties)
  (declare (ignorable precedent properties))
  (let ((operator) (operator-props) (left-operand-axes) (left-operand) (left-operand-props)
	(right-operand-axes) (items tokens) (item (first tokens)) (rest-items (rest tokens))
	(preceding-type (getf (first pre-properties) :type))
	(preceding-special-props (getf (first pre-properties) :special)))
    (assign-element operator operator-props process-operator process '(:valence :pivotal)
		    space item items rest-items)
    (if operator
	(progn (assign-axes left-operand-axes process item items rest-items)
	       (assign-subprocessed
		left-operand left-operand-props process
		(list :special '(:omit (:value-assignment :function-assignment :operation)))
		item items rest-items)))
    ;; (print (list :prpr properties pre-properties))
    (let ((output
	   (if operator
	       (let ((right-operand (insym precedent))
		     (right-operand-axes (first (getf (first pre-properties) :axes)))
		     (left-operand (insym left-operand))
		     ;; (left-operand-axes (first (getf (second properties) :axes)))
		     (omega (gensym)) (alpha (gensym)))
		 ;; (print (list :rri right-operand-axes (second pre-properties)))
		 (if (symbolp operator) `(apl-compose :op ,(list 'inws operator)
						      ,(if (listp left-operand)
							   left-operand
							   (if (characterp left-operand)
							       `(lambda (,omega &optional ,alpha)
								  (if ,alpha
								      (apl-call :fn ,(resolve-function
										      :dyadic left-operand)
										,omega ,alpha)
								      (apl-call :fn ,(resolve-function
										      :monadic left-operand)
										,omega)))))
						      ,(if (listp right-operand)
							   left-operand
							   (if (characterp right-operand)
							       `(lambda (,omega &optional ,alpha)
								  (if ,alpha
								      (apl-call :fn ,(resolve-function
										      :dyadic right-operand)
										,omega ,alpha)
								      (apl-call :fn ,(resolve-function
										      :monadic right-operand)
										,omega)))))
						      ;; TODO: implement operand axes
						      ;; operand-axes
						      )
		     (if (listp operator)
			 `(apl-call :fn (apl-compose :op ,operator
						     ,(if (listp left-operand)
							  left-operand
							  (if (characterp left-operand)
							      `(lambda (,omega &optional ,alpha)
								 (if ,alpha
								     (apl-call :fn ,(resolve-function
										     :dyadic left-operand)
									       ,omega ,alpha)
								     (apl-call :fn ,(resolve-function
										     :monadic left-operand)
									       ,omega))))))
				    ,precedent)
			 (cons 'apl-compose
			       (cons (intern (string-upcase operator))
				     (funcall (funcall (resolve-operator :pivotal operator)
						       left-operand left-operand-axes
						       right-operand right-operand-axes)
					      right-operand left-operand)))))))))
      (if output (values output '(:type (:function :operator-composed :pivotal)) items)
	  (values nil nil tokens)))))

(setf (getf (nth 7 (vex::idiom-composer-following-patterns *april-idiom*)) :function)
      #'pivotal-or-lateral-inline-composition)

(defun operation (tokens space process &optional precedent properties pre-properties)
  (declare (ignorable precedent properties))
  ;; (print (list :tok tokens))
  (let ((function-axes) (fn-element) (function-props) (is-function) (value) (value-props)
	(prior-items) (items tokens) (item (first tokens)) (rest-items (rest tokens))
	(preceding-type (getf (first pre-properties) :type))
	(preceding-special-props (getf (first pre-properties) :special)))
  (if (and (eq :array (first preceding-type))
	   (not (member :operation (getf preceding-special-props :omit))))
      (progn (assign-subprocessed fn-element function-props process
				  (list :special '(:omit (:function-assignment :value-assignment-by-selection)))
				  item items rest-items)
	     (setq is-function (eq :function (first (getf function-props :type)))
		   prior-items items)
	     (if is-function
		 (assign-subprocessed value value-props process
		 		      (list :special '(:omit (:value-assignment :function-assignment
							      :value-assignment-by-selection :branch
		 					      :operation :operator-assignment))
					    :valence :lateral)
		 		      item items rest-items))
	     (if (not (eq :array (first (getf value-props :type))))
		 (setq items prior-items value nil))
	     (if (and (not function-axes) (member :axes function-props))
		 (setq function-axes (getf function-props :axes)))
	     (let ((output (if is-function
			       (let ((fn-content (if (or (functionp fn-element)
							 (member fn-element '(⍺⍺ ⍵⍵))
							 (and (listp fn-element)
							      (eql 'function (first fn-element))))
						     fn-element (resolve-function (if value :dyadic :monadic)
										  (insym fn-element))))
				     (fn-sym (or-functional-character fn-element :fn)))
				 `(apl-call ,fn-sym ,fn-content ,precedent ,@(if value (list value))
					    ,@(if function-axes `((list ,@(first function-axes)))))))))
	       ;; (print (list :oo output items))
	       (if output (values output '(:type (:array :evaluated)) items)
		   (values nil nil tokens))))
      (values nil nil tokens))))

(setf (getf (nth 8 (vex::idiom-composer-following-patterns *april-idiom*)) :function)
      #'operation)

