;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; grammar.lisp

(in-package #:april)

"This file contains the specification of April's basic grammar elements, including the basic language components - array, function and operator - and the patterns comprising those elements that make up the language's strucures."

(defun process-value (this-item properties process idiom space)
  ;; TODO: add a passthrough value mode for symbols that are being assigned!
  ;; this is the only way to get them assignable after the first time
  (cond ((and (listp this-item)
	      (not (member (first this-item) '(:fn :op :axes))))
	 ;; if the item is a closure, evaluate it and return the result
	 (multiple-value-bind (output out-properties)
	     (funcall process this-item)
	   (if (eq :array (first (getf out-properties :type)))
	       (progn (if (not (member :enclosed (getf out-properties :type)))
			  (setf (getf out-properties :type)
				(cons (first (getf out-properties :type))
				      (cons :enclosed (rest (getf out-properties :type))))))
		      (values output out-properties))
	       (values nil nil))))
	;; process the empty vector expressed by the [⍬ zilde] character
	((eq :empty-array this-item)
	 (values (make-array 0) '(:type (:array :empty))))
	;; process numerical values
	((and (numberp this-item)
	      (or (not (getf properties :type))
		  (eq :number (first (getf properties :type)))))
	 (values this-item '(:type (:array :number))))
	;; process string values
	((and (stringp this-item)
	      (or (not (getf properties :type))
		  (eq :string (first (getf properties :type)))))
	 (values this-item '(:type (:array :string))))
	;; process scalar character values
	((and (characterp this-item)
	      (or (not (getf properties :type))
		  (eq :character (first (getf properties :type)))))
	 (values this-item '(:type (:array :character))))
	;; process symbol-referenced values
	((and (symbolp this-item)
	      (or (member this-item '(⍵ ⍺) :test #'eql)
		  (getf properties :symbol-overriding)
		  (not (is-workspace-function this-item)))
	      (or (not (is-workspace-operator this-item))
		  (getf properties :symbol-overriding))
	      (not (member (intern (string-upcase this-item))
			   (rest (assoc :function (idiom-symbols idiom)))))
	      (not (member this-item '(⍺⍺ ⍵⍵) :test #'eql))
	      (or (not (getf properties :type))
		  (eq :symbol (first (getf properties :type)))))
	 (values (if (not (member (intern (string-upcase this-item))
				  (rest (assoc :function (idiom-symbols idiom)))))
		     this-item (intern (string-upcase this-item)))
		 '(:type (:symbol))))
	(t (values nil nil))))

(defun process-function (this-item properties process idiom space)
  (if (listp this-item)
      ;; process a function specification starting with :fn
      (if (or (eq :fn (first this-item))
	      ;; if marked as an operator, check whether the character is one entered as both
	      ;; a function and an operator; such functions must be dyadic
	      (and (eq :op (first this-item))
		   (or (of-lexicon idiom :dyadic-functions (third this-item))
		       (of-lexicon idiom :symbolic-functions (third this-item)))))
	  (let ((fn (first (last this-item)))
		(obligate-dyadic (and (eq :op (first this-item))
				      (of-lexicon idiom :dyadic-functions (third this-item))))
		(overloaded-operator (and (eq :op (first this-item))
					  (or (of-lexicon idiom :dyadic-functions (third this-item))
					      (of-lexicon idiom :symbolic-functions (third this-item))))))
	    (cond ((and (characterp fn)
			(or (not (getf properties :glyph))
			    (and (char= fn (aref (string (getf properties :glyph)) 0)))))
		   (values fn (list :type (append '(:function :glyph)
						  (if overloaded-operator '(:overloaded-operator))
						  (if (of-lexicon idiom :symbolic-functions (third this-item))
						      '(:symbolic-function))))))
		  ((and (listp fn)
			(not (getf properties :glyph)))
		   (let* ((polyadic-args (if (and (listp (first (last (first fn))))
						  (eq :axes (caar (last (first fn)))))
					     (mapcar #'caar (cdar (last (first fn))))))
			  (fn (if (not polyadic-args)
				  fn (cons (butlast (first fn) 1)
					   (rest fn))))
			  (symbols-used (glean-symbols-from-tokens fn space))
			  (is-inline-operator (intersection '(⍺⍺ ⍵⍵) (rest (assoc :args symbols-used)))))
		     ;; if this is an inline operator, pass just that keyword back
		     (if is-inline-operator :is-inline-operator
			 (values (output-function (if (= 1 (length fn))
						      (list (funcall process fn))
						      (mapcar process fn))
						  polyadic-args symbols-used)
				 (list :type '(:function :closure)
				       :obligate-dyadic obligate-dyadic)))))
		  (t (values nil nil))))
	  ;; process sub-list in case it is a functional expression like (+∘*),
	  ;; but don't do this if looking for a specific functional glyph
	  (if (not (getf properties :glyph))
	      (multiple-value-bind (output out-properties)
	    	  (funcall process this-item)
	    	(if (eq :function (first (getf out-properties :type)))
	    	    (progn (setf (getf out-properties :type)
	    			 (cons (first (getf out-properties :type))
	    			       (cons :enclosed (rest (getf out-properties :type)))))
	    		   (values output out-properties))
	    	    (values nil nil)))
	      (values nil nil)))
      (if (and (symbolp this-item)
	       (not (getf properties :glyph)))
	  (cond ((or (is-workspace-function this-item)
		     (get-workspace-alias space this-item))
		 ;; process workspace-aliased lexical functions, as when f←+ has been set
		 (values (if (not (get-workspace-alias space this-item))
			     this-item (get-workspace-alias space this-item))
			 (list :type '(:function :referenced))))
		((member this-item '(⍵⍵ ⍺⍺))
		 (values this-item (list :type '(:function :operator-reference))))
		((member (intern (string-upcase this-item))
			 (rest (assoc :function (idiom-symbols idiom))))
		 (values (list 'function (getf (rest (assoc :function (idiom-symbols idiom)))
					       (intern (string-upcase this-item))))
			 (list :type '(:function :referenced))))
		(t (values nil nil))))))

(defun process-operator (this-item properties process idiom space)
  (if (listp this-item)
      (if (eq :op (first this-item))
	  ;; process an operator token, allowing specification of the valence,
	  ;; either :lateral or :pivotal
	  (destructuring-bind (op-type op-symbol)
	      (rest this-item)
	    (let ((valid-by-valence (or (not (getf properties :valence))
					(eq op-type (getf properties :valence)))))
	      (cond ((and valid-by-valence (getf properties :glyph))
		     (if (char= op-symbol (aref (string (getf properties :glyph)) 0))
			 (values op-symbol (list :type (list :operator op-type)))
			 (values nil nil)))
		    (valid-by-valence (values op-symbol (list :type (list :operator op-type))))
		    (t (values nil nil)))))
	  (if (and (eql :fn (first this-item))
		   (listp (first (last this-item))))
	      (let* ((fn (first (last this-item)))
		     (symbols-used (glean-symbols-from-tokens fn space))
		     (is-inline (intersection '(⍺⍺ ⍵⍵) (rest (assoc :args symbols-used))))
		     (is-pivotal (member '⍵⍵ (rest (assoc :args symbols-used)))))
		(if is-inline (values (output-function (if (= 1 (length fn))
							   (list (funcall process fn))
							   (mapcar process fn))
						       nil symbols-used)
				      (list :type (list :operator :closure
							(if is-pivotal :pivotal :lateral))))
		    (values nil nil)))
	      (values nil nil)))
      (if (symbolp this-item)
	  ;; if the operator is represented by a symbol, it is a user-defined operator
	  ;; and the appropriate variable name should be verified in the workspace
	  (let* ((symbol-string (string this-item))
		 (type-to-find (getf properties :valence))
		 (lop-string (if (eq :lateral (getf properties :valence))
				 (concatenate 'string "𝕆𝕃∇" symbol-string)))
		 (pop-string (if (eq :pivotal (getf properties :valence))
				 (concatenate 'string "𝕆ℙ∇" symbol-string)))
		 (bound-op (if (and lop-string (fboundp (intern lop-string space)))
			       (intern lop-string)
			       (if (and pop-string (fboundp (intern pop-string space)))
				   (intern pop-string)))))
	    (if bound-op
		(values bound-op (list :type (list :operator (or (getf properties :valence)
								 (if (fboundp (intern pop-string space))
								     :pivotal :lateral)))))
		(values nil nil))))))

(defun composer-pattern-value (tokens space idiom process &optional precedent properties preceding-properties)
  (declare (ignorable precedent properties preceding-properties))
  (symbol-macrolet ((item (first items)) (rest-items (rest items)))
    (let ((axes) (value-elements) (value-props) (stopped) (items tokens))
      (labels ((axes-enclose (item axes)
		 (if (not axes) item (enclose-axes item axes))))
	(progn (if (and (listp item) (eql :axes (first item)))
    		   (setq axes (list (loop :for axis :in (rest item) :collect (funcall process axis)))
    			 items rest-items))
	       (loop :while (not stopped)
		  :do (or (if (and (listp item) (eq :axes (first item)))
			      ;; if axes are encountered, process the axes and the preceding
			      ;; value as a new value
			      (multiple-value-bind (output properties remaining)
				  (funcall process items)
				(if (eq :array (first (getf properties :type)))
				    (setq items remaining
					  value-elements (cons output value-elements)
					  value-props (cons properties value-props)
					  stopped t))))
			  (if (and (listp item) (not (member (first item) '(:op :fn :axes))))
			      ;; if a closure is encountered, recurse to process it
			      (multiple-value-bind (output properties remaining) (funcall process item)
				(if (eq :array (first (getf properties :type)))
				    (setq items rest-items
					  value-elements (cons output value-elements)
					  value-props (cons properties value-props)))))
			  (multiple-value-bind (value-out value-properties)
			      (process-value item properties process idiom space)
			    (if value-out (setq items rest-items
						value-elements (cons value-out value-elements)
						value-props (cons value-properties value-props))))
			  (setq stopped t))))
	(if value-elements
	    (values (axes-enclose (output-value space (if (< 1 (length value-elements))
							  value-elements (first value-elements))
						value-props)
				  axes)
		    '(:type (:array :explicit))
		    items)
	    (values nil nil tokens))))))

(vex::composer-pattern-template
 (composer-pattern assign-axes assign-element assign-subprocessed)
 tokens space idiom process precedent properties preceding-properties items item rest-items)

;; (composer-pattern
;;     composer-pattern-function (axes function-form function-props)
;;     ((assign-axes axes process item items rest-items)
;;      (assign-element function-form function-props process-function process
;; 		     nil space item items rest-items))
;;   (let ((is-function (or (not (member :overloaded-operator (getf function-props :type)))
;; 			 (let ((next (if items (multiple-value-list (funcall process items)))))
;; 			   (not (member :function (getf (second next) :type)))))))
;;     (if (and function-form is-function)
;; 	(values (if (or (not axes) (of-lexicon idiom :functions function-form))
;; 		    (if (not (and (symbolp function-form) (is-workspace-function function-form)))
;; 			function-form `(function (inws ,function-form)))
;; 		    `(apl-call :nafn (function ,(insym function-form)) ,@(first axes)))
;; 		(list :type (if (member :operator (getf function-props :type))
;; 				(list :operator :inline-operator
;; 				      (if (member :pivotal (getf function-props :type))
;; 					  :pivotal :lateral))
;; 				'(:function :inline-function))
;; 		      :axes axes)
;; 		items))))

(defun composer-pattern-function (tokens space idiom process &optional precedent properties preceding-properties)
  (declare (ignorable precedent properties preceding-properties))
  (symbol-macrolet ((item (first items)) (rest-items (rest items)))
    (let ((axes) (function-form) (function-props) (prior-items) (items tokens))
      (progn (assign-axes axes)
	     (setq prior-items items)
	     (assign-element function-form function-props process-function)
	     (if (eq :is-inline-operator function-form)
		 (progn (setq items prior-items)
			(assign-element function-form function-props process-operator))))
      (let ((is-function (or (not (member :overloaded-operator (getf function-props :type)))
			     (let ((next (if items (multiple-value-list (funcall process items)))))
			       (not (member :function (getf (second next) :type)))))))
	(if (and function-form is-function)
	    (values (if (or (not axes) (of-lexicon idiom :functions function-form))
			(if (not (and (symbolp function-form) (is-workspace-function function-form)))
			    function-form `(function (inws ,function-form)))
			(let ((call-form (if (listp function-form)
					     function-form `(function ,(insym function-form)))))
			  `(apl-call :nafn ,call-form ,@(first axes))))
		    (list :type (if (member :operator (getf function-props :type))
				    (list :operator :inline-operator
					  (if (member :pivotal (getf function-props :type))
					      :pivotal :lateral))
				    '(:function :inline-function))
			  :axes (or axes (getf function-props :axes)))
		    items)
	    (values nil nil tokens))))))

(defun composer-pattern-lateral-composition
    (tokens space idiom process &optional precedent properties preceding-properties)
  (declare (ignorable precedent properties preceding-properties))
  (labels ((verify-lateral-operator-symbol (symbol)
	     (if (symbolp symbol) (let ((symbol (intern (concatenate 'string "𝕆𝕃∇" (string symbol)))))
				    (if (fboundp (intern (string symbol) space)) symbol)))))
    (symbol-macrolet ((item (first items)) (rest-items (rest items)))
      (let ((operator-axes) (operator-form) (operator-props) (operand-axes) (operand-form) (operand-props)
	    (symbol-referenced) (items tokens) (alpha (gensym)) (omega (gensym)))
	(assign-axes operator-axes)
	(setq symbol-referenced (verify-lateral-operator-symbol item))
	(assign-element operator-form operator-props process-operator '(:valence :lateral))
	(if operator-form
	    (progn (assign-axes operand-axes)
		   (assign-subprocessed operand-form operand-props
		 			'(:special (:omit (:value-assignment :function-assignment
		 					   :operation :operator-assignment))))))
	(if symbol-referenced
	    (values (list 'apl-compose :op (list 'inws symbol-referenced)
	  		  (if (listp operand-form)
	  		      operand-form
			      (if (characterp operand-form)
	  		    	  `(lambda (,omega &optional ,alpha)
	  		    	     (if ,alpha (apl-call :fn ,(resolve-function :dyadic operand-form)
	  		    				  ,omega ,alpha)
	  		    		 (apl-call :fn ,(resolve-function :monadic operand-form)
	  		    			   ,omega)))))
	  		  ;; TODO: implement operand axes
	  		  ;; operand-axes
	  		  )
	  	    '(:type (:function :operator-composed :lateral))
	  	    items)
	    (let ((operator (and (member :operator (getf operator-props :type))
	      			 (member :lateral (getf operator-props :type))
	      			 operator-form)))
	      (if operator (values (cons 'apl-compose
	      				 (cons (intern (string-upcase operator))
	      				       (funcall (funcall (resolve-operator :lateral operator)
	      							 operand-form (first operand-axes))
	      						(first operator-axes))))
	      			   '(:type (:function :operator-composed :lateral))
	      			   items)
		  (values nil nil tokens))))))))

(defun composer-pattern-unitary-operation
    (tokens space idiom process &optional precedent properties preceding-properties)
  (symbol-macrolet ((item (first items)) (rest-items (rest items)))
    (let ((operator-axes) (operator-form) (operator-props) (items tokens))
      (assign-axes operator-axes)
      (assign-element operator-form operator-props process-operator)
      (let ((operator (and (member :operator (getf operator-props :type))
			   (member :unitary (getf operator-props :type))
			   operator-form)))
	(if (resolve-operator :unitary operator)
	    (values (funcall (resolve-operator :unitary operator) space (first operator-axes))
		    '(:type (:array :evaluated))
		    items)
	    (values nil nil tokens))))))

(defvar *composer-opening-patterns-new*)

(setq *composer-opening-patterns-new*
      '((:name :value :function composer-pattern-value)
	(:name :function :function composer-pattern-function)
	(:name :lateral-composition :function composer-pattern-lateral-composition)
	(:name :unitary-operator :function composer-pattern-unitary-operation)))

(defun value-assignment-by-function-result
    (tokens space idiom process &optional precedent properties preceding-properties)
  "Match the assignment of a function result to a value, like a+←5."
  (declare (ignorable precedent properties preceding-properties))
  (symbol-macrolet ((item (first items)) (rest-items (rest items)))
    (let ((asop) (asop-props) (fn-element) (fnel-specs) (function-axes) (symbol) (symbol-props) (symbol-axes)
	  (items tokens) (preceding-type (getf (first preceding-properties) :type))
	  (preceding-special-props (getf (first preceding-properties) :special)))
      (if (not (member :value-assignment-by-function-result (getf preceding-special-props :omit)))
	  (assign-element asop asop-props process-function '(:glyph ←)))
      (if asop (assign-axes function-axes))
      (if asop (assign-subprocessed fn-element fnel-specs
				    '(:special (:omit (:value-assignment :function-assignment)))))
      (if fn-element (assign-axes symbol-axes))
      (if fn-element (assign-element symbol symbol-props process-value '(:symbol-overriding t)))
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
	  (values nil nil tokens)))))

(defun value-assignment-by-selection
    (tokens space idiom process &optional precedent properties preceding-properties)
  "Match a selective value assignment like (3↑x)←5."
  (declare (ignorable precedent properties preceding-properties))
  (symbol-macrolet ((item (first items)) (rest-items (rest items)))
    (let ((asop) (asop-props) (selection-form) (sform-specs) (items tokens)
	  (preceding-type (getf (first preceding-properties) :type))
	  (preceding-special-props (getf (first preceding-properties) :special)))
      (if (not (member :value-assignment-by-selection (getf preceding-special-props :omit)))
	  (assign-element asop asop-props process-function '(:glyph ←)))
      (if asop (let ((items (if (not (listp (first items)))
				items (first items))))
		 (assign-subprocessed selection-form sform-specs
				      '(:special (:omit (:value-assignment :function-assignment))))))
      (if selection-form (setf items (rest items)))
      (let ((output
	     (if (and selection-form (listp selection-form) (eql 'apl-call (first selection-form)))
		 (multiple-value-bind (sel-form sel-item placeholder set-form)
		     (generate-selection-form selection-form space)
		   (if sel-form
		       ;; generate an array whose each cell is its row-major index, perform the
		       ;; subtractive function on it and then use assign-selected to assign new values
		       ;; to the cells at the remaining indices of the original array
		       (if sel-item
			   (let ((item (gensym)) (indices (gensym)))
			     (funcall (lambda (form)
					(if (not (or (symbolp sel-item)
						     (and (listp sel-item)
							  (eql 'inws (first sel-item))
							  (symbolp (second sel-item)))))
					    ;; the assigned value is returned at the end so things like
					    ;; a←⍳5 ⋄ b←(3⊃a)←30 ⋄ a b work
					    form `(progn (apl-assign ,sel-item ,form)
							 ,precedent)))
				      `(let* ((,item ,sel-item)
					      (,placeholder (generate-index-array ,item))
					      (,indices (enclose-atom ,sel-form))
					      ,@(if set-form `((,placeholder
								(make-array nil :initial-element
									    (assign-selected
									     (disclose2 ,item)
									     ,indices ,precedent))))))
					 ,(or set-form `(assign-selected
							 ,sel-item ,indices ,precedent)))))
			   (let ((output (gensym)))
			     `(let* ((,placeholder ,precedent)
				     (,output ,sel-form))
				(if ,output (setf ,set-form ,output))))))))))
	(if output (values output '(:type (:array :assigned)) items)
	    (values nil nil tokens))))))

(defun value-assignment-standard (tokens space idiom process &optional precedent properties preceding-properties)
  "Match a value assignment like a←1 2 3, part of a value expression."
  (declare (ignorable precedent properties))
  (symbol-macrolet ((item (first items)) (rest-items (rest items)))
    (let ((asop) (asop-props) (axes) (symbol) (symbol-props) (items tokens)
	  (preceding-type (getf (first preceding-properties) :type))
	  (preceding-special-props (getf (first preceding-properties) :special)))
      (if (and (eq :array (first preceding-type))
	       (not (member :value-assignment (getf preceding-special-props :omit))))
	  (assign-element asop asop-props process-function '(:glyph ←)))
      (if asop (assign-axes axes))
      (if asop (assign-element symbol symbol-props process-value '(:symbol-overriding t)))

      ;; (if asop (assign-subprocessed symbol symbol-props
      ;; 				    '(:special (:omit (:value-assignment :function-assignment)))))
      ;; (APRIL "5+(a b c)←1 2 3") fn←{⍺+⍵} ⋄ ⌊10_000×{⍺+÷⍵}/40/1 ⋄ {⍺×⍵+3}⌿3 4⍴⍳12 ⋄ {⍵÷3}¨10 ⋄ 1 {⍺+⍵÷3}¨10
      ;; (⍳3)⌽[3]¨⊂2 3 4⍴⍳9 ⋄ (3 3⍴⊂3 3⍴⍳9)×¨3 3⍴⍳9 ⋄ fn←{⍺+2×⍵} ⋄ 15 25 35 fn⍤1⊢2 2 3⍴⍳8
      
      (let ((output
	     (if symbol
		 (progn
		   ;; ensure symbol(s) are not bound to function values in the workspace, and
		   ;; define them as dynamic variables if they're unbound there;
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
	    (values nil nil tokens))))))

(defun function-assignment (tokens space idiom process &optional precedent properties preceding-properties)
  "Match a function assignment like f←{⍵×2}, part of a functional expression."
  (declare (ignorable precedent properties))
  (symbol-macrolet ((item (first items)) (rest-items (rest items)))
    (let ((asop) (asop-props) (symbol) (symbol-props) (items tokens)
	  (preceding-type (getf (first preceding-properties) :type))
	  (preceding-special-props (getf (first preceding-properties) :special)))
      (if (and (eq :function (first preceding-type))
	       (not (member :function-assignment (getf preceding-special-props :omit))))
	  (assign-element asop asop-props process-function '(:glyph ←)))
      (if asop (assign-element symbol symbol-props process-value '(:symbol-overriding t)))
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
	    (values nil nil tokens))))))

(defun operator-assignment (tokens space idiom process &optional precedent properties pre-properties)
  "Match an operator assignment like f←{⍵×2}, part of a functional expression."
  (declare (ignorable precedent properties))
  (symbol-macrolet ((item (first items)) (rest-items (rest items)))
    (let* ((asop) (asop-props) (symbol) (symbol-props) (items tokens)
	   (preceding-type (getf (first pre-properties) :type))
	   (preceding-special-props (getf (first pre-properties) :special)))
      (if (and (eq :operator (first preceding-type))
	       (not (member :operator-assignment (getf preceding-special-props :omit))))
	  (assign-element asop asop-props process-function '(:glyph ←)))
      (if asop (assign-element symbol symbol-props process-value '(:symbol-overriding t)))
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
	    (values nil nil tokens))))))

(defun branch (tokens space idiom process &optional precedent properties pre-properties)
  "Match a branch-to statement like →1 or a branch point statement like 1→⎕."
  (declare (ignorable precedent properties))
  (symbol-macrolet ((item (first items)) (rest-items (rest items)))
    (let* ((asop) (asop-props) (branch-from) (from-props) (items tokens)
	   (preceding-type (getf (first pre-properties) :type))
	   (preceding-special-props (getf (first pre-properties) :special)))
      (if (and (eq :array (first preceding-type))
	       (not (member :branch (getf preceding-special-props :omit))))
	  (assign-element asop asop-props process-function '(:glyph →)))
      (if asop (assign-element branch-from from-props process-value))
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
				   ;; if there is an explicit index to the left of the arrow,
				   ;; grab the corresponding symbol unless the index is outside the
				   ;; array's scope, in which case a (list) is returned so nothing is done
				   (if (< 0 branch-from (length (rest precedent)))
				       (list 'go (second (nth (1- branch-from) (rest precedent))))
				       (list 'list))
				   ;; otherwise, there must be an expression to the left of the arrow, as with
				   ;; (3-2)→tagOne tagTwo, so pass it through for the postprocessor
				   (list 'go precedent branch-from))
			       (list 'go precedent))))))))
	(if output (values output '(:type (:branch)) items)
	    (values nil nil tokens))))))

(defun train-composition (tokens space idiom process &optional precedent properties pre-properties)
  "Match a train function composition like (-,÷)."
  (declare (ignorable precedent properties))
  (symbol-macrolet ((item (first items)) (rest-items (rest items)))
    (let ((center) (center-props) (is-center-function) (left) (left-props) (items tokens) 
	  (preceding-type (getf (first pre-properties) :type))
	  (preceding-special-props (getf (first pre-properties) :special)))
      (if (and (eq :function (first preceding-type))
	       (not (member :function-assignment (getf preceding-special-props :omit))))
	  (progn (assign-subprocessed center center-props
				      '(:special (:omit (:value-assignment :function-assignment))))
		 (setq is-center-function (eq :function (first (getf center-props :type))))
		 (if is-center-function
		     (assign-subprocessed left left-props
					  '(:special (:omit (:value-assignment :function-assignment)))))))
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
	    (values nil nil tokens))))))

(defun pivotal-or-lateral-inline-composition
    (tokens space idiom process &optional precedent properties pre-properties)
  "Match a pivotal function composition like ×.+, part of a functional expression. It may come after either a function or an array, since some operators take array operands."
  (declare (ignorable precedent properties))
  (symbol-macrolet ((item (first items)) (rest-items (rest items)))
    (let ((operator) (operator-props) (left-operand-axes) (left-operand) (left-operand-props) (prior-items)
	  (right-operand-axes) (items tokens)
	  (preceding-type (getf (first pre-properties) :type))
	  (preceding-special-props (getf (first pre-properties) :special)))
      (assign-element operator operator-props process-operator '(:valence :pivotal))
      (if operator (progn (assign-axes left-operand-axes)
			  (setq prior-items items)
			  (assign-element left-operand left-operand-props process-function)
			  ;; if the next function is symbolic, assign it uncomposed;
			  ;; this is needed for things like ∊∘.+⍨10 2 to work correctly
			  (if (not (member :symbolic-function (getf left-operand-props :type)))
			      (progn (setq items prior-items item (first items) rest-items (rest items))
				     (assign-subprocessed
				      left-operand left-operand-props
				      '(:special (:omit (:value-assignment :function-assignment :operation)))
				      )))))
      (let ((output
	     (if operator
		 (let ((right-operand (insym precedent))
		       (right-operand-axes (first (getf (first pre-properties) :axes)))
		       (left-operand (insym left-operand))
		       ;; (left-operand-axes (first (getf (second properties) :axes)))
		       (omega (gensym)) (alpha (gensym)))
		   ;; (print (list :right right-operand pre-properties))
		   (if (or (symbolp operator) (and (listp operator)
						   (member :pivotal (getf operator-props :type))))
		       `(apl-compose :op ,(if (listp operator)
					      operator (list 'inws operator))
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
		       (if (and (listp operator)
				(member :lateral (getf operator-props :type)))
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
			   (cons 'apl-compose (cons (intern (string-upcase operator))
						    (funcall (funcall (resolve-operator :pivotal operator)
								      left-operand left-operand-axes
								      right-operand right-operand-axes)
							     right-operand left-operand)))))))))
	(if output (values output '(:type (:function :operator-composed :pivotal)) items)
	    (values nil nil tokens))))))

(defun operation (tokens space idiom process &optional precedent properties pre-properties)
  "Match an operation on values like 1+1 2 3, ⍳9 or +/⍳5, these operations are the basis of APL."
  (declare (ignorable precedent properties))
  (symbol-macrolet ((item (first items)) (rest-items (rest items)))
    (let ((function-axes) (fn-element) (function-props) (is-function) (value) (value-props)
	  (prior-items) (items tokens)
	  (preceding-type (getf (first pre-properties) :type))
	  (preceding-special-props (getf (first pre-properties) :special)))
      (if (and (eq :array (first preceding-type))
	       (not (member :operation (getf preceding-special-props :omit))))
	  ;; the value match is canceled when encountering a pivotal operator composition on the left side
	  ;; of the function element so that expressions like ÷.5 ⊢10 20 30 work properly
	  (progn (assign-subprocessed fn-element function-props
				      '(:special (:omit (:function-assignment :value-assignment-by-selection))))
		 (setq is-function (eq :function (first (getf function-props :type)))
		       prior-items items)
		 (if is-function
		     (assign-subprocessed value value-props
					  '(:special (:omit (:value-assignment :function-assignment
							     :value-assignment-by-selection :branch
		 					     :operation :operator-assignment))
					    :valence :lateral)))
		 (if (not (eq :array (first (getf value-props :type))))
		     (setq items prior-items value nil))
		 (if (and (not function-axes) (member :axes function-props))
		     (setq function-axes (getf function-props :axes)))
		 (let ((output (if is-function
				   (let* ((fn-content (if (or (functionp fn-element)
							      (member fn-element '(⍺⍺ ⍵⍵))
							      (and (listp fn-element)
								   (eql 'function (first fn-element))))
							  fn-element (or (resolve-function
									  (if value :dyadic :monadic)
									  (insym fn-element))
									 (resolve-function
									  :symbolic fn-element))))
					  (fn-content (if (not (eq :self-reference fn-content))
							  fn-content '#'∇self))
					  (fn-sym (or-functional-character fn-element :fn)))
				     `(apl-call ,fn-sym ,fn-content ,precedent ,@(if value (list value))
						,@(if function-axes `((list ,@(first function-axes)))))))))
		   (if output (values output '(:type (:array :evaluated)) items)
		       (values nil nil tokens))))
	  (values nil nil tokens)))))

(defvar *composer-following-patterns-new*)

(setq *composer-following-patterns-new*
      '((:name :value-assignment-by-function-result :function value-assignment-by-function-result)
	(:name :value-assignment-by-selection :function value-assignment-by-selection)
	(:name :value-assignment-standard :function value-assignment-standard)
	(:name :function-assignment :function function-assignment)
	(:name :operator-assignment :function operator-assignment)
	(:name :branch :function branch)
	(:name :train-composition :function train-composition)
	(:name :pivotal-or-lateral-inline-composition :function pivotal-or-lateral-inline-composition)
	(:name :operation :function operation)))

;; (set-composer-patterns
;;  composer-following-patterns-apl-standard
;;  (with :idiom-symbol idiom :space-symbol space :process-symbol process
;;        :properties-symbol properties :precedent-symbol precedent :pre-properties-symbol pre-properties)
;;  (value-assignment-by-function-result
;;   ;; match the assignment of a function result to a value, like a+←5
;;   ((:with-preceding-type :array)
;;    (assignment-operator :element (function :glyph ←))
;;    (fn-element :pattern (:type (:function) :special '(:omit (:value-assignment :function-assignment))))
;;    (symbol :element (array :symbol-overriding t)))
;;   (if (is-workspace-value symbol)
;;       (let ((fn-content (resolve-function :dyadic fn-element))
;; 	    (fn-sym (or-functional-character fn-element :fn))
;; 	    (symbol-axes (getf (third properties) :axes))
;; 	    (function-axes (getf (first properties) :axes)))
;; 	(if (not symbol-axes)
;; 	    `(setq (inws ,symbol) (apl-call ,fn-sym ,fn-content (inws ,symbol) ,precedent
;; 					    ,@(if function-axes `((list ,@(first function-axes))))))
;; 	    (enclose-axes `(inws, symbol)
;; 			  symbol-axes :set-by `(lambda (item item2) (apl-call ,fn-sym ,fn-content item item2))
;; 			  :set precedent))))
;;   '(:type (:array :assigned :by-result-assignment-operator)))
;;  (value-assignment-by-selection
;;   ;; match a selective value assignment like (3↑x)←5
;;   ((:with-preceding-type :array)
;;    (assignment-function :element (function :glyph ←))
;;    (selection-form :pattern (:type (:array) :special '(:omit (:value-assignment)))))
;;   (if (and (listp selection-form) (eql 'apl-call (first selection-form)))
;;       (multiple-value-bind (sel-form sel-item placeholder set-form)
;; 	  (generate-selection-form selection-form space)
;; 	;; (print (list :sel sel-form selection-form))
;; 	(if sel-form
;; 	    ;; generate an array whose each cell is its row-major index, perform the subtractive function on
;; 	    ;; it and then use assign-selected to assign new values to the cells at the remaining indices
;; 	    ;; of the original array
;; 	    (if sel-item
;; 		(let ((item (gensym)) (indices (gensym)))
;; 		  ;; `(let* ((,item ,sel-item)
;; 		  ;; 	  (,placeholder (generate-index-array ,item))
;; 		  ;; 	  (,indices (enclose-atom ,sel-form))
;; 		  ;; 	  ,@(if set-form `((,placeholder (make-array nil :initial-element
;; 		  ;; 						     (assign-selected (disclose2 ,item)
;; 		  ;; 								      ,indices ,precedent))))))
;; 		  ;;    ,(or set-form `(setq ,sel-item (assign-selected ,sel-item ,indices ,precedent))))
;; 		  `(apl-assign
;; 		    ,sel-item
;; 		    (let* ((,item ,sel-item)
;; 			   (,placeholder (generate-index-array ,item))
;; 			   (,indices (enclose-atom ,sel-form))
;; 			   ,@(if set-form `((,placeholder (make-array nil :initial-element
;; 								      (assign-selected (disclose2 ,item)
;; 										       ,indices ,precedent))))))
;; 		      ,(or set-form `(assign-selected ,sel-item ,indices ,precedent))))
;; 		  )
;; 		`(let ((,placeholder ,precedent))
;; 		   (setf ,set-form ,sel-form))))))
;;   '(:type (:array :assigned)))
;;  (value-assignment
;;   ;; match a value assignment like a←1 2 3, part of an array expression
;;   ((:with-preceding-type :array)
;;    (assignment-function :element (function :glyph ←))
;;    (symbol :element (array :symbol-overriding t)))
;;   (let ((axes (getf (second properties) :axes)))
;;     ;; ensure symbol(s) are not bound to function values in the workspace, and
;;     ;; define them as dynamic variables if they're unbound there
;;     (loop :for symbol :in (if (not (listp symbol)) (list symbol)
;; 			      ;; remove symbols from (inws) unless they're bare and thus idiom-native
;; 			      (mapcar (lambda (s) (if (not (listp s)) s (second s))) symbol))
;;        :do (if (is-workspace-function symbol)
;; 	       (fmakunbound (intern (string symbol) space)))
;; 	 (if (not (boundp (intern (string symbol) space)))
;; 	     (progn (proclaim (list 'special (intern (string symbol) space)))
;; 		    (set (intern (string symbol) space) nil))))
;;     (cond ((eql 'to-output symbol)
;; 	   ;; a special case to handle ⎕← quad output
;; 	   `(apl-output ,precedent :print-precision print-precision
;; 			:print-to output-stream :print-assignment t))
;; 	  ((eql 'output-stream symbol)
;; 	   ;; a special case to handle ⎕ost← setting the output stream; the provided string
;; 	   ;; is interned in the current working package
;; 	   (if (stringp precedent)
;; 	       ;; setq is used instead of apl-assign because output-stream is a lexical variable
;; 	       `(setq output-stream ,(intern precedent (package-name *package*)))
;; 	       (if (listp precedent)
;; 		   (destructuring-bind (vector-symbol package-string symbol-string) precedent
;; 		     (if (and (eql 'avector vector-symbol)
;; 			      (stringp package-string)
;; 			      (stringp symbol-string))
;; 			 ;; if the argument is a vector of two strings like ('APRIL' 'OUT-STR'),
;; 			 ;; intern the symbol like (intern "OUT-STR" "APRIL")
;; 			 `(setq output-stream ,(intern symbol-string package-string))
;; 			 (error "Invalid assignment to ⎕OST.")))
;; 		   (error "Invalid assignment to ⎕OST."))))
;; 	  (t (if (symbolp symbol)
;; 		 (set-workspace-alias space symbol nil))
;; 	     (let ((symbol (if (or (listp symbol) (member symbol *idiom-native-symbols*))
;; 			       symbol (list 'inws symbol))))
;; 	       (if axes (enclose-axes symbol axes :set precedent)
;; 		   ;; enclose the symbol in (inws) so the (with-april-workspace) macro will corretly
;; 		   ;; intern it, unless it's one of the system variables
;; 		   `(apl-assign ,symbol ,precedent))))))
;;   '(:type (:array :assigned)))
;;  (function-assignment
;;   ;; match a function assignment like f←{⍵×2}, part of a functional expression
;;   ((:with-preceding-type :function)
;;    (assignment-function :element (function :glyph ←))
;;    (symbol :element (array :symbol-overriding t)))
;;   (let* ((inverted (if (listp precedent) (invert-function precedent)))
;; 	 (inverted-symbol (if inverted (intern (concatenate 'string "𝕚∇" (string symbol))))))
;;     (if (is-workspace-value symbol)
;; 	(makunbound (intern (string symbol) space)))
;;     (setf (symbol-function (intern (string symbol) space)) #'dummy-nargument-function)
;;     (if inverted (progn (if (is-workspace-value inverted-symbol)
;; 			    (makunbound (intern (string inverted-symbol) space)))
;; 			(setf (symbol-function (intern (string inverted-symbol) space))
;; 			      #'dummy-nargument-function)))
;;     (if (characterp precedent)
;; 	(if (or (resolve-function :monadic precedent)
;; 		(resolve-function :dyadic precedent))
;; 	    (progn (set-workspace-alias space symbol precedent)
;; 		   (format nil "~a aliases ~a" symbol precedent)))
;; 	(progn (set-workspace-alias space symbol nil)
;; 	       `(setf (symbol-function (quote (inws ,symbol))) ,precedent
;; 		      ,@(if inverted `((symbol-function (quote (inws ,inverted-symbol)))
;; 				       ,inverted))))))
;;   '(:type (:function :assigned)))
;;  (operator-assignment
;;   ;; match an operator assignment like f←{⍵×2}, part of a functional expression
;;   ((:with-preceding-type :operator)
;;    (assignment-function :element (function :glyph ←))
;;    (symbol :element (array :symbol-overriding t)))
;;   (let* ((operator-type (getf (first pre-properties) :type))
;; 	 (operator-symbol (intern (concatenate 'string (if (member :pivotal operator-type) "𝕆ℙ∇" "𝕆𝕃∇")
;; 					       (string symbol)))))
;;     (if (is-workspace-value symbol)
;; 	(makunbound (intern (string symbol) space)))
;;     (setf (symbol-function (intern (string operator-symbol) space)) #'dummy-nargument-function)
;;     (if (characterp precedent)
;; 	(if (or (resolve-operator :lateral precedent)
;; 		(resolve-operator :pivotal precedent))
;; 	    (progn (set-workspace-alias space symbol precedent)
;; 		   (format nil "~a aliases ~a" symbol precedent)))
;; 	(progn (set-workspace-alias space symbol nil)
;; 	       `(setf (symbol-function (quote (inws ,operator-symbol))) ,precedent))))
;;   '(:type (:operator :assigned)))
;;  (branch
;;   ;; match a branch-to statement like →1 or a branch point statement like 1→⎕
;;   ((:with-preceding-type :array)
;;    (branch-glyph :element (function :glyph →))
;;    (branch-from :element (array :cancel-if :pivotal-composition) :optional t :times :any)
;;    (determine-branch-by :element function :optional t :times 1))
;;   (progn
;;     (if (listp precedent)
;; 	(if (loop :for item :in precedent :always (and (listp item) (eql 'inws (first item))))
;; 	    (setq precedent (mapcar #'second precedent))
;; 	    (if (eql 'inws (first precedent))
;; 		(setq precedent (second precedent)))))
;;     (if (and branch-from (eql 'to-output precedent)) ;;(string= "TO-OUTPUT" (string precedent)))
;; 	;; if this is a branch point statement like X→⎕, do the following:
;; 	(if (integerp branch-from)
;; 	    ;; if the branch is designated by an integer like 5→⎕
;; 	    (let ((branch-symbol (gensym "AB"))) ;; AB for APL Branch
;; 	      (setf *branches* (cons (list branch-from branch-symbol) *branches*))
;; 	      branch-symbol)
;; 	    ;; if the branch is designated by a symbol like doSomething→⎕
;; 	    (if (symbolp branch-from)
;; 		(progn (setf *branches* (cons branch-from *branches*))
;; 		       branch-from)
;; 		(error "Invalid left argument to →; must be a single integer value or a symbol.")))
;; 	;; otherwise, this is a branch-to statement like →5 or →doSomething
;; 	(if (or (integerp precedent)
;; 		(symbolp precedent))
;; 	    ;; if the target is an explicit symbol as in →mySymbol, or explicit index
;; 	    ;; as in →3, just pass the symbol through
;; 	    (list 'go precedent)
;; 	    (if (loop :for item :in (rest precedent) :always (symbolp item)) ;;(symbolp (second item)))
;; 		;; if the target is one of an array of possible destination symbols...
;; 		(if (integerp branch-from)
;; 		    ;; if there is an explicit index to the left of the arrow, grab the corresponding
;; 		    ;; symbol unless the index is outside the array's scope, in which case a (list) is returned
;; 		    ;; so nothing is done
;; 		    (if (< 0 branch-from (length (rest precedent)))
;; 			(list 'go (second (nth (1- branch-from) (rest precedent))))
;; 			(list 'list))
;; 		    ;; otherwise, there must be an expression to the left of the arrow, as with
;; 		    ;; (3-2)→tagOne tagTwo, so pass it through for the postprocessor
;; 		    (list 'go precedent branch-from))
;; 		(list 'go precedent)))))
;;   '(:type (:branch)))
;;  (train-composition
;;   ;; match a train function composition like (-,÷)
;;   ((:with-preceding-type :function)
;;    (center :pattern (:type (:function) :special '(:omit (:value-assignment :function-assignment))))
;;    (left :pattern (:special '(:omit (:value-assignment :function-assignment)))))
;;   (destructuring-bind (right omega alpha center)
;;       (list precedent (gensym) (gensym)
;;  	    (if (listp center)
;;  		center (resolve-function :dyadic (if (not (symbolp center))
;;  						     center (intern (string center) space)))))
;;     ;; train composition is only valid when there is only one function in the precedent
;;     ;; or when continuing a train composition as for (×,-,÷)5
;;     (if (and center (or (= 1 (length pre-properties))
;;  			(and (member :train-composition (getf (first pre-properties) :type))
;;  			     (not (member :closed (getf (first pre-properties) :type))))))
;;  	;; functions are resolved here, failure to resolve indicates a value in the train
;;  	(let ((right-fn-monadic (if (and (listp right) (eql 'function (first right)))
;;  				    right (resolve-function :monadic right)))
;;  	      (right-fn-dyadic (if (and (listp right) (eql 'function (first right)))
;;  				   right (resolve-function :dyadic right)))
;;  	      (left-fn-monadic (if (and (listp left) (eql 'function (first left)))
;;  				   left (resolve-function :monadic left)))
;;  	      (left-fn-dyadic (if (and (listp left) (eql 'function (first left)))
;;  				  left (resolve-function :dyadic left))))
;;  	  `(lambda (,omega &optional ,alpha)
;;  	     (if ,alpha (apl-call ,(or-functional-character center :fn) ,center
;;  				  (apl-call ,(or-functional-character right :fn)
;;  					    ,right-fn-dyadic ,omega ,alpha)
;;  				  ,(if (not left-fn-dyadic)
;;  				       left `(apl-call ,(or-functional-character left :fn)
;;  						       ,left-fn-dyadic ,omega ,alpha)))
;;  		 (apl-call ,(or-functional-character center :fn) ,center
;;  			   (apl-call ,(or-functional-character right :fn) ,right-fn-monadic ,omega)
;;  			   ,(if (not left-fn-monadic)
;;  				left `(apl-call ,(or-functional-character left :fn)
;;  						,left-fn-monadic ,omega))))))))
;;   (list :type (list :function :train-composition (if (resolve-function :monadic left) :open :closed))))
;;  (pivotal-or-lateral-inline-composition
;;   ;; match a pivotal function composition like ×.+, part of a functional expression
;;   ;; it may come after either a function or an array, since some operators take array operands
;;   ((operator :element (operator :valence :pivotal))
;;    (left-operand :pattern (:special '(:omit (:value-assignment :function-assignment :operation)))))
;;   ;; the special :omit property makes it so that the pattern matching the operand may not be processed as
;;   ;; a value assignment, function assignment or operation, which allows for expressions like
;;   ;; fn←5∘- where an operator-composed function is assigned
;;   (let ((right-operand (insym precedent))
;; 	(right-operand-axes (first (getf (first pre-properties) :axes)))
;; 	(left-operand (insym left-operand))
;; 	(left-operand-axes (first (getf (second properties) :axes)))
;; 	(omega (gensym)) (alpha (gensym)))
;;     ;; get left axes from the left operand and right axes from the precedent's properties so the
;;     ;; functions can be properly curried if they have axes specified
;;     ;; (append (list 'apl-compose (intern (string-upcase operator)))
;;     ;; 	    (funcall (funcall (resolve-operator :pivotal operator)
;;     ;; 			      left-operand left-operand-axes right-operand right-operand-axes)
;;     ;; 		     right-operand left-operand)))
;;     ;; (cons 'apl-compose
;; 	  ;; call the operator constructor on the output of the operand constructor which integrates axes
;;     (if (symbolp operator) `(apl-compose :op ,(list 'inws operator)
;; 					 ,(if (listp left-operand)
;; 					      left-operand (if (characterp left-operand)
;; 							       `(lambda (,omega &optional ,alpha)
;; 								  (if ,alpha
;; 								      (apl-call :fn ,(resolve-function
;; 										      :dyadic left-operand)
;; 										,omega ,alpha)
;; 								      (apl-call :fn ,(resolve-function
;; 										      :monadic left-operand)
;; 										,omega)))))
;; 					 ,(if (listp right-operand)
;; 					      left-operand (if (characterp right-operand)
;; 							       `(lambda (,omega &optional ,alpha)
;; 								  (if ,alpha
;; 								      (apl-call :fn ,(resolve-function
;; 										      :dyadic right-operand)
;; 										,omega ,alpha)
;; 								      (apl-call :fn ,(resolve-function
;; 										      :monadic right-operand)
;; 										,omega)))))
;; 				       ;; TODO: implement operand axes
;; 				       ;; operand-axes
;; 				       )
;; 	      (if (listp operator)
;; 		  `(apl-call :fn (apl-compose :op ,operator
;; 					      ,(if (listp left-operand)
;; 						   left-operand (if (characterp left-operand)
;; 								    `(lambda (,omega &optional ,alpha)
;; 								       (if ,alpha
;; 									   (apl-call :fn ,(resolve-function
;; 											  :dyadic left-operand)
;; 										     ,omega ,alpha)
;; 									   (apl-call :fn ,(resolve-function
;; 											   :monadic left-operand)
;; 										     ,omega))))))
;; 			     ,precedent)
;; 		  (cons 'apl-compose
;; 			(cons (intern (string-upcase operator))
;; 			      (funcall (funcall (resolve-operator :pivotal operator)
;; 						left-operand left-operand-axes right-operand right-operand-axes)
;; 				       right-operand left-operand))))))
;;   '(:type (:function :operator-composed :pivotal)))
;;  (operation
;;   ;; match an operation on arrays like 1+1 2 3, ⍳9 or +/⍳5, these operations are the basis of APL
;;   ((:with-preceding-type :array)
;;    (fn-element :pattern (:type (:function) :special '(:omit (:value-assignment :function-assignment))))
;;    ;; the value match is canceled when encountering a pivotal operator composition on the left side
;;    ;; of the function element so that expressions like ÷.5 ⊢10 20 30 work properly
;;    (value :element (array :cancel-if :pivotal-composition) :optional t :times :any))
;;   (let ((fn-content (if (or (functionp fn-element)
;; 			    (member fn-element '(⍺⍺ ⍵⍵))
;; 			    (and (listp fn-element)
;; 				 (eql 'function (first fn-element))))
;; 			fn-element (resolve-function (if value :dyadic :monadic) (insym fn-element))))
;; 	(fn-sym (or-functional-character fn-element :fn))
;; 	(axes (getf (first properties) :axes)))
;;     ;; (print (list :val value))
;;     `(apl-call ,fn-sym ,fn-content ,precedent
;; 	       ,@(if value (list (output-value space value (rest properties))))
;; 	       ,@(if axes `((list ,@(first axes))))))
;;   '(:type (:array :evaluated))))



#|
(vex::composer-pattern-template 
 composer-pattern tokens space idiom process precedent properties preceding-properties items item rest-items)



(composer-pattern
 composer-pattern-function (axes function-form function-props)
 ((assign-axes axes process item items rest-items)
  (assign-element function-form function-props process-function process
		  nil space item items rest-items))
 (let ((is-function (or (not (member :overloaded-operator (getf function-props :type)))
			   (let ((next (if items (multiple-value-list (funcall process items)))))
			     (not (member :function (getf (second next) :type)))))))
      (if (and function-form is-function)
	  (values (if (or (not axes) (of-lexicon idiom :functions function-form))
		      (if (not (and (symbolp function-form) (is-workspace-function function-form)))
			  function-form `(function (inws ,function-form)))
		      `(apl-call :nafn (function ,(insym function-form)) ,@(first axes)))
		  (list :type (if (member :operator (getf function-props :type))
				  (list :operator :inline-operator
					(if (member :pivotal (getf function-props :type))
					    :pivotal :lateral))
				  '(:function :inline-function))
			:axes axes)
		  items))))

|#
