;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; grammar.lisp

(in-package #:april)

"This file contains the specification of April's basic grammar elements, including the basic language components - array, function and operator - and the patterns comprising those elements that make up the language's strucures."

(define-symbol-macro branches (symbol-value (intern "*BRANCHES*" workspace)))

(defmacro is-workspace-value (item)
  `(and (boundp (intern (string ,item) workspace))
	(not (fboundp (intern (string ,item) workspace)))))

(defmacro is-workspace-function (item)
  `(fboundp (intern (string ,item) workspace)))

(set-composer-elements
 composer-elements-apl-standard
 (with :tokens-symbol tokens :idiom-symbol idiom :space-symbol workspace
       :properties-symbol properties :processor-symbol process)
 ;; match an array, either inline like "1 2 3", referenced by a variable, or contained within a (closure)
 (array (multiple-value-bind (axes this-item remaining)
	    (extract-axes process tokens)
	  (cond ((and (listp this-item)
		      (not (or (eq :fn (first this-item))
			       (eq :op (first this-item)))))
		 ;; if the item is a closure, evaluate it and return the result
		 (multiple-value-bind (output out-properties)
		     (funcall process this-item)
		   (if (eq :array (first (getf out-properties :type)))
		       (progn (setf (getf out-properties :type)
				    (cons (first (getf out-properties :type))
					  (cons :enclosed (rest (getf out-properties :type))))
				    (getf out-properties :axes) axes)
			      (values output out-properties remaining))
		       (values nil nil tokens))))
		((and (listp this-item)
		      (eq :op (first this-item))
		      (eq :unitary (second this-item)))
		 ;; if the item is a unitary operator, compose it, skipping the branches other than for
		 ;; unitary operators, the only one that must be skipped is :value since it causes an infinite loop
		 (multiple-value-bind (output out-properties)
		     (funcall process (list (first tokens) (second tokens))
			      '(:special (:omit (:value :function :lateral-composition))))
		   (if (eq :array (first (getf out-properties :type)))
		       (progn (values output out-properties (cddr tokens))))))
		;; process the empty array conveyed by the [⍬ zilde] character
		((eq :empty-array this-item)
		 (values (make-array nil)
			 (list :type '(:array :empty))
			 (rest tokens)))
		;; process numerical values
		((and (numberp this-item)
		      (or (not (getf properties :type))
			  (eq :number (first (getf properties :type)))))
		 (if axes (error "Axes cannot be applied to numbers.")
		     (values this-item '(:type (:array :number))
			     (rest tokens))))
		;; process string values
		((and (stringp this-item)
		      (or (not (getf properties :type))
			  (eq :string (first (getf properties :type)))))
		 (values this-item (list :axes axes :type '(:array :string))
			 remaining))
		;; process symbol-referenced values
		((and (symbolp this-item)
		      (or (eql '⍵ this-item)
			  (eql '⍺ this-item)
			  (getf properties :symbol-overriding)
			  ;; (gethash this-item (gethash :values workspace))
			  ;; (not (gethash this-item (gethash :functions workspace)))
			  (is-workspace-value this-item)
			  )
		      (or (not (getf properties :type))
			  (eq :symbol (first (getf properties :type)))))
		 (print (list :it this-item))
		 (values this-item (list :axes axes :type '(:symbol))
			 remaining))
		;; if the pattern is set to cancel upon encountering a pivotal operator, it will do so and throw
		;; the appropriate cancellation flag
		((and (getf properties :cancel-if)
		      (eq :pivotal-composition (getf properties :cancel-if))
		      (listp this-item)
		      (eq :op (first this-item))
		      (eq :pivotal (second this-item)))
		 (values nil '(:cancel-flag :pivotal-composition)
			 tokens))
		(t (values nil nil tokens)))))
 ;; match a function, whether lexical like ⍳, symbolic like fn, or inline like {⍵+5}
 (function (multiple-value-bind (axes this-item remaining)
	       (extract-axes process tokens)
	     (if (listp this-item)
		 ;; process a function specification starting with :fn
		 (if (or (eq :fn (first this-item))
			 ;; if marked as an operator, check whether the character is one entered as both
			 ;; a function and an operator; such functions must be dyadic
			 (and (eq :op (first this-item))
			      (or (vex::of-lexicon idiom :dyadic-functions (third this-item))
				  (vex::of-lexicon idiom :symbolic-functions (third this-item)))
			      ;; check that the following item is a value and not a function, otherwise it
			      ;; must be an operator
			      (or (and (not (listp (first remaining)))
				       (or (not (symbolp (first remaining)))
					   ;; (gethash (first remaining) (gethash :variables workspace))
					   ;; (and (boundp (intern (first remaining) workspace))
					   ;; 	(not (fboundp (intern (first remaining) workspace))))
					   (is-workspace-value (first remaining))))
				  ;; this clause is needed in case of an index-referenced value being passed
				  ;; as the function's left value, i.e. v←⍳5 ⋄ v[4]/7 8
				  (and (listp (first remaining))
				       (eq :axes (caar remaining))
				       (symbolp (second remaining)))
				  ;; this clause is needed to test for closures returning a value
				  ;; to the left of the overloaded glyph as in (1⌷3 4)/5
				  (multiple-value-bind (output out-properties)
				      (funcall process (list (first remaining)))
				    (declare (ignore output))
				    (eq :array (first (getf out-properties :type)))))))
		     (let ((fn (first (last this-item)))
			   (obligate-dyadic (and (eq :op (first this-item))
						 (vex::of-lexicon idiom :dyadic-functions (third this-item)))))
		       (cond ((and (characterp fn)
				   (or (not (getf properties :glyph))
				       (and (char= fn (aref (string (getf properties :glyph)) 0)))))
			      (values fn (list :axes axes :type '(:function :glyph))
				      remaining))
			     ((and (listp fn)
				   (not (getf properties :glyph)))
			      (let* ((polyadic-args (if (and (listp (first (last (first fn))))
							     (eq :axes (caar (last (first fn)))))
							(mapcar #'caar (cdar (last (first fn))))))
				     (fn (if (not polyadic-args)
					     fn (cons (butlast (first fn) 1)
						      (rest fn)))))
				(values (output-function (if (= 1 (length fn))
							     (list (funcall process fn))
							     (mapcar process fn))
							 polyadic-args)
					(list :type '(:function :closure)
					      :axes axes :obligate-dyadic obligate-dyadic)
					remaining)))
			     (t (values nil nil tokens))))
		     ;; process sub-list in case it is a functional expression, but don't do this
		     ;; if looking for a specific functional glyph
		     (if (not (getf properties :glyph))
			 (multiple-value-bind (output out-properties)
			     (funcall process this-item)
			   (if (eq :function (first (getf out-properties :type)))
			       (progn (setf (getf out-properties :type)
					    (cons (first (getf out-properties :type))
						  (cons :enclosed (rest (getf out-properties :type)))))
				      (values output properties remaining))
			       (values nil nil tokens)))
			 (values nil nil tokens)))
		 (let ((fn this-item))
		   (if (and (symbolp fn)
			    (not (getf properties :glyph))
			    (is-workspace-value fn)
			    ;; (gethash fn (gethash :functions workspace))
			    ;; (not (gethash fn (gethash :values workspace)))
			    )
		       (values fn (list :axes axes :type '(:function :referenced))
			       remaining)
		       (values nil nil tokens))))))
 ;; match a reference to an operator, this must be a lexical reference like ⍣
 (operator (multiple-value-bind (axes this-item remaining)
	       (extract-axes process tokens)
	     (if (and (listp this-item)
		      (eq :op (first this-item)))
		 ;; process an operator token, allowing specification of the valence, either :lateral or :pivotal
		 (destructuring-bind (op-type op-symbol)
		     (rest this-item)
		   (let ((valid-by-valence (or (not (getf properties :valence))
					       (eq op-type (getf properties :valence)))))
		     (cond ((and valid-by-valence (getf properties :glyph))
			    (if (char= op-symbol (aref (string (getf properties :glyph)) 0))
				(values op-symbol (list :axes axes :type (list :operator op-type))
					remaining)
				(values nil nil tokens)))
			   (valid-by-valence
			    (values op-symbol (list :axes axes :type (list :operator op-type)) 
						     remaining))
			   (t (values nil nil tokens)))))))))

(set-composer-patterns
 composer-opening-patterns-apl-standard
 (with :idiom-symbol idiom :space-symbol workspace :process-symbol process
       :properties-symbol properties :pre-properties-symbol pre-properties)
 (value
  ;; match an array like 1 2 3, marking the beginning of an array expression
  ;; ...or a functional expression if the array is an operand to a pivotal operator
  ((value :element array :times :any))
  (output-value workspace value properties)
  '(:type (:array :explicit)))
 (function
  ;; match a function like × or {⍵+10}, marking the beginning of a functional expression
  ((function-element :element function :times 1))
  (let ((axes (getf (first properties) :axes)))
    (if (or (not axes) (vex::of-lexicon idiom :functions function-element))
	function-element `(apl-call :fn ,function-element ,@(first axes))))
  (list :type (if (and (getf (first properties) :axes)
		       (not (vex::of-lexicon idiom :functions function-element)))
		  '(:array :evaluated)
		  '(:function :symbol-function))
	:axes (getf (first properties) :axes)))
 (lateral-composition
  ;; match a lateral function composition like +/, marking the beginning of a functional expression
  ((operator :element (operator :valence :lateral))
   (operand :pattern (:type (:function)
		      :special '(:omit (:value-assignment :function-assignment :operation)))))
  (let ((operator-axes (first (getf (first properties) :axes)))
	(operand-axes (first (getf (second properties) :axes))))
    (append (list 'apl-compose (intern (string-upcase operator)))
	    ;; call the operator constructor on the output of the operand constructor which integrates axes
	    (funcall (funcall (resolve-operator :lateral operator)
			      workspace operand operand-axes)
		     operator-axes)))
  '(:type (:function :operator-composed :lateral)))
 (unitary-operator
  ((operator :element (operator :valence :unitary)))
  (let ((axes (first (getf (first properties) :axes))))
    (funcall (resolve-operator :unitary operator)
	     workspace axes))
  '(:type (:array :evaluated))))

(set-composer-patterns
 composer-following-patterns-apl-standard
 (with :idiom-symbol idiom :space-symbol workspace :process-symbol process
       :properties-symbol properties :precedent-symbol precedent :pre-properties-symbol pre-properties)
 (evaluation-of-character-array
  ;; match the use of the code string evaluation function ⍎, evaluating the code with access to
  ;; the local workspace as cannot be done through a normal function
  ((:with-preceding-type :array)
   (evaluate-function :element (function :glyph ⍎)))
  (let ((o (gensym)))
    `(funcall (lambda (,o) (eval (vex-program this-idiom (list (list :space ,workspace)
							       '(:state :print-output nil))
					      ,o)))
	      ,precedent))
  '(:type (:array :result-of-evaluated-string)))
 (value-assignment-by-function-result
  ;; match the assignment of a function result to a value, like a+←5
  ((:with-preceding-type :array)
   (assignment-operator :element (function :glyph ←))
   (fn-element :pattern (:type (:function)))
   (symbol :element (array :symbol-overriding t)))
  (if ;; (gethash symbol (gethash :values workspace))
      (is-workspace-value symbol)
      (let ((fn-content (resolve-function workspace :dyadic fn-element))
	    (fn-sym (or-functional-character fn-element :fn))
	    (symbol-axes (getf (third properties) :axes))
	    (function-axes (getf (first properties) :axes)))
	(if (not symbol-axes)
	    `(setq ,symbol (apl-call ,fn-sym ,fn-content ,symbol ,precedent
				     ,@(if function-axes `((list ,@(first function-axes))))))
	    (enclose-axes symbol symbol-axes :set `(lambda (item)
						     (apl-call ,fn-sym ,fn-content item ,precedent))))))
  '(:type (:array :assigned :by-result-assignment-operator)))
 (value-assignment
  ;; match a value assignment like a←1 2 3, part of an array expression
  ((:with-preceding-type :array)
   (assignment-function :element (function :glyph ←))
   (symbol :element (array :symbol-overriding t)))
  (let ((axes (getf (second properties) :axes)))
    ;; (if ;; (not (gethash symbol (gethash :values workspace)))
    ;; 	(not (is-workspace-value symbol))
    ;; 	(setf (gethash symbol (gethash :values workspace))
    ;; 	      t))
    ;; (if (gethash symbol (gethash :functions workspace))
    ;; 	(setf (gethash symbol (gethash :functions workspace))
    ;; 	      nil))
    (cond ((eql 'to-output symbol)
	   ;; a special case to handle ⎕← quad output
	   `(apl-output ,precedent :print-precision print-precision :print-to output-stream :print-assignment t))
	  ((eql 'output-stream symbol)
	   ;; a special case to handle ⎕ost← setting the output stream; the provided string
	   ;; is interned in the current working package
	   (if (stringp precedent)
	       `(apl-assign output-stream ,(intern precedent (package-name *package*)))
	       (if (listp precedent)
		   (destructuring-bind (vector-symbol package-string symbol-string)
		       precedent
		     (if (and (eql 'avector vector-symbol)
			      (stringp package-string)
			      (stringp symbol-string))
			 ;; if the argument is a vector of two strings like ('APRIL' 'OUT-STR'),
			 ;; intern the symbol like (intern "OUT-STR" "APRIL")
			 `(apl-assign output-stream ,(intern symbol-string package-string))
			 (error "Invalid assignment to ⎕OST.")))
		   (error "Invalid assignment to ⎕OST."))))
	  (t (if axes (enclose-axes symbol axes :set `(disclose ,precedent))
		 `(apl-assign ,symbol ,precedent)))))
  '(:type (:array :assigned)))
 (function-assignment
  ;; match a function assignment like f←{⍵×2}, part of a functional expression
  ((:with-preceding-type :function)
   (assignment-function :element (function :glyph ←))
   (symbol :element (array :symbol-overriding t)))
  (progn ;; (setf (gethash symbol (gethash :functions workspace))
	 ;;       precedent)
         (setf (symbol-function (intern (string symbol) workspace)) #'identity)
         `(setf (symbol-function (quote ,(intern (string symbol) workspace))) ,precedent)
	 ;; (if (gethash symbol (gethash :values workspace))
	 ;;     (setf (gethash symbol (gethash :values workspace))
	 ;; 	   nil))
	 ;; `(setq ,symbol ,precedent)
	 )
  '(:type (:function :assigned)))
 (branch
  ;; match a branch-to statement like →1 or a branch point statement like 1→⎕
  ((:with-preceding-type :array)
   (branch-glyph :element (function :glyph →))
   (branch-from :element (array :cancel-if :pivotal-composition) :optional t :times :any)
   (determine-branch-by :element function :optional t :times 1))
  (if (and branch-from ;; (listp precedent)
	   ;; (numberp (first precedent))
	   ;; (eql 'to-output (second precedent))
	   ;; (not (third precedent))
	   (eql 'to-output precedent))
      ;; if this is a branch point statement like X→⎕, do the following:
      (if (integerp branch-from)
	  ;; if the branch is designated by an integer like 5→⎕
	  (let ((branch-symbol (gensym "AB"))) ;; AB for APL Branch
	    (setf branches (cons (list branch-from branch-symbol) branches))
	    branch-symbol)
	  ;; if the branch is designated by a symbol like doSomething→⎕
	  (if (symbolp branch-from)
	      (progn (setf branches (cons branch-from branches))
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
		  (list 'go (rest precedent)
			branch-from))
	      (list 'go precedent))))
  `(type (:branch)))
 (pivotal-composition
  ;; match a pivotal function composition like ×.+, part of a functional expression
  ;; it may come after either a function or an array, since some operators take array operands
  ((operator :element (operator :valence :pivotal))
   (left-operand :pattern (:special '(:omit (:value-assignment :function-assignment :operation)))))
  ;; the special :omit property makes it so that the pattern matching the operand may not be processed as
  ;; a value assignment, function assignment or operation, which allows for expressions like
  ;; fn←5∘- where an operator-composed function is assigned
  (let ((right-operand precedent)
	(left-operand-axes (first (getf (second properties) :axes)))
	(right-operand-axes (first (getf pre-properties :axes))))
    ;; get left axes from the left operand and right axes from the precedent's properties so the
    ;; functions can be properly curried if they have axes specified
    (append (list 'apl-compose (intern (string-upcase operator)))
	    (funcall (funcall (resolve-operator :pivotal operator)
			      workspace left-operand left-operand-axes precedent right-operand-axes)
		     right-operand left-operand)))
  '(:type (:function :operator-composed :pivotal)))
 (operation
  ;; match an operation on arrays like 1+1 2 3, ⍳9 or +/⍳5, these operations are the basis of APL
  ((:with-preceding-type :array)
   (fn-element :pattern (:type (:function) :special '(:omit (:value-assignment :function-assignment))))
   ;; the value match is canceled when encountering a pivotal operator composition on the left side
   ;; of the function element so that expressions like ÷.5 ⊢10 20 30 work properly
   (value :element (array :cancel-if :pivotal-composition) :optional t :times :any))
  (let ((fn-content (resolve-function workspace (if value :dyadic :monadic) fn-element))
	(fn-sym (or-functional-character fn-element :fn))
	(axes (getf (first properties) :axes)))
    (print (list :ioio fn-content fn-sym axes))
    `(apl-call ,fn-sym ,fn-content ,precedent ,@(if value (list (output-value workspace value (rest properties))))
	       ,@(if axes `((list ,@(first axes))))))
  '(:type (:array :evaluated))))
