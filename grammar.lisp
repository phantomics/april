 ;;;; grammar.lisp

(in-package #:april)

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
		;; process the empty array conveyed by the [⍬ zilde] character
		((eq :empty-array this-item)
		 (values (make-array (list 0))
			 (list :type (list :array :empty))
			 (rest tokens)))
		;; process numerical values
		((and (numberp this-item)
		      (or (not (getf properties :type))
			  (eq :number (first (getf properties :type)))))
		 (if axes (error "Axes cannot be applied to numbers.")
		     (values this-item (list :type (list :array :number))
			     (rest tokens))))
		;; process string values
		((and (stringp this-item)
		      (or (not (getf properties :type))
			  (eq :string (first (getf properties :type)))))
		 (values this-item (list :axes axes :type (list :array :string))
			 remaining))
		;; process symbol-referenced values
		((and (symbolp this-item)
		      (or (eql '⍵ this-item)
			  (eql '⍺ this-item)
			  (getf properties :symbol-overriding)
			  (gethash this-item (gethash :values workspace))
			  (not (gethash this-item (gethash :functions workspace))))
		      (or (not (getf properties :type))
			  (eq :symbol (first (getf properties :type)))))
		 (values this-item (list :axes axes :type (list :symbol))
			 remaining))
		;; if the pattern is set to cancel upon encountering a pivotal operator, it will do so and throw
		;; the appropriate cancellation flag
		((and (getf properties :cancel-if)
		      (eq :pivotal-composition (getf properties :cancel-if))
		      (listp this-item)
		      (eq :op (first this-item))
		      (eq :pivotal (second this-item)))
		 (values nil (list :cancel-flag :pivotal-composition)
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
					   (gethash (first remaining) (gethash :variables workspace))))
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
			      (values fn (list :axes axes :type (list :function :glyph))
				      remaining))
			     ((and (listp fn)
				   (not (getf properties :glyph)))
			      (if axes (error "Axes can only be used with functions represented by symbols.")
				  (values (output-function (funcall process (first fn)))
					  (list :type (list :function :closure)
						:obligate-dyadic obligate-dyadic)
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
			    (gethash fn (gethash :functions workspace))
			    (not (gethash fn (gethash :values workspace))))
		       (values fn (list :type (list :function :referenced))
			       remaining)
		       (values nil nil tokens))))))
 ;; match a reference to an operator, this must be a lexical reference like ⍣
 (operator (multiple-value-bind (axes this-item remaining)
	       (extract-axes process tokens)
	     ;; (print (list :xx axes this-item remaining))
	     (if (and (listp this-item)
		      (eq :op (first this-item)))
		 ;; process an operator token, allowing specification of the valence, either :monadic or :dyadic
		 (destructuring-bind (op-type op-symbol)
		     (rest this-item)
		   (let ((valid-by-valence (or (not (getf properties :valence))
					       (eq op-type (getf properties :valence)))))
		     (cond ((and valid-by-valence (getf properties :glyph))
			    (if (char= op-symbol (aref (string (getf properties :glyph)) 0))
				(values op-symbol (list :axes axes :type (list :operator op-type))
					remaining)
				(values nil nil tokens)))
			   (valid-by-valence (values op-symbol (list :axes axes :type (list :operator op-type)) 
						     remaining))
			   (t (values nil nil tokens)))))))))

(set-composer-patterns
 composer-opening-patterns-apl-standard
 (with :idiom-symbol idiom :space-symbol workspace :process-symbol process :properties-symbol properties)
 (value
  ;; match an array like 1 2 3, marking the beginning of an array expression
  ;; ...or a functional expression if the array is an operand to a pivotal operator
  ((value :element array :times :any))
  (output-value workspace value properties)
  (list :type (list :array :explicit)))
 (function
  ;; match a function like × or {⍵+10}, marking the beginning of a functional expression
  ((function-element :element function :times 1))
  function-element
  (list :type (list :function :symbol-function)
	:axes (getf (first properties) :axes)))
 (lateral-composition
  ;; match a lateral function composition like +/, marking the beginning of a functional expression
  ((operator :element (operator :valence :lateral))
   (operand :pattern (:type (:function)
		      :special (list :omit (list :value-assignment :function-assignment :operation)))))
  (let ((axes (first (getf (first properties) :axes))))
    (funcall (resolve-operator :lateral operator)
	     operand workspace axes))
  (list :type (list :function :operator-composed :lateral))))

(set-composer-patterns
 composer-following-patterns-apl-standard
 (with :idiom-symbol idiom :space-symbol workspace :process-symbol process
       :properties-symbol properties :precedent-symbol precedent)
 (evaluation-of-character-array
  ;; match the use of the code string evaluation function ⍎, evaluating the code with access to
  ;; the local workspace as cannot be done through a normal function
  ((:with-preceding-type :array)
   (evaluate-function :element (function :glyph ⍎)))
  (let ((o (gensym)))
    `(funcall (lambda (,o) (eval (vex-program this-idiom (list (list :space ,workspace)
							       (list :state :print-output nil))
					      ,o)))
	      ,precedent))
  (list :type (list :array :result-of-evaluated-string)))
 (value-assignment-by-function-result
  ;; match the assignment of a function result to a value, like a+←5
  ((:with-preceding-type :array)
   (assignment-operator :element (function :glyph ←))
   (fn-element :pattern (:type (:function)))
   (symbol :element (array :symbol-overriding t)))
  (if (gethash symbol (gethash :values workspace))
      (let ((fn-content (resolve-function :dyadic fn-element))
	    (fn-sym (or-functional-character fn-element :fn))
	    (symbol-axes (getf (third properties) :axes))
	    (function-axes (getf (first properties) :axes)))
	(if (not symbol-axes)
	    `(setq ,symbol (apl-call ,fn-sym ,fn-content ,symbol ,precedent
				     ,@(if function-axes `((list ,@(first function-axes))))))
	    (enclose-axes symbol symbol-axes :set `(lambda (item)
						     (apl-call ,fn-sym ,fn-content item ,precedent))))))
  (list :type (list :array :assigned :by-result-assignment-operator)))
 (value-assignment
  ;; match a value assignment like a←1 2 3, part of an array expression
  ((:with-preceding-type :array)
   (assignment-function :element (function :glyph ←))
   (symbol :element (array :symbol-overriding t)))
  (let ((axes (getf (second properties) :axes)))
    (setf (gethash symbol (gethash :values workspace))
	  precedent)
    (if (gethash symbol (gethash :functions workspace))
	(setf (gethash symbol (gethash :functions workspace))
	      nil))
    (cond ((eql 'to-output symbol)
	   ;; a special case to handle ⎕← quad output
	   `(apl-output ,precedent :print-precision print-precision :print-to output-stream))
	  ((eql 'output-stream symbol)
	   ;; a special case to handle ⎕ost← setting the output stream; the provided string
	   ;; is interned in the current working package
	   `(apl-assign output-stream ,(intern precedent (package-name *package*))))
	  (t (if axes (enclose-axes symbol axes :set `(disclose ,precedent))
		 `(apl-assign ,symbol ,precedent)))))
  (list :type (list :array :assigned)))
 (function-assignment
  ;; match a function assignment like f←{⍵×2}, part of a functional expression
  ((:with-preceding-type :function)
   (assignment-function :element (function :glyph ←))
   (symbol :element (array :symbol-overriding t)))
  (progn (setf (gethash symbol (gethash :functions workspace))
	       precedent)
	 (if (gethash symbol (gethash :values workspace))
	     (setf (gethash symbol (gethash :values workspace))
		   nil))
	 `(setq ,symbol ,precedent))
  (list :type (list :function :assigned)))
 (pivotal-composition
  ;; match a pivotal function composition like ×.+, part of a functional expression
  ;; it may come after either a function or an array, since some operators take array operands
  ((operator :element (operator :valence :pivotal))
   (operand :pattern (:special (list :omit (list :value-assignment :function-assignment :operation)))))
  ;; the special :omit property makes it so that the pattern matching the operand may not be processed as
  ;; a value assignment, function assignment or operation, which allows for expressions like
  ;; fn←5∘- where an operator-composed function is assigned
  (funcall (resolve-operator :pivotal operator)
	   precedent operand workspace)
  (list :type (list :function :operator-composed :pivotal)))
 (operation
  ;; match an operation on arrays like 1+1 2 3, ⍳9 or +/⍳5, these operations are the basis of APL
  ((:with-preceding-type :array)
   (fn-element :pattern (:type (:function) :special (list :omit (list :value-assignment :function-assignment))))
   ;; the value match is canceled when encountering a pivotal operator composition on the left side
   ;; of the function element so that expressions like ÷.5 ⊢10 20 30 work properly
   (value :element (array :cancel-if :pivotal-composition) :optional t :times :any))
  (let ((fn-content (resolve-function (if value :dyadic :monadic) fn-element))
	(fn-sym (or-functional-character fn-element :fn))
	(axes (getf (first properties) :axes)))
    `(apl-call ,fn-sym ,fn-content ,precedent ,@(if value (list (output-value workspace value (rest properties))))
	       ,@(if axes `((list ,@(first axes))))))
  (list :type (list :array :evaluated))))
