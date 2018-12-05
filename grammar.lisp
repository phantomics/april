 ;;;; grammar.lisp

(in-package #:april)

(set-composer-elements
 composer-elements-apl-standard
 (with :tokens-symbol tokens :idiom-symbol idiom :space-symbol space
       :properties-symbol properties :processor-symbol process)
 ;; match an array, either inline line "1 2 3", referenced by a variable, or contained within a (closure)
 (array (multiple-value-bind (axes this-item remaining)
	    (extract-axes process tokens)
	  ;; if the item is a closure, evaluate it and return the result
	  (cond ((and (listp this-item)
		      (not (or (eq :fn (first this-item))
			       (eq :op (first this-item)))))
		 (multiple-value-bind (output out-properties)
		     (funcall process this-item)
		   ;; (print (list :pro output out-properties))
		   (if (eq :array (first (getf out-properties :type)))
		       (progn (setf (getf out-properties :type)
				    (cons (first (getf out-properties :type))
					  (cons :enclosed (rest (getf out-properties :type))))
				    (getf out-properties :axes) axes)
			      (values output out-properties remaining))
		       (values nil nil tokens))))
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
			  (gethash this-item (gethash :values space))
			  (not (gethash this-item (gethash :functions space))))
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
					   (gethash (first remaining)
						    (gethash :variables space))))
				  ;; TODO: add a clause to test for closures returning
				  ;; a value to left of overloaded glyph
				  )))
		     
		     (let ((fn (first (last this-item)))
			   (obligate-dyadic (and (eq :op (first this-item))
						 (vex::of-lexicon idiom :dyadic-functions (third this-item)))))
		       (cond ((and (characterp fn)
				   (or (not (getf properties :glyph))
				       (and (char= fn (aref (string (getf properties :glyph)) 0)))))
			      ;; (print (list :ggg fn axes))
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
			   ;; (print (list :fn-enc output out-properties))
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
			    (gethash fn (gethash :functions space))
			    (not (gethash fn (gethash :values space))))
		       (values fn (list :type (list :function :referenced))
			       remaining)
		       (values nil nil tokens))))))
 ;; match a reference to an operator, this must be a lexical reference like ⍣
 (operator (multiple-value-bind (axes this-item remaining)
	       (extract-axes process tokens)
	     (if (and (listp this-item)
		      (eq :op (first this-item)))
		 ;; process an operator token
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
 (with :idiom-symbol idiom :space-symbol space :process-symbol process :properties-symbol properties)
 (value
  ;; match an array like 1 2 3, marking the beginning of an array expression
  ;; ...or a functional expression if the array is an operand to a pivotal operator
  ((value :element array :times :any))
  (let ((value (output-value value properties)))
    value)
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
   (operand :pattern (:type (:function))))
  (let ((axes (first (getf (first properties) :axes))))
    (funcall (get-operator-data idiom operator :lateral)
	     operand space axes))
  (list :type (list :function :operator-composed :lateral))))

(set-composer-patterns
 composer-following-patterns-apl-standard
 (with :idiom-symbol idiom :space-symbol space :process-symbol process
       :properties-symbol properties :precedent-symbol precedent)
 (value-assignment
  ;; match a value assignment like a←1 2 3, part of an array expression
  ((:with-preceding-type :array)
   (assignment-function :element (function :glyph ←))
   (symbol :element (array :symbol-overriding t)))
  (let ((axes (getf (second properties) :axes)))
    (setf (gethash symbol (gethash :values space))
	  precedent)
    (if (gethash symbol (gethash :functions space))
	(setf (gethash symbol (gethash :functions space))
	      nil))
    (if axes (enclose-axes symbol axes :set `(disclose ,precedent))
	`(setq ,symbol ,precedent)))
  (list :type (list :array :assigned)))
 (function-assignment
  ;; match a function assignment like f←{⍵×2}, part of a functional expression
  ((:with-preceding-type :function)
   (assignment-function :element (function :glyph ←))
   (symbol :element (array :symbol-overriding t)))
  (progn (setf (gethash symbol (gethash :functions space))
	       precedent)
	 (if (gethash symbol (gethash :values space))
	     (setf (gethash symbol (gethash :values space))
		   nil))
	 `(setq ,symbol ,precedent)))
 (pivotal-composition
  ;; match a pivotal function composition like ×.+, part of a functional expression
  ;; it may come after either a function or an array, since some operators take array operands
  ((operator :element (operator :valence :pivotal))
   (operand :pattern (:special (list :omit (list :value-assignment :function-assignment :operation)))))
  ;; the special :omit property makes it so that the pattern matching the operand may not be processed as
  ;; a value assignment, function assignment or operation, which allows for expressions like
  ;; fn←5∘- where an operator-composed function is assigned
  (funcall (get-operator-data idiom operator :pivotal)
	   precedent operand space)
  (list :type (list :function :operator-composed :pivotal)))
 (operation
  ;; match an operation on arrays like 1+1 2 3, ⍳9 or +/⍳5, these operations are the basis of APL
  ((:with-preceding-type :array)
   (fn-element :pattern (:type (:function)))
   ;; the value match is canceled when encountering a pivotal operator composition on the left side
   ;; of the function element so that expressions like ÷.5 ⊢10 20 30 work properly
   (value :element (array :cancel-if :pivotal-composition) :optional t :times :any))
  (let ((fn-content (if (not (characterp fn-element))
			fn-element (get-function-data idiom fn-element (if value :dyadic :monadic))))
	(axes (getf (first properties) :axes)))
    `(apl-call ,fn-content ,precedent ,@(if value (list (output-value value (rest properties))))
	       ,@(if axes `((list ,@(first axes))))))
  (list :type (list :array :evaluated))))
