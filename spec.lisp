;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; spec.lisp

(in-package #:april)

(defparameter *circular-functions*
  ;; APL's set of circular functions called using the ○ symbol with a left argument
  (vector (lambda (input) (exp (complex 0 input)))
	  (lambda (input) (complex 0 input))
	  #'conjugate #'identity (lambda (input) (sqrt (- -1 (* 2 input))))
	  #'atanh #'acosh #'asinh (lambda (input) (* (1+ input) (sqrt (/ (1+ input) (1- input)))))
	  #'atan #'acos #'asin (lambda (input) (sqrt (- 1 (* 2 input))))
	  #'sin #'cos #'tan (lambda (input) (sqrt (1+ (* 2 input))))
	  #'sinh #'cosh #'tanh (lambda (input) (sqrt (- -1 (* 2 input))))
	  #'realpart #'abs #'imagpart #'phase))

(defvar *digit-vector* "0123456789")

(defvar *alphabet-vector* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defvar *atomic-vector*
  (concatenate 'string "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
	       "%'._¤#&\"’¶@‘:?!£€$()[]{}<≤=≥>≠∨∧⊂⊃∩∪/\\+-⍺⍵"
	       "⌶¯⍬∆⍙⌿⍀⊣⊢⌷¨⍨÷×∊⍴~↑↓⍳○*⌈⌊∇∘⊥⊤|;,⍱⍲⍒⍋⍉⌽⊖⍟⌹⍕⍎⍫⍪≡≢ø^∣⍷⋄←→⍝§⎕⍞⍣⍇⍈⍐⍗ ┘┐┌└┼─├┤┴┬│"))

(define-symbol-macro *apl-timestamp* (apl-timestamp))

;; top-level specification for the April language
(specify-vex-idiom
 april

 ;; system variables and default state of an April workspace
 (system :atomic-vector *atomic-vector* :disclose-output t :output-printed nil
	 :base-state (list :index-origin 1 :comparison-tolerance 1e-14 :print-precision 10
			   :output-stream '*standard-output*))

 ;; standard grammar components, with elements to match the basic language forms and pattern-matching systems to
 ;; register combinations of those forms
 (grammar (:elements composer-elements-apl-standard)
	  (:opening-patterns composer-opening-patterns-apl-standard)
	  (:following-patterns composer-following-patterns-apl-standard
			       composer-optimized-patterns-common))

 ;; parameters for describing and documenting the idiom in different ways; currently, these options give
 ;; the order in which output from the blocks of tests is printed out for the (test) and (demo) options
 (doc-profiles (:test :lexical-functions-scalar-numeric :lexical-functions-scalar-logical
		      :lexical-functions-mixed :lexical-functions-special :lexical-operators-lateral
		      :lexical-operators-pivotal :general-tests :system-variable-function-tests
		      :array-function-scalar-index-input-tests :printed-format-tests)
	       (:demo :general-tests :lexical-functions-scalar-numeric
		      :lexical-functions-scalar-logical :lexical-functions-mixed :lexical-functions-special
		      :lexical-operators-lateral :lexical-operators-pivotal
		      :system-variable-function-tests :printed-format-tests))

 ;; utilities for compiling the language
 (utilities :match-blank-character (lambda (char) (member char (list #\  #\Tab)))
	    :match-newline-character (lambda (char) (member char (list #\◊ #\⋄ #\Newline #\Return)))
	    ;; set the language's valid blank, newline characters and token characters
	    :match-token-character
	    (lambda (char)
	      (or (alphanumericp char)
		  (member char (list #\. #\_ #\⎕ #\∆ #\⍙ #\¯ #\⍺ #\⍵ #\⍬))))
	    ;; overloaded numeric characters may be functions or operators or may be part of a numeric token
	    ;; depending on their context
	    :match-overloaded-numeric-character (lambda (char) (char= #\. char))
	    ;; this code preprocessor removes comments, including comment-only lines
	    :prep-code-string
	    (lambda (string)
	      (regex-replace-all (concatenate 'string "^\\s{0,}⍝(.*)[\\r\\n]|(?<=[\\r\\n])\\s{0,}⍝(.*)[\\r\\n]"
					      "|(?<=[\\r\\n])\\s{0,}⍝(.*)[\\r\\n]"
					      "|(?<=[^\\r\\n])\\s{0,}⍝(.*)(?=[\\r\\n])")
				 string ""))
	    ;; handles axis strings like "'2;3;;' from 'array[2;3;;]'"
	    :process-axis-string (lambda (string) (cl-ppcre:split #\; string))
	    ;; macro to process lexical specs of functions and operators
	    :process-lexicon #'april-function-glyph-processor
	    :format-value #'format-value
	    ;; process system state input passed when April is invoked, i.e. with (april (with (:state ...)) "...")
	    :preprocess-state-input
	    (lambda (state)
	      (if (getf state :count-from)
		  (progn (if (not (getf state :index-origin))
			     (setf (getf state :index-origin)
				   (getf state :count-from)))
			 (setf (getf state :count-from) nil)))
	      state)
	    ;; converts parts of the system state into lists that will form part of the local lexical
	    ;; environment in which the compiled APL code runs, i.e. the (let) form into which
	    ;; the APL-generating macros are expanded
	    :system-lexical-environment-interface
	    (lambda (state)
	      ;; currently, the only system value passed into the local environment is the index-origin
	      (append (list (list (intern "OUTPUT-STREAM" "APRIL")
				  (getf state :output-stream)))
		      (loop :for var :in (list :index-origin :print-precision)
			 :collect (list (intern (string-upcase var) "APRIL")
					(getf state var)))))
	    :process-compiled-as-per-workspace
	    (lambda (workspace form)
	      (funcall (if (not workspace)
			   #'identity (lambda (form) `(in-apl-workspace ,workspace ,form)))
		       form))
	    ;; postprocessors for language output
	    :postprocess-compiled
	    (lambda (state)
	      (lambda (form)
		;; wrap the last element of the compiled output in a disclose form if discloseOutput is set
		(append (butlast form)
			(list (append (list 'apl-output
					    (funcall (if (not (getf state :disclose-output))
							 #'identity (lambda (item) (list 'disclose-atom item)))
						     (first (last form))))
				      (append (list :print-precision 'print-precision)
					      (if (getf state :print)
						  (list :print-to 'output-stream))
					      (if (getf state :output-printed)
						  (list :output-printed (getf state :output-printed)))))))))
	    :postprocess-value
	    (lambda (form state)
	      (append (list 'apl-output (funcall (if (not (getf state :disclose-output))
						     #'identity (lambda (item) (list 'disclose-atom item)))
						 form))
		      (append (list :print-precision 'print-precision)
			      (if (getf state :print) (list :print-to 'output-stream))
			      (if (getf state :output-printed)
				  (list :output-printed (getf state :output-printed)))))))

 ;; specs for multi-character symbols exposed within the language
 (symbols (:variable ⎕ to-output ⎕io index-origin ⎕pp print-precision ⎕ost output-stream)
	  (:constant ⎕a *alphabet-vector* ⎕d *digit-vector* ⎕av *atomic-vector* ⎕ts *apl-timestamp*))
 
 ;; APL's set of functions represented by characters
 (functions
  (with (:name :lexical-functions-scalar-numeric)
	(:tests-profile :title "Scalar Numeric Function Tests")
	(:demo-profile :title "Scalar Numeric Function Demos"
		       :description "Scalar numeric functions change individual numeric values. They include basic arithmetic and other numeric operations, and they can be applied over arrays."))
  (+ (has :titles ("Conjugate" "Add"))
     (ambivalent :asymmetric-scalar conjugate +)
     (tests (is "+5" 5)
	    (is "+5J2" #C(5 -2))
	    (is "1+1" 2)
	    (is "1+1 2 3" #(2 3 4))))
  (- (has :titles ("Negate" "Subtract"))
     (ambivalent :symmetric-scalar (reverse-op -))
     (tests (is "2-1" 1)
	    (is "7-2 3 4" #(5 4 3))))
  (× (has :titles ("Sign" "Multiply"))
     (ambivalent :asymmetric-scalar signum *)
     (tests (is "×20 5 0 ¯7 3 ¯9" #(1 1 0 -1 1 -1))
	    (is "2×3" 6)
	    (is "4 5×8 9" #(32 45))))
  (÷ (has :titles ("Reciprocal" "Divide"))
     (ambivalent :symmetric-scalar (reverse-op /))
     (tests (is "6÷2" 3)
	    (is "12÷6 3 2" #(2 4 6))
	    (is "÷2 4 8" #(1/2 1/4 1/8))))
  (⋆ (has :titles ("Exponential" "Power") :aliases (*))
     (ambivalent :asymmetric-scalar exp (reverse-op :dyadic expt))
     (tests (is "⌊1000×⋆2" 7389)
	    (is "2⋆4" 16)
	    (is "⌊16⋆÷2" 4)))
  (⍟ (has :titles ("Natural Logarithm" "Logarithm"))
     (ambivalent :symmetric-scalar log)
     (tests (is "⌊1000×⍟5" 1609)
	    (is "⌊2⍟8" 3)))
  (\| (has :titles ("Magnitude" "Residue"))
      (ambivalent :asymmetric-scalar abs mod)
      (tests (is "|55" 55)
	     (is "|¯33" 33)
	     (is "8|39" 7)))
  (! (has :titles ("Factorial" "Binomial"))
     (ambivalent :asymmetric-scalar sprfact binomial)
     (tests (is "!5" 120)
	    (is "5!12" 792)))
  (⌈ (has :titles ("Ceiling" "Maximum"))
     (ambivalent :asymmetric-scalar ceiling (reverse-op max))
     (tests (is "⌈1.0001" 2)
	    (is "⌈1.9998" 2)
	    (is "3⌈0 1 2 3 4 5" #(3 3 3 3 4 5))))
  (⌊ (has :titles ("Floor" "Minimum"))
     (ambivalent :asymmetric-scalar floor (reverse-op min))
     (tests (is "⌊1.0001" 1)
	    (is "⌊1.9998" 1)
	    (is "3⌊0 1 2 3 4 5" #(0 1 2 3 3 3))))
  (? (has :titles ("Random" "Deal"))
     (ambivalent (scalar-function (lambda (omega)
				    (if (integerp omega)
					(+ index-origin (random omega))
					(error "The right arguments to ? must be non-negative integers."))))
		 (lambda (omega alpha)
		   (let ((omega (disclose omega))
			 (alpha (disclose alpha)))
		     (if (or (not (integerp omega))
			     (not (integerp alpha)))
			 (error "Both arguments to ? must be single non-negative integers.")
			 (make-array (list alpha)
				     :element-type (list 'integer 0 alpha)
				     :initial-contents (loop :for i :below alpha
							  :collect (+ index-origin (random omega)))))))))
  (○ (has :titles ("Pi Times" "Circular"))
     (ambivalent :asymmetric-scalar (lambda (omega) (* pi omega))
		 (lambda (omega alpha)
		   (if (and (integerp alpha) (<= -12 alpha 12))
		       (funcall (aref *circular-functions* (+ 12 alpha))
				omega)
		       (error (concatenate 'string "Invalid argument to ○; the left argument must be an"
					   " integer between ¯12 and 12.")))))
     (tests (is "⌊100000×○1" 314159)
	    (is "(⌊1000×1÷2⋆÷2)=⌊1000×1○○÷4" 1)
	    (is "⌊1000×1○⍳9" #(841 909 141 -757 -959 -280 656 989 412))
	    (is "⌈1 2 3○○.5 2 .25" #(1 1 1))))
  (\~ (has :titles ("Not" "Without"))
      (ambivalent (scalar-function (lambda (omega)
				     (cond ((= 0 omega) 1)
					   ((= 1 omega) 0)
					   (t (error "Domain error: arguments to ~~ must be 1 or 0.")))))
		  (lambda (omega alpha)
		    (flet ((compare (o a)
			     (funcall (if (and (characterp a) (characterp o))
					  #'char= (if (and (numberp a) (numberp o))
						      #'= (error "Compared incompatible types.")))
				      o a)))
		      (let* ((included))
			(across alpha (lambda (element coords)
					(declare (ignore coords))
					(let ((include t))
					  (if (arrayp omega)
					      (across omega (lambda (ex co)
							      (declare (ignore co))
							      (if (compare ex element) (setq include nil))))
					      (if (compare omega element) (setq include nil)))
					  (if include (setq included (cons element included))))))
			(make-array (list (length included))
				    :element-type (element-type alpha)
				    :initial-contents (reverse included))))))
      (tests (is "~1 0 1" #(0 1 0))
	     (is "1 2 3 4 5 6 7~3 5" #(1 2 4 6 7))
	     (is "(⍳9)~2 2⍴⍳9" #(5 6 7 8 9))
	     (is "'MACARONI'~'ALFREDO'" "MCNI"))))

 (functions
  (with (:name :lexical-functions-scalar-logical)
	(:tests-profile :title "Scalar Logical Function Tests")
	(:demo-profile :title "Scalar Logical Function Demos"
		       :description "Scalar logical functions compare individual values, and like scalar numeric functions they can be applied over arrays."))
  (< (has :title "Less")
     (dyadic (scalar-function (boolean-op <)))
     (tests (is "3<1 2 3 4 5" #*00011)))
  (≤ (has :title "Less or Equal")
     (dyadic (scalar-function (boolean-op <=)))
     (tests (is "3≤1 2 3 4 5" #*00111)))
  (= (has :title "Equal")
     (dyadic (scalar-function (boolean-op (lambda (omega alpha)
					    (funcall (if (and (characterp alpha) (characterp omega))
							 #'char= (if (and (numberp alpha) (numberp omega))
								     #'= (error "Compared incompatible types.")))
						     omega alpha)))))
     (tests (is "3=1 2 3 4 5" #*00100)
	    (is "'cat'='hat'" #*011)))
  (≥ (has :title "Greater or Equal")
     (dyadic (scalar-function (boolean-op >=)))
     (tests (is "3≥1 2 3 4 5" #*11100)))
  (> (has :title "Greater")
     (dyadic (scalar-function (boolean-op >)))
     (tests (is "3>1 2 3 4 5" #*11000)))
  (≠ (has :title "Not Equal")
     (dyadic (scalar-function (boolean-op /=)))
     (tests (is "3≠1 2 3 4 5" #*11011)))
  (∧ (has :title "And" :aliases (^))
     (dyadic (scalar-function (reverse-op lcm)))
     (tests (is "0 1 0 1∧0 0 1 1" #*0001)))
  (⍲ (has :title "Nand")
     (dyadic (scalar-function (boolean-op (lambda (omega alpha) (not (= omega alpha 1))))))
     (tests (is "0 1 0 1⍲0 0 1 1" #*1110)))
  (∨ (has :title "Or")
     (dyadic (scalar-function (reverse-op gcd)))
     (tests (is "0 1 0 1∨0 0 1 1" #*0111)))
  (⍱ (has :title "Nor")
     (dyadic (scalar-function (boolean-op (lambda (omega alpha) (= omega alpha 0)))))
     (tests (is "0 1 0 1⍱0 0 1 1" #*1000))))

 (functions
  (with (:name :lexical-functions-mixed)
	(:tests-profile :title "Mixed Function Tests")
	(:demo-profile :title "Mixed Function Demos"
		       :description "These functions affect entire arrays, changing their structure or deriving data from them in some way."))
  (⍳ (has :titles ("Interval" "Index Of"))
     (ambivalent (lambda (omega)
		   (let ((omega (disclose omega)))
		     (if (not (integerp omega))
			 (error "The argument to ⍳ must be a single integer, i.e. ⍳9.")
			 (let ((output (make-array (list omega) :element-type (list 'integer 0 omega))))
			   (loop :for ix :below omega :do (setf (aref output ix) (+ ix index-origin)))
			   output))))
		 (lambda (omega alpha) (index-of omega alpha index-origin)))
     (tests (is "⍳5" #(1 2 3 4 5))
	    (is "3⍳1 2 3 4 5" #(2 2 1 2 2))))
  (⍴ (has :titles ("Shape" "Reshape"))
     (ambivalent (lambda (omega)
		   (if (= 0 (array-total-size omega))
		       #*0 (let* ((omega (enclose-atom omega))
				  (omega-dims (dims omega))
				  (max-dim (reduce #'max omega-dims)))
			     (make-array (list (length omega-dims))
					 :element-type (list 'integer 0 max-dim)
					 :initial-contents omega-dims))))
		 (lambda (omega alpha) (reshape-array-fitting omega (if (arrayp alpha)
									(array-to-list alpha)
									(list alpha)))))
     (tests (is "⍴1 2 3" 3)
	    (is "⍴3 5⍴⍳8" #(3 5))
	    (is "4 5⍴⍳3" #2A((1 2 3 1 2) (3 1 2 3 1) (2 3 1 2 3) (1 2 3 1 2)))))
  (⌷ (has :title "Index")
     (dyadic (lambda (omega alpha &optional axes)
	       (enclose (choose omega (let ((coords (array-to-list (apply-scalar #'- alpha index-origin)))
					    ;; the inefficient array-to-list is used here in case of nested
					    ;; alpha arguments like (⊂1 2 3)⌷...
					    (axis (if (first axes) (loop :for item :across (first axes)
								      :collect (- item index-origin)))))
					(if (not axis)
					    coords (loop :for dim :below (rank omega)
						      :collect (if (member dim axis) (first coords))
						      :when (member dim axis)
						      :do (setq coords (rest coords)))))))))
     (tests (is "3⌷⍳9" 3)
	    (is "2 2⌷4 5⍴⍳9" 7)
	    (is "2 3 4⌷4 5 6⍴⍳9" 1)
	    (is "1 3⌷2 3 4⍴⍳5" #(4 5 1 2))
	    (is "1 3⌷[1 3]2 3 4⍴⍳5" #(3 2 1))
	    (is "(⊂4 5 2 6 3 7 1)⌷'MARANGA'" "ANAGRAM")))
  (≡ (has :titles ("Depth" "Match"))
     (ambivalent (lambda (omega)
		   (if (or (not (arrayp omega))
			   (is-unitary omega))
		       #*0 (array-depth omega)))
		 (boolean-op array-compare))
     (tests (is "≡1" 0)
	    (is "≡⍳3" 1)
	    (is "≡(1 2)(3 4)" 2)
	    (is "≡1 (2 3) (4 5 (6 7)) 8" -3)))
  (≢ (has :titles ("First Dimension" "Not Match"))
     (ambivalent (lambda (omega)
		   (if (or (not (arrayp omega))
			   (is-unitary omega))
		       #*1 (first (dims omega))))
		 (boolean-op (lambda (omega alpha) (not (array-compare omega alpha)))))
     (tests (is "≢1 2 3" 3)
	    (is "≢2 3 4⍴⍳9" 2)))
  (∊ (has :titles ("Enlist" "Membership"))
     (ambivalent #'enlist
		 (lambda (omega alpha)
		   (let ((output (make-array (dims alpha) :element-type 'bit :initial-element 0))
			 (to-search (make-array (list (array-total-size omega))
						:displaced-to omega :element-type (element-type omega))))
		     ;; TODO: this could be faster with use of a hash table and other additions
		     (dotimes (index (array-total-size output))
		       (let ((found))
			 (loop :for item :across to-search :while (not found)
			    :do (setq found (or (and (numberp item)
						     (numberp (row-major-aref alpha index))
						     (= item (row-major-aref alpha index)))
						(and (characterp item)
						     (characterp (row-major-aref alpha index))
						     (char= item (row-major-aref alpha index)))
						(and (arrayp item)
						     (arrayp (row-major-aref alpha index))
						     (array-compare item (row-major-aref alpha index))))))
			 (if found (setf (row-major-aref output index) 1))))
		     output)))
     (tests (is "∊2 2 2⍴⍳9" #(1 2 3 4 5 6 7 8))
	    (is "2 5 7∊1 2 3 4 5" #*110)))
  (⍷ (has :title "Find")
     (dyadic #'find-array)
     (tests (is "(2 2⍴6 7 1 2)⍷2 3 4⍴⍳9" #3A(((0 0 0 0) (0 1 0 0) (0 0 0 0))
					     ((0 0 1 0) (0 0 0 0) (0 0 0 0))))))
  (⍸ (has :titles ("Where" "Interval Index"))
     (ambivalent (lambda (omega)
		   (let* ((indices) (match-count 0)
			  (omega (enclose-atom omega))
			  (orank (rank omega)))
		     (across omega (lambda (index coords)
				     (if (= 1 index)
					 (let* ((max-coord 0)
						(coords (mapcar (lambda (i)
								  (setq max-coord
									(max max-coord (+ i index-origin)))
								  (+ i index-origin))
								coords)))
					   (incf match-count)
					   (setq indices (cons (if (< 1 orank)
								   (make-array (list orank)
									       :element-type
									       (list 'integer 0 max-coord)
									       :initial-contents coords)
								   (first coords))
							       indices))))))
		     (if (not indices)
			 (make-array (list 0))
			 (make-array (list match-count)
				     :element-type (if (< 1 orank)
						       t (list 'integer 0 (reduce #'max indices)))
				     :initial-contents (reverse indices)))))
		 (interval-index atomic-vector))
     (tests (is "⍸0 0 1 0 1 0 0 1 1 0" #(3 5 8 9))
	    (is "⍸3=2 3 4⍴⍳9" #(#(1 1 3) #(1 3 4) #(2 3 1)))
	    (is "⍸(2 3 4⍴⍳9)∊3 5" #(#(1 1 3) #(1 2 1) #(1 3 4) #(2 1 2) #(2 3 1) #(2 3 3)))
	    (is "10 20 30 40⍸5 12 19 24 35 42 51" #(0 1 1 2 3 4 4))
	    (is "(2 5⍴'RADIUS')⍸3 4 5⍴'BOXCAR'" #2A((0 1 0 0) (2 0 0 1) (0 0 2 0)))
	    (is "(2 3 5⍴'ABCDEFHIJKLM')⍸3 3 5⍴'BOREAL'" #(1 2 1))))
  (\, (has :titles ("Ravel" "Catenate or Laminate"))
      (ambivalent (lambda (omega &optional axes)
		    (if (not (arrayp omega))
			(enclose omega)
			(ravel index-origin omega axes)))
		  (lambda (omega alpha &optional axes)
		    (if (and (or (not axes)
				 (integerp (aref (first axes) 0)))
			     (vectorp alpha)
			     (vectorp omega))
			(if (and axes (< 0 (- (aref (first axes) 0)
					      index-origin)))
			    (error (concatenate 'string "Specified axis is greater than 1; vectors"
						" have only one axis along which to catenate."))
			    (if (and axes (> 0 (- (aref (first axes) 0)
						  index-origin)))
				(error (format nil "Specified axis is less than ~a." index-origin))
				(catenate alpha omega 0)))
			(if (and axes (not (integerp (aref (first axes) 0))))
			    ;; laminate in the case of a fractional axis argument
			    (laminate alpha omega (ceiling (- (aref (first axes) 0)
							      index-origin)))
			    ;; simply stack the arrays if there is no axis argument or it's an integer
			    (catenate alpha omega (if axes (- (aref (first axes) 0)
							      index-origin)
						      (1- (max (rank alpha)
							       (rank omega)))))))))
      (tests (is ",3 4⍴⍳9" #(1 2 3 4 5 6 7 8 9 1 2 3))
	     (is ",[0.5]3 4⍴⍳9" #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3))))
	     (is ",[1.5]3 4⍴⍳9" #3A(((1 2 3 4)) ((5 6 7 8)) ((9 1 2 3))))
	     (is ",[2.5]3 4⍴⍳9" #3A(((1) (2) (3) (4)) ((5) (6) (7) (8)) ((9) (1) (2) (3))))
	     (is ",[1 2]2 3 3⍴⍳12" #2A((1 2 3) (4 5 6) (7 8 9) (10 11 12) (1 2 3) (4 5 6)))
	     (is ",[2 3]2 3 3⍴⍳12" #2A((1 2 3 4 5 6 7 8 9) (10 11 12 1 2 3 4 5 6)))
	     (is ",[1 2 3]2 3 3⍴⍳12" #(1 2 3 4 5 6 7 8 9 10 11 12 1 2 3 4 5 6))
	     (is "0,3 4⍴⍳9" #2A((0 1 2 3 4) (0 5 6 7 8) (0 9 1 2 3)))
	     (is "0,[1]3 4⍴⍳9" #2A((0 0 0 0) (1 2 3 4) (5 6 7 8) (9 1 2 3)))
	     (is "(3 6⍴⍳6),3 4⍴⍳9" #2A((1 2 3 4 5 6 1 2 3 4) (1 2 3 4 5 6 5 6 7 8)
				       (1 2 3 4 5 6 9 1 2 3)))
	     (is "(5 4⍴⍳6),[1]3 4⍴⍳9" #2A((1 2 3 4) (5 6 1 2) (3 4 5 6) (1 2 3 4)
					  (5 6 1 2) (1 2 3 4) (5 6 7 8) (9 1 2 3)))
	     (is "(6 7 8 9 0),⍪1 2 3 4 5" #2A((6 1) (7 2) (8 3) (9 4) (0 5)))
	     (is "(2 3 4⍴⍳5),2 3⍴9" #3A(((1 2 3 4 9) (5 1 2 3 9) (4 5 1 2 9))
					((3 4 5 1 9) (2 3 4 5 9) (1 2 3 4 9))))
	     (is "(4 4⍴5),4 4 4⍴3" #3A(((5 3 3 3 3) (5 3 3 3 3) (5 3 3 3 3) (5 3 3 3 3))
				       ((5 3 3 3 3) (5 3 3 3 3) (5 3 3 3 3) (5 3 3 3 3))
				       ((5 3 3 3 3) (5 3 3 3 3) (5 3 3 3 3) (5 3 3 3 3))
				       ((5 3 3 3 3) (5 3 3 3 3) (5 3 3 3 3) (5 3 3 3 3))))
	     (is "1 2 3,4 5 6" #(1 2 3 4 5 6))
	     (is "1 2 3,[1]4 5 6" #(1 2 3 4 5 6))
	     (is "(3 4⍴5),[1]2 3 4⍴9" #3A(((5 5 5 5) (5 5 5 5) (5 5 5 5))
					  ((9 9 9 9) (9 9 9 9) (9 9 9 9))
					  ((9 9 9 9) (9 9 9 9) (9 9 9 9))))
	     (is "(2 4⍴5),[2]2 3 4⍴9" #3A(((5 5 5 5) (9 9 9 9) (9 9 9 9) (9 9 9 9))
					  ((5 5 5 5) (9 9 9 9) (9 9 9 9) (9 9 9 9))))
	     (is "(2 3⍴5),[3]2 3 4⍴9" #3A(((5 9 9 9 9) (5 9 9 9 9) (5 9 9 9 9))
					  ((5 9 9 9 9) (5 9 9 9 9) (5 9 9 9 9))))
	     (is "1 2 3 4,[0.5]1 2 3 4" #2A((1 2 3 4) (1 2 3 4)))
	     (is "1 2 3 4,[1.5]1 2 3 4" #2A((1 1) (2 2) (3 3) (4 4)))
	     (is "(2 3⍴⍳9),[0.5]2 3⍴⍳9" #3A(((1 2 3) (4 5 6)) ((1 2 3) (4 5 6))))
	     (is "(2 3⍴⍳9),[2.5]2 3⍴⍳9" #3A(((1 1) (2 2) (3 3)) ((4 4) (5 5) (6 6))))
	     (is "'UNDER',[0.5]'-'" #2A((#\U #\N #\D #\E #\R) (#\- #\- #\- #\- #\-)))
	     (is "'HELLO',[1.5]'.'" #2A((#\H #\.) (#\E #\.) (#\L #\.) (#\L #\.) (#\O #\.)))))
  (⍪ (has :titles ("Table" "Catenate First"))
     (ambivalent (lambda (omega) (if (not (arrayp omega))
				     (enclose omega)
				     (if (vectorp omega)
					 (make-array (list (length omega) 1)
						     :element-type (element-type omega)
						     :initial-contents
						     (loop :for i :below (length omega)
							:collect (list (aref omega i))))
					 (let ((o-dims (dims omega)))
					   (make-array (list (first o-dims) (apply #'* (rest o-dims)))
						       :element-type (element-type omega)
						       :displaced-to (copy-array omega))))))
		 (lambda (omega alpha &optional axes)
		   (if (and (vectorp alpha) (vectorp omega))
		       (if (and axes (< 0 (- (aref (first axes) 0)
					     index-origin)))
			   (error (concatenate 'string "Specified axis is greater than 1, vectors"
					       " have only one axis along which to catenate."))
			   (if (and axes (> 0 (- (aref (first axes) 0)
						 index-origin)))
			       (error (format nil "Specified axis is less than ~a." index-origin))
			       (catenate alpha omega 0)))
		       (if (or (not axes)
			       (integerp (aref (first axes) 0)))
			   (catenate alpha omega (if (not axes) 0 (- (aref (first axes) 0)
								     index-origin)))))))
     (tests (is "⍪'MAKE'" #2A((#\M) (#\A) (#\K) (#\E)))
	    (is "⍪3 4⍴⍳9" #2A((1 2 3 4) (5 6 7 8) (9 1 2 3)))
	    (is "⍪2 3 4⍴⍳24" #2A((1 2 3 4 5 6 7 8 9 10 11 12)
				 (13 14 15 16 17 18 19 20 21 22 23 24)))
	    (is "0⍪3 4⍴⍳9" #2A((0 0 0 0) (1 2 3 4) (5 6 7 8) (9 1 2 3)))
	    (is "0⍪[2]3 4⍴⍳9" #2A((0 1 2 3 4) (0 5 6 7 8) (0 9 1 2 3)))
	    (is "(3⍴5)⍪3 3⍴3" #2A((5 5 5) (3 3 3) (3 3 3) (3 3 3)))
	    (is "(5 4⍴⍳6)⍪3 4⍴⍳9" #2A((1 2 3 4) (5 6 1 2) (3 4 5 6) (1 2 3 4)
				      (5 6 1 2) (1 2 3 4) (5 6 7 8) (9 1 2 3)))
	    (is "(3 6⍴⍳6)⍪[2]3 4⍴⍳9" #2A((1 2 3 4 5 6 1 2 3 4) (1 2 3 4 5 6 5 6 7 8)
					 (1 2 3 4 5 6 9 1 2 3)))))
  (↑ (has :titles ("Mix" "Take"))
     (ambivalent (lambda (omega &optional axes)
		   (mix-arrays (if axes (aops:each (lambda (item) (- (ceiling item) index-origin))
						   (first axes))
				   (vector (rank omega)))
			       omega))
		 (lambda (omega alpha &optional axes)
		   (let ((omega (enclose-atom omega))
			 (alpha-index (if (not (arrayp alpha))
					  alpha (aref alpha 0)))
			 (alpha (if (arrayp alpha)
				    (array-to-list alpha)
				    (list alpha))))
		     (section omega (if axes (loop :for axis :below (rank omega)
						:collect (if (= axis (- (aref (first axes) 0)
									index-origin))
							     alpha-index (nth axis (dims omega))))
					alpha)))))
     (tests (is "↑(1)(1 2)(1 2 3)" #2A((1 0 0) (1 2 0) (1 2 3)))
	    (is "↑[0.5](1)(1 2)(1 2 3)" #2A((1 1 1) (0 2 2) (0 0 3)))
	    (is "↑(2 3⍴⍳5)(4 2⍴⍳8)" #3A(((1 2 3) (4 5 1) (0 0 0) (0 0 0))
					((1 2 0) (3 4 0) (5 6 0) (7 8 0))))
	    (is "↑(2 5⍴⍳9)(3 2 1)(4 3⍴⍳8)" #3A(((1 2 3 4 5) (6 7 8 9 1) (0 0 0 0 0) (0 0 0 0 0))
					       ((3 2 1 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0))
					       ((1 2 3 0 0) (4 5 6 0 0) (7 8 1 0 0) (2 3 4 0 0))))
	    (is "↑[0.5](2 3⍴⍳5)(4 2⍴⍳8)" #3A(((1 1) (2 2) (3 0))
					     ((4 3) (5 4) (1 0))
					     ((0 5) (0 6) (0 0))
					     ((0 7) (0 8) (0 0))))
	    (is "↑[1.5](2 3⍴⍳5)(4 2⍴⍳8)" #3A(((1 2 3) (4 5 1) (0 0 0) (0 0 0))
					     ((1 2 0) (3 4 0) (5 6 0) (7 8 0))))
	    (is "↑2 2 2⍴(1)(1 2)(3 4)(1 2 3)" #4A((((1 0 0) (1 2 0)) ((3 4 0) (1 2 3)))
						  (((1 0 0) (1 2 0)) ((3 4 0) (1 2 3)))))
	    (is "¯1↑⍳5" 5)
	    (is "2 3 4↑4 5 6⍴⍳9" #3A(((1 2 3 4) (7 8 9 1) (4 5 6 7))
				     ((4 5 6 7) (1 2 3 4) (7 8 9 1))))
	    (is "2 ¯2 ¯2↑4 5 6⍴⍳9" #3A(((5 6) (2 3)) ((8 9) (5 6))))
	    (is "5 ¯5↑(3 3⍴⍳9)∊1 2 3 4 8" #2A((0 0 1 1 1) (0 0 1 0 0) (0 0 0 1 0) (0 0 0 0 0) (0 0 0 0 0)))
	    (is "1↑[1]2 3 4⍴⍳9" #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3))))
	    (is "1↑[2]2 3 4⍴⍳9" #3A(((1 2 3 4)) ((4 5 6 7))))
	    (is "2↑[2]2 3 4⍴⍳9" #3A(((1 2 3 4) (5 6 7 8)) ((4 5 6 7) (8 9 1 2))))
	    (is "2↑[3]2 3 4⍴⍳9" #3A(((1 2) (5 6) (9 1)) ((4 5) (8 9) (3 4))))))
  (↓ (has :titles ("Split" "Drop"))
     (ambivalent (lambda (omega &optional axes)
		   (split-array omega (if axes (- (aref (first axes) 0)
						  index-origin))))
		 (lambda (omega alpha &optional axes)
		   (let ((omega (enclose-atom omega))
			 (alpha-index (if (not (arrayp alpha))
					  alpha (aref alpha 0)))
			 (alpha (if (arrayp alpha)
				    (array-to-list alpha)
				    (list alpha))))
		     (section omega (if axes (loop :for axis :below (rank omega)
						:collect (if (/= axis (- (aref (first axes) 0)
									 index-origin))
							     0 alpha-index))
					alpha)
			      :inverse t))))
     (tests (is "↓3 4⍴⍳9" #(#(1 2 3 4) #(5 6 7 8) #(9 1 2 3)))
	    (is "↓[1]3 4⍴⍳9" #(#(1 5 9) #(2 6 1) #(3 7 2) #(4 8 3)))
	    (is "2↓⍳9" #(3 4 5 6 7 8 9))
	    (is "2 2 2↓4 5 6⍴⍳9" #3A(((3 4 5 6) (9 1 2 3) (6 7 8 9))
				     ((6 7 8 9) (3 4 5 6) (9 1 2 3))))
	    (is "1↓[1]2 3 4⍴⍳9" #3A(((4 5 6 7) (8 9 1 2) (3 4 5 6))))
	    (is "1↓[2]2 3 4⍴⍳9" #3A(((5 6 7 8) (9 1 2 3)) ((8 9 1 2) (3 4 5 6))))
	    (is "2↓[2]2 3 4⍴⍳9" #3A(((9 1 2 3)) ((3 4 5 6))))
	    (is "2↓[3]2 3 4⍴⍳9" #3A(((3 4) (7 8) (2 3)) ((6 7) (1 2) (5 6))))
	    (is "¯2↓⍳9" #(1 2 3 4 5 6 7))
	    (is "¯2 ¯2↓5 8⍴⍳9" #2A((1 2 3 4 5 6) (9 1 2 3 4 5) (8 9 1 2 3 4)))))
  (⊂ (has :titles ("Enclose" "Partitioned Enclose"))
     (ambivalent (lambda (omega &optional axes)
		   (if axes (re-enclose omega (aops:each (lambda (axis) (- axis index-origin))
							 (first axes)))
		       (if (loop :for dim :in (dims omega) :always (= 1 dim))
			   omega (vector omega))))
		 (lambda (omega alpha &optional axes)
		   (partitioned-enclose alpha omega (if axes (- (aref (first axes) 0)
								index-origin)
							(1- (rank omega))))))
     (tests (is "⊂⍳5" #(#(1 2 3 4 5)))
	    (is "1+⊂⍳5" #(#(2 3 4 5 6)))
	    (is "1,⊂3 4⍴⍳7" #(1 #2A((1 2 3 4) (5 6 7 1) (2 3 4 5))))
	    (is "⊂[3]2 3 4⍴'GRAYGOLDBLUESILKWOOLYARN'"
		#2A(("GRAY" "GOLD" "BLUE") ("SILK" "WOOL" "YARN")))
	    (is "⊂[2]2 3 4⍴'GRAYGOLDBLUESILKWOOLYARN'"
		#2A(("GGB" "ROL" "ALU" "YDE") ("SWY" "IOA" "LOR" "KLN")))
	    (is "⊂[1]2 3 4⍴'GRAYGOLDBLUESILKWOOLYARN'"
		#2A(("GS" "RI" "AL" "YK") ("GW" "OO" "LO" "DL") ("BY" "LA" "UR" "EN")))
	    (is "⊂[2 3]2 3 4⍴'GRAYGOLDBLUESILKWOOLYARN'"
		#(#2A((#\G #\R #\A #\Y) (#\G #\O #\L #\D) (#\B #\L #\U #\E))
		  #2A((#\S #\I #\L #\K) (#\W #\O #\O #\L) (#\Y #\A #\R #\N))))
	    (is "⊂[1 3]2 3 4⍴'GRAYGOLDBLUESILKWOOLYARN'"
		#(#2A((#\G #\R #\A #\Y) (#\S #\I #\L #\K))
		  #2A((#\G #\O #\L #\D) (#\W #\O #\O #\L))
		  #2A((#\B #\L #\U #\E) (#\Y #\A #\R #\N))))
	    (is "⊂[1 2]2 3 4⍴'GRAYGOLDBLUESILKWOOLYARN'"
		#(#2A((#\G #\G #\B) (#\S #\W #\Y)) #2A((#\R #\O #\L) (#\I #\O #\A))
		  #2A((#\A #\L #\U) (#\L #\O #\R)) #2A((#\Y #\D #\E) (#\K #\L #\N))))
	    (is "0 1 0 0 1 1 0 0 0⊂⍳9" #(#(2 3 4) #(5) #(6 7 8 9)))
	    (is "0 1 0 0 1 1 0 0⊂4 8⍴⍳9"
		#(#2A((2 3 4) (1 2 3) (9 1 2) (8 9 1)) #2A((5) (4) (3) (2))
		  #2A((6 7 8) (5 6 7) (4 5 6) (3 4 5))))
	    (is "0 1 0 1⊂[1]4 8⍴⍳9"
		#(#2A((9 1 2 3 4 5 6 7) (8 9 1 2 3 4 5 6)) #2A((7 8 9 1 2 3 4 5))))))
  (⊆ (has :title "Partition")
     (dyadic (lambda (omega alpha &optional axes)
	       (partition-array alpha omega (if axes (- (aref (first axes) 0)
							index-origin)
						(1- (rank omega))))))
     (tests (is "1 1 2 2 2 3 3 3 3⊆⍳9" #(#(1 2) #(3 4 5) #(6 7 8 9)))
	    (is "1 1 0 1⊆4 4 4⍴⍳9" #3A(((#(1 2) #(4)) (#(5 6) #(8)) (#(9 1) #(3)) (#(4 5) #(7)))
				       ((#(8 9) #(2)) (#(3 4) #(6)) (#(7 8) #(1)) (#(2 3) #(5)))
				       ((#(6 7) #(9)) (#(1 2) #(4)) (#(5 6) #(8)) (#(9 1) #(3)))
				       ((#(4 5) #(7)) (#(8 9) #(2)) (#(3 4) #(6)) (#(7 8) #(1)))))
	    (is "1 1 0 1⊆[2]4 4 4⍴⍳9" #3A(((#(1 5) #(2 6) #(3 7) #(4 8)) (#(4) #(5) #(6) #(7)))
					  ((#(8 3) #(9 4) #(1 5) #(2 6)) (#(2) #(3) #(4) #(5)))
					  ((#(6 1) #(7 2) #(8 3) #(9 4)) (#(9) #(1) #(2) #(3)))
					  ((#(4 8) #(5 9) #(6 1) #(7 2)) (#(7) #(8) #(9) #(1)))))))
  (⊃ (has :titles ("Disclose" "Pick"))
     (ambivalent (lambda (omega)
		   (if (vectorp omega)
		       (let ((output (aref omega 0)))
			 (if (arrayp output)
			     output (enclose output)))
		       (disclose omega)))
		 (lambda (omega alpha)
		   (labels ((layer-index (object indices)
			      (if indices (layer-index (aref object (- (first indices) index-origin))
						       (rest indices))
				  object)))
		     (if (= 1 (array-total-size omega))
			 (error "Right argument to dyadic ⊃ may not be unitary.")
			 (let ((found (layer-index omega (array-to-list alpha))))
			   (if (arrayp found)
			       found (make-array (list 1) :element-type (element-type omega)
						 :initial-element found)))))))
     (tests (is "⊃⍳4" 1)
	    (is "⊃⊂⍳4" #(1 2 3 4))
	    (is "2⊃(1 2 3)(4 5 6)(7 8 9)" #(4 5 6))
	    (is "2 2⊃(1 2 3)(4 5 6)(7 8 9)" 5)))
  (∩ (has :title "Intersection")
     (dyadic (lambda (omega alpha)
	       (if (or (not (vectorp alpha))
		       (not (vectorp omega)))
		   (error "Arguments must be vectors.")
		   (let* ((match-count 0)
			  (matches (loop :for item :across alpha :when (find item omega :test #'array-compare)
				      :collect item :and :do (incf match-count))))
		     (make-array (list match-count)
				 :element-type (type-in-common (element-type alpha)
							       (element-type omega))
				 :initial-contents matches)))))
     (tests (is "'MIXTURE'∩'LATER'" "TRE")
	    (is "'STEEL'∩'SABER'" "SEE")
	    (is "1 4 8∩⍳5" #(1 4))))
  (∪ (has :titles ("Unique" "Union"))
     (ambivalent (lambda (omega)
		   (let ((vector (if (vectorp omega)
				     omega (re-enclose omega (make-array (list (1- (rank omega)))
									 :element-type 'fixnum
									 :initial-contents
									 (loop :for i :from 1
									    :to (1- (rank omega))
									    :collect i))))))
		     (let ((uniques)
			   (unique-count 0))
		       (loop :for item :across vector :when (not (find item uniques :test #'array-compare))
			  :do (setq uniques (cons item uniques))
			  (incf unique-count))
		       (funcall (if (vectorp omega)
				    #'identity (lambda (output) (mix-arrays (vector 1) output)))
				(make-array (list unique-count)
					    :element-type (element-type vector)
					    :initial-contents (reverse uniques))))))
		 (lambda (omega alpha)
		   (if (or (not (vectorp alpha))
			   (not (vectorp omega)))
		       (error "Arguments must be vectors.")
		       (let* ((unique-count 0)
			      (uniques (loop :for item :across omega :when (not (find item alpha
										      :test #'array-compare))
					  :collect item :and :do (incf unique-count))))
			 (catenate alpha (make-array (list unique-count)
						     :element-type (type-in-common (element-type alpha)
										   (element-type omega))
						     :initial-contents uniques)
				   0)))))
     (tests (is "∪1 2 3 4 5 1 2 8 9 10 11 7 8 11 12" #(1 2 3 4 5 8 9 10 11 7 12))
	    (is "∪'MISSISSIPPI'" "MISP")
	    (is "∪2 3 4⍴⍳12" #3A(((1 2 3 4) (5 6 7 8) (9 10 11 12))))
	    (is "∪2 3 4⍴⍳24" #3A(((1 2 3 4) (5 6 7 8) (9 10 11 12))
				 ((13 14 15 16) (17 18 19 20) (21 22 23 24))))
	    (is "3 10 14 18 11∪9 4 5 10 8 3" #(3 10 14 18 11 9 4 5 8))
	    (is "'STEEL'∪'SABER'" "STEELABR")
	    (is "'APRIL' 'MAY'∪'MAY' 'JUNE'" #("APRIL" "MAY" "JUNE"))))
  (⌽ (has :titles ("Reverse" "Rotate"))
     (ambivalent (lambda (omega &optional axes)
		   (let ((axis (- (if axes (aref (first axes) 0)
				      (rank omega))
				  index-origin)))
		     (if (vectorp omega)
			 (let ((new-array (copy-array omega)))
			   (funcall (make-rotator) new-array)
			   new-array)
			 (if (arrayp omega)
			     ;; TODO: apply-marginal here is inefficient
			     (apply-marginal (make-rotator)
					     omega axis)
			     (enclose omega)))))
		 (lambda (omega alpha &optional axes)
		   (let ((alpha (disclose alpha))
			 (axis (- (if axes (aref (first axes) 0)
				      (rank omega))
				  index-origin))
			 (dims (dims omega)))
		     (if (vectorp omega)
			 (let ((new-array (copy-array omega)))
			   (if (not (integerp alpha))
			       (error (concatenate 'string "The left argument to ⌽ must be a single integer, "
						   "i.e. 2⌽⍳10, if the right argument is a vector."))
			       (funcall (make-rotator alpha) new-array))
			   new-array)
			 (if (arrayp omega)
			     (let ((alpha (if (arrayp alpha)
					      alpha (make-array (loop :for d :below (rank omega)
								   :when (not (= d axis))
								   :collect (nth d dims))
								:initial-element alpha)))
				   (fn (lambda (item arg) (funcall (make-rotator arg) item))))
			       (subprocess fn axis omega alpha))
			     (enclose omega))))))
     (tests (is "⌽1 2 3 4 5" #(5 4 3 2 1))
	    (is "⌽3 4⍴⍳9" #2A((4 3 2 1) (8 7 6 5) (3 2 1 9)))
	    (is "2⌽3 4⍴⍳9" #2A((3 4 1 2) (7 8 5 6) (2 3 9 1)))
	    (is "(2 2⍴1 2 3 4)⌽2 2 5⍴⍳9" #3A(((2 3 4 5 1) (8 9 1 6 7)) ((5 6 2 3 4) (2 7 8 9 1))))))
  (⊖ (has :titles ("Reverse First" "Rotate First"))
     (ambivalent (lambda (omega &optional axes)
		   (let ((axis (if axes (- (aref (first axes) 0)
					   index-origin)
				   0)))
		     (if (vectorp omega)
			 (let ((new-array (copy-array omega)))
			   (funcall (make-rotator) new-array)
			   new-array)
			 (if (arrayp omega)
			     ;; TODO: apply-marginal here is inefficient
			     (apply-marginal (make-rotator)
					     omega axis)
			     (enclose omega)))))
		 (lambda (omega alpha &optional axes)
		   (let ((alpha (disclose alpha))
			 (axis (if axes (- (aref (first axes) 0)
					   index-origin)
				   0))
			 (dims (dims omega)))
		     (if (vectorp omega)
			 (let ((new-array (copy-array omega)))
			   (if (not (integerp alpha))
			       (error (concatenate 'string "The left argument to ⊖ must be a single integer, "
						   "i.e. 2⊖⍳10, if the right argument is a vector."))
			       (funcall (make-rotator alpha) new-array))
			   new-array)
			 (if (arrayp omega)
			     (let ((alpha (if (arrayp alpha)
					      alpha (make-array (loop :for d :below (rank omega)
								   :when (not (= d axis))
								   :collect (nth d dims))
								:initial-element alpha)))
				   (fn (lambda (item arg) (funcall (make-rotator arg) item))))
			       (subprocess fn axis omega alpha))
			     (enclose omega))))))
     (tests (is "⊖1 2 3 4 5" #(5 4 3 2 1))
	    (is "⊖3 4⍴⍳9" #2A((9 1 2 3) (5 6 7 8) (1 2 3 4)))
	    (is "1⊖3 4⍴⍳9" #2A((5 6 7 8) (9 1 2 3) (1 2 3 4)))
	    (is "(3 4 5⍴⍳4)⊖2 3 4 5⍴⍳9" #4A((((7 2 9 4 2) (6 4 8 6 1) (8 3 1 5 3) (7 5 9 7 2))
					     ((9 4 2 6 4) (8 6 1 8 3) (1 5 3 7 5) (9 7 2 9 4))
					     ((2 6 4 8 6) (1 8 3 1 5) (3 7 5 9 7) (2 9 4 2 6)))
					    (((1 8 3 1 5) (3 7 5 9 7) (2 9 4 2 6) (4 8 6 1 8))
					     ((3 1 5 3 7) (5 9 7 2 9) (4 2 6 4 8) (6 1 8 3 1))
					     ((5 3 7 5 9) (7 2 9 4 2) (6 4 8 6 1) (8 3 1 5 3)))))))
  (⍉ (has :titles ("Transpose" "Permute"))
     (ambivalent (lambda (omega)
		   (if (arrayp omega)
		       (aops:permute (loop :for i :from (1- (rank omega)) :downto 0 :collect i)
				     omega)
		       (enclose omega)))
		 (lambda (omega alpha)
		   (if (arrayp omega)
		       (aops:permute (loop :for i :across (enclose-atom alpha) :collect (- i index-origin))
				     omega)
		       (enclose omega))))
     (tests (is "⍉2 3 4⍴⍳9" #3A(((1 4) (5 8) (9 3)) ((2 5) (6 9) (1 4))
				((3 6) (7 1) (2 5)) ((4 7) (8 2) (3 6))))
	    (is "1 3 2⍉2 3 4⍴⍳9" #3A(((1 5 9) (2 6 1) (3 7 2) (4 8 3))
				     ((4 8 3) (5 9 4) (6 1 5) (7 2 6))))))
  (/ (has :title "Replicate")
     (dyadic (lambda (omega alpha &optional axes)
	       (expand-array alpha omega (if axes (- (aref (first axes) 0)
						     index-origin)
					     (max 0 (1- (rank omega))))
			     :compress-mode t)))
     (tests (is "5/3" #(3 3 3 3 3))
	    (is "1 0 1 0 1/⍳5" #(1 3 5))
	    (is "3/⍳5" #(1 1 1 2 2 2 3 3 3 4 4 4 5 5 5))
	    (is "3/⊂⍳5" #(#(1 2 3 4 5) #(1 2 3 4 5) #(1 2 3 4 5)))
	    (is "1 ¯2 3 ¯4 5/3 5⍴⍳5" #2A((1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)
					 (1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)
					 (1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)))))
  (⌿ (has :title "Replicate First")
     (dyadic (lambda (omega alpha &optional axes)
	       (expand-array alpha omega (if axes (- (aref (first axes) 0)
						     index-origin)
					     0)
			     :compress-mode t)))
     (tests (is "1 0 1 0 1⌿⍳5" #(1 3 5))
	    (is "1 ¯2 3⌿3 5⍴⍳9" #2A((1 2 3 4 5) (0 0 0 0 0) (0 0 0 0 0)
				    (2 3 4 5 6) (2 3 4 5 6) (2 3 4 5 6)))
	    (is "1 ¯2 3 ¯4 5⌿[2]3 5⍴⍳5" #2A((1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)
					    (1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)
					    (1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)))))
  (\\ (has :title "Expand")
      (dyadic (lambda (omega alpha &optional axes)
		(expand-array alpha omega (if axes (- (aref (first axes) 0)
						      index-origin)
					      (max 0 (1- (rank omega)))))))
      (tests (is "1 ¯2 3 ¯4 5\\'.'" ".  ...    .....")
	     (is "1 ¯2 2 0 1\\3+2 3⍴⍳6" #2A((4 0 0 5 5 0 6) (7 0 0 8 8 0 9)))
	     (is "1 0 1\\[1]3+2 3⍴⍳6" #2A((4 5 6) (0 0 0) (7 8 9)))
	     (is "1 ¯2 3 4\\[1]3 5⍴⍳9" #2A((1 2 3 4 5) (0 0 0 0 0) (0 0 0 0 0)
					   (6 7 8 9 1) (6 7 8 9 1) (6 7 8 9 1)
					   (2 3 4 5 6) (2 3 4 5 6) (2 3 4 5 6)
					   (2 3 4 5 6)))))
  (⍀ (has :title "Expand First")
     (dyadic (lambda (omega alpha &optional axes)
	       (expand-array alpha omega (if axes (- (aref (first axes) 0)
						     index-origin)
					     0))))
     (tests (is "1 ¯2 3 ¯4 5⍀3" #(3 0 0 3 3 3 0 0 0 0 3 3 3 3 3))
	    (is "1 0 1⍀3+2 3⍴⍳6" #2A((4 5 6) (0 0 0) (7 8 9)))))
  (⍋ (has :titles ("Grade Up" "Grade Up By"))
     (ambivalent (lambda (omega)
		   (if (is-unitary omega)
		       index-origin (grade omega (alpha-compare atomic-vector #'<=)
					   index-origin)))
		 (lambda (omega alpha) (grade (if (vectorp alpha)
						  (index-of alpha omega index-origin)
						  (array-grade alpha omega))
					      (alpha-compare atomic-vector #'<)
					      index-origin)))
     (tests (is "⍋8 3 4 9 1 5 2" #(5 7 2 3 6 1 4))
	    (is "⍋5 6⍴⍳16" #(1 4 2 5 3))
	    (is "st←'aodjeignwug' ⋄ st[⍋st]" "adeggijnouw")
	    (is "(2 5⍴'ABCDEabcde')⍋'ACaEed'" #(1 3 2 6 4 5))))
  (⍒ (has :titles ("Grade Down" "Grade Down By"))
     (ambivalent (lambda (omega)
		   (if (is-unitary omega)
		       index-origin (grade omega (alpha-compare atomic-vector #'>=)
					   index-origin)))
		 (lambda (omega alpha) (grade (if (vectorp alpha)
						  (index-of alpha omega index-origin)
						  (array-grade alpha omega))
					      (alpha-compare atomic-vector #'>)
					      index-origin)))
     (tests (is "⍒6 1 8 2 4 3 9" #(7 3 1 5 6 4 2))
	    (is "⍒5 6⍴⍳12" #(2 4 1 3 5))
	    (is "st←'aodjeignwug' ⋄ st[⍒st]" "wuonjiggeda")
	    (is "(2 5⍴'ABCDEabcde')⍒'ACaEed'" #(5 4 6 2 3 1))))
  (⌹ (has :titles ("Matrix Inverse" "Matrix Divide"))
     (ambivalent (lambda (omega)
		   (if (and (= 1 (rank omega))
			    (= 1 (length omega)))
		       (/ (disclose omega))
		       (if (< 2 (rank omega))
			   (error "Matrix inversion only works on arrays of rank 2 or 1.")
			   (if (let ((odims (dims omega)))
				 (and (= 2 (length odims))
				      (= (first odims) (second odims))))
			       (invert-matrix omega)
			       (left-invert-matrix omega)))))
		 (lambda (omega alpha)
		   (each-scalar t (array-inner-product (invert-matrix omega)
						       alpha (lambda (arg1 arg2) (apply-scalar #'* arg1 arg2))
						       #'+))))
     (tests (is "⌹1 2 3 4" #(1/30 1/15 1/10 2/15))
	    (is "⌹2 2⍴4 9 8 2" #2A((-1/32 9/64) (1/8 -1/16)))
	    (is "⌹4 2⍴1 3 ¯4 9" #2A((3/14 -1/14 3/14 -1/14) (2/21 1/42 2/21 1/42)))
	    (is "35 89 79⌹3 3⍴3 1 4 1 5 9 2 6 5" #(193/90 739/90 229/45))
	    (is "(3 2⍴1 2 3 6 9 10)⌹3 3⍴1 0 0 1 1 0 1 1 1" #2A((1 2) (2 4) (6 4)))))
  (⊤ (has :title "Encode")
     (dyadic (lambda (omega alpha)
	       (flet ((rebase (bases number &optional out-vector)
			(let ((operand number)
			      (last-base 1)
			      (base 1)
			      (component 1)
			      (element))
			  (loop :for index :from (1- (length bases)) :downto 0
			     :do (setq last-base base
				       base (* base (aref bases index))
				       component (if (= 0 base)
						     operand (* base (nth-value 1 (floor (/ operand base)))))
				       operand (- operand component)
				       element (/ component last-base))
			     (setf (aref out-vector index) element)))))
		 (if (is-unitary omega)
		     (let ((out-vector (make-array (list (first (dims alpha)))
						   :element-type (element-type alpha))))
		       (rebase alpha (disclose omega) out-vector)
		       out-vector)
		     (let* ((d-o (dims omega))
			    (d-a (dims alpha))
			    (adim1 (first d-a))
			    (bases (make-array (list adim1)
					       :element-type (element-type alpha)))
			    (out-vector (make-array (list adim1)
						    :element-type (element-type alpha)))
			    (output (make-array (append (remove 1 d-a)
							(remove 1 d-o)))))
		       (across omega (lambda (elem-o ocs)
				       (across alpha
					       (lambda (elem-a acs)
						 (declare (ignore elem-a))
						 (loop :for ix :below adim1
						    :do (setf (aref bases ix)
							      (apply #'aref (cons alpha (cons ix (rest acs)))))))
					       :dimensions (cons 1 (rest d-a)))
				       (rebase bases elem-o out-vector)
				       (loop :for ix :below adim1
					  :do (setf (apply #'aref (cons output (cons ix ocs)))
						    (aref out-vector ix)))))
		       (each-scalar t output))))))
     (tests (is "1760 3 12⊤82" #(2 0 10))
	    (is "16 16 16 16⊤100" #(0 0 6 4))
	    (is "2 2 2 2 2⊤⍳5" #2A((0 0 0 0 0) (0 0 0 0 0) (0 0 0 1 1) (0 1 1 0 0) (1 0 1 0 1)))
	    (is "16 16 16 16⊤2 2⍴100 200 300 400"
		#3A(((0 0) (0 0)) ((0 0) (1 1)) ((6 12) (2 9)) ((4 8) (12 0))))))
  (⊥ (has :title "Decode")
     (dyadic (lambda (omega alpha)
	       (flet ((rebase (bases numerators)
			(let ((result 0)
			      (base 1))
			  (loop :for index :from (1- (length numerators)) :downto 0
			     :do (incf result (* base (aref numerators index)))
			     (setf base (* base (aref bases index))))
			  result)))
		 (let* ((d-o (dims omega))
			(d-a (dims alpha)))
		   (if (and (not (is-unitary alpha))
			    (not (is-unitary omega))
			    (not (= (first d-a) (first (last d-o)))))
		       (error (concatenate 'string "If neither argument to ⊥ is scalar, the first dimension"
					   " of the left argument must equal the last dimension of"
					   "the right argument."))
		       (let* ((numerators (if (not (vectorp omega))
					      (make-array (list (first d-o))
							  :element-type (element-type alpha))))
			      (omega (if (and (is-unitary omega)
					      (not (is-unitary alpha)))
					 (make-array (list (first d-a))
						     :element-type (element-type omega)
						     :initial-element (disclose omega))
					 omega))
			      (output (if (and (< 1 (rank omega))
					       (< 1 (rank alpha)))
					  (make-array (append (butlast d-a 1)
							      (rest d-o)))))
			      (bases (if output (make-array (list (first (last d-a)))
							    :element-type (element-type alpha))
					 (if (is-unitary alpha)
					     (make-array (list (first d-o))
							 :element-type (element-type alpha)
							 :initial-element (disclose alpha))
					     alpha))))
			 (if output
			     (across output (lambda (elem coords)
					      (declare (ignore elem))
					      (loop :for dm :below (first (last d-a))
						 :do (setf (aref bases dm)
							   (apply #'aref (cons alpha
									       (append (butlast coords
												(1- (rank output)))
										       (list dm))))))
					      (if numerators
						  (loop :for dm :below (first d-o)
						     :do (setf (aref numerators dm)
							       (apply #'aref
								      (cons omega
									    (append (list dm)
										    (nthcdr (1- (rank output))
											    coords)))))))
					      (setf (apply #'aref (cons output coords))
						    (rebase bases (if numerators numerators omega))))))
			 (if output (each-scalar t output)
			     (if (vectorp omega)
				 (enclose (rebase bases omega))
				 (each-scalar t (aops:margin (lambda (sub-array) (rebase bases sub-array))
							     omega 0))))))))))
     (tests (is "10⊥2 6 7 1" 2671)
	    (is "32 14⊥7" 105)
	    (is "1760 3 12⊥2 2 5" 101)
	    (is "1760 3 12⊥3 3⍴1 2 1 5 0 2 2 3 7" #(98 75 67))
	    (is "(3 3⍴1760 3 12)⊥(3 3⍴2 2 5 1 4 9 6 6 7)" #2A((90 126 295) (90 126 295) (90 126 295))))))

 (functions
  (with (:name :lexical-functions-special)
	(:tests-profile :title "Special Function Tests")
	(:demo-profile :title "Special Function Demos"
		       :description "These functions expose features of the language that aren't directly related to computing or transforming array values."))
  (⊢ (has :titles ("Identity" "Right"))
     (ambivalent #'identity
		 (lambda (omega alpha)
		   (declare (ignore alpha))
		   omega))
     (tests (is "⊢77" 77)
	    (is "55⊢77" 77)))
  (⊣ (has :titles ("Empty" "Left"))
     (ambivalent (lambda (omega)
		   (declare (ignore omega))
		   (make-array (list 0)))
		 (lambda (omega alpha)
		   (declare (ignore omega))
		   alpha))
     (tests (is "⊣77" #())
	    (is "55⊣77" 55)))
  (⍕ (has :titles ("Format" "Format At Precision"))
     (ambivalent (lambda (omega)
		   (let ((omega (enclose-atom omega)))
		     (array-impress omega :collate t
				    :format (lambda (n) (print-apl-number-string n t print-precision)))))
		 (lambda (omega alpha)
		   (let ((alpha (disclose alpha))
			 (omega (enclose-atom omega)))
		     (if (not (integerp alpha))
			 (error (concatenate 'string "The left argument to ⍕ must be an integer specifying"
					     " the precision at which to print floating-point numbers."))
			 (array-impress omega :collate t
					:format (lambda (n) (print-apl-number-string n t alpha)))))))
     (tests (is "⍕3 4⍴⍳9" #2A((#\1 #\  #\2 #\  #\3 #\  #\4) (#\5 #\  #\6 #\  #\7 #\  #\8)
			      (#\9 #\  #\1 #\  #\2 #\  #\3)))
	    (is "⍕2 3 4⍴⍳9" #3A(((#\1 #\  #\2 #\  #\3 #\  #\4) (#\5 #\  #\6 #\  #\7 #\  #\8)
				 (#\9 #\  #\1 #\  #\2 #\  #\3))
				((#\4 #\  #\5 #\  #\6 #\  #\7) (#\8 #\  #\9 #\  #\1 #\  #\2)
				 (#\3 #\  #\4 #\  #\5 #\  #\6))))
	    (is "3⍕○3 4⍴⍳9" #2A((#\  #\3 #\. #\1 #\4 #\  #\  #\6 #\. #\2 #\8
				     #\  #\  #\9 #\. #\4 #\2 #\  #\1 #\2 #\. #\6 #\ )
				(#\1 #\5 #\. #\7 #\  #\  #\1 #\8 #\. #\8 #\ 
				     #\  #\2 #\2 #\. #\0 #\  #\  #\2 #\5 #\. #\1 #\ )
				(#\2 #\8 #\. #\3 #\  #\  #\  #\3 #\. #\1 #\4
				     #\  #\  #\6 #\. #\2 #\8 #\  #\  #\9 #\. #\4 #\2)))
	    (is "5⍕○3 4⍴⍳9" #2A((#\  #\3 #\. #\1 #\4 #\1 #\6 #\  #\  #\6 #\. #\2 #\8 #\3 #\2 #\  #\  #\9
				     #\. #\4 #\2 #\4 #\8 #\  #\1 #\2 #\. #\5 #\6 #\6 #\ )
				(#\1 #\5 #\. #\7 #\0 #\8 #\  #\  #\  #\1 #\8 #\. #\8 #\5 #\  #\  #\2 #\1
				     #\. #\9 #\9 #\1 #\  #\  #\2 #\5 #\. #\1 #\3 #\3 #\ )
				(#\2 #\8 #\. #\2 #\7 #\4 #\  #\  #\  #\3 #\. #\1 #\4 #\1 #\6 #\  #\  #\6
				     #\. #\2 #\8 #\3 #\2 #\  #\  #\9 #\. #\4 #\2 #\4 #\8)))))
  (⍎ (has :title "Evaluate")
     (symbolic :special-lexical-form-evaluate)
     (tests (is "⍎'1+1'" 2)
	    (is "⍎'5','+3 2 1'" #(8 7 6))))
  (← (has :title "Assign")
     (symbolic :special-lexical-form-assign)
     (tests (is "x←55 ⋄ x" 55)
	    (is "x←2 3 4⍴⍳9 ⋄ x[;1;]←7 ⋄ x" #3A(((7 7 7 7) (5 6 7 8) (9 1 2 3))
						((7 7 7 7) (8 9 1 2) (3 4 5 6))))))
  (∘ (has :title "Find Outer Product, Not Inner")
     (symbolic :outer-product-designator)))
 ;; APL's character-represented operators, which take one or two functions or arrays as input
 ;; and generate a function

 (operators
  (with (:name :lexical-operators-lateral)
	(:tests-profile :title "Lateral Operator Tests")
	(:demo-profile :title "Lateral Operator Demos"
		       :description "Lateral operators take a single operand function to their left, hence the name 'lateral.' The combination of operator and function yields another function which may be applied to one or two arguments depending on the operator."))
  (/ (has :title "Reduce")
     (lateral (lambda (operand workspace axes)
		(let ((function (gensym)) (input (gensym)) (axis (gensym)) (item (gensym)))
		  (over-operator-template
		   axes (resolve-function :dyadic operand)
		   :for-vector `(lambda (,function ,input) (reduce ,function (reverse ,input)))
		   :for-array `(lambda (,function ,input ,axis)
				 (each-scalar t (aops:margin (lambda (,item) (reduce ,function (reverse ,item)))
							     ,input ,axis)))))))
     (tests (is "+/1 2 3 4 5" 15)
	    (is "⊢/⍳5" 5)
	    (is "+/3 4⍴⍳12" #(10 26 42))
	    (is "-/3 4⍴⍳12" #(-2 -2 -2))
	    (is "+/[1]3 4⍴⍳12" #(15 18 21 24))
	    (is "fn←{⍺+⍵} ⋄ fn/1 2 3 4 5" 15)
	    (is "⌊10000×{⍺+÷⍵}/40/1" 16180)))
  (⌿ (has :title "Reduce First")
     (lateral (lambda (operand workspace axes)
		(let ((function (gensym)) (input (gensym)) (axis (gensym)) (item (gensym)))
		  (over-operator-template
		   (or axes :first) (resolve-function :dyadic operand)
		   :for-vector `(lambda (,function ,input) (vector (reduce ,function (reverse ,input))))
		   :for-array `(lambda (,function ,input ,axis)
				 (each-scalar t (aops:margin (lambda (,item)
							       (disclose (reduce ,function (reverse ,item))))
							     ,input ,axis)))))))
     (tests (is "+⌿3 4⍴⍳12" #(15 18 21 24))
	    (is "-⌿3 4⍴⍳12" #(5 6 7 8))
	    (is "{⍺×⍵+3}⌿3 4⍴⍳12" #(63 162 303 492))
	    (is "+⌿[2]3 4⍴⍳12" #(10 26 42))))
  (\\ (has :title "Scan")
      (lateral (lambda (operand workspace axes)
		 (let ((function (gensym)) (input (gensym)) (axis (gensym)))
		   (over-operator-template
		    axes (resolve-function :dyadic operand)
		    :for-vector `(lambda (,function ,input) (funcall (make-back-scanner ,function) ,input))
		    :for-array `(lambda (,function ,input ,axis)
				  (each-scalar t (apply-marginal (make-back-scanner ,function)
								 ,input (if ,axis ,axis (1- (rank ,input))))))))))
      (tests (is "+\\1 2 3 4 5" #(1 3 6 10 15))
	     (is "+\\3 4⍴⍳12" #2A((1 3 6 10) (5 11 18 26) (9 19 30 42)))
	     (is "+\\[1]3 4⍴⍳12" #2A((1 2 3 4) (6 8 10 12) (15 18 21 24)))))
  (⍀ (has :title "Scan First")
     (lateral (lambda (operand workspace axes)
		(let ((function (gensym)) (input (gensym)) (axis (gensym)))
		  (over-operator-template
		   (or axes :first) (resolve-function :dyadic operand)
		   :for-vector `(lambda (,function ,input) (funcall (make-back-scanner ,function) ,input))
		   :for-array `(lambda (,function ,input ,axis)
				 (each-scalar t (apply-marginal (make-back-scanner ,function)
								,input (if ,axis ,axis (1- (rank ,input))))))))))
     (tests (is "+⍀1 2 3 4 5" #(1 3 6 10 15))
	    (is "+⍀3 4⍴⍳12" #2A((1 2 3 4) (6 8 10 12) (15 18 21 24)))
	    (is "{⍺×⍵+3}⍀3 4⍴⍳12" #2A((1 2 3 4) (20 30 42 56) (288 450 660 924)))
	    (is "+⍀[2]3 4⍴⍳12" #2A((1 3 6 10) (5 11 18 26) (9 19 30 42)))))
  (\¨ (has :title "Each")
      (lateral (lambda (operand workspace axes)
		 (declare (ignore axes))
		 (let ((operation-monadic (resolve-function :monadic operand))
		       (operation-dyadic (resolve-function :dyadic operand))
		       (op-symbol (or-functional-character operand :fn))
		       (index (gensym)) (item (gensym)) (omega (gensym)) (alpha (gensym))
		       (a (gensym)) (o (gensym)))
		   (flet ((expand-dyadic (a1 a2 &optional reverse)
			    (let ((call (if reverse `(apl-call ,op-symbol ,operation-dyadic
							       (enclose-atom (aref ,a1 ,index)) ,a2)
					    `(apl-call ,op-symbol ,operation-dyadic ,a2
						       (enclose-atom (aref ,a1 ,index))))))
			      `(let ((,a2 (if (not (arrayp (row-major-aref ,a2 0)))
					      ,a2 (disclose ,a2))))
				 (make-array (dims ,a1) :initial-contents (loop :for ,index :below (length ,a1)
									     :collect (each-scalar t ,call)))))))
		     `(lambda (,omega &optional ,alpha)
			(declare (ignorable ,alpha))
			(each-scalar
			 t ,(if (or (not (listp operation-dyadic))
				    (not (listp (second operation-dyadic)))
				    (< 1 (length (second operation-dyadic))))
				;; don't create the dyadic clauses if the function being passed is monadic-only
				`(if ,alpha (cond ((= 1 (length ,omega))
						   ,(expand-dyadic alpha omega))
						  ((= 1 (length ,alpha))
						   ,(expand-dyadic omega alpha t))
						  ((= (length ,alpha) (length ,omega))
						   (aops:each (lambda (,o ,a)
								(apl-call ,op-symbol ,operation-dyadic
									  (enclose-atom ,o) (enclose-atom ,a)))
							      ,omega ,alpha))
						  (t (error "Mismatched argument lengths to ¨.")))
				     (aops:each (lambda (,item) (apl-call ,op-symbol ,operation-monadic ,item))
						,omega))
				`(aops:each (lambda (,item) (apl-call ,op-symbol ,operation-monadic ,item))
					    ,omega))))))))
      (tests (is "⍳¨1 2 3" #(#(1) #(1 2) #(1 2 3)))
	     (is "3⍴¨1 2 3" #(#(1 1 1) #(2 2 2) #(3 3 3)))
	     (is "3 4 5⍴¨3" #(#(3 3 3) #(3 3 3 3) #(3 3 3 3 3)))
	     (is "1 ¯1⌽¨⊂1 2 3 4 5" #(#(2 3 4 5 1) #(5 1 2 3 4)))))
  (⍨ (has :title "Commute")
     (lateral (lambda (operand workspace axes)
		(declare (ignore axes))
		(let ((omega (gensym)) (alpha (gensym)))
		  `(lambda (,omega &optional ,alpha)
		     (apl-call ,(or-functional-character operand :fn)
			       ,(resolve-function :dyadic operand)
			       (if ,alpha ,alpha ,omega)
			       ,omega)))))
     (tests (is "5-⍨10" 5)
	    (is "+⍨10" 20)
	    (is "fn←{⍺+3×⍵} ⋄ 16 fn⍨8" 56)))
  (⌸ (has :title "Key")
     (lateral (lambda (operand workspace axes)
		(declare (ignore axes))
		(let ((key (gensym)) (keys (gensym)) (key-test (gensym)) (indices-of (gensym))
		      (key-table (gensym)) (key-list (gensym)) (item-sets (gensym)) (li (gensym))
		      (item (gensym)) (items (gensym)) (vector (gensym)) (coords (gensym))
		      (alpha (gensym)) (omega (gensym)))
		  `(lambda (,omega &optional ,alpha)
		     (let* ((,keys (if ,alpha ,alpha ,omega))
			    (,key-test #'equalp)
			    (,indices-of (lambda (,item ,vector)
					   (loop :for ,li :below (length ,vector)
					      :when (funcall ,key-test ,item (aref ,vector ,li))
					      :collect (+ index-origin ,li))))
			    (,key-table (make-hash-table :test ,key-test))
			    (,key-list))
		       (across ,keys (lambda (,item ,coords)
				       (if (loop :for ,key :in ,key-list :never (funcall ,key-test ,item ,key))
					   (setq ,key-list (cons ,item ,key-list)))
				       (setf (gethash ,item ,key-table)
					     (cons (apply #'aref (cons ,omega ,coords))
						   (gethash ,item ,key-table)))))
		       (let* ((,item-sets (loop :for ,key :in (reverse ,key-list)
					     :collect (apl-call ,(or-functional-character operand :fn)
								,(resolve-function :dyadic operand)
								(let ((,items (if ,alpha (gethash ,key ,key-table)
										  (funcall ,indices-of
											   ,key ,keys))))
								  (funcall (if (= 1 (length ,items))
									       (lambda (v) (vector (vector v)))
									       #'identity)
									   (make-array (list (length ,items))
										       :initial-contents
										       (reverse ,items))))
								,key))))
			 (mix-arrays (vector 1) (apply #'vector ,item-sets))))))))
     (tests (is "fruit←'Apple' 'Orange' 'Apple' 'Pear' 'Orange' 'Peach' 'Pear' 'Pear'
    quantities ← 12 3 2 6 8 16 7 3 ⋄ fruit {⍺ ⍵}⌸ quantities"
    	        #2A(("Apple" #(12 2)) ("Orange" #(3 8)) ("Pear" #(6 7 3)) ("Peach" #(16))))
	    (is "fruit←'Apple' 'Orange' 'Apple' 'Pear' 'Orange' 'Peach' ⋄ {⍴⍵}⌸ fruit"
		#2A((2) (2) (1) (1))))))

 (operators
  (with (:name :lexical-operators-pivotal)
	(:tests-profile :title "Pivotal Operator Tests")
	(:demo-profile :title "Pivotal Operator Demos"
		       :description "Pivotal operators are so called because they are entered between two operands. Depending on the operator, these operands may be functions or array values, with the combination yielding a new function."))
  (\. (has :title "Inner/Outer Product")
      (pivotal (lambda (right left workspace)
		 (let ((op-right `(lambda (alpha omega)
				    (apl-call ,(or-functional-character right :fn)
					      ,(resolve-function :dyadic right)
					      omega alpha)))
		       (right-resolved (resolve-function :dyadic right))
		       (op-left (let ((left-op (or (resolve-function :symbolic left)
						   (resolve-function :dyadic left)))
				      (left-sym (or-functional-character left :fn)))
				  (if (not (eq :outer-product-designator left-op))
				      `(lambda (alpha omega) (apl-call ,left-sym ,left-op omega alpha))
				      left-op)))
		       (inverse (gensym)) (result (gensym)) (arg1 (gensym)) (arg2 (gensym))
		       (element (gensym)) (alpha (gensym)) (omega (gensym)) (a (gensym)) (o (gensym))
		       (placeholder (gensym)))
		   (if (eq :outer-product-designator op-left)
		       `(lambda (,omega ,alpha)
			  (if (is-unitary ,omega)
			      (each-scalar t (aops:each (lambda (,element)
							  (let ((,a ,element)
								(,o (disclose-unitary-array (disclose ,omega))))
							    (apl-call :fn ,op-right ,a ,o)))
							,alpha))
			      (let ((,inverse (aops:outer (lambda (,o ,a)
							    (let ((,o (if (arrayp ,o) ,o (vector ,o)))
								  (,a (if (arrayp ,a) ,a (vector ,a))))
							      ',(resolve-function :dyadic right)
							      (if (is-unitary ,o)
								  ;; swap arguments in case of a
								  ;; unitary omega argument
								  ;; ,op-right ',right
								  (let ((,placeholder ,a))
								    (setq ,a ,o
									  ,o ,placeholder)))
							      (each-scalar t (funcall
									      ;; disclose the output of
									      ;; user-created functions; otherwise
									      ;; fn←{⍺×⍵+1}
									      ;; 1 2 3∘.fn 4 5 6 (for example)
									      ;; will fail
									      ,(if (or (symbolp right-resolved)
										       (and (listp right-resolved)
											    (eq 'scalar-function
												(first
												 right-resolved))))
										   '#'disclose '#'identity)
									      (apl-call :fn ,op-right ,a ,o)))))
							  ,alpha ,omega)))
				(each-scalar t (if (not (is-unitary ,alpha))
						   ,inverse (aops:permute (reverse (alexandria:iota
										    (rank ,inverse)))
									  ,inverse))))))
		       `(lambda (,omega ,alpha)
			  (if (and (vectorp ,omega)
				   (vectorp ,alpha))
			      (funcall (lambda (,result)
					 (if (not (and (arrayp ,result)
						       (< 1 (rank ,result))))
					     ,result (vector ,result)))
				       ;; enclose the result in a vector if its rank is > 1
				       ;; to preserve the rank of the result
				       (reduce ,op-left (aops:each (lambda (e) (aops:each #'disclose e))
								   (apply-scalar ,op-right ,alpha ,omega))))
			      (each-scalar t (array-inner-product ,alpha ,omega
								  (lambda (,arg1 ,arg2)
								    (if (or (arrayp ,arg1) (arrayp ,arg2))
									(apply-scalar ,op-right ,arg1 ,arg2)
									(funcall ,op-right ,arg1 ,arg2)))
								  ,op-left))))))))
      (tests (is "2+.×3 4 5" 24)
	     (is "2 3 4+.×8 15 21" 145)
	     (is "2 3 4+.×3 3⍴3 1 4 1 5 9 2 6 5" #(17 41 55))
	     (is "(3 3⍴3 1 4 1 5 9 2 6 5)+.×2 3 4" #(25 53 42))
	     (is "{⍵ ⍵+.+⍵ ⍵} 3 3⍴⍳9" #(#2A((4 8 12) (16 20 24) (28 32 36))))
	     (is "4 5 6∘.+20 30 40 50" #2A((24 34 44 54) (25 35 45 55) (26 36 46 56)))
	     (is "1 2 3∘.-1 2 3" #2A((0 -1 -2) (1 0 -1) (2 1 0)))
	     (is "1 2 3∘.⍴1 2 3" #2A((#(1) #(2) #(3))(#(1 1) #(2 2) #(3 3)) (#(1 1 1) #(2 2 2) #(3 3 3))))
	     (is "1 2 3∘.⍴⊂1 2 3" #(#(1) #(1 2) #(1 2 3)))
	     (is "1 2 3∘.⌽⊂1 2 3" #(#(2 3 1) #(3 1 2) #(1 2 3)))
	     (is "1 2 3∘.⌽⊂4 5 6 7" #(#(5 6 7 4) #(6 7 4 5) #(7 4 5 6)))
	     (is "fn←{⍺×⍵+1} ⋄ 1 2 3∘.fn 4 5 6" #2A((5 6 7) (10 12 14) (15 18 21)))))
  (∘ (has :title "Compose")
     (pivotal (lambda (right left workspace)
		(let* ((fn-right (verify-function right))
		       (fn-left (verify-function left))
		       ;; check whether the left function is monadic so that the (if alpha) clause
		       ;; can be omitted if so
		       (is-confirmed-monadic (or (and (listp left)
						      (eql 'lambda (first left))
						      (= 1 (length (second left))))
						 (not (resolve-function :dyadic left))))
		       (alpha (gensym)) (omega (gensym)) (processed (gensym)))
		  `(lambda (,omega &optional ,alpha)
		     (declare (ignorable ,alpha))
		     ,(if (and fn-right fn-left)
			  (let ((clauses (loop :for type :in (list :dyadic :monadic)
					    :collect `(apl-call ,(or-functional-character left :fn)
								,(resolve-function type left)
								,processed ,@(if (eq :dyadic type)
										 (list alpha))))))
			    `(let ((,processed (apl-call ,(or-functional-character right :fn)
							 ,(resolve-function :monadic right)
							 ,omega)))
			       ,(if is-confirmed-monadic (second clauses)
				    `(if ,alpha ,@clauses))))
			  `(apl-call :fn ,(resolve-function :dyadic (if fn-right right left))
				     ,(if (not fn-right) right omega)
				     ,(if (not fn-left) left omega)))))))
     (tests (is "fn←⍴∘⍴ ⋄ fn 2 3 4⍴⍳9" 3)
	    (is "⍴∘⍴ 2 3 4⍴⍳9" 3)
	    (is "⍴∘⍴∘⍴ 2 3 4⍴⍳9" 1)
	    (is "÷∘5 ⊢30" 6)
	    (is "⌊10000×(+∘*∘0.5) 4 16 25" #(56487 176487 266487))
	    (is "fn←5∘- ⋄ fn 2" 3)
	    (is "⌊0.5∘+∘*5 8 12" #(148 2981 162755))
	    (is "⌊10000×+∘÷/40/1" 16180)
	    (is "fn←+/ ⋄ fn∘⍳¨2 5 8" #(3 15 36))
	    (is "3 4⍴∘⍴2 4 5⍴9" #2A((2 4 5 2) (4 5 2 4) (5 2 4 5)))))
  (⍤ (has :title "Rank")
     (pivotal (lambda (right left workspace)
		(let ((rank (gensym)) (orank (gensym)) (arank (gensym)) (fn (gensym))
		      (romega (gensym)) (ralpha (gensym)) (alpha (gensym)) (omega (gensym))
		      (o (gensym)) (a (gensym)) (r (gensym)))
		  `(lambda (,omega &optional ,alpha)
		     (let* ((,rank (disclose ,right))
			    (,orank (rank ,omega))
			    (,arank (rank ,alpha))
			    (,fn (if (not ,alpha)
				     (lambda (,o) (apl-call ,(or-functional-character left :fn)
							    ,(resolve-function :monadic left)
							    ,o))
				     (lambda (,o ,a) (apl-call ,(or-functional-character left :fn)
							       ,(resolve-function :dyadic left)
							       ,o ,a))))
			    (,romega (if (and ,omega (< ,rank ,orank))
					 (re-enclose ,omega (each (lambda (,r) (- ,r index-origin))
								  (make-array (list ,rank)
									      :initial-contents
									      (nthcdr (- ,orank ,rank)
										      (iota ,orank :start
											    index-origin)))))))
			    (,ralpha (if (and ,alpha (< ,rank ,arank))
					 (re-enclose ,alpha (each (lambda (,r) (- ,r index-origin))
								  (make-array (list ,rank)
									      :initial-contents
									      (nthcdr (- ,arank ,rank)
										      (iota ,arank :start
											    index-origin))))))))
		       (if ,alpha (merge-arrays (if ,romega (if ,ralpha (each ,fn ,romega ,ralpha)
								(each ,fn ,romega
								      (make-array (dims ,romega)
										  :initial-element ,alpha)))
						    (if ,ralpha (each ,fn (make-array (dims ,ralpha)
										      :initial-element ,omega)
								      ,ralpha)
							(funcall ,fn ,omega ,alpha))))
			   (if ,romega (merge-arrays (each ,fn ,romega))
			       (funcall ,fn ,omega))))))))
     (tests (is "⊂⍤2⊢2 3 4⍴⍳9" #(#2A((1 2 3 4) (5 6 7 8) (9 1 2 3))
				 #2A((4 5 6 7) (8 9 1 2) (3 4 5 6))))
	    (is "{(⊂⍋⍵)⌷⍵}⍤1⊢3 4 5⍴⍳9" #3A(((1 2 3 4 5) (1 6 7 8 9) (2 3 4 5 6) (1 2 7 8 9))
					   ((3 4 5 6 7) (1 2 3 8 9) (4 5 6 7 8) (1 2 3 4 9))
					   ((5 6 7 8 9) (1 2 3 4 5) (1 6 7 8 9) (2 3 4 5 6))))
	    (is "10 20 30 40+⍤1⊢4 4 4⍴⍳16"
		#3A(((11 22 33 44) (15 26 37 48) (19 30 41 52) (23 34 45 56))
		    ((11 22 33 44) (15 26 37 48) (19 30 41 52) (23 34 45 56))
		    ((11 22 33 44) (15 26 37 48) (19 30 41 52) (23 34 45 56))
		    ((11 22 33 44) (15 26 37 48) (19 30 41 52) (23 34 45 56))))
	    (is "(⍳5)+⍤1⊢1 5⍴⍳5" #2A((2 4 6 8 10)))
	    (is "fn←{⍺+2×⍵} ⋄ 15 25 35 fn⍤1⊢2 2 3⍴⍳8" #3A(((17 29 41) (23 35 47)) ((29 41 37) (19 31 43))))))
  (⍣ (has :title "Power")
     (pivotal (lambda (right left workspace)
		(let ((op-right (or (resolve-function :dyadic right)
				    right))
		      (op-left (or (resolve-function :monadic left)
				   left))
		      (sym-right (or-functional-character right :fn))
		      (sym-left (or-functional-character left :fn)) (alpha (gensym)) (omega (gensym))
		      (arg (gensym)) (index (gensym)) (prior-arg (gensym)))
		  (cond ((not (verify-function op-right))
			 ;; if the right operand is not a function, it must be a number counting the times
			 ;; the left function is to be compounded
			 `(lambda (,omega &optional ,alpha)
			    (let ((,arg (disclose ,omega)))
			      (loop :for ,index :below (disclose ,op-right)
				 :do (setq ,arg (enclose (if ,alpha (apl-call ,sym-left ,op-left ,arg ,alpha)
							     (apl-call ,sym-left ,op-left ,arg)))))
			      ,arg)))
			((verify-function op-right)
			 ;; if the right operand is a function, it expresses the criteria for ending the
			 ;; activity of the left function
			 `(lambda (,omega &optional ,alpha)
			    (declare (ignorable ,alpha))
			    (let ((,arg ,omega)
				  (,prior-arg ,omega))
			      (loop :while (= 0 (disclose (apl-call ,sym-right ,op-right ,prior-arg ,arg)))
				 :do (setq ,prior-arg ,arg
					   ,arg (enclose (if ,alpha (apl-call ,sym-left ,op-left ,arg ,alpha)
							     (apl-call ,sym-left ,op-left ,arg)))))
			      ,arg)))))))
     (tests (is "fn←{2+⍵}⍣3 ⋄ fn 5" 11)
	    (is "{2+⍵}⍣3⊢9" 15)
	    (is "2{⍺×2+⍵}⍣3⊢9" 100)
	    (is "fn←{2+⍵}⍣{10<⍺} ⋄ fn 2" 12)
	    (is "fn←{2+⍵}⍣{10<⍵} ⋄ fn 2" 14)
	    (is "fn←{⍵×2} ⋄ fn⍣3⊢4" 32)))
  (@ (has :title "At")
     (pivotal (lambda (right left workspace)
		(let* ((index (gensym)) (omega-var (gensym)) (output (gensym)) (item (gensym))
		       (coord (gensym)) (coords (gensym)) (alen (gensym))
		       (alpha (gensym)) (omega (gensym)))
		  (cond ((verify-function right)
			 `(lambda (,omega &optional ,alpha)
			    (declare (ignorable ,alpha))
			    (each-scalar (lambda (,item ,coords)
					   (declare (ignore ,coords))
					   (if (/= 0 (disclose (apl-call ,(or-functional-character right :fn)
									 ,(resolve-function :monadic right)
									 ,item)))
					       (disclose ,(cond ((verify-function left)
								 `(if ,alpha
								      (apl-call ,(or-functional-character left :fn)
										,(resolve-function :dyadic left)
										,item ,alpha)
								      (apl-call ,(or-functional-character left :fn)
										,(resolve-function :monadic left)
										,item)))
								(t left)))
					       ,item))
					 ,omega ,alpha)))
			(t `(lambda (,omega)
			      (let* ((,omega-var (apply-scalar #'- ,right index-origin))
				     (,output (make-array (dims ,omega)))
				     (,coord))
				;; make copy of array without type constraint; TODO: is there a more
				;; efficient way to do this?
				(across ,omega (lambda (,item ,coords)
						 (setf (apply #'aref (cons ,output ,coords))
						       ,item)))
				(loop :for ,index :below (length ,omega-var)
				   :do (setq ,coord (aref ,omega-var ,index))
				   (choose ,output (if (arrayp ,coord)
						       (mapcar #'list (array-to-list ,coord))
						       (list (list ,coord)))
					   :set ,@(cond ((verify-function left)
							 (list (resolve-function :monadic left)))
							(t `((if (is-unitary ,left)
								 (disclose ,left)
								 (lambda (,item ,coords)
								   (declare (ignore ,item))
								   (let ((,alen (if (not (listp ,coord))
										    1 (length ,coord))))
								     (choose
								      ,left
								      (mapcar #'list
									      (append (list ,index)
										      (nthcdr ,alen
											      ,coords)))))))
							     :set-coords t)))))
				,output)))))))
     (tests (is "20 20@3 8⍳9" #(1 2 20 4 5 6 7 20 9))
	    (is "((2 5⍴0 1)@2 5) 5 5⍴⍳9" #2A((1 2 3 4 5) (0 1 0 1 0) (2 3 4 5 6)
					     (7 8 9 1 2) (1 0 1 0 1)))
	    (is "0@(3∘|) ⍳9" #(0 0 3 0 0 6 0 0 9))
	    (is "÷@3 5 ⍳9" #(1 2 1/3 4 1/5 6 7 8 9))
	    (is "{⍵×2}@{⍵>3}⍳9" #(1 2 3 8 10 12 14 16 18))
	    (is "fn←{⍺+⍵×12} ⋄ test←{0=3|⍵} ⋄ 4 fn@test ⍳12" #(1 2 40 4 5 76 7 8 112 10 11 148))))
  (⌺ (has :title "Stencil")
     (pivotal (lambda (right left workspace)
		(let ((omega (gensym)) (window-dims (gensym)) (movement (gensym))
		      (op-left (or (resolve-function :dyadic left)
				   left)))
		  `(lambda (,omega)
		     (cond ((< 2 (rank ,right))
			    (error "The right operand of ⌺ may not have more than 2 dimensions."))
			   ((not ,(verify-function left))
			    (error "The left operand of ⌺ must be a function."))
			   (t (let ((,window-dims (if (= 1 (rank ,right))
						      ,right (choose ,right (list 0))))
				    (,movement (if (= 2 (rank ,right))
						   (choose ,right (list 1))
						   (make-array (list (length ,right))
							       :element-type 'fixnum :initial-element 1))))
				(merge-arrays (stencil ,omega ,op-left ,window-dims ,movement)))))))))
     (tests (is "{⊂⍵}⌺(⍪3 2)⍳8" #(#(0 1 2) #(2 3 4) #(4 5 6) #(6 7 8)))
	    (is "{⊂⍵}⌺(⍪5 2)⍳9" #(#(0 0 1 2 3) #(1 2 3 4 5) #(3 4 5 6 7) #(5 6 7 8 9) #(7 8 9 0 0)))
	    (is "{⊂⍵}⌺2⍳8" #(#(1 2) #(2 3) #(3 4) #(4 5) #(5 6) #(6 7) #(7 8)))
	    (is "{⊂⍵}⌺4⍳8" #(#(0 1 2 3) #(1 2 3 4) #(2 3 4 5) #(3 4 5 6) #(4 5 6 7) #(5 6 7 8) #(6 7 8 0)))
	    (is "{⊂⍵}⌺4⍳9" #(#(0 1 2 3) #(1 2 3 4) #(2 3 4 5) #(3 4 5 6)
			     #(4 5 6 7) #(5 6 7 8) #(6 7 8 9) #(7 8 9 0)))
	    (is "{⊂⍵}⌺(⍪4 2)⍳8" #(#(0 1 2 3) #(2 3 4 5) #(4 5 6 7) #(6 7 8 0)))
	    (is "{⊂⍵}⌺(⍪6 2)⍳8" #(#(0 0 1 2 3 4) #(1 2 3 4 5 6) #(3 4 5 6 7 8) #(5 6 7 8 0 0)))
	    (is "{⍵}⌺3 3⊢3 3⍴⍳9" #4A((((0 0 0) (0 1 2) (0 4 5)) ((0 0 0) (1 2 3) (4 5 6))
				      ((0 0 0) (2 3 0) (5 6 0)))
				     (((0 1 2) (0 4 5) (0 7 8)) ((1 2 3) (4 5 6) (7 8 9))
				      ((2 3 0) (5 6 0) (8 9 0)))
				     (((0 4 5) (0 7 8) (0 0 0)) ((4 5 6) (7 8 9) (0 0 0))
				      ((5 6 0) (8 9 0) (0 0 0)))))
	    (is "{⊂⍺ ⍵}⌺3 3⊢3 3⍴⍳9" #2A((#(#(1 1) #2A((0 0 0) (0 1 2) (0 4 5)))
					  #(#(1 0) #2A((0 0 0) (1 2 3) (4 5 6)))
					  #(#(1 -1) #2A((0 0 0) (2 3 0) (5 6 0))))
					(#(#(0 1) #2A((0 1 2) (0 4 5) (0 7 8)))
					  #(#(0 0) #2A((1 2 3) (4 5 6) (7 8 9)))
					  #(#(0 -1) #2A((2 3 0) (5 6 0) (8 9 0))))
					(#(#(-1 1) #2A((0 4 5) (0 7 8) (0 0 0)))
					  #(#(-1 0) #2A((4 5 6) (7 8 9) (0 0 0)))
					  #(#(-1 -1) #2A((5 6 0) (8 9 0) (0 0 0)))))))))

 ;; tests for general language functions not associated with a particular function or operator
 (test-set
  (with (:name :general-tests)
	(:tests-profile :title "General Tests")
	(:demo-profile :title "General Demos"
		       :description "These are demos of basic April language features."))
  (for "Scalar value." "5" 5)
  (for "Array value." "1 2 3" #(1 2 3))
  (for "Scalar values operated upon." "3×3" 9)
  (for "Array and scalar values operated upon." "5+1 2 3" #(6 7 8))
  (for "Two array values operated upon." "4 12 16÷2 3 4" #(2 4 4))
  (for "Value assigned to a variable." "x←9" 9)
  (for "Value assigned to a variable and operated upon." "3+x←9" 12)
  (for "Two statements on one line separated by a [⋄ diamond] character."
       "a←9 ⋄ a×2 3 4" #(18 27 36))
  (for "Basic function definition and use, with comments delineated by the [⍝ lamp] character."
       "⍝ This code starts with a comment.
    f1←{⍵+3} ⋄ f2←{⍵×2} ⍝ A comment after the functions are defined.
    ⍝ This is another comment.
    v←⍳3 ⋄ f2 f1 v,4 5"
       #(8 10 12 14 16))
  (for "Monadic inline function." "{⍵+3} 3 4 5" #(6 7 8))
  (for "Dyadic inline function." "1 2 3 {⍺×⍵+3} 3 4 5" #(6 14 24))
  (for "Vector of input variables and discrete values processed within a function."
       "fn←{3+⍵} ⋄ {fn 8 ⍵} 9" #(11 12))
  (for "Variable-referenced values, including an element within an array, in a vector."
       "a←9 ⋄ b←2 3 4⍴⍳9 ⋄ 1 2 a 3 b[1;2;1]" #(1 2 9 3 5))
  (for "Application of functions to indexed array elements."
       "g←2 3 4 5 ⋄ 9,g[2],3 4" #(9 3 3 4))
  (for "Assignment of an element within an array."
       "a←2 3⍴⍳9 ⋄ a[1;2]←20 ⋄ a" #2A((1 20 3) (4 5 6)))
  (for "Selection from an array with multiple elided dimensions."
       "(2 3 3 4 5⍴⍳9)[2;;3;;2]" #2A((6 2 7 3) (3 8 4 9) (9 5 1 6)))
  (for "Elided assignment."
       "a←2 3 4⍴⍳9 ⋄ a[2;;3]←0 ⋄ a" #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3)) ((4 5 0 7) (8 9 0 2) (3 4 0 6))))
  (for "Elision and indexed array elements."
       "(6 8⍴⍳9)[1 4;]" #2A((1 2 3 4 5 6 7 8) (7 8 9 1 2 3 4 5)))
  (for "As above but more complex."
       "(6 8 5⍴⍳9)[1 4;;2 1]"
       #3A(((2 1) (7 6) (3 2) (8 7) (4 3) (9 8) (5 4) (1 9))
	   ((5 4) (1 9) (6 5) (2 1) (7 6) (3 2) (8 7) (4 3))))
  (for "Indices of indices."
       "(6 8 5⍴⍳9)[1 4;;2 1][1;2 4 5;]" #2A((7 6) (8 7) (4 3)))
  (for "Array as array index."
       "(10+⍳9)[2 3⍴⍳9]" #2A((11 12 13) (14 15 16)))
  (for "Sub-coordinates of nested arrays." "(3 4⍴⍳9)[(1 2)(3 1)]" #(2 9))
  (for "Choose indexing of nested array sub-coordinates."
       "(3 4⍴⍳9)[2 2⍴⊂(2 3)]" #2A((7 7) (7 7)))
  (for "Reach indexing of components within sub-arrays."
       "(2 3⍴('JAN' 1)('FEB' 2)('MAR' 3)('APR' 4)('MAY' 5)('JUN' 6))[((2 3)1)((1 1)2)]"
       #("JUN" 1))
  (for "Assignment by function." "a←3 2 1 ⋄ a+←5 ⋄ a" #(8 7 6))
  (for "Assignment by function at index." "a←3 2 1 ⋄ a[2]+←5 ⋄ a" #(3 7 1))
  (for "Elided assignment of applied function's results."
       "a←2 3 4⍴⍳9 ⋄ a[2;;3]+←10 ⋄ a" #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3)) ((4 5 16 7) (8 9 11 2) (3 4 15 6))))
  (for "Operation over portions of an array."
       "a←4 8⍴⍳9 ⋄ a[2 4;1 6 7 8]+←10 ⋄ a"
       #2A((1 2 3 4 5 6 7 8) (19 1 2 3 4 15 16 17)
	   (8 9 1 2 3 4 5 6) (17 8 9 1 2 13 14 15)))
  (for "Glider 1." "(3 3⍴⍳9)∊1 2 3 4 8" #2A((1 1 1) (1 0 0) (0 1 0)))
  (for "Glider 2." "3 3⍴⌽⊃∨/1 2 3 4 8=⊂⍳9" #2A((0 1 0) (0 0 1) (1 1 1))))

 (test-set
  (with (:name :system-variable-function-tests)
	(:tests-profile :title "System Variable and Function Tests")
	(:demo-profile :title "System Variable and Function Demos"
		       :description "Demos illustrating the use of system variables and functions."))
  (for "Setting the index origin." "a←⍳3 ⋄ ⎕io←0 ⋄ a,⍳3" #(1 2 3 0 1 2))
  (for-printed "Setting the print precision." "⎕pp←3 ⋄ a←⍕*⍳3 ⋄ ⎕pp←6 ⋄ a,'  ',⍕*⍳3"
	       "2.72 7.39 20.1  2.71828 7.38906 20.0855")
  (for "Alphabetical and numeric vectors." "⎕a,⎕d" "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
  (for "Seven elements in the timestamp vector." "⍴⎕ts" 7))

 (test-set
  (with (:name :array-function-scalar-index-input-tests)
 	(:tests-profile :title "Array Function Scalar Index Input Tests"))
  (for "Scalar dyadic input to ~." "v←⍳5 ⋄ 1 2 3 4~v[2]" #(1 3 4))
  (for "Scalar monadic input to ⍳." "v←⍳5 ⋄ ⍳v[3]" #(1 2 3))
  (for "Scalar dyadic input to ⍳, right." "v←⍳5 ⋄ (2⍳v[3]),2 3⍳v[4]" #(2 3))
  (for "Scalar dyadic input to ⍳, left." "v←⍳5 ⋄ v[3]⍳⍳4" #(2 2 1 2))
  (for "Scalar monadic input to ⍴." "v←⍳5 ⋄ ⍴v[1]" 1)
  (for "Scalar dyadic input to ⍳, right." "v←⍳5 ⋄ 3⍴v[2]" #(2 2 2))
  (for "Scalar dyadic input to ⍳, left." "v←⍳5 ⋄ v[3]⍴3" #(3 3 3))
  (for "Scalar dyadic input to ⌷, right." "v←⍳5 ⋄ 1⌷v[3]" 3)
  (for "Scalar dyadic input to ⌷, left." "v←⍳5 ⋄ v[3]⌷2 4 6 8 10" 6)
  (for "Scalar monadic input to ≡." "v←⍳5 ⋄ ≡v[1]" 0)
  (for "Scalar dyadic input to ≡, right." "v←⍳5 ⋄ 3≡v[3]" 1)
  (for "Scalar dyadic input to ≡, left." "v←⍳5 ⋄ v[4]≡2" 0)
  (for "Scalar monadic input to ≢." "v←⍳5 ⋄ ≢v[2]" 1)
  (for "Scalar dyadic input to ≢, right." "v←⍳5 ⋄ v[5]≢5" 0)
  (for "Scalar dyadic input to ≢, left." "v←⍳5 ⋄ 3≢v[1]" 1)
  (for "Scalar monadic input to ∊." "v←⍳5 ⋄ ∊v[2]" 2)
  (for "Scalar dyadic input to ∊, right." "v←⍳5 ⋄ 2 3∊v[2]" #*10)
  (for "Scalar dyadic input to ∊, left." "v←⍳5 ⋄ v[3]∊3 4 5" 1)
  (for "Scalar dyadic input to ⍷, right." "v←⍳5 ⋄ v[2]⍷3 4⍴⍳9" #2A((0 1 0 0) (0 0 0 0) (0 0 1 0)))
  (for "Scalar dyadic input to ⍷, left." "v←⍳5 ⋄ 4⍷v[4]" 1)
  (for "Scalar monadic input to ⍸." "v←⍳5 ⋄ ⍸v[1]" 1)
  (for "Scalar dyadic input to ⍸, right." "v←⍳5 ⋄ 2 4 6 8⍸v[3]" 1)
  (for "Scalar monadic input to ,." "v←⍳5 ⋄ ,v[5]" 5)
  (for "Scalar dyadic input to ,, right." "v←⍳5 ⋄ 5 6,v[3]" #(5 6 3))
  (for "Scalar dyadic input to ,, left." "v←⍳5 ⋄ v[2],⍳3" #(2 1 2 3))
  (for "Scalar monadic input to ⍪." "v←⍳5 ⋄ ⍪v[4]" 4)
  (for "Scalar dyadic input to ⍪, right." "v←⍳5 ⋄ (2 3⍴⍳6)⍪v[3]" #2A((1 2 3) (4 5 6) (3 3 3)))
  (for "Scalar dyadic input to ⍪, left." "v←⍳5 ⋄ v[2]⍪⍳4" #(2 1 2 3 4))
  (for "Scalar monadic input to ↑." "v←⍳5 ⋄ ↑v[2]" 2)
  (for "Scalar dyadic input to ↑, right." "v←⍳5 ⋄ 2↑v[2]" #(2 0))
  (for "Scalar dyadic input to ↑, left." "v←⍳5 ⋄ v[3]↑⍳9" #(1 2 3))
  (for "Scalar monadic input to ↓." "v←⍳5 ⋄ ↓v[5]" 5)
  (for "Scalar dyadic input to ↓, right." "v←⍳5 ⋄ 2↓v[3]" #())
  (for "Scalar dyadic input to ↓, left." "v←⍳5 ⋄ v[4]↓⍳9" #(5 6 7 8 9))
  (for "Scalar monadic input to ⊂." "v←⍳5 ⋄ ⊂v[2]" 2)
  (for "Scalar dyadic input to ⊂, right." "v←⍳5 ⋄ 1⊂v[2]" #(#(2)))
  (for "Scalar dyadic input to ⊂, left." "v←⍳5 ⋄ v[1]⊂5" #(#(5)))
  (for "Scalar dyadic input to ⊆, left." "v←⍳5 ⋄ v[2]⊆⍳3" #(#(1 2 3)))
  (for "Scalar monadic input to ⊃." "v←⍳5 ⋄ ⊃v[3]" 3)
  (for "Scalar dyadic input to ⊃, left." "v←⍳5 ⋄ v[2]⊃2 4 6 8" 4)
  (for "Scalar dyadic input to ∩, right." "v←⍳5 ⋄ v[2]∩⍳4" 2)
  (for "Scalar dyadic input to ∩, left." "v←⍳5 ⋄ 4 5 6∩v[4]" 4)
  (for "Scalar monadic input to ∪." "v←⍳5 ⋄ ∪v[3]" 3)
  (for "Scalar dyadic input to ∪, right." "v←⍳5 ⋄ 2 3 4∪v[5]" #(2 3 4 5))
  (for "Scalar dyadic input to ∪, left." "v←⍳5 ⋄ v[2]∪⍳3" #(2 1 3))
  (for "Scalar monadic input to ⌽." "v←⍳5 ⋄ ⌽v[3]" 3)
  (for "Scalar dyadic input to ⌽, right." "v←⍳5 ⋄ 3⌽v[1]" 1)
  (for "Scalar dyadic input to ⌽, left." "v←⍳5 ⋄ v[3]⌽⍳5" #(4 5 1 2 3))
  (for "Scalar monadic input to ⊖." "v←⍳5 ⋄ ⊖v[4]" 4)
  (for "Scalar dyadic input to ⊖, right." "v←⍳5 ⋄ 2⊖v[5]" 5)
  (for "Scalar dyadic input to ⊖, left." "v←⍳5 ⋄ v[2]⊖⍳6" #(3 4 5 6 1 2))
  (for "Scalar monadic input to ⍉." "v←⍳5 ⋄ ⍉v[2]" 2)
  (for "Scalar dyadic input to ⍉, right." "v←⍳5 ⋄ 1⍉v[5]" 5)
  (for "Scalar dyadic input to ⍉, left." "v←⍳5 ⋄ v[1]⍉⍳3" #(1 2 3))
  (for "Scalar dyadic input to /, right." "v←⍳5 ⋄ 3/v[1]" #*111)
  (for "Scalar dyadic input to /, left." "v←⍳5 ⋄ v[2]/8" #(8 8))
  (for "Scalar dyadic input to ⌿, right." "v←⍳5 ⋄ 3⌿v[2]" #(2 2 2))
  (for "Scalar dyadic input to ⌿, left." "v←⍳5 ⋄ v[4]⌿7 8" #(7 7 7 7 8 8 8 8))
  (for "Scalar dyadic input to \, right." "v←⍳5 ⋄ 4\\v[2]" #(2 2 2 2))
  (for "Scalar dyadic input to \, left." "v←⍳5 ⋄ v[3]\\7" #(7 7 7))
  (for "Scalar dyadic input to ⍀, right." "v←⍳5 ⋄ 2⍀v[5]" #(5 5))
  (for "Scalar dyadic input to ⍀, left." "v←⍳5 ⋄ v[2]⍀1" #*11)
  (for "Scalar monadic input to ⍋." "v←⍳5 ⋄ ⍋v[2]" 1)
  (for "Scalar dyadic input to ⍋, right." "v←'cdef' ⋄ 'abcd'⍋v[2]" #(4 1 2 3))
  (for "Scalar monadic input to ⍒." "v←⍳5 ⋄ ⍒v[3]" 1)
  (for "Scalar dyadic input to ⍒, right." "v←'cdef' ⋄ 'abcd'⍒v[2]" #(1 2 3 4))
  (for "Scalar monadic input to ⌹." "v←⍳5 ⋄ ⌹v[3]" 1/3)
  (for "Scalar dyadic input to ⊤, right." "v←12 24 36 ⋄ 6 2 8⊤v[1]" #(0 1 4))
  (for "Scalar dyadic input to ⊤, left." "v←3 6 9 ⋄ v[3]⊤15" 6)
  (for "Scalar dyadic input to ⊥, right." "v←3 6 9 ⋄ v[2]⊥12 50" 122)
  (for "Scalar dyadic input to ⊥, left." "v←5 7 9 ⋄ 14⊥v[2]" 7))
 
 (test-set
  (with (:name :printed-format-tests)
	(:tests-profile :title "Printed Data Format Tests")
	(:demo-profile :title "Data Format Demos"
		       :description "More demos showing how different types of data are formatted in April."))
  (for-printed "Numeric vector." "1+1 2 3" "2 3 4
")
  (for-printed "Vector of mixed integers and floats." "12.5 3 42.890 90.5001 8 65"
	       "12.5 3 42.89 90.5001 8 65
")
  (for-printed "Numeric matrix." "3 4⍴⍳9" "1 2 3 4
5 6 7 8
9 1 2 3
")
  (for-printed "3D numeric array." "2 3 4⍴⍳9" "1 2 3 4
5 6 7 8
9 1 2 3
       
4 5 6 7
8 9 1 2
3 4 5 6
")
  (for-printed "4D numeric array." "2 3 2 5⍴⍳9" "1 2 3 4 5
6 7 8 9 1
         
2 3 4 5 6
7 8 9 1 2
         
3 4 5 6 7
8 9 1 2 3
         
         
4 5 6 7 8
9 1 2 3 4
         
5 6 7 8 9
1 2 3 4 5
         
6 7 8 9 1
2 3 4 5 6
")
  
  (for-printed "Vector of numeric matrices." "⊂[1 2]2 3 4⍴4 5 6"
	       " 4 5 6  5 6 4  6 4 5  4 5 6
 4 5 6  5 6 4  6 4 5  4 5 6
")
  (for-printed "Matrix of numeric matrices." "2 3⍴⊂2 2⍴⍳4"
	       " 1 2  1 2  1 2
 3 4  3 4  3 4
 1 2  1 2  1 2
 3 4  3 4  3 4
")
  (for-printed "Vector with nested vectors." "1 2 (1 2 3) 4 5 (6 7 8)"
	       "1 2  1 2 3  4 5  6 7 8
")
  (for-printed "Vector with initial nested vector." "(1 2 3) 4 5 (6 7) 8 9"
	       " 1 2 3  4 5  6 7  8 9
")
  (for-printed "Vector with nested arrays." "1 2 (3 4⍴⍳9) 5 6 (2 2 3⍴5) 7 8"
	       "1 2  1 2 3 4  5 6  5 5 5  7 8
     5 6 7 8       5 5 5     
     9 1 2 3                 
                   5 5 5     
                   5 5 5     
")
  (for-printed "Character vector (string)." "'ABCDE'" "ABCDE")
  (for-printed "Character matrix." "2 5⍴'ABCDEFGHIJ'" "ABCDE
FGHIJ
")
  (for-printed "3D character array." "2 3 4⍴'GRAYGOLDBLUESILKWOOLYARN'" "GRAY
GOLD
BLUE
    
SILK
WOOL
YARN
")
  (for-printed "2D array of character strings." "⊂[3]2 3 4⍴'GRAYGOLDBLUESILKWOOLYARN'"
	       " GRAY  GOLD  BLUE
 SILK  WOOL  YARN
")
  (for-printed "Vector of character matrices." "⊂[1 2]2 3 4⍴'GRAYGOLDBLUESILKWOOLYARN'"
	       " GGB  ROL  ALU  YDE
 SWY  IOA  LOR  KLN
")
  (for-printed "Matrix of character matrices." "⊂[1 2]2 3 3 4⍴'GRAYGOLDBLUESILKWOOLYARN'"
	       " GSG  RIR  ALA  YKY
 SGS  IRI  LAL  KYK
 GWG  OOO  LOL  DLD
 WGW  OOO  OLO  LDL
 BYB  LAL  URU  ENE
 YBY  ALA  RUR  NEN
")
  (for-printed "Stacked strings." "⍪'A' 'Stack' 'Of' 'Strings'"
	       " A      
 Stack  
 Of     
 Strings
")
  (for-printed "Mixed strings." "↑'These' 'Strings' 'Are' 'Mixed'"
	       "These  
Strings
Are    
Mixed  
")
  (for-printed "Matrix containing nested arrays of differing shapes." "{⊂⍺ ⍵}⌺3 3⊢3 3⍴⍳9"
	       "  1 1  0 0 0    1 0  0 0 0    1 ¯1  0 0 0 
       0 1 2         1 2 3          2 3 0 
       0 4 5         4 5 6          5 6 0 
  0 1  0 1 2    0 0  1 2 3    0 ¯1  2 3 0 
       0 4 5         4 5 6          5 6 0 
       0 7 8         7 8 9          8 9 0 
  ¯1 1  0 4 5   ¯1 0  4 5 6   ¯1 ¯1  5 6 0
        0 7 8         7 8 9          8 9 0
        0 0 0         0 0 0          0 0 0
")
  (for-printed "Array of differently-shaped nested arrays." "{⍺ ⍵}⌺3 3⊢3 3⍴⍳9"
	       " 1 1    0 0 0
        0 1 2
        0 4 5
 1 0    0 0 0
        1 2 3
        4 5 6
 1 ¯1   0 0 0
        2 3 0
        5 6 0
             
 0 1    0 1 2
        0 4 5
        0 7 8
 0 0    1 2 3
        4 5 6
        7 8 9
 0 ¯1   2 3 0
        5 6 0
        8 9 0
             
 ¯1 1   0 4 5
        0 7 8
        0 0 0
 ¯1 0   4 5 6
        7 8 9
        0 0 0
 ¯1 ¯1  5 6 0
        8 9 0
        0 0 0
")
  (for-printed "Nested vector with mixed numeric and character values."
	       "(1 2 'gh' 3) 4 'abc' (6 7) 8 9" " 1 2  gh  3  4  abc  6 7  8 9
")
  (for-printed "Column of integer and float values at varying precisions."
	       "⎕pp←6 ⋄ ⍪8 900.17814 3005 ¯15.90 88.1,÷2 4 8"
	       "   8    
 900.178
3005    
 ¯15.9  
  88.1  
   0.5  
   0.25 
   0.125
")
  (for-printed "Matrix of mixed strings and numeric vectors."
	       "2 2⍴'Test' (1 2 3) 'Hello' 5"
	       " Test   1 2 3
 Hello      5
")
  (for-printed "Matrix with columns of mixed string and numeric values."
	       "3 3⍴'a' 12 34 'b' 'cde' 'fgh' 'i' 900 'kl'"
	       "a  12   34
b cde  fgh
i 900   kl
")
  (for-printed "Another mixed matrix." "3 3⍴'a' 12 34 'b' 'cde' 'fgh' 'i' 900 'k'"
	       "a  12   34
b cde  fgh
i 900    k
")
  (for-printed "Another mixed matrix." "1⌽3 3⍴'a' 12 34 'b' 'cde' 'fgh' 'i' 900 'k'"
	       "  12   34 a
 cde  fgh b
 900    k i
")
  (for-printed "Another mixed matrix." "g←⍪12 'abc' 900 ⋄ g,(⍪1 2 3),g"
	       "  12  1   12
 abc  2  abc
 900  3  900
")
  (for-printed "Mixed matrix with floats." "1⌽3 3⍴'a' 12 3.045 'b' 'cde' 8.559 'i' 900 'k'"
	       "  12  3.045 a
 cde  8.559 b
 900      k i
")
  (for-printed "Mixed numeric, string and float matrix." "(⍪'abc'),(⍪1.2×1 2 2),⍪⍳3"
	       "a 1.2 1
b 2.4 2
c 2.4 3
")
  (for-printed "Matrix with intermixed character and float column." "(⍪'abc'),(⍪'a',1.2×1 2),⍪⍳3"
	       "a   a 1
b 1.2 2
c 2.4 3
")
  (for-printed "Catenated character and numeric arrays."
	       "(⍪⍳3),(3 3⍴'abcdef'),(3 3⍴⍳9),(3 3⍴'defghi'),⍪⍳3"
	       "1 abc 1 2 3 def 1
2 def 4 5 6 ghi 2
3 abc 7 8 9 def 3
")
  (for-printed "Array with mixed string/float column, string longer than floats."
	       "(⍪'abc'),(⍪(⊂'abcdef'),1.2 2.56),⍪⍳3"
	       "a abcdef  1
b   1.2   2
c   2.56  3
")
  (for-printed "Mixed array with nested multidimensional array."
	       "1⌽3 3⍴'a' 12 (2 4⍴5) 'b' 'cde' 'gg' 'i' 900 'k'"
	       "  12  5 5 5 5 a
      5 5 5 5  
 cde  gg      b
 900  k       i
")
  (for-printed "Another mixed/nested array."
	       "1⌽3 3⍴8 12 (2 4⍴5) 9 'cde' 'gg' 10 900 'k'"
	       "  12  5 5 5 5   8
      5 5 5 5    
 cde  gg        9
 900  k        10
")
  (for-printed "Mixed array with column holding longer number than nested array."
	       "(⍪22,2⍴⊂'abc'),(⍪(⊂2 2⍴1),'c' 12345678),⍪⍳3"
	       "  22       1 1  1
           1 1   
 abc         c  2
 abc  12345678  3
")
  (for-printed "Stacked floats with negative value under 1." "⍪¯0.75 1.25" "¯0.75
 1.25
")))

#|
This is an example showing how the April idiom can be extended with Vex's extend-vex-idiom macro.
A not-very-useful scalar function that adds 3 to its argument(s) is specified here.

(extend-vex-idiom
 april
 (utilities :process-lexicon #'april-function-glyph-processor)
 (functions
  (with (:name :extra-functions))
  (⍛ (has :title "Add3")
     (ambivalent (scalar-function (lambda (omega) (+ 3 omega)))
		 (scalar-function (lambda (alpha omega) (+ 3 alpha omega))))
     (tests (is "⍛77" 80)
	    (is "8⍛7" 18)))))
|#
