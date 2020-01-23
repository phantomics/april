;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; spec.lisp

(in-package #:april)

"This specification defines the April language. All of the standard functions and operators and their symbols, along with the language's grammar, utilities, reserved symbols, tests and demo suite are specified here."

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
	       "%'._¤#&\"‘’¶@:?!£€$()[]{}<≤=≥>≠∨∧⊂⊃∩∪/\\+-⍺⍵"
	       "⌶¯⍬∆⍙⌿⍀⊣⊢⌷¨⍨÷×∊⍴~↑↓⍳○*⌈⌊∇∘⊥⊤|;,⍱⍲⍒⍋⍉⌽⊖⍟⌹⍕⍎⍫⍪≡≢ø^∣⍷⋄←→⍝§⎕⍞⍣⍇⍈⍐⍗ ┘┐┌└┼─├┤┴┬│"))

;; top-level specification for the April language
(specify-vex-idiom
 april

 ;; system variables and default state of an April workspace
 (system :atomic-vector *atomic-vector* :output-printed nil
	 :base-state '(:index-origin 1 :comparison-tolerance 1e-14 :print-precision 10
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
		      :lexical-functions-array :lexical-functions-special :lexical-operators-lateral
		      :lexical-operators-pivotal :lexical-operators-unitary :general-tests
		      :system-variable-function-tests :printed-format-tests)
	       (:arbitrary-test :output-specification-tests)
	       (:time :lexical-functions-scalar-numeric :lexical-functions-scalar-logical
	       	      :lexical-functions-array :lexical-functions-special :lexical-operators-lateral
	       	      :lexical-operators-pivotal :lexical-operators-unitary :general-tests)
	       (:demo :general-tests :lexical-functions-scalar-numeric
	       	      :lexical-functions-scalar-logical :lexical-functions-array :lexical-functions-special
	       	      :lexical-operators-lateral :lexical-operators-pivotal :lexical-operators-unitary
	       	      :system-variable-function-tests :printed-format-tests))

 ;; utilities for compiling the language
 (utilities :match-blank-character (lambda (char) (member char '(#\  #\Tab)))
	    :match-newline-character (lambda (char) (member char '(#\⋄ #\◊ #\Newline #\Return)))
	    ;; set the language's valid blank, newline characters and token characters
	    :match-token-character
	    (lambda (char)
	      (or (alphanumericp char)
		  (member char '(#\. #\_ #\⎕ #\∆ #\⍙ #\¯ #\⍺ #\⍵ #\⍬))))
	    ;; overloaded numeric characters may be functions or operators or may be part of a numeric token
	    ;; depending on their context
	    :match-overloaded-numeric-character (lambda (char) (char= #\. char))
	    ;; this code preprocessor removes comments, including comment-only lines
	    :prep-code-string
	    (lambda (string)
	      (regex-replace-all "^\\s{0,}⍝(.*)[\\r\\n]|(?<=[\\r\\n])\\s{0,}⍝(.*)[\\r\\n]|
                                  (?<=[\\r\\n])\\s{0,}⍝(.*)[\\r\\n]|(?<=[^\\r\\n])\\s{0,}⍝(.*)(?=[\\r\\n])"
				 string ""))
	    ;; handles axis strings like "'2;3;;' from 'array[2;3;;]'"
	    :process-axis-string
	    (lambda (string)
	      (let ((indices) (last-index)
		    (nesting (vector 0 0 0))
		    (delimiters '(#\[ #\( #\{ #\] #\) #\})))
		(loop :for char :across string :counting char :into charix
		   :do (let ((mx (length (member char delimiters))))
			 (if (< 3 mx) (incf (aref nesting (- 6 mx)) 1)
			     (if (< 0 mx 4) (incf (aref nesting (- 3 mx)) -1)
				 (if (and (char= char #\;)
					  (= 0 (loop :for ncount :across nesting :summing ncount)))
				     (setq indices (cons (1- charix) indices)))))))
		(loop :for index :in (reverse (cons (length string) indices))
		   :counting index :into iix
		   :collect (make-array (- index (if last-index 1 0)
					   (if last-index last-index 0))
					:element-type 'character :displaced-to string
					:displaced-index-offset (if last-index (1+ last-index) 0))
		   :do (setq last-index index))))
	    
	    ;; macro to process lexical specs of functions and operators
	    :process-lexicon #'april-function-glyph-processor
	    :format-value #'format-value
	    ;; process system state input passed when April is invoked, i.e. with (april (with (:state ...)) "...")
	    :preprocess-state-input
	    (lambda (state)
	      (if (getf state :count-from)
		  (setf (getf state :index-origin)
			(getf state :count-from)))
	      state)
	    ;; converts parts of the system state into lists that will form part of the local lexical
	    ;; environment in which the compiled APL code runs, i.e. the (let) form into which
	    ;; the APL-generating macros are expanded
	    :system-lexical-environment-interface
	    (lambda (state)
	      ;; the index origin, print precision and output stream values are
	      ;; passed into the local lexical environment
	      (append (list (list (intern "OUTPUT-STREAM" "APRIL")
				  (if (getf state :print-to)
				      (getf state :print-to)
				      (second (getf state :output-stream)))))
		      (loop :for var :in '(:index-origin :print-precision)
			 :collect (list (intern (string-upcase var) "APRIL")
					(getf state var)))))
	    :postprocess-compiled
	    (lambda (state &rest inline-arguments)
	      (lambda (form)
		(let ((final-form (if inline-arguments `(apl-call :fn ,(first (last form))
								  ,@inline-arguments)
				      (first (last form)))))
		  (append (butlast form)
			  (list (append (list 'apl-output final-form)
					(append (list :print-precision 'print-precision)
						(if (getf state :print)
						    (list :print-to 'output-stream))
						(if (getf state :output-printed)
						    (list :output-printed (getf state :output-printed))))))))))
	    :postprocess-value
	    (lambda (form state)
	      (append (list 'apl-output form)
		      (append (list :print-precision 'print-precision)
			      (if (getf state :print) (list :print-to 'output-stream))
			      (if (getf state :output-printed)
				  (list :output-printed (getf state :output-printed))))))
	    :build-variable-declarations #'build-variable-declarations
	    :build-compiled-code #'build-compiled-code)

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
     (ambivalent (scalar-function (λω (if (integerp omega)
					  (+ index-origin (random omega))
					  (error "The right arguments to ? must be non-negative integers."))))
		 (λωα (let ((omega (disclose omega))
			    (alpha (disclose alpha)))
			(if (or (not (integerp omega))
				(not (integerp alpha)))
			    (error "Both arguments to ? must be single non-negative integers.")
			    (make-array (list alpha)
					:element-type (list 'integer 0 omega)
					:initial-contents (loop :for i :below alpha
							     :collect (+ index-origin (random omega)))))))))
  (○ (has :titles ("Pi Times" "Circular"))
     (ambivalent :asymmetric-scalar (λω (* pi omega))
		 (λωα (if (and (integerp alpha) (<= -12 alpha 12))
			  (funcall (aref *circular-functions* (+ 12 alpha))
				   omega)
			  (error (concatenate 'string "Invalid argument to ○; the left argument must be an"
					      " integer between ¯12 and 12.")))))
     (tests (is "⌊100000×○1" 314159)
	    (is "(⌊1000×1÷2⋆÷2)=⌊1000×1○○÷4" 1)
	    (is "⌊1000×1○⍳9" #(841 909 141 -757 -959 -280 656 989 412))
	    (is "⌈1 2 3○○.5 2 .25" #(1 1 1))))
  (\~ (has :titles ("Not" "Without"))
      (ambivalent (scalar-function (λω (cond ((= 0 omega) 1)
					     ((= 1 omega) 0)
					     (t (error "Domain error: arguments to ~~ must be 1 or 0.")))))
		  #'without)
      (tests (is "~1 0 1" #(0 1 0))
	     (is "1 2 3 4 5 6 7~3 5" #(1 2 4 6 7))
	     (is "1 2 3 4~2" #(1 3 4))
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
     (dyadic (scalar-function (boolean-op scalar-compare)))
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
     (dyadic (scalar-function (boolean-op (λωα (not (= omega alpha 1))))))
     (tests (is "0 1 0 1⍲0 0 1 1" #*1110)))
  (∨ (has :title "Or")
     (dyadic (scalar-function (reverse-op gcd)))
     (tests (is "0 1 0 1∨0 0 1 1" #*0111)))
  (⍱ (has :title "Nor")
     (dyadic (scalar-function (boolean-op (λωα (= omega alpha 0)))))
     (tests (is "0 1 0 1⍱0 0 1 1" #*1000))))

 (functions
  (with (:name :lexical-functions-array)
	(:tests-profile :title "Array Function Tests")
	(:demo-profile :title "Array Function Demos"
		       :description "These functions affect entire arrays, changing their structure or deriving data from them in some way."))
  (⍳ (has :titles ("Interval" "Index Of"))
     (ambivalent (λω (count-to omega index-origin))
		 (λωα (index-of omega alpha index-origin)))
     (tests (is "⍳5" #(1 2 3 4 5))
	    (is "(2⍳3),2 3⍳4" #(2 3))
	    (is "3⍳⍳4" #(2 2 1 2))
	    (is "2 4⍳⍳5" #(3 1 3 2 3))))
  (⍴ (has :titles ("Shape" "Reshape"))
     (ambivalent #'shape (λωα (reshape-to-fit omega (if (arrayp alpha)
							(array-to-list alpha)
							(list alpha)))))
     (tests (is "⍴1" #0A0)
	    (is "⍴1 2 3" 3)
	    (is "⍴3 5⍴⍳8" #(3 5))
	    (is "3⍴2" #(2 2 2))
	    (is "3⍴3" #(3 3 3))
	    (is "4 5⍴⍳3" #2A((1 2 3 1 2) (3 1 2 3 1) (2 3 1 2 3) (1 2 3 1 2)))))
  (⌷ (has :title "Index")
     (dyadic (λωαχ (at-index omega alpha axes index-origin)))
     (tests (is "1⌷3" 3)
	    (is "3⌷2 4 6 8 10" 6)
	    (is "3⌷⍳9" 3)
  	    (is "2 2⌷4 5⍴⍳9" 7)
  	    (is "2 3 4⌷4 5 6⍴⍳9" 1)
  	    (is "1 3⌷2 3 4⍴⍳5" #(4 5 1 2))
  	    (is "1 3⌷[1 3]2 3 4⍴⍳5" #(3 2 1))
  	    (is "(⊂4 5 2 6 3 7 1)⌷'MARANGA'" "ANAGRAM")))
  (≡ (has :titles ("Depth" "Match"))
     (ambivalent #'find-depth (boolean-op array-compare))
     (tests (is "≡1" 0)
  	    (is "≡⍳3" 1)
  	    (is "≡(1 2)(3 4)" 2)
  	    (is "≡1 (2 3) (4 5 (6 7)) 8" -3)
	    (is "3≡3" 1)
	    (is "4≡2" 0)))
  (≢ (has :titles ("First Dimension" "Not Match"))
     (ambivalent #'find-first-dimension (boolean-op (lambda (omega alpha) (not (array-compare omega alpha)))))
     (tests (is "≢2" 1)
	    (is "≢1 2 3" 3)
   	    (is "≢2 3 4⍴⍳9" 2)
	    (is "5≢5" 0)
	    (is "3≢1" 1)))
  (∊ (has :titles ("Enlist" "Membership"))
     (ambivalent #'enlist #'membership)
     (tests (is "∊2" 2)
	    (is "∊2 2 2⍴⍳9" #(1 2 3 4 5 6 7 8))
	    (is "2 3∊2" #*10)
	    (is "3∊3 4 5" 1)
  	    (is "2 5 7∊1 2 3 4 5" #*110)))
  (⍷ (has :title "Find")
     (dyadic #'find-array)
     (tests (is "5⍷5" 1)
	    (is "2⍷3 4⍴⍳9" #2A((0 1 0 0) (0 0 0 0) (0 0 1 0)))
	    (is "(2 2⍴6 7 1 2)⍷2 3 4⍴⍳9" #3A(((0 0 0 0) (0 1 0 0) (0 0 0 0))
  					     ((0 0 1 0) (0 0 0 0) (0 0 0 0))))))
  (⍸ (has :titles ("Where" "Interval Index"))
     (ambivalent (λω (where-equal-to-one omega index-origin))
  		 (interval-index atomic-vector))
     (tests (is "⍸1" 1)
	    (is "⍸0 0 1 0 1 0 0 1 1 0" #(3 5 8 9))
  	    (is "⍸3=2 3 4⍴⍳9" #(#(1 1 3) #(1 3 4) #(2 3 1)))
  	    (is "⍸(2 3 4⍴⍳9)∊3 5" #(#(1 1 3) #(1 2 1) #(1 3 4) #(2 1 2) #(2 3 1) #(2 3 3)))
	    (is "2 4 6 8⍸3" 1)
  	    (is "10 20 30 40⍸5 12 19 24 35 42 51" #(0 1 1 2 3 4 4))
  	    (is "(2 5⍴'RADIUS')⍸3 4 5⍴'BOXCAR'" #2A((0 1 0 0) (2 0 0 1) (0 0 2 0)))
  	    (is "(2 3 5⍴'ABCDEFHIJKLM')⍸3 3 5⍴'BOREAL'" #(1 2 1))))
  (\, (has :titles ("Ravel" "Catenate or Laminate"))
      (ambivalent (ravel-array index-origin)
		  (catenate-arrays index-origin))
      (tests (is ",5" 5)
	     (is ",3 4⍴⍳9" #(1 2 3 4 5 6 7 8 9 1 2 3))
  	     (is ",[0.5]3 4⍴⍳9" #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3))))
  	     (is ",[1.5]3 4⍴⍳9" #3A(((1 2 3 4)) ((5 6 7 8)) ((9 1 2 3))))
  	     (is ",[2.5]3 4⍴⍳9" #3A(((1) (2) (3) (4)) ((5) (6) (7) (8)) ((9) (1) (2) (3))))
  	     (is ",[1 2]2 3 3⍴⍳12" #2A((1 2 3) (4 5 6) (7 8 9) (10 11 12) (1 2 3) (4 5 6)))
  	     (is ",[2 3]2 3 3⍴⍳12" #2A((1 2 3 4 5 6 7 8 9) (10 11 12 1 2 3 4 5 6)))
  	     (is ",[1 2 3]2 3 3⍴⍳12" #(1 2 3 4 5 6 7 8 9 10 11 12 1 2 3 4 5 6))
	     (is "5 6,3" #(5 6 3))
	     (is "2,⍳3" #(2 1 2 3))
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
     (ambivalent #'tabulate (catenate-on-first index-origin))
     (tests (is "⍪4" 4)
	    (is "⍪'MAKE'" #2A((#\M) (#\A) (#\K) (#\E)))
  	    (is "⍪3 4⍴⍳9" #2A((1 2 3 4) (5 6 7 8) (9 1 2 3)))
  	    (is "⍪2 3 4⍴⍳24" #2A((1 2 3 4 5 6 7 8 9 10 11 12)
  				 (13 14 15 16 17 18 19 20 21 22 23 24)))
	    (is "2⍪⍳4" #(2 1 2 3 4))
	    (is "(2 3⍴⍳6)⍪3" #2A((1 2 3) (4 5 6) (3 3 3)))
  	    (is "0⍪3 4⍴⍳9" #2A((0 0 0 0) (1 2 3 4) (5 6 7 8) (9 1 2 3)))
  	    (is "0⍪[2]3 4⍴⍳9" #2A((0 1 2 3 4) (0 5 6 7 8) (0 9 1 2 3)))
  	    (is "(3⍴5)⍪3 3⍴3" #2A((5 5 5) (3 3 3) (3 3 3) (3 3 3)))
  	    (is "(5 4⍴⍳6)⍪3 4⍴⍳9" #2A((1 2 3 4) (5 6 1 2) (3 4 5 6) (1 2 3 4)
  				      (5 6 1 2) (1 2 3 4) (5 6 7 8) (9 1 2 3)))
  	    (is "(3 6⍴⍳6)⍪[2]3 4⍴⍳9" #2A((1 2 3 4 5 6 1 2 3 4) (1 2 3 4 5 6 5 6 7 8)
  					 (1 2 3 4 5 6 9 1 2 3)))))
  (↑ (has :titles ("Mix" "Take"))
     (ambivalent (λωχ (mix-arrays (if axes (- (ceiling (first axes)) index-origin)
  				      (rank omega))
  				  omega))
  		 (section-array index-origin))
     (tests (is "↑2" 2)
	    (is "↑(1)(1 2)(1 2 3)" #2A((1 0 0) (1 2 0) (1 2 3)))
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
	    (is "2↑2" #(2 0))
	    (is "3↑⍳9" #(1 2 3))
  	    (is "¯1↑⍳5" 5)
  	    (is "2 3 4↑4 5 6⍴⍳9" #3A(((1 2 3 4) (7 8 9 1) (4 5 6 7))
  				     ((4 5 6 7) (1 2 3 4) (7 8 9 1))))
  	    (is "2 ¯2 ¯2↑4 5 6⍴⍳9" #3A(((5 6) (2 3)) ((8 9) (5 6))))
  	    (is "5 ¯5↑(3 3⍴⍳9)∊1 2 3 4 8" #2A((0 0 1 1 1) (0 0 1 0 0) (0 0 0 1 0) (0 0 0 0 0) (0 0 0 0 0)))
	    (is "2 ¯2↑2 2⍴⍳4" #2A((1 2) (3 4)))
  	    (is "1↑[1]2 3 4⍴⍳9" #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3))))
  	    (is "1↑[2]2 3 4⍴⍳9" #3A(((1 2 3 4)) ((4 5 6 7))))
  	    (is "2↑[2]2 3 4⍴⍳9" #3A(((1 2 3 4) (5 6 7 8)) ((4 5 6 7) (8 9 1 2))))
  	    (is "2↑[3]2 3 4⍴⍳9" #3A(((1 2) (5 6) (9 1)) ((4 5) (8 9) (3 4))))))
  (↓ (has :titles ("Split" "Drop"))
     (ambivalent (λωχ (split-array omega *last-axis*))
		 (section-array index-origin t))
     (tests (is "↓5" 5)
	    (is "↓3 4⍴⍳9" #(#(1 2 3 4) #(5 6 7 8) #(9 1 2 3)))
  	    (is "↓[1]3 4⍴⍳9" #(#(1 5 9) #(2 6 1) #(3 7 2) #(4 8 3)))
	    (is "2↓3" #0A0)
  	    (is "2↓⍳9" #(3 4 5 6 7 8 9))
	    (is "4↓⍳9" #(5 6 7 8 9))
  	    (is "2 2 2↓4 5 6⍴⍳9" #3A(((3 4 5 6) (9 1 2 3) (6 7 8 9))
  				     ((6 7 8 9) (3 4 5 6) (9 1 2 3))))
  	    (is "1↓[1]2 3 4⍴⍳9" #3A(((4 5 6 7) (8 9 1 2) (3 4 5 6))))
  	    (is "1↓[2]2 3 4⍴⍳9" #3A(((5 6 7 8) (9 1 2 3)) ((8 9 1 2) (3 4 5 6))))
  	    (is "2↓[2]2 3 4⍴⍳9" #3A(((9 1 2 3)) ((3 4 5 6))))
  	    (is "2↓[3]2 3 4⍴⍳9" #3A(((3 4) (7 8) (2 3)) ((6 7) (1 2) (5 6))))
  	    (is "¯2↓⍳9" #(1 2 3 4 5 6 7))
  	    (is "¯2 ¯2↓5 8⍴⍳9" #2A((1 2 3 4 5 6) (9 1 2 3 4 5) (8 9 1 2 3 4)))))
  (⊂ (has :titles ("Enclose" "Partitioned Enclose"))
     (ambivalent (λωχ (if axes (re-enclose omega (aops:each (lambda (axis) (- axis index-origin))
  							    (if (arrayp (first axes))
								(first axes)
								(vector (first axes)))))
  			  (if (loop :for dim :in (dims omega) :always (= 1 dim))
  			      omega (vector omega))))
  		 (λωαχ (partitioned-enclose alpha omega *last-axis*)))
     (tests (is "⊂2" 2)
	    (is "⊂⍳5" #(#(1 2 3 4 5)))
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
	    (is "1⊂2" #(#(2)))
	    (is "1⊂5" #(#(5)))
  	    (is "0 1 0 0 1 1 0 0 0⊂⍳9" #(#(2 3 4) #(5) #(6 7 8 9)))
  	    (is "0 1 0 0 1 1 0 0⊂4 8⍴⍳9"
  		#(#2A((2 3 4) (1 2 3) (9 1 2) (8 9 1)) #2A((5) (4) (3) (2))
  		  #2A((6 7 8) (5 6 7) (4 5 6) (3 4 5))))
  	    (is "0 1 0 1⊂[1]4 8⍴⍳9"
  		#(#2A((9 1 2 3 4 5 6 7) (8 9 1 2 3 4 5 6)) #2A((7 8 9 1 2 3 4 5))))))
  (⊆ (has :title "Partition")
     (dyadic (λωαχ (partition-array alpha omega *last-axis*)))
     (tests (is "2⊆⍳3" #(#(1 2 3)))
	    (is "1 1 2 2 2 3 3 3 3⊆⍳9" #(#(1 2) #(3 4 5) #(6 7 8 9)))
  	    (is "1 1 0 1⊆4 4 4⍴⍳9" #3A(((#(1 2) #(4)) (#(5 6) #(8)) (#(9 1) #(3)) (#(4 5) #(7)))
  				       ((#(8 9) #(2)) (#(3 4) #(6)) (#(7 8) #(1)) (#(2 3) #(5)))
  				       ((#(6 7) #(9)) (#(1 2) #(4)) (#(5 6) #(8)) (#(9 1) #(3)))
  				       ((#(4 5) #(7)) (#(8 9) #(2)) (#(3 4) #(6)) (#(7 8) #(1)))))
  	    (is "1 1 0 1⊆[2]4 4 4⍴⍳9" #3A(((#(1 5) #(2 6) #(3 7) #(4 8)) (#(4) #(5) #(6) #(7)))
  					  ((#(8 3) #(9 4) #(1 5) #(2 6)) (#(2) #(3) #(4) #(5)))
  					  ((#(6 1) #(7 2) #(8 3) #(9 4)) (#(9) #(1) #(2) #(3)))
  					  ((#(4 8) #(5 9) #(6 1) #(7 2)) (#(7) #(8) #(9) #(1)))))))
  (⊃ (has :titles ("Disclose" "Pick"))
     (ambivalent #'get-first-or-disclose (pick index-origin))
     (tests (is "⊃3" 3)
	    (is "⊃⍳4" 1)
  	    (is "⊃⊂⍳4" #(1 2 3 4))
	    (is "2⊃2 4 6 8" 4)
  	    (is "2⊃(1 2 3)(4 5 6)(7 8 9)" #(4 5 6))
  	    (is "2 2⊃(1 2 3)(4 5 6)(7 8 9)" 5)))
  (∩ (has :title "Intersection")
     (dyadic #'array-intersection)
     (tests (is "2∩⍳4" 2)
	    (is "4 5 6∩4" 4)
	    (is "'MIXTURE'∩'LATER'" "TRE")
  	    (is "'STEEL'∩'SABER'" "SEE")
  	    (is "1 4 8∩⍳5" #(1 4))))
  (∪ (has :titles ("Unique" "Union"))
     (ambivalent #'unique #'array-union)
     (tests (is "∪3" 3)
	    (is "∪1 2 3 4 5 1 2 8 9 10 11 7 8 11 12" #(1 2 3 4 5 8 9 10 11 7 12))
  	    (is "∪'MISSISSIPPI'" "MISP")
  	    (is "∪2 3 4⍴⍳12" #3A(((1 2 3 4) (5 6 7 8) (9 10 11 12))))
  	    (is "∪2 3 4⍴⍳24" #3A(((1 2 3 4) (5 6 7 8) (9 10 11 12))
  				 ((13 14 15 16) (17 18 19 20) (21 22 23 24))))
	    (is "2 3 4∪5" #(2 3 4 5))
	    (is "2∪⍳3" #(2 1 3))
  	    (is "3 10 14 18 11∪9 4 5 10 8 3" #(3 10 14 18 11 9 4 5 8))
  	    (is "'STEEL'∪'SABER'" "STEELABR")
  	    (is "'APRIL' 'MAY'∪'MAY' 'JUNE'" #("APRIL" "MAY" "JUNE"))))
  (⌽ (has :titles ("Reverse" "Rotate"))
     (ambivalent (λωχ (turn omega *last-axis*))
  		 (λωαχ (turn omega *last-axis* (disclose alpha))))
     (tests (is "⌽3" 3)
	    (is "⌽1 2 3 4 5" #(5 4 3 2 1))
  	    (is "⌽3 4⍴⍳9" #2A((4 3 2 1) (8 7 6 5) (3 2 1 9)))
	    (is "3⌽1" 1)
	    (is "3⌽⍳5" #(4 5 1 2 3))
  	    (is "2⌽3 4⍴⍳9" #2A((3 4 1 2) (7 8 5 6) (2 3 9 1)))
  	    (is "(2 2⍴1 2 3 4)⌽2 2 5⍴⍳9" #3A(((2 3 4 5 1) (8 9 1 6 7)) ((5 6 2 3 4) (2 7 8 9 1))))))
  (⊖ (has :titles ("Reverse First" "Rotate First"))
     (ambivalent (λωχ (turn omega *first-axis*))
  		 (λωαχ (turn omega *first-axis* (disclose alpha))))
     (tests (is "⊖4" 4)
	    (is "⊖1 2 3 4 5" #(5 4 3 2 1))
  	    (is "⊖3 4⍴⍳9" #2A((9 1 2 3) (5 6 7 8) (1 2 3 4)))
	    (is "2⊖5" 5)
	    (is "2⊖⍳6" #(3 4 5 6 1 2))
  	    (is "1⊖3 4⍴⍳9" #2A((5 6 7 8) (9 1 2 3) (1 2 3 4)))
  	    (is "(3 4 5⍴⍳4)⊖2 3 4 5⍴⍳9" #4A((((7 2 9 4 2) (6 4 8 6 1) (8 3 1 5 3) (7 5 9 7 2))
  					     ((9 4 2 6 4) (8 6 1 8 3) (1 5 3 7 5) (9 7 2 9 4))
  					     ((2 6 4 8 6) (1 8 3 1 5) (3 7 5 9 7) (2 9 4 2 6)))
  					    (((1 8 3 1 5) (3 7 5 9 7) (2 9 4 2 6) (4 8 6 1 8))
  					     ((3 1 5 3 7) (5 9 7 2 9) (4 2 6 4 8) (6 1 8 3 1))
  					     ((5 3 7 5 9) (7 2 9 4 2) (6 4 8 6 1) (8 3 1 5 3)))))))
  (⍉ (has :titles ("Transpose" "Permute"))
     (ambivalent (permute-array index-origin)
		 (permute-array index-origin))
     (tests (is "⍉2" 2)
	    (is "⍉2 3 4⍴⍳9" #3A(((1 4) (5 8) (9 3)) ((2 5) (6 9) (1 4))
  				((3 6) (7 1) (2 5)) ((4 7) (8 2) (3 6))))
	    (is "1⍉5" 5)
	    (is "1⍉⍳3" #(1 2 3))
  	    (is "1 3 2⍉2 3 4⍴⍳9" #3A(((1 5 9) (2 6 1) (3 7 2) (4 8 3))
  				     ((4 8 3) (5 9 4) (6 1 5) (7 2 6))))))
  (/ (has :title "Replicate")
     (dyadic (λωαχ (expand-array alpha omega *last-axis* :compress-mode t)))
     (tests (is "3/1" #*111)
	    (is "2/8" #(8 8))
	    (is "5/3" #(3 3 3 3 3))
  	    (is "1 0 1 0 1/⍳5" #(1 3 5))
  	    (is "3/⍳5" #(1 1 1 2 2 2 3 3 3 4 4 4 5 5 5))
  	    (is "3/⊂⍳5" #(#(1 2 3 4 5) #(1 2 3 4 5) #(1 2 3 4 5)))
  	    (is "1 ¯2 3 ¯4 5/3 5⍴⍳5" #2A((1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)
  					 (1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)
  					 (1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)))))
  (⌿ (has :title "Replicate First")
     (dyadic (λωαχ (expand-array alpha omega *first-axis* :compress-mode t)))
     (tests (is "3⌿2" #(2 2 2))
	    (is "4⌿7 8" #(7 7 7 7 8 8 8 8))
	    (is "1 0 1 0 1⌿⍳5" #(1 3 5))
  	    (is "1 ¯2 3⌿3 5⍴⍳9" #2A((1 2 3 4 5) (0 0 0 0 0) (0 0 0 0 0)
  				    (2 3 4 5 6) (2 3 4 5 6) (2 3 4 5 6)))
  	    (is "1 ¯2 3 ¯4 5⌿[2]3 5⍴⍳5" #2A((1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)
  					    (1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)
  					    (1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)))))
  (\\ (has :title "Expand")
      (dyadic (λωαχ (expand-array alpha omega *last-axis*)))
      (tests (is "4\\2" #(2 2 2 2))
	     (is "3\\7" #(7 7 7))
	     (is "1 ¯2 3 ¯4 5\\'.'" ".  ...    .....")
  	     (is "1 ¯2 2 0 1\\3+2 3⍴⍳6" #2A((4 0 0 5 5 0 6) (7 0 0 8 8 0 9)))
  	     (is "1 0 1\\[1]3+2 3⍴⍳6" #2A((4 5 6) (0 0 0) (7 8 9)))
  	     (is "1 ¯2 3 4\\[1]3 5⍴⍳9" #2A((1 2 3 4 5) (0 0 0 0 0) (0 0 0 0 0)
  					   (6 7 8 9 1) (6 7 8 9 1) (6 7 8 9 1)
  					   (2 3 4 5 6) (2 3 4 5 6) (2 3 4 5 6)
  					   (2 3 4 5 6)))))
  (⍀ (has :title "Expand First")
     (dyadic (λωαχ (expand-array alpha omega *first-axis*)))
     (tests (is "2⍀5" #(5 5))
	    (is "2⍀1" #*11)
	    (is "1 ¯2 3 ¯4 5⍀3" #(3 0 0 3 3 3 0 0 0 0 3 3 3 3 3))
  	    (is "1 0 1⍀3+2 3⍴⍳6" #2A((4 5 6) (0 0 0) (7 8 9)))))
  (⍋ (has :titles ("Grade Up" "Grade Up By"))
     (ambivalent (λω (grade omega index-origin (alpha-compare atomic-vector #'<=)))
  		 (λωα (grade (if (vectorp alpha)
  				 (index-of alpha omega index-origin)
  				 (array-grade alpha omega))
  			     index-origin (alpha-compare atomic-vector #'<))))
     (tests (is "⍋2" 1)
	    (is "⍋8 3 4 9 1 5 2" #(5 7 2 3 6 1 4))
  	    (is "⍋5 6⍴⍳16" #(1 4 2 5 3))
	    (is "'abcd'⍋'d'" #(4 1 2 3))
  	    (is "st←'aodjeignwug' ⋄ st[⍋st]" "adeggijnouw")
  	    (is "(2 5⍴'ABCDEabcde')⍋'ACaEed'" #(1 3 2 6 4 5))))
  (⍒ (has :titles ("Grade Down" "Grade Down By"))
     (ambivalent (λω (grade omega index-origin (alpha-compare atomic-vector #'>=)))
  		 (λωα (grade (if (vectorp alpha)
  				 (index-of alpha omega index-origin)
  				 (array-grade alpha omega))
  			     index-origin (alpha-compare atomic-vector #'>))))
     (tests (is "⍒3" 1)
	    (is "⍒6 1 8 2 4 3 9" #(7 3 1 5 6 4 2))
  	    (is "⍒5 6⍴⍳12" #(2 4 1 3 5))
	    (is "'abcd'⍒'d'" #(1 2 3 4))
  	    (is "st←'aodjeignwug' ⋄ st[⍒st]" "wuonjiggeda")
  	    (is "(2 5⍴'ABCDEabcde')⍒'ACaEed'" #(5 4 6 2 3 1))))
  (⌹ (has :titles ("Matrix Inverse" "Matrix Divide"))
     (ambivalent #'matrix-inverse #'matrix-divide)
     (tests (is "⌹3" 1/3)
	    (is "⌹1 2 3 4" #(1/30 1/15 1/10 2/15))
  	    (is "⌹2 2⍴4 9 8 2" #2A((-1/32 9/64) (1/8 -1/16)))
  	    (is "⌹4 2⍴1 3 ¯4 9" #2A((3/14 -1/14 3/14 -1/14) (2/21 1/42 2/21 1/42)))
  	    (is "35 89 79⌹3 3⍴3 1 4 1 5 9 2 6 5" #(193/90 739/90 229/45))
  	    (is "(3 2⍴1 2 3 6 9 10)⌹3 3⍴1 0 0 1 1 0 1 1 1" #2A((1 2) (2 4) (6 4)))))
  (⊤ (has :title "Encode")
     (dyadic #'encode)
     (tests (is "9⊤15" 6)
	    (is "6 2 8⊤12" #(0 1 4))
	    (is "1760 3 12⊤82" #(2 0 10))
  	    (is "16 16 16 16⊤100" #(0 0 6 4))
  	    (is "2 2 2 2 2⊤⍳5" #2A((0 0 0 0 0) (0 0 0 0 0) (0 0 0 1 1) (0 1 1 0 0) (1 0 1 0 1)))
  	    (is "16 16 16 16⊤2 2⍴100 200 300 400"
  		#3A(((0 0) (0 0)) ((0 0) (1 1)) ((6 12) (2 9)) ((4 8) (12 0))))))
  (⊥ (has :title "Decode")
     (dyadic #'decode)
     (tests (is "14⊥7" 7)
	    (is "6⊥12 50" 122)
	    (is "10⊥2 6 7 1" 2671)
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
		 (λωα (declare (ignore alpha))
		      omega))
     (tests (is "⊢77" 77)
	    (is "55⊢77" 77)))
  (⊣ (has :titles ("Empty" "Left"))
     (ambivalent (λω (declare (ignore omega))
		     #0A0)
		 (λωα (declare (ignore omega))
		      alpha))
     (tests (is "⊣77" #0A0)
	    (is "55⊣77" 55)))
  (⍕ (has :titles ("Format" "Format At Precision"))
     (ambivalent (format-array print-precision)
		 (format-array print-precision))
     (tests (is "⍕3 4⍴⍳9" #2A((#\1 #\  #\2 #\  #\3 #\  #\4) (#\5 #\  #\6 #\  #\7 #\  #\8)
			      (#\9 #\  #\1 #\  #\2 #\  #\3)))
	    (is "⍕2 3 4⍴⍳9" #3A(((#\1 #\  #\2 #\  #\3 #\  #\4) (#\5 #\  #\6 #\  #\7 #\  #\8)
				 (#\9 #\  #\1 #\  #\2 #\  #\3))
				((#\4 #\  #\5 #\  #\6 #\  #\7) (#\8 #\  #\9 #\  #\1 #\  #\2)
				 (#\3 #\  #\4 #\  #\5 #\  #\6))))
	    (is "3⍕○3 4⍴⍳9" #2A((#\  #\3 #\. #\1 #\4 #\2 #\  #\  #\6 #\. #\2 #\8 #\3 #\  #\  #\9 #\. #\4
				     #\2 #\5 #\  #\1 #\2 #\. #\5 #\6 #\6)
				(#\1 #\5 #\. #\7 #\0 #\8 #\  #\1 #\8 #\. #\8 #\5 #\0 #\  #\2 #\1 #\. #\9
				     #\9 #\1 #\  #\2 #\5 #\. #\1 #\3 #\3)
				(#\2 #\8 #\. #\2 #\7 #\4 #\  #\  #\3 #\. #\1 #\4 #\2 #\  #\  #\6 #\. #\2
				     #\8 #\3 #\  #\  #\9 #\. #\4 #\2 #\5)))
	    (is "5⍕○3 4⍴⍳9" #2A((#\  #\3 #\. #\1 #\4 #\1 #\5 #\9 #\  #\  #\6 #\. #\2 #\8 #\3 #\1 #\9 #\ 
				     #\  #\9 #\. #\4 #\2 #\4 #\7 #\8 #\  #\1 #\2 #\. #\5 #\6 #\6 #\3 #\7)
				(#\1 #\5 #\. #\7 #\0 #\7 #\9 #\6 #\  #\1 #\8 #\. #\8 #\4 #\9 #\5 #\6 #\ 
				     #\2 #\1 #\. #\9 #\9 #\1 #\1 #\5 #\  #\2 #\5 #\. #\1 #\3 #\2 #\7 #\4)
				(#\2 #\8 #\. #\2 #\7 #\4 #\3 #\3 #\  #\  #\3 #\. #\1 #\4 #\1 #\5 #\9 #\ 
				     #\  #\6 #\. #\2 #\8 #\3 #\1 #\9 #\  #\  #\9 #\. #\4 #\2 #\4 #\7 #\8)))))
  (⍎ (has :title "Evaluate")
     (symbolic :special-lexical-form-evaluate)
     (tests (is "⍎'1+1'" 2)
	    (is "⍎'5','+3 2 1'" #(8 7 6))))
  (← (has :title "Assign")
     (symbolic :special-lexical-form-assign)
     (tests (is "x←55 ⋄ x" 55)
	    (is "x←2 3 4⍴⍳9 ⋄ x[;1;]←7 ⋄ x" #3A(((7 7 7 7) (5 6 7 8) (9 1 2 3))
						((7 7 7 7) (8 9 1 2) (3 4 5 6))))))
  (→ (has :title "Branch") 
     (symbolic :special-lexical-form-branch)
     (tests (is "x←1 ⋄ →1              ⋄ x×←11 ⋄ 1→⎕   ⋄ x×←3 ⋄ 2→⎕   ⋄ x×←5 ⋄ 3→⎕     ⋄ x×←7" 105)
	    (is "x←1 ⋄ →1+1            ⋄ x×←11 ⋄ 1→⎕   ⋄ x×←3 ⋄ 2→⎕   ⋄ x×←5 ⋄ 3→⎕     ⋄ x×←7" 35)
	    (is "x←1 ⋄ →2+3            ⋄ x×←11 ⋄ 1→⎕   ⋄ x×←3 ⋄ 2→⎕   ⋄ x×←5 ⋄ 3→⎕     ⋄ x×←7" 1155)
	    (is "x←1 ⋄ →0              ⋄ x×←11 ⋄ 1→⎕   ⋄ x×←3 ⋄ 2→⎕   ⋄ x×←5 ⋄ 3→⎕     ⋄ x×←7" 1155)
	    (is "x←1 ⋄ →three          ⋄ x×←11 ⋄ one→⎕ ⋄ x×←3 ⋄ two→⎕ ⋄ x×←5 ⋄ three→⎕ ⋄ x×←7" 7)
	    (is "x←1 ⋄ (3-2)→two three ⋄ x×←11 ⋄ one→⎕ ⋄ x×←3 ⋄ two→⎕ ⋄ x×←5 ⋄ three→⎕ ⋄ x×←7" 35)
	    (is "x←1 ⋄ 0→two three     ⋄ x×←11 ⋄ one→⎕ ⋄ x×←3 ⋄ two→⎕ ⋄ x×←5 ⋄ three→⎕ ⋄ x×←7" 1155)))
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
     (lateral (with-operand-derived (left-glyph left-function-dyadic)
		(lambda (axes) `(apply-reducing ,left-glyph ,left-function-dyadic ,axes))))
     (tests (is "+/1 2 3 4 5" 15)
	    (is "⊢/⍳5" 5)
	    (is "+/3 4⍴⍳12" #(10 26 42))
	    (is "-/3 4⍴⍳12" #(-2 -2 -2))
	    (is "+/[1]3 4⍴⍳12" #(15 18 21 24))
	    (is "fn←{⍺+⍵} ⋄ fn/1 2 3 4 5" 15)
	    (is "⌊10000×{⍺+÷⍵}/40/1" 16180)))
  (⌿ (has :title "Reduce First")
     (lateral (with-operand-derived (left-glyph left-function-dyadic)
		(lambda (axes) `(apply-reducing ,left-glyph ,left-function-dyadic ,axes t))))
     (tests (is "+⌿3 4⍴⍳12" #(15 18 21 24))
	    (is "-⌿3 4⍴⍳12" #(5 6 7 8))
	    (is "{⍺×⍵+3}⌿3 4⍴⍳12" #(63 162 303 492))
	    (is "+⌿[2]3 4⍴⍳12" #(10 26 42))))
  (\\ (has :title "Scan")
      (lateral (with-operand-derived (left-glyph left-function-dyadic)
		 (lambda (axes) `(apply-scanning ,left-glyph ,left-function-dyadic ,axes))))
      (tests (is "+\\1 2 3 4 5" #(1 3 6 10 15))
  	     (is "+\\3 4⍴⍳12" #2A((1 3 6 10) (5 11 18 26) (9 19 30 42)))
  	     (is "+\\[1]3 4⍴⍳12" #2A((1 2 3 4) (6 8 10 12) (15 18 21 24)))))
  (⍀ (has :title "Scan First")
     (lateral (with-operand-derived (left-glyph left-function-dyadic)
		(lambda (axes) `(apply-scanning ,left-glyph ,left-function-dyadic ,axes t))))
     (tests (is "+⍀1 2 3 4 5" #(1 3 6 10 15))
  	    (is "+⍀3 4⍴⍳12" #2A((1 2 3 4) (6 8 10 12) (15 18 21 24)))
  	    (is "{⍺×⍵+3}⍀3 4⍴⍳12" #2A((1 2 3 4) (8 18 30 44) (96 234 420 660)))
  	    (is "+⍀[2]3 4⍴⍳12" #2A((1 3 6 10) (5 11 18 26) (9 19 30 42)))))
  (\¨ (has :title "Each")
      (lateral (with-operand-derived (left-glyph left-function-monadic left-function-dyadic)
		 (lambda (axes) (declare (ignore axes))
			 `(apply-to-each ,left-glyph ,left-function-monadic ,left-function-dyadic))))
      (tests (is "⍳¨1 2 3" #(#(1) #(1 2) #(1 2 3)))
  	     (is "3⍴¨1 2 3" #(#(1 1 1) #(2 2 2) #(3 3 3)))
  	     (is "3 4 5⍴¨3" #(#(3 3 3) #(3 3 3 3) #(3 3 3 3 3)))
  	     (is "1 ¯1⌽¨⊂⍳5" #(#(2 3 4 5 1) #(5 1 2 3 4)))
	     (is "3+¨3 3⍴⍳9" #2A((4 5 6) (7 8 9) (10 11 12)))
	     (is "⊃⍪/,/(⊂2 2⍴2 3 1 4){⍺+⍵××/⍴⍺}¨3 3⍴⍳9" #2A((6 7 10 11 14 15) (5 8 9 12 13 16)
							    (18 19 22 23 26 27) (17 20 21 24 25 28)
							    (30 31 34 35 38 39) (29 32 33 36 37 40)))
	     (is "(⍳3)⌽[1]¨⊂2 3 4⍴⍳9" #(#3A(((4 5 6 7) (8 9 1 2) (3 4 5 6)) ((1 2 3 4) (5 6 7 8) (9 1 2 3)))
					#3A(((1 2 3 4) (5 6 7 8) (9 1 2 3)) ((4 5 6 7) (8 9 1 2) (3 4 5 6)))
					#3A(((4 5 6 7) (8 9 1 2) (3 4 5 6)) ((1 2 3 4) (5 6 7 8) (9 1 2 3)))))
	     (is "(⍳3)⌽[2]¨⊂2 3 4⍴⍳9" #(#3A(((5 6 7 8) (9 1 2 3) (1 2 3 4)) ((8 9 1 2) (3 4 5 6) (4 5 6 7)))
					#3A(((9 1 2 3) (1 2 3 4) (5 6 7 8)) ((3 4 5 6) (4 5 6 7) (8 9 1 2)))
					#3A(((1 2 3 4) (5 6 7 8) (9 1 2 3)) ((4 5 6 7) (8 9 1 2) (3 4 5 6)))))
	     (is "(⍳3)⌽[3]¨⊂2 3 4⍴⍳9" #(#3A(((2 3 4 1) (6 7 8 5) (1 2 3 9)) ((5 6 7 4) (9 1 2 8) (4 5 6 3)))
					#3A(((3 4 1 2) (7 8 5 6) (2 3 9 1)) ((6 7 4 5) (1 2 8 9) (5 6 3 4)))
					#3A(((4 1 2 3) (8 5 6 7) (3 9 1 2)) ((7 4 5 6) (2 8 9 1) (6 3 4 5)))))))
  (⍨ (has :title "Commute")
     (lateral (with-operand-derived (left-glyph left-function-dyadic)
		(lambda (axes) (declare (ignore axes))
			`(apply-commuting ,left-glyph ,left-function-dyadic))))
     (tests (is "5-⍨10" 5)
  	    (is "+⍨10" 20)
  	    (is "fn←{⍺+3×⍵} ⋄ 16 fn⍨8" 56)))
  (⌸ (has :title "Key")
     (lateral (with-operand-derived (left-glyph left-function-dyadic)
		(lambda (axes) (declare (ignore axes))
			`(apply-to-grouped ,left-glyph ,left-function-dyadic))))
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
      (pivotal (with-operand-derived (right-glyph right-function-dyadic
						  left-glyph left-function-dyadic left-function-symbolic)
		 (lambda (right left)
		   (declare (ignore right left))
		   (if (eq :outer-product-designator left-function-symbolic)
		       `(apply-producing-outer ,right-glyph ,right-function-dyadic)
		       `(apply-producing-inner ,right-glyph ,right-function-dyadic
					       ,left-glyph ,left-function-dyadic)))))
      (tests (is "2+.×3 4 5" 24)
	     (is "2 3 4+.×8 15 21" 145)
	     (is "2 3 4+.×3 3⍴3 1 4 1 5 9 2 6 5" #(17 41 55))
	     (is "(3 3⍴3 1 4 1 5 9 2 6 5)+.×2 3 4" #(25 53 42))
	     (is "{⍵ ⍵+.+⍵ ⍵} 3 3⍴⍳9" #(#2A((4 8 12) (16 20 24) (28 32 36))))
	     (is "5∘.+5" 10)
	     (is "4 5 6∘.+20 30 40 50" #2A((24 34 44 54) (25 35 45 55) (26 36 46 56)))
	     (is "1 2 3∘.-1 2 3" #2A((0 -1 -2) (1 0 -1) (2 1 0)))
	     (is "1 2 3∘.⍴1 2 3" #2A((#(1) #(2) #(3))(#(1 1) #(2 2) #(3 3)) (#(1 1 1) #(2 2 2) #(3 3 3))))
	     (is "1 2 3∘.⍴⊂1 2 3" #(#(1) #(1 2) #(1 2 3)))
	     (is "1 2 3∘.⌽⊂1 2 3" #(#(2 3 1) #(3 1 2) #(1 2 3)))
	     (is "1 2 3∘.⌽⊂4 5 6 7" #(#(5 6 7 4) #(6 7 4 5) #(7 4 5 6)))
	     (is "1 2 3∘.⌽[1]⊂2 3 4⍴⍳9" #(#3A(((4 5 6 7) (8 9 1 2) (3 4 5 6)) ((1 2 3 4) (5 6 7 8) (9 1 2 3)))
					  #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3)) ((4 5 6 7) (8 9 1 2) (3 4 5 6)))
					  #3A(((4 5 6 7) (8 9 1 2) (3 4 5 6)) ((1 2 3 4) (5 6 7 8) (9 1 2 3)))))
	     (is "1 2 3∘.⌽[2]⊂2 3 4⍴⍳9" #(#3A(((5 6 7 8) (9 1 2 3) (1 2 3 4)) ((8 9 1 2) (3 4 5 6) (4 5 6 7)))
					  #3A(((9 1 2 3) (1 2 3 4) (5 6 7 8)) ((3 4 5 6) (4 5 6 7) (8 9 1 2)))
					  #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3)) ((4 5 6 7) (8 9 1 2) (3 4 5 6)))))
	     (is "1 2 3∘.⌽[3]⊂2 3 4⍴⍳9" #(#3A(((2 3 4 1) (6 7 8 5) (1 2 3 9)) ((5 6 7 4) (9 1 2 8) (4 5 6 3)))
					  #3A(((3 4 1 2) (7 8 5 6) (2 3 9 1)) ((6 7 4 5) (1 2 8 9) (5 6 3 4)))
					  #3A(((4 1 2 3) (8 5 6 7) (3 9 1 2)) ((7 4 5 6) (2 8 9 1) (6 3 4 5)))))
	     (is "fn←{⍺×⍵+1} ⋄ 1 2 3∘.fn 4 5 6" #2A((5 6 7) (10 12 14) (15 18 21)))))
  (∘ (has :title "Compose")
     (pivotal (with-operand-derived (right-glyph right-function-monadic right-function-dyadic
						 left-glyph left-function-monadic left-function-dyadic)
  		(lambda (right left)
		  `(apply-composed ,right-glyph ,right ,right-function-monadic ,right-function-dyadic
				   ,left-glyph ,left ,left-function-monadic ,left-function-dyadic
				   ,(or (and (listp left)
					     (eql 'lambda (first left))
					     (= 1 (length (second left))))
					(not left-function-dyadic))))))
     (tests (is "fn←⍴∘⍴ ⋄ fn 2 3 4⍴⍳9" 3)
  	    (is "⍴∘⍴ 2 3 4⍴⍳9" 3)
  	    (is "⍴∘⍴∘⍴ 2 3 4⍴⍳9" #0A0)
  	    (is "÷∘5 ⊢30" 6)
  	    (is "⌊10000×(+∘*∘0.5) 4 16 25" #(56487 176487 266487))
  	    (is "fn←5∘- ⋄ fn 2" 3)
  	    (is "⌊0.5∘+∘*5 8 12" #(148 2981 162755))
  	    (is "⌊10000×+∘÷/40/1" 16180)
  	    (is "fn←+/ ⋄ fn∘⍳¨2 5 8" #(3 15 36))
  	    (is "3 4⍴∘⍴2 4 5⍴9" #2A((2 4 5 2) (4 5 2 4) (5 2 4 5)))))
  (⍤ (has :title "Rank")
     (pivotal (with-operand-derived (left-glyph left-function-monadic left-function-dyadic)
		(lambda (right left)
		  (declare (ignore left))
		  `(apply-at-rank ,right ,left-glyph ,left-function-monadic ,left-function-dyadic))))
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
     (pivotal (with-operand-derived (right-glyph right-function-dyadic left-glyph left-function-monadic)
		(lambda (right left)
		  (let ((op-left (or left-function-monadic left)))
		    ;; if the right operand is a function, it expresses the criteria for ending the
		    ;; activity of the left function; if the right operand is not a function, it must
		    ;; be a number counting the times the left function is to be compounded
		    (if right-function-dyadic
			`(apply-until ,right-glyph ,right-function-dyadic ,left-glyph ,op-left)
			`(apply-to-power ,right ,left-glyph ,op-left))))))
     (tests (is "fn←{2+⍵}⍣3 ⋄ fn 5" 11)
  	    (is "{2+⍵}⍣3⊢9" 15)
  	    (is "2{⍺×2+⍵}⍣3⊢9" 100)
  	    (is "fn←{2+⍵}⍣{10<⍺} ⋄ fn 2" 12)
  	    (is "fn←{2+⍵}⍣{10<⍵} ⋄ fn 2" 14)
  	    (is "fn←{⍵×2} ⋄ fn⍣3⊢4" 32)))
  (@ (has :title "At")
     (pivotal (with-operand-derived (right-glyph right-function-monadic
						 left-glyph left-function-monadic left-function-dyadic)
		(lambda (right left)
		  `(apply-at ,right-glyph ,right ,right-function-monadic
			     ,left-glyph ,left ,left-function-monadic ,left-function-dyadic))))
     (tests (is "20 20@3 8⍳9" #(1 2 20 4 5 6 7 20 9))
  	    (is "((2 5⍴0 1)@2 5) 5 5⍴⍳9" #2A((1 2 3 4 5) (0 1 0 1 0) (2 3 4 5 6)
  					     (7 8 9 1 2) (1 0 1 0 1)))
  	    (is "0@(×∘(3∘|)) ⍳9" #(0 0 3 0 0 6 0 0 9))
  	    (is "÷@3 5 ⍳9" #(1 2 1/3 4 1/5 6 7 8 9))
  	    (is "{⍵×2}@{⍵>3}⍳9" #(1 2 3 8 10 12 14 16 18))
  	    (is "fn←{⍺+⍵×12} ⋄ test←{0=3|⍵} ⋄ 4 fn@test ⍳12" #(1 2 40 4 5 76 7 8 112 10 11 148))))
  (⌺ (has :title "Stencil")
     (pivotal (with-operand-derived (left-glyph left-function-dyadic)
		(lambda (right left)
		  (declare (ignore left))
		  `(apply-stenciled ,right ,left-glyph ,left-function-dyadic))))
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
  					  #(#(-1 -1) #2A((5 6 0) (8 9 0) (0 0 0))))))
  	    (is "+⌺(⍪6 2)⍳8" #2A((2 2 3 4 5 6) (1 2 3 4 5 6) (3 4 5 6 7 8) (3 4 5 6 -2 -2))))))

 (operators
  (with (:name :lexical-operators-unitary)
	(:tests-profile :title "Unitary Operator Tests")
	(:demo-profile :title "Unitary Operator Demos"
		       :description "Unitary operators take no operands and return a niladic function that returns a value; the use of unitary operators is to manifest syntax structures wherein depending on the outcome of some expressions, other expressions may or may not be evaluated, as with the [$ if] operator."))
  ($ (has :title "If")
     (unitary (lambda (workspace axes)
		(declare (ignore workspace))
		(let ((condition (gensym)))
		  (labels ((build-clauses (clauses)
			     `(let ((,condition (disclose-atom ,(first clauses))))
				(if (or (not (integerp ,condition))
					(/= 0 ,condition))
				    ,(second clauses)
				    ,(if (third clauses)
					 (if (fourth clauses)
					     (build-clauses (cddr clauses))
					     (third clauses))
					 (make-array nil))))))
		    (build-clauses axes)))))
     (tests (is "$[1;2;3]" 2)
	    (is "$[0;2;3]" 3)
	    (is "x←5 ⋄ y←3 ⋄ $[y>2;x+←10;x+←20] ⋄ x" 15))))

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
  (for "Quote marks in string escaped using traditional double-quote method." "'''abc'''" "'abc'")
  (for "Quote marks in string escaped with backslashes." "'\\'abc\\''" "'abc'")
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
  (for "Definition and use of n-argument function."
       "fn←{[x;y;z] x+y×z} ⋄ fn[4;5;6]" 34)
  (for "Inline n-argument function."
       "{[a;b;c;d](a-c)×b/d}[7;4;2;⍳3]" #(5 5 5 5 10 10 10 10 15 15 15 15))
  (for "Variable-referenced values, including an element within an array, in a vector."
       "a←9 ⋄ b←2 3 4⍴⍳9 ⋄ 1 2 a 3 b[1;2;1]" #(1 2 9 3 5))
  (for "Application of functions to indexed array elements."
       "g←2 3 4 5 ⋄ 9,g[2],3 4" #(9 3 3 4))
  (for "Assignment of an element within an array."
       "a←2 3⍴⍳9 ⋄ a[1;2]←20 ⋄ a" #2A((1 20 3) (4 5 6)))
  (for "Selection from an array with multiple elided dimensions."
       "(2 3 3 4 5⍴⍳9)[2;;3;;2]" #2A((6 2 7 3) (3 8 4 9) (9 5 1 6)))
  (for "Selection from an array with multi-index, array and elided dimensions."
       "(3 3 3⍴⍳27)[1 2;2 2⍴⍳3;]" #4A((((1 2 3) (4 5 6)) ((7 8 9) (1 2 3)))
				      (((10 11 12) (13 14 15)) ((16 17 18) (10 11 12)))))
  (for "Elided assignment."
       "a←2 3 4⍴⍳9 ⋄ a[2;;3]←0 ⋄ a" #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3)) ((4 5 0 7) (8 9 0 2) (3 4 0 6))))
  (for "Assignment from an array to an area of an array with the same shape."
       "x←8 8⍴0 ⋄ x[2+⍳3;3+⍳4]←3 4⍴⍳9 ⋄ x" #2A((0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0)
					       (0 0 0 1 2 3 4 0) (0 0 0 5 6 7 8 0)
					       (0 0 0 9 1 2 3 0) (0 0 0 0 0 0 0 0)
					       (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0)))
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
  (with (:name :printed-format-tests)
	(:tests-profile :title "Printed Data Format Tests")
	(:demo-profile :title "Data Format Demos"
		       :description "More demos showing how different types of data are formatted in April."))
  (for-printed "Numeric vector." "1+1 2 3" "2 3 4
")
  (for-printed "Vector of mixed integers and floats." "12.5 3 42.890 90.5001 8 65"
	       "12.5 3 42.89 90.5001 8 65
")
  (for-printed "Oversized take of float vector, the filler zeroes printed without decimal points."
	       "6↑○⍳3" "3.141592654 6.283185307 9.424777961 0 0 0
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
"))

 (arbitrary-test-set
  (with (:name :output-specification-tests)
	(:tests-profile :title "Output Specification Tests"))
  ((progn (princ (format nil "λ Evaluation of ⍳ with specified index origin.~%"))
	  (is (print-and-run (april (with (:state :index-origin 0)) "⍳9"))
	      #(0 1 2 3 4 5 6 7 8) :test #'equalp))
   (let ((out-str (make-string-output-stream)))
     (princ (format nil "λ Printed output at given precisions.~%"))
     (print-and-run (april-p (with (:state :print-to out-str :print-precision 3)) "○1 2 3"))
     (is (get-output-stream-string out-str)
	 "3.14 6.28 9.42
")
     (princ (format nil "~%"))
     (print-and-run (april-p (with (:state :print-to out-str :print-precision 6)) "○1 2 3"))
     (is (get-output-stream-string out-str)
	 "3.14159 6.28319 9.42478
"))
   (progn (princ (format nil "λ Output of one input and one declared variable with index origin set to 0.~%"))
	  (multiple-value-bind (out1 out2)
	      (print-and-run (april (with (:state :count-from 0 :in ((a 3) (b 5))
						  :out (a c)))
				    "c←a+⍳b"))
	    (is out1 3)
	    (princ (format nil "~%"))
	    (is out2 #(3 4 5 6 7) :test #'equalp)))
   (progn (princ (format nil "λ Output of both value and APL-formatted value string.~%"))
	  (multiple-value-bind (out1 out2)
	      (print-and-run (april (with (:state :output-printed t)) "2 3⍴⍳9"))
	    
	    (is out1 #2A((1 2 3) (4 5 6)) :test #'equalp)
	    (princ (format nil "~%"))
	    (is out2 "1 2 3
4 5 6
")))
   (progn (format nil "λ Output of APL-formatted value string alone.~%")
	  (is (print-and-run (april (with (:state :output-printed :only)) "2 3⍴⍳9"))
	      "1 2 3
4 5 6
"))
   (progn (princ (format nil "λ Output of three internally-declared variables.~%"))
	  (multiple-value-bind (out1 out2 out3)
	      (print-and-run (april (with (:state :out (a b c)))
				    "a←9+2 ⋄ b←5+3 ⋄ c←2×9"))
	    (princ (format nil "~%"))
	    
	    (is out1 11)
	    (princ (format nil "~%"))
	    (is out2 8)
	    (princ (format nil "~%"))
	    (is out3 18)))
   (progn (princ (format nil "λ Output using ⎕← to specified output stream.~%"))
	  (let* ((out-str (make-string-output-stream))
		 (vector (print-and-run (april (with (:state :print-to out-str))
					       "a←1 2 3 ⋄ ⎕←a+5 ⋄ ⎕←3 4 5 ⋄ 3+a"))))

	    (is vector #(4 5 6) :test #'equalp)

	    (princ (format nil "~%"))
	    
	    (is (print-and-run (get-output-stream-string out-str))
		"6 7 8
3 4 5
")))
   (let* ((out-str (make-string-output-stream))
	  (other-out-str (make-string-output-stream)))
     (print-and-run (april-p "a←1 2 3 ⋄ ⎕ost←('APRIL' 'OUT-STR') ⋄ ⎕←a+5 ⋄ ⎕←3 4 5 
⎕ost←('APRIL' 'OTHER-OUT-STR') ⋄ 3+a"))
     (princ (format nil "~%~%"))
     (is (print-and-run (get-output-stream-string out-str))
	 "6 7 8
3 4 5
" :test #'equalp)
     (princ (format nil "~%"))
     (is (print-and-run (get-output-stream-string other-out-str))
	 "4 5 6
" :test #'equalp))
   (progn (princ (format nil "λ Compact function calls.~%"))
	  
	  (is (print-and-run (april-c "{⍺×⍵}" 2 8)) 16)
	  
	  (princ (format nil "~%"))
	  
	  (is (print-and-run (april-c "{[a;b;c;d] d↑c⍴a+b}" 3 5 6 10))
	      #(8 8 8 8 8 8 0 0 0 0) :test #'equalp)
	  
	  (princ (format nil "~%"))

	  (is (print-and-run (april-c (with (:state :count-from 0)) "{⍳⍵}" 7))
	      #(0 1 2 3 4 5 6) :test #'equalp))
   
   )))

#|
This is an example showing how the April idiom can be extended with Vex's extend-vex-idiom macro.
A not-very-useful scalar function that adds 3 to its argument(s) is specified here.

(extend-vex-idiom
 april
 (utilities :process-lexicon #'april-function-glyph-processor)
 (functions
  (with (:name :extra-functions))
  (⍛ (has :title "Add3")
     (ambivalent (scalar-function (λω (+ 3 omega)))
		 (scalar-function (lambda (alpha omega) (+ 3 alpha omega))))
     (tests (is "⍛77" 80)
	    (is "8⍛7" 18)))))
|#
