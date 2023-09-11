;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; spec.lisp

(in-package #:april)

"This specification defines the April language. All of the standard functions and operators and their symbols, along with the language's grammar, utilities, reserved symbols, tests and demo suite are specified here."

(let ((circular-functions ;; APL's set of circular functions called using the ○ symbol with a left argument
       (vector (lambda (x) (exp (complex 0 x)))
               (lambda (x) (complex 0 x))
               #'conjugate #'identity (lambda (x) (- (sqrt (- (1+ (expt x 2))))))
               #'atanh #'acosh #'asinh (lambda (x) (if (= -1 x) 0 (* (1+ x) (sqrt (/ (1- x) (1+ x))))))
               #'atan #+ecl #'cmucl-complex-acos #+(not ecl) #'acos
               #'asin (lambda (x) (sqrt (- 1 (expt x 2))))
               #'sin #'cos #'tan (lambda (x) (sqrt (1+ (expt x 2))))
               #'sinh #'cosh #'tanh (lambda (x) (sqrt (- (1+ (expt x 2)))))
               #'realpart #'abs #'imagpart #'phase))
      (coercing-indices (make-array 6 :element-type '(unsigned-byte 8)
                                    :initial-contents '(1 2 3 21 22 23))))
  ;; ECL defaults to C's standard acos thus its function must be specially assigned
  (defun call-circular (&optional inverse)
    (lambda (value function-index)
      (if (and (integerp function-index) (<= -12 function-index 12))
          (let ((vector-index (+ 12 (funcall (if inverse #'- #'identity)
                                             function-index))))
            (funcall (aref circular-functions vector-index)
                     ;; for some functions, double coercion is not needed
                     (* value (if (position vector-index coercing-indices :test #'=)
                                  1 1.0d0))))
          (error "Invalid argument to [○ circular]; the left argument must be an~a"
                 " integer between ¯12 and 12.")))))
  
(setf *value-composable-lexical-operators* (list #\⍨))

;; top-level specification for the April language
(specify-vex-idiom
 april

 ;; system variables and default state of an April workspace
 (system :output-printed nil
         :base-state '(:output-stream '*standard-output*)
         :workspace-defaults '(:index-origin 1 :print-precision 10 :division-method 0
                               :comparison-tolerance double-float-epsilon
                               :rngs (list :generators :rng (aref *rng-names* 1)))
         :variables *system-variables* :string-delimiters "'\"" :comment-delimiters "⍝"
         :closure-wrapping "()" :function-wrapping "{}" :axis-wrapping "[]"
         :negative-signs-pattern "[¯]" :number-spacers-pattern "[_]" :axis-separators ";"
         :path-separators "." :supplemental-numeric-chars "._¯eEjJrR" :supplemental-token-chars "._⎕∆⍙¯"
         :newline-characters (coerce '(#\Newline #\Return) 'string))

 ;; parameters for describing and documenting the idiom in different ways; currently, these options give
 ;; the order in which output from the blocks of tests is printed out for the (test) and (demo) options
 (profiles (:test :lexical-functions-scalar-numeric :lexical-functions-scalar-logical
                  :lexical-functions-array :lexical-functions-special :lexical-operators-lateral
                  :lexical-operators-pivotal :lexical-statements :general-tests
                  :system-variable-function-tests :function-inversion-tests :namespace-tests
                  :printed-format-tests)
           (:arbitrary-test :output-specification-tests)
           (:time :lexical-functions-scalar-numeric :lexical-functions-scalar-logical
                  :lexical-functions-array :lexical-functions-special :lexical-operators-lateral
                  :lexical-operators-pivotal :lexical-statements :general-tests)
           (:demo :general-tests :lexical-functions-scalar-numeric :lexical-functions-scalar-logical
                  :lexical-functions-array :lexical-functions-special :lexical-operators-lateral
                  :lexical-operators-pivotal :lexical-statements :system-variable-function-tests
                  :function-inversion-tests :namespace-tests :printed-format-tests))

 ;; utilities for compiling the language
 (utilities :match-blank-character (let ((cstring (coerce '(#\  #\Tab) 'string)))
                                     (lambda (char) (position char cstring :test #'char=)))
            :match-newline-character (let ((cstring (coerce '(#\⋄ #\◊ #\Newline #\Return) 'string)))
                                       (lambda (char) (position char cstring :test #'char=)))
            ;; set the language's valid blank, newline characters and token characters
            :match-numeric-character
            (let ((other-chars))
              (lambda (char idiom)
                (unless other-chars (setf other-chars (of-system idiom :supplemental-numeric-chars)))
                (or (digit-char-p char) (position char other-chars :test #'char=))))
            :match-token-character
            (let ((other-chars))
              (lambda (char idiom)
                (unless other-chars (setf other-chars (of-system idiom :supplemental-token-chars)))
                (or (is-alphanumeric char) (position char other-chars :test #'char=))))
            ;; match characters that can only appear in homogenous symbols, this is needed so that
            ;; things like ⍺⍺.⍵⍵, ⍺∇⍵ or ⎕NS⍬ can work without spaces between the symbols
            :match-uniform-token-character (lambda (char) (position char "⍺⍵⍶⍹∇⍬" :test #'char=))
            ;; match characters specifically representing function/operator arguments, this is needed
            ;; so ⍵.path.to will work
            :match-arg-token-character (lambda (char) (position char "⍺⍵⍶⍹" :test #'char=))
            ;; match characters used to link parts of paths together like namespace.path.to,
            ;; this is needed so that ⍵.path.to will work
            :match-path-joining-character (let ((chars))
                                            (lambda (char idiom)
                                              (unless chars (setf chars (of-system idiom :path-separators)))
                                              (position char chars :test #'char=)))
            ;; overloaded numeric characters may be functions or operators or may be part of a numeric token
            ;; depending on their context
            :match-overloaded-numeric-character (lambda (char) (char= char #\.))
            ;; match character(s) used to separate axes
            :match-axis-separating-character (let ((chars))
                                               (lambda (char idiom)
                                                 (unless chars
                                                   (setf chars (of-system idiom :axis-separators)))
                                                 (position char chars :test #'char=)))
            ;; generate the string of matched closing and opening characters that wrap code sections;
            ;; used to identify stray closing characters such as ) without a corresponding (
            :collect-delimiters
            (lambda (idiom)
              (let ((output) (cw (of-system idiom :closure-wrapping))
                    (fw (of-system idiom :function-wrapping)) (aw (of-system idiom :axis-wrapping)))
                (loop :for i :from (/ (length cw) 2) :to (1- (length cw)) :do (push (aref cw i) output))
                (loop :for i :from (/ (length fw) 2) :to (1- (length fw)) :do (push (aref fw i) output))
                (loop :for i :from (/ (length aw) 2) :to (1- (length aw)) :do (push (aref aw i) output))
                (loop :for i :below (/ (length cw) 2) :do (push (aref cw i) output))
                (loop :for i :below (/ (length fw) 2) :do (push (aref fw i) output))
                (loop :for i :below (/ (length aw) 2) :do (push (aref aw i) output))
                (reverse (coerce output 'string))))
            ;; this code preprocessor removes comments, starting with each ⍝ and ending before the next newline
            :prep-code-string
            (lambda (idiom)
              (let ((nlstring (of-system idiom :newline-characters))
                    (comment-delimiters (of-system idiom :comment-delimiters)))
                (lambda (string)
                  (let ((commented) (osindex 0)
                        (out-string (make-string (length string) :initial-element #\ )))
                    (loop :for char :across string
                          :do (if commented (when (position char nlstring :test #'char=)
                                              (setf commented nil
                                                    (row-major-aref out-string osindex) char
                                                    osindex (1+ osindex)))
                                  (if (position char comment-delimiters :test #'char=)
                                      (setf commented t)
                                      (setf (row-major-aref out-string osindex) char
                                            osindex (1+ osindex)))))
                    ;; return displaced string to save time processing blanks
                    (make-array osindex :element-type 'character :displaced-to out-string)))))
            ;; handles axis strings like "'2;3;;' from 'array[2;3;;]'"
            :process-axis-string
            (let ((delimiters) (axis-separators) (full-len) (half-len) (nesting (vector 0 0 0)))
              (lambda (string idiom)
                (unless delimiters
                  (setf delimiters (reverse (funcall (of-utilities idiom :collect-delimiters) idiom))
                        full-len (length delimiters)
                        half-len (/ full-len 2)
                        axis-separators (of-system idiom :axis-separators)))
                (let ((indices) (last-index) (quoted))
                  (loop :for i :below (length nesting) :do (setf (aref nesting i) 0))
                  (loop :for char :across string :counting char :into charix
                        :do (let ((mx (or (loop :for d :across delimiters :counting d :into dx
                                                :when (char= d char) :do (return (- full-len -1 dx)))
                                          0)))
                              (if (position char (of-system idiom :string-delimiters) :test #'char=)
                                  (setf quoted (not quoted))
                                  (unless quoted
                                    (if (< half-len mx) (incf (aref nesting (- full-len mx)))
                                        (if (<= 1 mx half-len)
                                            (if (< 0 (aref nesting (- half-len mx)))
                                                (decf (aref nesting (- half-len mx)))
                                                (error "Each closing ~a must match with an opening ~a."
                                                       (aref delimiters mx)
                                                       (aref delimiters (- half-len mx))))
                                            (when (and (position char axis-separators :test #'char=)
                                                       (zerop (loop :for ncount :across nesting
                                                                    :summing ncount)))
                                              (setq indices (cons (1- charix) indices)))))))))
                  (loop :for index :in (reverse (cons (length string) indices))
                        :counting index :into iix
                        :collect (make-array (- index (if last-index 1 0)
                                                (or last-index 0))
                                             :element-type 'character :displaced-to string
                                             :displaced-index-offset (if last-index (1+ last-index) 0))
                        :do (setq last-index index)))))
            ;; macro to process lexical specs of functions and operators
            :process-fn-op-specs #'process-fnspecs
            :test-parameters '((:space unit-test-staging))
            :number-formatter #'parse-apl-number-string
            :format-value #'format-value
            ;; process system state input passed as with (april (with (:state ...)) "...")
            :preprocess-state-input
            (lambda (state)
              (when (getf state :count-from)
                (setf (getf state :index-origin) (getf state :count-from)))
              state)
            ;; converts parts of the system state into lists that will form part of the local lexical
            ;; environment in which the compiled APL code runs, i.e. the (let) form into which
            ;; the APL-generating macros are expanded
            :system-lexical-environment-interface
            (lambda (state)
              ;; the index origin, print precision and output stream values are
              ;; passed into the local lexical environment
              (append (list (list (find-symbol "OUTPUT-STREAM" *package-name-string*)
                                  (or (getf state :print-to)
                                      (second (getf state :output-stream)))))
                      (loop :for (key value) :on *system-variables* :by #'cddr
                         :collect (list (find-symbol (string-upcase key) *package-name-string*)
                                        (or (getf state key) `(inwsd ,value))))))
            :lexer-postprocess #'lexer-postprocess
            :compile-form #'compile-form
            :postprocess-compiled
            (lambda (state &rest inline-arguments)
              (lambda (form)
                ;; form assignment accounts for cases like (april-c "+" 1 2)
                (let* ((form (if (not (and (= 1 (length form)) (characterp (first form))
                                           (of-lexicons this-idiom (first form) :functions)))
                                 form (list (build-call-form (first form)))))
                       ;; operands for cases like (april-c "{⍵⍵ ⍺⍺/⍵}" #'+ #'- #(1 2 3 4 5))
                       (operands (when (and inline-arguments (listp (first form))
                                            (eql 'olambda (caar form)))
                                   (cadar form)))
                       (final-form (if inline-arguments
                                       (if operands
                                           `(a-call (a-comp :op ,(first (last form))
                                                            ,(first inline-arguments)
                                                            ,@(if (intersection operands '(⍵⍵ ⍹))
                                                                  (list (second inline-arguments))))
                                                    ,@(if (intersection operands '(⍵⍵ ⍹))
                                                          (cddr inline-arguments)
                                                          (rest inline-arguments)))
                                           `(a-call ,(first (last form)) ,@inline-arguments))
                                       (first (last form)))))
                  (append (butlast form)
                          (list (append (list 'a-out final-form)
                                        (append (list :print-precision 'print-precision)
                                                (when (getf state :unrendered) (list :unrendered t))
                                                (when (getf state :print) (list :print-to 'output-stream))
                                                (when (getf state :output-printed)
                                                  (list :output-printed
                                                        (getf state :output-printed))))))))))
            :postprocess-value
            (lambda (form state)
              (append (list 'a-out form)
                      (append (list :print-precision 'print-precision)
                              (when (getf state :print) (list :print-to 'output-stream))
                              (when (getf state :output-printed)
                                (list :output-printed (getf state :output-printed))))))
            :process-stored-symbol
            (lambda (symbol space is-function)
              (if is-function (let ((found-sym (find-symbol symbol space)))
                                (when (and found-sym (boundp found-sym)
                                           (not (fboundp found-sym)))
                                  (makunbound found-sym))
                                (setf (symbol-function found-sym) #'dummy-nargument-function))
                  (let ((found-sym (find-symbol symbol space)))
                    (when (fboundp found-sym) (fmakunbound found-sym))
                    (unless (and found-sym (boundp found-sym))
                      (proclaim (list 'special (intern symbol space)))
                      (set (intern symbol space) nil)))))
            ;; build multiple output of April expression, rendering unless (:unrendered) option is passed
            :process-multiple-outputs
            (lambda (outputs space &optional will-render)
              (list (cons 'values (mapcar (lambda (return-var)
                                            (let ((symbol (intern (lisp->camel-case return-var)
                                                                  space)))
                                              (if (not will-render)
                                                  symbol `(process-ns-output (vrender ,symbol)))))
                                          outputs))))
            :build-variable-declarations #'build-variable-declarations
            :build-compiled-code #'build-compiled-code
            :assign-val-sym 'ws-assign-val :assign-fun-sym 'ws-assign-fun)

 ;; specs for multi-character symbols exposed within the language
 (symbols (:variable ⎕ to-output ⎕io *index-origin* ⎕pp print-precision ⎕div *division-method*
                     ⎕ost output-stream ⎕ct *comparison-tolerance* ⎕rl *rngs*)
          (:constant ⎕a *alphabet-vector* ⎕d *digit-vector* ⎕ts *apl-timestamp*)
          (:function ⎕ns make-namespace ⎕cs change-namespace ⎕ty coerce-or-get-type
                     ⎕ucs scalar-code-char ⎕fmt (format-array-uncollated print-precision)
                     ⎕xwv external-workspace-value ⎕xwf external-workspace-function
                     ⎕xwo external-workspace-operator))
 
 ;; APL's set of lexical functions, monadic or dyadic operations represented by a single character
 (functions
  (with (:name :lexical-functions-scalar-numeric)
        (:tests-profile :title "Scalar Numeric Function Tests")
        (:demo-profile :title "Scalar Numeric Function Demos"
                       :description "Scalar numeric functions change individual numeric values. They include basic arithmetic and other numeric operations, and they can be applied over arrays."))
  (+ (has :titles ("Conjugate" "Add"))
     (ambivalent (scalar-function conjugate)
                 (scalar-function +))
     (meta (monadic :id 0 :inverse (ac-wrap :m (scalar-function conjugate)))
           (dyadic :id 0 :commutative t :inverse (ac-wrap :d (scalar-function -))
                   :inverse-right (ac-wrap :d (scalar-function (reverse-op -)))
                   :inverse-commuted (ac-wrap :m (scalar-function (λω (/ omega 2))))))
     (tests (is "+5" 5)
            (is "+5J2" #C(5 -2))
            (is "1+1" 2)
            (is "1+1 2 3" #(2 3 4))))
  (- (has :titles ("Negate" "Subtract"))
     (ambivalent (scalar-function -)
                 (scalar-function (reverse-op -)))
     (meta (monadic :id 0 :inverse (ac-wrap :m (scalar-function (reverse-op -))))
           (dyadic :id 0 :inverse (ac-wrap :d (scalar-function (reverse-op -)))
                   :inverse-right (ac-wrap :d (scalar-function +)) :scan-alternating #'-))
     (tests (is "2-1" 1)
            (is "7-2 3 4" #(5 4 3))))
  (× (has :titles ("Sign" "Multiply"))
     (ambivalent (scalar-function signum)
                 (scalar-function *))
     (meta (monadic :id 1)
           (dyadic :id 1 :commutative t :inverse (ac-wrap :d (scalar-function /))
                   :inverse-right (ac-wrap :d (scalar-function (reverse-op /)))
                   :inverse-commuted (ac-wrap :m (scalar-function sqrt))))
     (tests (is "×20 5 0 ¯7 3 ¯9" #(1 1 0 -1 1 -1))
            (is "2×3" 6)
            (is "4 5×8 9" #(32 45))))
  (÷ (has :titles ("Reciprocal" "Divide"))
     (ambivalent (scalar-function (apl-divide division-method))
                 (scalar-function (apl-divide division-method)))
     (meta (primary :implicit-args (division-method) :scan-alternating #'/)
           (monadic :id 1 :inverse (ac-wrap :m (scalar-function (apl-divide division-method))))
           (dyadic :id 1 :inverse (ac-wrap :d (scalar-function (apl-divide division-method)))
                   :inverse-right (ac-wrap :d (scalar-function *)) :scan-alternating #'/))
     (tests (is "6÷2" 3)
            (is "12÷6 3 2" #(2 4 6))
            (is "÷2 4 8" #(1/2 1/4 1/8))
            (is "{⎕div←0 ⋄ ÷⍨⍵} 0" 1)
            (is "{⎕div←1 ⋄ ÷⍨⍵} 0" 0)
            (is "{⎕div←1 ⋄ ÷ ⍵} 0" 0)))
  (⋆ (has :titles ("Exponential" "Power") :aliases (*))
     (ambivalent (scalar-function apl-exp)
                 (scalar-function (reverse-op :dyadic apl-expt)))
     (meta (monadic :id 1 :inverse (ac-wrap :m (scalar-function apl-log)))
           (dyadic :id 1 :inverse (ac-wrap :d (scalar-function apl-log))
                   :inverse-right (ac-wrap :d (scalar-function (λωα (apl-expt alpha (/ omega)))))))
     (tests (is "⌊1000×⋆2" 7389)
            (is "2⋆4" 16)
            (is "⌊16⋆÷2" 4)
            (is "⌊100000×⋆0J1" #C(54030 84147))))
  (⍟ (has :titles ("Natural Logarithm" "Logarithm"))
     (ambivalent (scalar-function apl-log)
                 (scalar-function apl-log))
     (meta (monadic :inverse (ac-wrap :m (scalar-function apl-exp)))
           (dyadic :inverse (ac-wrap :d (scalar-function (reverse-op :dyadic apl-expt)))
                   :inverse-right (ac-wrap :d (scalar-function (λωα (apl-expt omega (/ alpha)))))))
     (tests (is "⌊1000×⍟5" 1609)
            (is "⌊2⍟8" 3)))
  (\| (has :titles ("Magnitude" "Residue"))
      (ambivalent (scalar-function abs)
                  (scalar-function (apl-residue comparison-tolerance)))
      (meta (primary :implicit-args (comparison-tolerance))
            (monadic :id 0)
            (dyadic :id 0))
      (tests (is "|55" 55)
             (is "|¯33" 33)
             (is "8|39" 7)
             (is "(3r8J12r7×⍳12)|7r2J5r9×⍳12"
                 #(#C(1/14 47/36) #C(1/7 47/18) #C(3/14 47/12) #C(2/7 47/9) #C(5/14 235/36) #C(3/7 47/6)
                   #C(1/2 329/36) #C(4/7 94/9) #C(9/14 47/4) #C(5/7 235/18) #C(11/14 517/36) #C(6/7 47/3)))))
  (! (has :titles ("Factorial" "Binomial"))
     (ambivalent (scalar-function sprfact)
                 (scalar-function binomial))
     (meta (monadic)
           (dyadic :id 1))
     (tests (is "!5" 120)
            (is "5!12" 792)
            (is "∘.!⍨¯3+⍳7" #2A((1 -1 0 0 0 0 0) (0 1 0 0 0 0 0) (1 1 1 1 1 1 1) (-2 -1 0 1 2 3 4)
                                (3 1 0 0 1 3 6) (-4 -1 0 0 0 1 4) (5 1 0 0 0 0 1)))))
  (⌈ (has :titles ("Ceiling" "Maximum"))
     (ambivalent (scalar-function (apl-ceiling comparison-tolerance))
                 (scalar-function (reverse-op max)))
     (meta (primary :implicit-args (comparison-tolerance))
           (monadic)
           (dyadic :id most-negative-double-float :commutative t
                   :inverse-commuted (ac-wrap :m (scalar-function identity))))
     (tests (is "⌈1.0001" 2)
            (is "⌈1.9998" 2)
            (is "3⌈0 1 2 3 4 5" #(3 3 3 3 4 5))
            (is "⌈21r5J3r11×⍳20"
                #(#C(4 1) #C(8 1) #C(13 1) #C(17 1) #C(21 2) #C(25 2) #C(30 2) #C(34 2) #C(38 3) #C(42 3)
                  #C(47 3) #C(51 3) #C(55 4) #C(59 4) #C(63 5) #C(67 5) #C(72 5) #C(76 5) #C(80 5) #C(84 6)))))
  (⌊ (has :titles ("Floor" "Minimum"))
     (ambivalent (scalar-function (apl-floor comparison-tolerance))
                 (scalar-function (reverse-op min)))
     (meta (primary :implicit-args (comparison-tolerance))
           (monadic)
           (dyadic :id most-positive-double-float :commutative t
                   :inverse-commuted (ac-wrap :m (scalar-function identity))))
     (tests (is "⌊1.0001" 1)
            (is "⌊1.9998" 1)
            (is "3⌊0 1 2 3 4 5" #(0 1 2 3 3 3))
            (is "⌊1.5J0.5" #C(1 1))
            (is "⌊21r5J3r11×⍳20"
                #(4 8 #C(12 1) #C(16 1) #C(21 1) #C(25 1) #C(29 2) #C(33 2) #C(38 2) #C(42 2) #C(46 3)
                  #C(50 3) #C(55 3) #C(58 4) #C(63 4) #C(67 4) #C(71 5) #C(75 5) #C(79 5) #C(84 5)))))
  (? (has :titles ("Random" "Deal"))
     (ambivalent (λω (make-instance 'vader-random :base omega :rng rngs :index-origin index-origin))
                 (λωα (make-instance 'vader-deal :base omega :argument alpha
                                                 :rng rngs :index-origin index-origin)))
     (meta (primary :implicit-args (index-origin rngs)))
     (tests (is "⍴5?⍴⍳5" #(5))
            (is "0=+/,3<?3 3⍴2" 1)
            (is "∧/,∘.=⍨⍤1⊢⍉↑{⎕RL←5 1 ⋄ 10?⍵}¨10⍴1000" 1)
            (is "∧/,∘.=⍨⍤1⊢⍉↑{⎕RL←7 0 ⋄ 10?⍵}¨10⍴1000" 1)
            (is "∧/,∘.=⍨⍤1⊢⍉↑{⎕RL←⍬ 2 ⋄ 10?⍵}¨10⍴1000" 0)))
  (○ (has :titles ("Pi Times" "Circular"))
     (ambivalent (scalar-function (λω (* omega (coerce pi 'double-float))))
                 (scalar-function (call-circular)))
     (meta (monadic :inverse (ac-wrap :m (scalar-function (λω (/ omega pi)))))
           (dyadic :inverse (ac-wrap :d (scalar-function (call-circular :inverse)))
                   :inverse-right (scalar-function (λωα (declare (ignore omega alpha))
                                                        (error "Inverse [○ circular] may not take an ~a"
                                                               "implicit right argument.")))))
     (tests (is "⌊100000×○1" 314159)
            (is "(⌊1000×1÷2⋆÷2)=⌊1000×1○○÷4" 1)
            (is "⌊1000×1○⍳9" #(841 909 141 -757 -959 -280 656 989 412))
            (is "⌈1 2 3○○.5 2 .25" #(1 1 1))
            ;; omit asin and atanh from the tests below because they
            ;; are not consistent across CL implementations
            (is "¯11 ¯10 ¯9 9 10 11○1" #(#C(0 1) 1 1 1 1 0))
            (is "⌊1000×⊃,/9 11○⊂(¯1 ¯7~⍨¯8+⍳16) ∘.○ 0 ¯2 2 ¯2J2 2J3.5"
                #2A((0 1316 1316 1734 2095 1570 3141 0 2325 1064)
                    (0 -1444 1443 -1735 2079 0 0 0 754 1038)
                    (0 -1733 1732 -1880 1940 1000 0 0 2128 3607)
                    (0 -1108 1107 -1312 1442 0 0 0 238 215)
                    (1570 3141 0 2325 1064 0 -1317 1316 -1735 -2096)
                    (1000 0 0 2128 3607 0 1732 1732 1879 -1941)
                    (0 -910 909 -3421 15069 0 0 0 -1510 -6885)
                    (1000 -417 -417 -1566 -6897 0 0 0 3297 -15043)
                    (0 2185 -2186 28 -2 0 0 0 1023 1001)
                    (1000 2236 2236 2128 2063 0 0 0 -1880 3392)
                    (0 -3627 3626 1509 -3397 0 0 0 3420 -1320)
                    (1000 3762 3762 -1566 -3524 0 0 0 -3298 -1273)
                    (0 -965 964 -1024 972 0 0 0 -29 23)
                    (0 0 0 1879 3392 1000 2236 2236 2128 -2064)))))
  (\~ (has :titles ("Not" "Without"))
      (ambivalent (scalar-function binary-not)
                  (λωα (make-instance 'vader-without :base omega :argument alpha)))
      (meta (monadic :inverse (ac-wrap :m (scalar-function binary-not))))
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
     (dyadic (scalar-function (boolean-op (compare-by '< comparison-tolerance))
                              :binary-output t))
     (meta (primary :implicit-args (comparison-tolerance))
           (dyadic :id 0))
     (tests (is "3<1 2 3 4 5" #*00011)))
  (≤ (has :title "Less or Equal")
     (dyadic (scalar-function (boolean-op (compare-by '<= comparison-tolerance))
                              :binary-output t))
     (meta (primary :implicit-args (comparison-tolerance))
           (dyadic :id 1))
     (tests (is "3≤1 2 3 4 5" #*00111)
            (is "1.0≤1.0" 1)
            (is "{⎕CT←0.0001 ⋄ (1.0000000001≤⍵),(1.0000000001≤⍨⍵),1.01≤⍵} 1.0" #*110)))
  (= (has :title "Equal")
     (dyadic (scalar-function (boolean-op (scalar-compare comparison-tolerance))
                              :binary-output t))
     (meta (primary :implicit-args (comparison-tolerance))
           (dyadic :id 1 :commutative t))
     (tests (is "3=1 2 3 4 5" #*00100)
            (is "'cat'='hat'" #*011)))
  (≥ (has :title "Greater or Equal")
     (dyadic (scalar-function (boolean-op (compare-by '>= comparison-tolerance))
                              :binary-output t))
     (meta (primary :implicit-args (comparison-tolerance))
           (dyadic :id 1))
     (tests (is "3≥1 2 3 4 5" #*11100)
            (is "1.0≥1.0" 1)
            (is "{⎕CT←0.0001 ⋄ (⍵≥1.0000000001),(⍵≥⍨1.0000000001),⍵≥1.01} 1.0" #*110)))
  (> (has :title "Greater")
     (dyadic (scalar-function (boolean-op (compare-by '> comparison-tolerance))
                              :binary-output t))
     (meta (primary :implicit-args (comparison-tolerance))
           (dyadic :id 0))
     (tests (is "3>1 2 3 4 5" #*11000)))
  (≠ (has :titles ("Unique Mask" "Not Equal"))
     (ambivalent (λω (make-instance 'vader-umask :base omega))
                 (scalar-function (boolean-op (λωα (not (funcall (scalar-compare comparison-tolerance)
                                                                 omega alpha))))
                                  :binary-output t))
     (meta (primary :implicit-args (comparison-tolerance))
           (dyadic :id 0 :commutative t))
     (tests (is "≠2 4 7 4 6 8 3 5 2 4 2 5 6 7" #*11101111000000)
            (is "≠'ONE' 'TWO' 'ONE' 'THREE' 'TWO' 'THREE'" #*110100)
            (IS "≠↑'ONE' 'TWO' 'ONE' 'THREE' 'TWO' 'THREE'" #*110100)
            (is "3≠1 2 3 4 5" #*11011)
            (is "'Harrison'≠'Bergeron'" #*11011100)))
  (∧ (has :title "And" :aliases (^))
     (dyadic (scalar-function (apl-lcm comparison-tolerance)))
     (meta (primary :implicit-args (comparison-tolerance))
           (dyadic :id 1 :commutative t))
     (tests (is "0 1 0 1∧0 0 1 1" #*0001)
            (is "3.2∧1.9" 60.8d0)
            (is "6.3∧5.1" 107.1d0)
            (is "⌈1000×1.3J2.6∧4.5J8.9" #C(-172900 232700))
            (is "⌈1000×3.8J7.6∧5.2J6.8" #C(-159600 326800))))
  (⍲ (has :title "Nand")
     (dyadic (scalar-function (boolean-op (λωα (not (= omega alpha 1))))))
     (meta (dyadic :commutative t))
     (tests (is "0 1 0 1⍲0 0 1 1" #*1110)))
  (∨ (has :title "Or")
     (dyadic (scalar-function (apl-gcd comparison-tolerance)))
     (meta (primary :implicit-args (comparison-tolerance))
           (dyadic :id 0 :commutative t))
     (tests (is "0 1 0 1∨0 0 1 1" #*0111)
            (is "3.2∨1.9" 0.1d0)
            (is "6.3∨5.1" 0.3d0)
            (is "1.3J2.6∨4.5J8.9" 0.1d0)
            (is "3.8J7.6∨5.2J6.8" 0.2d0)))
  (⍱ (has :title "Nor")
     (dyadic (scalar-function (boolean-op (λωα (= omega alpha 0)))))
     (meta (dyadic :commutative t))
     (tests (is "0 1 0 1⍱0 0 1 1" #*1000))))

 (functions
  (with (:name :lexical-functions-array)
        (:tests-profile :title "Array Function Tests")
        (:demo-profile :title "Array Function Demos"
                       :description "These functions affect entire arrays, changing their structure or deriving data from them in some way."))
  (⍳ (has :titles ("Interval" "Index Of"))
     (ambivalent (λω (count-to omega index-origin))
                 (λωα (make-instance 'vader-index :base omega :argument alpha :index-origin index-origin)))
     (meta (primary :implicit-args (index-origin))
           (monadic :inverse (λω (inverse-count-to omega index-origin))))
     (tests (is "⍳5" #(1 2 3 4 5))
            (is "⍳0" #())
            (is "⍳⍴⍳5" #(1 2 3 4 5))
            (is "⍳2 3" #2A((#*11 #(1 2) #(1 3)) (#(2 1) #(2 2) #(2 3))))
            (is "⍳4 3" #2A((#*11 #(1 2) #(1 3)) (#(2 1) #(2 2) #(2 3))
                           (#(3 1) #(3 2) #(3 3)) (#(4 1) #(4 2) #(4 3))))
            (is "⍳2 4 3" #3A(((#*111 #(1 1 2) #(1 1 3)) (#(1 2 1) #(1 2 2) #(1 2 3))
                              (#(1 3 1) #(1 3 2) #(1 3 3)) (#(1 4 1) #(1 4 2) #(1 4 3)))
                             ((#(2 1 1) #(2 1 2) #(2 1 3)) (#(2 2 1) #(2 2 2) #(2 2 3))
                              (#(2 3 1) #(2 3 2) #(2 3 3)) (#(2 4 1) #(2 4 2) #(2 4 3)))))
            (is "2×1-⍨⍳4" #(0 2 4 6))
            (is "((,2)⍳3),2 3⍳4" #(2 3))
            (is "(,3)⍳⍳4" #(2 2 1 2))
            (is "2 4⍳⍳5" #(3 1 3 2 3))
            (is "'aabc'⍳'b'" 3)
            (is "'THIS' 'IS' 'A' 'TEST'⍳'IS' 'IT'" #(2 5))
            (is "'RAT' 'CAT' 'DOG'⍳⊂'DOG'" 3)
            (is "(3 3⍴'CATRATDOG')⍳'RAT'" 2)
            (is "(3 3⍴'CATRATDOG')⍳4 3⍴'RATDOGPIG'" #(2 3 4 2))
            (is "2÷⍨⎕IO-⍨¯10+⍳21"
                #(-5 -9/2 -4 -7/2 -3 -5/2 -2 -3/2 -1 -1/2 0 1/2 1 3/2 2 5/2 3 7/2 4 9/2 5))))
  (⍴ (has :titles ("Shape" "Reshape"))
     (ambivalent (λω (make-instance 'vader-shape :base omega))
                 (λωα (make-instance 'vader-reshape :base omega :argument alpha)))
     (tests (is "⍴1" #())
            (is "⍴1 2 3" #(3))
            (is "⍴3 5⍴1" #(3 5))
            (is "⍴⍴3 4⍴2" #(2))
            (is "⍴⍴⍴4 5 6 7⍴3" #(1))
            (is "⍴⍬" #(0))
            (is "3⍴2" #(2 2 2))
            (is "3⍴3" #(3 3 3))
            (is "4 5⍴⍳3" #2A((1 2 3 1 2) (3 1 2 3 1) (2 3 1 2 3) (1 2 3 1 2)))
            (is "⍬⍴5" 5)
            (is "⍬⍴5 6 7" 5)
            (is "⍬⍴(4 5) 6" #0A#(4 5))
            (is "3⍴0⍴⊂2 2⍴5" #(#2A((0 0) (0 0)) #2A((0 0) (0 0)) #2A((0 0) (0 0))))
            (is "2 2⍴0⍴3⍴⊂2 3⍴5" #2A((#2A((0 0 0)(0 0 0)) #2A((0 0 0)(0 0 0)))
                                     (#2A((0 0 0)(0 0 0)) #2A((0 0 0)(0 0 0)))))
            (is "(,0)⍴0 0⍴0" #())))
  (⌷ (has :title "Index")
     (dyadic (at-index index-origin axes))
     (meta (primary :axes axes :implicit-args (index-origin))
           (dyadic :selective-assignment-compatible t :selective-assignment-function :index))
     (tests (is "1⌷3" 3)
            (is "3⌷2 4 6 8 10" 6)
            (is "3⌷⍳9" 3)
            (is "2 2⌷4 5⍴⍳9" 7)
            (is "2 3 4⌷4 5 6⍴⍳9" 1)
            (is "1 3⌷2 3 4⍴⍳5" #(4 5 1 2))
            (is "1 3⌷[1 3]2 3 4⍴⍳5" #(3 2 1))
            (is "1⌷[2]3 3⍴⍳9" #(1 4 7))
            (is "(⊂4 5 2 6 3 7 1)⌷'MARANGA'" "ANAGRAM")
            (is "(⍬,5) 1⌷5 5⍴⍳25" #(21))
            (is "(5 4) 1⌷5 5⍴⍳25" #(21 16))))
  (≡ (has :titles ("Depth" "Match"))
     (ambivalent (λω (make-instance 'vader-depth :base omega))
                 (λωα (make-instance 'vader-compare :base (vector omega alpha)
                                                    :comparison-tolerance comparison-tolerance)))
     (meta (primary :implicit-args (comparison-tolerance)))
     (tests (is "≡1" 0)
            (is "≡⍳3" 1)
            (is "≡(1 2)(3 4)" 2)
            (is "≡1 (2 3) (4 5 (6 7)) 8" -3)
            (IS "≡↓↓2 3⍴⍳6" 3)
            (IS "≡↓↓↓2 3⍴⍳6" 4)
            (is "3≡3" 1)
            (is "4≡2" 0)
            (is "⍬≡⍬" 1)
            (is "''≡''" 1)
            (is "⍬≡''" 0)
            (is "⍬≡1↓'a'" 0)
            (is "('a',⍬)≡1↑'amy'" 1)
            (is "v←1 2 3 ⋄ (⊂v)≡⊂1 2 3" 1)))
  (≢ (has :titles ("First Dimension" "Not Match"))
     (ambivalent (λω (make-instance 'vader-first-dim :base omega))
                 (λωα (make-instance 'vader-compare :base (vector omega alpha) :inverse t
                                                    :comparison-tolerance comparison-tolerance)))
     (meta (primary :implicit-args (comparison-tolerance)))
     (tests (is "≢2" 1)
            (is "≢1 2 3" 3)
            (is "≢2 3 4⍴⍳9" 2)
            (is "5≢5" 0)
            (is "3≢1" 1)))
  (∊ (has :titles ("Enlist" "Membership"))
     (ambivalent (λω (make-instance 'vader-enlist :base omega))
                 (λωα (make-instance 'vader-membership :base alpha :argument omega)))
     (tests (is "∊2" #(2))
            (is "∊2 2 2⍴⍳9" #(1 2 3 4 5 6 7 8))
            (is "∊⊂2 3" #(2 3))
            (is "∊1 2 (⊂3 4) 5 6 (7 8)" #(1 2 3 4 5 6 7 8))
            (is "∊1⍴⊂⍬,1" #(1))
            (is "∊'a'" "a")
            (is "2 3∊2" #*10)
            (is "3∊3 4 5" 1)
            (is "2 5 7∊1 2 3 4 5" #*110)
            (is "'IS' 'IT' ∊ 'THIS' 'IS' 'A' 'TEST'" #*10)
            (is "1∊3 3⍴⍳9" 1)
            (is "(1⍴1)∊3 3⍴⍳9" #(1))
            (is "∊(⊂⍬),⊂,3" #(3))
            (is "1 2 3 4 8∊⍨3 3⍴⍳9" #2A((1 1 1) (1 0 0) (0 1 0)))))
  (⍷ (has :title "Find")
     (dyadic (λωα (make-instance 'vader-find :base omega :argument alpha)))
     (tests (is "5⍷5" 1)
            (is "2⍷3 4⍴⍳9" #2A((0 1 0 0) (0 0 0 0) (0 0 1 0)))
            (is "(2 2⍴6 7 1 2)⍷2 3 4⍴⍳9" #3A(((0 0 0 0) (0 1 0 0) (0 0 0 0))
                                             ((0 0 1 0) (0 0 0 0) (0 0 0 0))))))
  (⍸ (has :titles ("Where" "Interval Index"))
     (ambivalent (λω (make-instance 'vader-where :base omega :index-origin index-origin))
                 (λωα (make-instance 'vader-interval-index
                                     :base omega :argument alpha :index-origin index-origin)))
     (meta (primary :implicit-args (index-origin))
           (monadic :inverse (λω (make-instance 'vader-inverse-where
                                                :base omega :index-origin index-origin))))
     (tests (is "⍸1" #(#()))
            (is "⍸0" #())
            (is "⍸0 0 1 0 1 0 0 1 1 0" #(3 5 8 9))
            (is "⍸3=2 3 4⍴⍳9" #(#(1 1 3) #(1 3 4) #(2 3 1)))
            (is "⍸(2 3 4⍴⍳9)∊3 5" #(#(1 1 3) #(1 2 1) #(1 3 4) #(2 1 2) #(2 3 1) #(2 3 3)))
            (is "2 4 6 8⍸3" 1)
            (is "10 20 30 40⍸5 12 19 24 35 42 51" #(0 1 1 2 3 4 4))
            (is "(2 5⍴'RADIUS')⍸3 4 5⍴'BOXCAR'" #2A((0 1 0 0) (2 0 0 1) (0 0 2 0)))
            (is "(2 3 5⍴'ABCDEFHIJKLM')⍸3 3 5⍴'BOREAL'" #(1 2 1))))
  (\, (has :titles ("Ravel" "Catenate or Laminate"))
      (ambivalent (λω (make-instance 'vader-pare :base omega :index-origin index-origin
                                                 :axis (first axes)))
                  (λωα (make-instance
                        'vader-catenate :base (if (eq omega :arg-vector)
                                                  alpha (vector alpha omega))
                                        :index-origin index-origin
                                        :axis (or (first axes) :last))))
      (meta (primary :axes axes :implicit-args (index-origin))
            (dyadic :on-axis :last :id #()))
      (tests (is ",5" #(5))
             (is ",3 4⍴⍳9" #(1 2 3 4 5 6 7 8 9 1 2 3))
             (is ",↓⍬,9" #(#(9)))
             (is ",[1]3 3⍴⍳9" #2A((1 2 3) (4 5 6) (7 8 9)))
             (is ",[⍬,1]3 3⍴⍳9" #2A((1 2 3) (4 5 6) (7 8 9)))
             (is ",[0.5]3 4⍴⍳9" #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3))))
             (is ",[1.5]3 4⍴⍳9" #3A(((1 2 3 4)) ((5 6 7 8)) ((9 1 2 3))))
             (is ",[2.5]3 4⍴⍳9" #3A(((1) (2) (3) (4)) ((5) (6) (7) (8)) ((9) (1) (2) (3))))
             (is ",[1 2]2 3 3⍴⍳12" #2A((1 2 3) (4 5 6) (7 8 9) (10 11 12) (1 2 3) (4 5 6)))
             (is ",[2 3]2 3 3⍴⍳12" #2A((1 2 3 4 5 6 7 8 9) (10 11 12 1 2 3 4 5 6)))
             (is ",[1 2 3]2 3 3⍴⍳12" #(1 2 3 4 5 6 7 8 9 10 11 12 1 2 3 4 5 6))
             (is "⊃,[1]/(⊂3 3)⍴¨⍳5" #2A((1 1 1) (1 1 1) (1 1 1) (2 2 2) (2 2 2) (2 2 2)
                                        (3 3 3) (3 3 3) (3 3 3) (4 4 4) (4 4 4) (4 4 4)
                                        (5 5 5) (5 5 5) (5 5 5)))
             (is "⊃,[2]/(⊂3 3)⍴¨⍳5" #2A((1 1 1 2 2 2 3 3 3 4 4 4 5 5 5)
                                        (1 1 1 2 2 2 3 3 3 4 4 4 5 5 5)
                                        (1 1 1 2 2 2 3 3 3 4 4 4 5 5 5)))
             (is ",[⍬]5" #(5))
             (is ",[⍬]⍳5" #2A((1) (2) (3) (4) (5)))
             (is "5 6,3" #(5 6 3))
             (is "2,⍳3" #(2 1 2 3))
             (is "0,3 4⍴⍳9" #2A((0 1 2 3 4) (0 5 6 7 8) (0 9 1 2 3)))
             (is "⍬,⍳5" #(1 2 3 4 5))
             (is "⍬,3" #(3))
             (is "'a',⍬" "a")
             (is "↓(2 2⍴'a'),'*'" #("aa*" "aa*"))
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
             (is "'UNDER',[1.0]'-'" "UNDER-")
             (is "↓'UNDER',[1r2]'-'" #("UNDER"
                                       "-----"))
             (is "↓'HELLO',[1.5]'.'" #("H." "E." "L." "L." "O."))
             (is "(8+2 2 2⍴⍳8),[1.5]2 2 2⍴⍳8" #4A((((9 10) (11 12)) ((1 2) (3 4)))
                                                  (((13 14) (15 16)) ((5 6) (7 8)))))
             (is "(8+2 2 2⍴⍳8),[5r2]2 2 2⍴⍳8" #4A((((9 10) (1 2)) ((11 12) (3 4)))
                                                  (((13 14) (5 6)) ((15 16) (7 8)))))
             (is "(8+2 2 2⍴⍳8),[0.5]2 2 2⍴⍳8" #4A((((9 10) (11 12)) ((13 14) (15 16)))
                                                  (((1 2) (3 4)) ((5 6) (7 8)))))))
  (⍪ (has :titles ("Table" "Catenate First"))
     (ambivalent (λω (make-instance 'vader-pare :base omega :index-origin index-origin
                                                :axis :tabulate))
                 (λωα (make-instance
                       'vader-catenate :base (if (eq omega :arg-vector)
                                                 alpha (vector alpha omega))
                                       :index-origin index-origin
                                       :axis (or (first axes) index-origin))))
     (meta (primary :axes axes :implicit-args (index-origin))
           (dyadic :on-axis :first))
     (tests (is "⍪4" #2A((4)))
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
     (ambivalent (λω (make-instance 'vader-mix :base omega :index-origin index-origin
                                               :axis (or (first axes) :last)))
                 (λωα (make-instance
                       'vader-section :base omega :argument alpha
                                      :index-origin index-origin :axis (or (first axes) :last))))
     (meta (primary :axes axes :implicit-args (index-origin))
           (monadic :inverse (λω (make-instance 'vader-split :base omega :index-origin index-origin
                                                             :axis (or (first axes) :last))))
           (dyadic :on-axis :last :selective-assignment-compatible t
                   :selective-assignment-function t))
     (tests (is "↑2" 2)
            (is "↑'a'" #\a)
            (is "↑⍬" #())
            (is "⍴1↑⍳3" #*1)
            (is "↑⊂2 4" #(2 4))
            (is "↑⊂⊂⍳5" #0A#(1 2 3 4 5))
            (is "↑(1)(1 2)(1 2 3)" #2A((1 0 0) (1 2 0) (1 2 3)))
            (is "↑[0.5](1)(1 2)(1 2 3)" #2A((1 1 1) (0 2 2) (0 0 3)))
            (is "↑(2 3⍴⍳5)(4 2⍴⍳8)" #3A(((1 2 3) (4 5 1) (0 0 0) (0 0 0))
                                        ((1 2 0) (3 4 0) (5 6 0) (7 8 0))))
            (is "↑(2 5⍴⍳9)(3 2 1)(4 3⍴⍳8)" #3A(((1 2 3 4 5) (6 7 8 9 1) (0 0 0 0 0) (0 0 0 0 0))
                                               ((3 2 1 0 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0))
                                               ((1 2 3 0 0) (4 5 6 0 0) (7 8 1 0 0) (2 3 4 0 0))))
            (is "↑[0.5](2 3⍴⍳5)(4 2⍴⍳8)" #3A(((1 1) (2 2) (3 0)) ((4 3) (5 4) (1 0))
                                             ((0 5) (0 6) (0 0)) ((0 7) (0 8) (0 0))))
            (is "↑[1.5](2 3⍴⍳5)(4 2⍴⍳8)" #3A(((1 2 3) (4 5 1) (0 0 0) (0 0 0))
                                             ((1 2 0) (3 4 0) (5 6 0) (7 8 0))))
            (is "↑[0.5]2 3⍴(2 5⍴⍳9)(4 3 2 1)(4 3⍴⍳8)"
                #4A((((1 4 1) (1 4 1)) ((2 3 2) (2 3 2)) ((3 2 3) (3 2 3)) ((4 1 0) (4 1 0)) ((5 0 0) (5 0 0)))
                    (((6 0 4) (6 0 4)) ((7 0 5) (7 0 5)) ((8 0 6) (8 0 6)) ((9 0 0) (9 0 0)) ((1 0 0) (1 0 0)))
                    (((0 0 7) (0 0 7)) ((0 0 8) (0 0 8)) ((0 0 1) (0 0 1)) ((0 0 0) (0 0 0)) ((0 0 0) (0 0 0)))
                    (((0 0 2) (0 0 2)) ((0 0 3) (0 0 3)) ((0 0 4) (0 0 4)) ((0 0 0) (0 0 0)) ((0 0 0) (0 0 0)))))
            (is "↑[1.5]2 3⍴(2 5⍴⍳9)(4 3 2 1)(4 3⍴⍳8)"
                #4A((((1 4 1) (2 3 2) (3 2 3) (4 1 0) (5 0 0)) ((6 0 4) (7 0 5) (8 0 6) (9 0 0) (1 0 0))
                     ((0 0 7) (0 0 8) (0 0 1) (0 0 0) (0 0 0)) ((0 0 2) (0 0 3) (0 0 4) (0 0 0) (0 0 0)))
                    (((1 4 1) (2 3 2) (3 2 3) (4 1 0) (5 0 0)) ((6 0 4) (7 0 5) (8 0 6) (9 0 0) (1 0 0))
                     ((0 0 7) (0 0 8) (0 0 1) (0 0 0) (0 0 0)) ((0 0 2) (0 0 3) (0 0 4) (0 0 0) (0 0 0)))))
            (is "↑[2.5]2 3⍴(2 5⍴⍳9)(4 3 2 1)(4 3⍴⍳8)"
                #4A((((1 2 3 4 5) (6 7 8 9 1) (0 0 0 0 0) (0 0 0 0 0))
                     ((4 3 2 1 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0))
                     ((1 2 3 0 0) (4 5 6 0 0) (7 8 1 0 0) (2 3 4 0 0)))
                    (((1 2 3 4 5) (6 7 8 9 1) (0 0 0 0 0) (0 0 0 0 0))
                     ((4 3 2 1 0) (0 0 0 0 0) (0 0 0 0 0) (0 0 0 0 0))
                     ((1 2 3 0 0) (4 5 6 0 0) (7 8 1 0 0) (2 3 4 0 0)))))
            (is "↑2 2 2⍴(1)(1 2)(3 4)(1 2 3)" #4A((((1 0 0) (1 2 0)) ((3 4 0) (1 2 3)))
                                                  (((1 0 0) (1 2 0)) ((3 4 0) (1 2 3)))))
            (is "2↑2" #(2 0))
            (is " 2  2↑5" #2A((5 0) (0 0)))
            (is " 2 ¯2↑5" #2A((0 5) (0 0)))
            (is " 3 ¯2↑5" #2A((0 5) (0 0) (0 0)))
            (is "¯3  2↑5" #2A((0 0) (0 0) (5 0)))
            (is " 2 ¯5↑5" #2A((0 0 0 0 5) (0 0 0 0 0)))
            (is " 6  6↑¯3 2↑5" #2A((0 0 0 0 0 0) (0 0 0 0 0 0) (5 0 0 0 0 0)
                                   (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)))
            (is " 6 ¯6↑¯3 2↑5" #2A((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 5 0)
                                   (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)))
            (is "¯6  6↑¯3 2↑5" #2A((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
                                   (0 0 0 0 0 0) (0 0 0 0 0 0) (5 0 0 0 0 0)))
            (is "¯6 ¯6↑¯3 2↑5" #2A((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)
                                   (0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 0 0 5 0)))
            (is "3↑⍳9" #(1 2 3))
            (is "¯1↑⍳5" #(5))
            (is "3↑'abcdef'" "abc")
            (is "8↑'a',1 2 3" #(#\a 1 2 3 #\  #\  #\  #\ ))
            (is "8↑1 2,'ab',3 4" #(1 2 #\a #\b 3 4 0 0))
            (is "3↑''" "   ")
            (is "3↑⍬" #(0 0 0))
            (is "0↑⍬" #())
            (is "0 0↑⍬" #2A())
            (is "3↑⊂3 3⍴5" #(#2A((5 5 5) (5 5 5) (5 5 5)) #2A((0 0 0) (0 0 0) (0 0 0))
                             #2A((0 0 0) (0 0 0) (0 0 0))))
            (is "¯5↑⊂1 2" #(#(0 0) #(0 0) #(0 0) #(0 0) #(1 2)))
            (is "3↑0↑3⍴⊂3 3⍴5" #(#2A((0 0 0) (0 0 0) (0 0 0)) #2A((0 0 0) (0 0 0) (0 0 0))
                                 #2A((0 0 0) (0 0 0) (0 0 0))))
            (is "2 3 4↑4 5 6⍴⍳9" #3A(((1 2 3 4) (7 8 9 1) (4 5 6 7))
                                     ((4 5 6 7) (1 2 3 4) (7 8 9 1))))
            (is "2 ¯2 ¯2↑4 5 6⍴⍳9" #3A(((5 6) (2 3)) ((8 9) (5 6))))
            (is "5 ¯5↑(3 3⍴⍳9)∊1 2 3 4 8" #2A((0 0 1 1 1) (0 0 1 0 0) (0 0 0 1 0) (0 0 0 0 0) (0 0 0 0 0)))
            (is "2 ¯2↑2 2⍴⍳4" #2A((1 2) (3 4)))
            (is "2 ¯3↑3 4⍴⍳12" #2A((2 3 4) (6 7 8)))
            (is "2 ¯5↑3 4⍴⍳12" #2A((0 1 2 3 4) (0 5 6 7 8)))
            (is "1↑3 4⍴⍳12" #2A((1 2 3 4)))
            (is "1↓3 4⍴⍳12" #2A((5 6 7 8) (9 10 11 12)))
            (is "1↑[1]2 3 4⍴⍳9" #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3))))
            (is "1↑[2]2 3 4⍴⍳9" #3A(((1 2 3 4)) ((4 5 6 7))))
            (is "2↑[2]2 3 4⍴⍳9" #3A(((1 2 3 4) (5 6 7 8)) ((4 5 6 7) (8 9 1 2))))
            (is "2↑[3]2 3 4⍴⍳9" #3A(((1 2) (5 6) (9 1)) ((4 5) (8 9) (3 4))))
            (is "2 2↑[2 3]3 4 5⍴⍳9" #3A(((1 2) (6 7)) ((3 4) (8 9)) ((5 6) (1 2))))
            (is "0↑4 5 6" #())
            (is "0↑'a' 5 6" "")
            (is "4↑(3 4⍴⍳12) 8 9" #(#2A((1 2 3 4) (5 6 7 8) (9 10 11 12)) 8 9
                                    #2A((0 0 0 0) (0 0 0 0) (0 0 0 0))))
            (is "6↑(⊂3 4⍴⍳9) 1 2" #(#0A#2A((1 2 3 4) (5 6 7 8) (9 1 2 3)) 1 2
                                    #0A#2A((0 0 0 0) (0 0 0 0) (0 0 0 0))
                                    #0A#2A((0 0 0 0) (0 0 0 0) (0 0 0 0))
                                    #0A#2A((0 0 0 0) (0 0 0 0) (0 0 0 0))))
            (is "⍴0↑3 4 5⍴5" #(0 4 5))
            (is "⍴0 0↑3 4 5⍴5" #(0 0 5))
            (is "3↑0↑⊂2 3⍴5" #(#2A((0 0 0) (0 0 0)) #2A((0 0 0) (0 0 0)) #2A((0 0 0) (0 0 0))))
            (is "4↑0↑⊂2 2⍴(⊂2 2⍴⍳4) 2 3" #(#2A((#0A#2A((0 0) (0 0)) 0) (0 #0A#2A((0 0) (0 0))))
                                           #2A((#0A#2A((0 0) (0 0)) 0) (0 #0A#2A((0 0) (0 0))))
                                           #2A((#0A#2A((0 0) (0 0)) 0) (0 #0A#2A((0 0) (0 0))))
                                           #2A((#0A#2A((0 0) (0 0)) 0) (0 #0A#2A((0 0) (0 0))))))
            (is "2↑⍬ ⍬ ⍬ ⍬ ⍬" #(#() #()))
            (is "8↑3⍴⊂0 0 0⍴1" #(#3A() #3A() #3A() #3A() #3A() #3A() #3A() #3A()))
            (is "1↑2 3 4⍴⍳9" #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3))))
            (is "1 2↑2 3 4⍴⍳9" #3A(((1 2 3 4) (5 6 7 8))))
            (is "2 2 2↑1 0 2⍴⍳30" #3A(((0 0) (0 0)) ((0 0) (0 0))))
            (is "5 5↑3 3⍴1" #2A((1 1 1 0 0) (1 1 1 0 0) (1 1 1 0 0) (0 0 0 0 0) (0 0 0 0 0)))
            (is "(¯5↑1),(,¯5)↑1" #*0000100001)
            (is "4 3↑1 1↓4 5⍴⍳20" #2A((7 8 9) (12 13 14) (17 18 19) (0 0 0)))
            (is "2 2↑[2 3]3 4 5↑4 5 6⍴⍳9" #3A(((1 2) (7 8)) ((4 5) (1 2)) ((7 8) (4 5))))
            (is "¯2 ¯2↑[2 3]3 4 5↑4 5 6⍴⍳9" #3A(((7 8) (4 5)) ((1 2) (7 8)) ((4 5) (1 2))))
            (is "¯2 ¯2↑[2 3]3 4 8↑3 4 5↑4 5 6⍴⍳9" #3A(((0 0) (0 0)) ((0 0) (0 0)) ((0 0) (0 0))))
            (is "¯11↑[3]1 4 8↑3 4 5↑4 5 6⍴⍳9"
                #3A(((0 0 0 1 2 3 4 5 0 0 0) (0 0 0 7 8 9 1 2 0 0 0) (0 0 0 4 5 6 7 8 0 0 0)
                                             (0 0 0 1 2 3 4 5 0 0 0))))
            (is "2↑[3]¯11↑[3]1 4 8↑3 4 5↑4 5 6⍴⍳9" #3A(((0 0) (0 0) (0 0) (0 0))))
            (is "2 3↑¯2 ¯2↑1 1⍴5" #2A((0 0 0) (0 5 0)))
            (is "2 3↑¯2 ¯4↑1 1⍴5" #2A((0 0 0) (0 0 0)))))
  (↓ (has :titles ("Split" "Drop"))
     (ambivalent (λω (make-instance 'vader-split :base omega :index-origin index-origin
                                                 :axis (or (first axes) :last)))
                 (λωα (make-instance
                       'vader-section :base omega :argument alpha :index-origin index-origin
                                      :inverse t :axis (or (first axes) :last))))
     (meta (primary :axes axes :implicit-args (index-origin))
           (monadic :on-axis :last
                    :inverse (λωχ (make-instance 'vader-mix :base omega :index-origin index-origin
                                                            :axis (or (first axes) :last))))
           (dyadic :on-axis :last :selective-assignment-compatible t :selective-assignment-function t))
     (tests (is "↓5" 5)
            (is "↓'b'" #\b)
            (is "↓⍳5" #0A#(1 2 3 4 5))
            (is "↓3 4⍴⍳9" #(#(1 2 3 4) #(5 6 7 8) #(9 1 2 3)))
            (is "↓[1]3 4⍴⍳9" #(#(1 5 9) #(2 6 1) #(3 7 2) #(4 8 3)))
            (is "↓[3]3 4 5⍴⍳9" #2A((#(1 2 3 4 5) #(6 7 8 9 1) #(2 3 4 5 6) #(7 8 9 1 2))
                                   (#(3 4 5 6 7) #(8 9 1 2 3) #(4 5 6 7 8) #(9 1 2 3 4))
                                   (#(5 6 7 8 9) #(1 2 3 4 5) #(6 7 8 9 1) #(2 3 4 5 6))))
            (is "↓2 2⍴⍳4" #(#(1 2) #(3 4)))
            (is "↓↓2 2⍴⍳4" #0A#(#(1 2) #(3 4)))
            (is "↓↓↓2 2⍴⍳4" #0A#0A#(#(1 2) #(3 4)))
            (is "1↓2" #())
            (is "2↓3" #())
            (is "2↓⍳9" #(3 4 5 6 7 8 9))
            (is "4↓⍳9" #(5 6 7 8 9))
            (is "3↓'abcdef'" "def")
            (is "1↓'a'" "")
            (is "2↓'ab'" "")
            (is "2 2 2↓4 5 6⍴⍳9" #3A(((3 4 5 6) (9 1 2 3) (6 7 8 9))
                                     ((6 7 8 9) (3 4 5 6) (9 1 2 3))))
            (is "1↓[1]2 3 4⍴⍳9" #3A(((4 5 6 7) (8 9 1 2) (3 4 5 6))))
            (is "1↓[2]2 3 4⍴⍳9" #3A(((5 6 7 8) (9 1 2 3)) ((8 9 1 2) (3 4 5 6))))
            (is "2↓[2]2 3 4⍴⍳9" #3A(((9 1 2 3)) ((3 4 5 6))))
            (is "2↓[3]2 3 4⍴⍳9" #3A(((3 4) (7 8) (2 3)) ((6 7) (1 2) (5 6))))
            (is "2 2↓[2 3]3 4 5⍴⍳9" #3A(((4 5 6) (9 1 2)) ((6 7 8) (2 3 4)) ((8 9 1) (4 5 6))))
            (is "¯2↓⍳9" #(1 2 3 4 5 6 7))
            (is "¯2 ¯2↓5 8⍴⍳9" #2A((1 2 3 4 5 6) (9 1 2 3 4 5) (8 9 1 2 3 4)))
            (is "4 5↓2 3⍴1" #2A())
            (is "1↓2 3 4⍴⍳9" #3A(((4 5 6 7) (8 9 1 2) (3 4 5 6))))
            (is "1 1↓2 3 4⍴⍳9" #3A(((8 9 1 2) (3 4 5 6))))
            (is "1↓¯1↓' abcdefg'" "abcdef")
            (is "2 2↓[2 3]3 4 8↑3 4 5↑4 5 6⍴⍳9" #3A(((6 7 8 0 0 0) (3 4 5 0 0 0))
                                                    ((9 1 2 0 0 0) (6 7 8 0 0 0))
                                                    ((3 4 5 0 0 0) (9 1 2 0 0 0))))
            (is "¯2 ¯2↓[2 3]3 4 8↑3 4 5↑4 5 6⍴⍳9" #3A(((1 2 3 4 5 0) (7 8 9 1 2 0))
                                                      ((4 5 6 7 8 0) (1 2 3 4 5 0))
                                                      ((7 8 9 1 2 0) (4 5 6 7 8 0))))))
  (⊂ (has :titles ("Enclose" "Partitioned Enclose"))
     (ambivalent (λω (make-instance 'vader-enclose :base omega :index-origin index-origin
                                                   :axis (first axes)))
                 (λωα (make-instance 'vader-enclose :argument alpha :index-origin index-origin
                                                    :base omega :axis (first axes))))
     (meta (primary :axes axes :implicit-args (index-origin)))
     (tests (is "⊂2" 2)
            (is "(⊂2)=2" 1)
            (is "(⊂'a')='a'" 1)
            (is "⊂⍳5" #0A#(1 2 3 4 5))
            (is "1+⊂⍳5" #0A#(2 3 4 5 6))
            (is "⊂'abc'" #0A"abc")
            (is "≡⊂5 5" 2)
            (is "≡⊂⊂5 5" 3)
            (is "≡⊂⊂⊂5 5" 4)
            (is "1,⊂3 4⍴⍳7" #(1 #2A((1 2 3 4) (5 6 7 1) (2 3 4 5))))
            (is "⊂[⍬]⍳3" #(1 2 3))
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
            (is "{x←⊂[2] ⋄ x ⍵} 2 3 4⍴⍳9" #2A((#(1 5 9) #(2 6 1) #(3 7 2) #(4 8 3))
                                              (#(4 8 3) #(5 9 4) #(6 1 5) #(7 2 6))))
            (is "1⊂2" #(#(2)))
            (is "1⊂5" #(#(5)))
            (is "1⊂⍳5" #(#(1) #(2) #(3) #(4) #(5)))
            (is "2⊂⍳5" #(#() #(1) #() #(2) #() #(3) #() #(4) #() #(5)))
            (is "0 1 0 0 1 1 0 0 0⊂⍳9" #(#(2 3 4) #(5) #(6 7 8 9)))
            (is "0 1 0 0 1 1 0 0⊂4 8⍴⍳9"
                #(#2A((2 3 4) (1 2 3) (9 1 2) (8 9 1)) #2A((5) (4) (3) (2))
                  #2A((6 7 8) (5 6 7) (4 5 6) (3 4 5))))
            (is "0 1 0 1⊂[1]4 8⍴⍳9"
                #(#2A((9 1 2 3 4 5 6 7) (8 9 1 2 3 4 5 6)) #2A((7 8 9 1 2 3 4 5))))
            (is "2 0 1 3 0 2 0 1⊂'abcdefg'" #(#() "ab" "c" #() #() "de" #() "fg" #()))
            (is "0 0 2 0 1⊂'abcdefg'" #(#() "cd" "efg"))
            (is "3⍴0 0 0 0⊂⍳3" #(#() #() #()))
            (is "(0/⍳3 3)≡⊃0 0 0⊂⍳3 3" 1)
            (is "⊃0 0 0⊂⍳3 3" #2A(() () ()))
            (is "⊃⊃0 0 0⊂⍳3 3" #*00)))
  (⊆ (has :titles ("Nest" "Partition"))
     (ambivalent (λω (make-instance 'vader-partition :index-origin index-origin :base omega))
                 (λωα (make-instance 'vader-partition
                                     :argument alpha :index-origin index-origin :base omega
                                     :axis (or (first axes) :last))))
     (meta (primary :axes axes :implicit-args (index-origin))
           (monadic :inverse #'identity)
           (dyadic :on-axis :last))
     (tests (is " ⊆ ⍳3" #0A#(1 2 3))
            (is " ⊆⊂⍳3" #0A#(1 2 3))
            (is "⊃⊆⊂⍳3"    #(1 2 3))
            (is "⊆1 2 (1 2 3)" #(1 2 #(1 2 3)))
            (is "⊆ 'hello'" #0A"hello")
            (is "⊆⊂'hello'" #0A"hello")
            (is "⊆'hello' 'how' 'are' 'you'" #("hello" "how" "are" "you"))
            (is "2⊆⍳3" #(#(1 2 3)))
            (is "1 1 0⊆5 6 0" #(#(5 6)))
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
     (ambivalent (λω (make-instance 'vader-pick :base omega))
                 (λωα (make-instance 'vader-pick :base omega :argument alpha :index-origin index-origin)))
     (meta (primary :implicit-args (index-origin))
           (monadic :inverse (λωχ (if axes (error "Inverse [⊃ disclose] does not accept axis arguments.")
                                      (make-instance 'vader-enclose :base omega :index-origin index-origin)))
                    :selective-assignment-compatible t)
           (dyadic :selective-assignment-compatible t :selective-assignment-enclosing t
                   :selective-assignment-function :pick))
     (tests (is "⊃3" 3)
            (is "⊃⍳4" 1)
            (is "⊃⊂⍳4" #(1 2 3 4))
            (is "⊃(⊂'test'),3" "test")
            (is "⊃⍬" 0)
            (is "' '=⊃''" 1)
            (is "⊃¨⍴¨'one' 'a' 'two' 'three'" #(3 0 3 5))
            (is "⊃⊂¨3⍴⊂⍳3" #0A#(1 2 3))
            (is "1⊃5,⍬" 5)
            (is "2⊃2 4 6 8" 4)
            (is "2⊃(1 2 3)(4 5 6)(7 8 9)" #(4 5 6))
            (is "2 2⊃(1 2 3)(4 5 6)(7 8 9)" 5)
            (is "(⊂2 2)⊃3 4⍴⍳12" 6)
            (is "⊃⊃,⊂⊂⍳3" #(1 2 3))
            (is "⊃⊃,/⊂⊂⍳3" #(1 2 3))
            (is "4 (⊂1 3)⊃6⍴⊂3 4⍴⍳12" 3)
            (is "4 (⊂1 3)⊃(5×⍳6)×6⍴⊂3 4⍴⍳12" 60)))
  (∩ (has :title "Intersection")
     (dyadic (λωα (make-instance 'vader-intersection :base (if (eq omega :arg-vector)
                                                               alpha (vector alpha omega)))))
     (tests (is "2∩⍳4" #(2))
            (is "4 5 6∩4" #(4))
            (is "'MIXTURE'∩'LATER'" "TRE")
            (is "'STEEL'∩'SABER'" "SEE")
            (is "1 4 8∩⍳5" #(1 4))
            (is "'abc'∩'cde'" "c")
            (is "'abc'∩'c'" "c")))
  (∪ (has :titles ("Unique" "Union"))
     (ambivalent (λω (make-instance 'vader-unique :base omega))
                 (λωα (make-instance 'vader-union :base (vector alpha omega))))
     (meta (monadic) (dyadic :id #()))
     (tests (is "∪3" #(3))
            (is "∪1 2 3 4 5 1 2 8 9 10 11 7 8 11 12" #(1 2 3 4 5 8 9 10 11 7 12))
            (is "∪'MISSISSIPPI'" "MISP")
            (is "∪2 3 4⍴⍳12" #3A(((1 2 3 4) (5 6 7 8) (9 10 11 12))))
            (is "∪3 3 4⍴⍳24" #3A(((1 2 3 4) (5 6 7 8) (9 10 11 12))
                                 ((13 14 15 16) (17 18 19 20) (21 22 23 24))))
            (is "1∪1" #(1))
            (is "2 3 4∪5" #(2 3 4 5))
            (is "2∪⍳3" #(2 1 3))
            (is "3 10 14 18 11∪9 4 5 10 8 3" #(3 10 14 18 11 9 4 5 8))
            (is "'STEEL'∪'SABER'" "STEELABR")
            (is "'APRIL' 'MAY'∪'MAY' 'JUNE'" #("APRIL" "MAY" "JUNE"))))
  (⌽ (has :titles ("Reverse" "Rotate"))
     (ambivalent (λω (make-instance 'vader-turn :base omega :index-origin index-origin
                                                :axis (or (first axes) :last)))
                 (λωα (make-instance 'vader-turn :base omega :argument alpha :index-origin index-origin
                                                 :axis (or (first axes) :last))))
     (meta (primary :axes axes :implicit-args (index-origin))
           (monadic :on-axis :last :inverse #'identity)
           (dyadic :id 0 :on-axis :last
                   :inverse (λωαχ (make-instance 'vader-turn :base omega :argument alpha
                                                             :inverse t :index-origin index-origin
                                                             :axis (or (first axes) :last)))))
     (tests (is "⌽3" 3)
            (is "⌽1 2 3 4 5" #(5 4 3 2 1))
            (is "⌽3 4⍴⍳9" #2A((4 3 2 1) (8 7 6 5) (3 2 1 9)))
            (is "3⌽1" 1)
            (is "3⌽⍳5" #(4 5 1 2 3))
            (is "2⌽3 4⍴⍳9" #2A((3 4 1 2) (7 8 5 6) (2 3 9 1)))
            (is "(2 2⍴1 2 3 4)⌽2 2 5⍴⍳9" #3A(((2 3 4 5 1) (8 9 1 6 7)) ((5 6 2 3 4) (2 7 8 9 1))))))
  (⊖ (has :titles ("Reverse First" "Rotate First"))
     (ambivalent (λω (make-instance 'vader-turn :base omega :index-origin index-origin
                                                :axis (or (first axes) index-origin)))
                 (λωα (make-instance 'vader-turn :base omega :argument alpha :index-origin index-origin
                                                 :axis (or (first axes) index-origin))))
     (meta (primary :axes axes :implicit-args (index-origin))
           (monadic :on-axis :first :inverse #'identity)
           (dyadic :id 0 :on-axis :first
                   :inverse (λωαχ (make-instance 'vader-turn :base omega :argument alpha
                                                             :inverse t :index-origin index-origin
                                                             :axis (or (first axes) index-origin)))))
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
     (ambivalent (λω (make-instance 'vader-permute :base omega :index-origin index-origin))
                 (λωα (make-instance 'vader-permute :base omega :argument alpha :index-origin index-origin)))
     (meta (primary :implicit-args (index-origin))
           (monadic :inverse (λω (make-instance 'vader-permute
                                                :base omega :index-origin index-origin)))
           (dyadic :inverse (λωα (make-instance 'vader-permute
                                                :base omega :argument alpha :index-origin index-origin))
                   :selective-assignment-compatible t :selective-assignment-function t))
     (tests (is "⍉2" 2)
            (is "⍉2 3 4⍴⍳9" #3A(((1 4) (5 8) (9 3)) ((2 5) (6 9) (1 4))
                                ((3 6) (7 1) (2 5)) ((4 7) (8 2) (3 6))))
            (is "1⍉5" 5)
            (is "1⍉⍳3" #(1 2 3))
            (is "1 3 2⍉2 3 4⍴⍳9" #3A(((1 5 9) (2 6 1) (3 7 2) (4 8 3))
                                     ((4 8 3) (5 9 4) (6 1 5) (7 2 6))))
            (is "1 1⍉5 5⍴⍳25" #(1 7 13 19 25))
            (is "1 1 2⍉2 3 4⍴⍳24" #2A((1 2 3 4) (17 18 19 20)))
            (is "1 2 2⍉2 3 4⍴⍳24" #2A((1 6 11) (13 18 23)))
            (is "2 2 1⍉2 3 4⍴⍳24" #2A((1 17) (2 18) (3 19) (4 20)))
            (is "2 1 1⍉2 3 4⍴⍳24" #2A((1 13) (6 18) (11 23)))
            (is "3 2 2 1⍉2 5 3 4⍴⍳120" #3A(((1 61) (17 77) (33 93)) ((2 62) (18 78) (34 94))
                                           ((3 63) (19 79) (35 95)) ((4 64) (20 80) (36 96))))
            (is "3 2 1 3⍉2 5 3 4⍴⍳120" #3A(((1 62) (13 74) (25 86) (37 98) (49 110))
                                           ((5 66) (17 78) (29 90) (41 102) (53 114))
                                           ((9 70) (21 82) (33 94) (45 106) (57 118))))
            (is "1 2 1 1⍉3 2 7 2⍴⍳84" #2A((1 15) (32 46)))
            (is "3 1 1 2⍉4 3 6 2⍴⍳144" #3A(((1 37 73 109) (2 38 74 110))
                                           ((15 51 87 123) (16 52 88 124))
                                           ((29 65 101 137) (30 66 102 138))))
            (is "1 1 2 2 3 3 3⍉3 2 3 4 2 4 3⍴⍳1728"
                #3A(((1 17) (121 137) (241 257)) ((865 881) (985 1001) (1105 1121))))
            (is "3 1 3 2 2 3 2⍉3 2 3 4 2 4 3⍴⍳1728"
                #3A(((1 676 1351) (38 713 1388)) ((289 964 1639) (326 1001 1676))))
            (is "3 2 2 2 2 2 1⍉3 2 3 4 2 4 3⍴⍳1728"
                #3A(((1 577 1153) (424 1000 1576)) ((2 578 1154) (425 1001 1577))
                    ((3 579 1155) (426 1002 1578))))
            (is "2 1 1 2 3 3 2⍉3 2 3 4 2 4 3⍴⍳1728"
                #3A(((1 16) (602 617) (1203 1218)) ((385 400) (986 1001) (1587 1602))))))
  (/ (has :title "Replicate")
     (dyadic (λωα (make-instance 'vader-expand :base omega :argument alpha :index-origin index-origin
                                               :inverse t :axis (or (first axes) :last))))
     (meta (primary :axes axes :implicit-args (index-origin))
           (dyadic :on-axis :last
                   :inverse (λωαχ (if (is-unitary omega)
                                      ;; TODO: this inverse functionality is probably not complete
                                      (make-instance 'vader-expand
                                                     :base omega :argument alpha
                                                     :index-origin index-origin :inverse t
                                                     :axis (or (first axes) :last))
                                      (error "Inverse [/ replicate] can only accept~a"
                                             " a scalar right argument.")))
                   :selective-assignment-compatible t :selective-assignment-function t))
     (tests (is "3/1" #*111)
            (is "(1⍴2)/8" #(8 8))
            (is "5/3" #(3 3 3 3 3))
            (is "1 0 1 0 1/⍳5" #(1 3 5))
            (is "⍴0/1 1⍴0" #*10)
            (is "⍴1/1 1⍴1" #*11)
            (is "⍴(,0)/0 1⍴1" #*00)
            (is "3/⍳5" #(1 1 1 2 2 2 3 3 3 4 4 4 5 5 5))
            (is "(1⍴3)/⍳5" #(1 1 1 2 2 2 3 3 3 4 4 4 5 5 5))
            (is "3/⊂⍳5" #(#(1 2 3 4 5) #(1 2 3 4 5) #(1 2 3 4 5)))
            (is "3/3 3⍴⍳9" #2A((1 1 1 2 2 2 3 3 3) (4 4 4 5 5 5 6 6 6) (7 7 7 8 8 8 9 9 9)))
            (is "¯1/4 4⍴⍳16" #2A((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0)))
            (is "''≡0/∊'a'" 1)
            (is "1 ¯2 3 ¯4 5/3 5⍴⍳5" #2A((1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)
                                         (1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)
                                         (1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)))
            (is "2/[2]3 4 5⍴⍳9" #3A(((1 2 3 4 5) (1 2 3 4 5) (6 7 8 9 1) (6 7 8 9 1)
                                     (2 3 4 5 6) (2 3 4 5 6) (7 8 9 1 2) (7 8 9 1 2))
                                    ((3 4 5 6 7) (3 4 5 6 7) (8 9 1 2 3) (8 9 1 2 3)
                                     (4 5 6 7 8) (4 5 6 7 8) (9 1 2 3 4) (9 1 2 3 4))
                                    ((5 6 7 8 9) (5 6 7 8 9) (1 2 3 4 5) (1 2 3 4 5)
                                     (6 7 8 9 1) (6 7 8 9 1) (2 3 4 5 6) (2 3 4 5 6))))
            (is "2 1 2 3/[2]3 4 5⍴⍳9" #3A(((1 2 3 4 5) (1 2 3 4 5) (6 7 8 9 1) (2 3 4 5 6)
                                           (2 3 4 5 6) (7 8 9 1 2) (7 8 9 1 2) (7 8 9 1 2))
                                          ((3 4 5 6 7) (3 4 5 6 7) (8 9 1 2 3) (4 5 6 7 8)
                                           (4 5 6 7 8) (9 1 2 3 4) (9 1 2 3 4) (9 1 2 3 4))
                                          ((5 6 7 8 9) (5 6 7 8 9) (1 2 3 4 5) (6 7 8 9 1)
                                           (6 7 8 9 1) (2 3 4 5 6) (2 3 4 5 6) (2 3 4 5 6))))
            (is "⍴0 1 0 1/0 4⍴0" #(0 2))
            (is "⍴5/0 4⍴0" #(0 20))
            (is "⍴2 3/[2]0 2 0⍴0" #(0 5 0))
            (is "⍴0/2 3 4⍴⍳9" #(2 3 0))
            (is "+/⊂⊂⍳3" #0A#0A#(1 2 3))
            (is "+/,⊂⊂⍳3" #0A#0A#(1 2 3))
            (is "+/⍬,⊂⊂⍳3" #0A#0A#(1 2 3))
            (is "+/1⍴⊂⍳3" #0A#(1 2 3))
            (is "↑+/⍬,⊂⊂⍳3" #0A#(1 2 3))
            (is "+/1 1 1⍴1" #2A((1)))
            (is "+/1 1⍴1" #(1))
            (is "+/2 2 1⍴'a'" #2A((#\a #\a) (#\a #\a)))
            (is "⊃,/(⊂1 1)/¨⊂2 3" #(2 3))))
  (⌿ (has :title "Replicate First")
     (dyadic (λωα (make-instance 'vader-expand :base omega :argument alpha :index-origin index-origin
                                               :inverse t :axis (or (first axes) index-origin))))
     (meta (primary :axes axes :implicit-args (index-origin))
           (dyadic :on-axis :first
                   :inverse (λωαχ (if (is-unitary omega)
                                      ;; TODO: this inverse functionality is probably not complete
                                      (make-instance 'vader-expand :base omega :argument alpha
                                                                   :inverse t :index-origin index-origin
                                                                   :axis (or (first axes) index-origin))
                                      (error "Inverse [/ replicate] can only accept~a"
                                             " a scalar right argument.")))))
     (tests (is "3⌿2" #(2 2 2))
            (is "4⌿7 8" #(7 7 7 7 8 8 8 8))
            (is "3⌿3 3⍴⍳9" #2A((1 2 3) (1 2 3) (1 2 3) (4 5 6) (4 5 6) (4 5 6) (7 8 9) (7 8 9) (7 8 9)))
            (is "1 0 1 0 1⌿⍳5" #(1 3 5))
            (is "1 ¯2 3⌿3 5⍴⍳9" #2A((1 2 3 4 5) (0 0 0 0 0) (0 0 0 0 0)
                                    (2 3 4 5 6) (2 3 4 5 6) (2 3 4 5 6)))
            (is "1 ¯2 3 ¯4 5⌿[2]3 5⍴⍳5" #2A((1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)
                                            (1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)
                                            (1 0 0 3 3 3 0 0 0 0 5 5 5 5 5)))
            (is "+⌿1 2 2⍴'a'" #2A((#\a #\a) (#\a #\a)))))
  (\\ (has :title "Expand")
      (dyadic (λωα (make-instance 'vader-expand :base omega :argument alpha :index-origin index-origin
                                                :axis (or (first axes) :last))))
      (meta (primary :axes axes :implicit-args (index-origin))
            (dyadic :on-axis :last))
      (tests (is "4\\2" #(2 2 2 2))
             (is "3\\7" #(7 7 7))
             (is "1 ¯2 3 ¯4 5\\'.'" ".  ...    .....")
             (is "1 ¯2 2 0 1\\3+2 3⍴⍳6" #2A((4 0 0 5 5 0 6) (7 0 0 8 8 0 9)))
             (is "1 ¯2 2 0 1\\⍳3" #(1 0 0 2 2 0 3))
             (is "1 0 1\\[1]3+2 3⍴⍳6" #2A((4 5 6) (0 0 0) (7 8 9)))
             (is "1 ¯2 3 4\\[1]3 5⍴⍳9" #2A((1 2 3 4 5) (0 0 0 0 0) (0 0 0 0 0)
                                           (6 7 8 9 1) (6 7 8 9 1) (6 7 8 9 1)
                                           (2 3 4 5 6) (2 3 4 5 6) (2 3 4 5 6)
                                           (2 3 4 5 6)))
             (is "¯3\\0⍴⊂2 2⍴(⊂3 3⍴⍳6) 9 8 7" #(#2A((#0A#2A((0 0 0) (0 0 0) (0 0 0)) 0) (0 0))
                                                #2A((#0A#2A((0 0 0) (0 0 0) (0 0 0)) 0) (0 0))
                                                #2A((#0A#2A((0 0 0) (0 0 0) (0 0 0)) 0) (0 0))))
             (is "0\\⍬" #(0))
             (is "0\\''" " ")
             (is "0\\0⍴⊂3 3⍴⍳5" #(#2A((0 0 0) (0 0 0) (0 0 0))))
             (is "0\\0⍴⊂'abc'" #("   "))
             (is "0 0\\⍬" #(0 0))
             (is "0 0\\''" "  ")
             (is "0 0\\0⍴⊂⍳3" #(#(0 0 0) #(0 0 0)))
             (is "0 0\\0⍴⊂'abc'" #("   " "   "))
             (is "⍴0 0 0\\0 0⍴0" #(0 3))
             (is "0 0 0 0\\3 0⍴0" #2A((0 0 0 0) (0 0 0 0) (0 0 0 0)))))
  (⍀ (has :title "Expand First")
     (dyadic (λωα (make-instance 'vader-expand :base omega :argument alpha :index-origin index-origin
                                               :axis (or (first axes) index-origin))))
     (meta (primary :axes axes :implicit-args (index-origin))
           (dyadic :on-axis :first))
     (tests (is "2⍀5" #(5 5))
            (is "2⍀1" #*11)
            (is "1 ¯2 3 ¯4 5⍀3" #(3 0 0 3 3 3 0 0 0 0 3 3 3 3 3))
            (is "1 0 1⍀3+2 3⍴⍳6" #2A((4 5 6) (0 0 0) (7 8 9)))))
  (⍋ (has :titles ("Grade Up" "Grade Up By"))
     (ambivalent (λω (make-instance 'vader-grade :base omega :index-origin index-origin))
                 (λωα (make-instance 'vader-grade
                                     :base omega :argument alpha :index-origin index-origin)))
     (meta (primary :implicit-args (index-origin)))
     (tests (is "⍋8 3 4 9 1 5 2" #(5 7 2 3 6 1 4))
            (is "⍋5 6⍴⍳16" #(1 4 2 5 3))
            (is "⍋,0" #*1)
            (is "⍋1 1⍴0" #*1)
            (is "'abcd'⍋,'d'" #*1)
            (is "'nsew'⍋'swwewnh'" #(6 1 4 2 3 5 7))
            (is "st←'aodjeignwug' ⋄ st[⍋st]" "adeggijnouw")
            (is "{⍵[⍋⍵]}'abcABC012xyzXYZ789'" "012789ABCXYZabcxyz")
            (is "(2 5⍴'ABCDEabcde')⍋'ACaEed'" #(1 3 2 6 4 5))))
  (⍒ (has :titles ("Grade Down" "Grade Down By"))
     (ambivalent (λω (make-instance 'vader-grade :base omega :index-origin index-origin :inverse t))
                 (λωα (make-instance 'vader-grade :index-origin index-origin
                                                  :inverse t :base omega :argument alpha)))
     (meta (primary :implicit-args (index-origin)))
     (tests (is "⍒6 1 8 2 4 3 9" #(7 3 1 5 6 4 2))
            (is "⍒5 6⍴⍳12" #(2 4 1 3 5))
            (is "'abcd'⍒,'d'" #*1)
            (is "'nsew'⍒'swwewnh'" #(7 2 3 5 4 1 6))
            (is "st←'aodjeignwug' ⋄ st[⍒st]" "wuonjiggeda")
            (is "{⍵[⍒⍵]}'abcABC012xyzXYZ789'" "zyxcbaZYXCBA987210")
            (is "(2 5⍴'ABCDEabcde')⍒'ACaEed'" #(5 4 6 2 3 1))))
  (⌹ (has :titles ("Matrix Inverse" "Matrix Divide"))
     (ambivalent (λω (make-instance 'vader-matrix-inverse :base omega))
                 (λωα (make-instance 'vader-matrix-divide :base omega :argument alpha)))
     (meta (monadic :inverse (λω (make-instance 'vader-matrix-inverse :base omega))))
     (tests (is "⌹3" 1/3)
            (is "⌹1 2 3 4" #(1/30 1/15 1/10 2/15))
            (is "⌹2 2⍴4 9 8 2" #2A((-1/32 9/64) (1/8 -1/16)))
            (is "⌹4 2⍴1 3 ¯4 9" #2A((3/14 -1/14 3/14 -1/14) (2/21 1/42 2/21 1/42)))
            (is "35 89 79⌹3 3⍴3 1 4 1 5 9 2 6 5" #(193/90 739/90 229/45))
            (is "(3 2⍴1 2 3 6 9 10)⌹3 3⍴1 0 0 1 1 0 1 1 1" #2A((1 2) (2 4) (6 4)))
            ;; linear regression tests
            (is "5 1 4 2 8⌹1,⍪⍳5" #(19/10 7/10))
            (is "6 5 7 10⌹1,⍪⍳4" #(7/2 7/5))))
  (⊤ (has :title "Encode")
     (dyadic (λωα (make-instance 'vader-encode :base omega :argument alpha)))
     (meta (dyadic :id 0 :inverse (λωα (make-instance 'vader-decode :base omega :argument alpha))))
     (tests (is "9⊤15" 6)
            (is "6 2 8⊤12" #(0 1 4))
            (is "1760 3 12⊤82" #(2 0 10))
            (is "16 16 16 16⊤100" #(0 0 6 4))
            (is "16 16⊤¯5" #(15 11))
            (is "0 12⊤8 64 256" #2A((0 5 21) (8 4 4)))
            (is "2 2 2 2 2⊤⍳5" #2A((0 0 0 0 0) (0 0 0 0 0) (0 0 0 1 1) (0 1 1 0 0) (1 0 1 0 1)))
            (is "16 16 16 16⊤2 2⍴100×⍳4"
                #3A(((0 0) (0 0)) ((0 0) (1 1)) ((6 12) (2 9)) ((4 8) (12 0))))
            (is "(2 2⍴16)⊤2 2⍴100 200 300 400"
                #4A((((6 12) (2 9)) ((6 12) (2 9))) (((4 8) (12 0)) ((4 8) (12 0)))))
            (is "(2 2⍴16 8 8 16)⊤2 2⍴100×⍳4"
                #4A((((12 9) (5 2)) ((6 4) (2 1))) (((4 0) (4 0)) ((4 8) (12 0)))))
            (is "(2 2⍴16 8 16 8)⊤2 2⍴100 200 300 400"
                #4A((((6 12) (2 9)) ((4 1) (5 2))) (((4 8) (12 0)) ((4 0) (4 0)))))
            (is "(8 3⍴2 0 0 2 0 0 2 0 0 2 0 0 2 8 0 2 8 0 2 8 16 2 8 16)⊤83"
                #2A((0 0 0) (1 0 0) (0 0 0) (1 0 0) (0 0 0) (0 1 0) (1 2 5) (1 3 3)))))
  (⊥ (has :title "Decode")
     (dyadic (λωα (make-instance 'vader-decode :base omega :argument alpha)))
     (meta (dyadic :inverse (λωα (make-instance 'vader-encode :base omega :argument alpha :inverse t))))
     (tests (is "14⊥7" 7)
            (is "6⊥12 50" 122)
            (is "10⊥2 6 7 1" 2671)
            (is "32 14⊥7" 105)
            (is "1760 3 12⊥2 2 5" 101)
            (is "1J1⊥⍳4" #C(5 9))
            (is "1760 3 12⊥3 3⍴1 2 1 5 0 2 2 3 7" #(98 75 67))
            (is "(3 3⍴1760 3 12)⊥3 3⍴2 2 5 1 4 9 6 6 7" #2A((90 126 295) (90 126 295) (90 126 295)))
            (is "2⊥3 8⍴0 0 0 0 1 1 1 1 0 0 1 1 0 0 1 1 0 1 0 1 0 1 0 1" #(0 1 2 3 4 5 6 7))
            (is "(3/⍪5 8 12)⊥3 3⍴2 2 5 1 4 9 6 6 7" #2A((61 76 177) (142 166 399) (306 342 835)))
            (is "(3/⍪⍳4)⊥3 8⍴0 0 0 0 1 1 1 1 0 0 1 1 0 0 1 1 0 1 0 1 0 1 0 1"
                #2A((0 1 1 2 1 2 2 3) (0 1 2 3 4 5 6 7)
                    (0 1 3 4 9 10 12 13) (0 1 4 5 16 17 20 21)))
            (is "(⍪2 10)⊥3 8⍴0 0 0 0 1 1 1 1 0 0 1 1 0 0 1 1 0 1 0 1 0 1 0 1"
                #2A((0 1 2 3 4 5 6 7) (0 1 10 11 100 101 110 111)))
            (is "(3⍴2*8)⊥3 4 4⍴⍳39" #2A((69921 135714 201507 267300) (333093 398886 464679 530433)
                                        (596226 662019 727812 793605) (859398 925191 990984 1056777))))))

 (functions
  (with (:name :lexical-functions-special)
        (:tests-profile :title "Special Function Tests")
        (:demo-profile :title "Special Function Demos"
                       :description "These functions expose features of the language that aren't directly related to computing or transforming array values."))
  (⊢ (has :titles ("Identity" "Right"))
     (ambivalent (λω (make-instance 'vader-identity :base omega))
                 (λωα (declare (ignore alpha))
                      (make-instance 'vader-identity :base omega)))
     (meta (monadic :inverse (λω (make-instance 'vader-identity :base omega)))
           (dyadic :inverse (λωα (declare (ignore alpha))
                                 (make-instance 'vader-identity :base omega))
                   :selective-assignment-passthrough t))
     (tests (is "⊢77" 77)
            (is "55⊢77" 77)))
  (⊣ (has :titles ("Empty" "Left"))
     (ambivalent (λω (make-instance 'vader-identity :base omega :inverse t))
                 (λωα (declare (ignore omega))
                      (make-instance 'vader-identity :base alpha :inverse t)))
     (tests (is "⊣77" 77)
            (is "55⊣77" 55)))
  (⍕ (has :titles ("Format" "Format At Precision"))
     (ambivalent (format-array print-precision) (format-array print-precision))
     (meta (primary :implicit-args (print-precision)))
     (tests (is "↓⍕3 4⍴⍳9" #("1 2 3 4"
                             "5 6 7 8"
                             "9 1 2 3"))
            (is "↓⍕2 3 4⍴⍳9" #2A(("1 2 3 4" "5 6 7 8" "9 1 2 3")
                                 ("4 5 6 7" "8 9 1 2" "3 4 5 6")))
            (is "↓⍕⊂2 3 4⍴⍳9" #(" 1 2 3 4" " 5 6 7 8" " 9 1 2 3" "        "
                                " 4 5 6 7" " 8 9 1 2" " 3 4 5 6"))
            (is "↓⍕3⍴⊂3 4⍴⍳9" #(" 1 2 3 4  1 2 3 4  1 2 3 4"
                                " 5 6 7 8  5 6 7 8  5 6 7 8"
                                " 9 1 2 3  9 1 2 3  9 1 2 3"))
            (is "↓3⍕○3 4⍴⍳9" #(" 3.142  6.283  9.425 12.566"
                               "15.708 18.850 21.991 25.133"
                               "28.274  3.142  6.283  9.425"))
            (is "↓5⍕○3 4⍴⍳9" #(" 3.14159  6.28319  9.42478 12.56637"
                               "15.70796 18.84956 21.99115 25.13274"
                               "28.27433  3.14159  6.28319  9.42478"))
            (is "⍕'a'" #\a)))
  (⍎ (has :title "Evaluate")
     (monadic (λω (eval (vex-program *april-idiom* `((state :print-output nil) (:space ,+workspace-name+))
                                     (string (vrender omega))))))
     (meta (primary :implicit-args (+workspace-name+)))
     (tests (is "⍎'1+1'" 2)
            (is "⍎'5','+3 2 1'" #(8 7 6))
            (is "⍎'3'" 3)
            (is "v←⍳3 ⋄ ⍎'v'" #(1 2 3))
            (is "⍎¨'1+1' '2+2' '3+3'" #(2 4 6))
            (is "⍎(' '@{'x'=⍵}) '(22x11)'" #(22 11))))
  (← (has :title "Assign")
     (symbolic :special-lexical-form-assign)
     (tests (is "x←55 ⋄ x" 55)
            (is "x←2 3 4⍴⍳9 ⋄ x[;1;]←7 ⋄ x" #3A(((7 7 7 7) (5 6 7 8) (9 1 2 3))
                                                ((7 7 7 7) (8 9 1 2) (3 4 5 6))))
            (is "(a b c)←1 2 3 ⋄ a b c" #(1 2 3))
            (is "(a b c)←5 ⋄ ⊃+/a b c" 15)
            (is "(a b c)←10 20 30 ⋄ a+b×c" 610)
            (is "5+(a b c)←1 2 3" #(6 7 8))))
  (→ (has :title "Branch") 
     (symbolic :special-lexical-form-branch)
     (tests (is "x←1 ⋄ →1              ⋄ x×←11 ⋄ 1  →⎕ ⋄ x×←3 ⋄ 2  →⎕ ⋄ x×←5 ⋄ 3    →⎕ ⋄ x×←7" 105)
            (is "x←1 ⋄ →1+1            ⋄ x×←11 ⋄ 1  →⎕ ⋄ x×←3 ⋄ 2  →⎕ ⋄ x×←5 ⋄ 3    →⎕ ⋄ x×←7" 35)
            (is "x←1 ⋄ →2+3            ⋄ x×←11 ⋄ 1  →⎕ ⋄ x×←3 ⋄ 2  →⎕ ⋄ x×←5 ⋄ 3    →⎕ ⋄ x×←7" 1155)
            (is "x←1 ⋄ →0              ⋄ x×←11 ⋄ 1  →⎕ ⋄ x×←3 ⋄ 2  →⎕ ⋄ x×←5 ⋄ 3    →⎕ ⋄ x×←7" 1155)
            (is "x←1 ⋄ →three          ⋄ x×←11 ⋄ one→⎕ ⋄ x×←3 ⋄ two→⎕ ⋄ x×←5 ⋄ three→⎕ ⋄ x×←7" 7)
            (is "x←1 ⋄ (3-2)→two three ⋄ x×←11 ⋄ one→⎕ ⋄ x×←3 ⋄ two→⎕ ⋄ x×←5 ⋄ three→⎕ ⋄ x×←7" 35)
            (is "x←1 ⋄ 3→one two three ⋄ x×←11 ⋄ one→⎕ ⋄ x×←3 ⋄ two→⎕ ⋄ x×←5 ⋄ three→⎕ ⋄ x×←7" 7)
            (is "x←1 ⋄ 0→two three     ⋄ x×←11 ⋄ one→⎕ ⋄ x×←3 ⋄ two→⎕ ⋄ x×←5 ⋄ three→⎕ ⋄ x×←7" 1155)))
  (∘ (has :title "Find Outer Product, Not Inner")
     (symbolic :outer-product-designator)))

 ;; APL's lexical operators, which take one or two functions or arrays
 ;; as input and return a function that can be called on yet other values
 (operators
  (with (:name :lexical-operators-lateral)
        (:tests-profile :title "Lateral Operator Tests")
        (:demo-profile :title "Lateral Operator Demos"
                       :description "Lateral operators take a single operand function to their left, hence the name 'lateral.' The combination of operator and function yields another function which may be applied to one or two arguments depending on the operator."))
  (/ (has :title "Reduce")
     (lateral (lambda (operand)
                (values `(op-compose 'vacomp-reduce :left (sub-lex ,operand)
                                                    :index-origin index-origin)
                        '(:axis))))
     (tests (is "+/1 2 3 4 5" 15)
	    (is-error "+/⍳¯5")
	    (is "+/⍳5" 15)
	    (is "io←⎕IO⋄⎕IO←0⋄x←+/⍳5⋄⎕IO←io⋄x" 10)
	    (is "+/⍳2 5" #(#(5 15) #(10 15)))
	    (is "+/⍳2 3 4"
		#2A((#(4 4 10) #(4 8 10) #(4 12 10)) (#(8 4 10) #(8 8 10) #(8 12 10))))
            (is "⊢/⍳5" 5)
            (is "×/5" 5)
            (is "÷/5" 5)
            (is "+/3 4⍴⍳12" #(10 26 42))
            (is "-/3 4⍴⍳12" #(-2 -2 -2))
            (is "+/[1]3 4⍴⍳12" #(15 18 21 24))
            (is "fn←{⍺+⍵} ⋄ fn/1 2 3 4 5" 15)
            (is "⌊10_000×{⍺+÷⍵}/40/1" 16180)
            (is "+/⍬" 0)
            (is "-/⍬" 0)
            (is "×/⍬" 1)
            (is "÷/⍬" 1)
            (is "</⍬" 0)
            (is "≤/⍬" 1)
            (is "⊤/⍬" 0)
            (is "∪/⍬" #0A#())
            (is "f←+ ⋄ f/⍬" 0)
            (is "g←÷ ⋄ g/⍬" 1)
            (is "⍴×/0 0 0⍴0" #*00)
            (is "+/(1 2 3)(4 5 6)" #0A#(5 7 9))
            (is "∩/¨(1 0 0) (1 1 0 1 0)⊂¨'abc' 'a|b|c'" #(#0A"abc" #0A""))
            (is "3+/⍳20" #(6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57))
            (is "4,/⍳12" #(#(1 2 3 4) #(2 3 4 5) #(3 4 5 6) #(4 5 6 7) #(5 6 7 8)
                           #(6 7 8 9) #(7 8 9 10) #(8 9 10 11) #(9 10 11 12)))
            (is "⊃,/3 4+/¨⊂3 6⍴⍳9"
                #2A((6 9 12 15 10 14 18) (24 18 12 6 25 20 15) (15 18 21 24 22 26 30)))
            (is "⊃,/(⊂'abc') 'def' 'ghi'" #("abc" #\d #\e #\f #\g #\h #\i))
            (is "(×⌿,+⌿)+⌿(⍳2)∘.×⍬∘.×⍳4" #(1 1 1 1 0 0 0 0))
            (is "+/⍳2 5" #(#(5 15) #(10 15)))))
  (⌿ (has :title "Reduce First")
     (lateral (lambda (operand)
                (values `(op-compose 'vacomp-reduce :left (sub-lex ,operand)
                                                    :index-origin index-origin
                                                    :default-axis index-origin)
                        '(:axis))))
     (tests (is "+⌿3 4⍴⍳12" #(15 18 21 24))
            (is "-⌿3 4⍴⍳12" #(5 6 7 8))
            (is "{⍺×⍵+3}⌿3 4⍴⍳12" #(63 162 303 492))
            (is "+⌿[2]3 4⍴⍳12" #(10 26 42))
            (is "-⌿⍤1⊢1 2 2⍴⍳4" #2A((-1 -1)))
            (is "-⌿⍤2⊢1 2 2⍴⍳4" #2A((-2 -2)))
            (is "-⌿⍤3⊢1 2 2⍴⍳4" #2A((1 2) (3 4)))
            (is "-⌿⍤1⊢2 2 1⍴⍳4" #2A((1 2) (3 4)))
            (is "-⌿⍤2⊢2 2 1⍴⍳4" #2A((-1) (-1)))
            (is "-⌿⍤3⊢2 2 1⍴⍳4" #2A((-2) (-2)))))
  (\\ (has :title "Scan")
      (lateral (lambda (operand) (values `(op-compose 'vacomp-scan :left (sub-lex ,operand)
                                                                   :index-origin index-origin
                                                                   :inverse nil)
                                         '(:axis))))
      (tests (is "÷\\5" 5)
             (is "+\\1 2 3 4 5" #(1 3 6 10 15))
             (is "+\\3 4⍴⍳12" #2A((1 3 6 10) (5 11 18 26) (9 19 30 42)))
             (is "+\\[1]3 4⍴⍳12" #2A((1 2 3 4) (6 8 10 12) (15 18 21 24)))
             (is "-\\2 3 4⍴⍳24" #3A(((1 -1 2 -2) (5 -1 6 -2) (9 -1 10 -2))
                                    ((13 -1 14 -2) (17 -1 18 -2) (21 -1 22 -2))))))
  (⍀ (has :title "Scan First")
     (lateral (lambda (operand) (values `(op-compose 'vacomp-scan :left (sub-lex ,operand)
                                                                  :index-origin index-origin
                                                                  :default-axis index-origin
                                                                  :inverse nil)
                                        '(:axis))))
     (tests (is "+⍀1 2 3 4 5" #(1 3 6 10 15))
            (is "+⍀3 4⍴⍳12" #2A((1 2 3 4) (6 8 10 12) (15 18 21 24)))
            (is "{⍺×⍵+3}⍀3 4⍴⍳12" #2A((1 2 3 4) (8 18 30 44) (63 162 303 492)))
            (is "+⍀[2]3 4⍴⍳12" #2A((1 3 6 10) (5 11 18 26) (9 19 30 42)))))
  (\¨ (has :title "Each")
      (lateral (lambda (operand) `(operate-each (sub-lex ,operand))))
      (tests (is "⍳¨1 2 3" #(#(1) #(1 2) #(1 2 3)))
             (is "⊃¨↓⍳5" 1)
             (is "(1∘=)¨⍬,1" #(1))
             (is "{⍵÷3}¨10" 10/3)
             (is "⍴⊢¨⊂1 2 3" #())
             (is "1 {⍺+⍵÷3}¨10" 13/3)
             (is "2⍴¨'*'" #0A"**")
             (is "3⍴¨1 2 3" #(#(1 1 1) #(2 2 2) #(3 3 3)))
             (is "3 4 5⍴¨3" #(#(3 3 3) #(3 3 3 3) #(3 3 3 3 3)))
             (is "1 ¯1⌽¨⊂⍳5" #(#(2 3 4 5 1) #(5 1 2 3 4)))
             (is "3+¨3 3⍴⍳9" #2A((4 5 6) (7 8 9) (10 11 12)))
             (is "1 2 3+¨1⍴3" #(4 5 6))
             (is "⊃⍪/,/(⊂2 2⍴2 3 1 4){⍺+⍵××/⍴⍺}¨3 3⍴⍳9"
                 #2A((6 7 10 11 14 15) (5 8 9 12 13 16) (18 19 22 23 26 27) (17 20 21 24 25 28)
                     (30 31 34 35 38 39) (29 32 33 36 37 40)))
             (is "(⍳3)⌽[1]¨⊂2 3 4⍴⍳9" #(#3A(((4 5 6 7) (8 9 1 2) (3 4 5 6)) ((1 2 3 4) (5 6 7 8) (9 1 2 3)))
                                        #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3)) ((4 5 6 7) (8 9 1 2) (3 4 5 6)))
                                        #3A(((4 5 6 7) (8 9 1 2) (3 4 5 6)) ((1 2 3 4) (5 6 7 8) (9 1 2 3)))))
             (is "(⍳3)⌽[2]¨⊂2 3 4⍴⍳9" #(#3A(((5 6 7 8) (9 1 2 3) (1 2 3 4)) ((8 9 1 2) (3 4 5 6) (4 5 6 7)))
                                        #3A(((9 1 2 3) (1 2 3 4) (5 6 7 8)) ((3 4 5 6) (4 5 6 7) (8 9 1 2)))
                                        #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3)) ((4 5 6 7) (8 9 1 2) (3 4 5 6)))))
             (is "(⍳3)⌽[3]¨⊂2 3 4⍴⍳9" #(#3A(((2 3 4 1) (6 7 8 5) (1 2 3 9)) ((5 6 7 4) (9 1 2 8) (4 5 6 3)))
                                        #3A(((3 4 1 2) (7 8 5 6) (2 3 9 1)) ((6 7 4 5) (1 2 8 9) (5 6 3 4)))
                                        #3A(((4 1 2 3) (8 5 6 7) (3 9 1 2)) ((7 4 5 6) (2 8 9 1) (6 3 4 5)))))
             (is "(3 3⍴⊂3 3⍴⍳9)×¨3 3⍴⍳9" #2A((#2A((1 2 3) (4 5 6) (7 8 9))
                                                 #2A((2 4 6) (8 10 12) (14 16 18))
                                                 #2A((3 6 9) (12 15 18) (21 24 27)))
                                             (#2A((4 8 12) (16 20 24) (28 32 36))
                                                 #2A((5 10 15) (20 25 30) (35 40 45))
                                                 #2A((6 12 18) (24 30 36) (42 48 54)))
                                             (#2A((7 14 21) (28 35 42) (49 56 63))
                                                 #2A((8 16 24) (32 40 48) (56 64 72))
                                                 #2A((9 18 27) (36 45 54) (63 72 81)))))
             (is "(1 1 1⍴⊂1 1 1)↓¨⊂3 3 3⍴⍳27" #3A(((#3A(((14 15) (17 18)) ((23 24) (26 27)))))))
             (is "(1 0 0) (1 1 0 1 0)⊂¨'abc' 'a|b|c'"
                 #(#("abc") #("a" "|b" "|c")))
             (is "~∘3¨@2⊢(2 3) (3) (2 4) (1 5) (3)" #(#(2 3) #() #(2 4) #(1 5) 3))
             (is "(⊂'ab'),¨1⍴⊂⊂,'c'" #(#(#\a #\b "c")))
             (is "+/¨{1⊂↑⍵}⍴¨2 2⍴⊂'abc'" #(#2A((3 3) (3 3))))))
  (⍨ (has :title "Commute")
     (lateral (lambda (operand) `(operate-commuting (sub-lex ,operand))))
     (tests (is "5-⍨10" 5)
            (is "+⍨10" 20)
            (is "fn←{⍺+3×⍵} ⋄ 16 fn⍨8" 56)
            (is "(5⍨) 2 6" 5)
            (is "7 (5⍨) 2 6" 5)))
  (⌸ (has :title "Key")
     (lateral (lambda (operand) `(operate-grouping ,operand index-origin)))
     (tests (is "fruit←'Apple' 'Orange' 'Apple' 'Pear' 'Orange' 'Peach' 'Pear' 'Pear'
    quantities ← 12 3 2 6 8 16 7 3 ⋄ fruit {⍺ ⍵}⌸ quantities"
                #2A(("Apple" #(12 2)) ("Orange" #(3 8)) ("Pear" #(6 7 3)) ("Peach" #(16))))
            (is "{⊂⍵}⌸ quantities" #(#(1) #(2 8) #(3) #(4) #(5) #(6) #(7)))
            (is "fruit←'Apple' 'Orange' 'Apple' 'Pear' 'Orange' 'Peach' ⋄ {⍴⍵}⌸ fruit"
                #2A((2) (2) (1) (1)))
            (is "{(2|⍳≢⍵)⊢⌸⍵}10 2⍴⍳20" #3A(((1 2) (5 6) (9 10) (13 14) (17 18))
                                           ((3 4) (7 8) (11 12) (15 16) (19 20))))
            (is "{⍺⍵}⌸↑(1 2) (1 2) (3 4)" #2A((#(1 2) #(1 2)) (#(3 4) #(3)))))))

 (operators
  (with (:name :lexical-operators-pivotal)
        (:tests-profile :title "Pivotal Operator Tests")
        (:demo-profile :title "Pivotal Operator Demos"
                       :description "Pivotal operators are so called because they are entered between two operands. Depending on the operator, these operands may be functions or array values, with the combination yielding a new function."))
  (\. (has :title "Inner/Outer Product")
      (pivotal (lambda (right left)
                 (if (eq left :outer-product-designator)
                     `(operate-producing-outer (sub-lex ,right))
                     `(op-compose 'vacomp-produce
                                  :right (sub-lex ,right) :left (sub-lex ,left)
                                  :index-origin index-origin))))
      (tests (is "3+.×5" 15)
             (is "2+.×3 4 5" 24)
             (is "2 3 4+.×8 15 21" 145)
             (is "2 3 4+.×3 3⍴3 1 4 1 5 9 2 6 5" #(17 41 55))
             (is "⍴1 2 3+.+⍪1 2 3" #*1)
             (is "(3 3⍴3 1 4 1 5 9 2 6 5)+.×2 3 4" #(25 53 42))
             (is "{⍵ ⍵+.+⍵ ⍵} 3 3⍴⍳9" #0A#2A((4 8 12) (16 20 24) (28 32 36)))
             (is "1 2 3 ÷.+ 1 2 3" 3)
             (is "5∘.+5" 10)
             (is "16∘.*⍳3" #(16 256 4096))
             (is "4 5 6∘.+20 30 40 50" #2A((24 34 44 54) (25 35 45 55) (26 36 46 56)))
             (is "1 2 3∘.-1 2 3" #2A((0 -1 -2) (1 0 -1) (2 1 0)))
             (is "1 2 3∘.⍴1 2 3" #2A((#(1) #(2) #(3))
                                     (#(1 1) #(2 2) #(3 3)) (#(1 1 1) #(2 2 2) #(3 3 3))))
             (is "↑0 1 2∘.⌽↓3 3 ⍴⍳3" #3A(((1 2 3) (1 2 3) (1 2 3)) ((2 3 1) (2 3 1) (2 3 1))
                                         ((3 1 2) (3 1 2) (3 1 2))))
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
             (is "(1 2 3) (2 3 4)∘.⌽[1]⊂3 3⍴⍳9" #(#2A((4 8 3) (7 2 6) (1 5 9))
                                                  #2A((7 2 6) (1 5 9) (4 8 3))))
             (is "⍬∘.=⍬" #2A())
             (is "''∘.=''" #2A())
             (is "fn←{⍺×⍵+1} ⋄ 1 2 3∘.fn 4 5 6" #2A((5 6 7) (10 12 14) (15 18 21)))
             (is "' ' { A W←{(⍵≠(≢⍵)⍴' ')/⍵}¨⍺ ⍵ ⋄ ((⍴A)=⍴W)∧∧/(+/A∘.=W)=+/A∘.=A } 'dog'" #(0))
             (is "⍴+.×⌿?2 30 30⍴1e10" #(30 30))
             (is "'ADG',.,'EIHF' 'BIHC' 'BFEC'" #0A"AEIHFDBIHCGBFEC")
             (is "1 2 3,.-3 3⍴4 5 6" #(#(-3 -2 -1) #(-4 -3 -2) #(-5 -4 -3)))))
  (∘ (has :title "Beside")
     (pivotal (lambda (right left) `(operate-beside (sub-lex ,right) (sub-lex ,left))))
     (tests (is "-∘- 1" 1)
            (is "1 -∘- 1" 2)
            (is "fn←⍴∘⍴ ⋄ fn 2 3 4⍴⍳9" #(3))
            (is "⍴∘⍴ 2 3 4⍴⍳9" #(3))
            (is "⍴∘⍴∘⍴ 2 3 4⍴⍳9" #*1)
            (is "÷∘5 ⊢30" 6)
            (is "⌊10_000×(+∘*∘0.5) 4 16 25" #(56487 176487 266487))
            (is "fn←5∘- ⋄ fn 2" 3)
            (is "⌊0.5∘+∘*5 8 12" #(148 2981 162755))
            (is "⌊10_000×+∘÷/40/1" 16180)
            (is "fn←+/ ⋄ fn∘⍳¨2 5 8" #(3 15 36))
            (is "3 4⍴∘⍴2 4 5⍴9" #2A((2 4 5 2) (4 5 2 4) (5 2 4 5)))
            (is "(2 3 4 5∘+) 5" #(7 8 9 10))
            (is "qq←-∘⌽ ⋄ qq 3 3⍴⍳9" #2A((-3 -2 -1) (-6 -5 -4) (-9 -8 -7)))
            (is "rr←-∘⌽[1] ⋄ rr 3 3⍴⍳9" #2A((-7 -8 -9) (-4 -5 -6) (-1 -2 -3)))
            (is "+/∘(+/)¨4 5×⊂3 3⍴⍳9" #(180 225))))
  (⍛ (has :title "Before")
     (pivotal (lambda (right left) `(operate-before ,right ,left)))
     (tests (is "1 -⍛- 1" -2)
            (is "3 (2∘*)⍛+∘(2∘⍟) 4" 10)))
  (⍤ (has :title "Rank / Atop")
     (pivotal (lambda (right left)
                (let ((r (gensym)))
                  `(let ((,r ,right))
                     (if (functionp ,r) (operate-atop ,right ,left)
                         (operate-at-rank ,right ,left))))))
     (tests (is "⊂⍤2⊢2 3 4⍴⍳9" #(#2A((1 2 3 4) (5 6 7 8) (9 1 2 3)) #2A((4 5 6 7) (8 9 1 2) (3 4 5 6))))
            (is "⊂⍤1 0 2⊢2 3 4⍴⍳9" #2A((#(1 2 3 4) #(5 6 7 8) #(9 1 2 3)) (#(4 5 6 7) #(8 9 1 2) #(3 4 5 6))))
            (is "{(⊂⍋⍵)⌷⍵}⍤1⊢3 4 5⍴⍳9" #3A(((1 2 3 4 5) (1 6 7 8 9) (2 3 4 5 6) (1 2 7 8 9))
                                           ((3 4 5 6 7) (1 2 3 8 9) (4 5 6 7 8) (1 2 3 4 9))
                                           ((5 6 7 8 9) (1 2 3 4 5) (1 6 7 8 9) (2 3 4 5 6))))
            (is "10 20 30 40+⍤1⊢4 4 4⍴⍳16"
                #3A(((11 22 33 44) (15 26 37 48) (19 30 41 52) (23 34 45 56))
                    ((11 22 33 44) (15 26 37 48) (19 30 41 52) (23 34 45 56))
                    ((11 22 33 44) (15 26 37 48) (19 30 41 52) (23 34 45 56))
                    ((11 22 33 44) (15 26 37 48) (19 30 41 52) (23 34 45 56))))
            (is "(3 4 5⍴⍳9)+∘⊃⍤2⊢⍳3" #3A(((2 3 4 5 6) (7 8 9 10 2) (3 4 5 6 7) (8 9 10 2 3))
                                         ((4 5 6 7 8) (9 10 2 3 4) (5 6 7 8 9) (10 2 3 4 5))
                                         ((6 7 8 9 10) (2 3 4 5 6) (7 8 9 10 2) (3 4 5 6 7))))
            (is "(⍳5)+⍤1⊢1 5⍴⍳5" #2A((2 4 6 8 10)))
            (is "(⍳3)+⍤1⊢3 3⍴5" #2A((6 7 8) (6 7 8) (6 7 8)))
            (is "(⍳3)+⍤0 1⊢3 3⍴5" #2A((6 6 6) (7 7 7) (8 8 8)))
            (is "(4⍪⍤0⍨⍳3),4⍪⍨⍤0⍳3" #2A((1 4 1 4) (2 4 2 4) (3 4 3 4)))
            (is "(4⍪⍤1⍨⍳3),4⍪⍨⍤1⍳3" #(1 2 3 4 1 2 3 4))
            (is "(⍪⍳3)+⍤1⊢3 3⍴5" #2A((6 6 6) (7 7 7) (8 8 8)))
            (is "fn←{⍺+2×⍵} ⋄ 15 25 35 fn⍤1⊢2 2 3⍴⍳8" #3A(((17 29 41) (23 35 47)) ((29 41 37) (19 31 43))))
            (is ",⍤¯1⊢2 3 4⍴⍳24" #2A((1 2 3 4 5 6 7 8 9 10 11 12) (13 14 15 16 17 18 19 20 21 22 23 24)))
            (is "⍉⍤2⊢2 3 4⍴⍳9" #3A(((1 5 9) (2 6 1) (3 7 2) (4 8 3)) ((4 8 3) (5 9 4) (6 1 5) (7 2 6))))
            (is "10 20 30{⍺ ⍵}⍤¯1⊢3 4⍴⍳12" #2A((10 #(1 2 3 4)) (20 #(5 6 7 8)) (30 #(9 10 11 12))))
            (is "(-⍤÷) 4" -1/4)
            (is "⌊3 (⋆⍤×) 4" 162754)))
  (⍥ (has :title "Over")
     (pivotal (lambda (right left)
                (let ((omega (gensym)) (alpha (gensym)) (fn-right (gensym)) (fn-left (gensym)))
                  `(lambda (,omega &optional ,alpha)
                     (let ((,fn-right ,right) (,fn-left ,left))
                       (if ,alpha (funcall ,fn-left (funcall ,fn-right ,omega)
                                           (funcall ,fn-right ,alpha))
                           (funcall ,fn-left (funcall ,fn-right ,omega))))))))
     (tests (is "s←88 67 72 ⋄ w←15 35 22 ⋄ (w×s)÷⍥(+/)w" 5249/72)))
  (⍣ (has :title "Power")
     (pivotal (lambda (right left) `(operate-to-power (lambda () ,right) ,left)))
     (tests (is "fn←{2+⍵}⍣3 ⋄ fn 5" 11)
            (is "{2+⍵}⍣3⊢9" 15)
            (is "2{⍺×2+⍵}⍣3⊢9" 100)
            (is "{3×⍵}⍣(gg←3)⊢5" 135)
            (is "fn←{2+⍵}⍣{10<⍺} ⋄ fn 2" 12)
            (is "fn←{2+⍵}⍣{10<⍵} ⋄ fn 2" 14)
            (is "fn←{⍵×2} ⋄ fn⍣3⊢4" 32)
            (is "↓⍣2⊢2 2⍴⍳4" #0A#(#(1 2) #(3 4)))
            (is "⌊1_000_000_000×2○⍣=1" 739085133)
            (is "⌊100_000×{⍵{2.0÷⍨⍵+⍺÷⍵}⍣≡⍵}123456789" 1111111106)
            (is "⌊10_000×1+∘÷⍣=1.0" 16180)))
  (@ (has :title "At")
     (pivotal (lambda (right left) `(operate-at ,right ,left index-origin)))
     (tests (is "20 20@3 8⍳9" #(1 2 20 4 5 6 7 20 9))
            (is "(0@2 4)⍳9" #(1 0 3 0 5 6 7 8 9))
            (is "('*'@2)⍳5" #(1 #\* 3 4 5))
            (is "((2 5⍴0 1)@2 5) 5 5⍴⍳9" #2A((1 2 3 4 5) (0 1 0 1 0) (2 3 4 5 6)
                                             (7 8 9 1 2) (1 0 1 0 1)))
            (is "0@(×∘(3∘|)) ⍳9" #(0 0 3 0 0 6 0 0 9))
            (is "÷@3 5 ⍳9" #(1 2 1/3 4 1/5 6 7 8 9))
            (is "⌽@(2∘|)⍳5" #(5 2 3 4 1))
            (is "⌽@1 3 5⊢⍳5" #(5 2 3 4 1))
            (is "⌽@1 3⊢4 5⍴⍳40" #2A((5 4 3 2 1) (6 7 8 9 10) (15 14 13 12 11) (16 17 18 19 20)))
            (is "⌽@1⊢2 4 5⍴⍳40" #3A(((5 4 3 2 1) (10 9 8 7 6) (15 14 13 12 11) (20 19 18 17 16))
                                    ((21 22 23 24 25) (26 27 28 29 30) (31 32 33 34 35) (36 37 38 39 40))))
            (is "{⍵×2}@{⍵>3}⍳9" #(1 2 3 8 10 12 14 16 18))
            (is "fn←{⍺+⍵×12} ⋄ test←{0=3|⍵} ⋄ 4 fn@test ⍳12" #(1 2 40 4 5 76 7 8 112 10 11 148))
            (is "∪∘1@5⊢(2 3) (3) (2 4) (1 5) (3)" #(#(2 3) 3 #(2 4) #(1 5) #(3 1)))
            (is "(9+3 4⍴⍳12)⊣@3 2 3⊢4 4⍴⍳16" #2A((1 2 3 4) (14 15 16 17) (18 19 20 21) (13 14 15 16)))
            (is "{1 (3 {(⍹⊃⍵)@(⊂⍶ ⍺)⊢⍵} 4) ⍵} (0 0 0) (0 0 0) (0 0 0) 1 ⍬"
                #(#(0 0 0) #(0 0 0) #(1 0 0) 1 #()))
            (is "55@(⊂2 5)⊢4⍴⊂⍳9" #(#(1 2 3 4 5 6 7 8 9) #(1 2 3 4 55 6 7 8 9)
                                    #(1 2 3 4 5 6 7 8 9) #(1 2 3 4 5 6 7 8 9)))
            (is "3⌈@(⊂1 3)⊢3⍴⊂5⍴1" #(#(1 1 3 1 1) #*11111 #*11111))
            (is "5@(3 3)(4 4)⊢6 6⍴0" #2A((0 0 0 0 0 0) (0 0 0 0 0 0) (0 0 5 0 0 0)
                                                       (0 0 0 5 0 0) (0 0 0 0 0 0) (0 0 0 0 0 0)))
            (is "{⍵[2;2]÷⍨@2⊢⍵} 3 3⍴⍳9" #2A((1 2 3) (4/5 1 6/5) (7 8 9)))
            (is "9@(1+⍳⍴2 2 2⍴⍳8)⊢4 4 4⍴0" #3A(((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0))
                                               ((0 0 0 0) (0 9 9 0) (0 9 9 0) (0 0 0 0))
                                               ((0 0 0 0) (0 9 9 0) (0 9 9 0) (0 0 0 0))
                                               ((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0))))
            (is "0{⍵@(1+⍳⍴⍵)⊢⍺⍴⍨2+⍴⍵}2 2⍴⍳4" #2A((0 0 0 0) (0 1 2 0) (0 3 4 0) (0 0 0 0)))))
  (⌺ (has :title "Stencil")
     (pivotal (lambda (right left) `(operate-stenciling ,right ,left)))
     (tests (is "{⊂⍵}⌺(⍪3 2)⍳8" #(#(0 1 2) #(2 3 4) #(4 5 6) #(6 7 8)))
            (is "{⊂⍵}⌺(⍪5 2)⍳9" #(#(0 0 1 2 3) #(1 2 3 4 5) #(3 4 5 6 7) #(5 6 7 8 9) #(7 8 9 0 0)))
            (is "⊢∘⊂⌺2⍳8" #(#(1 2) #(2 3) #(3 4) #(4 5) #(5 6) #(6 7) #(7 8)))
            (is "⊢∘⊂⌺4⍳8" #(#(0 1 2 3) #(1 2 3 4) #(2 3 4 5) #(3 4 5 6)
                            #(4 5 6 7) #(5 6 7 8) #(6 7 8 0)))
            (is "⊢∘⊂⌺4⍳9" #(#(0 1 2 3) #(1 2 3 4) #(2 3 4 5) #(3 4 5 6)
                            #(4 5 6 7) #(5 6 7 8) #(6 7 8 9) #(7 8 9 0)))
            (is "⊢∘⊂⌺(⍪4 2)⍳8" #(#(0 1 2 3) #(2 3 4 5) #(4 5 6 7) #(6 7 8 0)))
            (is "⊢∘⊂⌺(⍪6 2)⍳8" #(#(0 0 1 2 3 4) #(1 2 3 4 5 6) #(3 4 5 6 7 8) #(5 6 7 8 0 0)))
            (is "⊢⌺3 3⊢3 3⍴⍳9"
                #4A((((0 0 0) (0 1 2) (0 4 5)) ((0 0 0) (1 2 3) (4 5 6)) ((0 0 0) (2 3 0) (5 6 0)))
                    (((0 1 2) (0 4 5) (0 7 8)) ((1 2 3) (4 5 6) (7 8 9)) ((2 3 0) (5 6 0) (8 9 0)))
                    (((0 4 5) (0 7 8) (0 0 0)) ((4 5 6) (7 8 9) (0 0 0)) ((5 6 0) (8 9 0) (0 0 0)))))
            (is "⊢⌺3 3⊢2 2⍴⍳12" #4A((((0 0 0) (0 1 2) (0 3 4)) ((0 0 0) (1 2 0) (3 4 0)))
                                    (((0 1 2) (0 3 4) (0 0 0)) ((1 2 0) (3 4 0) (0 0 0)))))
            (is "⊢⌺2 2⊢4 4⍴⍳16"
                #4A((((1 2) (5 6)) ((2 3) (6 7)) ((3 4) (7 8)))
                    (((5 6) (9 10)) ((6 7) (10 11)) ((7 8) (11 12)))
                    (((9 10) (13 14)) ((10 11) (14 15)) ((11 12) (15 16)))))
            (is "⊢⌺(2 2⍴3 3 2 1)⊢4 4⍴⍳16"
                #4A((((0 0 0) (0 1 2) (0 5 6)) ((0 0 0) (1 2 3) (5 6 7))
                                               ((0 0 0) (2 3 4) (6 7 8)) ((0 0 0) (3 4 0) (7 8 0)))
                    (((0 5 6) (0 9 10) (0 13 14)) ((5 6 7) (9 10 11) (13 14 15))
                                                  ((6 7 8) (10 11 12) (14 15 16))
                                                  ((7 8 0) (11 12 0) (15 16 0)))))
            (is "{⊂⍺ ⍵}⌺3 3⊢3 3⍴⍳9"
                #2A((#(#(1 1) #2A((0 0 0) (0 1 2) (0 4 5))) #(#(1 0) #2A((0 0 0) (1 2 3) (4 5 6)))
                      #(#(1 -1) #2A((0 0 0) (2 3 0) (5 6 0))))
                    (#(#(0 1) #2A((0 1 2) (0 4 5) (0 7 8))) #(#(0 0) #2A((1 2 3) (4 5 6) (7 8 9)))
                      #(#(0 -1) #2A((2 3 0) (5 6 0) (8 9 0))))
                    (#(#(-1 1) #2A((0 4 5) (0 7 8) (0 0 0))) #(#(-1 0) #2A((4 5 6) (7 8 9) (0 0 0)))
                      #(#(-1 -1) #2A((5 6 0) (8 9 0) (0 0 0))))))
            (is ",∘⊂⌺3 3⊢3 3⍴⍳9" #3A(((1 1 #2A((0 0 0) (0 1 2) (0 4 5))) (1 0 #2A((0 0 0) (1 2 3) (4 5 6)))
                                      (1 -1 #2A((0 0 0) (2 3 0) (5 6 0))))
                                     ((0 1 #2A((0 1 2) (0 4 5) (0 7 8))) (0 0 #2A((1 2 3) (4 5 6) (7 8 9)))
                                      (0 -1 #2A((2 3 0) (5 6 0) (8 9 0))))
                                     ((-1 1 #2A((0 4 5) (0 7 8) (0 0 0))) (-1 0 #2A((4 5 6) (7 8 9) (0 0 0)))
                                      (-1 -1 #2A((5 6 0) (8 9 0) (0 0 0))))))
            (is "+⌺(⍪6 2)⍳8" #2A((2 2 3 4 5 6) (1 2 3 4 5 6) (3 4 5 6 7 8) (3 4 5 6 -2 -2)))
            (is ",⌺5⊢⍳6" #2A((2 0 0 1 2 3) (1 0 1 2 3 4) (0 1 2 3 4 5)
                             (0 2 3 4 5 6) (-1 3 4 5 6 0) (-2 4 5 6 0 0)))
            (is "+/,{+/,⍵}⌺3 3⊢6 5 ⍴ ⍳5" 624)
            (is "⊢⌺3 3⊢6 5⍴⍳5"
                #4A((((0 0 0) (0 1 2) (0 1 2)) ((0 0 0) (1 2 3) (1 2 3)) ((0 0 0) (2 3 4) (2 3 4))
                     ((0 0 0) (3 4 5) (3 4 5)) ((0 0 0) (4 5 0) (4 5 0)))
                    (((0 1 2) (0 1 2) (0 1 2)) ((1 2 3) (1 2 3) (1 2 3)) ((2 3 4) (2 3 4) (2 3 4))
                     ((3 4 5) (3 4 5) (3 4 5)) ((4 5 0) (4 5 0) (4 5 0)))
                    (((0 1 2) (0 1 2) (0 1 2)) ((1 2 3) (1 2 3) (1 2 3)) ((2 3 4) (2 3 4) (2 3 4))
                     ((3 4 5) (3 4 5) (3 4 5)) ((4 5 0) (4 5 0) (4 5 0)))
                    (((0 1 2) (0 1 2) (0 1 2)) ((1 2 3) (1 2 3) (1 2 3)) ((2 3 4) (2 3 4) (2 3 4))
                     ((3 4 5) (3 4 5) (3 4 5)) ((4 5 0) (4 5 0) (4 5 0)))
                    (((0 1 2) (0 1 2) (0 1 2)) ((1 2 3) (1 2 3) (1 2 3)) ((2 3 4) (2 3 4) (2 3 4))
                     ((3 4 5) (3 4 5) (3 4 5)) ((4 5 0) (4 5 0) (4 5 0)))
                    (((0 1 2) (0 1 2) (0 0 0)) ((1 2 3) (1 2 3) (0 0 0)) ((2 3 4) (2 3 4) (0 0 0))
                     ((3 4 5) (3 4 5) (0 0 0)) ((4 5 0) (4 5 0) (0 0 0))))))))
 
 (statements
  (with (:name :lexical-statements)
        (:tests-profile :title "Statement Tests")
        (:demo-profile :title "Statements Demos"
                       :description "Statement description goes here."))
  ($ (has :title "If")
     (unitary (lambda (axes) (cons 'apl-if axes)))
     (tests (is "$[1;2;3]" 2)
            (is "$[0;2;3]" 3)
            (is "x←5 ⋄ y←3 ⋄ $[y>2;x+←10;x+←20] ⋄ x" 15)
            (is "3+$[5>6;1;7>8;2;3]" 6)
            (is "{⍵+5}⍣$[3>2;4;5]⊢2" 22)
            (is "{$[⍵>5;G←3⋄H←5⋄G+H;C←8⋄D←2⋄C×D]}¨3 7" #(16 8))
            (is "{$[⍵<3;5;e←⍵+2⋄-{⍺⍺ ⍵} e]}¨⍳9" #(5 5 -5 -6 -7 -8 -9 -10 -11))))
  (⍢ (has :title "Variant") ;; TODO: implement this as a symbol since its use is implicit?
     (unitary (lambda (axes) (cons 'function-variant axes))))
  (\: (has :title "Guard Indicator")
      (symbolic :guard-indicator)))

 ;; tests for general language functions not associated with a particular function or operator
 (test-set
  (with (:name :general-tests)
        (:tests-profile :title "General Tests")
        (:demo-profile :title "General Demos"
                       :description "These are demos of basic April language features."))
  (for "Scalar value." "5" 5)
  (for "Array value." "1 2 3" #(1 2 3))
  (for "String value." "'abcde'" "abcde")
  (for "Empty string value." "''" "")
  (for "Scalar values operated upon." "3×3" 9)
  (for "Array and scalar values operated upon." "5+1 2 3" #(6 7 8))
  (for "Two array values operated upon." "4 12 16÷2 3 4" #(2 4 4))
  (for "Monadic operation upon nested vectors." "-(1 2 3)(4 5 6)" #(#(-1 -2 -3) #(-4 -5 -6)))
  (for "Dyadic operation upon nested vectors."
       "((1 2 3)(4 5 6))×(7 8 9)(10 11 12)" #(#(7 16 27) #(40 55 72)))
  (for "Scalar operation with axes on arrays of differing ranks over zero."
       "1 2 3+[1]3 4⍴⍳9" #2A((2 3 4 5) (7 8 9 10) (12 4 5 6)))
  (for "As above on the second axis." "1 2 3 4+[2]3 4⍴⍳9" #2A((2 4 6 8) (6 8 10 12) (10 3 5 7)))
  (for "Arithmetic with scalar and high-rank unitary array." "3+1 1 1 1⍴4" #4A((((7)))))
  (for "Boolean operation with vector of left arguments and enclosed vector on the right."
       "3 4=⊂3 4 5" #(#(1 0 0) #(0 1 0)))
  (for "Value assigned to a variable." "x←9" 9)
  (for "Value assigned to a variable and operated upon." "3+x←9" 12)
  (for "Two statements on one line separated by a [⋄ diamond] character."
       "a←9 ⋄ a×2 3 4" #(18 27 36))
  (for "Quote marks in string escaped using traditional double-quote method." "'''abc'''" "'abc'")
  (for "Basic function definition and use, with comments delineated by the [⍝ lamp] character."
       "⍝ This code starts with a comment.
    f1←{⍵+3} ⋄ f2←{⍵×2} ⍝ A comment after the functions are defined.
    ⍝ This is another comment.
    v←⍳3 ⋄ f2 f1 v,4 5"
       #(8 10 12 14 16))
  (for "One-line assignment followed by comment." "vc←1 2 3 ⍝ Comment follows." #(1 2 3))
  (for "Monadic inline function." "{⍵+3} 3 4 5" #(6 7 8))
  (for "Dyadic inline function." "1 2 3 {⍺×⍵+3} 3 4 5" #(6 14 24))
  (for "Vector of input variables and discrete values processed within a function."
       "fn←{3+⍵} ⋄ {fn 8 ⍵} 9" #(11 12))
  (for "Definition and use of n-argument function."
       "fn←{[x;y;z] x+y×z} ⋄ fn[4;5;6]" 34)
  (for "Inline n-argument function."
       "{[a;b;c;d](a-c)×b/d}[7;4;2;⍳3]" #(5 5 5 5 10 10 10 10 15 15 15 15))
  (for "Inline function containing lateral composition." "2 {⍺/¨⍵} 22 33" #(#(22 22) #(33 33)))
  (for "Function using default [⍺ left argument] assignment." "{⍺←3 ⋄ ⍵×⍺} 10" 30)
  (for "String operation involving mixed quoted and unquoted parentheses."
       "('(','asdf')⍳'('" 1)
  (for "Variable-referenced values, including an element within an array, in a vector."
       "a←9 ⋄ b←2 3 4⍴⍳9 ⋄ 1 2 a 3 (b[1;2;1])" #(1 2 9 3 5))
  (for "Index of inline vector." "5 6 7 8[2]" 6)
  (for "Index of inline vector starting with index of another inline vector."
       "a←9 10 11 ⋄ 1 2 a[2] 3 4 5 6[3]" 4)
  (for "Index of inline nested vector." "(1 2 3 4) 15[1]" #0A#(1 2 3 4))
  (for "Manifold left-associative indexing." "'a' 2[1] 2[1] 2[1] 2[1] 2[1]" #\a)
  (for "Indexed item in vector." "la←5 6 7 ⋄ la[1 3] 'apple'" #(#(5 7) "apple"))
  (for "Function taking an index applied to indices of vector."
       "↓[1]'abcde'[2 2⍴2 3]" #("bb" "cc"))
  (for "Index of vector of strings." "'abc' 'def' 'ghi'[2]" #0A"def")
  (for "Indexing with empty vectors to create n-dimensional empty arrays."
       "a←3 4⍴⍳12 ⋄ ⍴a[⍬;]" #(0 4))
  (for "Indexing with variables." "x←3 3⍴⍳9 ⋄ y←1 ⋄ x[;y]" #(1 4 7))
  (for "Application of functions to indexed array elements."
       "gg←2 3 4 5 ⋄ 9,gg[2],3 4" #(9 3 3 4))
  (for "Assignment of an element within an array."
       "a←2 3⍴⍳9 ⋄ a[1;2]←20 ⋄ a" #2A((1 20 3) (4 5 6)))
  (for "Assignment of enclosed array to multiple indices of an array."
       "a←⍳9 ⋄ a[3 6]←⊂9 8 ⋄ a" #(1 2 #(9 8) 4 5 #(9 8) 7 8 9))
  (for "Assignment to copy of an array." "a←3 3⍴⍳9 ⋄ b←a ⋄ b[1;]←0 ⋄ a,b"
       #2A((1 2 3 0 0 0) (4 5 6 4 5 6) (7 8 9 7 8 9)))
  (for "Strand assignment of variables including a system variable."
       "(x ⎕IO y)←10 0 2 ⋄ x+y×⍳5" #(10 12 14 16 18))
  (for "Strand assignment of nested scalar variable."
       "⎕IO←1 ⋄ (a b c)←⊂3 3⍴1 ⋄ ⊃+/a b c" #2A((3 3 3) (3 3 3) (3 3 3)))
  (for "Strand assignment of variables without parentheses." "a b c←4 5 6 ⋄ a×b,c" #(20 24))
  (for "Strand assignment with nesting." "cc (dd ee)←7 (8 9) ⋄ dd⍴cc×ee" #(63 63 63 63 63 63 63 63))
  (for "Assignment of axis-selected element within inline function."
       "{m←'+∘×'[2],⍵ ⋄ ↓m} 3 3⍴'ab'" #("∘aba" "∘bab" "∘aba"))
  (for "Selection from an array with multiple elided dimensions."
       "(2 3 3 4 5⍴⍳9)[2;;3;;2]" #2A((6 2 7 3) (3 8 4 9) (9 5 1 6)))
  (for "Selection from an array with multi-index, array and elided dimensions."
       "(3 3 3⍴⍳27)[1 2;2 2⍴⍳3;]" #4A((((1 2 3) (4 5 6)) ((7 8 9) (1 2 3)))
                                      (((10 11 12) (13 14 15)) ((16 17 18) (10 11 12)))))
  (for "Selection from within an array with spaces in axis specification."
       "(3 4⍴⍳12)[ ; 4 3 ]" #2A((4 3) (8 7) (12 11)))
  (for "Elided assignment."
       "a←2 3 4⍴⍳9 ⋄ a[2;;3]←0 ⋄ a" #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3)) ((4 5 0 7) (8 9 0 2) (3 4 0 6))))
  (for "Another elided assignment." "a←2 3 4⍴⍳40 ⋄ a[1;;]←3 4⍴0 ⋄ a"
       #3A(((0 0 0 0) (0 0 0 0) (0 0 0 0)) ((13 14 15 16) (17 18 19 20) (21 22 23 24))))
  (for "Assignment from an array to an area of an array with the same shape."
       "x←8 8⍴0 ⋄ x[2+⍳3;3+⍳4]←3 4⍴⍳9 ⋄ x" #2A((0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 1 2 3 4 0)
                                               (0 0 0 5 6 7 8 0) (0 0 0 9 1 2 3 0) (0 0 0 0 0 0 0 0)
                                                                 (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0)))
  (for "Elided assignment of a matrix of values." "gg←5 5⍴⍳9 ⋄ gg[2 5;]←2 5⍴0 1 ⋄ gg"
       #2A((1 2 3 4 5) (0 1 0 1 0) (2 3 4 5 6) (7 8 9 1 2) (1 0 1 0 1)))
  (for "Elision and indexed array elements."
       "(6 8⍴⍳9)[1 4;]" #2A((1 2 3 4 5 6 7 8) (7 8 9 1 2 3 4 5)))
  (for "As above but more complex."
       "(6 8 5⍴⍳9)[1 4;;2 1]" #3A(((2 1) (7 6) (3 2) (8 7) (4 3) (9 8) (5 4) (1 9))
                                  ((5 4) (1 9) (6 5) (2 1) (7 6) (3 2) (8 7) (4 3))))
  (for "Indices of indices."
       "(6 8 5⍴⍳9)[1 4;;2 1][1;2 4 5;]" #2A((7 6) (8 7) (4 3)))
  (for "Array as array index."
       "(10+⍳9)[2 3⍴⍳9]" #2A((11 12 13) (14 15 16)))
  (for "Sub-coordinates of nested arrays." "(3 4⍴⍳9)[(1 2)(3 1)]" #(2 9))
  (for "Choose indexing of nested array sub-coordinates."
       "(3 4⍴⍳9)[2 2⍴⊂(2 3)]" #2A((7 7) (7 7)))
  (for "Reach indexing of components within sub-vectors."
       "(('JAN' 1)('FEB' 2)('MAR' 3)('APR' 4)('MAY' 5)('JUN' 6))[(2 1)(1 2)]" #(#0A"FEB" 1))
  (for "Reach indexing of components within sub-arrays."
       "(2 3⍴('JAN' 1)('FEB' 2)('MAR' 3)('APR' 4)('MAY' 5)('JUN' 6))[((2 3)1)((1 1)2)]" #(#0A"JUN" 1))
  (for "Reach indexing assignment."
       "toasn←(('JAN' 1)('FEB' 2)('MAR' 3)('APR' 4)('MAY' 5)('JUN' 6)) ⋄ toasn[(2 1)(1 2)]←45 67 ⋄ toasn"
       #(#("JAN" 45) #(67 2) #("MAR" 3) #("APR" 4) #("MAY" 5) #("JUN" 6)))
  (for "Reach indexing assignment compared to duplicate index assignment."
       "{n←v←3/⊂5⍴0 ⋄ n[(1 1)]←⍵ ⋄ v[(⊂1 1)]←⍵ ⋄ n,v} 5" #(5 #*00000 #*00000 #(5 0 0 0 0) #*00000 #*00000))
  (for "Creation of empty array by passing empty vectors as indices." "(⍳3)[⍬]" #())
  (for "As above with multiple dimensions." "(⍴(5 5 5⍴1)[;⍬;2 3]),⍴(3 3⍴1)[⍬;]" #(5 0 2 0 3))
  (for "Assignment by function." "a←3 2 1 ⋄ a+←5 ⋄ a" #(8 7 6))
  (for "Assignment by function at index." "a←3 2 1 ⋄ a[2]+←5 ⋄ a" #(3 7 1))
  (for "Elided assignment of applied function's results."
       "a←2 3 4⍴⍳9 ⋄ a[2;;3]+←10 ⋄ a" #3A(((1 2 3 4) (5 6 7 8) (9 1 2 3)) ((4 5 16 7) (8 9 11 2) (3 4 15 6))))
  (for "Assignment by function result of array after after function applied."
       "q←3×1-⍨3 3⍴⍳4 ⋄ p←4 4⍴1 ⋄ p[⍳3;⍳3]×←q=0 ⋄ p" #2A((1 0 0 1) (0 1 0 1) (0 0 1 1) (1 1 1 1)))
  (for "Operation over portions of an array."
       "a←4 8⍴⍳9 ⋄ a[2 4;1 6 7 8]+←10 ⋄ a" #2A((1 2 3 4 5 6 7 8) (19 1 2 3 4 15 16 17)
                                               (8 9 1 2 3 4 5 6) (17 8 9 1 2 13 14 15)))
  (for "Assignment of array element referenced by [⌷ index] function."
       "x←3 3⍴⍳9 ⋄ (2 3⌷x)←33 ⋄ x" #2A((1 2 3) (4 5 33) (7 8 9)))
  (for "Assignment of array element referenced by [⌷ index] function to different type."
       "x←3 3⍴⍳9 ⋄ (1 2⌷x)←'a' ⋄ x" #2A((1 #\a 3) (4 5 6) (7 8 9)))
  (for "Selective assignment of vector portion to value by [↑ take] function."
       "x←⍳8 ⋄ (3↑x)←20 ⋄ x" #(20 20 20 4 5 6 7 8))
  (for "Selective assignment of vector portion to sub-vector by [↑ take] function."
       "x←⍳8 ⋄ (3↑x)←20 21 22 ⋄ x" #(20 21 22 4 5 6 7 8))
  (for "Selective assignment of matrix portion to value by [↓ drop] function."
       "x←4 5⍴⍳20 ⋄ (2 3↓x)←0 ⋄ x" #2A((1 2 3 4 5) (6 7 8 9 10) (11 12 13 0 0) (16 17 18 0 0)))
  (for "Selective assignment of matrix portion to sub-matrix by [↓ drop] function."
       "x←4 5⍴⍳20 ⋄ (2 3↓x)←2 2⍴-⍳4 ⋄ x" #2A((1 2 3 4 5) (6 7 8 9 10) (11 12 13 -1 -2) (16 17 18 -3 -4)))
  (for "Selective assignment of vector portion by successive [↓ drop] functions."
       "x←⍳9 ⋄ (2↑4↓x)←99 ⋄ x" #(1 2 3 4 99 99 7 8 9))
  (for "Selective assignment of matrix element by [⊃ pick] function."
       "x←3 4⍴⍳12 ⋄ ((⊂2 3)⊃x)←50 ⋄ x" #2A((1 2 3 4) (5 6 50 8) (9 10 11 12)))
  (for "Selective assignment of an array by [⊃ pick] function."
       "x←⍳4 ⋄ (⊃x)←2 2⍴⍳4 ⋄ x" #(#2A((1 2) (3 4)) 2 3 4))
  (for "Selective assignment of array elements by [/ compress] function."
       "x←6 8⍴⍳9 ⋄ ((30>+⌿x)/x)←0 ⋄ x" #2A((1 2 3 0 0 0 0 8) (9 1 2 0 0 0 0 7) (8 9 1 0 0 0 0 6)
                                           (7 8 9 0 0 0 0 5) (6 7 8 0 0 0 0 4) (5 6 7 0 0 0 0 3)))
  (for "Selective assignment of array elements by [/ compress] function."
       "x←6 8⍴⍳9 ⋄ ((30>+⌿x)/x)←6 4⍴10×⍳3 ⋄ x" #2A((1 2 3 10 20 30 10 8) (9 1 2 20 30 10 20 7)
                                                   (8 9 1 30 10 20 30 6) (7 8 9 10 20 30 10 5)
                                                   (6 7 8 20 30 10 20 4) (5 6 7 30 10 20 30 3)))
  (for "Selective assignment of elements within nested array by [↑ take] function."
       "{na←3⍴⊂⍳4 ⋄ (1↑na[1])←⍵ ⋄ na} 99" #(99 #(1 2 3 4) #(1 2 3 4)))
  (for "Selective assignment of elements within nested array by [⊃ pick] function."
       "{na←3⍴⊂⍳4 ⋄ (1↑⊃na[1])←⍵ ⋄ na} 99" #(#(99 2 3 4) #(1 2 3 4) #(1 2 3 4)))
  (for "Selective assignment of matrix elements by [⍉ transpose] function."
       "{mt←3 3⍴⍳9 ⋄ (1 1⍉mt)←⍵ ⋄ mt} 0" #2A((0 2 3) (4 0 6) (7 8 0)))
  (for "Selective assignment of vector to matrix elements by [⍉ transpose] function."
       "{mt←3 3⍴⍳9 ⋄ (1 1⍉mt)←⍵ ⋄ mt} 10 20 30" #2A((10 2 3) (4 20 6) (7 8 30)))
  (for "Selective assignment of matrix elements raveled by [↑ take] function."
       "{mt←3 4⍴⍳12 ⋄ (5↑,mt)←⍵ ⋄ mt} 0" #2A((0 0 0 0) (0 6 7 8) (9 10 11 12)))
  (for "Selective assignment of vector elements by take of sub-indices."
       "{mt←⍳20 ⋄ (3↑mt[2×⍳8])←⍵ ⋄ mt} 99" #(1 99 3 99 5 99 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
  (for "Selective assignment of nested character vector elements enlisted by [/ compress] function."
       "'a' {names←'Kent' 'Alan' 'Ryan' ⋄ ((⍺=∊names)/∊names)←⍵ ⋄ names} '*'"
       #("Kent" "Al*n" "Ry*n"))
  (for "Selective assignment of string elements within string applied by [/ compress] function."
       "{A←'STELLAR' ⋄ ((A∊'AEIOU')/A)←⍵ ⋄ A} '*'" #(#\S #\T #\* #\L #\L #\* #\R))
  (for "Multiple assignment with selective assignment in midstream."
       "a←⍳5 ⋄ b←(3⊃a)←30 ⋄ a b" #(#(1 2 30 4 5) 30))
  (for "Selective assignment with [¨ each]-composed [↑ take] function."
       "{A←'RANDOM' 'CHANCE' ⋄ (2↑¨A)←⍵ ⋄ A} '*'"
       #(#(#\* #\* #\N #\D #\O #\M) #(#\* #\* #\A #\N #\C #\E)))
  (for "Selective assignment with [¨ each]-composed [/ compress] function."
       "{A←'RANDOM' 'CHANCE' ⋄ ((A='A')/¨A)←⍵ ⋄ A} '*'"
       #(#(#\R #\* #\N #\D #\O #\M) #(#\C #\H #\* #\N #\C #\E)))
  (for "Selective assignment with [¨ each]-composed [⊃ pick] function."
       "{A←'RANDOM' 'CHANCE' ⋄ ((A∊¨⊂'ND')/¨A)←⍵ ⋄ A} '*'"
       #(#(#\R #\A #\* #\* #\O #\M) #(#\C #\H #\A #\* #\C #\E)))
  (for "Selective assignment with bracket indexing of array to be assigned to."
       "{A←4 3⍴'RANDOM' 'CHANCE' ⋄ (¯2↑¨A[;1 3])←⍵ ⋄ ⍕¨A} '*'"
       #2A(("RAND**" "CHANCE" "RAND**")
           ("CHAN**" "RANDOM" "CHAN**")
           ("RAND**" "CHANCE" "RAND**")
           ("CHAN**" "RANDOM" "CHAN**")))
  (for "Selective assignment using aliased [⌷ index] function."
       "{e←⍳⍵ ⋄ g←⌷ ⋄ (3 g e)←5 ⋄ e} 9" #(1 2 5 4 5 6 7 8 9))
  (for "Print the result of a function applied to assignment." "⎕←⍴x←1 2 3 ⋄ x" #(1 2 3))
  (for "Assignment of dynamic variable within function."
       "aa←3 ⋄ bob←{aa+←⍵ ⋄ aa} ⋄ bob 5" 8)
  (for "Creation of lexical variable unchanged by reassignment within a sub-function's scope."
       "{gg←1 ⋄ {gg←⍵}¨⍳⍵ ⋄ gg} 5" 1)
  (for "Index of variable with value assigned inside its own index."
       "y[⍋y←1 8 4 2]" #(1 2 4 8))
  (for "Alias of [× multiply], [⍴ shape] and [⍳ index] functions."
       "⊃,/{m←× ⋄ s←⍴ ⋄ i←⍳ ⋄ 5 m 2 3 s i ⍵}¨2 6" #2A((5 10 5 5 10 15) (10 5 10 20 25 30)))
  (for "Alias of [enclose ⊂] function with curried axes."
       "{ea←⊂[2] ⋄ ea ⍵} 2 3 4⍴⍳9" #2A((#(1 5 9) #(2 6 1) #(3 7 2) #(4 8 3))
                                       (#(4 8 3) #(5 9 4) #(6 1 5) #(7 2 6))))
  (for "Inline pivotal operation-derived function expression."
       "1 2 3 (∘.+) 4 5 6" #2A((5 6 7) (6 7 8) (7 8 9)))
  (for "Composed pivotal operation-derived function expression."
       "1 2 3∘(×.+)⊢4 5 6" 315)
  (for "Multiple composed pivotal operations called in sequence."
       "(4 5 6∘(∘.×)) (1 2 3∘(∘.+)) 10 20 30"
       #3A(((44 84 124) (48 88 128) (52 92 132))
           ((55 105 155) (60 110 160) (65 115 165))
           ((66 126 186) (72 132 192) (78 138 198))))
  (for "Pivotal composition of overloaded function, further composed laterally."
       "(1 0 1∘/)¨⍳3" #(#*11 #(2 2) #(3 3)))
  (for "Pivotal composition composed across a vector followed by another lateral composition."
       "+/×⍤1¨1 0 ¯1" 0)
  (for "Pivotal composition with another pivotal composition preceding right operand within defn."
       "{⊢⍤1(⊢⍤1)⍵}⍳3" #(1 2 3))
  (for "Multiple operator compositions in sequence." "1 0 {,¨+⌿×-⍵,.-⍺} 2 2⍴0 0 1 1" #0A#(1 -1))
  (for "Operator composition calling accumulating function."
       "{acm←⍬ ⋄ {acm,←⊃,/⍵ ⋄ ⌽¯1↓⍵}⍣⍵⊢⍳⍵ ⋄ acm} 5" #(1 2 3 4 5 4 3 2 1 2 3 4 3 2 3))
  (for "Operator composition involving lexical variables within function."
       "{next←⊂2 4 7 9 ⋄ back←⊃,/1+0×next ⋄ back@(⊃,/next)⊢⍵} 1 0 0 1 0 2 0 1 2 0 2 3 1 2 0"
       #(1 1 0 1 0 2 1 1 1 0 2 3 1 2 0))
  (for "Two-element monadic atop function train." "(↓⌽)4 5⍴⍳20"
       #(#(5 4 3 2 1) #(10 9 8 7 6) #(15 14 13 12 11) #(20 19 18 17 16)))
  (for "Two-element dyadic atop function train." "'mississippi'(⍸∊)'sp'" #(3 4 6 7 9 10))
  (for "Two-element monadic atop function train of lateral compositions assigned as function."
       "{ ee ← +/ {5⍴⍵}¨ ⋄ ee ⍵} ⍳9" #0A#(45 45 45 45 45))
  (for "Basic three-element monadic fork function train." "(-,÷)5" #(-5 1/5))
  (for "Three-element monadic fork function train with inline function." "(+ {⍺×⍵} -)5" -25)
  (for "Three-element monadic fork function train with variable-referenced and inline functions."
       "f←{⍺×⍵} ⋄ ({3+⍵} f -)5" -40)
  (for "Three-element fork function train with referenced functions at ends."
       "of←{⍵+1} ⋄ ef←{⍵+2} ⋄ (of , ef) 5" #(6 7))
  (for "Three-element averaging fork function train with operator-derived function."
       "(+/÷≢)⍳12" 13/2)
  (for "Five-element monadic fork function train." "(!⍴-,÷)3" #(-3 1/3 -3 1/3 -3 1/3))
  (for "Five-element monadic fork function train with second argument value at end."
       "(3 5⍴-,÷)5" #2A((-5 1/5 -5 1/5 -5) (1/5 -5 1/5 -5 1/5) (-5 1/5 -5 1/5 -5)))
  (for "As above but assigned as a function." "{gg←!⍴-,÷ ⋄ 3 gg ⍵} 5" #(-2 3/5 -2 3/5 -2 3/5 -2 3/5 -2 3/5))
  (for "Three-element dyadic fork function train."
       "' ' (≠⊆⊢) ' one two  three'" #("one" "two" "three"))
  (for "Three-element dyadic fork function train with left argument value."
       "(⍳8) (12>+) (⍳8)⋆1.2" #(1 1 1 1 1 0 0 0))
  (for "Three-element monadic fork function train including operator-composed function."
       "(1+-∘÷) 4" 3/4)
  (for "Five-element dyadic fork function train."
       "' ' (∊{⍺,⍵[⍺],⍵}≠⊆⊢) ' one two  three'" #(1 "one" "one" "two" "three"))
  (for "Five-element monadic fork function train including lateral and pivotal function compositions."
       "(⊢⌽⍨(-⎕IO)+⍳∘≢)5 5⍴⍳25"
       #2A((1 2 3 4 5) (7 8 9 10 6) (13 14 15 11 12) (19 20 16 17 18) (25 21 22 23 24)))
  (for "Three-element function train with composition as middle element."
       "(≠(⊢⍤/)⊢) 1 2 3 3 2 4" #(1 2 3 4))
  (for "More complex three-element train with sub-compositions."
       "(⍳∘≢(∘.⌷)⊂) 2 3 3⍴⍳18" #(#2A((1 2 3) (4 5 6) (7 8 9)) #2A((10 11 12) (13 14 15) (16 17 18))))
  (for "Function train including ⍥-composition." "∧/2(=⍥≢)/('abcd' 'abcd' 'abcd' 'abcd')" 1)
  (for "Recursive function." "refn←{A←⍵-1 ⋄ $[A≥0;A,refn A;0]} ⋄ refn 5" #(4 3 2 1 0 0))
  (for "Lateral operator definition." "lop←{8 ⍺⍺ 5×2+⍵} ⋄ × lop 5" 280)
  (for "Lateral operator defined and used within a funciton."
       "lop←{8 ⍺⍺ 5×2+⍵} ⋄ {× lop ⍵} 5" 280)
  (for "Simple inline lateral operator." "+{⍵⍵ ⍺⍺/⍵}+⊢3" 3)
  (for "Pivotal operator definition." "pop←{(⍵ ⍵⍵ ⍺) ⍺⍺ (⍺ ⍵⍵ ⍵)} ⋄ 2-pop≤⊢3" -1)
  (for "Pivotal operator defined and used within a function."
       "pop←{(⍵ ⍵⍵ ⍺) ⍺⍺ (⍺ ⍵⍵ ⍵)} ⋄ {2-pop≤⊢⍵} 3" -1)
  (for "Lateral recursive operator definition with reference to composed function."
       "rlop←{$[⍵<2000;⍵,∇ 3 ⍺⍺ 2×2+⊃⍵;⍵]} ⋄ × rlop 5" #(5 42 264 1596 9588))
  (for "Lateral recursive operator definition with self-reference."
       "rlop←{$[⍵<2000;⍵,⍺⍺ ∇∇ 3 ⍺⍺ 2×2+⊃⍵;⍵]} ⋄ × rlop 5" #(5 42 264 1596 9588))
  (for "Inline lateral operator." "× {8 ⍺⍺ 5×2+⍵} 5" 280)
  (for "Inline pivotal operator." "2-{(⍵ ⍵⍵ ⍺) ⍺⍺ (⍺ ⍵⍵ ⍵)}≤⊢3" -1)
  (for "Inline lateral operator with left argument." "3 +{⍺ ⍺⍺ ⍵} 4" 7)
  (for "Inline pivotal operator with unused left operand." "3 +{⍺ ⍵⍵ ⍵}× 4" 12)
  (for "Function applied to result of pivotal operator." "∊∘.+⍨10 2" #(20 12 12 4))
  (for "Function applied to invocation of train composition including operator."
       "⍴(+/⊢⌺3 3) 2 2⍴255" #(2 2 3))
  (for "As previous but with more complex train." "⍴⍴∘+/3 3⍴1 2 3" #(3))
  (for "Another variation of the prior train." "⍴(⍴∘+/⊢)⌺3 3⊢2 2⍴255" #(2 2 3))
  (for "Two-element train including reduction composition." "(≠/⊢) 1 2 3 3 2 4" 0)
  (for "Atop train including operator composition." "0 1 2 3 4 5 6 7 (⍳∘1>) 4" 6)
  (for "Fork spelling of previous." "0 1 2 3 4 5 6 7 (1⍳⍨>) 4" 6)
  (for "Train including [⍤ atop] composition." "(≠⊢⍤/⊢) 1 2 3 3 2 4" #(1 2 3 4))
  (for "Three-element train containing function with axes applied."
       "(-,[0.5]÷) 1 2 3" #2A((-1 -2 -3) (1 1/2 1/3)))
  (for "Lateral operator within a defined function." "fn←{÷ {⍺⍺ ⍵} 1+⍵} ⋄ - fn 2" -1/3)
  (for "Inline pivotal operator in parentheses with internal ⋄ breaks." "3 (+{⍺⍺ 2 ⋄ ⍺ ⍵⍵ ⍵}÷) 4" 3/4)
  (for "Inline lateral operator used with single-character function-referring operand."
       "'*' {⍶,⍵} ' b c d'" "* b c d")
  (for "Operator composition of function within named operator."
       "filter←{(⍺⍺¨⍵)/⍵} ⋄ {2|⍵} filter ⍳20" #(1 3 5 7 9 11 13 15 17 19))
  (for "Operator composition of function within inline operator."
       "{2|⍵} {(⍺⍺¨⍵)/⍵} ⍳20" #(1 3 5 7 9 11 13 15 17 19))
  (for "Defined lateral operator with value passed as operand."
       "(2{⍵+⍶}7),3{⍶+⍵}5" #(9 8))
  (for "Defined lateral operator with value passed as operand positioned in vector."
       "3{1 2 ⍶ 4 5×⍵}9" #(9 18 27 36 45))
  (for "Lateral operator with variable operand defined and used within function."
       "(⍳3) {q←{⍶+⍺×⍵} ⋄ ⍵(3 q)6⊣¨⍺} 5" #(33 33 33))
  (for "More complex lateral operator use within function."
       "{ ee←{↑⍪/(⊂⍺),⍶,⊂⍵} ⋄ ⍵⊃⊃↑{⍺ ee⌿⍵}/9⍴⊂⍳9 } 22" 1)
  (for "Simplification of above lateral operator mechanic."
       "{,/⍵}/3⍴⊂⍳3" #0A#0A#(1 2 3))
  (for "Compose operator composition within defined lateral operator."
       "÷{⍺⍺∘⌽⍵}⍳9" #(1/9 1/8 1/7 1/6 1/5 1/4 1/3 1/2 1))
  (for "Pivotal inline operator containing variable function/value ⍺ assignment."
       "2 ↑{⍺←⊢ ⋄ b←⍵ ⋄ (⍺ ⍺⍺ b)←⍵⍵ ⍺ ⍺⍺ b ⋄ b}⌽ ⍳3" #(2 1 3))
  (for "Lexically scoped function defined and used within defined lateral operator."
       ",{op←⍺⍺ ⋄ ⊃op{(⊂⍺ op⊃⍬⍴⍵),⍵}/1↓{⍵,⊂⍬⍴⍵}¯1⌽⍵}⍳4" #(#(1 2 3 4) #(2 3 4) #(3 4) 4))
  (for "As above with different left operand."
       "+{op←⍺⍺ ⋄ ⊃op{(⊂⍺ op⊃⍬⍴⍵),⍵}/1↓{⍵,⊂⍬⍴⍵}¯1⌽⍵}⍳4" #(10 9 7 4))
  (for "Array processing function applied over nested array."
       "{((5=¯1↑⍵)+1)⊃¯1 (⊂⍵)}¨(⊂1 5),⍨3⍴⊂⍳4" #(-1 -1 -1 #0A#(1 5)))
  (for "Indexed element of above array."
       "{⍵,≡⍵}4⌷{((5=¯1↑⍵)+1)⊃¯1 (⊂⍵)}¨(⊂1 5),⍨3⍴⊂⍳4" #(#0A#(1 5) 3))
  (for "Fibonacci sequence generated using [∇ self] for self-reference within a function."
       "{(⍵=1)∨⍵=2 : 1 ⋄ (∇ ⍵-2)+∇ ⍵-1}¨⍳12" #(1 1 2 3 5 8 13 21 34 55 89 144))
  (for "Locally-scoped function used with lateral operator within if-statement."
       "(⍳3){ g←{5+⍵} ⋄ b←-∘5 ⋄ h←{12×$[~2|⍺;b¨⍵;g ⍵]} ⋄ ⍺ h¨⍵} (⍳3)+3⍴⊂⍳3"
       #(#(84 96 108) #(-24 -12 0) #(108 120 132)))
  (for "Locally-scoped function used with function-overloaded lateral operator within if-statement."
       "(⍳3){ g←{5+⍵} ⋄ b←-∘× ⋄ h←{12×$[~2|⍺;b/⍵;g ⍵]} ⋄ ⍺ h¨⍵} (⍳3)+3⍴⊂⍳3"
       #(#(84 96 108) 24 #(108 120 132)))
  (for "Locally-scoped function used with pivotal operator within if-statement."
       "(⍳3){ g←{⍵×⍺-2} ⋄ b←{⍺×⍵÷3} ⋄ h←{12×$[~2|⍺;⍺ (b . g) ⍵;⍺ g ⍵]} ⋄ ⍺ h¨⍵} (⍳3)+3⍴⊂⍳3"
       #(#(-24 -36 -48) 0 #(48 60 72)))
  (for "Pivotal operator with value operands but no arguments defined and used."
       "2{⍶⋄⍹}3⊢10" 3)
  (for "Function containing multiple nested locally-scoped functions."
       "{aa←{⍵+5} ⋄ bb←{cc←{⍺,aa ⍵} ⋄ ⍺ cc ⍵} ⋄ 9 bb ⍵} 100" #(9 105))
  (for "Operator composition assigned as function with a right-value composition on the right."
       "fn←÷@(≤∘4) ⋄ fn ⍳9" #(1 1/2 1/3 1/4 5 6 7 8 9))
  (for "Dynamic aliasing of operator at top level." "key←⌸ ⋄ {(2|⍳≢⍵)⊢key ⍵}10 2⍴⍳20"
       #3A(((1 2) (5 6) (9 10) (13 14) (17 18)) ((3 4) (7 8) (11 12) (15 16) (19 20))))
  (for "Aliasing of [/ reduce] operator." "{r←/ ⋄ rf←/[1] ⋄ (+rf ⍵),(+ r[1] ⍵),+ r ⍵} 3 3⍴⍳9"
       #(12 15 18 12 15 18 6 15 24))
  (for "Aliasing of [\\ scan] operator." "{s←\\ ⋄ sf←\\[1] ⋄ (+ sf ⍵),(+ s[1] ⍵),+ s ⍵} 3 4⍴⍳12"
       #2A((1 2 3 4 1 2 3 4 1 3 6 10) (6 8 10 12 6 8 10 12 5 11 18 26)
           (15 18 21 24 15 18 21 24 9 19 30 42)))
  (for "Aliasing of [¨ each] operator." "{e←¨ ⋄ ⍴ e (⍳⍵)⍴¨⊂⍳9} 3" #(#*1 #(2) #(3)))
  (for "Aliasing of [⍨ commute] operator." "{c←⍨ ⋄ +c ⍵} 35" 70)
  (for "Aliasing of [⌸ key] operator."
       "{k←⌸ ⋄ {⍴⍵}k ⍵} 'Apple' 'Orange' 'Apple' 'Pear' 'Orange' 'Peach'" #2A((2) (2) (1) (1)))
  (for "Aliasing of [. inner product] operator." "{ip←. ⋄ ⍵ +ip× 4 5 6} 1 2 3" 32)
  (for "Aliasing of [∘ beside] operator." "{c←∘ ⋄ ⍴ c ⍴ ⍵} ⍳9" #*1)
  (for "Aliasing of [⍛ before] operator." "{c←⍛ ⋄ ⍵ - c - ⍵} 1" -2)
  (for "Aliasing of [⍤ rank] operator." "{r←⍤ ⋄ ⍵+r 1⊢3 3⍴⍳9} ⍳3" #2A((2 4 6) (5 7 9) (8 10 12)))
  (for "Aliasing of [⍥ over] operator." "8 10 12 {o←⍥ ⋄ (⍺×⍵)÷o(+/)⍺} 16 32 64" 608/15)
  (for "Aliasing of [⍣ power] operator." "{p←⍣ ⋄ ⍳ p ¯1⊢⍵} ⍳9" 9)
  (for "Aliasing of [@ at] operator." "{a←@ ⋄ 22 33 a 3 5⊢⍵} ⍳9" #(1 2 22 4 33 6 7 8 9))
  (for "Aliasing of [⌺ stencil] operator." "{s←⌺ ⋄ ⊢∘⊂ s 2⊢⍵} ⍳8"
       #(#(1 2) #(2 3) #(3 4) #(4 5) #(5 6) #(6 7) #(7 8)))
  (for "Conditional aliasing of pivotal operator." "0 1 {o←⍤ ⋄ {o←⍥⋄1}⍣⍺⊢0 ⋄ 1 +o- ⍵}¨2" #(-1 -3))
  (for "Glider 1." "(3 3⍴⍳9)∊1 2 3 4 8" #2A((1 1 1) (1 0 0) (0 1 0)))
  (for "Glider 2." "3 3⍴⌽⊃∨/1 2 3 4 8=⊂⍳9" #2A((0 1 0) (0 0 1) (1 1 1)))
  (for "Quine." "1⌽,⍨9⍴'''1⌽,⍨9⍴'''" "1⌽,⍨9⍴'''1⌽,⍨9⍴'''"))

 (test-set
  (with (:name :system-variable-function-tests)
        (:tests-profile :title "System Variable and Function Tests")
        (:demo-profile :title "System Variable and Function Demos"
                       :description "Demos illustrating the use of system variables and functions."))
  (for "Setting the index origin." "a←⍳3 ⋄ ⎕io←0 ⋄ a,⍳3" #(1 2 3 0 1 2))
  (for-printed "Setting the print precision." "⎕pp←3 ⋄ ⎕io←1 ⋄ a←⍕*⍳3 ⋄ ⎕pp←6 ⋄ a,'  ',⍕*⍳3"
               "2.72 7.39 20.1  2.71828 7.38906 20.0855")
  (for "Alphabetical and numeric vectors." "⎕pp←10 ⋄ ⎕a,⎕d" "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
  (for "Seven elements in the timestamp vector." "⍴⎕ts" #(7))
  (for "Characters to unicode indices." "⎕ucs 'abcd'" #(97 98 99 100))
  (for "Unicode indices to characters." "⎕ucs 13 10" #(#\Return #\Newline))
  (for "3D array formatted as matrix." "↓⎕fmt 2 3 3⍴⍳9" #("1 2 3"
                                                          "4 5 6"
                                                          "7 8 9"
                                                          "     "
                                                          "1 2 3"
                                                          "4 5 6"
                                                          "7 8 9")))

 (test-set
  (with (:name :function-inversion-tests)
        (:tests-profile :title "Function Inversion Tests")
        (:demo-profile :title "Function Inversion Demos"
                       :description "Demos of the negative-indexed [⍣ power] operator that inverts simple functions passed to it."))
  (for "Inverse addition."       "(3+⍣¯1⊢8),(3∘+⍣¯1⊢8),+∘3⍣¯1⊢8" #(5 5 5))
  (for "Inverse subtraction."    "(3-⍣¯1⊢8),(3∘-⍣¯1⊢8),-∘3⍣¯1⊢8" #(-5 -5 11))
  (for "Inverse multiplication." "(3×⍣¯1⊢8),(3∘×⍣¯1⊢8),×∘3⍣¯1⊢8" #(8/3 8/3 8/3))
  (for "Inverse division."       "(3÷⍣¯1⊢8),(3∘÷⍣¯1⊢8),÷∘3⍣¯1⊢8" #(3/8 3/8 24))
  (for "Inverse exponents."  "⌊100×.0000001+(3⋆⍣¯1⊢8),(3∘⋆⍣¯1⊢8),⋆∘3⍣¯1⊢8" #(189 189 200))
  (for "Inverse logarithms." "⌊100×.0000001+(3⍟⍣¯1⊢8),(3∘⍟⍣¯1⊢8),⍟∘3⍣¯1⊢8" #(656100 656100 114))
  (for "Inverse monadic scalar functions." "⌊1000×(+⍣¯1⊢5),(-⍣¯1⊢5),(÷⍣¯1⊢5),(⋆⍣¯1⊢5),⍟⍣¯1⊢5"
       #(5000 -5000 200 1609 148413))
  (for "Inverse circular ops."   "{(5○⍨-⍵)=⍵∘○⍣¯1⊢5} ⍳12" #(1 1 1 1 1 1 1 1 1 1 1 1))
  (for "Inverse indexing." "⍳⍣¯1⊢1 2 3 4 5" 5)
  (for "Inverse where." "(⍸⍣¯1) 4 5 9" #*000110001)
  (for "Inverse where, right tack-separated." "⍸⍣¯1⊢4 5 9" #*000110001)
  (for "Another inverse where." "(⍸⍣¯1) (1 2) (2 3)" #2A((0 1 0) (0 0 1)))
  (for "Inverse mix."      "↑⍣¯2⊢2 3 4⍴⍳9" #(#(#(1 2 3 4) #(5 6 7 8) #(9 1 2 3))
                                             #(#(4 5 6 7) #(8 9 1 2) #(3 4 5 6))))
  (for "Inverse split."    "↓⍣¯1⊢(1 2 3) (4 5 6) (7 8 9)" #2A((1 2 3) (4 5 6) (7 8 9)))
  (for "Inverse nest."     "⊆⍣¯1⊢⍳5" #(1 2 3 4 5))
  (for "Inverse disclose." "⊃⍣¯1⊢⍳5" #0A#(1 2 3 4 5))
  (for "Inverse reversal." "⌽⍣¯1⊢⍳5" #(1 2 3 4 5))
  (for "Inverse rotation." "(2⌽⍣¯1⊢⍳5)⍪1⊖⍣¯1⊢3 5⍴⍳9" #2A((4 5 1 2 3) (2 3 4 5 6) (1 2 3 4 5) (6 7 8 9 1)))
  (for "Inverse encode."   "1760 3 12⊤⍣¯1⊢2 0 10" 82)
  (for "Inverse decode."   "1760 3 12⊥⍣¯1⊢82" #(2 0 10))
  (for "Inverse composed decode extending left argument." "(2∘⊥)⍣¯1⊢5" #(1 0 1))
  (for "Inverse composed decode extending left argument." "2⊥⍣¯1⊢0" #())
  (for "Inverse composed decode extending left argument with array as right argument."
       "(6∘⊥)⍣¯1⊢10 5 8 3" #2A((1 0 1 0) (4 5 2 3)))
  (for "Inversion of nested compound functions." "(3 +∘(2∘-)⍣1⊢5),3 +∘(2∘-)⍣¯1⊢5" #(0 0))
  (for "Celsius-Fahrenheit conversion."              "⌊(32∘+)∘(×∘1.8)⍣ 1⊢100" 212)
  (for "Inversion of Celsius-Fahrenheit conversion." "⌊(32∘+)∘(×∘1.8)⍣¯1⊢212" 100)
  (for "Inverse composed function." "(3+∘÷∘-⍣1⊢5),3+∘÷∘-⍣¯1⊢5" #(14/5 -1/2))
  (for "Inversion of scanning addition." "+\\⍣¯1⊢+\\⍳5" #(1 2 3 4 5))
  (for "Inversion of composed addition applied over each." "+∘5¨⍣¯1⊢-\\⍳5" #(-4 -6 -3 -7 -2))
  (for "Inversion of composed division applied over each." "÷∘5¨⍣¯1⊢+\\⍳5" #(5 15 30 50 75))
  (for "Double inversion of addition." "3 (+⍣¯1)⍣¯1⊢5" 8)
  (for "Commutative inversion of addition."       "+⍨⍣¯1⊢64" 32)
  (for "Commutative inversion of multiplication." "×⍨⍣¯1⊢64" 8.0)
  (for "Commutative inversion of max and min."    "(⌈⍨⍣¯1⊢64),⌊⍨⍣¯1⊢64" #(64 64))
  (for "Inversion of commuted outer product." "(∘.×∘4 5 6)⍣¯1⊢1 2 3∘.×4 5 6" #(1 2 3))
  (for "Inversion of commuted outer product, other side." "(1 2 3∘(∘.×))⍣¯1⊢1 2 3∘.×4 5 6" #(4 5 6))
  (for "More complex outer product inversion."
       "(∘.×∘4 5 6)⍣¯1⊢(∘.×∘4 5 6) (1 2 3∘(∘.+)) 10 20 30" #2A((11 21 31) (12 22 32) (13 23 33)))
  (for "Power set." "{⌿∘⍵¨↓⌽⍉2⊥⍣¯1⊢¯1+⍳2*≢⍵} 'ab'" #("" "a" "b" "ab"))
  (for "Longer power set." "{⌿∘⍵¨↓⌽⍉2⊥⍣¯1⊢¯1+⍳2*≢⍵} 'abc'"
       #("" "a" "b" "ab" "c" "ac" "bc" "abc"))
  (for "Inversion of variable-referenced function." "vr←3∘× ⋄ vr⍣¯1⊢24" 8)
  (for "Inversion of arbitrary function." "({3-⍵}⍣¯1⊢8),{⍵-3}⍣¯1⊢8" #(-5 11))
  (for "Inversion of more complex arbitrary function." "{5×2+⍵}⍣¯1⊢20" 2)
  (for "Even more complex function inverted." "⌈{2*1+7-⍵}⍣¯1⊢64" 2)
  (for "Dyadic arbitrary function inverted." "(3 {⍵+÷-⍺}⍣¯1⊢5), 3 {⍺+÷-⍵}⍣¯1⊢5" #(16/3 -1/2)))

 (test-set
  (with (:name :namespace-tests)
        (:tests-profile :title "Namespace Tests")
        (:demo-profile :title "Namespace Demos"
                       :description "Demos of namespace functionality."))
  (for "Assignment of and operation on namespace values."
       "myns←⎕NS⍬ ⋄ myns.aa←5 ⋄ myns.bb←⍳9 ⋄ myns.cc←3 3⍴⍳9 ⋄ myns.cc[2;],myns.aa×myns.bb"
       #(4 5 6 5 10 15 20 25 30 35 40 45))
  (for "Assignment of values in nested namespaces."
       "myns←⎕NS⍬ ⋄ myns.aa←3 ⋄ myns.bb←⎕NS⍬ ⋄ myns.cc←⍳3 ⋄ myns.bb.dd←⎕NS⍬ 
    myns.bb.dd.ee←5 ⋄ myns.bb.ff←⍳4 ⋄ myns.bb.gg←2 2⍴⍳4 
    myns.cc,({⍵.bb.ff} myns),,myns.bb.gg×myns.bb.dd.ee+myns.aa"
       #(1 2 3 1 2 3 4 8 16 24 32))
  (for "Assignment, modification and display of values in nested namespaces."
       "myns←⎕NS⍬ ⋄ myns.aa←⎕NS⍬ ⋄ myns.aa.bb←⍳9 ⋄ myns.aa.bb[2 4]←⎕NS⍬ ⋄ myns.aa.bb[2].cc←3 
    myns.aa.bb[4].cc←5 ⋄ myns.aa.bb[2 4].cc+←3 ⋄ myns,myns.aa.bb[2 4].cc"
       #((:|aa| (:|bb| #(1 (:|cc| 6) 3 (:|cc| 8) 5 6 7 8 9))) 6 8))
  (for "Use of function within namespace."
       "myns←⎕NS⍬ ⋄ myns.f1←{⍵+3} ⋄ myns.a←⎕NS⍬ ⋄ myns.a.f2←{⍵×2} ⋄ myns.f1 myns.a.f2 6"
       15)
  (for "Assignment of values within namespace using namespace point."
       "⎕CS _ ⋄ myns←⎕NS⍬ ⋄ myns.aa←⎕NS⍬ ⋄ ⎕CS myns.aa ⋄ bb←33 ⋄ gg←⎕NS⍬ ⋄ gg.hh←5 ⋄ gg.ii←6 
    gg.jj←{⍺×⍵} ⋄ ff←{⍵+5} ⋄ cc←22 ⋄ dd←ff bb+cc ⋄ gg.kk←gg.hh gg.jj gg.ii ⋄ ⎕CS _ ⋄ myns"
       '(:|aa| (:|dd| 60 :|cc| 22 :|ff| :FUNCTION
                         :|gg| (:|kk| 30 :|jj| :FUNCTION :|ii| 6 :|hh| 5) :|bb| 33)))
  (for "Namespace points set in global and local scopes."
       "⎕CS _ ⋄ myns←⎕NS⍬ ⋄ myns.aa←10 ⋄ myns.bb←⎕NS⍬ ⋄ myns.bb.cc←3 ⋄ ⎕CS myns
    it←{⎕CS myns.bb ⋄ d←5 ⋄ e←3 ⋄ cc+d+e+⍵} 10+aa ⋄ ⎕CS _ ⋄ myns"
       '(:|it| 31 :|bb| (:|cc| 3) :|aa| 10))
  (for "Elision of namespaces within an array."
       "myns←⎕NS⍬ ⋄ myns.a←1 ⋄ myns.b←2 ⋄ myns.c←{n←⎕NS⍬ ⋄ n.a←⍵ ⋄ n.b←⍵×2 ⋄ n.d←⍳5 ⋄ n}¨⍳3 
    myns.c.a+←2 ⋄ myns.c.c←5 ⋄ myns.c.d[3 5]←⎕NS⍬ ⋄ myns.c.d[3 5].a←2 
    myns.c.e←3⍴⎕NS⍬ ⋄ myns.c.e.a←3 ⋄ myns"
       '(:|c| #((:|e| #((:|a| 3) (:|a| 3) (:|a| 3))
                 :|c| 5 :|d| #(1 2 (:|a| 2) 4 (:|a| 2)) :|b| 2 :|a| 3)
                (:|e| #((:|a| 3) (:|a| 3) (:|a| 3))
                 :|c| 5 :|d| #(1 2 (:|a| 2) 4 (:|a| 2)) :|b| 4 :|a| 4)
                (:|e| #((:|a| 3) (:|a| 3) (:|a| 3))
                 :|c| 5 :|d| #(1 2 (:|a| 2) 4 (:|a| 2)) :|b| 6 :|a| 5))
         :|b| 2 :|a| 1)))
 
 (test-set
  (with (:name :printed-format-tests)
        (:tests-profile :title "Printed Data Format Tests")
        (:demo-profile :title "Data Format Demos"
                       :description "More demos showing how different types of data are formatted in April."))
  (for-printed "Single integer." "5" "5")
  (for-printed "Negative integer." "¯5" "¯5")
  (for-printed "Rational numbers." "÷⍳5" "1 1r2 1r3 1r4 1r5
")
  (for-printed "String." "'abcd'" "abcd")
  (for-printed "Nested string." "⊂'efgh'" " efgh ")
  (for-printed "Doubly nested string." "⊂⊂'hijk'" "  hijk  ")
  (for-printed "Nested string within vector." "1 2 3 4 (⊂'lm') 5 6" "1 2 3 4   lm   5 6
")
  (for-printed "Floating point number." "25.006" "25.006")
  (for-printed "Imaginary number." "3J9" "3J9")
  (for-printed "Numeric vector." "1+1 2 3" "2 3 4
")
  (for-printed "Vector of mixed integers and floats." "12.5 3 42.890 90.5001 8 65"
               "12.5 3 42.89 90.5001 8 65
")
  (for-printed "Overtake of float vector."
               "6↑○⍳3" "3.141592654 6.283185307 9.424777961 0.0 0.0 0.0
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
  (for-printed "Vector with enclosed nested vector." "8 (⊂1 5)" "8   1 5 
")
  (for-printed "Single scalar character." "'A'" "A")
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
  (for-printed "Enclosed vector." "⊂1 2 3 4 5" " 1 2 3 4 5
")
  (for-printed "Enclosed matrix." "⊂3 4⍴⍳9" " 1 2 3 4
 5 6 7 8
 9 1 2 3
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
               "⎕pp←6 ⋄ ⍪8 900.17814 3005 ¯15.90 88.1,÷2.0 4.0 8.0"
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
  (for-printed "Another mixed matrix." "gg←⍪12 'abc' 900 ⋄ gg,(⍪1 2 3),gg"
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
  (for-printed "Matrix containing enclosed arrays."
               "¯3\\0⍴⊂2 2⍴(⊂3 3⍴⍳6) 9 8 7"
               "   0 0 0   0    0 0 0   0    0 0 0   0
   0 0 0        0 0 0        0 0 0    
   0 0 0        0 0 0        0 0 0    
        0  0         0  0         0  0
")
  (for-printed "Another matrix with enclosed arrays."
               "4↑0↑⊂2 2⍴(⊂2 2⍴⍳4) 2 3"
               "   0 0       0    0 0       0    0 0       0    0 0       0
   0 0            0 0            0 0            0 0        
      0   0 0        0   0 0        0   0 0        0   0 0 
          0 0            0 0            0 0            0 0 
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
  (for-printed "Mixed matrix of long numbers and small arrays."
               "2 3⍴10000,⊂2 2⍴⍳4"
               "10000    1 2  10000
         3 4       
  1 2  10000    1 2
  3 4           3 4
")
  (for-printed "Nested vector of vectors." "↓⍣2⊢2 2⍴⍳4" "  1 2  3 4
")
  (for-printed "Double-nested vector of vectors." "↓⍣3⊢2 2⍴⍳4" "   1 2  3 4
")
  (for-printed "Stacked floats with negative value under 1." "⍪¯0.75 1.25" "¯0.75
 1.25
")
  (for-printed "Stacked rational fractions and integers."
               "⍪21r8 90 122r17 3r21 3 9r212"
               " 21r__8
 90    
122r_17
  1r__7
  3    
  9r212
")
  (for-printed "Complex rational vector." "3r4J9r5×⍳4"
               "3r4J9r5 3r2J18r5 9r4J27r5 3J36r5
")
  (for-printed "Matrix of complex numbers." "3 3⍴3.2J5.3 32.95J12.15"
               " 3.20J_5.3  32.95J12.15  3.20J_5.3 
32.95J12.15  3.20J_5.3  32.95J12.15
 3.20J_5.3  32.95J12.15  3.20J_5.3 
")
  (for-printed "Another complex matrix." "⎕pp←10 ⋄ ¯2 ¯3 ¯4 ¯5 ¯6 ∘.○ ¯2 ¯2J3r5"
               " 3.141592654J¯1.316957897  2.813351012J¯1.379764132
¯1.107148718              ¯1.134728076J_0.113868882
¯1.732050808              ¯1.761796881J_0.681122786
¯1.443635475              ¯1.475251760J_0.263872484
 1.316957897J_3.141592654  1.379764132J_2.813351012
")
  (for-printed "Stacked complex float, integer and rational."
               "⍪12.2J44 3J8 19J210r17"
               "12.2J_44.0 
 3__J__8   
19__J210r17
")
  (for-printed "Stacked complex float, integer and non-complex rational."
               "⍪12.2J44 3J8 19r13"
               "12.20J44.0
 3___J_8  
19r13     
")
  (for-printed "Stacked complex float and rational fractions."
               "⍪12.2J44 3r8J8r21 19r313J21r17"
               "12.200J44.0 
 3r__8J_8r21
19r313J21r17
")
  (for-printed "Output of variable assignment (just a newline)." "x←1" "")
  (for-printed "Binomial of complex numbers." "⎕pp←4 ⋄ 2!3J2" "1.000J5.000")
  (for-printed "Binomial of positive and negative fractional numbers." "⎕pp←5 ⋄ 3!.05 2.5 ¯3.6"
               "0.0154 0.3125 ¯15.456
")
  (for-printed "Function name." "⎕pp←10 ⋄ fun←{⍵+5} ⋄ fun" "∇fun")
  (for-printed "Namespace with key/value pair count."
               "myns←⎕NS⍬ ⋄ myns.a←1 ⋄ myns.b←2 ⋄ myns.c←3 ⋄ myns" "[Ns.3]")
  (for-printed "Array containing namespaces and other values."
               "myns←⎕NS⍬ ⋄ myns.a←1 ⋄ myns.b←2 ⋄ 4 4⍴1 2 3 myns 'a'"
               "1      2      3 [Ns.2]
     a 1      2 3     
[Ns.2]      a 1 2     
3      [Ns.2] a 1     
"))

 ;; tests of April code involving structures outside the language itself, including configuration
 ;; parameters that can be passed to the (april) macro
 (arbitrary-test-set
  (with (:name :output-specification-tests)
        (:tests-profile :title "Output Specification Tests"))
  ((progn (format t "λ Evaluation of ⍳ with specified index origin.~%")
          (is (print-and-run (april (with (:state :index-origin 0)) "⍳9"))
              #(0 1 2 3 4 5 6 7 8) :test #'equalp))
   (let ((out-str (make-string-output-stream)))
     (format t "λ Printed output at given precisions.~%")
     (print-and-run (april-f (with (:state :print-to out-str :print-precision 3)) "○1 2 3"))
     (is (get-output-stream-string out-str)
         "3.14 6.28 9.42
")
     (format t "~%")
     (print-and-run (april-f (with (:state :print-to out-str :print-precision 6)) "○1 2 3"))
     (is (get-output-stream-string out-str)
         "3.14159 6.28319 9.42478
")

     (format t "~%λ Output of function definition (just a newline).~%")
     (print-and-run (april-f (with (:state :print-to out-str)) "{⍵+3}"))
     (is (get-output-stream-string out-str)
         "
"))
   (progn (format t "λ Floating-point comparisons with varying comparison tolerance.~%")
          (is (print-and-run (april-c "{G←1.00001<1.0001 ⋄ ⎕ct←0.0001 ⋄ H←G,1.00001<1.0001 ⋄ ⎕ct←⍵ ⋄ H}"
                                      double-float-epsilon))
              #*10))
   (progn (format t "λ Output of one input and one declared variable with index origin set to 0.~%")
          (multiple-value-bind (out1 out2)
              (print-and-run (april (with (:state :count-from 0 :in ((a 3) (b 5)) :out (a c)))
                                    "c←a+⍳b"))
            (is out1 3)
            (format t "~%")
            (is out2 #(3 4 5 6 7) :test #'equalp)))
   (progn (format t "λ Output of function containing reference to input variable.~%")
          (is (print-and-run (april (with (:state :in ((-a-b-c 10)))) "{ABC+⍵} 5")) 15))
   (progn (format t "λ Output of both value and APL-formatted value string.~%")
          (multiple-value-bind (out1 out2)
              (print-and-run (april (with (:state :output-printed t)) "2 3⍴⍳9"))
            
            (is out1 #2A((1 2 3) (4 5 6)) :test #'equalp)
            (format t "~%")
            (is out2 "1 2 3
4 5 6
")))
   (progn (format nil "λ Output of APL-formatted value string alone.~%")
          (is (print-and-run (april (with (:state :output-printed :only)) "2 3⍴⍳9"))
              "1 2 3
4 5 6
"))
   (progn (format t "λ Output of three internally-declared variables.~%")
          (multiple-value-bind (out1 out2 out3)
              (print-and-run (april (with (:state :out (a b c)))
                                    "a←9+2 ⋄ b←5+3 ⋄ c←2×9"))
            (format t "~%")
            
            (is out1 11)
            (format t "~%")
            (is out2 8)
            (format t "~%")
            (is out3 18)))
   (progn (format t "λ Type-dependent destructive or non-destructive modification of input values.~%")
          (let ((output (print-and-run (let ((a #(1 2 3)) (b #(2 3 4))
                                             (c (make-array 3 :element-type '(unsigned-byte 4)
                                                              :initial-contents '(7 8 9)))
                                             (d (make-array 3 :element-type '(unsigned-byte 4)
                                                              :initial-contents '(10 11 12))))
                                         (april (with (:state :in ((a a) (b b) (c c) (d d))))
                                                "a[1]←20 ⋄ (⊃b)←30 ⋄ c[1]←40 ⋄ (⊃d)←50")
                                         (vector a b c d)))))
            (format t "~%")
            
            (is output #(#(1 2 3) #(2 3 4) #(7 8 9) #(10 11 12)) :test #'equalp)))
   (progn (format t "λ Output using ⎕← to specified output stream.~%")
          (let* ((out-str (make-string-output-stream))
                 (vector (print-and-run (april (with (:state :print-to out-str))
                                               "a←1 2 3 ⋄ ⎕←a+5 ⋄ ⎕←3 4 5 ⋄ 3+a"))))

            (is vector #(4 5 6) :test #'equalp)

            (format t "~%")
            
            (is (print-and-run (get-output-stream-string out-str))
                "6 7 8
3 4 5
")))
   (progn (format t "λ Printed output of a variable assignment preceded by ⎕←.~%")
          (let* ((out-str (make-string-output-stream))
                 (vector (print-and-run (april (with (:state :print-to out-str))
                                               "⎕←x←1 2 3"))))
            
            (is vector #(1 2 3) :test #'equalp)
            
            (format t "~%")
            
            (is (print-and-run (get-output-stream-string out-str))
                "1 2 3
")))
   (let* ((out-str (make-string-output-stream))
          (other-out-str (make-string-output-stream)))
     (print-and-run (april-f "a←1 2 3 ⋄ ⎕ost←('APRIL' 'OUT-STR') ⋄ ⎕←a+5 ⋄ ⎕←3 4 5 
⎕ost←('APRIL' 'OTHER-OUT-STR') ⋄ 3+a"))
     (format t "~%~%")
     (is (print-and-run (get-output-stream-string out-str))
         "6 7 8
3 4 5
" :test #'string=)
     (format t "~%")
     (is (print-and-run (get-output-stream-string other-out-str))
         "4 5 6
" :test #'string=))
   (progn (format t "λ Multi-line function with comment at end.~%")
          
          (is (print-and-run (april "fun←{
  5+⍵
  ⍝ comment
}
fun 3")) 8))
   (progn (format t "λ Compact function calls.~%")
          
          (is (print-and-run (april-c "{⍺×⍵}" 2 8)) 16)
          
          (format t "~%")
          
          (is (print-and-run (april-c "{[a;b;c;d] d↑c⍴a+b}" 3 5 6 10))
              #(8 8 8 8 8 8 0 0 0 0) :test #'equalp)
          
          (format t "~%")
          
          (is (print-and-run (april-c "⍴" 5 3))
              #(5 5 5) :test #'equalp)
          
          (format t "~%")

          (is (print-and-run (april-c (with (:state :count-from 0)) "{⍳⍵}" 7))
              #(0 1 2 3 4 5 6) :test #'equalp)
          
          (format t "~%")

          (format t "λ Compact operator-composed function calls.~%")

          ;; TODO: need lazy wrapping for the passed functions
          ;; (is (print-and-run (april-c "{⍵⍵ ⍺⍺/⍵}" #'+ #'- #(1 2 3 4 5)))
          ;;     -15 :test #'=)
          
          (format t "~%")

          ;; (is (print-and-run (april-c "{⍵⍵ ⍺ ⍺⍺/⍵}" #'+ (scalar-function -) #(1 2 3 4 5) 3))
          ;;     #(-6 -9 -12) :test #'equalp)
          )
   )))

;; create the common workspace and the space for unit tests
(april-create-workspace common)
(april-create-workspace unit-test-staging)

#|
This is an example showing how the April idiom can be extended with Vex's extend-vex-idiom macro.
A not-very-useful scalar function that adds 3 to its argument(s) is specified here.

For a more complete and practical example of idiom extension, see extensions/uzuki.

(extend-vex-idiom
 april
 (utilities :process-fn-op-specs #'process-fnspecs)
 (functions
  (with (:name :extra-functions))
  (≣ (has :title "Add3")
     (ambivalent (scalar-function (λω (+ 3 omega)))
                 (scalar-function (lambda (alpha omega) (+ 3 alpha omega))))
     (tests (is "≣77" 80)
            (is "8≣7" 18)))))
|#
