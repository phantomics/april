
(in-package #:april)

(defun build-call-form (glyph-char &optional args axes)
  "Format a function to be called within generated APL code."
  (if (not (characterp glyph-char))
      glyph-char
      (let* ((fn-meta (handler-case (funcall (symbol-function (intern (format nil "APRIL-LEX-FN-~a" glyph-char)
                                                                      *package-name-string*))
                                             :get-metadata)
                        (error () nil)))
             (is-scalar (of-lexicons *april-idiom* glyph-char
                                     (if (eq :dyadic args) :functions-scalar-dyadic
                                         :functions-scalar-monadic))))
        (append (list (if is-scalar 'apl-fn-s 'apl-fn)
                      (intern (string glyph-char) *package-name-string*))
                (getf fn-meta :implicit-args)
                (if (and axes (or (getf fn-meta :axes)
                                  (eq :dyadic args)))
                    (list (if is-scalar `(apply-scalar #'- ,(caar axes) index-origin)
                              (cons 'list (first axes)))))))))

(defun proc-value (this-item &optional properties process idiom space)
  "Process a value token."
  (cond ;; process the empty vector expressed by the [⍬ zilde] character
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
              (or (member this-item '(⍵ ⍺ ⍹ ⍶) :test #'eql)
                  (getf properties :symbol-overriding)
                  (or (member this-item (of-meta-hierarchy (rest (getf (getf properties :special)
                                                                       :closure-meta))
                                                           :var-syms))
                      ;; if it's defined locally as a variable, disregard a global function definition
                      (not (is-workspace-function this-item))))
              (or (getf properties :symbol-overriding)
                  (member this-item (of-meta-hierarchy (rest (getf (getf properties :special)
                                                                   :closure-meta))
                                                       :var-syms))
                  (not (member this-item
                               (append '(⍺⍺ ⍵⍵)
                                       (of-meta-hierarchy (rest (getf (getf properties :special) :closure-meta))
                                                          :fn-syms)
                                       (of-meta-hierarchy (rest (getf (getf properties :special) :closure-meta))
                                                          :lop-syms)
                                       (of-meta-hierarchy (rest (getf (getf properties :special) :closure-meta))
                                                          :pop-syms)))))
              ;; make sure the symbol doesn't reference a lexically-defined function
              (or (not (is-workspace-operator this-item))
                  (member this-item (of-meta-hierarchy (rest (getf (getf properties :special)
                                                                   :closure-meta))
                                                       :var-syms))
                  (getf properties :symbol-overriding))
              (not (member (intern (string-upcase this-item) *package-name-string*)
                           (rest (assoc :function (idiom-symbols idiom)))))
              (not (member this-item '(⍺⍺ ⍵⍵ ∇ ∇∇) :test #'eql))
              (or (not (getf properties :type))
                  (eq :symbol (first (getf properties :type)))))
         (let* ((space "APRIL-WORKSPACE-COMMON")
                (path-val (or (getf (rest (getf (getf properties :special) :closure-meta)) :ns-point)
                              (symbol-value (intern "*NS-POINT*" space)))))
           (values (if (not (member (intern (string-upcase this-item) *package-name-string*)
                                    (rest (assoc :function (idiom-symbols idiom)))))
                       (if (or (not path-val)
                               (string= "_" (string this-item))
                               (member this-item '(⍵ ⍺ ⍹ ⍶))
                               (member this-item *system-variables*)
                               (member this-item
                                       (of-meta-hierarchy (rest (getf (getf properties :special) :closure-meta))
                                                          :var-syms))
                               (member this-item (assoc :variable (idiom-symbols idiom)))
                               (member this-item (assoc :constant (idiom-symbols idiom))))
                           this-item (cons 'nspath (append path-val (list (intern (string this-item)
                                                                                  "KEYWORD")))))
                       (intern (string-upcase this-item)))
                   '(:type (:symbol)))))
        (t (values nil nil))))

(defun proc-function (this-item &optional properties process idiom space)
  "Process a function token."
  (let* ((space "APRIL-WORKSPACE-COMMON")
         (current-path (or (getf (rest (getf (getf properties :special) :closure-meta)) :ns-point)
                           (symbol-value (intern "*NS-POINT*" space)))))
    (if (listp this-item)
        ;; process a function specification starting with :fn
        (if (or (eq :fn (first this-item))
                ;; if marked as an operator, check whether the character is one entered as both
                ;; a function and an operator; such functions must be dyadic
                ;; (and (eq :op (first this-item))
                ;;      (or (of-lexicons idiom (third this-item) :functions-dyadic)
                ;;          (of-lexicons idiom (third this-item) :functions-symbolic)))
                )
            (let ((fn (first (last this-item)))
                  (obligate-dyadic (and (eq :op (first this-item))
                                        (of-lexicons idiom (third this-item) :functions-dyadic)))
                  (overloaded-operator (and (eq :op (first this-item))
                                            (or (of-lexicons idiom (third this-item) :functions-dyadic)
                                                (of-lexicons idiom (third this-item) :functions-symbolic)))))
              (cond ((and (characterp fn)
                          (or (not (getf (getf properties :special) :exclude-symbolic))
                              ;; the :exclude-symbolic property prevents a
                              ;; symbolic function like ∘ from matching
                              (not (of-lexicons idiom fn :functions-symbolic)))
                          (or (not (getf properties :glyph))
                              (and (char= fn (aref (string (getf properties :glyph)) 0)))))
                     (values fn (list :type (append '(:function :glyph)
                                                    (if overloaded-operator '(:overloaded-operator))
                                                    (if (of-lexicons idiom (third this-item)
                                                                     :functions-symbolic)
                                                        '(:symbolic-function))))))
                    ((and (listp fn)
                          (not (getf properties :glyph)))
                     (let* ((polyadic-args ;; (if (and (listp (first (last (first fn))))
                                           ;;          (eq :axes (caar (last (first fn)))))
                                           ;;     (mapcar #'caar (cdar (last (first fn)))))
                                           )
                            (fn (if (not polyadic-args)
                                    fn (cons (butlast (first fn) 1)
                                             (rest fn))))
                            (arg-symbols (intersection '(⍺ ⍵ ⍶ ⍹ ⍺⍺ ⍵⍵ ∇∇)
                                                       (getf (cdadr this-item) :arg-syms)))
                            (this-closure-meta (second this-item))
                            (is-inline-operator (intersection arg-symbols '(⍶ ⍹ ⍺⍺ ⍵⍵ ∇∇))))
                       (if (= 2 (length (intersection arg-symbols '(⍶ ⍺⍺))))
                           (error "A defined operator may not include both [⍶ left value] and~a"
                                  " [⍺⍺ left function] operands."))
                       (if (= 2 (length (intersection arg-symbols '(⍹ ⍵⍵))))
                           (error "A defined operator may not include both [⍹ right value] and~⍺"
                                  " [⍵⍵ right function] operands."))
                       (if current-path (setf (getf (rest this-closure-meta) :ns-point)
                                              current-path))
                       ;; if this is an inline operator, pass just that keyword back
                       (if is-inline-operator :is-inline-operator
                           (let ((sub-props (list :special (list :closure-meta this-closure-meta))))
                             (setf (getf (rest this-closure-meta) :var-syms)
                                   (append polyadic-args (getf (rest this-closure-meta) :var-syms)))
                             (values (output-function ;; (mapcar (lambda (f) (funcall process f sub-props)) fn)
                                                      (compile-form fn)
                                                      polyadic-args (rest this-closure-meta))
                                     (list :type '(:function :closure)
                                           :obligate-dyadic obligate-dyadic))))))
                    (t (values nil nil)))))
        (if (and (symbolp this-item)
                 (not (getf properties :glyph)))
            (cond ((and current-path (fboundp (intern (format-nspath (append current-path (list this-item)))
                                                      space)))
                   (values (intern (format-nspath (append current-path (list (intern (string this-item)
                                                                                     "KEYWORD")))))
                           (list :type '(:function :referenced :at-path))))
                  ((and (is-workspace-function this-item)
                        ;; make sure it's not defined locally as a variable
                        (not (member this-item (of-meta-hierarchy (rest (getf (getf properties :special)
                                                                              :closure-meta))
                                                                  :var-syms))))
                   ;; process workspace-aliased lexical functions, as when f←+ has been set
                   (values this-item (list :type '(:function :referenced))))
                  ((eql this-item '∇)
                   (values this-item (list :type '(:function :self-reference))))
                  ((member this-item '(⍵⍵ ⍺⍺))
                   (values this-item (list :type '(:function :operand-function-reference))))
                  ((member this-item (of-meta-hierarchy (rest (getf (getf properties :special) :closure-meta))
                                                        :fn-syms))
                   (values (if (eql '⍺ this-item)
                               this-item (list 'inws this-item))
                           (list :type '(:function :lexical-function))))
                  ((member (intern (string-upcase this-item) *package-name-string*)
                           (rest (assoc :function (idiom-symbols idiom))))
                   (values (let ((idiom-function-object (getf (rest (assoc :function (idiom-symbols idiom)))
                                                              (intern (string-upcase this-item)
                                                                      *package-name-string*))))
                             (if (listp idiom-function-object)
                                 idiom-function-object (list 'function idiom-function-object)))
                           (list :type '(:function :referenced))))
                  (t (values nil nil)))
            (values nil nil)))))

(defun proc-operator (this-item &optional properties process idiom space)
  "Process an operator token."
  (declare (ignore idiom))
  (if (listp this-item)
      (if (and (eq :op (first this-item))
               (not (listp (first (last this-item))))
               (or (not (getf properties :glyph))
                   (not (characterp (first (last this-item))))
                   (char= (character (getf properties :glyph))
                          (first (last this-item)))))
          ;; process an operator token, allowing specification of the valence,
          ;; either :lateral or :pivotal
          (destructuring-bind (op-type op-symbol)
              (rest this-item)
            (let ((valid-by-valence (or (not (getf properties :valence))
                                        (eq op-type (getf properties :valence)))))
              (if (and valid-by-valence (eql '∇∇ op-symbol))
                  (values :operator-self-reference
                          (list :type (list :operator op-type)))
                  (cond ((and valid-by-valence (getf properties :glyph))
                         (if (char= op-symbol (aref (string (getf properties :glyph)) 0))
                             (values op-symbol (list :type (list :operator op-type)))
                             (values nil nil)))
                        (valid-by-valence (values op-symbol (list :type (list :operator op-type))))
                        (t (values nil nil))))))
          ;; (if (and (eql :op (first this-item))
          ;;          (listp (first (last this-item)))
          ;;          (not (getf properties :glyph)))
          ;;     (let* ((fn (first (last this-item)))
          ;;            (arg-symbols (intersection '(⍺ ⍵ ⍶ ⍹ ⍺⍺ ⍵⍵ ∇∇) (getf (cdadr this-item) :arg-syms)))
          ;;            (this-closure-meta (second this-item))
          ;;            (is-inline (intersection arg-symbols '(⍶ ⍹ ⍺⍺ ⍵⍵)))
          ;;            (is-dyadic (member '⍺ arg-symbols))
          ;;            (is-pivotal (intersection arg-symbols '(⍹ ⍵⍵)))
          ;;            (valence (getf properties :valence)))
          ;;       (if (= 2 (length (intersection arg-symbols '(⍶ ⍺⍺))))
          ;;           (error "A defined operator may not include both [⍶ left value] and~a"
          ;;                  " [⍺⍺ left function] operands."))
          ;;       (if (= 2 (length (intersection arg-symbols '(⍹ ⍵⍵))))
          ;;           (error "A defined operator may not include both [⍹ right value] and~⍺"
          ;;                  " [⍵⍵ right function] operands."))
          ;;       (if is-inline (if (or (not valence)
          ;;                             (and is-pivotal (eq :pivotal valence))
          ;;                             (and (not is-pivotal) (eq :lateral valence)))
          ;;                         (let ((sub-props (list :special (list :closure-meta this-closure-meta))))
          ;;                            (values (output-function
          ;;                                    (mapcar (lambda (f) (funcall process f sub-props)) fn)
          ;;                                    nil (rest this-closure-meta))
          ;;                                   (list :type (remove
          ;;                                                nil (list :operator :closure
          ;;                                                          (if is-pivotal :pivotal :lateral)
          ;;                                                          (if is-dyadic :dyadic :monadic)
          ;;                                                          ;; indicate the types of the operands for use
          ;;                                                          ;; in the function composer pattern below
          ;;                                                          (if (member '⍶ arg-symbols)
          ;;                                                              :left-operand-value
          ;;                                                              (if (member '⍺⍺ arg-symbols)
          ;;                                                                  :left-operand-function))
          ;;                                                          (if (member '⍹ arg-symbols)
          ;;                                                              :right-operand-value
          ;;                                                              (if (member '⍵⍵ arg-symbols)
          ;;                                                                  :right-operand-function))))))))
          ;;           (values nil nil)))
          ;;     (values nil nil))
          )
  (if (symbolp this-item)
      ;; if the operator is represented by a symbol, it is a user-defined operator
      ;; and the appropriate variable name should be verified in the workspace
      (let* ((symbol-string (string this-item))
             ;; (type-to-find (getf properties :valence))
             (closure-meta (rest (getf (getf properties :special) :closure-meta)))
             (lop-string (if (eq :lateral (getf properties :valence))
                             symbol-string))
             (pop-string (if (eq :pivotal (getf properties :valence))
                             symbol-string))
             (bound-op (if (and lop-string
                                (not (member this-item ;; make sure it's not defined locally as a variable
                                             (of-meta-hierarchy (rest (getf (getf properties :special)
                                                                            :closure-meta))
                                                                :var-syms)))
                                (or (and (fboundp (intern lop-string space))
                                         (boundp (intern lop-string space))
                                         (eq :lateral (getf (rest (symbol-value (intern lop-string space)))
                                                            :valence)))
                                    (member this-item (of-meta-hierarchy closure-meta :lop-syms))))
                           (intern lop-string)
                           (if (and pop-string
                                    (or (and (fboundp (intern pop-string space))
                                             (boundp (intern pop-string space))
                                             (eq :pivotal
                                                 (getf (rest (symbol-value (intern pop-string space)))
                                                       :valence)))
                                        (member this-item (of-meta-hierarchy closure-meta :pop-syms))))
                               (intern pop-string)))))
        (if bound-op
            (values bound-op (list :type (list :operator (or (getf properties :valence)
                                                             (if (fboundp (intern pop-string space))
                                                                 :pivotal :lateral)))))
            (values nil nil)))
      (values nil nil))))

(defun build-value (tokens &key (axes) (elements) (space))
  ;; (print (list :to tokens))
  (if (not tokens)
      (if elements ;; if no tokens are left and value elements are present, generate an output value
          (enclose-axes (output-value space (if (< 1 (length elements)) elements (first elements))
                                      (loop :for i :below (length elements) :collect nil)
                                      nil)
                        axes))
      (if (and (listp (first tokens)) (eq :fn (caar tokens))
               (characterp (cadar tokens)) (char= #\← (cadar tokens)))
          ;; if a ← is encountered, this is a value assignment form
          (let ((symbol (proc-value (second tokens) nil nil *april-idiom* space)))
            (if (symbolp symbol) `(a-set ,(output-value space symbol (list nil) nil)
                                         ,(build-value nil :axes axes :elements elements :space space))))
          (if (and (listp (first tokens)) (eq :axes (caar tokens))) ;; if axes like [2] are encountered
              ;; if elements are present, recurse and process the items after the axes as another
              ;; vector for the axes to be applied to
              (if elements (build-value 
                            nil :axes axes
                                :elements (cons (output-value
                                                 space (list (build-value
                                                              (rest tokens)
                                                              :axes (list (compile-form (cdar tokens)))
                                                              :space space))
                                                 (list nil) nil)
                                                elements))
                  ;; if no elements preceded the axes, just pass the axes to the next level of recursion
                  ;; to be applied to the next element encountered
                  (build-value (rest tokens) :axes (cons (compile-form (cdar tokens)) axes) :space space))
              (let* ((is-closure (and (first tokens) (listp (first tokens))
                                      (not (member (caar tokens) '(:fn :op :st :pt :axes)))))
                     ;; handle enclosed values like (1 2 3)
                     (first-value (if is-closure (build-value (first tokens) :space space)
                                      (proc-value (first tokens) nil nil *april-idiom* space))))
                ;; if a value element is confirmed, add it to the list of elements and recurse
                (if first-value
                    (build-value (rest tokens)
                                 :elements (cons (if (and axes is-closure)
                                                     (list (enclose-axes
                                                            (output-value space first-value (list nil) nil)
                                                            axes))
                                                     first-value)
                                                 elements)
                                 :space space :axes (if (not is-closure) axes))
                    (if elements
                        (if (and (listp (first tokens)) (eq :fn (caar tokens))
                                 (characterp (cadar tokens)) (char= #\← (cadar tokens)))
                            (print "Assigned.")
                            (let ((preceding (build-value nil :elements elements
                                                              :axes axes :space space)))
                              (multiple-value-bind (function remaining)
                                  (build-function tokens :space space :axes axes)
                                (if function (let ((second-value (build-value remaining :space space)))
                                               `(a-call ,function ,preceding
                                                        ,@(if remaining (list second-value))))
                                    preceding)))))))))))

;; (if (and (first tokens) (listp (first tokens)) ;; handle enclosed values like (1 2 3)
;;          (not (member (caar tokens) '(:fn :op :st :pt :axes))))
;;     (let ((sub-value (build-value (first tokens) :space space)))
;;       (print (list :sub sub-value))
;;       (if sub-value (build-value
;;                      (rest tokens)
;;                      :elements (if axes (list (enclose-axes
;;                                                (output-value space sub-value (list nil) nil)
;;                                                axes))
;;                                    (cons sub-value elements))
;;                      :space space)))

(defun build-function (tokens &key (axes) (found-function) (initial) (space))
  (print (list :to tokens))
  (if (and (first tokens) (listp (first tokens)) ;; handle enclosed functions like (,∘×)
           (not (member (caar tokens) '(:fn :op :st :pt :axes))))
      (let ((sub-function (build-function (first tokens) :initial t :space space)))
        ;; join sub-function to other functions if present, as with ⍴∘(,∘×)
        (if sub-function (build-function (rest tokens) :found-function sub-function :initial initial
                                         :space space)))
      (if (and (listp (first tokens)) (eq :axes (caar tokens)))
          (print "Test.")
          (if (not found-function)
              (let ((exp-operator (proc-operator (first tokens) nil nil *april-idiom*)))
                ;; (print (list :ee exp-operator))
                (if exp-operator ;; if a lateral operator is present as for +/
                    (let ((exp-function (proc-function (second tokens) nil nil *april-idiom*)))
                      (values (compose-function-lateral exp-operator (build-call-form exp-function))
                              (cddr tokens)))
                    (let ((exp-function (proc-function (first tokens) nil nil *april-idiom*)))
                      (if exp-function (build-function (rest tokens)
                                                       :initial initial :space space
                                                       :found-function (build-call-form exp-function))
                          #|TODO: clause for overloaded operators like /|#))))
              (if (and (listp (first tokens)) (eq :fn (caar tokens))
                       (characterp (cadar tokens)) (char= #\← (cadar tokens)))
                  ;; if a ← is encountered, this becomes a function assignment form
                  (if (third tokens) (error "Nothing can follow a function assignment.")
                      (let ((symbol (proc-value (second tokens) nil nil *april-idiom* space)))
                        (print (list :sym symbol (second tokens)))
                        (if (symbolp symbol) `(a-set ,(output-value space symbol (list nil) nil)
                                                     ,found-function))))
                  (let ((exp-operator (proc-operator (first tokens) nil nil *april-idiom*)))
                    (print (list :eex tokens exp-operator initial))
                    (if exp-operator ;; if a pivotal operator is present as for +.×
                        (let ((second-function (proc-function (second tokens) nil nil *april-idiom*)))
                          (if second-function
                              (values (compose-function-pivotal exp-operator found-function
                                                                (build-call-form second-function))
                                      (cddr tokens))))
                        (if initial ;; if the found-function begins a clause as with (-÷,)
                            (let ((first-function (proc-function (first tokens) nil nil *april-idiom*))
                                  (second-function (proc-function (second tokens) nil nil *april-idiom*)))
                              (print (list :ff first-function second-function))
                              ;; first function confirms an atop train like *÷
                              ;; second function confirms a three-element train like -÷,
                              ;; in either case what comes before may be a function of any complexity
                              (if first-function
                                  (build-function (funcall (if second-function #'cddr #'rest) tokens)
                                                  :found-function
                                                  (if second-function
                                                      (compose-function-train
                                                       found-function (build-call-form first-function)
                                                       (build-call-form second-function))
                                                      (compose-function-train
                                                       found-function (build-call-form first-function)))
                                                  :initial initial :space space)
                                  (values found-function tokens)))
                            (values found-function tokens)))))))))

(defun compose-function-lateral (operator function)
  (funcall (symbol-function (intern (format nil "APRIL-LEX-OP-~a" operator) *package-name-string*))
           function))

(defun compose-function-pivotal (operator function1 function2)
  (funcall (symbol-function (intern (format nil "APRIL-LEX-OP-~a" operator) *package-name-string*))
           function1 function2))

(defun compose-function-train (right center &optional left)
  (let ((omega (gensym)) (alpha (gensym)))
    (if left `(lambda (,omega &optional ,alpha)
                (if ,alpha (a-call ,center (a-call ,right ,omega ,alpha)
                                   (a-call ,left ,omega ,alpha))
                    (a-call ,center (a-call ,right ,omega)
                            (a-call ,left ,omega))))
        `(lambda (,omega &optional ,alpha)
           (if ,alpha (a-call ,center (a-call ,right ,omega ,alpha))
               (a-call ,center (a-call ,right ,omega)))))))

(defun compile-form (exprs)
  (loop :for expr :in exprs
        :collect (or (build-value expr :space "APRIL-WORKSPACE-COMMON")
                     (build-function expr :initial t :space "APRIL-WORKSPACE-COMMON"))))


`((3 2 1 (:FN #\+) 1) 
  (6 5 4 (:FN #\MULTIPLICATION_SIGN) (:OP :PIVOTAL #\.) (:FN #\+) 3 2 1))
