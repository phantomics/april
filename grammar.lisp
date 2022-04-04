;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; grammar.lisp

(in-package #:april)

"This file contains the specification of April's basic grammar elements, including the basic language components - array, function and operator - and the patterns comprising those elements that make up the language's strucures."

(defun resolve-path (input-sym space properties)
  "Generate an (nspath) namespace path form depending on the symbols found and the status of the workspace's namespace point, set using ⎕CS."
  (if (and (listp input-sym) (eql 'inws (first input-sym)))
      input-sym ;; lexically-scoped symbols don't get the path prepended, as for {a←⍵ ⋄ a+3} 5
      (let ((path-val (or (getf (rest (getf (getf properties :special) :closure-meta)) :ns-point)
                          (symbol-value (intern "*NS-POINT*" space))))
            ;; get the workspace path as set in the current context
            (symbol (if (and (listp input-sym) (eql 'inwsd (first input-sym)))
                        (second input-sym)
                        ;; if something like (:pt (a (:op :pivotal #\.) b)) is found, correct it
                        ;; TODO: can the (:op) form be decomposed further up the chain?
                        (if (and (listp input-sym)
                                 (eq :pt (first input-sym))
                                 (listp (second input-sym))
                                 (listp (second (second input-sym)))
                                 (eq :op (first (second (second input-sym))))
                                 (eq :pivotal (second (second (second input-sym)))))
                            (list :pt (first (second input-sym)) (third (second input-sym)))
                            input-sym))))
        (if path-val ;; if a context path is set, it must be prepended to the each path resolved
            (if (and (not (and (listp symbol) (member (first symbol) '(nspath :pt))))
                     (or (string= "_" (string symbol)) ;; these types of symbols don't get a path prepended
                         (member symbol '(⍵ ⍺ ⍹ ⍶))
                         (member symbol *system-variables*)
                         (of-meta-hierarchy (rest (getf (getf properties :special) :closure-meta))
                                            :var-syms symbol)
                         (member symbol (assoc :variable (idiom-symbols *april-idiom*)))
                         (member symbol (assoc :constant (idiom-symbols *april-idiom*)))))
                symbol (if (and (listp symbol) (member (first symbol) '(nspath :pt)))
                           (cons 'nspath (append path-val (cons (intern (string (if (listp (second symbol))
                                                                                    (cadadr symbol)
                                                                                    (second symbol)))
                                                                        "KEYWORD")
                                                                (cddr symbol))))
                           (cons 'nspath (append path-val (list (intern (string symbol) "KEYWORD"))))))
            (if (or (symbolp symbol)
                    (and (listp symbol) (member (first symbol) '(inws inwsd))))
                input-sym (cons 'nspath (cons (if (member (second symbol) '(⍵ ⍺ ⍹ ⍶))
                                                  (second symbol)
                                                  (list (if (of-meta-hierarchy
                                                             (rest (getf (getf properties :special)
                                                                         :closure-meta))
                                                             :var-syms (second symbol))
                                                            'inws 'inwsd)
                                                        (intern (string (if (listp (second symbol))
                                                                            (cadadr symbol)
                                                                            (second symbol)))
                                                                *package-name-string*)))
                                              (loop :for s :in (cddr symbol)
                                                    :collect (if (and (listp s) (eq :axes (first s)))
                                                                 (mapcar (lambda (item)
                                                                           (compile-form
                                                                            item :space space
                                                                                 :params properties))
                                                                         (rest s))
                                                                 (if (symbolp s)
                                                                     (intern (string s) "KEYWORD")))))))))))

(defun process-value (this-item &optional properties space)
  "Process a value token."
  (cond ((eq :empty-array this-item)
         ;; process the empty vector expressed by the [⍬ zilde] character
         (make-array 0))
        ;; process numerical values
        ((and (numberp this-item)
              (or (not (getf properties :type))
                  (eq :number (first (getf properties :type)))))
         this-item)
        ;; process string values
        ((and (stringp this-item)
              (or (not (getf properties :type))
                  (eq :string (first (getf properties :type)))))
         this-item)
        ;; process scalar character values
        ((and (characterp this-item)
              (or (not (getf properties :type))
                  (eq :character (first (getf properties :type)))))
         this-item)
        ((and this-item (listp this-item) (eq :pt (first this-item)))
         (let* ((current-path (or (getf (rest (getf (getf properties :special) :closure-meta))
                                        :ns-point)
                                  (symbol-value (intern "*NS-POINT*" space))))
                (nspath (format-nspath (if (not current-path)
                                           (rest this-item)
                                           (append current-path (rest this-item))))))
           (if (or (not nspath) (not (fboundp (intern nspath space)))
                   (getf properties :symbol-overriding))
               (resolve-path this-item space properties))))
        ;; process symbol-referenced values
        ((and (symbolp this-item)
              (or (member this-item '(⍵ ⍺ ⍹ ⍶) :test #'eql)
                  (getf properties :symbol-overriding)
                  (member this-item (getf (getf properties :call-scope) :input-vars))
                  (or (of-meta-hierarchy (rest (getf (getf properties :special) :closure-meta))
                                         :var-syms this-item)
                      ;; if it's defined locally as a variable, disregard a global function definition
                      (let ((current-path (or (getf (rest (getf (getf properties :special) :closure-meta))
                                                    :ns-point)
                                              (symbol-value (intern "*NS-POINT*" space)))))
                        (if (not current-path)
                            (not (is-workspace-function this-item))
                            (not (fboundp (intern (format-nspath (append current-path (list this-item)))
                                                  space)))))))
              (or (getf properties :symbol-overriding)
                  (of-meta-hierarchy (rest (getf (getf properties :special) :closure-meta))
                                     :var-syms this-item)
                  (not (or (member this-item '(⍺⍺ ⍵⍵))
                           (of-meta-hierarchy (rest (getf (getf properties :special) :closure-meta))
                                              :fn-syms this-item)
                           (of-meta-hierarchy (rest (getf (getf properties :special) :closure-meta))
                                              :lop-syms this-item)
                           (of-meta-hierarchy (rest (getf (getf properties :special) :closure-meta))
                                              :pop-syms this-item))))
              ;; make sure the symbol doesn't reference a lexically-defined function
              (or (not (is-workspace-operator this-item))
                  (of-meta-hierarchy (rest (getf (getf properties :special) :closure-meta))
                                     :var-syms this-item)
                  (getf properties :symbol-overriding))
              (not (member (intern (string-upcase this-item) *package-name-string*)
                           (rest (assoc :function (idiom-symbols *april-idiom*)))))
              (not (member this-item '(⍺⍺ ⍵⍵ ∇ ∇∇) :test #'eql))
              (or (not (getf properties :type))
                  (eq :symbol (first (getf properties :type)))))
         (if (not (member (intern (string-upcase this-item) *package-name-string*)
                          (rest (assoc :function (idiom-symbols *april-idiom*)))))
             (resolve-path this-item space properties)
             (intern (string-upcase this-item))))
        ((and (symbolp this-item) (getf properties :match-all-syms))
         this-item)))

(defun process-function (this-item &optional properties space)
  "Process a function token."
  (let* ((current-path (or (getf (rest (getf (getf properties :special) :closure-meta)) :ns-point)
                           (symbol-value (intern "*NS-POINT*" space)))))
    (if (listp this-item)
        ;; process a function specification starting with :fn
        (if (eq :fn (first this-item))
            (let ((fn (first (last this-item))))
              (cond ((and (characterp fn)
                          (or (not (getf (getf properties :special) :exclude-symbolic))
                              ;; the :exclude-symbolic property prevents a
                              ;; symbolic function like ∘ from matching
                              (not (of-lexicons *april-idiom* fn :functions-symbolic)))
                          (of-lexicons *april-idiom* fn :functions)
                          (or (not (getf properties :glyph))
                              (and (char= fn (aref (string (getf properties :glyph)) 0)))))
                     fn)
                    ((and (listp fn)
                          (not (getf properties :glyph)))
                     (if (eq :pass (second this-item))
                         ;; handle a (:fn)-enclosed operator form produced by build-value
                         fn
                         (let* ((polyadic-args (if (and (listp (first (last (first fn))))
                                                        (eq :axes (caar (last (first fn)))))
                                                   (mapcar #'caar (cdar (last (first fn))))))
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
                               (error "A defined operator may not include both [⍹ right value] and~a"
                                      " [⍵⍵ right function] operands."))
                           (if current-path (setf (getf (rest this-closure-meta) :ns-point)
                                                  current-path))
                           ;; if this is an inline operator, pass just that keyword back
                           (if is-inline-operator :is-inline-operator
                               (progn
                                 (setf (getf (rest this-closure-meta) :var-syms)
                                       (append polyadic-args (getf (rest this-closure-meta) :var-syms)))
                                 (output-function
                                  (compile-form
                                   fn :space space
                                      :params (list :special (list :closure-meta (second this-item))
                                                    :call-scope (getf properties :call-scope)))
                                  polyadic-args (rest this-closure-meta)))))))))
            (if (eq :pt (first this-item))
                (let* ((current-path (or (getf (rest (getf (getf properties :special) :closure-meta))
                                               :ns-point)
                                         (symbol-value (intern "*NS-POINT*" space))))
                       (nspath (format-nspath (if (not current-path)
                                                  (rest this-item)
                                                  (append current-path (rest this-item))))))
                  (list 'inwsd (intern nspath *package-name-string*)))))
        (if (and (symbolp this-item)
                 (not (getf properties :glyph)))
            (cond ((and current-path (fboundp (intern (format-nspath (append current-path (list this-item)))
                                                      space)))
                   (list 'inwsd (intern (format-nspath (append current-path (list (intern (string this-item)
                                                                                          "KEYWORD")))))))
                  ((and (is-workspace-function this-item)
                        ;; make sure it's not defined locally as a variable
                        (not (of-meta-hierarchy (rest (getf (getf properties :special) :closure-meta))
                                                :var-syms this-item))
                        (not (or (of-meta-hierarchy (rest (getf (getf properties :special) :closure-meta))
                                                    :fn-syms this-item)
                                 (of-meta-hierarchy (rest (getf (getf properties :special) :closure-meta))
                                                    :lop-syms this-item)
                                 (of-meta-hierarchy (rest (getf (getf properties :special) :closure-meta))
                                                    :pop-syms this-item))))
                   ;; process workspace-aliased lexical functions, as when f←+ has been set
                   `(inwsd ,this-item))
                  ((eql this-item '∇)
                   '#'∇self)
                  ((and (member this-item '(⍵⍵ ⍺⍺ ⍺)) ;; recall ⍺ may be a function following ⍺←function
                        (or (not (eql '⍺ this-item))
                            (of-meta-hierarchy (rest (getf (getf properties :special) :closure-meta))
                                               :fn-syms this-item)))
                   this-item)
                  ((of-meta-hierarchy (rest (getf (getf properties :special) :closure-meta))
                                      :fn-syms this-item)
                   (if (eql '⍺ this-item) this-item (list 'inws this-item)))
                  ((member (intern (string-upcase this-item) *package-name-string*)
                           (rest (assoc :function (idiom-symbols *april-idiom*))))
                   (let ((idiom-function-object (getf (rest (assoc :function (idiom-symbols *april-idiom*)))
                                                      (intern (string-upcase this-item)
                                                              *package-name-string*))))
                     (if (listp idiom-function-object)
                         idiom-function-object (list 'function idiom-function-object)))))))))

(defun process-operator (this-item &optional properties space)
  "Process an operator token."
  (if (listp this-item)
      (if (and (eq :op (first this-item))
               (not (listp (first (last this-item))))
               (or (not (getf properties :glyph))
                   (not (characterp (first (last this-item))))
                   (char= (character (getf properties :glyph))
                          (first (last this-item)))))
          ;; process an operator token, allowing specification of the valence, either :lateral or :pivotal
          (destructuring-bind (op-type op-symbol)
              (rest this-item)
            (let ((valid-by-valence (or (not (getf properties :valence))
                                        (eq op-type (getf properties :valence)))))
              (if (and valid-by-valence (eql '∇∇ op-symbol))
                  ;; return the operator self-reference function
                  '∇oself (cond ((and valid-by-valence (getf properties :glyph))
                                 (if (char= op-symbol (aref (string (getf properties :glyph)) 0))
                                     (values op-symbol op-type)))
                                (valid-by-valence op-symbol)))))
          (if (and (eql :op (first this-item))
                   (listp (first (last this-item)))
                   (not (getf properties :glyph)))
              (let* ((fn (first (last this-item)))
                     (arg-symbols (intersection '(⍺ ⍵ ⍶ ⍹ ⍺⍺ ⍵⍵ ∇∇) (getf (cdadr this-item) :arg-syms)))
                     (this-closure-meta (second this-item))
                     (is-inline (intersection arg-symbols '(⍶ ⍹ ⍺⍺ ⍵⍵)))
                     (is-pivotal (intersection arg-symbols '(⍹ ⍵⍵)))
                     (valence (getf properties :valence)))
                (if (= 2 (length (intersection arg-symbols '(⍶ ⍺⍺))))
                    (error "A defined operator may not include both [⍶ left value] and~a"
                           " [⍺⍺ left function] operands."))
                (if (= 2 (length (intersection arg-symbols '(⍹ ⍵⍵))))
                    (error "A defined operator may not include both [⍹ right value] and~a"
                           " [⍵⍵ right function] operands."))
                (if is-inline (if (or (not valence)
                                      (and is-pivotal (eq :pivotal valence))
                                      (and (not is-pivotal) (eq :lateral valence)))
                                  (output-function
                                   (compile-form fn :space space
                                                    :params (list :special
                                                                  (list :closure-meta (second this-item))
                                                                  :call-scope (getf properties :call-scope)))
                                   nil (rest this-closure-meta)))))))
      (if (symbolp this-item)
          ;; if the operator is represented by a symbol, it is a user-defined operator
          ;; and the appropriate variable name should be verified in the workspace
          (let* ((symbol-string (string this-item))
                 (closure-meta (rest (getf (getf properties :special) :closure-meta)))
                 (lop-string (if (or (not (getf properties :valence))
                                     (eq :lateral (getf properties :valence)))
                                 symbol-string))
                 (pop-string (if (or (not (getf properties :valence))
                                     (eq :pivotal (getf properties :valence)))
                                 symbol-string)))
            (if (and lop-string
                     ;; make sure it's not defined locally as a variable
                     (not (of-meta-hierarchy (rest (getf (getf properties :special) :closure-meta))
                                             :var-syms this-item))
                     (or (and (fboundp (intern lop-string space))
                              (boundp (intern lop-string space))
                              (listp (symbol-value (intern lop-string space)))
                              (eq :lateral (getf (rest (symbol-value (intern lop-string space)))
                                                 :valence)))
                         (of-meta-hierarchy closure-meta :lop-syms this-item)))
                (if (not (of-meta-hierarchy closure-meta :lop-syms this-item))
                    (list 'inwsd (intern lop-string))
                    (list 'inws (intern lop-string)))
                (if (and pop-string
                         (or (and (fboundp (intern pop-string space))
                                  (boundp (intern pop-string space))
                                  (listp (symbol-value (intern pop-string space)))
                                  (eq :pivotal (getf (rest (symbol-value (intern pop-string space)))
                                                     :valence)))
                             (of-meta-hierarchy closure-meta :pop-syms this-item)))
                    (if (not (of-meta-hierarchy closure-meta :pop-syms this-item))
                        (list 'inwsd (intern pop-string))
                        (list 'inws (intern pop-string)))))))))

(defun build-axes (elements &key space params)
  "Construct a set of axes from an (:axes) token form."
  (loop :for element :in elements
        :collect (let ((item-out (compile-form element :space space :params params)))
                   (if (= 1 (length element))
                       (first item-out) (cons 'progn item-out)))))

(defun set-namespace-point (path space params)
  "Set the namespace point to be used by the compiler; this point is prepended to all symbols or namespace segments."
  (let* ((current-path (or (getf (rest (getf (getf params :special) :closure-meta)) :ns-point)
                           (symbol-value (intern "*NS-POINT*" space))))
         (path-val (if (listp path)
                       (if (eql 'nspath (first path))
                           ;; remove the prepended symbols from the current
                           ;; path if present
                           (if current-path (funcall (lambda (list)
                                                       (cons (intern (string (first list)) space)
                                                             (rest list)))
                                        (nthcdr (length current-path) (rest path)))
                               (cons (intern (string (second (second path))) space)
                                     (loop :for item :in (cddr path)
                                           :collect (intern (string item) "KEYWORD"))))
                           (if (not (string= "_" (string (second path))))
                               (list (intern (string (second path)) space)))))))
    (if (getf (getf params :special) :closure-meta)
        (setf (getf (rest (getf (getf params :special) :closure-meta)) :ns-point)
              path-val)
        (setf (symbol-value (intern "*NS-POINT*" space))
              path-val))))

(defun build-value (tokens &key axes elements space params left axes-last)
  "Construct an APL value; this may be a scalar value like 5, a vector like 1 2 3, or the result of a function lilike 1+2."
  (if (not tokens) ;; if no tokens are left and value elements are present, generate an output value
      (if elements (enclose-axes (output-value space (if (< 1 (length elements)) elements (first elements))
                                               (loop :for i :below (length elements) :collect nil)
                                               (rest (getf (getf params :special) :closure-meta)))
                                 axes))
      (cond ((and (not left) (listp (first tokens)) (eq :fn (caar tokens))
                  (characterp (cadar tokens)) (char= #\← (cadar tokens)))
             ;; if a ← is encountered, this is a value assignment form
             (complete-value-assignment (rest tokens) elements space params axes))
            ((and (not left) (listp (first tokens)) (eq :fn (caar tokens))
                  (characterp (cadar tokens)) (char= #\→ (cadar tokens)))
             (complete-branch-composition (rest tokens)
                                          (build-value nil :elements elements :axes axes
                                                       :space space :params params)
                                          :space space :params params))
            ((and (listp (first tokens)) (eq :axes (caar tokens))) ;; if axes like [2] are encountered
             (if elements
                 ;; if elements are present, recurse and process the items after the axes as another
                 ;; vector for the axes to be applied to
                 (if left (values (build-value nil :axes axes :space space :axes-last t
                                                   :left left :params params :elements elements)
                                  tokens)
                     (if (or (and (listp (second tokens))
                                  (member (caadr tokens) '(:fn :op)))
                             ;; account for cases like ↓[1]'abcde'[2 2⍴2 3]
                             (of-meta-hierarchy (rest (getf (getf params :special) :closure-meta))
                                                :fn-syms (second tokens))
                             (of-meta-hierarchy (rest (getf (getf params :special) :closure-meta))
                                                :lop-syms (second tokens))
                             (of-meta-hierarchy (rest (getf (getf params :special) :closure-meta))
                                                :pop-syms (second tokens)))
                         (if axes (build-value tokens :space space :params params
                                                      :elements (list (build-value
                                                                       nil :space space :params params
                                                                           :elements elements :axes axes)))
                             (build-value (rest tokens) :elements elements :space space :params params
                                                        :axes (cons (build-axes (cdar tokens)
                                                                                :space space :params params)
                                                                    axes)
                                                        :axes-last t))
                         (build-value nil :elements (cons (build-value tokens :space space
                                                                              :params params)
                                                          elements)
                                          :axes axes)))
                 ;; if no elements preceded the axes, just pass the axes to the next
                 ;; level of recursion to be applied to the next element encountered
                 (build-value (rest tokens) :axes (cons (build-axes (cdar tokens)
                                                                    :space space :params params)
                                                        axes)
                                            :axes-last t :space space :params params :left left)))
            ((and (listp (first tokens)) (eq :st (caar tokens))) ; 1 2 3∘.⌽[1]⊂2 3 4⍴⍳9
             (let ((stm (funcall (symbol-function (intern (format nil "APRIL-LEX-ST-~a" (caddar tokens))
                                                          *package-name-string*))
                                 (first axes))))
               (build-value (rest tokens) :space space :params params
                                          :left left :elements (cons stm elements))))
            (t (let* ((is-closure (and (first tokens) (listp (first tokens))
                                       (not (member (caar tokens) '(:fn :op :st :pt :axes)))))
                      ;; handle enclosed values like (1 2 3)
                      (first-value (if is-closure (build-value (first tokens) :space space :params params)
                                       (process-value (first tokens) params space))))
                 ;; if a value element is confirmed, add it to the list of elements and recurse
                 (if first-value
                     (if (and is-closure (listp first-value) (listp (first first-value))
                              (eq :fn (caar first-value)) (eq :pass (cadar first-value)))
                         ;; handle the case of an enclosed pivotal operator with a value
                         ;; as right operand, i.e. (+∘5) 10
                         ;; TODO: add error messages for i.e. (2+) 5
                         (if (and left elements)
                             ;; if the value is to the left of a function and a pivotal operator
                             ;; is encountered, and elements are present, compose the existing elements
                             ;; and pass the operator to the next iteration, as for ((2 5⍴0 1)@2 5) 5 5⍴⍳9
                             (values (build-value nil :elements elements :params params :space space)
                                     tokens)
                             ;; if no elements are present proceed as if this expression is not
                             ;; to the left of a function/operand, as for (×∘10)⍣¯1⊢100
                             (let ((passed (build-function first-value :params params :space space
                                                           :axes axes :initial t)))
                               (if passed
                                   (if elements
                                       (multiple-value-bind (lval remaining remaining-axes raxes-last)
                                           (build-value (rest tokens) :space space :params params :left t)
                                         ;; look for a value on the left, as for {1 (3 {(⍹⊃⍵)@(⊂⍶ ⍺)⊢⍵} 4) ⍵}
                                         (if lval `(a-call ,passed ,(build-value
                                                                     nil :elements elements :axes axes
                                                                         :space space :params params)
                                                           ,@(if lval (list lval)))
                                             ;; if no value on the left, a functional expression is likely
                                             ;; as for ⍴(+/⊢⌺3 3) 2 2⍴255
                                             (build-value (rest tokens)
                                                          :elements `((a-call ,passed
                                                                              ,(build-value
                                                                                nil :elements elements
                                                                                    :axes axes :space space
                                                                                    :params params)))
                                                          :space space :params params)))
                                       (values nil (cons (list :fn :pass passed) (rest tokens))))
                                   ;; default if no composed function was passed
                                   (build-value (cons (first first-value) (rest tokens))
                                                :elements elements :params params :space space :axes axes))))
                         (let ((fv-output (output-value space first-value (list nil)
                                                        (rest (getf (getf params :special)
                                                                    :closure-meta)))))
                           (build-value (rest tokens)
                                        :elements (cons (if (not (and axes is-closure))
                                                            first-value fv-output)
                                                        elements)
                                        :params params :space space :left left :axes axes)))
                     (if left
                         ;; if this value is to the left of a function like the 5 in 5+3,
                         ;; process it without looking for a function to its left
                         (let ((exp-operator (build-operator (list (first tokens))
                                                             :params params :space space
                                                             :valence :pivotal :axes axes)))
                           (if exp-operator
                               (values nil (build-value tokens :elements elements :params params
                                                               :space space :axes axes)
                                       axes axes-last)
                               ;; axes not passed back if elements present for cases like m←'+∘×'[2],⍵
                               ;; if no elements, as for ⊂[⍬]⍳3, axes are passed
                               (values (build-value nil :elements elements :params params
                                                        :space space :axes axes)
                                       tokens (if (not elements) axes)
                                       axes-last)))
                         (if elements
                             ;; if axes were -not- encountered last, apply them to the preceding
                             ;; value, as with 3+1 2 3[2]
                             (let ((preceding (build-value nil :elements elements :params params
                                                               :axes (if (not axes-last) axes)
                                                               :space space)))
                               (multiple-value-bind (function remaining)
                                   ;; apply axes to the found function if they were encountered last,
                                   ;; as with 1 2 3∘.⌽[1]⊂2 3 4⍴⍳9
                                   (build-function tokens :space space :params params
                                                          :axes (if axes-last axes))
                                 (if function
                                     (multiple-value-bind (lval remaining remaining-axes raxes-last)
                                         (build-value remaining :space space :params params :left t)
                                       (if (and (listp lval) (listp (first lval))
                                                (eq :fn (caar lval)) (eq :pass (cadar lval)))
                                           ;; handle the case of a left-value pivotal composition
                                           ;; like +∘3 to the left of the function
                                           (let* ((left-fn (build-function lval :space space
                                                                                :params params))
                                                  (value `(a-call
                                                           ,left-fn
                                                           (a-call ,(if (not (and (listp function)
                                                                                  (eql 'inwsd
                                                                                       (first function))))
                                                                        function `(function ,function))
                                                                   ,preceding))))
                                             (if (not remaining) value
                                                 (build-value remaining :elements (list value)
                                                                        :axes remaining-axes
                                                                        :axes-last raxes-last
                                                                        :space space :params params)))
                                           (if (and (not lval)
                                                    (listp function)
                                                    (member (first function) '(apl-fn apl-fn-s))
                                                    (= 1 (length (string (second function))))
                                                    (not (of-lexicons *april-idiom*
                                                                      (aref (string (second function)) 0)
                                                                      :functions-monadic)))
                                               (error "The function ~a requires a left argument."
                                                      (string (second function)))
                                               (let ((value `(a-call ,(if (not (and (listp function)
                                                                                    (eql 'inwsd
                                                                                         (first function))))
                                                                          function `(function ,function))
                                                                     ,preceding
                                                                     ,@(if lval (list lval)))))
                                                 (if (and (listp function)
                                                          (eql 'change-namespace (second function)))
                                                     (set-namespace-point preceding space params))
                                                 (if (not remaining) value
                                                     (build-value remaining :elements (list value) :space space 
                                                                            :axes remaining-axes :params params
                                                                            :axes-last raxes-last))))))
                                     ;; the strangest part of (build-value), where
                                     ;; pivotal operators that begin with a value like +∘5 are matched
                                     (let ((exp-operator (build-operator (list (first tokens))
                                                                         :params params :space space
                                                                         :valence :pivotal :axes axes)))
                                       (if exp-operator
                                           (multiple-value-bind (composed remaining)
                                               (complete-pivotal-match
                                                exp-operator tokens nil
                                                (build-value nil :elements elements
                                                                 :params params :space space)
                                                space params nil)
                                             (cons (list :fn :pass composed) remaining))
                                           (if tokens
                                               (if (and (listp (first tokens))
                                                        (eq :op (caar tokens))
                                                        (eq :lateral (cadar tokens)))
                                                   (error
                                                    "No function found to the left of lateral operator ~a."
                                                    (third (first tokens))))
                                               preceding))))))
                             (if axes (if (or (symbolp (first tokens))
                                              (and (listp (first tokens))
                                                   (eq :fn (caar tokens))
                                                   (not (characterp (cadar tokens)))))
                                          ;; handle n-argument functions with arguments passed
                                          ;; in axis format as for fn←{[x;y;z] x+y×z} ⋄ fn[4;5;6]
                                          (let* ((first-fn (process-function (first tokens) params
                                                                             space))
                                                 (fn-form (if first-fn
                                                              (if (symbolp (first tokens))
                                                                  `(a-call #',first-fn ,@(first axes))
                                                                  `(a-call ,first-fn ,@(first axes))))))
                                            (if fn-form (build-value (rest tokens)
                                                                     :elements (cons fn-form elements)
                                                                     :space space :params params)
                                                (values nil tokens axes axes-last)))
                                          (values nil tokens axes axes-last))
                                 (values nil tokens))))))))))

(defun build-function (tokens &key axes found-function from-pivotal initial space params)
  "Construct an APL function; this may be a simple lexical function like +, an operator-composed function like +.× or a defn like {⍵+5}."
  (multiple-value-bind (function rest)
      ;; don't try function pattern matching if a function is already confirmed
      ;; or if the :ignore-patterns option is set in the params
      (if (or found-function from-pivotal (getf params :ignore-patterns)) (values nil nil)
          (match-function-patterns tokens axes space params))
    (if function (values function rest)
        (build-function-core tokens :axes axes :found-function found-function
                                    :initial initial :space space :params params))))

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
        ;; TODO: resolve issue with :dyadic args, need to build call form differently whether
        ;; there's a left argument or not
        (append (list (if is-scalar 'apl-fn-s 'apl-fn)
                      (intern (string glyph-char) *package-name-string*))
                (getf fn-meta :implicit-args)
                (if (and axes (or (getf fn-meta :axes)
                                  (eq :dyadic args)))
                    (list (if is-scalar `(apply-scalar #'- ,(caar axes) index-origin)
                              (cons 'list (first axes)))))))))

(defun build-function-core (tokens &key axes found-function initial space params)
  "Construct an APL function; this may be a simple lexical function like +, an operator-composed function like +.× or a defn like {⍵+5}."
  (let ((first-function))
    (cond ((and (first tokens) (listp (first tokens)) ;; handle enclosed functions like (,∘×)
                (not (member (caar tokens) '(:fn :op :st :pt :axes)))
                (or (not found-function)
                    (and (listp (caar tokens))
                         (not (setq first-function (build-function (first tokens)
                                                                   :space space :params params))))))
           (if found-function (values found-function tokens)
               ;; if a function is under construction, a following closure indicates that there
               ;; are no more components to the function, as with ⌽@(2∘|)⍳5
               (multiple-value-bind (sub-function remaining)
                   (build-function (first tokens) :initial t :space space :params params)
                 ;; join sub-function to other functions if present, as with ⍴∘(,∘×)
                 ;; (print (list :sf remaining sub-function (build-value remaining :space space :params params)))
                 (if sub-function ;; catch errors like (2+) 5
                     (if remaining
                         ;; if something remains to the left, check whether it's a value,
                         ;; otherwise function trains like (≠(⊢⍤/)⊢) will thro*w an error
                         (let ((left-val (build-value remaining :space space :params params)))
                           (if left-val
                               ;; if the left value is actually a passed function, compose a function
                               ;; train as for 0 1 2 3 4 5 6 7 (⍳∘1>) 4
                               (if (and (listp left-val) (listp (first left-val))
                                        (eq :fn (caar left-val)) (eq :pass (cadar left-val)))
                                   (values
                                    (compose-function-train
                                     sub-function (build-function
                                                   (list left-val)
                                                   :space space :initial initial :params params))
                                    (rest tokens))
                                   (error "Value to left of function statement."))
                               (values sub-function remaining)))
                         (build-function (rest tokens) :found-function sub-function :space space
                                                       :initial initial :params params))))))
          ((and (listp (first tokens)) (eq :axes (caar tokens)))
           (if found-function (values found-function tokens)
               (build-function (rest tokens) :axes (list (build-axes (cdar tokens) :space space :params params))
                                             :initial initial :space space :params params)))
          ((not found-function) ;; this is the beginning of the function composition
           (let* ((exp-operator (build-operator (list (first tokens))
                                                :params params :space space :initial initial
                                                :valence :lateral :axes axes))
                  ;; check whether the operator is to take a value as its left operand,
                  ;; necessary in cases like 3{1 2 ⍶ 4 5×⍵}9
                  (value-operand (or (of-meta-hierarchy (rest (getf (getf params :special) :closure-meta))
                                                        :op-syms-lval (first tokens))
                                     (and (symbolp (first tokens)) (not (getf params :special))
                                          (boundp (intern (string (first tokens)) space))
                                          (listp (symbol-value (intern (string (first tokens)) space)))
                                          (member '⍶ (getf (rest (symbol-value (intern (string (first tokens))
                                                                                       space)))
                                                           :arg-syms)))
                                     ;; check closure metadata for cases
                                     ;; like (⍳3) {q←{⍶+⍺×⍵} ⋄ ⍵(3 q)3 3 3} 5
                                     (and (listp (first tokens)) (eq :op (caar tokens))
                                          (listp (cadar tokens))
                                          (member '⍶ (getf (cdadar tokens) :arg-syms)))
                                     (and (eql '∇oself exp-operator)
                                          ;; if this is a self-reference, look for ⍶ in the list
                                          ;; of argument symbols
                                          (of-meta-hierarchy
                                           (rest (getf (getf params :special) :closure-meta))
                                           :arg-syms '⍶)))))
             (if exp-operator ;; if a lateral operator is present as for +/
                 (if value-operand
                     (multiple-value-bind (exp-value remaining)
                         (build-value (rest tokens) :space space :params params :left t)
                       (if exp-value ;; TODO: write negative clause
                           (build-function
                            remaining :space space :params params :initial initial
                                      :found-function (compose-function-lateral
                                                       exp-operator nil exp-value axes))))
                     (if (not (and (listp (second tokens)) (eq :fn (caadr tokens))
                                   (characterp (cadadr tokens)) (char= #\← (cadadr tokens))))
                         ;; if a lateral operator is encountered followed by ←, the operator is being
                         ;; assigned and the current form is not valid as a function, so return nil
                         (multiple-value-bind (exp-function remaining)
                             (build-function (rest tokens) :space space :params params)
                           (if exp-function
                               (let ((fn-wrap (if (not (and (listp exp-function)
                                                            (eql 'inwsd (first exp-function))))
                                                  exp-function `(function ,exp-function))))
                                 (build-function
                                  remaining :space space :params params :initial initial
                                  :found-function (compose-function-lateral
                                                   exp-operator fn-wrap nil axes)))
                               ;; if the operator was not followed by a function, check whether
                               ;; it's an overloaded lexical function and if so process it as such
                               (let* ((fn-token (if (and (listp (first tokens))
                                                         (characterp (third (first tokens))))
                                                    (list :fn (third (first tokens)))))
                                      (ol-function (if fn-token (process-function fn-token params
                                                                                  space))))
                                 (if ol-function (build-function
                                                  (rest tokens)
                                                  :space space :params params
                                                  :found-function (build-call-form
                                                                   ol-function :dyadic axes))
                                     (multiple-value-bind (fn-as-val-tokens fnrem)
                                         (build-value (rest tokens) :space space :params params)
                                       (if (and (listp fn-as-val-tokens)
                                                (listp (first fn-as-val-tokens))
                                                (eq :fn (caar fn-as-val-tokens))
                                                (eq :pass (cadar fn-as-val-tokens)))
                                           (multiple-value-bind (val-fn remaining)
                                               (build-function fn-as-val-tokens :space space :params params)
                                             (values (compose-function-lateral exp-operator val-fn nil axes)
                                                     remaining))
                                           (if (and (listp fnrem) (listp (first fnrem))
                                                    (eq :fn (caar fnrem))
                                                    (eq :pass (cadar fnrem)))
                                               (multiple-value-bind (val-fn remaining)
                                                   (build-function fnrem :space space :params params)
                                                 (values (compose-function-lateral exp-operator
                                                                                   val-fn nil axes)
                                                         remaining)))))))))))
                 (if (and (listp (first tokens)) (eq :fn (caar tokens))
                          (characterp (cadar tokens)) (char= #\← (cadar tokens)))
                     (values nil tokens)
                     (let ((exp-function (process-function (first tokens) params space)))
                       (if exp-function (build-function
                                         (rest tokens)
                                         :initial initial :space space :params params
                                         :found-function (if (not (characterp exp-function))
                                                             exp-function
                                                             (build-call-form exp-function :dyadic axes)))
                           #|TODO: clause for overloaded operators like /|#))))))
          ;; this clause continues a function composition that started in previous iterations
          ((and initial (listp (first tokens)) (eq :fn (caar tokens))
                (not (eq :no-assign initial))
                (characterp (cadar tokens)) (char= #\← (cadar tokens)))
           ;; if a ← is encountered, this becomes a function assignment form
           (if (third tokens) (error "Nothing can follow a function assignment.")
               (compose-function-assignment (second tokens) found-function :space space :params params)))
          (t (let ((exp-operator (build-operator (list (first tokens))
                                                 :params params :space space :initial initial
                                                 :valence :pivotal :axes axes)))
               (if exp-operator ;; if a pivotal operator is present as for +.×
                   (complete-pivotal-match exp-operator tokens found-function
                                           nil space params initial)
                   (if initial ;; if the found-function begins a clause as with (-÷,)
                       (multiple-value-bind (first-function remaining)
                           (if first-function (values first-function (rest tokens))
                               (build-function tokens :params params :space space))
                         (multiple-value-bind (second-function second-remaining)
                             (build-function remaining :params params :space space)
                           (multiple-value-bind (second-value second-val-remaining)
                               ;; build value of remaining if present; needed to compose
                               ;; for example the left-hand 1 in 0 1 2 3 4 5 6 7 (1⍳⍨>) 4
                               (if second-function (values nil nil)
                                   (build-value (or remaining (rest tokens))
                                                :params params :space space :left t))
                             ;; first function confirms an atop train like *÷
                             ;; second function confirms a three-element train like -÷,
                             ;; in either case what comes before may be a function of any complexity
                             (multiple-value-bind (second-val-fn second-val-fn-remaining)
                                 ;; if a right-value pivotal composition is at the
                                 ;; left of the train, as for (∊⍤1 , ⊣)
                                 (if (or second-function second-value
                                         (not (and (listp second-val-remaining)
                                                   (listp (first second-val-remaining))
                                                   (eq :fn (caar second-val-remaining))
                                                   (eq :pass (cadar second-val-remaining)))))
                                     (values (or second-function second-value)
                                             second-val-remaining)
                                     (build-function second-val-remaining :space space :params params))
                               (if first-function
                                   (build-function
                                    (if second-function second-remaining
                                        (if second-val-fn second-val-fn-remaining second-val-remaining))
                                    :found-function
                                    (if (or second-function second-value second-val-fn)
                                        (compose-function-train found-function first-function
                                                                (or second-function
                                                                    (if (not second-value) second-val-fn))
                                                                second-value)
                                        (compose-function-train found-function first-function))
                                    :initial initial :space space :params params)
                                   (values found-function tokens))))))
                       (values found-function tokens))))))))

(defun build-operator (tokens &key axes found-operator initial space params valence)
  "Build an operator like @ (a lexical operator), {⍺⍺/⍵} (a defined operator) or an operator assignment like p←⍣."
  (if (not found-operator) ;; no operator has yet been registered
      (if (and (listp (first tokens)) (eq :axes (caar tokens)))
          ;; concatenate axes as they are found
          (build-operator (rest tokens) :axes (cons (compile-form (cdar tokens) :space space
                                                                                :params params)
                                                    axes)
                                        :initial initial :space space :params params)
          (multiple-value-bind (op op-type)
              (process-operator (first tokens) (append params (if valence (list :valence valence)))
                                space)
            ;; register an operator when found
            (if op (build-operator (rest tokens) :axes axes :found-operator op :initial initial
                                                 :space space :params params :valence valence))))
      ;; if an operator has been registered, the only subsequent material that may still resolve
      ;; as an operator is an operator assignment like k←⌸, and only if this is the initial form
      (if (not (and initial (listp (first tokens)) (eq :fn (caar tokens))
                    (characterp (cadar tokens)) (char= #\← (cadar tokens))))
          (values found-operator tokens)
          (let* ((assign-symbol (if (getf (getf params :special) :closure-meta)
                                    (process-operator (second tokens) params space)
                                    (list 'inwsd (second tokens))))
                 (assign-sym (if (and (listp assign-symbol)
                                      (member (first assign-symbol) '(inws inwsd)))
                                 (second assign-symbol)))
                 (closure-meta (rest (getf (getf params :special) :closure-meta)))
                 (operator-type (or (getf params :valence)
                                    (if (or (member assign-sym (getf closure-meta :lop-syms))
                                            (and (characterp found-operator)
                                                 (of-lexicons *april-idiom* found-operator
                                                              :operators-lateral)))
                                        :lateral (if (or (member assign-sym (getf closure-meta :pop-syms))
                                                         (and (characterp found-operator)
                                                              (of-lexicons *april-idiom* found-operator
                                                                           :operators-pivotal)))
                                                     :pivotal)))))
            (if (and (listp found-operator) (eql 'olambda (first found-operator))
                     (getf params :special) (member '⍶ (second found-operator)))
                ;; if this operator is defined within a closure, add it to the
                ;; list of operand symbols that take a left value
                (push assign-sym (getf (rest (getf (getf params :special) :closure-meta)) :op-syms-lval)))
            (if (and (characterp found-operator)
                     (not (getf (getf params :special) :closure-meta)))
                ;; assign operator metadata for aliased operators at the top level
                (setf (symbol-value (intern (string assign-sym) space))
                      (list :meta :valence operator-type)
                      (symbol-function (intern (string assign-sym) space))
                      #'dummy-nargument-function))
            (values `(setf ,(if (getf (getf params :special) :closure-meta)
                                assign-symbol `(symbol-function ',assign-symbol))
                           ,(if (not (characterp found-operator))
                                found-operator
                                `(lambda ,(if (eq :lateral operator-type)
                                              (if axes '(operand) '(operand &optional axes))
                                              (if (eq :pivotal operator-type) '(left right)))
                                   ,@(if (and (not axes)
                                              (eq :lateral operator-type))
                                         '((declare (ignorable axes)))
                                         (if (eq :pivotal operator-type)
                                             '((declare (ignorable left right)))))
                                   ,(if axes (apply (symbol-function
                                                     (intern (format nil "APRIL-LEX-OP-~a" found-operator)
                                                             *package-name-string*))
                                                    (if (eq :lateral operator-type)
                                                        (list 'operand (if (listp (first axes))
                                                                           (cons 'list (first axes))
                                                                           `(list ,(first axes))))
                                                        (list 'right 'left)))
                                        (apply (symbol-function
                                                (intern (format nil "APRIL-LEX-OP-~a" found-operator)
                                                        *package-name-string*))
                                               (if (eq :lateral operator-type)
                                                   '(operand axes) '(right left)))))))
                    (cddr tokens))))))

(defun complete-value-assignment (tokens elements space params axes)
  "Complete the compilation of a value assignment; wraps the (compose-value-assignment) function."
  (labels ((all-symbols-p (list) ;; check whether list contains all symbols or lists of symbols
             (if (listp list) (loop :for item :in list
                                    :always (or (and (symbolp item) (not (member item '(⍺⍺ ⍵⍵))))
                                                (and (listp item) (member (first item) '(inws inwsd)))
                                                (and (listp item) (all-symbols-p item))))
                 (if (symbolp list) t))))
    (if (and (= 1 (length tokens))
             (symbolp (first tokens)))
        (build-value nil :elements
                     (list (compose-value-assignment
                            (if (and (member (first tokens)
                                             (rest (assoc :variable (idiom-symbols *april-idiom*))))
                                     (not (member (first tokens) *system-variables*)))
                                ;; intern symbols like print-precision properly for assignment
                                (first tokens)
                                (list (if (getf (getf params :special) :closure-meta)
                                          'inws 'inwsd)
                                      (first tokens)))
                            (build-value nil :axes axes :elements elements :space space :params params)
                            :params params :space space))
                         :space space :params params)
        (multiple-value-bind (function remaining)
            (if (or (= 1 (length tokens))
                    (and (= 2 (length tokens)) ;; check for trailing axes
                         (listp (first tokens)) (eq :axes (caar tokens)))
                    ;; don't do assign-by-function-result if the "values" being assigned are 
                    ;; actually functions from another workspace as for fn1 fn2 ← ⎕XWF 'fn1' 'fn2'
                    (and (listp (first elements)) (eql 'a-call (caar elements))
                         (listp (cadar elements)) (eql 'function (caadar elements))
                         (member (second (cadar elements)) '(external-workspace-function
                                                             external-workspace-operator))))
                (values nil tokens)
                (build-function tokens :space space :params params))
          (multiple-value-bind (symbol remaining2)
              ;; attempt to build a list of stranded symbols for assignment, as for d (e f)←7 (8 9)
              (build-value (if (not (and (or (symbolp function)
                                             (and (listp function)
                                                  (member (first function) '(inws inwsd))))
                                         (not remaining)))
                               remaining tokens)
                           :space space :left t :params (append (list :match-all-syms t) params))
            (multiple-value-bind (symbol remaining2)
                (if (all-symbols-p symbol) (values symbol remaining2)
                    (build-value (if (not (and (or (symbolp function)
                                                   (and (listp function)
                                                        (member (first function) '(inws inwsd))))
                                               (not remaining)))
                                     remaining tokens)
                                 :space space :left t :params params))
              ;; TODO: account for stuff after the assigned symbol
              (if (or symbol function)
                  (if (and symbol (listp symbol) (eql 'avec (first symbol))
                           (not (all-symbols-p symbol)))
                      ;; error occurs if an invalid strand assignment like ⎕←'hi' ⎕←'bye' is made
                      (error "Invalid assignment.")
                      (build-value remaining2
                                   :elements (list (compose-value-assignment
                                                    (or symbol function)
                                                    (build-value nil :axes axes :elements elements
                                                                     :space space :params params)
                                                    :params params :space space
                                                    :function (if symbol function)))
                                   :space space :params params)))))))))

(defun complete-branch-composition (tokens branch-to &key space params)
  "Complete the composition of a branch statement, either creating or optionally moving to a branch within an APL expression."
  (multiple-value-bind (branch-from-base remaining)
      (build-value tokens :space space :params params)
    (let ((branch-from (if (or (not (listp branch-from-base))
                               (not (member (first branch-from-base) '(inws inwsd))))
                           branch-from-base (second branch-from-base))))
      (values
       (if branch-to
           (progn (if (listp branch-to)
                      (if (loop :for item :in branch-to :always (and (listp item) (eql 'inws (first item))))
                          (setq branch-to (mapcar #'second branch-to))
                          (if (eql 'inws (first branch-to))
                              (setq branch-to (second branch-to)))))
                  (if (and branch-from (eql 'to-output branch-to))
                      ;; if this is a branch point statement like X→⎕, do the following:
                      (if (integerp branch-from)
                          ;; if the branch is designated by an integer like 5→⎕
                          (let ((branch-symbol (gensym "AB"))) ;; AB for APL Branch
                            (push (list branch-from branch-symbol) *branches*)
                            branch-symbol)
                          ;; if the branch is designated by a symbol like doSomething→⎕
                          (if (symbolp branch-from)
                              (progn (push branch-from *branches*)
                                     branch-from)
                              (error "Invalid left argument to →; must be a single integer value or a symbol.")))
                      ;; otherwise, this is a branch-to statement like →5 or →doSomething
                      (if (or (integerp branch-to) (symbolp branch-to))
                          ;; if the target is an explicit symbol as in →mySymbol, or explicit index
                          ;; as in →3, just pass the symbol through
                          (list 'go branch-to)
                          (if (loop :for item :in (rest branch-to)
                                    :always (or (symbolp item)
                                                (and (listp item) (eql 'inws (first item)))))
                              ;; if the target is one of an array of possible destination symbols...
                              (if (integerp branch-from)
                                  ;; if there is an explicit index to the left of the arrow,
                                  ;; grab the corresponding symbol unless the index is outside the
                                  ;; array's scope, in which case a (list) is returned so nothing is done
                                  (if (< 0 branch-from (1+ (length (rest branch-to))))
                                      ;; TODO: should this be affected by ⎕IO?
                                      (list 'go (second (nth (1- branch-from) (rest branch-to))))
                                      (list 'list))
                                  ;; otherwise, there must be an expression to the left of the arrow, as with
                                  ;; (3-2)→tagOne tagTwo, so pass it through for the postprocessor
                                  (list 'go (mapcar #'second (rest branch-to))
                                        branch-from))
                              (list 'go branch-to))))))
       remaining))))

(defun complete-pivotal-match (operator tokens right-function right-value space params initial)
  "Exension of (build-value) and (build-function) to process functions composed with pivotal operators."
  (let ((next-token (if (not (and (listp (second tokens)) (eq :op (caadr tokens))
                                  (characterp (third (second tokens)))
                                  (of-lexicons *april-idiom* (third (second tokens)) :operators-pivotal)))
                        (second tokens) (list :fn (third (second tokens))))))
    ;; if the next-token is ∘, it gets converted to its symbolic functional form;
    ;; this is needed to implement [∘. outer product]
    (multiple-value-bind (left-function remaining)
        (if (equalp next-token '(:fn #\∘))
            (values (build-function (list next-token) :space space :params params) (cddr tokens))
            (build-function (cons next-token (cddr tokens)) :space space :params params :from-pivotal t))
      ;; (build-function (cons next-token (cddr tokens)) :space space :params params)
      (multiple-value-bind (left-value remaining)
          (if left-function (values nil remaining)
              (build-value (rest tokens) :space space :params params :left t))
        (multiple-value-bind (left-function remaining)
            ;; if no value or function was confirmed to the left, it's likely that a pivotal
            ;; composition with a value on the right like +∘3 was found there, and if so its
            ;; (:fn) form will have been pushed onto the list of tokens, so process it
            (if (or left-function left-value) (values left-function remaining)
                (build-function remaining :space space :params params))
          (let ((lfn-wrap (if (not (and (listp left-function) (eql 'inwsd (first left-function))))
                              left-function `(function ,left-function)))
                (rfn-wrap (if (not (and (listp right-function) (eql 'inwsd (first right-function))))
                              right-function `(function ,right-function))))
            (if (or left-function left-value)
                (build-function remaining
                                :space space :params params :initial initial
                                :found-function (compose-function-pivotal
                                                 operator (or rfn-wrap right-value)
                                                 lfn-wrap left-value)))))))))

(defun compose-value-assignment (symbol value &key function space params)
  "Compose a value assignment like v←1 2 3."
  (cond ((eql 'to-output symbol)
         ;; a special case to handle ⎕← quad output
         `(a-out ,value :print-precision print-precision
                        :print-to output-stream :print-assignment t :with-newline t))
        ((eql 'output-stream symbol)
         ;; a special case to handle ⎕ost← setting the output stream; the provided string
         ;; is interned in the current working package
         (if (stringp value)
             ;; setq is used instead of a-set because output-stream is a lexical variable
             `(setq output-stream ,(intern value (package-name *package*)))
             (if (listp value)
                 (destructuring-bind (vector-symbol package-string symbol-string) value
                   (if (and (eql 'avec vector-symbol) (stringp package-string)
                            (stringp symbol-string))
                       ;; if the argument is a vector of two strings like ('APRIL' 'OUT-STR'),
                       ;; intern the symbol like (intern "OUT-STR" "APRIL")
                       `(setq output-stream ,(intern symbol-string package-string))
                       (error "Invalid assignment to ⎕OST.")))
                 (error "Invalid assignment to ⎕OST."))))
        ((and (listp symbol) (eql 'achoose (first symbol)))
         ;; compose indexed assignments like x[;1 2 3]←5
         (let ((val (gensym)) (var-sym (second symbol))
               (out1 (gensym)) (out2 (gensym)))
           (if (and (listp var-sym) (eql 'nspath (first var-sym)))
               `(a-set ,var-sym ,value
                       ,@(if (third symbol) (list :axes (list (rest (third (third symbol)))))))
               `(let ((,val ,value))
                  (multiple-value-bind (,out1 ,out2)
                      ,(append symbol (list :set val :modify-input t)
                               (if function `(:set-by (lambda (item item2)
                                                        (a-call ,function item item2)))))
                    (if ,out2 (setf ,var-sym ,out2))
                    ,out1)))))
        ((and (listp symbol) (eql 'a-call (first symbol)))
         ;; compose selective assignments like (3↑x)←5
         (let* ((selection-form symbol)
                (prime-function (second selection-form))
                (possible-prime-passthrough (and (listp (third selection-form))
                                                 (eql 'a-call (first (third selection-form)))
                                                 (or (symbolp prime-function)
                                                     (and (listp prime-function)
                                                          (member (first prime-function) '(inws inwsd)))
                                                     (equalp prime-function '(apl-fn '⊢)))))
                (selection-axes) (assign-sym)
                (item (gensym)))
           (labels ((set-assn-sym (form)
                      ;; get the symbol referencing the object to be reassigned,
                      ;; and fetch axes as well if present
                      (if (and (listp (third form))
                               (member (first (third form)) '(inws inwsd)))
                          (progn (setf assign-sym (third form))
                                 (setf (third form) item))
                          (if (and (listp (third form))
                                   (eql 'a-call (first (third form))))
                              (set-assn-sym (third form))
                              (if (and (listp (third form))
                                       (eql 'achoose (first (third form))))
                                  (progn (setf assign-sym (second (third form)))
                                         (setf selection-axes (third (third form)))
                                         (setf (third form) item)))))))
             (set-assn-sym selection-form)
             `(progn (a-set ,assign-sym
                            (assign-by-selection
                             ,(if (or (symbolp prime-function)
                                      (and (listp prime-function)
                                           (member (first prime-function) '(inws apl-fn function))))
                                  ;; TODO: make this work with an aliased ¨ operator
                                  prime-function
                                  (if (eql 'a-comp (first prime-function))
                                      (if (string= "¨" (string (second prime-function)))
                                          (fourth prime-function)
                                          (error "Invalid operator-composed expression ~a"
                                                 "used for selective assignment."))))
                             (lambda (,item) ,selection-form)
                             ,value ,assign-sym
                             :assign-sym (if (string= ,space (package-name (symbol-package ',assign-sym)))
                                             ',assign-sym)
                             ,@(if selection-axes (list :axes selection-axes))
                             ,@(if possible-prime-passthrough (list :secondary-prime-fn
                                                                    (second (third selection-form))))
                             ,@(if function (list :by function))))
                     ,value))))
        (t (let* ((syms (if (symbolp symbol) symbol
                            (if (and (listp symbol) (member (first symbol) '(inws inwsd)))
                                (second symbol) (if (and (listp symbol) (eql 'avec (first symbol)))
                                                    (rest symbol)))))
                  (symbols-list (if (symbolp syms) (list syms)))
                  (set-symbol (if (and (symbolp syms) (member syms '(⍺ ⍶ ⍺⍺)))
                                  syms (if (or (symbolp symbol) (and (listp symbol)
                                                                     (member (first symbol)
                                                                             '(inws inwsd))))
                                           (resolve-path symbol space params)
                                           symbol)))
                  (xfns-assigned (and (listp value) (eql 'a-call (first value))
                                      (listp (second value)) (eql 'function (caadr value))
                                      (member (cadadr value) '(external-workspace-function
                                                               external-workspace-operator)))))
             (labels ((get-symbol-list (list &optional inner)
                        (let ((valid t))
                          (if (listp list)
                              ;; build list of symbols to be assigned values
                              ;; (multiple for stranded/nested assignment)
                              (let ((out-list
                                      (loop :while valid :for i
                                              :in (if (and (not inner)
                                                           (not (eql 'avec (first list))))
                                                      list (rest list))
                                            :collect (setq valid
                                                           (if (symbolp i)
                                                               (progn (if (not (member i symbols-list))
                                                                          (setq symbols-list
                                                                                (cons i symbols-list)))
                                                                      i)
                                                               (if (and (listp i) (member (first i)
                                                                                          '(inws inwsd)))
                                                                   (progn (setq symbols-list
                                                                                (cons (second i)
                                                                                      symbols-list))
                                                                          i)
                                                                   (get-symbol-list i t)))))))
                                (if valid out-list))))))
               (or symbols-list (get-symbol-list syms))
               (if symbols-list
                   (progn (loop :for symbol :in symbols-list
                                :do (let ((insym (intern (string symbol) space)))
                                      (if (and (is-workspace-function symbol)
                                               (not (getf (getf params :special) :closure-meta)))
                                          ;; unbind the symbol as for a function if this
                                          ;; variable assignment is made at the top level
                                          (fmakunbound insym))
                                      (if (and (not (boundp insym))
                                               (not (member symbol (getf (rest (getf (getf params :special)
                                                                                     :closure-meta))
                                                                         :var-syms))))
                                          ;; only bind dynamic variables in the workspace if the
                                          ;; compiler is at the top level; i.e. not within a
                                          ;; { function }, where bound variables are lexical
                                          (progn (proclaim (list 'special insym))
                                                 (if xfns-assigned (setf (symbol-function insym)
                                                                         #'dummy-nargument-function)
                                                     (set insym nil))))))
                          (if (and function (not xfns-assigned))
                              (if (listp syms) ;; handle namespace paths
                                  `(a-set ,symbol ,value :by (lambda (item item2)
                                                               (a-call ,function item item2)))
                                  (let ((assigned (gensym)))
                                    `(let ((,assigned (a-call ,function ,value ,symbol)))
                                       (if (boundp (quote (inwsd ,(intern (string syms)))))
                                           (setf (symbol-value (quote (inwsd ,(intern (string syms)))))
                                                 ,assigned))
                                       (setq ,symbol ,assigned))))
                              `(a-set ,set-symbol ,value)))))))))

(defun compose-function-assignment (symbol function &key space params)
  "Compose a function assignment."
  (let ((symbol (resolve-path symbol space params)))
    (if (symbolp symbol) ;; handle the case of a function assigned to a symbol like f←{⍵+1}
        (let ((i-sym (intern (string symbol) space))
              (in-closure (getf (getf params :special) :closure-meta)))
          (if (boundp i-sym) (makunbound i-sym))
          (if (and (not in-closure)
                   (not (member symbol '(⍺ ⍺⍺)))) ;; don't bind assignments to argument symbols
              (setf (symbol-function i-sym) #'dummy-nargument-function))
          (if (and (listp function) (symbolp symbol)
                   (member (first function) '(lambda a-comp))
                   (member symbol (getf (rest (getf (getf params :special) :closure-meta)) :var-syms)))
              ;; for function assignments that aren't apparent to the lexer postprocessor like
              ;; fn←×∘3, it's necessary to remove their references as variables in the closure
              ;; metadata and create references to them as function symbols; this means that
              ;; things like g←{fn/⍳⍵} ⋄ fn←×∘3 won't work as the fn definition needs to come first
              (progn (setf (getf (rest (getf (getf params :special) :closure-meta)) :var-syms)
                           (remove symbol (getf (rest (getf (getf params :special) :closure-meta))
                                                :var-syms)))
                     (if (and (getf (getf params :special) :closure-meta)
                              (not (member symbol (getf (rest (getf (getf params :special) :closure-meta))
                                                        :fn-syms))))
                         (push symbol (getf (rest (getf (getf params :special) :closure-meta)) :fn-syms)))))
          `(a-set ,(if (eql '⍺ symbol) ;; handle the ⍺←function case
                       symbol (if in-closure `(inws ,symbol)                    
                                  `(symbol-function '(inwsd ,symbol))))
                  ,function))
        (if (and (listp symbol) (eql 'nspath (first symbol)))
            ;; handle the case of function assignment within a namespace like a.bc←{⍵+1}
            (let ((path-symbol (intern (format-nspath (rest symbol)) space)))
              (setf (symbol-function path-symbol) #'dummy-nargument-function)
              `(progn (a-set ,symbol :function)
                      (setf (symbol-function ',path-symbol) ,function)))))))

(defun compose-function-lateral (operator function value axes)
  "Compose a function using a lateral operator like [/ reduce]."
  (if (characterp operator)
      (append (list 'a-comp (intern (string operator)))
              (funcall (symbol-function (intern (format nil "APRIL-LEX-OP-~a" operator) *package-name-string*))
                       function `(list ,@(first axes))))
      `(a-comp :op ,(if (or (not (symbolp operator))
                            (eql '∇oself operator))
                        operator `(inws ,operator))
               ,(or function value)
               ,@(if axes `((list ,@(first axes)))))))
  
(defun compose-function-pivotal (operator function1 function2 value2)
  "Compose a function using a pivotal operator like [⍣ power]."
  (if (characterp operator)
      (append (list 'a-comp (intern (string operator)))
              (funcall (symbol-function (intern (format nil "APRIL-LEX-OP-~a" operator) *package-name-string*))
                       function1 (or function2 value2)))
      `(a-comp :op ,(if (not (symbolp operator))
                        operator `(inws ,operator))
               ,(or function2 value2) ,function1)))

(defun compose-function-train (right center &optional left left-value)
  "Compose a function train like (-,÷)."
  (let ((omega (gensym)) (alpha (gensym))
        (right (if (not (and (listp right) (eql 'inwsd (first right))))
                   right `(function ,right)))
        (center (if (not (and (listp center) (eql 'inwsd (first center))))
                    center `(function ,center)))
        (left (if (not (and (listp left) (eql 'inwsd (first left))))
                  left `(function ,left))))
    `(lambda (,omega &optional ,alpha)
       (if ,alpha (a-call ,center (a-call ,right ,omega ,alpha)
                          ,@(if left-value (list left-value)
                                (if left `((a-call ,left ,omega ,alpha)))))
           (a-call ,center (a-call ,right ,omega)
                   ,@(if left-value (list left-value)
                         (if left `((a-call ,left ,omega)))))))))

(defun fnexp-backup (form &key space params)
  "If a value build produces an pivotal function composition, it is built as a function. Needed for cases like fn←{2+⍵}⍣3 ⋄ fn 5."
  (if (not (and (listp form) (listp (first form)) (eq :fn (caar form))
                (eq :pass (cadar form))))
      form (build-function form :initial t :space space :params params)))

(defun compile-form (exprs &key space params)
  "Compile a series of APL expressions."
  (flet ((wrap-fn-sym (form)
           (if (not (and (listp form) (eql 'inwsd (first form))))
               form (list 'function form))))
    (loop :for expr :in exprs
          :collect (or (multiple-value-bind (value remaining)
                           (build-value expr :space space :params params)
                         (if value (fnexp-backup value :space space :params params)
                             (if (and remaining (listp remaining) (listp (first remaining))
                                      (eq :fn (caar remaining)) (eq :pass (cadar remaining)))
                                 ;; handle cases like fn←~.(≤∘0) where the
                                 ;; first element is a value-right composition
                                 (build-function remaining :initial t :space space :params params))))
                       (wrap-fn-sym (build-function expr :initial t :space space :params params))
                       (build-operator expr :initial t :space space :params params)))))
