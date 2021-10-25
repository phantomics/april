
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
        ;; TODO: resolve issue with :dyadic args, need to build call form differently whether
        ;; there's a left argument or not
        ;; (print (list :ff glyph-char fn-meta))
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
         (let ((path-val (or (getf (rest (getf (getf properties :special) :closure-meta)) :ns-point)
                             (symbol-value (intern "*NS-POINT*" space)))))
           (if (not (member (intern (string-upcase this-item) *package-name-string*)
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
                   this-item (cons 'nspath (append path-val
                                                   (list (intern (string this-item) "KEYWORD")))))
               (intern (string-upcase this-item)))))
        ((and (symbolp this-item)
              (getf properties :match-all-syms))
         this-item)))

(defun proc-function (this-item &optional properties process idiom space)
  "Process a function token."
  ;; (print (list :ti this-item properties))
  (let* ((current-path (or (getf (rest (getf (getf properties :special) :closure-meta)) :ns-point)
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
                          (of-lexicons idiom fn :functions)
                          (or (not (getf properties :glyph))
                              (and (char= fn (aref (string (getf properties :glyph)) 0)))))
                     (values fn (list :type (append '(:function :glyph)
                                                    (if overloaded-operator '(:overloaded-operator))
                                                    (if (of-lexicons idiom (third this-item)
                                                                     :functions-symbolic)
                                                        '(:symbolic-function))))))
                    ((and (listp fn)
                          (not (getf properties :glyph)))
                     (if (eq :pass (second this-item))
                         ;; handle a (:fn)-enclosed operator form produced by build-value
                         (values fn)
                         (let* ((polyadic-args (if (and (listp (first (last (first fn))))
                                                        (eq :axes (caar (last (first fn)))))
                                                   (mapcar #'caar (cdar (last (first fn))))))
                                ;; (polyadic-args)
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
                                 (values (output-function
                                          (compile-form fn :space space
                                                           :params (list :special
                                                                         (list :closure-meta
                                                                               (second this-item))))
                                          polyadic-args (rest this-closure-meta))
                                         (list :type '(:function :closure)
                                               :obligate-dyadic obligate-dyadic)))))))
                     (t (values nil nil)))))
        (if (and (symbolp this-item)
                 (not (getf properties :glyph)))
            (cond ((and current-path (fboundp (intern (format-nspath (append current-path (list this-item)))
                                                      space)))
                   (intern (format-nspath (append current-path (list (intern (string this-item)
                                                                             "KEYWORD"))))))
                  ((and (is-workspace-function this-item)
                        ;; make sure it's not defined locally as a variable
                        (not (member this-item (of-meta-hierarchy (rest (getf (getf properties :special)
                                                                              :closure-meta))
                                                                  :var-syms)))
                        (not (or (member this-item (of-meta-hierarchy
                                                    (rest (getf (getf properties :special) :closure-meta))
                                                    :fn-syms))
                                 (member this-item (of-meta-hierarchy
                                                    (rest (getf (getf properties :special) :closure-meta))
                                                    :lop-syms))
                                 (member this-item (of-meta-hierarchy
                                                    (rest (getf (getf properties :special) :closure-meta))
                                                    :pop-syms)))))
                   ;; process workspace-aliased lexical functions, as when f←+ has been set
                   `(inwsd ,this-item))
                  ((eql this-item '∇)
                   '#'∇self)
                  ((and (member this-item '(⍵⍵ ⍺⍺ ⍺)) ;; recall ⍺ may be a function following ⍺←function
                        (or (not (eql '⍺ this-item))
                            (member this-item (of-meta-hierarchy (rest (getf (getf properties :special)
                                                                             :closure-meta))
                                                                 :fn-syms))))
                   this-item)
                  ((member this-item (of-meta-hierarchy (rest (getf (getf properties :special) :closure-meta))
                                                        :fn-syms))
                   (if (eql '⍺ this-item) this-item (list 'inws this-item)))
                  ((member (intern (string-upcase this-item) *package-name-string*)
                           (rest (assoc :function (idiom-symbols idiom))))
                   (let ((idiom-function-object (getf (rest (assoc :function (idiom-symbols idiom)))
                                                      (intern (string-upcase this-item)
                                                              *package-name-string*))))
                     (if (listp idiom-function-object)
                         idiom-function-object (list 'function idiom-function-object)))))))))

(defun proc-operator (this-item &optional properties process idiom space)
  "Process an operator token."
  (declare (ignore idiom))
  ;; (print (list :pi this-item))
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
                  ;; return the operator self-reference function
                  '∇oself (cond ((and valid-by-valence (getf properties :glyph))
                                 (if (char= op-symbol (aref (string (getf properties :glyph)) 0))
                                     op-symbol))
                                (valid-by-valence op-symbol)))))
          (if (and (eql :op (first this-item))
                   (listp (first (last this-item)))
                   (not (getf properties :glyph)))
              (let* ((fn (first (last this-item)))
                     (arg-symbols (intersection '(⍺ ⍵ ⍶ ⍹ ⍺⍺ ⍵⍵ ∇∇) (getf (cdadr this-item) :arg-syms)))
                     (this-closure-meta (second this-item))
                     (is-inline (intersection arg-symbols '(⍶ ⍹ ⍺⍺ ⍵⍵)))
                     (is-dyadic (member '⍺ arg-symbols))
                     (is-pivotal (intersection arg-symbols '(⍹ ⍵⍵)))
                     (valence (getf properties :valence)))
                (if (= 2 (length (intersection arg-symbols '(⍶ ⍺⍺))))
                    (error "A defined operator may not include both [⍶ left value] and~a"
                           " [⍺⍺ left function] operands."))
                (if (= 2 (length (intersection arg-symbols '(⍹ ⍵⍵))))
                    (error "A defined operator may not include both [⍹ right value] and~⍺"
                           " [⍵⍵ right function] operands."))
                (if is-inline (if (or (not valence)
                                      (and is-pivotal (eq :pivotal valence))
                                      (and (not is-pivotal) (eq :lateral valence)))
                                  (let ((sub-props (list :special (list :closure-meta this-closure-meta))))
                                    (output-function
                                     (compile-form fn :space space
                                                      :params (list :special
                                                                    (list :closure-meta
                                                                          (second this-item))))
                                     nil (rest this-closure-meta))))))))
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
                     (not (member this-item ;; make sure it's not defined locally as a variable
                                  (of-meta-hierarchy (rest (getf (getf properties :special)
                                                                 :closure-meta))
                                                     :var-syms)))
                     (or (and (fboundp (intern lop-string space))
                              (boundp (intern lop-string space))
                              (listp (symbol-value (intern lop-string space)))
                              (eq :lateral (getf (rest (symbol-value (intern lop-string space)))
                                                 :valence)))
                         (member this-item (of-meta-hierarchy closure-meta :lop-syms))))
                (if (not (member this-item (of-meta-hierarchy closure-meta :lop-syms)))
                    (list 'inwsd (intern lop-string))
                    (list 'inws (intern lop-string)))
                (if (and pop-string
                         (or (and (fboundp (intern pop-string space))
                                  (boundp (intern pop-string space))
                                  (listp (symbol-value (intern pop-string space)))
                                  (eq :pivotal (getf (rest (symbol-value (intern pop-string space)))
                                                     :valence)))
                             (member this-item (of-meta-hierarchy closure-meta :pop-syms))))
                    (if (not (member this-item (of-meta-hierarchy closure-meta :pop-syms)))
                        (list 'inwsd (intern pop-string))
                        (list 'inws (intern pop-string)))))))))

(defun build-axes (elements &key space params)
  ;; (print (list :par params))
  (loop :for element :in elements
        :collect (let ((item-out (compile-form element :space space :params params)))
                   (if (= 1 (length element))
                       (first item-out) (cons 'progn item-out)))))

(defun build-value (tokens &key axes elements space params left axes-last)
  ;; (print (list :to tokens axes elements space params axes-last))
  (if (not tokens) ;; if no tokens are left and value elements are present, generate an output value
      (if elements (enclose-axes (output-value space (if (< 1 (length elements)) elements (first elements))
                                               (loop :for i :below (length elements) :collect nil)
                                               (of-meta-hierarchy (rest (getf (getf params :special)
                                                                              :closure-meta))
                                                                  :var-syms))
                                 axes))
      (if (and (not left) (listp (first tokens)) (eq :fn (caar tokens))
               (characterp (cadar tokens)) (char= #\← (cadar tokens)))
          ;; if a ← is encountered, this is a value assignment form
          (complete-value-assignment (rest tokens) elements space params axes)
          (if (and (listp (first tokens)) (eq :axes (caar tokens))) ;; if axes like [2] are encountered
              ;; if elements are present, recurse and process the items after the axes as another
              ;; vector for the axes to be applied to
              (if elements
                  (if left (values (build-value nil :axes axes :space space :axes-last t
                                                    :left left :params params :elements elements)
                                   tokens)
                      (if axes (build-value nil :elements (cons (build-value tokens :space space
                                                                                    :params params)
                                                                elements)
                                                :axes axes)
                          (build-value (rest tokens) :elements elements :space space :params params
                                                     :axes (cons (build-axes (cdar tokens)
                                                                             :space space :params params)
                                                                 axes)
                                                     :axes-last t)))
                  ;; if no elements preceded the axes, just pass the axes to the next
                  ;; level of recursion to be applied to the next element encountered
                  (build-value (rest tokens) :axes (cons (build-axes (cdar tokens)
                                                                     :space space :params params)
                                                         axes)
                                             :axes-last t :space space :params params :left left))
              (if (and (listp (first tokens)) (eq :st (caar tokens))) ; 1 2 3∘.⌽[1]⊂2 3 4⍴⍳9
                  (let ((stm (funcall (symbol-function (intern (format nil "APRIL-LEX-ST-~a" (caddar tokens))
                                                               *package-name-string*))
                                      (first axes))))
                    ;; (print (list :aaa tokens stm left))
                    (build-value (rest tokens) :space space :params params
                                               :left left :elements (cons stm elements)))
                  (let* ((is-closure (and (first tokens) (listp (first tokens))
                                          (not (member (caar tokens) '(:fn :op :st :pt :axes)))))
                         ;; handle enclosed values like (1 2 3)
                         (first-value (if is-closure (build-value (first tokens) :space space :params params)
                                          (proc-value (first tokens) params nil *april-idiom* space))))
                    ;; (print (list :fv is-closure left first-value tokens elements axes))
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
                                (build-value (cons (first first-value) (rest tokens))
                                             :elements elements :params params :space space :axes axes))
                            (let ((fv-output (output-value space first-value (list nil)
                                                           (of-meta-hierarchy (rest (getf (getf params :special)
                                                                                          :closure-meta))
                                                                              :var-syms))))
                              (build-value (rest tokens)
                                           :elements (cons (if (not (and axes is-closure))
                                                               first-value fv-output)
                                                           elements)
                                           :params params :space space :left left :axes axes)))
                        (if left
                            ;; if this value is to the left of a function like the 5 in 5+3,
                            ;; process it without looking for a function to its left
                            (let ((exp-operator (build-operator ;; TODO: is this clause used?
                                                 (list (first tokens)) :params params :space space
                                                                       :valence :pivotal :axes axes)))
                              ;; (print (list :ax exp-operator axes elements))
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
                                  ;; (print (list :iii preceding tokens elements axes-last axes))
                                  (multiple-value-bind (function remaining)
                                      ;; apply axes to the found function if they were encountered last,
                                      ;; as with 1 2 3∘.⌽[1]⊂2 3 4⍴⍳9
                                      (build-function tokens :space space :params params
                                                             :axes (if axes-last axes))
                                    ;; (print (list :ff function remaining tokens elements axes axes-last))
                                    (if function
                                        (multiple-value-bind (lval remaining remaining-axes raxes-last)
                                            (build-value remaining :space space :params params :left t)
                                          ;; (print (list :llv lval remaining function axes
                                          ;;              remaining-axes))
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
                                              ;; (print (list :ll axes remaining remaining-axes))
                                              (let ((value `(a-call ,(if (not (and (listp function)
                                                                                   (eql 'inwsd
                                                                                        (first function))))
                                                                         function `(function ,function))
                                                                    ,preceding
                                                                    ,@(if lval (list lval)))))
                                                ;; (print (list :rem remaining value function
                                                ;;              preceding axes))
                                                (if (not remaining) value
                                                    (build-value remaining :elements (list value)
                                                                           :axes remaining-axes
                                                                           :axes-last raxes-last
                                                                           :space space :params params)))))
                                        ;; the strangest part of (build-value), where
                                        ;; pivotal operators that begin with a value like +∘5 are matched
                                        (let ((exp-operator (build-operator
                                                             (list (first tokens))
                                                             :params params :space space
                                                             :valence :pivotal :axes axes)))
                                          (if exp-operator
                                              (multiple-value-bind (composed remaining)
                                                  (complete-pivotal-match
                                                   exp-operator tokens nil
                                                   (build-value nil :elements elements
                                                                    :params params :space space)
                                                   space params nil)
                                                ;; (print (list :rem remaining))
                                                ;; (print (list :comp tokens composed elements))
                                                (cons (list :fn :pass composed) remaining))
                                              preceding)))))
                                (if axes (if (or (symbolp (first tokens))
                                                 (and (listp (first tokens))
                                                      (eq :fn (caar tokens))
                                                      (not (characterp (cadar tokens)))))
                                             ;; handle n-argument functions with arguments passed
                                             ;; in axis format as for fn←{[x;y;z] x+y×z} ⋄ fn[4;5;6]
                                             (let* ((first-fn (proc-function (first tokens) params
                                                                             nil *april-idiom* space))
                                                    (fn-form (if first-fn
                                                                 (if (symbolp (first tokens))
                                                                     `(a-call #',first-fn ,@(first axes))
                                                                     `(a-call ,first-fn ,@(first axes))))))
                                               (if fn-form (build-value (rest tokens)
                                                                        :elements (cons fn-form elements)
                                                                        :space space :params params)
                                                   (values nil tokens axes axes-last)))
                                             (values nil tokens axes axes-last))
                                    (values nil tokens)))))))))))

(defun all-symbols-p (list)
  (if (listp list)
      (loop :for item :in list
            :always (or (and (symbolp item) (not (member item '(⍺⍺ ⍵⍵))))
                        (and (listp item) (member (first item) '(inws inwsd)))
                        (and (listp item) (all-symbols-p item))))
      (if (symbolp list) t)))

(defun complete-value-assignment (tokens elements space params axes)
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
                          (build-value nil :axes axes :elements elements
                                           :space space :params params)
                          :params params :space space))
                       :space space :params params)
      (multiple-value-bind (function remaining)
          (if (= 1 (length tokens)) (values nil tokens)
              (build-function tokens :space space :params params))
        (multiple-value-bind (symbol remaining2)
            ;; attempt to build a list of stranded symbols for assignment, as for d (e f)←7 (8 9)
            (or (build-value (if (not (and (or (symbolp function)
                                               (and (listp function)
                                                    (member (first function) '(inws inwsd))))
                                           (not remaining)))
                                 remaining tokens)
                             :space space :left t :params (append (list :match-all-syms t) params)))
          (multiple-value-bind (symbol remaining2)
              (if (all-symbols-p symbol) (values symbol remaining2)
                  (build-value (if (not (and (or (symbolp function)
                                                 (and (listp function)
                                                      (member (first function)
                                                              '(inws inwsd))))
                                             (not remaining)))
                                   remaining tokens)
                               :space space :left t :params params))
            ;; (setq symbol function)
            ;; (print (list :ss symbol function remaining2 params (all-symbols-p symbol)))
            ;; TODO: account for stuff after the assigned symbol
            ;; (print (list :rem remaining))
            (if (or symbol function)
                (build-value remaining2
                             :elements
                             (list (compose-value-assignment
                                    (or symbol function)
                                    (build-value nil :axes axes :elements elements
                                                     :space space :params params)
                                    :params params :space space :function (if symbol function)))
                             :space space :params params)))))))

(defun build-function (tokens &key axes found-function initial space params)
  ;; (print (list :to tokens found-function))
  (if (and (first tokens) (listp (first tokens)) ;; handle enclosed functions like (,∘×)
           (not (member (caar tokens) '(:fn :op :st :pt :axes))))
      (if found-function (values found-function tokens)
          ;; if a function is under construction, a following closure indicates that there
          ;; are no more components to the function, as with ⌽@(2∘|)⍳5
          (let ((sub-function (build-function (first tokens) :initial t :space space :params params)))
            ;; join sub-function to other functions if present, as with ⍴∘(,∘×)
            (if sub-function (build-function (rest tokens) :found-function sub-function :space space
                                                           :initial initial :params params))))
      (if (and (listp (first tokens)) (eq :axes (caar tokens)))
          (if found-function (values found-function tokens)
              (build-function (rest tokens) :axes (list (build-axes (cdar tokens)
                                                                    :space space :params params))
                                            :initial initial :space space :params params))
          (if (not found-function) ;; this is the beginning of the function composition
              (let ((exp-operator (build-operator (list (first tokens))
                                                  :params params :space space :initial initial
                                                  :valence :lateral :axes axes))
                    ;; check whether the operator is to take a value as its left operand,
                    ;; necessary in cases like 3{1 2 ⍶ 4 5×⍵}9
                    (value-operand (or (member (first tokens)
                                               (of-meta-hierarchy (rest (getf (getf params :special)
                                                                              :closure-meta))
                                                                  :op-syms-lval))
                                       ;; check closure metadata for cases
                                       ;; like (⍳3) {q←{⍶+⍺×⍵} ⋄ ⍵(3 q)3 3 3} 5
                                       (and (listp (first tokens)) (eq :op (caar tokens))
                                            (listp (cadar tokens))
                                            (member '⍶ (getf (cdadar tokens) :arg-syms))))))
                ;; (print (list :vl exp-operator value-operand tokens params))
                (if exp-operator ;; if a lateral operator is present as for +/
                    (if value-operand
                        (multiple-value-bind (exp-value remaining)
                            (build-value (rest tokens) :space space :params params :left t)
                          (if exp-value ;; TODO: write negative clause
                              (build-function remaining :space space :params params :initial initial
                                                        :found-function (compose-function-lateral
                                                                         exp-operator nil exp-value axes))))
                        (if (not (and (listp (second tokens)) (eq :fn (caadr tokens))
                                      (characterp (cadadr tokens)) (char= #\← (cadadr tokens))))
                            ;; if a lateral operator is encountered followed by ←, the operator is
                            ;; being assigned and the current form is not valid as a function, so return nil
                            (multiple-value-bind (exp-function remaining)
                                (build-function (rest tokens) :space space :params params)
                              ;; (print (list :ex exp-operator exp-function tokens
                              ;;              (build-function (rest tokens) :space space :params params)
                              ;;              :rem remaining))
                              (if exp-function
                                  (let ((fn-wrap (if (not (and (listp exp-function)
                                                               (eql 'inwsd (first exp-function))))
                                                     exp-function `(function ,exp-function))))
                                    (build-function remaining :space space :params params :initial initial
                                                              :found-function (compose-function-lateral
                                                                               exp-operator fn-wrap nil axes)))
                                  ;; if the operator was not followed by a function, check
                                  ;; whether it's an overloaded lexical function and if so process it as such
                                  (let* ((fn-token (if (characterp (third (first tokens)))
                                                       (list :fn (third (first tokens)))))
                                         (ol-function (if fn-token
                                                          (proc-function fn-token params nil
                                                                         *april-idiom* space))))
                                    ;; (print (list :oo ol-function (build-call-form ol-function) (rest tokens)))
                                    (if ol-function
                                        (build-function (rest tokens)
                                                        :space space :params params
                                                        :found-function (build-call-form
                                                                         ol-function :dyadic axes))
                                        (multiple-value-bind (fn-as-val-tokens fnrem)
                                            (build-value (rest tokens) :space space :params params)
                                          ;; (print (list :ff fn-as-val-tokens remaining fnrem))
                                          (if (and (listp fn-as-val-tokens) (listp (first fn-as-val-tokens))
                                                   (eq :fn (caar fn-as-val-tokens))
                                                   (eq :pass (cadar fn-as-val-tokens)))
                                              (multiple-value-bind (val-fn remaining)
                                                  (build-function fn-as-val-tokens :space space
                                                                                   :params params)
                                                (values (compose-function-lateral
                                                         exp-operator val-fn nil axes)
                                                        remaining))
                                              (if (and (listp fnrem) (listp (first fnrem))
                                                       (eq :fn (caar fnrem))
                                                       (eq :pass (cadar fnrem)))
                                                  (multiple-value-bind (val-fn remaining)
                                                      (build-function fnrem :space space
                                                                            :params params)
                                                    (values (compose-function-lateral
                                                             exp-operator val-fn nil axes)
                                                            remaining)))))))))))
                    (if (and (listp (first tokens)) (eq :fn (caar tokens))
                             (characterp (cadar tokens)) (char= #\← (cadar tokens)))
                        (values nil tokens)
                        (let ((exp-function (proc-function (first tokens) params nil *april-idiom* space)))
                          ;; (print (list :ex exp-function tokens space))
                          (if exp-function (build-function
                                            (rest tokens)
                                            :initial initial :space space :params params
                                            :found-function (if (not (characterp exp-function))
                                                                exp-function
                                                                (build-call-form exp-function :dyadic axes)))
                              #|TODO: clause for overloaded operators like /|#)))))
              ;; this clause continues a function composition that started in previous iterations
              (if (and initial (listp (first tokens)) (eq :fn (caar tokens))
                       (characterp (cadar tokens)) (char= #\← (cadar tokens)))
                  ;; if a ← is encountered, this becomes a function assignment form
                  (if (third tokens) (error "Nothing can follow a function assignment.")
                      (compose-function-assignment (second tokens) found-function
                                                   :space space :params params))
                  (let ((exp-operator (build-operator (list (first tokens))
                                                      :params params :space space :initial initial
                                                      :valence :pivotal :axes axes)))
                    ;; (print (list :eex tokens exp-operator initial))
                    (if exp-operator ;; if a pivotal operator is present as for +.×
                        (complete-pivotal-match exp-operator tokens found-function nil space params initial)
                        (if initial ;; if the found-function begins a clause as with (-÷,)
                            (multiple-value-bind (first-function remaining)
                                (build-function tokens :params params :space space)
                              (multiple-value-bind (second-function second-remaining)
                                  (build-function remaining :params params :space space)
                                (multiple-value-bind (second-value second-val-remaining)
                                    (if second-function (values nil nil)
                                        (build-value (rest tokens) :params params :space space :left t))
                                  ;; (print (list :ff first-function second-function second-value
                                  ;;              found-function second-val-remaining))
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
                                    ;; (print (list :ss second-function second-value second-val-fn
                                    ;;              second-val-fn-remaining second-remaining
                                    ;;              second-val-remaining))
                                    (if first-function
                                        (build-function
                                         (if second-function second-remaining
                                             (if second-val-fn second-val-fn-remaining
                                                 second-val-remaining))
                                         :found-function
                                         (if (or second-function second-value second-val-fn)
                                             (compose-function-train found-function first-function
                                                                     ;; second-function
                                                                     (or second-function
                                                                         (if (not second-value)
                                                                             second-val-fn))
                                                                     second-value)
                                             (compose-function-train found-function first-function))
                                         :initial initial :space space :params params)
                                        (values found-function (or tokens ;; :ggg
                                                                   )))))))
                            (values found-function (or tokens ;; :hhh
                                                       ))))))))))

(defun build-operator (tokens &key axes found-operator initial space params valence)
  (if (not found-operator) ;; no operator has yet been registered
      (if (and (listp (first tokens)) (eq :axes (caar tokens)))
          ;; concatenate axes as they are found
          (build-operator (rest tokens) :axes (cons (compile-form (cdar tokens)) axes)
                                        :initial initial :space space :params params)
          (let ((op (proc-operator (first tokens) (append params (if valence (list :valence valence)))
                                   nil *april-idiom* space)))
            ;; register an operator when found
            (if op (build-operator (rest tokens) :axes axes :found-operator op :initial initial
                                                 :space space :params params :valence valence))))
      ;; if an operator has been registered, the only subsequent material that may still resolve
      ;; as an operator is an operator assignment like k←⌸, and only if this is the initial form
      (if (and initial (listp (first tokens)) (eq :fn (caar tokens))
               (characterp (cadar tokens)) (char= #\← (cadar tokens)))
          (let* ((assign-symbol (proc-operator (second tokens) params nil *april-idiom* space))
                 (assign-sym (if (and (listp assign-symbol)
                                      (member (first assign-symbol) '(inws inwsd)))
                                 (second assign-symbol)))
                 (closure-meta (rest (getf (getf params :special) :closure-meta)))
                 (operator-type (or (getf params :valence)
                                    (if (member assign-sym (getf closure-meta :lop-syms))
                                        :lateral (if (member assign-sym (getf closure-meta :pop-syms))
                                                     :pivotal)))))
            ;; (print (list :fo found-operator params assign-sym))
            (if (and (listp found-operator) (eql 'olambda (first found-operator))
                     (member '⍶ (second found-operator)))
                (push assign-sym (getf (rest (getf (getf params :special) :closure-meta)) :op-syms-lval)))
            ;; (print (list :tt params (second tokens) assign-symbol))
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
                                   ,(if axes
                                        (apply (symbol-function
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
                    (cddr tokens)))
          (values found-operator tokens))))

(defun complete-pivotal-match (operator tokens right-function right-value space params initial)
  "Exension of (build-value) and (build-function) to process functions composed with pivotal operators."
  ;; (print (list :op operator tokens right-function))
  (let ((next-token (if (not (and (listp (second tokens)) (eq :op (caadr tokens))
                                  (characterp (third (second tokens)))
                                  (of-lexicons *april-idiom* (third (second tokens)) :operators-pivotal)))
                        (second tokens) (list :fn (third (second tokens))))))
    ;; if the next-token is ∘, it gets converted to its symbolic functional form;
    ;; this is needed to implement [∘. outer product]
    (multiple-value-bind (left-function remaining)
        (build-function (cons next-token (cddr tokens)) :space space :params params)
      ;; (print (list :ll left-function remaining))
      (multiple-value-bind (left-value remaining)
          (if left-function (values nil remaining)
              (build-value (rest tokens) :space space :params params :left t))
        ;; (print (list :ll left-value left-function remaining))
        (multiple-value-bind (left-function remaining)
            ;; if no value or function was confirmed to the left, it's likely that a pivotal
            ;; composition with a value on the right like +∘3 was found there, and if so its
            ;; (:fn) form will have been pushed onto the list of tokens, so process it
            (if (or left-function left-value) (values left-function remaining)
                (build-function remaining :space space :params params))
          (let ((lfn-wrap (if (not (and (listp left-function)
                                        (eql 'inwsd (first left-function))))
                              left-function `(function ,left-function)))
                (rfn-wrap (if (not (and (listp right-function)
                                        (eql 'inwsd (first right-function))))
                              right-function `(function ,right-function))))
            (if (or left-function left-value)
                (build-function remaining
                                :space space :params params :initial initial
                                :found-function (compose-function-pivotal
                                                 operator (or rfn-wrap right-value)
                                                 lfn-wrap left-value)))))))))

(defun compose-value-assignment (symbol value &key function space params)
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
                   (if (and (eql 'avector vector-symbol) (stringp package-string)
                            (stringp symbol-string))
                       ;; if the argument is a vector of two strings like ('APRIL' 'OUT-STR'),
                       ;; intern the symbol like (intern "OUT-STR" "APRIL")
                       `(setq output-stream ,(intern symbol-string package-string))
                       (error "Invalid assignment to ⎕OST.")))
                 (error "Invalid assignment to ⎕OST."))))
        ((and (listp symbol) (eql 'achoose (first symbol)))
         (let ((val (gensym)) (var-sym (second symbol))
               (out1 (gensym)) (out2 (gensym)))
           `(let ((,val ,value))
              (multiple-value-bind (,out1 ,out2)
                  ,(append symbol (list :set val :modify-input t)
                           (if function `(:set-by (lambda (item item2)
                                                    (a-call ,function item item2)))))
                (if ,out2 (setf ,var-sym ,out2))
                ,out1))))
        ((and (listp symbol) (eql 'a-call (first symbol)))
         ;; (print (list :sym symbol))
         (let* ((val-sym (if (symbolp (first symbol)) (first symbol)))
                (selection-form symbol)
                (context-vars (getf (rest (getf (getf params :special) :closure-meta)) :var-syms))
                (val-sym-form (list (if (and (boundp (intern (string val-sym) space))
                                             (not (member val-sym context-vars)))
                                        'inwsd 'inws)
                                    val-sym))
                (prime-function (second selection-form))
                (selection-axes) (assign-sym)
                (item (gensym)))
           (labels ((set-assn-sym (form)
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
             ;; (print (list :pr prime-function))
             `(progn (a-set ,assign-sym
                            (assign-by-selection
                             ,(if (or (symbolp prime-function)
                                      (and (listp prime-function)
                                           (member (first prime-function) '(inws apl-fn function))))
                                  ;; TODO: make this work with an aliased ¨ operator
                                  prime-function
                                  (if (eql 'a-comp (first prime-function))
                                      (if (eql '|¨| (second prime-function))
                                          (fourth prime-function)
                                          (error "Invalid operator-composed expression ~a"
                                                 "used for selective assignment."))))
                             (lambda (,item) ,selection-form)
                             ,value ,assign-sym ,@(if selection-axes (list :axes selection-axes)))
                            ,@(if function (list :by function)))
                     ,value))))
        (t (let* ((syms (if (symbolp symbol) symbol
                            (if (and (listp symbol) (member (first symbol) '(inws inwsd)))
                                (second symbol) (if (and (listp symbol) (eql 'avector (first symbol)))
                                                    (rest symbol)))))
                  (symbols-list (if (symbolp syms) (list syms)))
                  (set-symbol (if (and (symbolp syms) (member syms '(⍺ ⍶ ⍺⍺)))
                                  syms symbol)))
             (labels ((get-symbol-list (list &optional inner)
                        (let ((valid t))
                          (if (listp list)
                              ;; build list of symbols to be assigned values
                              ;; (multiple for stranded/nested assignment)
                              (let ((out-list
                                      (loop :while valid :for i
                                              :in (if (and (not inner)
                                                           (not (eql 'avector (first list))))
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
                                      (if (is-workspace-function symbol)
                                          (fmakunbound insym))
                                      (if (and (not (boundp insym))
                                               (not (member symbol (getf (rest (getf (getf params :special)
                                                                                     :closure-meta))
                                                                         :var-syms))))
                                          ;; only bind dynamic variables in the workspace if the compiler
                                          ;; is at the top level; i.e. not within a { function }, where
                                          ;; bound variables are lexical
                                          (progn (proclaim (list 'special insym))
                                                 (set insym nil)))))
                          ;; (print (list :ff function))
                          (if function
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
  ;; (declare (ignore params))
  (if (symbolp symbol)
      (let ((i-sym (intern (string symbol) space))
            (in-closure (getf (getf params :special) :closure-meta)))
        (if (boundp i-sym) (makunbound i-sym))
        (if (not (member symbol '(⍺ ⍺⍺))) ;; don't bind assignments to argument symbols
            (setf (symbol-function i-sym) #'dummy-nargument-function))
        (if (and (listp function) (symbolp symbol)
                 (member (first function) '(lambda a-comp))
                 (member symbol (getf (rest (getf (getf params :special) :closure-meta)) :var-syms)))
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
                ,function))))

(defun compose-function-lateral (operator function value axes)
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
  (if (characterp operator)
      (append (list 'a-comp (intern (string operator)))
              (funcall (symbol-function (intern (format nil "APRIL-LEX-OP-~a" operator) *package-name-string*))
                       function1 (or function2 value2)))
      `(a-comp :op ,(if (not (symbolp operator))
                        operator `(inws ,operator))
               ,(or function2 value2) ,function1)))

(defun compose-function-train (right center &optional left left-value)
  (let ((omega (gensym)) (alpha (gensym))
        (right (if (not (and (listp right) (eql 'inwsd (first right))))
                   right `(function ,right)))
        (center (if (not (and (listp center) (eql 'inwsd (first center))))
                    center `(function ,center)))
        (left (if (not (and (listp left) (eql 'inwsd (first left))))
                  left `(function ,left))))
    (if left `(lambda (,omega &optional ,alpha)
                (if ,alpha (a-call ,center (a-call ,right ,omega ,alpha)
                                   (a-call ,left ,omega ,alpha))
                    (a-call ,center (a-call ,right ,omega)
                            (a-call ,left ,omega))))
        ;; TODO: condense these two clauses into one
        `(lambda (,omega &optional ,alpha)
           (if ,alpha (a-call ,center (a-call ,right ,omega ,alpha)
                              ,@(if left-value (list left-value)))
               (a-call ,center (a-call ,right ,omega) ,@(if left-value (list left-value))))))))

(defun fnexp-backup (form &key space params)
  "If a value build produces an pivotal function composition, it is built as a function. Needed for cases like fn←{2+⍵}⍣3 ⋄ fn 5."
  (if (not (and (listp form) (listp (first form)) (eq :fn (caar form))
                (eq :pass (cadar form))))
      form (build-function form :initial t :space space :params params)))

(defun wrap-fn-sym (form)
  (if (not (and (listp form) (eql 'inwsd (first form))))
      form (list 'function form)))

(defun compile-form (exprs &key space params)
  (loop :for expr :in exprs
        :collect (or (fnexp-backup (build-value expr :space space :params params)
                                   :space space :params params)
                     (multiple-value-bind (tokens other)
                         (build-function expr :initial t :space space :params params)
                       ;; (print (list :oo tokens other))
                       (wrap-fn-sym tokens))
                     (build-operator expr :initial t :space space :params params))))

(defun compile-test (string &optional space output)
  (if (= 0 (length string))
      ;; (compile-form (reverse output) :space "APRIL-WORKSPACE-COMMON")
      (compile-form (reverse output) :space space)
      (let ((result (lexer-postprocess (vex::parse string (vex::=vex-string *april-idiom*))
                                       *april-idiom* space)))
        (compile-test (second result)
                      space (cons (first result) output)))))

(defun compile-test2 (string)
  (vex-program *april-idiom* nil string))

(defun run-compile-test (string)
  (eval (compile-test string)))
