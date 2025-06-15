;;;; cape.lisp

(in-package #:cape)

(defclass base ()
  ((%idiom :accessor base-idiom
           :initform nil
           :initarg  :idiom
           :documentation "")
   (%space :accessor base-space
           :initform nil
           :initarg  :space
           :documentation "")
   (%expr  :accessor base-expr
           :initform nil
           :initarg  :expr
           :documentation "")))

(defclass entity (base)
  ((%meta :accessor ent-meta
          :initform nil
          :initarg  :meta
          :documentation "")
   (%data :accessor ent-data
          :initform nil
          :initarg  :data
          :documentation "")
   (%axes :accessor ent-axes
          :initform nil
          :initarg  :axes
          :documentation "")))

(defclass expression (base)
  ((%scope    :accessor exp-scope
              :initform nil
              :initarg  :scope
              :documentation "")
   (%assigned :accessor exp-assigned
              :initform nil
              :initarg  :assigned
              :documentation "")))

(defclass en-value (entity)
  ())

(defclass en-function (entity)
  ((%lexicon :accessor enfun-lexicon
             :initform nil
             :initarg  :lexicon
             :documentation "")))

(defclass en-operator (entity)
  ())

(defclass ex-value (expression)
  ((%object    :accessor exval-object
               :initform nil
               :initarg  :object
               :documentation "")
   (%function  :accessor exval-function
               :initform nil
               :initarg  :function
               :documentation "")
   (%predicate :accessor exval-predicate
               :initform nil
               :initarg  :predicate
               :documentation "")))

(defclass ex-function (expression) ;; express trains
  ((%value    :accessor exfun-value
              :initform nil
              :initarg  :value
              :documentation "")
   (%primary  :accessor exfun-primary
              :initform nil
              :initarg  :primary
              :documentation "")
   (%operator :accessor exfun-operator
              :initform nil
              :initarg  :operator
              :documentation "")
   (%composed :accessor exfun-composed
              :initform nil
              :initarg  :composed
              :documentation "")))

(let ((process-value) (process-function) (process-operator))
  (defun provision-processors (for-value for-function for-operator)
    (setf process-value    for-value
          process-function for-function
          process-operator for-operator))
  
  (defun determine (idiom space scope token)
    (let ((output))
      (cond ((and (listp token) (eq :ax (first token)))
             (values token :axes))
            ;; (let ((output (construct scope idiom space token)))
            ;;    (values output (typecase output (ex-value :value) (ex-function :function))))
            ;; ((and (listp token) (eq :fn (first token)) (not (characterp (second token))))
            ;;  (values (mapcar (lambda (exp) (construct idiom space scope exp)) (third token))
            ;;          :function))
            ((and (listp token) (not (keywordp (first token))))
             (values token  :closure))
            ((and (listp token) (eq :fn (first token)) (not (characterp (first (last token)))))
             (values :defn  :function (third token) (second token)))
            ((setf output (funcall process-value    token nil "APRIL-WORKSPACE-COMMON"))
             (values output :value))
            ((setf output (funcall process-function token nil "APRIL-WORKSPACE-COMMON"))
             (values output :function))
            ((setf output (funcall process-operator token nil "APRIL-WORKSPACE-COMMON"))
             (values output :operator))))))

(defun construct (idiom space scope tokens &optional entity collected-axes)
  (if tokens
      (multiple-value-bind (item type subexprs meta) (determine idiom space scope (first tokens))
        ;; (print (list :it item type entity))
        (case type
          (:axes
           (let ((processed-axes (mapcar (lambda (token-list) (construct idiom space scope token-list))
                                         (rest item))))
             (construct idiom space scope (rest tokens) entity (cons processed-axes collected-axes))))
          
          (:closure
           ;; (print (list :it tokens item))
           (let* ((enclosed (construct idiom space scope item))
                  (enclosed-type (typecase enclosed (ex-value :value) (ex-function :function))))
             ;; (print (list :ty (type-of enclosed)))
             (construct idiom space scope (rest tokens)
                        (attach entity idiom meta enclosed enclosed-type collected-axes))))
           
          (t (let ((next-output (attach entity idiom meta item type collected-axes)))
               ;; (print (list :nn next-output space))

               (setf (base-space next-output) space)
               (when (and scope (not (exp-scope next-output)))
                 ;; assign the expression's scope when it's present
                 (setf (exp-scope next-output) scope))
               
               (when (eq :defn item)
                 ;; when the item to be processed is a defn, it must be set as the scope
                 ;; for expressions within and those expressions passed as subexprs must be
                 ;; processed and assigned
                 (let* ((fnexp (typecase next-output
                                 (ex-function next-output) (ex-value (exval-function next-output))))
                        (fn-scope (if (eq :defn (ent-data (exfun-primary fnexp)))
                                      (exfun-primary fnexp)
                                      (and (eq :defn (ent-data (exfun-composed fnexp)))
                                           (exfun-composed fnexp)))))
                   (when fn-scope
                     (let ((exprs (mapcar (lambda (exp) (construct idiom space scope exp)) subexprs)))
                       (dolist (expr exprs) (setf (base-expr expr) fnexp))
                       (setf (ent-data fn-scope) exprs)))))
               (if next-output (construct idiom space scope (rest tokens) next-output)
                   entity)))))
        entity))

;; (cond ((eq :defn (ent-data (exfun-primary fnexp)))
;;        (let ((scope (exfun-primary fnexp)))
;;          (setf (ent-data (exfun-primary fnexp))
;;                (mapcar (lambda (exp) (construct idiom space scope exp)) subexprs))))
;;       ((eq :defn (ent-data (exfun-composed fnexp)))
;;        (let ((scope (exfun-composed fnexp)))
;;          (setf (ent-data (exfun-composed fnexp))
;;                (mapcar (lambda (exp) (construct idiom space scope exp)) subexprs)))))

;; (defun construct (tokens)
;;   (let ((output) (collected-axes))
;;     (loop :for token :in tokens
;;           :do (multiple-value-bind (item type) (determine token)
;;                 (if (eq :axes type)
;;                     (push item collected-axes)
;;                     (let ((next-output (attach output item type collected-axes)))
;;                       (setf collected-axes nil)
;;                       (when next-output (setf output next-output))))))
;;     output))

;; (cape::provision-processors #'process-value #'process-function #'process-operator)
;; (express (construct '((:AX ((1))) 3 2 1 (:FN #\+) 1 (:FN #\-) :SPECIAL-LEXICAL-FORM-ASSIGN |x|)))


(defgeneric attach (entity idiom meta item type &optional axes))

(defmethod attach ((entity null) idiom meta item type &optional axes)
  ;; (print (list :en entity item type axes))
  (let ((output))
    (case type
      (:value
       (setf output (make-instance 'ex-value :idiom idiom)
             (exval-object output) (make-instance 'en-value :data (list item) :meta meta
                                                            :axes axes :idiom idiom :expr output)))
      (:function
       (setf output (make-instance 'ex-function :idiom idiom)
             (exfun-primary output) (make-instance 'en-function :data item :meta meta
                                                                :axes axes :idiom idiom :expr output))))
    output))

(defmethod attach ((entity ex-value) idiom meta item type &optional axes)
  "Attach an item to a value expression."
  (let ((to-return entity))
    ;; (print (list :it item)) ;; (setf iio entity)))
    (case type
      (:value ;; if the item to be attached represents a value
       (if (exp-assigned entity) ;; if assignment has been made, gather symbols for destination(s)
           (if (listp (exp-assigned entity))
               (push item (exp-assigned entity))
               (setf (exp-assigned entity) (list item)))
           (if (exval-function entity) ;; if a function has been registered, build the left value;
               (if (and (exfun-operator (exval-function entity))
                        (position (ent-data (exfun-operator (exval-function entity)))
                                  (getf (vex::idiom-lexicons idiom) :operators-pivotal) :test #'char=)
                        ;; the or clause needed to correctly process i.e. (⍳5)+⍤1⊢1 5⍴⍳5, so the
                        ;; rightmost value is applied as the predicate to the value expression
                        (or (not (exfun-composed (exval-function entity)))
                            (typep (exfun-composed (exval-function entity)) 'ex-value)))
                   ;; handle the case of a numeric operand found after a pivotal operator
                   ;; preceding a value expression, as for 3∘+⊢1 2 3
                   (setf (exfun-composed (exval-function entity))
                         (if (null (exfun-composed (exval-function entity)))
                             item (cons item (list (exfun-composed (exval-function entity))))))
                   (if (exval-predicate entity)
                       (push item (ent-data (exval-predicate entity)))
                       (setf (exval-predicate entity) ;; initialize the left value if not yet present;
                             (make-instance 'en-value :axes axes :idiom idiom
                                                      :expr entity :data (list item)))))
               (push item (ent-data (exval-object entity))))) ;; otherwise, build the right value

       (when (and (exval-object entity)    ;; handle the case of an overloaded
                  (exval-predicate entity) ;; function/operator like  / and \ in APL
                  (exfun-operator (exval-function entity))      ;; only an "operator" is present...
                  (not (exfun-primary (exval-function entity))) ;; with no function found
                  (position (ent-data (exfun-operator (exval-function entity)))
                            (getf (vex::idiom-lexicons idiom) :functions)
                            :test #'char=)) ;; check that the operator may also represent a function
         (let ((overloaded (exfun-operator (exval-function entity))))
           (setf (exfun-operator (exval-function entity)) nil
                 (exfun-primary  (exval-function entity)) ;; port operator data like axes to the function
                 (make-instance 'en-function :data (ent-data overloaded) :axes (ent-axes overloaded)
                                             :idiom idiom :expr (exval-function entity)
                                             :meta (ent-meta overloaded))))))
       
      (:function ;; if the item to be attached represents a function
       ;; (print (list :ii item))
       (if (eq item :special-lexical-form-assign)
           (setf (exp-assigned entity) :missing)
           (if (exval-function entity)
               (if (exfun-operator (exval-function entity))
                   (if (exfun-primary (exval-function entity))
                       ;; if a primary function and and operator are attached but no composed
                       ;; function yet (as for the × in ×.+), register the composed function
                       (if (position (ent-data (exfun-operator (exval-function entity)))
                                     (getf (vex::idiom-lexicons idiom) :operators-lateral))
                           ;; if the operator in the functional expression is lateral, then begin a
                           ;; new value expression; otherwise set the "composed"/left operand
                           (setf to-return (attach (make-instance 'ex-value :object entity)
                                                   idiom meta item type axes))
                           (setf (exfun-composed (exval-function entity))
                                 (make-instance 'en-function :data item :axes axes :idiom idiom :meta meta
                                                             :expr (exval-function entity))))
                       ;; if no primary function is registered, set it
                       (setf (exfun-primary (exval-function entity))
                             (make-instance 'en-function :data item :axes axes :idiom idiom :meta meta
                                                         :expr (exval-function entity))))
                   ;; if a functional expression is encountered following a value as for
                   ;; -1 2+3 4, create a new value expression with the current entity as its object
                   (setf to-return (make-instance 'ex-value :function (typecase item
                                                                        (ex-function item)
                                                                        (t (attach nil idiom meta item type)))
                                                  :object entity :idiom idiom)))
               (setf (exval-function entity) (attach nil idiom meta item type)
                     (base-expr (exval-function entity)) entity))))
      
      (:operator ;; if the item to be attached represents an operator
       ;; (print (list :op item type))
       (if (and (exval-function entity) (exfun-operator (exval-function entity))
                (position item (getf (vex::idiom-lexicons idiom) :operators))
                (position item (getf (vex::idiom-lexicons idiom) :functions)))
           (attach entity idiom meta item :function axes)
           (if (position item (getf (vex::idiom-lexicons idiom) :operators-pivotal) :test #'char=)
               (if (and (exval-function entity)         ;; the case of a pivotal operator composition
                        (not (exval-predicate entity))) ;; preceding a value expression like -∘+⊢1 2 3
                   (setf (exfun-operator (exval-function entity))
                         (make-instance 'en-operator :data item :meta meta :axes axes
                                                     :idiom idiom :expr (exval-function entity)))
                   ;; the case of a pivotal operator with a value as right operand
                   ;;  preceded by a function composition, as for 1760 3 12⊤⍣¯1⊢2 0 10
                   (if (exval-object entity)
                       (let ((fn-expr (make-instance 'ex-function :idiom idiom
                                                                  :primary (exval-predicate entity))))
                         (setf (exfun-operator fn-expr) (make-instance 'en-operator
                                                                       :data item :meta meta :axes axes
                                                                       :idiom idiom :expr fn-expr)
                               (exval-predicate entity) nil) ;; remove the apparent left argument...
                         ;; and begin a new value composition with the operator composing its function
                         (setf to-return (make-instance 'ex-value :function fn-expr
                                                                  :object entity :idiom idiom)))))
               (if (exval-function entity)
                   (if (exfun-operator (exval-function entity))
                       ;; handle successive operator compositions, as for ⊃¨⍴¨'one' 'a' 'two' 'three'
                       (setf to-return (attach (make-instance 'ex-value :object entity)
                                               idiom meta item type axes))
                       (if (exval-predicate entity)
                           ;; handle an operator composition following
                           ;; a dyadic value composition i.e. 1 2∊⍨9⍴⍳4
                           (setf to-return (attach (make-instance 'ex-value :object entity)
                                                   idiom meta item type axes))
                           (if (position item (getf (vex::idiom-lexicons idiom) :operators-pivotal)
                                         :test #'char=)
                               (setf (exfun-operator (exval-function entity)) ;; item
                                     (make-instance 'en-operator
                                                    :data item :meta meta :axes axes
                                                    :idiom idiom :expr (exval-function entity))))))
                   ;; the case of i.e. +/1 2 3 ; a lateral operator is seen before a function
                   (and (position item (getf (vex::idiom-lexicons idiom) :operators-lateral) :test #'char=)
                        (let ((operator (make-instance 'en-operator
                                                       :data item :meta meta :axes axes
                                                       :idiom idiom :expr (exval-function entity))))
                          (setf (exval-function entity)
                                (make-instance 'ex-function :operator operator :idiom idiom)))))))))
    to-return))

(defmethod attach ((entity ex-function) idiom meta item type &optional axes)
  "Attach an item to a value expression."
  (labels ((train-link (item expr)
             (if (exfun-composed expr)
                 (train-link item (exfun-composed expr))
                 (setf (exfun-composed expr) (typecase item
                                               (ex-function item)
                                               (t (attach nil idiom meta item type)))))))
    (let ((to-return entity))
      (case type
        (:function
         (if (eq item :special-lexical-form-assign)
             (setf (exp-assigned entity) :missing)
             (if (exfun-operator entity)
                 (if (exfun-primary entity)
                     ;; if a primary function and and operator are attached but no composed
                     ;; function yet (as for the × in ×.+), register the composed function
                     (setf (exfun-composed entity)
                           (make-instance 'en-function :data item :axes axes :idiom idiom :meta meta))
                     ;; if no primary function is registered, set it
                     (setf (exfun-primary  entity)
                           (make-instance 'en-function :data item :axes axes :idiom idiom :meta meta)))
                 ;; join the function expression to another to create a train,
                 ;; implemented as a kind of linked list using function expressions
                 (train-link item entity)))))
      to-return)))

(defgeneric express (item &rest params))

(defmethod express ((item t) &rest params)
  item)

;; (defmethod express ((entity en-value) &rest params)
;;   ;; (print (list :ee (ent-data entity) (exp-scope (base-expr entity))))
;;   (funcall (if (not (ent-axes entity))
;;                #'identity (lambda (form)
;;                             `(make-virtual
;;                               'vader-select :base ,form :nested t :index-origin index-origin
;;                                             :argument ,(list 'list (express (caar (ent-axes entity)))))))
;;            (if (second (ent-data entity))
;;                (cons 'svec (ent-data entity))
;;                (express (first (ent-data entity))))))

;; (defmethod express ((entity ex-value) &rest params)
;;   (flet ((express-value (value)
;;            (if (not (second value))
;;                (first value) (cons 'svec value))))
;;     (funcall (if (not (exp-assigned entity))
;;                  #'identity (or (and (eq :missing (exp-assigned entity))
;;                                      (error "No name found for assignment."))
;;                                 (lambda (form)
;;                                   `(a-set (inwsd ,(intern (string (first (exp-assigned entity)))))
;;                                           ,form))))
;;              (if (exval-function entity)
;;                  `(a-call ,(express (exval-function entity))
;;                           ,(express (exval-object entity))
;;                           ,@(let ((second (exval-predicate entity)))
;;                               (and second (list (express second)))))
;;                  (express (exval-object entity))))))

;; (defmethod express ((entity ex-function) &rest params)
;;   (if (exfun-operator entity)
;;       (let ((expfun1 (express (exfun-primary entity)))
;;             (expfun2 (and (exfun-composed entity) (express (exfun-composed entity)))))
;;         (append (list 'a-comp (exfun-operator entity))
;;                 (apply (symbol-function (find-symbol (format nil "~a-LEX-OP-~a"
;;                                                              (vex::idiom-name (base-idiom entity))
;;                                                              (exfun-operator entity))
;;                                                      (string (vex::idiom-name (base-idiom entity)))
;;                                                      ;; TODO: allow for different idiom and package name
;;                                                      ))
;;                        (cons expfun1 (if expfun2 (list expfun2))))))
;;       ;; (april (with (:print-tokens) (:cape-test) (:compile-only)) "(*+-)1 2 3+1 2 3")
;;       (if (or (getf params :train-preceding)
;;               (and (not (base-expr entity))
;;                    (typep (exfun-composed entity) 'ex-function)))
;;           ;; if the functional expression is not linked to a value expression
;;           ;; (i.e. it is expressed discretely as with (⊢,⌽)) then it is
;;           ;; recognized as a function train
;;           (let* ((omega (or (getf params :symbol-o) (gensym)))
;;                  (alpha (or (getf params :symbol-a) (gensym)))
;;                  ;; (left-value (if (typep expfun2 'en-value)
;;                  ;;                 (express expfun2)))
;;                  (right  (or (getf params :train-preceding)
;;                              (express (express (exfun-primary entity))
;;                                       :valence :dyadic :primary-only t)))
;;                  (center (or (and (getf params :train-preceding)
;;                                   (express (exfun-primary entity)))
;;                              (and (exfun-composed entity)
;;                                   (express (exfun-primary (exfun-composed entity))
;;                                            :valence :dyadic))))
;;                  (left   (or (and (getf params :train-preceding) (exfun-composed entity)
;;                                   (express (exfun-primary (exfun-composed entity))))
;;                              (and (exfun-composed entity) (exfun-composed (exfun-composed entity))
;;                                   (typep (exfun-composed (exfun-composed entity)) 'ex-function)
;;                                   (express (exfun-primary (exfun-composed (exfun-composed entity)))
;;                                            :valence :dyadic))))
;;                  (left-value nil)
;;                  (containing (and left (exfun-composed (exfun-composed (exfun-composed entity))))))
;;             ;; (print (list :en entity containing right center left))
;;             ;; (unless april::jjj (setf april::jjj entity))
;;             (let ((code `(alambda (,omega &optional ,alpha)
;;                            (with (:sys-vars index-origin))
;;                            (if ,alpha (a-call ,center (a-call ,right ,omega ,alpha)
;;                                               ,@(if left-value (list left-value)
;;                                                     (if left `((a-call ,left ,omega ,alpha)))))
;;                                (a-call ,center (a-call ,right ,omega)
;;                                        ,@(if left-value (list left-value)
;;                                              (if left `((a-call ,left ,omega)))))))))
;;               (if (not containing)
;;                   code (express containing :train-preceding code :symbol-o omega :symbol-a alpha))))
;;           (express (exfun-primary entity)))))

;; (defmethod express ((entity en-function) &rest params)
;;   (let ((valence (getf params :valence)))
;;     ;; (print (list :aa (ent-data entity)))
;;     ;; (when (base-expr entity)
;;     ;;   (print (list :aa (exval-predicate (base-expr (base-expr entity)))))
;;     ;;   (print (list :aa (ent-data (exfun-primary (base-expr entity)))))
;;     ;;   )
;;     (if (and (ent-data entity)
;;              (listp (ent-data entity)))
;;         `(alambda (⍵ &OPTIONAL ⍺) (with (:meta)) ,@(mapcar #'express (ent-data entity)))
;;         (list (if (vex::of-lexicons (base-idiom entity)
;;                                     ;; (ent-data (exfun-primary (base-expr entity)))
;;                                     (ent-data entity)
;;                                     (if (or (and (base-expr (base-expr entity))
;;                                                  (exval-predicate (base-expr (base-expr entity))))
;;                                             (eq (getf params :valence) :dyadic))
;;                                         :functions-scalar-dyadic :functions-scalar-monadic))
;;                   'apl-fn-s 'apl-fn)
;;               (intern (string (ent-data entity)))))))
