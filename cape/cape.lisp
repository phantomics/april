;;;; cape.lisp

(in-package #:cape)

(defclass base ()
  ((%idiom :accessor base-idiom
           :initform nil
           :initarg  :idiom
           :documentation "")
   (%expr  :accessor base-expr
           :initform nil
           :initarg  :expr
           :documentation "")))

(defclass entity (base)
  ((%data :accessor ent-data
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
  
  (defun determine (idiom token)
    (let ((output))
      (cond ((and (listp token) (eq :ax (first token)))
             (values token :axes))
            ((and (listp token) (not (keywordp (first token))))
             (let ((output (construct idiom token)))
               (values output (typecase output (ex-value :value) (ex-function :function)))))
            ((setf output (funcall process-value    token nil "APRIL-WORKSPACE-COMMON"))
             (values output :value))
            ((and (listp token) (eq :fn (first token)))
             ;; (print (list :tk token))
             (values (mapcar (lambda (exp) (construct idiom exp)) (third token))
                     :function))
            ((setf output (funcall process-function token nil "APRIL-WORKSPACE-COMMON"))
             (values output :function))
            ((setf output (funcall process-operator token nil "APRIL-WORKSPACE-COMMON"))
             (values output :operator))))))

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

(defun construct (idiom tokens &optional entity collected-axes)
  (if (not tokens)
      entity (multiple-value-bind (item type) (determine idiom (first tokens))
               ;; (print (list :it item type entity))
               (if (eq :axes type)
                   (let ((processed-axes (mapcar (lambda (token-list) (construct idiom token-list))
                                                 (rest item))))
                     (construct idiom (rest tokens) entity (cons processed-axes collected-axes)))
                   (let ((next-output (attach entity idiom item type collected-axes)))
                     (if (not next-output) entity (construct idiom (rest tokens) next-output)))))))

(defgeneric attach (entity idiom item type &optional axes))

(defmethod attach ((entity null) idiom item type &optional axes)
  ;; (print (list :en entity item type axes))
  (let ((output))
    (case type
      (:value
       (setf output (make-instance 'ex-value :idiom idiom)
             (exval-object output) (make-instance 'en-value :data (list item)
                                                            :axes axes :idiom idiom :expr output)))
      (:function
       (setf output (make-instance 'ex-function :idiom idiom)
             (exfun-primary output) (make-instance 'en-function :data item
                                                                :axes axes :idiom idiom :expr output))))
    output))

(defmethod attach ((entity ex-value) idiom item type &optional axes)
  "Attach an item to a value expression."
  (let ((to-return entity))
    (case type
      (:value ;; if the item to be attached represents a value
       (if (exp-assigned entity) ;; if assignment has been made, gather symbols for destination(s)
           (if (listp (exp-assigned entity))
               (push item (exp-assigned entity))
               (setf (exp-assigned entity) (list item)))
           (if (exval-function entity) ;; if a function has been registered, build left value;
               (if (exval-predicate entity)
                   (push item (ent-data (exval-predicate entity)))
                   (setf (exval-predicate entity) ;; initialize left value if not yet present
                         (make-instance 'en-value :axes axes :idiom idiom
                                                  :expr entity :data (list item))))
               (push item (ent-data (exval-object entity))))))  ;; otherwise build right value
      (:function ;; if the item to be attached represents a function
       (if (eq item :special-lexical-form-assign)
           (setf (exp-assigned entity) :missing)
           (if (exval-function entity)
               (if (exfun-operator (exval-function entity))
                   (if (exfun-primary (exval-function entity))
                       ;; if a primary function and and operator are attached but no composed
                       ;; function yet (as for the × in ×.+), register the composed function
                       (setf (exfun-composed (exval-function entity))
                             (make-instance 'en-function :data item :axes axes :idiom idiom
                                                         :expr (exval-function entity)))
                       ;; if no primary function is registered, set it
                       (setf (exfun-primary (exval-function entity))
                             (make-instance 'en-function :data item :axes axes :idiom idiom
                                                         :expr (exval-function entity))))
                   (setf to-return (make-instance 'ex-value :function (typecase item
                                                                        (ex-function item)
                                                                        (t (attach nil idiom item type)))
                                                  :object entity :idiom idiom)))
               (setf (exval-function entity) (attach nil idiom item type)
                     (base-expr (exval-function entity)) entity))))
      (:operator ;; if the item to be attached represents an operator
       ;; (print (list :op item type))
       (if (exval-function entity)
           (if (position item (getf (vex::idiom-lexicons idiom) :operators-pivotal))
               (setf (exfun-operator (exval-function entity)) item))
           ;; the case of i.e. +/1 2 3 ; a lateral operator is seen before a function
           (if (position item (getf (vex::idiom-lexicons idiom) :operators-lateral))
               (setf (exval-function entity)
                     (make-instance 'ex-function :operator item :idiom idiom))))))
    to-return))

(defmethod attach ((entity ex-function) idiom item type &optional axes)
  "Attach an item to a value expression."
  (labels ((train-link (item expr)
             (if (exfun-composed expr)
                 (train-link item (exfun-composed expr))
                 (setf (exfun-composed expr) (typecase item
                                               (ex-function item)
                                               (t (attach nil idiom item type)))))))
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
                           (make-instance 'en-function :data item :axes axes :idiom idiom))
                     ;; if no primary function is registered, set it
                     (setf (exfun-primary  entity)
                           (make-instance 'en-function :data item :axes axes :idiom idiom)))
                 ;; join the function expression to another to create a train,
                 ;; implemented as a kind of linked list using function expressions
                 (train-link item entity)))))
      to-return)))

(defgeneric express (item &rest params))

(defmethod express ((item t) &rest params)
  item)

(defmethod express ((entity en-value) &rest params)
  (funcall (if (not (ent-axes entity))
               #'identity (lambda (form)
                            `(make-virtual
                              'vader-select :base ,form :nested t :index-origin index-origin
                                            :argument ,(list 'list (express (caar (ent-axes entity)))))))
           (if (second (ent-data entity))
               (cons 'svec (ent-data entity))
               (express (first (ent-data entity))))))

(defmethod express ((entity ex-value) &rest params)
  (flet ((express-value (value)
           (if (not (second value))
               (first value) (cons 'svec value))))
    (funcall (if (not (exp-assigned entity))
                 #'identity (or (and (eq :missing (exp-assigned entity))
                                     (error "No name found for assignment."))
                                (lambda (form)
                                  `(a-set (inwsd ,(intern (string (first (exp-assigned entity)))))
                                          ,form))))
             (if (exval-function entity)
                 `(a-call ,(express (exval-function entity))
                          ,(express (exval-object entity))
                          ,@(let ((second (exval-predicate entity)))
                              (and second (list (express second)))))
                 (express (exval-object entity))))))

(defmethod express ((entity ex-function) &rest params)
  (if (exfun-operator entity)
      (let ((expfun1 (express (exfun-primary entity)))
            (expfun2 (and (exfun-composed entity) (express (exfun-composed entity)))))
        (append (list 'a-comp (exfun-operator entity))
                (apply (symbol-function (find-symbol (format nil "~a-LEX-OP-~a"
                                                             (vex::idiom-name (base-idiom entity))
                                                             (exfun-operator entity))
                                                     (string (vex::idiom-name (base-idiom entity)))
                                                     ;; TODO: allow for different idiom and package name
                                                     ))
                       (cons expfun1 (if expfun2 (list expfun2))))))
      ;; (april (with (:print-tokens) (:cape-test) (:compile-only)) "(*+-)1 2 3+1 2 3")
      (if (or (getf params :train-preceding)
              (and (not (base-expr entity))
                   (typep (exfun-composed entity) 'ex-function)))
          ;; if the functional expression is not linked to a value expression
          ;; (i.e. it is expressed discretely as with (⊢,⌽)) then it is
          ;; recognized as a function train
          (let* ((omega (or (getf params :symbol-o) (gensym)))
                 (alpha (or (getf params :symbol-a) (gensym)))
                 ;; (left-value (if (typep expfun2 'en-value)
                 ;;                 (express expfun2)))
                 (right  (or (getf params :train-preceding)
                             (express (express (exfun-primary entity))
                                      :valence :dyadic :primary-only t)))
                 (center (or (and (getf params :train-preceding)
                                  (express (exfun-primary entity)))
                             (and (exfun-composed entity)
                                  (express (exfun-primary (exfun-composed entity))
                                           :valence :dyadic))))
                 (left   (or (and (getf params :train-preceding) (exfun-composed entity)
                                  (express (exfun-primary (exfun-composed entity))))
                             (and (exfun-composed entity) (exfun-composed (exfun-composed entity))
                                  (typep (exfun-composed (exfun-composed entity)) 'ex-function)
                                  (express (exfun-primary (exfun-composed (exfun-composed entity)))
                                           :valence :dyadic))))
                 (left-value nil)
                 (containing (and left (exfun-composed (exfun-composed (exfun-composed entity))))))
            ;; (print (list :en entity containing right center left))
            ;; (unless april::jjj (setf april::jjj entity))
            (let ((code `(alambda (,omega &optional ,alpha)
                           (with (:sys-vars index-origin))
                           (if ,alpha (a-call ,center (a-call ,right ,omega ,alpha)
                                              ,@(if left-value (list left-value)
                                                    (if left `((a-call ,left ,omega ,alpha)))))
                               (a-call ,center (a-call ,right ,omega)
                                       ,@(if left-value (list left-value)
                                             (if left `((a-call ,left ,omega)))))))))
              (if (not containing)
                  code (express containing :train-preceding code :symbol-o omega :symbol-a alpha))))
          (express (exfun-primary entity)))))

(defmethod express ((entity en-function) &rest params)
  (let ((valence (getf params :valence)))
    ;; (print (list :aa (ent-data entity)))
    ;; (when (base-expr entity)
    ;;   (print (list :aa (exval-predicate (base-expr (base-expr entity)))))
    ;;   (print (list :aa (ent-data (exfun-primary (base-expr entity)))))
    ;;   )
    (if (listp (ent-data entity))
        `(alambda (⍵ &OPTIONAL ⍺) (with (:meta)) ,@(mapcar #'express (ent-data entity)))
        (list (if (vex::of-lexicons (base-idiom entity) (ent-data (exfun-primary (base-expr entity)))
                                    (if (or (and (base-expr (base-expr entity))
                                                 (exval-predicate (base-expr (base-expr entity))))
                                            (eq (getf params :valence) :dyadic))
                                        :functions-scalar-dyadic :functions-scalar-monadic))
                  'apl-fn-s 'apl-fn)
              (intern (string (ent-data entity)))))))
