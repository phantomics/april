;;;; cape.lisp

(in-package #:cape)

(defclass base ()
  ((%idiom :accessor base-idiom
           :initform nil
           :initarg  :idiom
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
  ((%assigned :accessor exp-assigned
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
  
  (defun determine (token)
    (let ((output))
      (cond ((and (listp token) (eq :ax (first token)))
             (values token :axes))
            ;; ((and (listp token) (not (keywordp (first token))))
            ;;  (construct token))
            ((setf output (funcall process-value    token nil "APRIL-WORKSPACE-COMMON"))
              (values output :value))
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

;; (express (construct '((:AX ((1))) 3 2 1 (:FN #\+) 1 (:FN #\-) :SPECIAL-LEXICAL-FORM-ASSIGN |x|)))

(defun construct (idiom tokens &optional entity collected-axes)
  (if (not tokens)
      entity (multiple-value-bind (item type) (determine (first tokens))
               (print (list :it item type entity))
               (if (eq :axes type)
                   (construct idiom (rest tokens) entity (cons (rest item) collected-axes))
                   (let ((next-output (attach entity idiom item type collected-axes)))
                     (if (not next-output) entity (construct idiom (rest tokens) next-output)))))))

(defgeneric attach (entity idiom item type &optional axes))

(defmethod attach ((entity null) idiom item type &optional axes)
  ;; (print (list :en entity item type axes))
  (case type
    (:value    (make-instance 'ex-value :object (make-instance 'en-value :data (list item) :axes axes
                                                                         :idiom idiom)
                              :idiom idiom))
    (:function (make-instance 'ex-function :primary (make-instance 'en-value :data (list item)
                                                                             :axes axes :idiom idiom)
                              :idiom idiom))))

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
                         (make-instance 'en-value :data (list item) :axes axes :idiom idiom)))
               (push item (ent-data (exval-object entity))))))  ;; otherwise build right value
      (:function ;; if the item to be attached represents a function
       (if (eq item :special-lexical-form-assign)
           (setf (exp-assigned entity) :missing)
           (if (exval-function entity)
               (if (and (not (exfun-primary (exval-function entity)))
                        (exfun-operator (exval-function entity)))
                   (setf (exfun-primary (exval-function entity)) item)
                   (setf to-return (make-instance 'ex-value :function (attach nil idiom item type)
                                                            :object entity :idiom idiom)))
               (setf (exval-function entity) (attach nil idiom item type)
                     (exfun-value (exval-function entity)) entity))))
      (:operator
       (print (list :op item type))
       (if (exval-function entity)
           (if (position item (getf (vex::idiom-lexicons idiom) :operators-pivotal))
               (setf (exfun-operator (exval-function entity)) item))
           ;; the case of i.e. +/1 2 3 ; a lateral operator is seen before a function
           (if (position item (getf (vex::idiom-lexicons idiom) :operators-lateral))
               (setf (exval-function entity)
                     (make-instance 'ex-function :operator item :idiom idiom)))
           )))
    to-return))

(defgeneric express (item))

(defmethod express ((entity en-value))
  (funcall (if (not (ent-axes entity))
               #'identity (lambda (form)
                            `(make-virtual
                              'vader-select :base ,form :nested t :index-origin index-origin
                                            :argument ,(cons 'list (caaar (ent-axes entity))))))
           (if (second (ent-data entity))
               (cons 'sved (ent-data entity))
               (first (ent-data entity)))))

(defmethod express ((entity ex-value))
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

(defmethod express ((entity ex-function))
  ;; (print (list :eee entity))
  (list 'apl-fn-s (express (exfun-primary entity))))

(defmethod express ((entity en-function))
  (intern (string (enfun-data entity))))
