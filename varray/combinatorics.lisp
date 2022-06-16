;;;; combinatorics.lisp

(in-package #:varray)

(defclass va-class (standard-class)
  nil (:documentation "Metaclass for virtual array objects."))

(defmethod closer-mop:validate-superclass ((class va-class)
                                           (superclass cl:standard-class))
  t)

(setf *combos* nil)

(defmacro assign-combo (from to args &body body)
  `(progn (when (not (assoc (quote ,from) *combos*))
            (push (list (quote ,from)) *combos*))
          (push (list (quote ,to) (lambda ,args ,@body))
                (rest (assoc (quote ,from) *combos*)))))

(defmacro build-allocator ()
  `(defmethod allocate-instance
       ((this-class va-class) &key base axis argument)
     (let ((default (call-next-method)))
       (typecase default
         ,@(loop :for combo :in *combos*
              :collect
              (list (first combo)
                    `(typecase base
                       ,@(loop :for subc :in (rest combo)
                            :collect
                            (list (first subc)
                                  `(let* ((fn (second
                                               (assoc (quote ,(first combo))
                                                      (rest (assoc
                                                             (quote ,(first
                                                                      subc))
                                                             *combos*)))))
                                          (output (funcall fn base axis argument)))
                                     (when output (setf default output)))))))))
       default)))

(assign-combo vader-turn vader-turn (base axis argument)
  (let ((base-axis (vads-axis base))
        (base-arg (vads-argument base)))
    ;; (print (list :bb base axis argument))
    (when (or (and (eq :last axis)
                   (eq :last base-axis))
              (and (numberp axis)
                   (numberp base-axis)
                   (= axis base-axis)))
      (if (and argument base-arg)
          (progn (setf (vads-argument base) (+ argument base-arg))
                 base)
          (when (and (not argument) (not base-arg))
            base)))))

(build-allocator)
