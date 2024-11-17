;;;; logic.lisp

(in-package #:varray)

"Special combinatoric logic for virtual arrays, mostly determining object allocations that return a modified form of the base object rather than an instance of the class being invoked."

(let* ((add-sub-functions (list #\+ #\-))
       (mul-div-functions (list #\×)) ; #\÷))
       (arith-functions (append add-sub-functions mul-div-functions)))
  (defun extend-allocator-vader-calculate (&key base axis function index-origin params)
    "Extend allocation behavior of operate class; currently, this allows for things like 1+⍳9 returning a modified integer progression vector rather than an instance of the vader-calculate class."
    (declare (ignore axis index-origin))
    (typecase base
      (list
       (cond
         ((= 1 (length base))
          (let ((lex-ref (getf params :lexical-reference)))
            (cond ((and lex-ref (numberp (first base)))
                   (make-instance 'vader-enclose :base (funcall function (first base))))
                  ((and lex-ref (typep (first base) 'vader-enclose)
                        (numberp (vader-base (first base))))
                   (make-instance 'vader-enclose :base (funcall function (vader-base (first base))))))))
         ((= 2 (length base))
          (let ((iota-first (and (typep (first base) 'integer)
                                 (typep (second base) 'vapri-apro-vector)))
                (iota-second (and (typep (first base) 'vapri-apro-vector)
                                  (typep (second base) 'integer)))
                (lex-ref (getf params :lexical-reference)))
            (cond ((and lex-ref (or (numberp (first base))
                                    (and (typep (first base) 'vader-enclose)
                                         (numberp (vader-base (first base)))))
                        (or (numberp (second base))
                            (and (typep (second base) 'vader-enclose)
                                 (numberp (vader-base (second base))))))
                   (let ((first (if (numberp (first base))
                                    (first base) (vader-base (first base))))
                         (second (if (numberp (second base))
                                     (second base) (vader-base (second base)))))
                     (make-instance 'vader-enclose :base (funcall function first second))))
                  ((and (or iota-first iota-second)
                        lex-ref (member lex-ref arith-functions :test #'char=)
                        ;; don't combine AP vectors if the new operation is multiplicative and
                        ;; there is a preexisting change to the offset; this prevents order
                        ;; of operations problems - TODO: logic to get around this?
                        (not (and (member lex-ref mul-div-functions :test #'char=)
                                  (not (zerop (vapip-offset (if iota-second (first base)
                                                                (second base)))))))
                        (not (and iota-second (char= #\÷ lex-ref))))
                   (destructuring-bind (iota number) (if iota-second base (reverse base))
                     (make-instance 'vapri-apro-vector
                                    :number (vapip-number iota) :origin (vapip-origin iota)
                                    :offset (if (not (member lex-ref add-sub-functions :test #'char=))
                                                (vapip-offset iota)
                                                (if (char= #\+ lex-ref)
                                                    (funcall function number (vapip-offset iota))
                                                    (if iota-first
                                                        (funcall function number (vapip-offset iota))
                                                        (funcall function (vapip-offset iota) number))))
                                    :factor (if (not (member lex-ref mul-div-functions :test #'char=))
                                                (funcall (if (and iota-second (char= #\- lex-ref))
                                                             #'- #'identity)
                                                         (vapip-factor iota))
                                                (if (char= #\× lex-ref)
                                                    (funcall function number (vapip-factor iota))
                                                    (when iota-first ;; currently always true
                                                      (funcall function number (vapip-factor iota)))))
                                    :repeat (vapip-repeat iota))))))))))))

(defun extend-allocator-vader-inverse-where (&key base argument index-origin)
  "Extend allocation behavior of inverse-where class; allows the use of ⍸⍣¯1 to create one-hot vectors."
  (declare (ignore argument))
  (let ((base-shape (shape-of base)))
    (when (and (not (second base-shape))
               (first base-shape) (= 1 (first base-shape)))
      (let ((base-val (funcall (generator-of base) 0)))
        (when (and (integerp base-val) (plusp base-val))
          (make-instance 'vapri-onehot-vector :shape (list base-val) :index (- base-val index-origin)))))))

(defun extend-allocator-vader-where (&key base argument index-origin)
  "Extend allocation behavior of where class; allows for the general case of {(,⍵)/,⍳⍴⍵} applying to a non-boolean argument for monadic ⍸."
  (let* ((base-shape (shape-of base))
         (base-type (etype-of base))
         ;; TODO: add more validity heuristics - work can be saved if the input type is known
         (known-valid (or (eql 'bit base-type)
                          (and (listp base-type)
                               (member 'unsigned-byte base-type)
                               (and (eql 'integer (first base-type)) ;; ⍸0 0 1 0 1 0 0 1 1 0  ⍸1  ⍸3=2 3 4⍴⍳9
                                    (<= 0 (second base-type))))))
         (arg (funcall (if known-valid #'identity #'render)
                       (make-instance 'vader-pare :base base))))

    ;; (print (list :aa arg base))
    
    (when (and (not known-valid)
               (not (loop :for index :below (array-total-size arg)
                          :never (let ((value (row-major-aref arg index)))
                                   ;; (print (list :val value))
                                   (or (not (integerp value))
                                       (> 0 value))))))
      (error "Invalid input to monadic ⍸ - all elements of argument must be non-negative integers."))
    
    (case (etype-of base)
      (bit) ;; in the case of a binary array, instantiate the dedicated class
      (t (make-instance
          'vader-expand :index-origin index-origin :axis :last :inverse t :argument arg
                        :base (if (< 1 (length base-shape))
                                  (make-instance 'vader-pare
                                                 :base (make-instance 'vapri-coordinate-identity
                                                                      :shape base-shape
                                                                      :index-origin index-origin))
                                  (make-instance 'vapri-apro-vector :offset index-origin
                                                                    :number (first base-shape))))))))

(defun extend-allocator-vader-section (&key base argument inverse axis index-origin)
  "Extend allocation behavior of section class; allows resizing of one-hot vectors."
  (declare (ignore axis))
  (typecase base
    (bit (when (and (not (zerop base)) (not inverse)
                    (or (not (listp argument))
                        (= 1 (length argument))))
           (let ((arg (disclose-unitary (render argument))))
             (when (and (integerp arg) (not (zerop arg)))
               (make-instance 'vapri-onehot-vector :shape (list (abs arg))
                                                   :index (if (plusp arg) 0 (1- (abs arg))))))))
    (vapri-onehot-vector (when (and (not inverse)
                                    (or (not (listp argument))
                                        (= 1 (length argument))))
                           (let ((arg (disclose-unitary (render argument))))
                             (make-instance 'vapri-onehot-vector
                                            :shape (list (abs arg))
                                            :index (+ (vaohv-index base)
                                                      (if (plusp arg)
                                                          0 (- (abs arg) (size-of base))))))))))

(defun extend-allocator-vader-permute (&key base argument inverse axis index-origin)
  "Extend allocation behavior of permute class; allows simple inversion of permutation without an argument."
  (declare (ignore axis index-origin inverse))
  (typecase base
    (vader-permute
     (when (and (not argument) (not (vads-argument base)))
       (vader-base base)))))

(defun extend-allocator-vader-expand (&key base argument index-origin inverse axis)
  "Extend allocation behavior of expand class; allows for 3/⍳3 to produce a repeating integer progression vector instead of a vader-expand instance."
  (declare (ignore axis index-origin inverse))
  (typecase base
    (vapri-apro-vector
     (let ((rendered-argument (unless (shape-of argument) (render argument))))
       (when (integerp rendered-argument)
         (make-instance 'vapri-apro-vector
                        :number (vapip-number base) :origin (vapip-origin base)
                        :offset (vapip-offset base) :factor (vapip-factor base)
                        :repeat (* rendered-argument (vapip-repeat base))))))))
