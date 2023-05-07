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
                                 (typep (second base) 'vapri-arith-provec)))
                (iota-second (and (typep (first base) 'vapri-arith-provec)
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
                        (not (and iota-second (char= #\÷ lex-ref))))
                   (destructuring-bind (iota number) (if iota-second base (reverse base))
                     (make-instance 'vapri-arith-provec
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

(defun extend-allocator-vader-section (&key base argument inverse axis index-origin)
  "Extend allocation behavior of section class; allows resizing of one-hot vectors."
  (declare (ignore axis))
  (typecase base
    (bit (when (and (= 1 base)
                    (or (not (listp argument))
                        (= 1 (length argument))))
           (let ((arg (disclose-unitary (render argument))))
             (make-instance 'vapri-onehot-vector :shape (list (abs arg))
                                                 :index (if (plusp arg)
                                                            0 (- (abs arg) index-origin))))))
    (vapri-onehot-vector (when (or (not (listp argument))
                                   (= 1 (length argument)))
                           (let ((arg (disclose-unitary (render argument))))
                             (make-instance 'vapri-onehot-vector :shape (list (abs arg))
                                                                 :index (+ (vaohv-index base)
                                                                           (if (plusp arg)
                                                                               0 (- (abs arg)
                                                                                    (size-of base))))))))))

(defun extend-allocator-vader-permute (&key base argument index-origin)
  "Extend allocation behavior of permute class; allows simple inversion of permutation without an argument."
  (declare (ignore axis index-origin))
  (typecase base
    (vader-permute
     (when (and (not argument) (not (vads-argument base)))
       (vader-base base)))))

(defun extend-allocator-vader-expand (&key base argument index-origin inverse axis)
  "Extend allocation behavior of expand class; allows for 3/⍳3 to produce a repeating integer progression vector instead of a vader-expand instance."
  (declare (ignore axis index-origin inverse))
  (typecase base
    (vapri-arith-provec
     (let ((rendered-argument (unless (shape-of argument) (render argument))))
       (when (integerp rendered-argument)
         (make-instance 'vapri-arith-provec
                        :number (vapip-number base) :origin (vapip-origin base)
                        :offset (vapip-offset base) :factor (vapip-factor base)
                        :repeat (* rendered-argument (vapip-repeat base))))))))
