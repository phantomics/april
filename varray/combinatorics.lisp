;;;; combinatorics.lisp

(in-package #:varray)

"Special combinatoric rules for virtual arrays, mostly determining object allocations that return a modified form of the base object rather than an instance of the class being invoked."

(let* ((add-sub-functions (list #\+ #\-))
       (mul-div-functions (list #\× #\÷))
       (arith-functions (append add-sub-functions mul-div-functions)))
  (defun extend-allocator-vader-calculate (&key base axis function index-origin params)
    "Extend allocation behavior of operate class; currently, this allows for things like 1+⍳9 returning a modified integer progression vector rather than an instance of the vader-calculate class."
    (declare (ignore axis index-origin))
    ;; (print (list :ba base params))
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
                                 (typep (second base) 'vapri-integer-progression)))
                (iota-second (and (typep (first base) 'vapri-integer-progression)
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
                     (make-instance 'vapri-integer-progression
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

(defun extend-allocator-vader-expand (&key base argument index-origin inverse axis)
  "Extend allocation behavior of expand class; allows for 3/⍳3 to produce a repeating integer progression vector instead of a vader-expand instance."
  (declare (ignore axis index-origin inverse))
  (typecase base
    (vapri-integer-progression
     (let ((rendered-argument (unless (shape-of argument) (render argument))))
       (when (integerp rendered-argument)
         (make-instance 'vapri-integer-progression
                        :number (vapip-number base) :origin (vapip-origin base)
                        :offset (vapip-offset base) :factor (vapip-factor base)
                        :repeat (* rendered-argument (vapip-repeat base))))))))
