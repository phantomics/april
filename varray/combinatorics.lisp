;;;; combinatorics.lisp

(in-package #:varray)

"Special combinatoric rules for virtual arrays, mostly determining object allocations that return a modified form of the base object rather than an instance of the class being invoked."

(let* ((add-sub-functions (list #\+ #\-))
       (mul-div-functions (list #\× #\÷))
       (arith-functions (append add-sub-functions mul-div-functions)))
  (defun extend-allocator-vader-operate (&key base axis function index-origin params)
    "Extend allocation behavior of operate class; currently, this allows for things like 1+⍳9 returning a modified integer progression vector rather than an instance of the vader-operate class."
    (typecase base
      (list
       (when (= 2 (length base))
         (let ((iota-first (and (typep (first base) 'integer)
                                (typep (second base) 'vapri-integer-progression)))
               (iota-second (and (typep (first base) 'vapri-integer-progression)
                                 (typep (second base) 'integer)))
               (lex-ref (getf params :lexical-reference)))
           (when (and (or iota-first iota-second)
                      (member lex-ref arith-functions :test #'char=)
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
                                                  (funcall function number (vapip-factor iota))))))))))))))
