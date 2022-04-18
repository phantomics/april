;;;; varray.lisp

(in-package #:varray)

;; virtual array - the ancestor class
(defclass varray ()
  ((shape :accessor varray-shape ; the array's shape - typically populated by a (shape-of) method
          :initform nil
          :initarg :shape)))

;; to get the element type of an array
(defgeneric etype-of (varray))

;; to get the shape of an array
(defgeneric shape-of (varray))

;; to get an indexing function for an array
(defgeneric indexer-of (varray))

;; to render an array into memory
(defgeneric render (varray))

;; the default element type is t
(defmethod etype-of ((varray varray))
  t)

;; by default, get the array's stored shape
(defmethod shape-of ((varray varray))
  (varray-shape varray))

(defmethod render ((varray varray))
  (let ((indexer (indexer-of varray))
        (output (make-array (shape-of varray)
                            :element-type (etype-of varray))))
    (dotimes (i (array-total-size output))
      (setf (row-major-aref output i)
            (funcall indexer i)))
    output))

;; primal array - a virtual array defined wholly by its parameters, not derived from another array
(defclass varray-primal (varray) nil)

;; derived array - a virtual array derived from another array
(defclass varray-derived (varray)
  ((base :accessor vader-base ; the array from which the array is derived
         :initform nil
         :initarg :base)))

(defmacro with-base-shape (base-shape array-symbol &body body)
  `(let ((,base-shape (shape-of (vader-base ,array-symbol))))
     ,@body))

(defmacro get-or-assign-shape (object form)
  `(or (call-next-method) (setf (varray-shape ,object) ,form)))

;; integer progression vector - a series of numeric values generated by [⍳ index]
(defclass vvector-integer-progression (varray-primal)
  ((number :accessor vvip-number ; the number of values
           :initform 1
           :initarg :number)
   (origin :accessor vvip-origin ; the origin point - by default, the index origin
           :initform 0
           :initarg :origin)
   (factor :accessor vvip-factor ; multiplying factor of values
           :initform 1
           :initarg :factor)
   (repeat :accessor vvip-repeat ; value repetition count
           :initform 1
           :initarg :repeat)))

(defmethod etype-of ((vvector vvector-integer-progression))
  (if (floatp (vvip-factor vvector))
      'double-float (list 'integer (min 0 (vvip-origin vvector))
                          (+ (vvip-origin vvector)
                             (first (shape-of vvector))))))

;; the shape of an IP vector is its number times its repetition
(defmethod shape-of ((vvector vvector-integer-progression))
  (get-or-assign-shape vvector (list (* (vvip-number vvector)
                                        (vvip-repeat vvector)))))

;; the IP vector's parameters are used to index its contents
(defmethod indexer-of ((vvector vvector-integer-progression))
  (lambda (index)
    (if (< -1 index (vvip-number vvector))
        (* (+ (floor index (vvip-repeat vvector))
              (vvip-origin vvector))
           (vvip-factor vvector)))))

;; a reshaped array as with the [⍴ shape] function
(defclass vader-reshape (varray-derived)
  ((argument :accessor vareshape-argument
             :initform nil
             :initarg :argument)))

(defmethod shape-of ((varray vader-reshape))
  "The shape of a reshaped array is simply its argument."
  (vareshape-argument varray))

;; a sectioned array as from the [↑ take] or [↓ drop] functions
(defclass vader-section (varray-derived)
  ((argument :accessor vasec-argument
             :initform nil
             :initarg :argument)
   (inverse :accessor vasec-inverse
            :initform nil
            :initarg :inverse)
   (axis :accessor vasec-axis
         :initform :last
         :initarg :axis)))

(defmethod shape-of ((varray vader-section))
  "The shape of a sectioned array is the parameters (if not inverse, as for [↑ take]) or the difference between the parameters and the shape of the original array (if inverse, as for [↓ drop])."
  (get-or-assign-shape varray (if (vasec-inverse varray)
                                  (with-base-shape base-shape varray
                                    (loop :for b :in base-shape :for d :in (vasec-argument varray)
                                          :collect (- b (abs d))))
                                  (vasec-argument varray))))

;; a rotated array as from the [⌽ rotate] function
(defclass vader-turn (varray-derived)
  ((argument :accessor vaturn-argument
             :initform nil
             :initarg :argument)
   (axis :accessor vaturn-axis
         :initform :last
         :initarg :axis)))

(defmethod shape-of ((varray vader-turn))
  "The shape of a rotated array is the same as the original array."
  (get-or-assign-shape varray (shape-of (vader-base varray))))

;; a permuted array as from the [⍉ permute] function
(defclass vader-permute (varray-derived)
  ((argument :accessor vapermute-argument
             :initform nil
             :initarg :argument)))

