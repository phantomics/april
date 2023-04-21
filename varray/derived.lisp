;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Varray -*-
;;;; derived.lisp

(in-package #:varray)

"Definitions of virtual arrays derived from other arrays, implementing most APL array processing functions."

;; superclasses encompassing array derivations taking different types of parameters

(defclass vad-reindexing ()
  nil (:metaclass va-class)
  (:documentation "Superclass of array transformations that add an index transformation to those accumulated."))

(defclass vad-on-axis ()
  ((%axis :accessor vads-axis
          :initform nil
          :initarg :axis
          :documentation "The axis along which to transform."))
  (:metaclass va-class)
  (:documentation "Superclass of array transformations occuring along an axis."))

(defclass vad-with-default-axis ()
  ((%default-axis :accessor vads-default-axis
                  :initform nil
                  :initarg :default-axis
                  :documentation "The default axis along which to transform."))
  (:metaclass va-class)
  (:documentation "Superclass of array transformations with a default axis."))

(defclass vad-with-argument ()
  ((%argument :accessor vads-argument
              :initform nil
              :initarg :argument
              :documentation "Parameters passed to an array transformation as an argument."))
  (:metaclass va-class)
  (:documentation "Superclass of array transformations occuring along an axis."))

(defclass vad-with-ct ()
  ((%comp-tolerance :accessor vads-ct
                    :initform 0
                    :initarg :comparison-tolerance
                    :documentation "Parameter specifying the comparison tolerance for an array operation."))
  (:metaclass va-class)
  (:documentation "Superclass of array transformations taking comparison tolerance as an implicit argument."))

(defclass vad-with-rng ()
  ((%rng :accessor vads-rng
         :initform nil
         :initarg :rng
         :documentation "Random number generator specification for array transformations that use it."))
  (:metaclass va-class)
  (:documentation "Superclass of array transformations occuring along an axis."))

(defclass vad-maybe-shapeless ()
  ((%determined :accessor vads-shapeset
                :initform nil
                :initarg :determined
                :documentation "Whether array's shape is determined."))
  (:metaclass va-class)
  (:documentation "Superclass of array transformations taking index origin as an implicit argument."))

(defclass vad-invertable ()
  ((%inverse :accessor vads-inverse
             :initform nil
             :initarg :inverse
             :documentation "Parameters passed to an array transformation as an argument."))
  (:metaclass va-class)
  (:documentation "Superclass of array transformations that have an inverse variant as [↓ drop] is to [↑ take]."))

(defclass vad-limitable ()
  nil (:metaclass va-class)
  (:documentation "Superclass of indefinite array transformations whose output can be dimensionally limited to avoid needless computation."))

(defclass vad-render-mutable ()
  ((%rendered :accessor vads-rendered
              :initform nil
              :initarg :rendered
              :documentation "Whether the array has been rendered."))
  (:metaclass va-class)
  (:documentation "Superclass of array transformations whose content may be changed in place upon rendering and whose rendered/non-rendered status must therefore be tracked."))

;; this is nested for the case of ≡↓↓2 3⍴⍳6
(defclass vader-subarray (vad-nested varray-derived)
  ((%index :accessor vasv-index
           :initform nil
           :initarg :index
           :documentation "Index variable for subarray; often used to index the subarray as part of the larger array it derives from."))
  (:metaclass va-class)
  (:documentation "Subarray of a larger array; its relationship to the larger array is defined by its index parameter, indexer and prototype functions."))

(defmethod prototype-of ((varray vader-subarray))
  (varray-prototype varray))

(defmethod generator-of ((varray vader-subarray) &optional indexers params)
  (declare (ignore indexers))
  (case (getf params :base-format)
    (:encoded)
    (:linear)
    (t (varray-generator varray))))

(defmethod assign-rank ((varray vad-on-axis) rank)
  "Assign a rank at which the [⌽⊖ rotate] function shall operate, for its composition with the [⍤ rank] operator."
  (when (= 1 rank)
    (setf (vads-axis varray) (- (rank-of (vader-base varray))
                                (abs (1- (vads-io varray)))))
    varray))

(defclass vader-random (varray-derived vad-with-rng vad-with-io)
  nil (:metaclass va-class)
  (:documentation "An array of randomized elements as from the [? random] function."))

(defmethod etype-of ((varray vader-random))
  (declare (ignore varray)) ;; TODO: define narrower types in certain cases
  t)

(defun apl-random-process (item index-origin generator)
  "Core of (apl-random), randomizing an individual integer or float."
  (if (integerp item)
      (if (zerop item) (if (eq :system generator)
                           (+ double-float-epsilon (random (- 1.0d0 (* 2 double-float-epsilon))))
                           (random-state:random-float generator double-float-epsilon
                                                      (- 1.0d0 double-float-epsilon)))
          (if (eq :system generator) (+ index-origin (random item))
              (random-state:random-int generator index-origin (1- (+ item index-origin)))))
      (if (floatp item)
          (if (eq :system generator)
              (random item)
              (random-state:random-float
               generator double-float-epsilon (- (coerce item 'double-float)
                                                 double-float-epsilon)))
          (error "The right argument to ? can only contain non-negative integers or floats."))))

(defmethod shape-of ((varray vader-random))
  (get-promised (varray-shape varray) (shape-of (vader-base varray))))

(defmethod generator-of ((varray vader-random) &optional indexers params)
  (declare (ignore indexers))
  (let* ((rngs (vads-rng varray))
         (base-gen (generator-of (vader-base varray)))
         (scalar-base (not (or (varrayp (vader-base varray))
                               (arrayp (vader-base varray)))))
         (gen-name (getf (rest rngs) :rng))
         (generator (or (getf (rest rngs) gen-name)
                        (setf (getf (rest rngs) gen-name)
                              (if (eq :system gen-name)
                                  :system (random-state:make-generator gen-name)))))
         ;; (seed (getf (rest rngs) :seed))
         )
    
     ;; randomized array content is generated synchronously
     ;; and cached in case a random seed is in use
     (when (and ; seed
            (not (vader-content varray)))
       (setf (vader-content varray)
             (make-array (size-of (vader-base varray))
                         :element-type (etype-of varray)))
       (loop :for i :below (size-of (vader-base varray))
             :do (setf (row-major-aref (vader-content varray) i)
                       (apl-random-process (if (not (functionp base-gen))
                                               base-gen (funcall base-gen i))
                                           (vads-io varray) generator))))
    
    (case (getf params :base-format)
      (:encoded)
      (:linear)
      (t (lambda (index)
           (if scalar-base (apl-random-process (funcall base-gen index) (vads-io varray)
                                               generator)
               (row-major-aref (vader-content varray) index)
               ;; (if seed (row-major-aref (vader-content varray) index)
               ;;     (apl-random-process (funcall base-gen index) (vads-io varray)
               ;;                         generator))
               ))))))

(defclass vader-deal (varray-derived vad-with-argument vad-with-rng vad-with-io)
  ((%cached :accessor vadeal-cached
            :initform nil
            :initarg :cached
            :documentation "Cached shuffled vector elements."))
  (:metaclass va-class)
  (:documentation "A shuffled index vector as from the [? deal] function."))

(defmethod etype-of ((varray vader-deal))
  (if (/= 1 (reduce #'* (shape-of (vads-argument varray))))
      (error "Both arguments to ? must be non-negative integers.")
      (let* ((base-gen (generator-of (vader-base varray)))
             (max (if (not (functionp base-gen))
                      base-gen (funcall base-gen 0))))
        (list 'integer 0 (+ max (vads-io varray))))))

(defmethod prototype-of ((varray vader-deal))
  (declare (ignore varray))
  0)

(defmethod shape-of ((varray vader-deal))
  (get-promised (varray-shape varray)
                (let ((arg-indexer (generator-of (vads-argument varray))))
                  (if (/= 1 (reduce #'* (shape-of (vads-argument varray))))
                      (error "Both arguments to ? must be non-negative integers.")
                      (let ((length (if (not (functionp arg-indexer))
                                        arg-indexer (funcall arg-indexer 0))))
                        (if (integerp length) (list length)
                            (error "Both arguments to ? must be non-negative integers.")))))))

(defmethod generator-of ((varray vader-deal) &optional indexers params)
  (declare (ignore indexers))
  (let* ((rngs (vads-rng varray))
         (base-gen (generator-of (vader-base varray)))
         (count (if (not (functionp base-gen))
                    base-gen (funcall base-gen 0)))
         (gen-name (getf (rest rngs) :rng))
         (generator (or (getf (rest rngs) gen-name)
                        (setf (getf (rest rngs) gen-name)
                              (if (eq :system gen-name)
                                  :system (random-state:make-generator gen-name)))))
         (vector (make-array count :element-type (etype-of varray))))

    (setf (vader-content varray) vector)
    (dotimes (x (length vector))
      (setf (aref vector x) (+ x (vads-io varray))))
    (loop :for i :from count :downto 2
          :do (rotatef (aref vector (if (eq :system generator)
                                        (random i)
                                        (random-state:random-int generator 0 (1- i))))
                       (aref vector (1- i))))
    
    (case (getf params :base-format)
      (:encoded)
      (:linear)
      (t (lambda (index) (aref (vader-content varray) index))))))

(defclass vader-without (varray-derived vad-with-argument vad-limitable)
  nil (:metaclass va-class)
  (:documentation "An array without given elements as from the [~ without] function."))

(defmethod etype-of ((varray vader-without))
  (type-in-common (etype-of (vader-base varray))
                  (etype-of (vads-argument varray))))

(defmethod shape-of ((varray vader-without))
  (get-promised (varray-shape varray)
                (let ((this-indexer (generator-of varray)))
                  (declare (ignore this-indexer))
                  (shape-of (vader-content varray)))))

(defmethod generator-of ((varray vader-without) &optional indexers params)
  (declare (ignore indexers))
  (flet ((compare (o a)
           (funcall (if (and (characterp a) (characterp o))
                        #'char= (if (and (numberp a) (numberp o))
                                    #'= (lambda (a o) (declare (ignore a o)))))
                    o a)))
    (let ((derivative-count (when (and (varray-shape varray)
                                       (listp (varray-shape varray)))
                              (reduce #'* (varray-shape varray))))
          (contents (render (vader-base varray)))
          (argument (render (vads-argument varray))))
      (if (and (arrayp argument)
               (< 1 (rank-of argument)))
          (error "The left argument to [~~ without] must be a vector.")
          (let* ((included) (count 0)
                 (cont-vector (if (or (vectorp contents) (not (arrayp contents)))
                                  contents
                                  (make-array (size-of contents)
                                              :displaced-to contents
                                              :element-type (etype-of contents)))))
            (if (vectorp argument)
                (loop :for element :across argument :while (or (not derivative-count)
                                                               (< count derivative-count))
                      :do (let ((include t))
                            (if (vectorp cont-vector)
                                (loop :for ex :across cont-vector
                                      :do (when (compare ex element) (setq include nil)))
                                (when (compare cont-vector element) (setq include nil)))
                            (when include (push element included)
                                  (incf count))))
                (let ((include t))
                  (if (vectorp cont-vector)
                      (loop :for ex :across cont-vector
                            :do (when (compare ex argument) (setq include nil)))
                      (when (compare cont-vector argument) (setq include nil)))
                  (when include (push argument included)
                        (incf count))))
            ;; if the shape set by an upstream limiting function is greater than the actual number of
            ;; results in the output vector, as with 20↑'This is a test'~' ', correct the length
            (when (or (not (fulfilledp (varray-shape varray)))
                      (< (length included) (first (varray-shape varray))))
              (setf (varray-shape varray) (list (length included))))
            ;; (print (list :ii included))
            (setf (vader-content varray)
                  (make-array (length included) :element-type (etype-of argument)
                                                :initial-contents (reverse included)))))
      (case (getf params :base-format)
        (:encoded)
        (:linear)
        (t (lambda (index) (aref (vader-content varray) index)))))))

(defclass vader-umask (varray-derived vad-limitable)
  nil (:metaclass va-class)
  (:documentation "The unique mask of an array as from the [≠ unique mask] function."))

(defmethod etype-of ((varray vader-umask))
  (declare (ignore varray))
  'bit)

(defmethod prototype-of ((varray vader-umask))
  (declare (ignore varray))
  0)

(defmethod shape-of ((varray vader-umask))
  (get-promised (varray-shape varray)
                (let ((this-indexer (generator-of varray)))
                  (declare (ignore this-indexer))
                  (shape-of (vader-content varray)))))

(defmethod generator-of ((varray vader-umask) &optional indexers params)
  (declare (ignore indexers))
  (let* ((derivative-count (when (and (varray-shape varray)
                                      (listp (varray-shape varray)))
                             (reduce #'* (varray-shape varray))))
         (base-shape (shape-of (vader-base varray)))
         (contents (render (vader-base varray)))
         (output-length (if (not derivative-count) (first base-shape)
                            (min derivative-count (first base-shape))))
         (increment (reduce #'* (rest base-shape)))
         (displaced (when (< 1 (array-rank contents))
                      (make-array (rest base-shape) :displaced-to contents
                                                    :element-type (etype-of contents))))
         (uniques)
         (output (make-array output-length :element-type 'bit :initial-element 1)))
    (dotimes (x output-length)
      (if (and displaced (< 0 x))
          (setq displaced (make-array (rest base-shape)
                                      :displaced-to contents 
                                      :element-type (etype-of contents) 
                                      :displaced-index-offset (* x increment))))
      (if (member (or displaced (aref contents x)) uniques :test #'array-compare)
          (setf (aref output x) 0)
          (push (if displaced (make-array (rest base-shape)
                                          :displaced-to contents
                                          :element-type (etype-of contents)
                                          :displaced-index-offset (* x increment))
                    (aref contents x))
                uniques)))
    (setf (vader-content varray) output)
    (case (getf params :base-format)
      (:encoded)
      (:linear)
      (t (lambda (index) (aref (vader-content varray) index))))))

(defclass vader-inverse-where (varray-derived vad-with-argument vad-with-io)
  nil (:metaclass va-class)
  (:documentation "An inverted product of the [⍸ where] function."))

(defmethod prototype-of ((varray vader-inverse-where))
  (declare (ignore varray))
  0)

(defmethod etype-of ((varray vader-inverse-where))
  (declare (ignore varray))
  'bit)

(defmethod shape-of ((varray vader-inverse-where))
  (get-promised
   (varray-shape varray)
   (let ((base (vader-base varray)))
     (typecase base (vader-identity (setf base (vader-base base))))
     (typecase base
       (vapri-arith-provec
        (if (and (= 1 (vapip-repeat base))
                 (= 1 (vapip-factor base))
                 (= (vads-io varray) (vapip-origin base)))
            (list (+ (vads-io varray) (vapip-number varray)))
            (error "Attempted to invoke inverse [⍸ where] on an altered integer progression vector.")))
       (vapri-coordinate-identity (vapci-shape base))
       (vader-where (shape-of (vader-base base)))
       (array (let* ((rank (if (not (vectorp (aref base 0)))
                               1 (length (aref base 0))))
                     (dims (make-array rank :initial-element 0)))
                (if (= 1 rank)
                    (list (aref base (1- (length base))))
                    (progn (dotimes (i (array-total-size base))
                             (if (= rank (length (row-major-aref base i)))
                                 (loop :for b :across (row-major-aref base i) :for i :from 0
                                       :when (not (and (integerp b) (or (zerop b) (plusp b))))
                                         :do (error "t")
                                       :when (> b (aref dims i)) :do (setf (aref dims i) b))
                                 (error "This array contains inconsistent coordinate vectors and cannot be an argument to the inverse [⍸ where] function.")))
                           (coerce dims 'list)))))
       (t (error "The inverse [⍸ where] function cannot be applied to this object."))))))

(defmethod generator-of ((varray vader-inverse-where) &optional indexers params)
  (declare (ignore indexers))
  (let ((base (vader-base varray)))
     (typecase base (vader-identity (setf base (vader-base base))))
    (typecase base
      (vapri-arith-provec 1)
      (vapri-coordinate-identity 1)
      (vader-where
       (setf (vader-content varray)
             (make-array (shape-of varray) :element-type 'bit :initial-element 0))
       (loop :for i :across (vader-content base)
             :do (setf (row-major-aref (vader-content varray) i) 1))
       (lambda (index) (row-major-aref (vader-content base) index)))
      (array
       (setf (vader-content varray)
             (make-array (shape-of varray) :element-type 'bit :initial-element 0))
       (dotimes (i (array-total-size base))
         (setf (varef (vader-content varray)
                      (row-major-aref base i)
                      (vads-io varray))
               1))
       (case (getf params :base-format)
         (:encoded)
         (:linear)
         (t (lambda (index) (row-major-aref (vader-content varray) index))))))))

(defclass vader-index (varray-derived vad-with-argument vad-with-io)
  ((%cache :accessor vaix-cache
           :initform nil
           :initarg :cache
           :documentation "Cached items to search in array.")
   (%set   :accessor vaix-set
           :initform nil
           :initarg :set
           :documentation "Cached items to search in array."))
  (:metaclass va-class)
  (:documentation "An indexed array as from the [⍳ index of] function."))

(defmethod prototype-of ((varray vader-index))
  (declare (ignore varray))
  0)

(defmethod etype-of ((varray vader-index))
  (list 'integer 0 (max 255 (1+ (first (last (shape-of (vads-argument varray))))))))

(defmethod shape-of ((varray vader-index))
  (get-promised (varray-shape varray)
                (butlast (shape-of (vader-base varray))
                         (1- (length (shape-of (vads-argument varray)))))))

(defmethod generator-of ((varray vader-index) &optional indexers params)
  (declare (ignore indexers))
  (let* ((base-gen (generator-of (vader-base varray)))
         (argument (or (vaix-set varray)
                       (setf (vaix-set varray) (render (vads-argument varray)))))
         (arg-cell-size (reduce #'* (rest (shape-of argument))))
         (major-cells-count (when (/= 1 arg-cell-size) (first (shape-of argument))))
         (base (when major-cells-count (or (vaix-cache varray)
                                           (setf (vaix-cache varray)
                                                 (render (vader-base varray)))))))
    (if major-cells-count ;; if comparing sub-arrays
        (case (getf params :base-format)
          (:encoded)
          (:linear)
          (t (lambda (index)
               (let ((base-segment (make-array (rest (shape-of argument))
                                               :element-type (etype-of base) :displaced-to base
                                               :displaced-index-offset (* index arg-cell-size))))
                 (loop :for a :below major-cells-count
                       :while (not (varray-compare base-segment
                                                   (make-array (rest (shape-of argument))
                                                               :displaced-to argument
                                                               :element-type (etype-of argument)
                                                               :displaced-index-offset (* arg-cell-size a))))
                       :counting a :into asum :finally (return (+ asum (vads-io varray))))))))
        ;; if comparing individual vector elements
        (case (getf params :base-format)
          (:encoded)
          (:linear)
          (t (lambda (index)
               (let ((base-index (if (not (functionp base-gen))
                                     base-gen (funcall base-gen index))))
                 (loop :for a :across argument :while (not (varray-compare a base-index))
                       :counting a :into asum :finally (return (+ asum (vads-io varray)))))))))))

(defclass vader-shape (varray-derived)
  nil (:metaclass va-class)
  (:documentation "The shape of an array as from the [⍴ shape] function."))

(defmethod prototype-of ((varray vader-shape))
  (declare (ignore varray))
  0)

(defmethod etype-of ((varray vader-shape))
  (or (apply #'type-in-common (loop :for dimension :in (shape-of (vader-base varray))
                                    :collect (assign-element-type dimension)))
      t))

(defmethod shape-of ((varray vader-shape))
  "The shape of a reshaped array is simply its argument."
  (get-promised (varray-shape varray) (list (length (shape-of (vader-base varray))))))

(defmethod generator-of ((varray vader-shape) &optional indexers params)
  (declare (ignore indexers))
  "Index a reshaped array."
  (unless (vader-content varray)
    (setf (vader-content varray) ;; TODO: control type of the shape vector
          (coerce (shape-of (vader-base varray)) 'vector)))
  (case (getf params :base-format)
    (:encoded)
    (:linear)
    (t (lambda (index) (aref (vader-content varray) index)))))

(defclass vader-reshape (varray-derived vad-with-argument vad-reindexing)
  nil (:metaclass va-class)
  (:documentation "A reshaped array as from the [⍴ reshape] function."))

(defmethod prototype-of ((varray vader-reshape))
  (prototype-of (vader-base varray)))

(defmethod shape-of ((varray vader-reshape))
  "The shape of a reshaped array is simply its argument."
  (get-promised (varray-shape varray)
                (let ((arg (setf (vads-argument varray)
                                 (render (vads-argument varray)))))
                  (if (typep arg 'sequence)
                      (coerce arg 'list)
                      (list arg)))))

(defmethod indexer-of ((varray vader-reshape) &optional params)
  "Index a reshaped array."
  (declare (ignore params))
  (let* ((base-gen (generator-of (vader-base varray)))
         (input-size (the (unsigned-byte 62)
                          (max (the bit 1)
                               (the fixnum (size-of (vader-base varray))))))
         (output-shape (shape-of varray))
         (output-size (the (unsigned-byte 62) (reduce #'* output-shape))))
    (if (shape-of (vader-base varray))
        (if output-shape (if (<= output-size input-size)
                             #'identity (lambda (index) (mod index input-size)))
            (lambda (index) (declare (ignore index)) 0))
        (if (functionp base-gen)
            (lambda (index) (declare (ignore index)) 0)
            (lambda (index) (declare (ignore index)) base-gen)))))

(defmethod generator-of ((varray vader-reshape) &optional indexers params)
  (let ((output-size (size-of varray)))
    (when (and (typep (vader-base varray) 'vad-nested)
               (vads-nested (vader-base varray))
               (< (size-of (vader-base varray)) (size-of varray)))
      ;; render the base if it's a subarray with nested enabled, as for
      ;; 2 2⍴(⊂2 2⍴⍳4) 2 3 or 2 3⍴⊂2 2⍴⍳4, where the enclosed subarray will otherwise attempt to
      ;; render twice and cause a race condition - TODO: is there a more efficient way to do this?
      (render (vader-base varray)))
    (if (zerop output-size)
        (let ((prototype (prototype-of varray)))
          (lambda (index) (declare (ignore index)) prototype))
        (if (zerop (size-of (vader-base varray)))
            (prototype-of (vader-base varray))
            (let ((base-gen (generator-of (vader-base varray))))
              (case (getf params :format)
                (:encoded (let ((enco-type (getf (getf params :gen-meta) :index-width))
                                (coord-type (getf (getf params :gen-meta) :index-type)))
                            (setf (getf params :format) :linear)
                            (generator-of varray
                                          (cons (decode-rmi enco-type coord-type (rank-of varray)
                                                            (get-dimensional-factors (shape-of varray) t))
                                                indexers)
                                          params)))
                (:linear (if (shape-of (vader-base varray))
                             (generator-of (vader-base varray)
                                           (cons (indexer-of varray params) indexers)
                                           params)
                             (lambda (index)
                               (declare (ignore index))
                               (if (not (functionp base-gen))
                                   base-gen (funcall base-gen 0)))))
                (t (if (shape-of (vader-base varray))
                       (let ((indexer (indexer-of varray params)))
                         (generator-of (vader-base varray) (cons indexer indexers)))
                       (lambda (index)
                         (declare (ignore index))
                         (if (not (functionp base-gen))
                             base-gen (funcall base-gen 0)))))))))))

(defclass vader-depth (varray-derived)
  nil (:metaclass va-class)
  (:documentation "The first dimension of an array as from the [≢ first dimension] function."))

(defmethod prototype-of ((varray vader-depth))
  (declare (ignore varray))
  0)

(defmethod etype-of ((varray vader-depth))
  'fixnum)

(defmethod shape-of ((varray vader-depth))
  "The shape of a reshaped array is simply its argument."
  (declare (ignore varray))
  nil)

(defun varray-depth (input &optional layer (uniform t) possible-depth)
  "Find the maximum depth of nested arrays within an array."
  (let ((indexer (generator-of input)))
    ;; (print (list :ind indexer))
    (flet ((vap (item) (or (arrayp item) (varrayp item))))
      (if (not (functionp indexer))
          (if (and (not (shape-of indexer))
                   (not (arrayp indexer))
                   (not (varrayp indexer)))
              1 (1+ (varray-depth indexer 1)))
          (let* ((first-level (not layer))
                 (layer (or layer 1))
                 (new-layer layer))
            (dotimes (i (size-of input))
              (let ((item (funcall indexer i)))
                (if (vap item)
                    (multiple-value-bind (next-layer new-uniform new-possible-depth)
                        (varray-depth item (1+ layer) uniform possible-depth)
                      (setq new-layer (max new-layer next-layer)
                            uniform new-uniform
                            possible-depth new-possible-depth))
                    (if possible-depth (when (/= layer possible-depth) (setq uniform nil))
                        (setq possible-depth new-layer)))))
            (values (funcall (if (and first-level (not uniform)) #'- #'identity)
                             new-layer)
                    uniform possible-depth))))))

(defmethod generator-of ((varray vader-depth) &optional indexers params)
  "Index an array depth value."
  (declare (ignore indexers params))
  (if (not (or (shape-of (vader-base varray))
               (varrayp (vader-base varray))
               (arrayp (vader-base varray))))
      0 (varray-depth (vader-base varray))))

(defclass vader-first-dim (varray-derived)
  nil (:metaclass va-class)
  (:documentation "The first dimension of an array as from the [≢ first dimension] function."))

(defmethod prototype-of ((varray vader-first-dim))
  (declare (ignore varray))
  0)

(defmethod etype-of ((varray vader-first-dim))
  (list 'integer 0 (first (shape-of (vader-base varray)))))

(defmethod shape-of ((varray vader-first-dim))
  "The shape of a reshaped array is simply its argument."
  (declare (ignore varray))
  nil)

(defmethod generator-of ((varray vader-first-dim) &optional indexers params)
  "Index a reshaped array."
  (declare (ignore indexers))
  (case (getf params :base-format)
    (:encoded)
    (:linear)
    (t (or (first (shape-of (vader-base varray))) 1))))
    
(defun varray-compare (item1 item2 &optional comparison-tolerance)
  "Perform a deep comparison of two APL arrays, which may be multidimensional or nested."
  (let ((shape1 (shape-of item1))
        (shape2 (shape-of item2))
        (rank1 (rank-of item1))
        (rank2 (rank-of item2))
        (generator1 (generator-of item1))
        (generator2 (generator-of item2)))
    (if shape1 (and shape2 (= rank1 rank2)
                    (loop :for d1 :in shape1 :for d2 :in shape2 :always (= d1 d2))
                    ;; compared arrays must be either character or non-character to match
                    (or (< 0 (size-of item1)) ;; 0-size arrays must be of same type, as for ⍬≡''
                        (if (not (position (etype-of item1) #(character base-char) :test #'eql))
                            (not (position (etype-of item2) #(character base-char) :test #'eql))
                            (position (etype-of item2) #(character base-char) :test #'eql)))
                    ;; TODO: this can be optimized - currently the
                    ;; function check happens for every element
                    (loop :for i :below (size-of item1)
                          :always (varray-compare (if (not (functionp generator1))
                                                      generator1 (funcall generator1 i))
                                                  (if (not (functionp generator2))
                                                      generator2 (funcall generator2 i)))))
        (unless shape2
          (or (and (arrayp item1) (arrayp item2)
                   (varray-compare (funcall generator1 0) (funcall generator2 0)))
              (and comparison-tolerance (floatp generator1) (floatp generator2)
                   (> comparison-tolerance (abs (- generator1 generator2))))
              (and (numberp generator1)
                   (numberp generator2)
                   (= item1 generator2))
              (and (characterp generator1)
                   (characterp generator2)
                   (char= item1 generator2)))))))

(defclass vader-compare (varray-derived vad-with-ct vad-invertable)
  nil (:metaclass va-class)
  (:documentation "The first dimension of an array as from the [≢ first dimension] function."))

(defmethod prototype-of ((varray vader-compare))
  (declare (ignore varray))
  0)

(defmethod etype-of ((varray vader-compare))
  (declare (ignore varray))
  'bit)

(defmethod shape-of ((varray vader-compare))
  "The shape of a comparison result is always nil."
  (declare (ignore varray))
  nil)

(defmethod generator-of ((varray vader-compare) &optional indexers params)
  (declare (ignore indexers))
  (let ((omega (render (aref (vader-base varray) 0))) ;; TODO: why is rendering needed?
        (alpha (render (aref (vader-base varray) 1))))
    (case (getf params :base-format)
      (:encoded)
      (:linear)
      (t (if (funcall (if (vads-inverse varray) #'not #'identity)
                      (varray-compare omega alpha (vads-ct varray)))
             1 0)))))

(defclass vader-enlist (varray-derived vad-limitable)
  nil (:metaclass va-class)
  (:documentation "An array decomposed into a vector as from the [∊ enlist] function."))

(defmethod shape-of ((varray vader-enlist))
  (get-promised (varray-shape varray)
                (let ((this-indexer (generator-of varray)))
                  (declare (ignore this-indexer))
                  (shape-of (vader-content varray)))))

(defmethod generator-of ((varray vader-enlist) &optional indexers params)
  (declare (ignore indexers))
  (let ((base-gen (generator-of (vader-base varray))))
    (unless (vader-content varray)
      (let ((first-item (if (not (functionp base-gen))
                            base-gen (funcall base-gen 0))))
        (if (and (not (shape-of (vader-base varray)))
                 (not (or (arrayp first-item)
                          (varrayp first-item))))
            (setf (vader-content varray)
                  (make-array 1 :initial-element first-item :element-type (etype-of first-item)))
            (if (not (eq t (etype-of (vader-base varray))))
                (setf (vader-content varray)
                      (make-instance 'vader-pare :base (vader-base varray)))
                (let ((index 0) (length 0)
                      (input (render (vader-base varray)))
                      (derivative-count (when (getf params :shape-deriving)
                                          (reduce #'* (getf params :shape-deriving)))))
                  (labels ((measure-array (in)
                             (loop :for i :below (array-total-size in)
                                   :while (or (not derivative-count)
                                              (< length derivative-count))
                                   :do (if (arrayp (row-major-aref in i))
                                           (measure-array (row-major-aref in i))
                                           (incf length))))
                           (copy-contents (in destination)
                             (loop :for i :below (array-total-size in)
                                   :while (or (not derivative-count)
                                              (< index derivative-count))
                                   :do (if (arrayp (row-major-aref in i))
                                           (copy-contents (row-major-aref in i) destination)
                                           (progn (setf (row-major-aref destination index)
                                                        (row-major-aref in i))
                                                  (incf index))))))
                    (measure-array input)
                    (let ((output (make-array length)))
                      (copy-contents input output)
                      (setf (vader-content varray) output))))))))
    (case (getf params :base-format)
      (:encoded)
      (:linear)
      (t (generator-of (vader-content varray))))))

(defclass vader-membership (varray-derived vad-with-argument)
  ((%to-search :accessor vamem-to-search
               :initform nil
               :initarg :to-search
               :documentation "Set of elements to be checked for membership in array."))
  (:metaclass va-class)
  (:documentation "A membership array as from the [∊ membership] function."))

(defmethod etype-of ((varray vader-membership))
  (declare (ignore varray))
  'bit)

(defmethod generator-of ((varray vader-membership) &optional indexers params)
  (declare (ignore indexers))
  (let ((base (render (vader-base varray))))
    ;; TODO: does this require rendering?
    (labels ((compare (item1 item2)
               (if (and (characterp item1) (characterp item2))
                   (char= item1 item2)
                   (if (and (numberp item1) (numberp item2))
                       (= item1 item2)
                       (when (and (arrayp item1) (arrayp item2))
                         (varray-compare item1 item2))))))
      (unless (vamem-to-search varray)
        (let ((argument (render (vads-argument varray))))
          ;; TODO: possible to optimize this?
          (if (arrayp base)
              (setf (vamem-to-search varray) argument)
              (if (not (arrayp argument))
                  (setf (vamem-to-search varray)
                        (if (compare argument base) 1 0))
                  (setf (vamem-to-search varray)
                        (if (not (loop :for i :below (array-total-size argument)
                                       :never (compare base (row-major-aref argument i))))
                            1 0))))))
      
      (case (getf params :base-format)
        (:encoded)
        (:linear)
        (t (if (arrayp (vamem-to-search varray))
               (lambda (index)
                 (let ((found))
                   (loop :for ix :below (size-of (vamem-to-search varray)) :while (not found)
                         :do (setq found (compare (row-major-aref base index)
                                                  (row-major-aref (vamem-to-search varray) ix))))
                   (if found 1 0)))
               (if (arrayp base)
                   (lambda (index)
                     (if (compare (vamem-to-search varray) (row-major-aref base index))
                         1 0))
                   (vamem-to-search varray))))))))

(defclass vader-find (varray-derived vad-with-argument)
  ((%pattern :accessor vafind-pattern
             :initform nil
             :initarg :pattern
             :documentation "Pattern to find.")
   (%cached  :accessor vafind-cached
             :initform nil
             :initarg :cached
             :documentation "Cached data to search."))
  (:metaclass va-class)
  (:documentation "An array of search matches as from the [⍷ find] function."))

(defmethod etype-of ((varray vader-find))
  (declare (ignore varray))
  'bit)

(defmethod prototype-of ((varray vader-find))
  (declare (ignore varray))
  0)

(defmethod generator-of ((varray vader-find) &optional indexers params)
  (declare (ignore indexers))
  (let* ((target (or (vafind-pattern varray)
                     (setf (vafind-pattern varray)
                           (render (vads-argument varray)))))
         (is-target-array (arrayp target))
         (target-shape (shape-of target))
         (target-rank (length target-shape))
         (target-size (size-of target))
         (target-factors (get-dimensional-factors target-shape t))
         (tshape-vector (coerce target-shape 'vector))
         (base (when is-target-array
                 (or (vafind-cached varray)
                     (setf (vafind-cached varray)
                           (render (vader-base varray))))))
         (base-gen (unless is-target-array (generator-of (vader-base varray))))
         (base-shape (shape-of base))
         (base-factors (get-dimensional-factors base-shape t))
         (rank-delta (- (length base-factors) target-rank))
         (target-head (if (not is-target-array)
                          target (row-major-aref target 0))))
    
    (case (getf params :base-format)
      (:encoded)
      (:linear)
      (t (if is-target-array
             (lambda (index)
               (if (not (array-compare target-head (row-major-aref base index)))
                   0 (let ((remaining index) (valid t) (sx 0))
                       ;; determine whether the target array fits within the dimensions
                       ;; of the searched array from the row-major starting point
                       (loop :for f :across base-factors :for b :in base-shape :for ix :from 0 :while valid
                             :do (multiple-value-bind (item remainder) (floor remaining f)
                                   (when (>= ix rank-delta)
                                     (if (< b (+ item (aref tshape-vector sx)))
                                         (setf valid nil)
                                         (incf sx)))
                                   (setf remaining remainder)))
                       (if (not valid)
                           0 (progn
                               (loop :for it :below target-size :while valid
                                     :do (let ((tremaining it) (bremaining index)
                                               (base-index 0) (tfactors target-factors))
                                           (loop :for b :across base-factors :for ix :from 0
                                                 :do (multiple-value-bind (titem tremainder)
                                                         (when (>= ix rank-delta)
                                                           (floor tremaining
                                                                  (aref tfactors (- ix rank-delta))))
                                                       (multiple-value-bind (bitem bremainder)
                                                           (floor bremaining b)
                                                         (incf base-index (* b (+ bitem (or titem 0))))
                                                         (setf tremaining (or tremainder it)
                                                               bremaining bremainder))))
                                           (setf valid (array-compare (row-major-aref target it)
                                                                      (row-major-aref base base-index)))))
                               (if valid 1 0))))))
             (lambda (index)
               (if (array-compare target-head (if (not (functionp base-gen))
                                                  base-gen (funcall base-gen index)))
                   1 0)))))))

(defclass vader-where (varray-derived vad-with-io vad-with-dfactors vad-limitable vad-render-mutable)
  nil (:metaclass va-class)
  (:documentation "The coordinates of array elements equal to 1 as from the [⍸ where] function."))

(defmethod etype-of ((varray vader-where))
  (declare (ignore varray))
  t)

(defmethod prototype-of ((varray vader-where))
  "Prototype is an array of zeroes with length equal to the base array's rank."
  (make-array (rank-of (vader-base varray)) :element-type 'bit :initial-element 0))

(defmethod shape-of ((varray vader-where))
  (get-promised (varray-shape varray)
                (progn (if (second (shape-of (vader-base varray)))
                           ;; set array to subrender if it will return coordinate vector objects
                           (setf (vads-nested varray) t)
                           ;; the array's rendered status can be set to true if it contains
                           ;; integers, since they will be IO-adjusted upon generation
                           (setf (vads-rendered varray) t))
                       (generator-of varray)
                       (let* ((base-gen (generator-of (vader-base varray)))
                              (shape (or (shape-of (vader-content varray))
                                         (list (if (and (not (functionp base-gen))
                                                        (integerp base-gen)
                                                        (= 1 base-gen))
                                                   1 0)))))
                         (unless (vads-dfactors varray)
                           (setf (vads-dfactors varray)
                                 (get-dimensional-factors (shape-of (vader-base varray)) t)))
                         shape))))

(defmethod generator-of ((varray vader-where) &optional indexers params)
  (declare (ignore indexers))
  (let* ((base-shape (shape-of (vader-base varray)))
         (base-rank (length base-shape))
         (maximum (when base-shape (reduce #'max base-shape))))
    (if (and maximum (not (vader-content varray)))
        (let* ((derivative-count (when (and (varray-shape varray)
                                            (listp (varray-shape varray)))
                                   (reduce #'* (varray-shape varray))))
               (base-gen (generator-of (vader-base varray)))
               (indices) (match-count 0))
          (loop :for i :below (size-of (vader-base varray))
                :while (or (not derivative-count) (< match-count derivative-count))
                :do (let ((this-item (funcall base-gen i)))
                      (when (and (integerp this-item) (= 1 this-item))
                        (push i indices)
                        (incf match-count))))
          (let ((output (make-array match-count :element-type (list 'integer 0
                                                                    (size-of (vader-base varray))))))
            (loop :for in :in indices :for ix :from (1- match-count) :downto 0
                  :do (setf (aref output ix) (+ in (if (not (vads-rendered varray))
                                                       0 (vads-io varray)))))
            (setf (vader-content varray) output))))
    ;; (print (list :rr (vads-rendered varray) (vader-content varray)))
    (case (getf params :base-format)
      (:encoded)
      (:linear)
      (t (if (zerop base-rank) (make-array 0)
             (if (= 1 base-rank)
                 (lambda (index) (aref (vader-content varray) index))
                 (lambda (index)
                   (make-instance 'vapri-coordinate-vector
                                  :reference varray :index (aref (vader-content varray) index)))))))))

(defclass vader-interval-index (varray-derived vad-with-io vad-with-argument)
  nil (:metaclass va-class)
  (:documentation "Interval indices of value(s) as from the [⍸ interval index] function."))

(defmethod etype-of ((varray vader-interval-index))
  (list 'integer 0 (if (not (shape-of (vader-base varray)))
                       1 (+ (vads-io varray)
                            (reduce #'max (shape-of (vader-base varray)))))))

(defmethod prototype-of ((varray vader-interval-index))
  (declare (ignore varray))
  0)

(defmethod shape-of ((varray vader-interval-index))
  (let ((base-shape (shape-of (vader-base varray))))
    ;; (PRINT (list :bs base-shape (shape-of (vads-argument varray))))
    (when base-shape (butlast base-shape (1- (length (shape-of (vads-argument varray))))))))

(defmethod generator-of ((varray vader-interval-index) &optional indexers params)
  (declare (ignore indexers))
  (let* ((base-gen (generator-of (vader-base varray)))
         (argument (render (vads-argument varray))) ;; TODO: rendering needed?
         (arg-shape (shape-of argument))
         (base-rendered (when (second arg-shape) (render (vader-base varray))))
         (increment (when (second arg-shape)
                      (/ (reduce #'* (shape-of base-rendered))
                         (reduce #'* (shape-of varray)))))
         (arg-span (when (second arg-shape) (/ (size-of argument) increment))))
    (case (getf params :base-format)
      (:encoded)
      (:linear)
      (t (if (second arg-shape)
             (let ((br-type (etype-of base-rendered))
                   (arg-type (etype-of argument)))
               (lambda (index)
                 (let ((count 0)
                       (sub-base (make-array increment
                                             :element-type br-type :displaced-to base-rendered
                                             :displaced-index-offset (* index increment))))
                   (loop :for ix :below arg-span
                         :while (vector-grade (alpha-compare #'<)
                                              (make-array increment
                                                          :displaced-to argument :element-type arg-type
                                                          :displaced-index-offset (* ix increment))
                                              sub-base)
                         :do (incf count))
                   count)))
             (if (functionp base-gen)
                 (lambda (index)
                   (let ((count 0)
                         (value (funcall base-gen index)))
                     (loop :for item :across argument :while (funcall (alpha-compare #'>) value item)
                           :do (incf count))
                     count))
                 (let ((count 0))
                   (loop :for item :across argument :while (funcall (alpha-compare #'>)
                                                                    base-gen item)
                         :do (incf count))
                   count)))))))

;; TODO: is nested needed here? Check render function
(defclass vader-pare (vader-reshape vad-on-axis vad-with-io)
  nil (:metaclass va-class)
  (:documentation "An array with a reduced shape as from the [, catenate] or [⍪ table] functions."))

(defmethod shape-of ((varray vader-pare))
  "The shape of a reshaped array is simply its argument."
  (get-promised
   (varray-shape varray)
   (let ((base-shape (shape-of (vader-base varray)))
         (axis (setf (vads-axis varray)
                     (if (and (vads-axis varray)
                              (not (keywordp (vads-axis varray))))
                         (funcall (lambda (ax)
                                    (if (numberp ax)
                                        (- ax (vads-io varray))
                                        (if (zerop (size-of ax))
                                            ax (loop :for a :across ax
                                                     :collect (- a (vads-io varray))))))
                                  (disclose (render (vads-axis varray))))
                         (vads-axis varray)))))
     (if (not axis) (list (reduce #'* base-shape))
         (if (eq :tabulate axis)
             (list (or (first base-shape) 1)
                   (reduce #'* (rest base-shape)))
             (if (numberp axis)
                 (if (integerp axis)
                     base-shape (let ((output) (added) (ix 0) (real-axis (ceiling axis)))
                                  (dolist (s base-shape)
                                    (when (= ix real-axis) (setf added (push 1 output)))
                                    (push s output)
                                    (incf ix))
                                  (unless added (push 1 output))
                                  (reverse output)))
                 (if (zerop (size-of axis))
                     (append base-shape (list 1))
                     (let ((output) (reducing) (prev-axis)
                           (ix 0) (axis (copy-list axis)))
                       (dolist (s base-shape)
                         (if reducing (if axis (if (/= (first axis) (1+ prev-axis))
                                                   (error "Invalid axis for [, ravel].")
                                                   (when (= ix (first axis))
                                                     (setf (first output)
                                                           (* s (first output))
                                                           prev-axis (first axis)
                                                           axis (rest axis))))
                                          (progn (push s output)
                                                 (setf reducing nil)))
                             (progn (push s output)
                                    (when (and axis (= ix (first axis)))
                                      (setf reducing t
                                            prev-axis (first axis)
                                            axis (rest axis)))))
                         (incf ix))
                       (reverse output)))))))))

(defclass vader-catenate (varray-derived vad-on-axis vad-with-argument vad-with-io)
  ((%laminating :accessor vacat-laminating
                :initform nil
                :initarg :laminating
                :documentation "Whether this catenation laminates arrays."))
  (:metaclass va-class)
  (:documentation "A catenated array as from the [, catenate] function."))

(defmethod etype-of ((varray vader-catenate))
  (let ((base-gen (generator-of (vader-base varray)))
        (base-size (size-of (vader-base varray))))
    (apply #'type-in-common (loop :for i :below base-size
                                  :when (< 0 (size-of (funcall base-gen i)))
                                    :collect (etype-of (funcall base-gen i))))))

(defmethod prototype-of ((varray vader-catenate)) ;; TODO: how does this handle multiple args as for ,/X?
  (let ((base-gen (generator-of (vader-base varray))))
    (when base-gen (prototype-of (funcall base-gen 0)))))

(defmethod shape-of ((varray vader-catenate))
  (get-promised
   (varray-shape varray)
   (let* ((ref-shape) (uneven)
          (base-gen (generator-of (vader-base varray)))
          (base-size (size-of (vader-base varray)))
          (each-shape (loop :for i :below base-size
                            :collect (let* ((a (funcall base-gen i))
                                            (shape (shape-of a)))
                                       ;; (shape-of) must be called before checking whether an
                                       ;; array subrenders since (shape-of) determines whether a
                                       ;; catenated array subrenders
                                       (when (or (nested-p a)
                                                 (nested-base a))
                                         (setf (vads-nested varray) t))
                                       (when (or (varrayp a) (arrayp a))
                                         shape))))
          (max-rank (reduce #'max (mapcar #'length each-shape)))
          (axis (setf (vads-axis varray)
                      (disclose-unitary (render (vads-axis varray)))))
          (axis (if (eq :last axis)
                    (max 0 (1- max-rank))
                    (- axis (vads-io varray))))
          (to-laminate (setf (vacat-laminating varray)
                             (or (typep axis 'ratio)
                                 (and (floatp axis)
                                      (< double-float-epsilon (nth-value 1 (floor axis)))))))
          (axis (setf (vads-axis varray) (ceiling axis))))
     (if to-laminate
         (dolist (shape each-shape)
           (if shape (if ref-shape
                         (if (not (and (= (length shape) (length ref-shape))
                                       (loop :for r :in ref-shape :for a :in shape :always (= a r))))
                             (error "Mismatched array dimensions for laminate operation."))
                                 (setf ref-shape shape))))
         (dolist (shape each-shape)
           (if (and shape (< (length ref-shape) (length shape)))
               (if (or (not ref-shape)
                       (unless uneven (setf uneven (= 1 (- (length shape)
                                                           (length ref-shape))))))
                   (if (destructuring-bind (longer-dims shorter-dims)
                           (funcall (if (>= (length ref-shape) (length shape))
                                        #'identity #'reverse)
                                    (list ref-shape shape))
                         (or (not shorter-dims)
                             (loop :for ld :in longer-dims :for ax :from 0
                                   :always (if (< ax axis)
                                               (= ld (nth ax shorter-dims))
                                               (if (= ax axis)
                                                   t (= ld (nth (1- ax) shorter-dims)))))))
                       (setf ref-shape shape)
                       (error "Mismatched array dimensions."))
                   (error "Catenated arrays must be at most one rank apart."))
               (if (and shape (not (loop :for d :in shape
                                         :for s :in ref-shape :for dx :from 0
                                         :always (or (= d s) (= dx axis)))))
                   (error "Mismatched array dimensions.")))))

     ;; if all elements to be catenated are scalar as for 1,2,
     ;; the output length is the same as the input length
     (if (not ref-shape) (setf ref-shape (list ;; (length (vader-base varray))
                                               base-size)))
     
     (if to-laminate
         (if (zerop max-rank) ;; the ref-shape can be left as is if all elements are scalar
             ref-shape (loop :for dx :below (1+ (length ref-shape))
                             :collect (if (< dx axis) (nth dx ref-shape)
                                          (if (= dx axis) base-size
                                              (nth (1- dx) ref-shape)))))
         (loop :for d :in ref-shape :for dx :from 0
               :collect (if (/= dx axis)
                            d (loop :for shape :in each-shape
                                    :summing (if (or (not shape)
                                                     (/= (length shape)
                                                         (length ref-shape)))
                                                 1 (nth axis shape)))))))))

(defmethod generator-of ((varray vader-catenate) &optional indexers params)
  (declare (ignore indexers))
  (let* ((out-shape (shape-of varray))
         (to-laminate (vacat-laminating varray))
         (axis (disclose-unitary (vads-axis varray)))
         (ofactors (get-dimensional-factors out-shape t))
         (base-gen (generator-of (vader-base varray)))
         (base-size (size-of (vader-base varray)))
         (each-shape (loop :for i :below base-size ; :for a :across (vader-base varray)
                           :collect (let ((a (funcall base-gen i)))
                                      (if (or (varrayp a) (arrayp a))
                                          (shape-of a)))))
         (increments (loop :for shape :in each-shape
                           :summing (if (or (not shape)
                                            (< (length shape) (length out-shape)))
                                        1 (nth (max 0 axis) shape))
                             :into total :collect total))
         (generators (make-array base-size
                                 :initial-contents (loop :for i :below base-size 
                                                         :collect (generator-of (funcall base-gen i)))))
         (ifactors (make-array base-size
                               :initial-contents (loop :for i :below base-size 
                                                       :collect (let ((a (funcall base-gen i)))
                                                                  (if (or (arrayp a) (varrayp a))
                                                                      (reverse (get-dimensional-factors
                                                                                (shape-of a)))))))))
    
    (case (getf params :base-format)
      (:encoded)
      (:linear)
      (t (lambda (orig-index)
           (let ((remaining orig-index) (sum 0) (sub-indices) (array-index 0)
                 (axis-offset (abs (- axis (1- (length (shape-of varray))))))
                 (row-major-index) (source-array))
             ;; (print (list :oo ofactors orig-index axis-offset))
             (loop :for ofactor :across ofactors :for fx :from 0
                   :do (multiple-value-bind (index remainder) (floor remaining ofactor)
                         (setf remaining remainder)
                         (push (if (/= fx axis)
                                   index (loop :for i :in increments :for ix :from 0
                                               :while (>= index i)
                                               :do (progn (setf sum i) (incf array-index))
                                               :finally (return (- index sum))))
                               sub-indices)))
             (setf source-array (funcall base-gen array-index)
                   row-major-index
                   (when (or (arrayp source-array) (varrayp source-array))
                     (if to-laminate
                         (loop :for si :in sub-indices :for ix :from 0
                               :summing (* si (or (nth (max 0 (if (< ix axis-offset)
                                                                  ix (1- ix)))
                                                       (aref ifactors array-index))
                                                  1))
                                 :into rmi :finally (return rmi))
                         (loop :for si :in (loop :for si :in sub-indices :for sx :from 0
                                                 :when (or (= (length out-shape)
                                                              (length (shape-of source-array)))
                                                           (/= sx axis-offset))
                                                   :collect si)
                               :for df :in (aref ifactors array-index)
                               :summing (* si df) :into rmi :finally (return rmi)))))
             (if (functionp (aref generators array-index))
                 (let ((indexed (funcall (aref generators array-index) row-major-index)))
                   (if (not (nested-p indexed))
                       indexed (render indexed)))
                 (aref generators array-index))))))))

(defclass vader-mix (varray-derived vad-on-axis vad-with-io)
  ((%shape-indices   :accessor vamix-shape-indices
                     :initform nil
                     :initarg :shape-indices
                     :documentation "Indices of shape dimensions.")
   (%cached-elements :accessor vamix-cached-elements
                     :initform nil
                     :initarg :cached-elements
                     :documentation "Cached mixed array elements."))
  (:metaclass va-class)
  (:documentation "A mixed array as from the [↑ mix] function."))

(defmethod etype-of ((varray vader-mix))
  ;; (let ((base-gen (generator-of (vader-base varray))))
    ;; (or (apply #'type-in-common (loop :for aix :below (size-of (vader-base varray))
    ;;                                   :when (and (functionp base-gen)
    ;;                                              (< 0 (size-of (funcall base-gen aix))))
    ;;                                     :collect (etype-of (funcall base-gen aix))
    ;;                                   :when (not (functionp base-gen))
    ;;                                     :collect (assign-element-type base-gen)))
    ;;     t)
    ;; above is very slow
    t
    )

(defmethod prototype-of ((varray vader-mix))
  (let ((base-gen (generator-of (vader-base varray))))
    (prototype-of (or (vamix-cached-elements varray)
                      (if (not (functionp base-gen))
                          base-gen (funcall base-gen 0))))))

(defmethod shape-of ((varray vader-mix))
  (get-promised
   (varray-shape varray)
   (let* ((axis (vads-axis varray))
          (base (vader-base varray))
          (base-shape (shape-of base))
          (base-gen (generator-of base))
          (max-rank 0) (each-shape))
     ;; (print (list :ba base base-gen base-shape (render base) (shape-of base)))
     (cond ((and (not (functionp base-gen))
                 (not (arrayp base-gen))
                 (not (varrayp base-gen)))
            base-shape) ;; handle the ↑⍬ case
           ((not base-shape)
            (setf (vamix-cached-elements varray)
                  (if (not (functionp base-gen))
                      base-gen (funcall base-gen 0)))
            (shape-of (vamix-cached-elements varray)))
           (t (loop :for ix :below (reduce #'* base-shape)
                    :do (let ((member (if (not (functionp base-gen))
                                          base-gen (funcall base-gen ix))))
                          (setf max-rank (max max-rank (length (shape-of member))))
                          (push (shape-of member) each-shape)))
              (let ((out-shape) (shape-indices) (dx 0)
                    (max-shape (make-array max-rank :element-type 'fixnum :initial-element 0)))
                (dolist (shape each-shape)
                  (setf dx 0)
                  (dolist (d shape)
                    (setf (aref max-shape dx) (max d (aref max-shape dx)))
                    (incf dx)))
                
                (setf axis (setf (vads-axis varray)
                                 (if (eq :last axis) (length base-shape)
                                     (ceiling (- axis (vads-io varray)))))
                      dx 0)
                ;; push the outer shape elements to the complete shape
                (dolist (odim base-shape)
                  (when (= dx axis)
                    (loop :for ms :across max-shape :for mx :from 0
                          :do (push ms out-shape)
                              (push (+ mx (length base-shape)) shape-indices)))
                  (push odim out-shape)
                  (push dx shape-indices)
                  (incf dx))
                
                (when (= axis (length base-shape))
                  ;; push the inner shape elements if for the last axis
                  (loop :for ms :across max-shape :for mx :from 0
                        :do (push ms out-shape)
                            (push (+ mx (length base-shape)) shape-indices)))

                (setf (vamix-shape-indices varray) (reverse shape-indices))
                (reverse out-shape)))))))

(defmethod generator-of ((varray vader-mix) &optional indexers params)
  (declare (ignore indexers))
  (let* ((oshape (shape-of varray))
         (ofactors (get-dimensional-factors oshape t))
         (oindexer (generator-of (vader-base varray)))
         (dim-indices (vamix-shape-indices varray))
         (orank (length (shape-of (vader-base varray))))
         (outer-shape (loop :for i :in dim-indices :for s :in oshape
                            :when (> orank i) :collect s))
         (inner-shape (loop :for i :in dim-indices :for s :in oshape
                            :when (<= orank i) :collect s))
         (inner-rank (length inner-shape))
         (iofactors (get-dimensional-factors outer-shape t)))
    ;; TODO: add logic to simply return the argument if it's an array containing no nested arrays
    ;; (print (list :os oshape oindexer (render oindexer)))
    ;; (if (not (functionp oindexer))
    ;;     (setf oindexer (generator-of oindexer)))
    (case (getf params :base-format)
      (:encoded)
      (:linear)
      (t (if (not oshape) ;; if the argument is a scalar
             (if (not (functionp oindexer)) ;; a scalar value like 5
                 (if (or (not (varrayp oindexer))
                         (typep (vader-base varray) 'vader-enclose))
                     oindexer (make-instance 'vader-pick :base (vader-base varray)))
                 ;; TODO: change indexer-of for rank 0 arrays to obviate this
                 (let* ((indexed (funcall oindexer 0))
                        (iindexer (generator-of indexed))
                        (sub-index (if (not (functionp iindexer))
                                       iindexer (funcall (generator-of indexed) 0))))
                   (if (and (typep (vader-base varray) 'varray)
                            (not (shape-of (vader-base varray))))
                       (generator-of (vader-base varray))
                       sub-index)))
             (if (not (shape-of (vader-base varray)))
                 ;; pass through the indexer of enclosed arrays as for ↑⊂2 4
                 (generator-of (vamix-cached-elements varray))
                 (if (vamix-cached-elements varray)
                     (lambda (index) (row-major-aref (vamix-cached-elements varray) index))
                     (let* ((base (vader-base varray))
                            (iarray (unless (shape-of varray) (render base)))
                            (ishape (when iarray (copy-list (shape-of iarray))))
                            (iifactors (when iarray (get-dimensional-factors ishape)))
                            (generators (unless iarray
                                          (coerce (loop :for i :below (size-of base)
                                                        :collect (generator-of (funcall oindexer i)))
                                                  'vector)))
                            (prototype (prototype-of varray)))
                       (lambda (index)
                         (let ((remaining index) (outer-indices) (inner-indices))
                           (loop :for ofactor :across ofactors :for di :in dim-indices :for fx :from 0
                                 :do (multiple-value-bind (this-index remainder) (floor remaining ofactor)
                                       (setf remaining remainder)
                                       (if (> orank di) (push this-index outer-indices)
                                           (push this-index inner-indices))))
                           (let* ((inner-indices (reverse inner-indices))
                                  (oindex (unless iarray
                                            (loop :for i :in (reverse outer-indices)
                                                  :for f :across iofactors :summing (* i f))))
                                  (iarray (or iarray (funcall oindexer oindex)))
                                  (ishape (or ishape (copy-list (shape-of iarray))))
                                  (iifactors (or iifactors (get-dimensional-factors ishape)))
                                  (this-gen (aref generators oindex))
                                  (irank (length ishape))
                                  (doffset (- inner-rank irank))
                                  (iindex 0))
                             (if (shape-of varray)
                                 (progn (loop :for i :in inner-indices :for ix :from 0 :while iindex
                                              :do (if (< ix doffset) (unless (zerop i)
                                                                       (setf iindex nil))
                                                      (if (< i (first ishape))
                                                          (progn (incf iindex (* i (first iifactors)))
                                                                 (setf ishape (rest ishape)
                                                                       iifactors (rest iifactors)))
                                                          (setf iindex nil))))
                                        (if iindex (if (not (functionp this-gen))
                                                       this-gen (funcall this-gen iindex))
                                            prototype))
                                 (when (zerop (reduce #'+ inner-indices)) this-gen)))))))))))))

(defclass vader-split (vad-nested varray-derived vad-on-axis vad-with-io vad-maybe-shapeless)
  nil (:metaclass va-class)
  (:documentation "A split array as from the [↓ split] function."))

(defmethod etype-of ((varray vader-split))
  "The [↓ split] function returns a nested array unless its argument is scalar."
  (let ((base (vader-base varray)))
    (if (or (arrayp base) (varrayp base))
        t (assign-element-type base))))

(defmethod shape-of ((varray vader-split))
  (get-promised
   (varray-shape varray)
   (if (vads-shapeset varray)
       (varray-shape varray)
       (let* ((base-shape (shape-of (vader-base varray)))
              (axis (setf (vads-axis varray)
                          (if (eq :last (vads-axis varray))
                              (max 0 (1- (length base-shape)))
                              (- (disclose (render (vads-axis varray)))
                                 (vads-io varray))))))
         (setf (vads-shapeset varray) t)
         (loop :for d :in base-shape :for ix :from 0 :when (not (= axis ix)) :collect d)))))

(defclass vader-subarray-split (vader-subarray)
  ((%core-indexer :accessor vasbs-core-indexer
                  :initform nil
                  :initarg :core-indexer
                  :documentation "Core indexer for split subarray."))
  (:metaclass va-class)
  (:documentation "An element of a split array as from the [↓ split] function."))

(defmethod prototype-of ((varray vader-subarray-split))
  (get-promised (varray-prototype varray)
                (let ((first-item (funcall (generator-of varray) 0)))
                  (if (varrayp first-item) (prototype-of first-item)
                      (apl-array-prototype first-item)))))

(defmethod generator-of ((varray vader-subarray-split) &optional indexers params)
  (declare (ignore indexers))
  (let ((base-gen (generator-of (vader-base varray))))
    (case (getf params :base-format)
      (:encoded)
      (:linear)
      (t (lambda (index)
           (funcall base-gen (funcall (vasbs-core-indexer varray) (vasv-index varray) index)))))))

(defmethod generator-of ((varray vader-split) &optional indexers params)
  (declare (ignore indexers))
  (let* ((output-shape (shape-of varray))
         (axis (vads-axis varray))
         (base-shape (shape-of (vader-base varray)))
         (sv-length (nth axis base-shape))
         (base-gen (generator-of (vader-base varray)))
         (base-factors (get-dimensional-factors base-shape t))
         (core-indexer (indexer-split axis (length output-shape)
                                      base-factors (get-dimensional-factors output-shape t))))
    
    (if (functionp base-gen)
        (case (getf params :base-format)
          (:encoded)
          (:linear)
          (t (lambda (index)
               (make-instance 'vader-subarray-split
                              :base (vader-base varray) :shape (when sv-length (list sv-length))
                              :index index :core-indexer core-indexer
                              :prototype (unless output-shape
                                           (prototype-of (vader-base varray)))))))
        (case (getf params :base-format)
          (:encoded)
          (:linear)
          (t (lambda (index) (declare (ignore index)) base-gen))))))

(defclass vader-section (varray-derived vad-on-axis vad-with-argument vad-with-io vad-invertable vad-reindexing)
  ((%span :accessor vasec-span
          :initform nil
          :initarg :span
          :documentation "The start and end points of the section within the base array.")
   (%pad  :accessor vasec-pad
          :initform nil
          :initarg :pad
          :documentation "The number of prototype elements padding the output in each dimension."))
  (:metaclass va-class)
  (:documentation "A sectioned array as from the [↑ take] or [↓ drop] functions."))

(defmethod prototype-of ((varray vader-section))
  (get-promised (varray-prototype varray)
                (progn (generator-of varray)
                       (varray-prototype varray))))

(defmethod effector-of ((varray vader-section) &optional params)
  (declare (ignore params))
  (let* ((format (getf params :format))
         (metadata (getf (metadata-of varray) :shape))
         (encoding (getf (rest (getf metadata :gen-meta)) :index-width))
         (coordinate-type (getf (rest (getf metadata :gen-meta)) :index-type))
         ;; (etag (case encoding (8 :byte) (16 :word) (32 :dword) (64 :qword)))
         ;; (ctag (case coordinate-type (8 :byte) (16 :word) (32 :dword) (64 :qword)))
         )
    (case format
      (:lisp
       ;; (lambda (ein-sym)
       ;;   )
       nil
       )
      ;; (:x86-asm
      ;;  (lambda (symbols)
      ;;    (let ((sum 0))
      ;;      (loop :for dx :below (rank-of varray)
      ;;            :do (incf sum (ash (+ (aref (vasec-span varray) dx)
      ;;                                  (aref (vasec-pad varray) dx))
      ;;                               (* 8 (- (rank-of varray) (1+ dx))))))
      ;;      (destructuring-bind (ra rc rd rb) symbols
      ;;        (declare (ignorable ra rc rd rb))
      ;;        `((inst add ,etag ,rd ,sum))))))

      )))

(defmethod shape-of ((varray vader-section))
  "The shape of a sectioned array is the parameters (if not inverse, as for [↑ take]) or the difference between the parameters and the shape of the original array (if inverse, as for [↓ drop])."
  (get-promised
   (varray-shape varray)
   (let* ((base (vader-base varray))
          (arg-shape (shape-of (vads-argument varray)))
          (arg-indexer (generator-of (setf (vads-argument varray)
                                           (render (vads-argument varray)))))
          (is-inverse (vads-inverse varray))
          (iorigin (vads-io varray))
          (axis (vads-axis varray))
          (ax-generator (generator-of (vads-axis varray)))
          (base-shape (shape-of base))
          (limited-shape))
     (when (typep base 'vader-section) ;; 4 3↑1 1↓4 5⍴⍳20  x←⍳9 ⋄ (2↑4↓x)←99 ⋄ x
       ;; merge successive section objects into a single object applying to the base of the
       ;; first section object, so only one array transform happens for cases like 4 3↑1 1↓4 5⍴⍳20
       (let* ((argument (vads-argument varray))
              (base-span (vasec-span base))
              (base-pad (vasec-pad base))
              (base-rank (* 1/2 (length base-span)))
              (sub-base (vader-base base))
              (new-span) (new-pad))
         (flet ((update-take (a position pre-position)
                  ;; (print (list :aa a negative))
                  (symbol-macrolet ((before-span (aref new-span pre-position))
                                    ( after-span (aref new-span position))
                                    ( before-pad (aref new-pad  pre-position))
                                    (  after-pad (aref new-pad  position)))
                    (let ((remaining a) (negative (minusp a)))
                      (if negative
                          (if (< 0 (+ remaining after-pad))
                              (setf  after-pad  (abs remaining)
                                     after-span 0
                                    before-span 0
                                    before-pad  0)
                              (let ((remaining (+ remaining after-pad)))
                                (if (< 0 (+ remaining (- after-span before-span)))
                                    (setf before-span (+ after-span remaining)
                                          before-pad  0)
                                    (setf before-pad (abs (+ remaining
                                                             (- after-span before-span)))))))
                          (if (< remaining before-pad)
                              (setf before-pad  remaining
                                    before-span 0
                                     after-span 0
                                     after-pad  0)
                              (let ((remaining (- remaining before-pad)))
                                (if (< remaining (- after-span before-span before-pad))
                                    (setf after-span (+ remaining before-span)
                                          after-pad  0)
                                    (setf after-pad (- remaining (- after-span before-span))))))))))
                (update-drop (a position pre-position)
                  (let ((negative (minusp a)))
                    (setf (aref new-span (if negative position pre-position))
                          (if negative (+ (aref new-span position)
                                          (min 0 (+ a (aref new-pad position))))
                              (+ a (aref new-span pre-position)))
                          (aref new-pad (if negative position pre-position))
                          (if negative (max 0 (+ (aref new-pad position) a))
                              (max 0 (- (aref new-pad pre-position) a)))))))
           
           (setf new-span (make-array (length base-span) :element-type 'fixnum :initial-element 0)
                 new-pad (make-array (length base-pad) :element-type 'fixnum :initial-element 0))
           
           (loop :for bs :across base-span :for ix :from 0 :do (setf (aref new-span ix) bs))
           (loop :for bp :across base-pad :for ix :from 0 :do (setf (aref new-pad ix) bp))

           (if (vectorp axis)
               (if (functionp arg-indexer)
                   (loop :for x :across axis :for ix :below (first (shape-of argument))
                         :do (let ((a (funcall arg-indexer ix))
                                   (ax (- x iorigin)))
                               (if is-inverse (update-drop a (+ ax base-rank) ax)
                                   (update-take a (+ ax base-rank) ax)))))
               (if (functionp arg-indexer)
                   (loop :for ix :below (first (shape-of argument))
                         :do (let ((a (funcall arg-indexer ix))
                                   (ax (if (eq :last axis)
                                           ix (funcall ax-generator ix))))
                               (if is-inverse (update-drop a (+ ax base-rank) ax)
                                   (update-take a (+ ax base-rank) ax))))
                   (let ((ax (if (eq :last axis) 0 (- axis iorigin))))
                     (if is-inverse (update-drop argument (+ ax base-rank) ax)
                         (update-take argument (+ ax base-rank) ax)))))
           (setf (vasec-span varray) new-span
                 (vasec-pad varray) new-pad
                 (vader-base varray) (if new-span sub-base base)))))
     
     (unless (vasec-span varray)
       ;; this will usually apply to a section object without another section as its base
       (let* ((base-rank (rank-of base))
              ;; length of span/pad vectors
              (sp-length (* 2 (max (or base-rank 1) (or (first arg-shape) 1)))))
         (if (zerop base-rank)
             (let ((sp-offset (/ sp-length 2)))
               (setf (vasec-span varray)
                     (make-array sp-length :element-type 'fixnum :initial-element 0)
                     (vasec-pad varray)
                     (make-array sp-length :element-type 'fixnum :initial-element 0))
               (when (not is-inverse)
                 (loop :for s :below sp-offset
                       :do (let ((a (if (not (functionp arg-indexer))
                                        arg-indexer (funcall arg-indexer s))))
                             (setf (aref (vasec-pad varray) (if (> 0 a) s (+ s sp-offset)))
                                   (max 0 (1- (abs a)))
                                   (aref (vasec-span varray) (+ s sp-offset))
                                   (signum (abs a)))))))
             (if (and (not is-inverse) (eq :last axis)
                      (typep base 'vad-limitable)
                      (= 1 (size-of (vads-argument varray))))
                 ;; a scalar left argument can be used to limit the computation of
                 ;; a base virtual array, as for the case of 5↑'This is a test'~' ';
                 ;; this case is computed separately because other cases require that the shape
                 ;; of the base array be obtained, which precludes setting it as is done here
                 (let ((shape (list (abs arg-indexer))))
                   (setf (varray-shape base) shape)
                   (generator-of base)
                   (setf (vasec-span varray)
                         (make-array sp-length :element-type 'fixnum :initial-element 0)
                         (vasec-pad varray)
                         (make-array sp-length :element-type 'fixnum :initial-element 0))
                   (if base-shape
                       (loop :for s :in base-shape :for i :from base-rank :to (1- (* 2 base-rank))
                             :do (setf (aref (vasec-span varray) i) s))
                       (unless is-inverse (setf (aref (vasec-span varray) 1) 1)))
                   (setf limited-shape shape))
                 
                 (let* ((span-offset (max 1 base-rank))
                        (base-shape (copy-list base-shape))
                        ;; the shape of the base is only needed for [↓ drop]
                        (pre-shape (coerce (loop :for b :below (max (length base-shape)
                                                                    (if (not arg-shape) 1 (first arg-shape)))
                                                 :for i :from 0
                                                 :collect (or (nth b base-shape) (if (zerop i) 0 0)))
                                           'vector)))
                   (setf (vasec-span varray)
                         (make-array sp-length :element-type 'fixnum :initial-element 0)
                         (vasec-pad varray)
                         (make-array sp-length :element-type 'fixnum :initial-element 0))
                   
                   (if base-shape
                       (loop :for s :in base-shape :for i :from base-rank :to (1- (* 2 base-rank))
                             :do (setf (aref (vasec-span varray) i) s))
                       (unless is-inverse (setf (aref (vasec-span varray) 1) 1)))

                   ;; populate the span and padding vectors according to the arguments
                   ;; and the dimensions of the input array
                   (flet ((process-element (arg ax orig element)
                            (if is-inverse
                                (progn (when (< 0 base-rank)
                                         (setf (aref (vasec-span varray)
                                                     (+ ax (* span-offset (if (< 0 arg) 0 1))))
                                               (if (> arg 0) element (- orig element))))
                                       (max 0 (- orig element)))
                                (progn (if (<= element orig)
                                           (setf (aref (vasec-span varray)
                                                       (+ ax (* span-offset (if (> 0 arg) 0 1))))
                                                 (if (<= 0 arg)
                                                     element (+ arg orig (if (zerop base-rank) 1 0))))
                                           (setf (aref (vasec-span varray)
                                                       (+ ax (* span-offset (if (not base-shape)
                                                                                1 (if (> 0 arg) 0 1)))))
                                                 ;; the span is nominally 0 1 in the case of any take
                                                 ;; of a scalar, as with ¯5↑1 or 2↑2
                                                 (if (not base-shape) 1 (if (> 0 arg) 0 orig))
                                                 (aref (vasec-pad varray)
                                                       (+ ax (* span-offset (if (> 0 arg) 0 1))))
                                                 ;; subtract 1 from the pad for takes of scalars
                                                 (- element orig (if base-shape 0 1))))
                                       element))))
                     (if (vectorp axis)
                         (loop :for x :across axis :for ix :from 0
                               :do (let ((arg (funcall arg-indexer ix)))
                                     (process-element arg (- x iorigin) (aref pre-shape (- x iorigin))
                                                      (abs arg))))
                         (if (eq :last axis)
                             (if (functionp arg-indexer)
                                 (loop :for a :below (first arg-shape) :for ix :from 0
                                       :do (let ((arg (funcall arg-indexer a)))
                                             ;; (print (list :ar arg pre-shape))
                                             (process-element arg ix (aref pre-shape ix) (abs arg))))
                                 (process-element arg-indexer 0 (aref pre-shape 0) (abs arg-indexer)))
                             (process-element arg-indexer (- axis iorigin)
                                              (aref pre-shape (- axis iorigin))
                                              (abs arg-indexer))))))))))
     
     ;; (print (list :ts (vasec-span varray) (vasec-pad varray)
     ;;              (vads-argument varray) (vads-axis varray) (render base)
     ;;              is-inverse))
     (or limited-shape
         (let ((orank (* 1/2 (length (vasec-span varray)))))
           (loop :for ix :below orank
                 :collect (+ (max 0 (- (aref (vasec-span varray) (+ ix orank))
                                       (aref (vasec-span varray) ix)))
                             (aref (vasec-pad varray) (+ ix orank))
                             (aref (vasec-pad varray) ix))))))))

(defmethod indexer-of ((varray vader-section) &optional params)
  "Indexer for a sectioned array."
  (get-promised
   (varray-generator varray)
   (let* ((assigning (getf params :for-selective-assign))
          (base-gen (generator-of (vader-base varray)))
          (size (size-of varray))
          (base-size (size-of (vader-base varray)))
          (layers-below (when assigning
                          (typecase (vader-base varray) (vader-section t)
                                    (vader-pick (setf (vapick-assign (vader-base varray))
                                                      (getf params :assigning))
                                     (funcall (getf params :toggle-toplevel-nested))
                                     :pick)))))
          
     (let* ((indexer (indexer-section (shape-of (vader-base varray))
                                      (vasec-span varray) (vasec-pad varray)
                                      assigning nil nil t)))
       (unless assigning
         (setf (varray-prototype varray)
               (if (or (zerop size) (zerop base-size))
                   (prototype-of (vader-base varray))
                   (let ((indexed (funcall #'identity
                                           (if (functionp indexer)
                                               (funcall indexer 0) 0))))
                     (if indexed (if (functionp base-gen)
                                     (aplesque:make-empty-array
                                      (render (funcall base-gen indexed)))
                                     (aplesque:make-empty-array (render base-gen)))
                         (prototype-of (vader-base varray)))))))
       (if assigning (lambda (index)
                       (declare (type integer index))
                       (let ((indexed (funcall indexer index)))
                         (if (not layers-below) indexed
                             ;; handle the next layer, as for x←⍳9 ⋄ (2↑4↓x)←99 ⋄ x
                             (let ((inext (funcall base-gen index)))
                               (when inext (if (eq :pick layers-below)
                                               inext (funcall indexer inext)))))))
           (if (functionp base-gen) ;; TODO: why is the disclose needed?
               indexer (lambda (index)
                         (when (funcall indexer index)
                           (disclose base-gen)))))))))

(defmethod generator-of ((varray vader-section) &optional indexers params)
  (if (getf params :for-selective-assign)
      (let ((indexer (indexer-section (shape-of (vader-base varray))
                                      (vasec-span varray) (vasec-pad varray)
                                      :assign nil nil))
            (layers-below (typecase (vader-base varray)
                            (vader-section t)
                            (vader-pick (setf (vapick-assign (vader-base varray))
                                              (getf params :assigning))
                             (funcall (getf params :toggle-toplevel-nested))
                             :pick))))
        (lambda (index)
          (declare (type integer index))
          (let ((indexed (funcall indexer index)))
            (if (not layers-below) indexed
                ;; handle the next layer, as for x←⍳9 ⋄ (2↑4↓x)←99 ⋄ x
                (let ((inext (funcall indexer index)))
                  (when inext (if (eq :pick layers-below)
                                  inext (funcall indexer inext))))))))
      (case (getf params :base-format)
        (:encoded (when (loop :for item :across (vasec-pad varray) :always (zerop item))
                    ;; TODO: add case for padding, checking invalid indices
                    (let* ((enco-type (getf (getf params :gen-meta) :index-width))
                           (coord-type (getf (getf params :gen-meta) :index-type))
                           (indexer (indexer-section
                                     (shape-of (vader-base varray))
                                     (vasec-span varray) (vasec-pad varray)
                                     nil enco-type coord-type))
                           (these-indexers))
                      (when indexer
                        (when (eq :linear (getf params :format))
                          (push (encode-rmi (get-dimensional-factors (shape-of varray) t)
                                            enco-type coord-type)
                                these-indexers)
                          (setf (getf params :format) :encoded))
                        (unless (eq t indexer)
                          (setf these-indexers (append indexer these-indexers)))
                        (generator-of (vader-base varray)
                                      (append these-indexers indexers) params)))))
        (:linear (when (loop :for item :across (vasec-pad varray) :always (zerop item))
                   (let ((indexer (indexer-section
                                   (shape-of (vader-base varray))
                                   (vasec-span varray) (vasec-pad varray)
                                   nil (getf (getf params :gen-meta) :index-width)
                                   (getf (getf params :gen-meta) :index-type))))
                     (when indexer
                       (generator-of (vader-base varray) (cons indexer indexers)
                                     params)))))
        (t (let ((indexer (indexer-of varray params)))
             (if (loop :for item :across (vasec-pad varray) :always (zerop item))
                 (if (zerop (size-of varray))
                     (prototype-of varray)
                     (generator-of (vader-base varray)
                                   (if (or (not indexer) (eq t indexer))
                                       indexers (cons indexer indexers))))
                 (let* ((composite-indexer (or (join-indexers indexers params) #'identity))
                        (arg (vads-argument varray))
                        (scalar-index (unless (shape-of (vader-base varray)) 0))
                        (prototype (prototype-of varray))
                        (base-gen (generator-of (vader-base varray))))
                   (when scalar-index ;; find the item's scalar index for efficient matching
                     (if (loop :for i :from (rank-of varray) :to (1- (* 2 (rank-of varray)))
                               :always (= 1 (aref (vasec-span varray) i)))
                         (loop :for p :across (vasec-pad varray)
                               :for f :in (get-dimensional-factors (shape-of varray))
                               :do (incf scalar-index (* f p)))
                         (setf scalar-index -1)))
                   ;; IPV-TODO: figure out how to allow n-rank mixed positive-negative takes for scalars
                   (if scalar-index
                       (if (= -1 scalar-index)
                           prototype
                           (let ((scalar-item (funcall (if (functionp indexer) indexer #'identity)
                                                       (funcall composite-indexer scalar-index))))
                             (lambda (index) (if (= index scalar-index)
                                                 scalar-item prototype))))
                       (lambda (index)
                         (let ((indexed (funcall (if (functionp indexer) indexer #'identity)
                                                 (funcall composite-indexer index))))
                           ;; (print (list :ind indexed arg))
                           (if indexed (if (numberp indexed) ;; IPV-TODO: remove after refactor ?
                                           (if (not (functionp base-gen))
                                               indexed (funcall base-gen indexed))
                                           (if (zerop index) indexed prototype))
                               prototype)))))))))))

(defclass vader-enclose (vad-nested varray-derived vad-on-axis vad-with-io
                         vad-with-argument vad-maybe-shapeless)
  ((%inner-shape :accessor vaenc-inner-shape
                 :initform nil
                 :initarg :inner-shape
                 :documentation "Inner shape value for re-enclosed array."))
  (:metaclass va-class)
  (:documentation "An enclosed or re-enclosed array as from the [⊂ enclose] function."))

(defmethod etype-of ((varray vader-enclose))
  (let ((base (vader-base varray)))
    (if (or (arrayp base) (varrayp base)
            (vads-argument varray))
        t (assign-element-type base))))

(defmethod prototype-of ((varray vader-enclose))
  (if (zerop (size-of varray))
      (make-instance 'vader-expand :argument 0 :axis :last :base (vader-base varray)
                                   :index-origin (vads-io varray))
      (call-next-method)))

(defmethod shape-of ((varray vader-enclose))
  (get-promised (varray-shape varray)
                (if (vads-shapeset varray)
                    (varray-shape varray)
                    (let* ((axis (setf (vads-axis varray)
                                       (when (vads-axis varray)
                                         (apply-scalar #'- (disclose (render (vads-axis varray)))
                                                       (vads-io varray)))))
                           (positions (when (vads-argument varray)
                                        (enclose-atom (render (vads-argument varray)))))
                           (base-shape (shape-of (vader-base varray)))
                           (axis-size (if (and axis positions)
                                          (nth axis base-shape)
                                          (or (first (last base-shape)) 1)))
                           (input-offset 0) (intervals (list 0)))
                      (when positions
                        (dotimes (i (if (is-unitary positions)
                                        (or (first (last base-shape)) 1)
                                        (length positions)))
                          (let ((p (if (is-unitary positions)
                                       (disclose-unitary positions)
                                       (aref positions i))))
                            (if (zerop p) (progn (incf (first intervals))
                                                 (incf input-offset))
                                (progn (setq intervals
                                             (append (loop :for i :below p :collect 0) intervals))
                                       (when (and axis-size (> axis-size input-offset))
                                         (incf input-offset)
                                         (when (first intervals) (incf (first intervals)))))))))
                      
                      (when (and axis-size (second intervals))
                        (if (>= axis-size input-offset)
                            (incf (first intervals) (- axis-size input-offset))
                            (error "Size of partitions exceeds size of input array on axis ~w." axis)))
                      
                      (setf (vads-shapeset varray)
                            (if (not positions)
                                t (coerce (reverse intervals) 'vector)))

                      (if positions (list (1- (length intervals)))
                          (when axis (let ((outer-shape) (inner-shape) (dx 0)
                                           (base-shape (shape-of (vader-base varray))))
                                       (dolist (d base-shape)
                                         (if (if (integerp axis) (not (= dx axis))
                                                 (loop :for a :across axis :never (= a dx)))
                                             (push d outer-shape)
                                             (push d inner-shape))
                                         (incf dx))
                                       (setf (vaenc-inner-shape varray) (reverse inner-shape))
                                       (reverse outer-shape))))))))

(defmethod generator-of ((varray vader-enclose) &optional indexers params)
  (declare (ignore indexers))
  (let* ((base-shape (shape-of (vader-base varray)))
         (output-shape (shape-of varray))
         (output-size (size-of varray))
         (inner-shape (vaenc-inner-shape varray))
         (axis (or (vads-axis varray)
                   (max 0 (1- (length base-shape)))))
         (ofactors (get-dimensional-factors base-shape))
         (base-gen (generator-of (vader-base varray)))
         (outer-factors (get-dimensional-factors output-shape t))
         (inner-factors (get-dimensional-factors inner-shape t))
         (intervals (vads-shapeset varray))
         (offset-indexer
           (lambda (o)
             (lambda (i)
               (let ((orest o) (irest i) (index 0) (inner-dx 0) (outer-dx 0) (fx 0))
                 (dolist (f ofactors)
                   (let ((in-outer (if (numberp axis) (/= fx axis)
                                       (loop :for a :across axis :never (= fx a)))))
                     (multiple-value-bind (factor remaining)
                         (if in-outer (floor orest (aref outer-factors outer-dx))
                             (floor irest (aref inner-factors inner-dx)))
                       (if in-outer (progn (incf outer-dx) (setf orest remaining))
                           (progn (incf inner-dx) (setf irest remaining)))
                       (incf index (* f factor))
                       (incf fx))))
                 (funcall base-gen index)))))
         (each-offset (when (vectorp intervals)
                        (coerce (loop :for i :across intervals :summing i :into s :collect s)
                                'vector)))
         (section-size (when (vectorp intervals)
                         (reduce #'* (loop :for d :in base-shape :for dx :from 0
                                           :when (> dx axis) :collect d))))
         (iseg (when section-size (if (< 1 section-size)
                                      section-size (or (nth axis base-shape) 1))))
         (last-indim (first (last base-shape)))
         (partition-indexer
           (lambda (o shape)
             (let ((input-offset (aref each-offset o))
                   (last-idim (or (first (last shape)) 1))
                   (this-size (reduce #'* shape)))
               (lambda (i)
                 (unless (zerop this-size)
                   (let ((oseg (floor i iseg)) (ivix (mod i iseg)))
                     (if (functionp base-gen)
                         (funcall base-gen (if (= 1 section-size)
                                               (+ input-offset (mod i last-idim)
                                                  (* last-indim (floor i last-idim)))
                                               (+ ivix (* iseg (+ input-offset oseg)))))
                         base-gen))))))))
    
    (case (getf params :base-format)
      (:encoded)
      (:linear)
      (t (if (vectorp intervals)
             (if (< 0 output-size)
                 (lambda (index)
                   (let* ((this-shape (loop :for dim :in base-shape :for dx :from 0
                                            :collect (if (/= dx axis)
                                                         dim (aref intervals (1+ index)))))
                          (sub-indexer (funcall partition-indexer index this-shape))
                          (first-item (funcall sub-indexer 0))
                          (prototype (if (varrayp first-item)
                                         (prototype-of first-item)
                                         (apl-array-prototype first-item))))
                     (make-instance 'vader-subarray :prototype prototype :generator sub-indexer
                                                    :shape (or this-shape '(1)) :base (vader-base varray))))
                 (make-instance 'vader-subarray :prototype (prototype-of (vader-base varray))
                                                :shape output-shape :base (vader-base varray)))
             (if inner-shape (if (functionp base-gen)
                                 (lambda (index)
                                   (let* ((sub-indexer (funcall offset-indexer index))
                                          (first-item (funcall sub-indexer 0))
                                          (prototype (if (not output-shape)
                                                         (aplesque::make-empty-array
                                                          (vader-base varray))
                                                         (if (varrayp first-item)
                                                             (prototype-of first-item)
                                                             (apl-array-prototype first-item)))))
                                     (make-instance 'vader-subarray
                                                    :prototype prototype :base (vader-base varray)
                                                    :shape inner-shape :generator sub-indexer)))
                                 base-gen)
                 (if (vads-axis varray)
                     (if (not (functionp base-gen))
                         base-gen (lambda (index) (funcall base-gen index)))
                     (vader-base varray))))))))

(defclass vader-partition (vad-nested varray-derived vad-on-axis vad-with-io
                           vad-with-argument vad-maybe-shapeless)
  ((%params :accessor vapart-params
            :initform nil
            :initarg :params
            :documentation "Parameters for partitioning."))
  (:metaclass va-class)
  (:documentation "A partitioned array as from the [⊆ partition] function."))

(defmethod etype-of ((varray vader-partition))
  (let ((base (vader-base varray)))
    (if (or (arrayp base) (varrayp base)
            (vads-argument varray))
        t (assign-element-type base))))

(defmethod shape-of ((varray vader-partition))
  (get-promised
   (varray-shape varray)
   (let* ((idims (shape-of (vader-base varray)))
          (arank (length idims))
          (positions (when (vads-argument varray)
                       (setf (vads-argument varray)
                             (funcall (lambda (i)
                                        (if (or (not (arrayp i))
                                                (< 0 (array-rank i)))
                                            i (vector (aref i))))
                                      (disclose-unitary (render (vads-argument varray)))))))
          (axis (setf (vads-axis varray)
                      (when (vads-axis varray)
                        (max 0 (if (eq :last (vads-axis varray))
                                   (1- arank)
                                   (- (vads-axis varray)
                                      (vads-io varray))))))))
     (if (not positions)
         (let ((base-type (etype-of (vader-base varray))))
           (when (eq t base-type) ;; array is simple if not t-type
             (let ((is-complex) (base-gen (generator-of (vader-base varray))))
               (when (and base-gen (functionp base-gen))
                 (loop :for i :below (size-of (vader-base varray))
                       :do (when (shape-of (funcall base-gen i))
                             (setf is-complex t)))
                 (when is-complex (shape-of (vader-base varray)))))))
         (if (not (arrayp positions))
             (when (< 1 (size-of (vader-base varray)))
               (setf (vapart-params varray) (list :partitions 1))
               (list 1))
             (let ((r-indices) (r-intervals) (indices) (intervals)
                   (interval-size 0) (current-interval -1) (partitions 0))
               (declare (dynamic-extent r-indices r-intervals indices intervals
                                        interval-size current-interval partitions))
               ;; find the index where each partition begins in the
               ;; input array and the length of each partition
               (loop :for pos :across positions :for p :from 0
                     :do (unless (zerop current-interval)
                           (incf interval-size))
                         ;; if a position lower than the current interval index is encountered,
                         ;; decrement the current index to it, as for 1 1 1 2 1 1 2 1 1⊆⍳9
                         (when (and current-interval (< 0 pos current-interval))
                           (setq current-interval pos))
                     :when (or (< current-interval pos)
                               (and (zerop pos) (not (zerop current-interval))))
                       :do (setq r-indices (cons p r-indices)
                                 r-intervals (when (rest r-indices)
                                               (cons interval-size r-intervals)))
                           (incf partitions (if (zerop pos) 0 1))
                           (setq current-interval pos interval-size 0))
               ;; add the last entry to the intervals provided the
               ;; positions list didn't have a 0 value at the end
               (unless (zerop (aref positions (1- (length positions))))
                 (push (- (length positions) (first r-indices))
                       r-intervals))
               
               (when (/= (length r-indices) (length r-intervals))
                 (setq r-indices (rest r-indices)))
               ;; collect the indices and intervals into lists the right way
               ;; around, dropping indices with 0-length intervals
               ;; corresponding to zeroes in the positions list
               (loop :for rint :in r-intervals :for rind :in r-indices
                     :when (not (zerop rint))
                       :do (push rint intervals)
                           (push rind indices))
               
               (setf (vapart-params varray)
                     (list :partitions (or partitions 1)
                           :intervals (coerce intervals 'vector)
                           :indices (coerce indices 'vector)))

               (loop :for dim :in idims :for dx :below arank
                     :collect (if (= dx axis) partitions dim))))))))

(defmethod generator-of ((varray vader-partition) &optional indexers params)
  (declare (ignore indexers))
  (if (not (or (vads-argument varray)
               (vapart-params varray)))
      (if (or (shape-of varray) ;; pass through the generator of i.e. ⊂⍳3
              (typep (vader-base varray) 'vader-enclose))
          (generator-of (vader-base varray))
          (vader-base varray))
      (let* ((idims (shape-of (vader-base varray)))
             (partitions (getf (vapart-params varray) :partitions))
             (intervals (getf (vapart-params varray) :intervals))
             (indices (getf (vapart-params varray) :indices))
             (base-gen (generator-of (vader-base varray)))
             (axis (vads-axis varray))
             (section-size (reduce #'* (loop :for d :in idims :for dx :from 0
                                             :when (> dx axis) :collect d)))
             (output-shape (shape-of varray))
             (ofactors (get-dimensional-factors output-shape t))
             (ifactors (get-dimensional-factors idims t))
             (partition-indexer
               (lambda (i focus)
                 (lambda (ix)
                   (let ((rest i) (input-index 0))
                     (loop :for of :across ofactors :for if :across ifactors :for fx :from 0
                           :do (multiple-value-bind (factor remaining) (floor rest of)
                                 (setq rest remaining)
                                 (incf input-index (* if (if (/= fx axis) factor
                                                             (+ ix (if (not indices)
                                                                       0 (aref indices focus))))))))
                     (funcall base-gen input-index))))))
        
        (case (getf params :base-format)
          (:encoded)
          (:linear)
          (t (if (not (functionp base-gen))
                 base-gen (lambda (index)
                            (let* ((focus (mod (floor index section-size) partitions))
                                   (sub-indexer (funcall partition-indexer index focus))
                                   (first-item (funcall sub-indexer 0))
                                   (prototype (if (not output-shape)
                                                  (aplesque::make-empty-array
                                                   (vader-base varray))
                                                  (if (varrayp first-item)
                                                      (prototype-of first-item)
                                                      (apl-array-prototype first-item)))))
                              (make-instance 'vader-subarray
                                             :prototype prototype :base (vader-base varray)
                                             :shape (if (not intervals)
                                                        idims (list (aref intervals focus)))
                                             :generator sub-indexer)))))))))

(defclass vader-expand (varray-derived vad-on-axis vad-with-io vad-with-argument vad-invertable vad-reindexing)
  ((%separating :accessor vadex-separating
                :initform nil
                :initarg :separating
                :documentation "Whether this section separates sections of the input with empty space in between to be filled with the array's prototype."))
  (:metaclass va-class)
  (:documentation "An expanded (as from [\ expand]) or compressed (as from [/ compress]) array."))

(defmethod effector-of ((varray vader-expand) &optional params)
  (declare (ignore params))
  (let* ((format (getf params :format))
         (axis (vads-axis varray))
         (metadata (getf (metadata-of varray) :shape))
         (encoding (getf (rest (getf metadata :gen-meta)) :index-width))
         (etag (case encoding (8 :byte) (16 :word) (32 :dword) (64 :qword))))
    (case format
      (:lisp
       nil))))

(defmethod shape-of ((varray vader-expand))
  "The shape of an expanded or compressed array."
  (get-promised
   (varray-shape varray)
   (let* ((degrees-count (first (shape-of (vads-argument varray))))
          (degrees (setf (vads-argument varray)
                         (funcall (lambda (i)
                                    (if (not (arrayp i))
                                        i (if (< 0 (array-rank i))
                                              i (vector (aref i)))))
                                  (disclose-unitary (render (vads-argument varray))))))
          (base-shape (copy-list (shape-of (vader-base varray))))
          (base-rank (length base-shape))
          (is-inverse (vads-inverse varray))
          (axis (setf (vads-axis varray)
                      (max 0 (if (eq :last (vads-axis varray))
                                 (1- base-rank)
                                 (- (vads-axis varray)
                                    (vads-io varray)))))))
     
     ;; designate the array as separating if appropriate;
     ;; this will determine the generator logic
     (if (not (vectorp degrees)) (unless (plusp degrees)
                                   (setf (vadex-separating varray) t))
         (loop :for d :across degrees :when (not (plusp d))
               :do (setf (vadex-separating varray) t)))
     ;; REDUCE SHAPE ERROR CAUSED HERE

     (cond ((and base-shape (zerop (reduce #'* base-shape)))
            (if is-inverse
                (if (> axis (1- base-rank))
                    (error "This array does not have an axis ~a." axis)
                    (if (or (not (arrayp degrees))
                            (or (not degrees-count)
                                (= 1 degrees-count)))
                        (loop :for d :in base-shape :for dx :from 0
                              :collect (if (/= dx axis) d (* d (disclose-unitary degrees))))
                        (when degrees-count
                          (if (and degrees-count (/= degrees-count (nth axis base-shape)))
                              (error "Compression degrees must equal size of array in dimension to compress.")
                              (let ((output-size (loop :for d :below degrees-count
                                                       :summing (if (not (arrayp degrees))
                                                                    degrees (aref degrees d)))))
                                (loop :for d :in base-shape :for dx :from 0
                                      :collect (if (= dx axis) output-size d)))))))
                (if (or (not degrees-count)
                        (= 1 degrees-count)
                        (not (arrayp degrees)))
                    (list (if (and (= 1 base-rank)
                                   (zerop (if (not (arrayp degrees))
                                              degrees (aref degrees 0))))
                              1 (abs (if (not (arrayp degrees))
                                         degrees (aref degrees 0)))))
                    (if (and (loop :for d :across degrees :always (zerop d))
                             (zerop (nth axis base-shape)))
                        (if (= 1 base-rank) (list degrees-count)
                            (loop :for d :in base-shape :for dx :from 0
                                  :collect (if (= dx axis) degrees-count d)))
                        (error "An empty array can only be expanded to a single negative degree ~a"
                               "or to any number of empty dimensions.")))))
           ((and (not is-inverse)
                 (or (and (not (arrayp degrees))
                          (zerop degrees))
                     (and degrees-count (= 1 degrees-count)
                          (if (not (arrayp degrees))
                              (arrayp degrees)
                              (zerop (aref degrees 0))))))
            (append (butlast base-shape) (list 0)))
           ((and (not base-shape)
                 (not (arrayp degrees)))
            (setf (vads-argument varray) (list (abs degrees))))
           ((and is-inverse base-shape degrees-count (< 1 degrees-count)
                 (nth axis base-shape)
                 (/= degrees-count (nth axis base-shape)))
            (error "Attempting to replicate elements across array but ~a"
                   "degrees are not equal to length of selected input axis."))
           ((and (not is-inverse)
                 base-shape (< 1 (reduce #'* base-shape))
                 (nth axis base-shape)
                 (/= (or (and (arrayp degrees)
                              (loop :for degree :across degrees :when (plusp degree)
                                    :counting degree :into dcount :finally (return dcount)))
                         degrees)
                     (nth axis base-shape)))
            (error "Attempting to expand elements across array but ~a"
                   "positive degrees are not equal to length of selected input axis."))
           (t (let ((ex-dim))
                (if (arrayp degrees)
                    (loop :for degree :across degrees :for dx :from 0
                          :summing (max (abs degree) (if is-inverse 0 1)) :into this-dim
                          :finally (setf ex-dim this-dim))
                    (setf ex-dim (* (abs degrees) (or (nth axis base-shape) 1))))
                (loop :for dim :in (or base-shape '(1)) :for index :from 0
                      :collect (if (/= index axis) dim (* 1 ex-dim)))))))))

(defmethod indexer-of ((varray vader-expand) &optional params)
  (get-promised (varray-generator varray)
                (let* ((arg-rendered (render (vads-argument varray)))
                       ;; TODO: why can't (vector item) be used below?
                       (arg-vector (if (not (typep arg-rendered 'sequence))
                                       arg-rendered (coerce arg-rendered 'vector))))
                  (shape-of varray) ;; needed to replace :last axis with actual last axis index
                  (when (or (integerp arg-rendered)
                            (< 0 (size-of arg-vector)))
                    (indexer-expand arg-vector (shape-of (vader-base varray))
                                    (vads-axis varray)
                                    (vads-inverse varray)
                                    (getf params :for-selective-assign))))))

(defmethod generator-of ((varray vader-expand) &optional indexers params)
  (let ((indexer (indexer-of varray params)))
    (case (getf params :base-format)
      (:encoded)
      (:linear)
      (t (if (getf params :for-selective-assign)
             indexer
             (if (vadex-separating varray)
                 (let ((generator (generator-of (vader-base varray)))
                       (prototype (prototype-of (vader-base varray)))
                       (composite-indexer (join-indexers indexers params)))
                   (lambda (index)
                     (let ((indexed (funcall indexer (funcall composite-indexer index))))
                       (if (not indexed)
                           prototype (if (not (functionp generator))
                                         generator (funcall generator indexed))))))
                 (generator-of (vader-base varray)
                               (cons indexer indexers))))))))

(defclass vader-pick (varray-derived vad-with-argument vad-with-io)
  ((%reference :accessor vapick-reference
               :initform nil
               :initarg  :reference
               :documentation "Reference to the array designated by this function.")
   (%assign    :accessor vapick-assign
               :initform nil
               :initarg  :assign
               :documentation "Item to be assigned to selected index in array.")
   (%selector  :accessor vapick-selector
               :initform nil
               :initarg  :selector
               :documentation "Virtual array selecting items within index to assign.")
   (%function  :accessor vapick-function
               :initform nil
               :initarg  :assign
               :documentation "Function to be applied to selected index in array.")
   (%ap-index  :accessor vapick-apath-index
               :initform 0
               :initarg  :assign-path-index
               :documentation "Index for assignment path."))
  (:metaclass va-class)
  (:documentation "An element from within an array as from the [⊃ disclose] or [⊃ pick] functions."))

(defun get-path-value (varray index base)
  (if (numberp index)
      (- index (vads-io varray))
      (let* ((ix 0)
             (igen (generator-of index))
             (iindexer (if (shape-of index) (generator-of index)
                           (generator-of (if (not (functionp igen))
                                             igen (funcall igen 0)))))
             (index-length (if (shape-of index) (size-of index)
                               (size-of (if (not (functionp igen))
                                            igen (funcall igen 0)))))
             (factors (get-dimensional-factors (shape-of base))))
        (loop :for f :in factors :for s :below index-length
              :do (incf ix (* f (- (if (not (functionp iindexer))
                                       iindexer (funcall iindexer s))
                                   (vads-io varray)))))
        ix)))

(defgeneric fetch-reference (varray base &optional path path-index)
  (:documentation "Fetch the array targeted by an invocation of [⊃ disclose] or [⊃ pick]."))

(defmethod fetch-reference ((varray vader-pick) base &optional path path-index)
  (or (vapick-reference varray)
      (let* ((path (or path (vads-argument varray)))
             (path-indexer (generator-of path))
             (path-length (size-of path))
             (base-gen (generator-of base)))
        (if path
            (let ((path-value (get-path-value varray (if (not (functionp path-indexer))
                                                         path-indexer (funcall path-indexer
                                                                               (or path-index 0)))
                                              base)))
              ;; (print (list :pv path-value))
              (if path-index (if (/= path-index (1- path-length))
                                 (fetch-reference varray (funcall base-gen path-value)
                                                  path (1+ (or path-index 0)))
                                 (let ((indexer (if (not (functionp base-gen))
                                                    ;; TODO: special mix case, generalize
                                                    base-gen (if (typep base 'vader-mix)
                                                                 (let ((bix (funcall base-gen 0)))
                                                                   (if (not (arrayp bix))
                                                                       bix (row-major-aref
                                                                            bix path-value)))
                                                                 (funcall base-gen path-value)))))
                                   (when (and (not (shape-of indexer))
                                              (or (arrayp indexer) (varrayp indexer)))
                                     (setf (vads-nested varray) t))
                                   indexer))
                  (setf (vapick-reference varray)
                        (if (= 1 path-length)
                            (let ((indexer (if (not (functionp base-gen))
                                               base-gen
                                               (if (or (typep base 'vader-mix)
                                                       (and (typep base 'vader-pick)
                                                            (typep (vader-base base) 'vader-mix)))
                                                   ;; vader-pick is handled here for cases like
                                                   ;; { ee←{↑⍪/(⊂⍺),⍶,⊂⍵} ⋄ ⍵⊃⊃↑{⍺ ee⌿⍵}/9⍴⊂⍳9 } 22
                                                   ;; TODO: condense successive picks like ⍵⊃⊃ into one
                                                   (let ((bix (funcall base-gen 0)))
                                                     ;; (print (list :ib bix path-value))
                                                     (if (arrayp bix)
                                                         (row-major-aref bix path-value)
                                                         (funcall (generator-of bix) path-value)))
                                                   (funcall base-gen path-value)))))
                              (when (and (not (shape-of indexer))
                                         (or (arrayp indexer) (varrayp indexer)))
                                (setf (vads-nested varray) t))
                              indexer)
                            (fetch-reference varray (if (not (functionp base-gen))
                                                        base-gen (funcall base-gen path-value))
                                             path (1+ (or path-index 0)))))))
            (setf (vapick-reference varray)
                  ;; the 'vader-pick clause handles nested pick references like 2⊃⊃⊃{,/⍵}/3⍴⊂⍳3
                  (let ((indexer (if (functionp base-gen)
                                     ;; return the 1st element, as for ⊃,/1+0×⊂1 2 3
                                     (if (zerop (size-of base))
                                         (prototype-of base)
                                         (let ((bix (funcall base-gen 0)))
                                           (if (typep bix 'vader-pick)
                                               ;; needed for i.e. 0 in (1 1⍴⊂)⍣4⊢0
                                               base-gen (if (or (not (arrayp bix))
                                                                ;; TODO: special mix case, generalize
                                                                (not (typep base 'vader-mix)))
                                                            bix (row-major-aref bix 0)))))
                                     (if (or (shape-of base-gen)
                                             (and (not (varrayp base-gen))
                                                  (not (functionp base-gen))))
                                         ;; return just the base indexer in cases like ⊃3
                                         (or base-gen (prototype-of base))
                                         (let ((sub-gen (generator-of base-gen)))
                                           (if (functionp sub-gen)
                                               (funcall (generator-of base-gen) 0)
                                               (if (= 1 (size-of base-gen))
                                                   ;; handle the case of i.e. ⊃⊃,/⊂⊂⍳3
                                                   base-gen sub-gen)))))))
                    (when (and (shape-of base) (not (shape-of indexer))
                               (or (arrayp indexer)
                                   (varrayp indexer)))
                      (setf (vads-nested varray) t))
                    indexer))))))

(defgeneric assign-reference (varray base &optional path path-indexer path-index))

(defmethod assign-reference ((varray vader-pick) base &optional path path-indexer path-index)
  (let ((base-gen (generator-of base))
        (assigned-index (if (not path)
                            0 (get-path-value
                               varray (if (not (functionp path-indexer))
                                          path-indexer (funcall path-indexer (or path-index 0)))
                               base))))
    (setf (vads-nested varray) t)
    (lambda (index)
      (if (= index assigned-index)
          (if (vapick-selector varray)
              (make-instance
               'vader-select :base (funcall base-gen index) :index-origin (vads-io varray)
                             :assign (vapick-assign varray) :selector (vapick-selector varray))
              (if (vapick-function varray)
                  (funcall (vapick-function varray)
                           (funcall base-gen index) (vapick-assign varray))
                  (vapick-assign varray)))
          (funcall base-gen index)))))

(defmethod etype-of ((varray vader-pick))
  (if (vapick-assign varray) ;; TODO: specify
      t (etype-of (fetch-reference varray (vader-base varray)))))

(defmethod prototype-of ((varray vader-pick))
  (if (vapick-assign varray)
      (prototype-of (vader-base varray))
      (prototype-of (fetch-reference varray (vader-base varray)))))

(defmethod shape-of ((varray vader-pick))
  (get-promised
   (varray-shape varray)
   (if (vapick-assign varray)
       (shape-of (vader-base varray))
       (if (zerop (size-of (vader-base varray)))
           (shape-of (prototype-of (vader-base varray)))
           (let* ((ref (fetch-reference varray (vader-base varray)))
                  (this-indexer (generator-of ref)))
             (if (and (not (functionp this-indexer)) ;; handle cases like (scc≡⍳∘≢) (⍳10),⊂⍬
                      (not (typep ref 'vader-pare)))
                 ;; vader-pare covers the case of (april-c "{⊃,∘⊂¨⍵}" #2A((#(#*11)))),
                 ;; is there another way to do this? 
                 (if (arrayp this-indexer)
                     (shape-of (row-major-aref this-indexer 0))
                     (when (arrayp ref) (shape-of ref)))
                 (shape-of ref)))))))

(defmethod generator-of ((varray vader-pick) &optional indexers params)
  (declare (ignore indexers))
  (case (getf params :base-format)
    (:encoded)
    (:linear)
    (t (if (vapick-assign varray)
           (if (= (vapick-apath-index varray)
                  (1- (size-of (vads-argument varray))))
               (assign-reference varray (vader-base varray) (vads-argument varray)
                                 (generator-of (vads-argument varray))
                                 0)
               (let* ((base-gen (generator-of (vader-base varray)))
                      (path-gen (generator-of (vads-argument varray)))
                      (this-index (get-path-value
                                   varray (if (not (functionp path-gen))
                                              path-gen (funcall path-gen
                                                                (vapick-apath-index varray)))
                                   (vader-base varray))))
                 (setf (vads-nested varray) t)
                 (lambda (index)
                   (if (= index this-index)
                       (if (vapick-selector varray)
                           (make-instance
                            'vader-select :base (funcall base-gen this-index)
                                          :index-origin (vads-io varray)
                                          :assign (vapick-assign varray)
                                          :selector (vapick-selector varray))
                           (make-instance
                            'vader-pick :base (funcall base-gen this-index)
                                        :argument (vads-argument varray)
                                        :index-origin (vads-io varray)
                                        :assign (vapick-assign varray)
                                        :assign-path-index (1+ (vapick-apath-index varray))))
                       (funcall base-gen index)))))
           
           (let* ((this-ref (fetch-reference varray (vader-base varray)))
                  (this-gen (if (or (typep this-ref 'vader-enclose)
                                    (typep this-ref 'vader-pare))
                                ;; vader-pare covers the case of (april-c "{⊃,∘⊂¨⍵}" #2A((#(#*11)))),
                                ;; is there another way to do this?
                                this-ref (generator-of this-ref)))) ;; IPV-TODO: generator bug!
             ;; TODO: there's a problem with the pick in i.e. 0 in (1 1⍴⊂)⍣4⊢0 (in array-lib-space)
             ;; when the vader-enclose generator just returns its base with no enclosing function,
             ;; find out what's wrong - rewriting of the enclose and pick classes may be needed
             ;; (print (list :ii this-gen (vader-base varray) this-ref (render this-ref)
             ;;              (shape-of varray)))
             (if (varrayp this-gen)
                 (generator-of this-gen)
                 (if (not (arrayp this-gen))
                     this-gen (generator-of (row-major-aref this-gen
                                                            (or (vads-argument varray) 0))))))))))

(defclass vader-intersection (varray-derived vad-limitable)
  nil (:metaclass va-class)
  (:documentation "An array intersection as from the [∩ intersection] function."))

(defmethod etype-of ((varray vader-intersection))
  (let ((this-indexer (generator-of varray)))
    (declare (ignore this-indexer))
    (etype-of (vader-content varray))))

(defmethod prototype-of ((varray vader-intersection))
  (let ((this-indexer (generator-of varray)))
    (declare (ignore this-indexer))
    (prototype-of (vader-content varray))))

(defmethod shape-of ((varray vader-intersection))
  (get-promised (varray-shape varray) (let ((this-indexer (generator-of varray)))
                                        (declare (ignore this-indexer))
                                        (shape-of (vader-content varray)))))

(defmethod generator-of ((varray vader-intersection) &optional indexers params)
  (declare (ignore indexers))
  (unless (vader-content varray)
    (let ((derivative-count (when (and (varray-shape varray)
                                       (listp (varray-shape varray)))
                              (reduce #'* (varray-shape varray))))
          (contents (loop :for a :across (render (vader-base varray)) :collect (render a))))
      (if (not (loop :for a :in contents :always (not (second (shape-of a)))))
          (error "Arguments to [∩ intersection] must be vectors.")
          (let* ((match-count 0)
                 (matches (if (arrayp (first contents))
                              (loop :for item :across (first contents)
                                    :while (or (not derivative-count) (< match-count derivative-count))
                                    :when (loop :for c :in (rest contents)
                                                :always (if (arrayp c)
                                                            (find item c :test #'array-compare)
                                                            (array-compare item c)))
                                      :collect item :and :do (incf match-count))
                              (when (loop :for c :in (rest contents)
                                          :always (if (arrayp c)
                                                      (find (first contents)
                                                            c :test #'array-compare)
                                                      (array-compare (first contents) c)))
                                (incf match-count)
                                (list (first contents))))))
            (setf (vader-content varray)
                  (make-array match-count :initial-contents matches
                                          :element-type (apply #'type-in-common
                                                               (mapcar #'etype-of contents))))))))
  (case (getf params :base-format)
    (:encoded)
    (:linear)
    (t (lambda (index) (aref (vader-content varray) index)))))

(defclass vader-unique (varray-derived vad-limitable)
  ((%indices :accessor vauni-indices
             :initform nil
             :initarg :indices
             :documentation "Parameters for partitioning."))
  (:metaclass va-class)
  (:documentation "An array intersection as from the [∩ intersection] function."))

(defmethod etype-of ((varray vader-unique))
  (declare (ignore varray))
  t)
 
(defmethod shape-of ((varray vader-unique))
  (get-promised (varray-shape varray)
                (progn (generator-of varray)
                       (if (vader-content varray)
                           (cons (length (vauni-indices varray))
                                 (rest (shape-of (vader-content varray))))
                           (list (length (vauni-indices varray)))))))

(defmethod generator-of ((varray vader-unique) &optional indexers params)
  (declare (ignore indexers))
  (let* ((base-shape (shape-of (vader-base varray)))
         (cell-size (reduce #'* (rest base-shape)))
         (base-size (size-of (vader-base varray)))
         (base-rank (length base-shape))
         (base-gen (generator-of (vader-base varray))))
    (if (not (vauni-indices varray))
        (let ((derivative-count (when (and (varray-shape varray)
                                           (listp (varray-shape varray)))
                                  (reduce #'* (varray-shape varray)))))
          (if (not (functionp base-gen))
              (setf (vauni-indices varray)
                    (if (not base-shape) #*0 #()))
              (if (= 1 base-rank)
                  (let ((uniques) (indices) (unique-count 0))
                    (loop :for ix :below base-size
                          :while (or (not derivative-count) (< unique-count derivative-count))
                          :do (let ((item (render (funcall base-gen ix))))
                                (unless (find item uniques :test #'array-compare)
                                  (push item uniques)
                                  (push ix indices)
                                  (incf unique-count))))
                    (setf (vauni-indices varray)
                          (make-array unique-count :element-type (list 'integer 0 base-size)
                                                   :initial-contents (reverse indices))))
                  (let ((base (render (vader-base varray)))
                        (major-cells (make-array (first base-shape)))
                        (indices) (uniques) (unique-count 0))
                    (loop :for i :below (first base-shape)
                          :do (setf (aref major-cells i)
                                    (make-array (rest base-shape)
                                                :displaced-to base :element-type (etype-of base)
                                                :displaced-index-offset (* i cell-size))))
                    (loop :for item :across major-cells :for ix :from 0
                          :when (not (find item uniques :test #'array-compare))
                            :do (push ix indices)
                                (push item uniques)
                                (incf unique-count))
                    (setf (vader-content varray) base
                          (vauni-indices varray)
                          (make-array unique-count :element-type (list 'integer 0 (first base-shape))
                                                   :initial-contents (reverse indices))))))))
    (case (getf params :base-format)
      (:encoded)
      (:linear)
      (t (if (functionp base-gen)
             (if (= 1 base-rank)
                 (lambda (index) (funcall base-gen (aref (vauni-indices varray) index)))
                 (lambda (index)
                   (multiple-value-bind (count remainder) (floor index cell-size)
                     (row-major-aref (vader-content varray)
                                     (+ remainder (* cell-size (aref (vauni-indices varray)
                                                                     count)))))))
             base-gen)))))

(defclass vader-union (varray-derived vad-limitable vad-render-mutable)
  nil (:metaclass va-class)
  (:documentation "An array intersection as from the [∩ union] function."))

(defmethod etype-of ((varray vader-union))
  (apply #'type-in-common (loop :for array :across (vader-base varray) :collect (etype-of array))))

(defmethod shape-of ((varray vader-union))
  (get-promised (varray-shape varray)
                (progn (generator-of varray)
                       (list (+ (or (first (shape-of (aref (vader-content varray) 0))) 1)
                                (or (first (shape-of (aref (vader-content varray) 1))) 1))))))

(defmethod generator-of ((varray vader-union) &optional indexers params)
  (declare (ignore indexers))
  (let* ((contents (loop :for a :across (vader-base varray) :collect (render a)))
         (shapes (loop :for a :across (vader-base varray) :collect (shape-of a)))
         (first (first contents)))
    (if (not (loop :for shape :in shapes :always (not (second shape))))
        (error "Arguments to [∪ union] must be vectors.")
        (let ((matched) (appended))
          (dolist (this-item (rest contents))
            (push nil matched)
            (if (arrayp first)
                (if (arrayp this-item)
                    (loop :for ti :across this-item
                          :when (not (find ti first :test #'array-compare))
                            :do (push ti (first matched)))
                    (unless (find this-item first :test #'array-compare)
                      (push this-item (first matched))))
                (if (arrayp this-item)
                    (loop :for ti :across this-item
                          :when (not (array-compare ti first))
                            :do (push ti (first matched)))
                    (unless (array-compare this-item first)
                      (push this-item (first matched)))))
            (dolist (m (rest matched))
              (if (arrayp this-item)
                  (loop :for ti :across this-item
                        :when (not (find ti m :test #'array-compare))
                          :do (push ti (first matched)))
                  (unless (find this-item m :test #'array-compare)
                    (push this-item (first matched))))))
          (dolist (m matched) (dolist (subm m) (push subm appended)))
          (setf (vader-content varray)
                (vector first (make-array (length appended)
                                          :initial-contents appended)))))
    (case (getf params :base-format)
      (:encoded)
      (:linear)
      (t (if (arrayp (aref (vader-content varray) 0))
             (let ((first-length (length (aref (vader-content varray) 0))))
               (lambda (index)
                 (if (< index (length (aref (vader-content varray) 0)))
                     (aref (aref (vader-content varray) 0) index)
                     (aref (aref (vader-content varray) 1)
                           (- index first-length)))))
             (lambda (index)
               (if (zerop index) (aref (vader-content varray) 0)
                   (aref (aref (vader-content varray) 1)
                         (1- index)))))))))

(defclass vader-turn (varray-derived vad-on-axis vad-with-io vad-with-argument vad-invertable vad-reindexing)
  ((%degrees :accessor vaturn-degrees
             :initform nil
             :initarg :degrees
             :documentation "Real degrees to rotate the array (with modulo applied)."))
  (:metaclass va-class)
  (:documentation "A rotated array as from the [⌽ rotate] function."))

(defun arg-process (varray)
  (let ((argument (vads-argument varray)))
    (funcall (if (vads-inverse varray) #'- #'identity)
             (if (or (listp argument) (numberp argument))
                 argument (if (varrayp argument)
                              (render argument) ;; TODO: eliminate forced render here
                              (when (arrayp argument) argument))))))

(defmethod effector-of ((varray vader-turn) &optional params)
  (declare (ignore params))
  (let* ((format (getf params :format))
         (coordinate-type (getf (rest (getf params :gen-meta)) :index-type))
         (axis (max 0 (if (eq :last (vads-axis varray))
                          (1- (rank-of varray))
                          (- (vads-axis varray)
                             (vads-io varray)))))
         (unit (expt 2 (* 8 (max 0 (- (rank-of varray) (1+ axis))))))
         (limit (* unit (or (nth axis (shape-of varray)) 1)))
         (arg (setf (vads-argument varray) (arg-process varray)))
         (dimension (nth axis (shape-of varray))))
    ;; (print (list :ct coordinate-type))
    (unless (vaturn-degrees varray)
      (setf (vaturn-degrees varray)
            (if (integerp arg) (when dimension (mod arg dimension))
                (when (arrayp arg)
                  (let ((out (make-array (array-dimensions arg)
                                         :element-type (array-element-type arg))))
                    (dotimes (i (array-total-size arg))
                      (setf (row-major-aref out i)
                            (mod (row-major-aref arg i) dimension)))
                    out)))))
    (case format
      (:lisp
       (if (vaturn-degrees varray)
           (when (numberp (vaturn-degrees varray))
             ;; dyadic, as for X⌽Y
             (lambda (ein-sym)
               `((incf ,ein-sym ,(* unit (vaturn-degrees varray)))
                 (when (>= (ldb (byte ,coordinate-type
                                      ,(if (= axis (1- (rank-of varray)))
                                           0 (* coordinate-type axis)))
                                ,ein-sym)
                           ,dimension)
                   (decf ,ein-sym ,limit)))))
           (lambda (ein-sym)
             ;; monadic, as for ⌽Y
             `((setf ,ein-sym (+ (logand ,ein-sym ,(ash 255 (* 8 (- (rank-of varray)
                                                                    (1+ axis)))))
                                 (abs (- ,(1- dimension)
                                         (ldb (byte ,coordinate-type
                                                    ,(if (= axis (1- (rank-of varray)))
                                                         0 (* coordinate-type (- (rank-of varray) axis))))
                                              ,ein-sym))))))))))))

(defmethod generator-of ((varray vader-turn) &optional indexers params)
  (when (and (vads-argument varray)
             (shape-of varray) (not (vaturn-degrees varray)))
    (let ((arg (setf (vads-argument varray)
                     (arg-process varray)))
          (dimension (nth (if (eq :last (vads-axis varray))
                              (1- (rank-of varray))
                              (- (vads-axis varray)
                                 (vads-io varray)))
                          (shape-of varray))))
      (setf (vaturn-degrees varray)
            (if (integerp arg) (mod arg dimension)
                (when (arrayp arg)
                  (let ((out (make-array (array-dimensions arg)
                                         :element-type (array-element-type arg))))
                    (dotimes (i (array-total-size arg))
                      (setf (row-major-aref out i)
                            (mod (row-major-aref arg i) dimension)))
                    out))))))
  (case (getf params :base-format)
    (:tokenized (cons (list :turn :degrees (vaturn-degrees varray))
                      (generator-of (vader-base varray) nil params)))
    (:encoded (unless (shape-of (vads-argument varray))
                ;; disabled for non-scalar left arguments; TODO: can this be changed?
                (let* ((enco-type (getf (getf params :gen-meta) :index-width))
                       (coord-type (getf (getf params :gen-meta) :index-type))
                       (indexer (when (shape-of varray)
                                  (indexer-turn (if (eq :last (vads-axis varray))
                                                    (1- (rank-of varray))
                                                    (- (vads-axis varray)
                                                       (vads-io varray)))
                                                (shape-of varray)
                                                enco-type coord-type (vaturn-degrees varray))))
                       (all-indexers))
                  (when indexer
                    (when (eq :linear (getf params :format))
                      (push (encode-rmi (get-dimensional-factors (shape-of varray) t)
                                        enco-type coord-type)
                            all-indexers)
                      (setf (getf params :format) :encoded))
                    (push indexer all-indexers)
                    (generator-of (vader-base varray) (append all-indexers indexers)
                                  params)))))
    (:linear)
    (t (let ((indexer (when (shape-of varray)
                        (indexer-turn (if (eq :last (vads-axis varray))
                                          (1- (rank-of varray))
                                          (- (vads-axis varray)
                                             (vads-io varray)))
                                      (shape-of varray)
                                      nil nil (vaturn-degrees varray)))))
         (generator-of (vader-base varray)
                       (if (not indexer) indexers (cons indexer indexers))
                       params)))))

(defmethod initialize-instance :after ((varray vader-turn) &key)
  "Sum cumulative rotations into a single rotation; currently only works with a scalar left argument."
  (let ((base (vader-base varray))
        (axis (vads-axis varray))
        (argument (vads-argument varray)))
    (when (typep base 'vader-turn)
      (let ((base-axis (vads-axis base))
            (base-arg (vads-argument base))
            (sub-base (vader-base base)))
        (when (or (and (eq :last axis)
                       (eq :last base-axis))
                  (and (numberp axis)
                       (numberp base-axis)
                       (= axis base-axis)))
          (when (and argument base-arg)
            (setf (vads-argument varray) (+ argument base-arg)
                  (vader-base varray) sub-base)))))))

(defmethod assign-rank ((varray vader-turn) rank)
  "Assign a rank at which the [⌽⊖ rotate] function shall operate, for its composition with the [⍤ rank] operator."
  (when (= 1 rank)
    (setf (vads-axis varray) (- (rank-of (vader-base varray))
                                (abs (1- (vads-io varray)))))
    varray))

(defclass vader-permute (varray-derived vad-with-io vad-with-argument vad-reindexing)
  ((%is-diagonal :accessor vaperm-is-diagonal
                 :initform nil
                 :initarg :is-diagonal
                 :documentation "Whether this permutation is diagonal, as from 1 1 2⍉2 3 4⍴⍳9."))
  (:metaclass va-class)
  (:documentation "A permuted array as from the [⍉ permute] function."))

(defmethod effector-of ((varray vader-permute) &optional params)
  (declare (ignore params))
  (let* ((format (getf params :format))
         (metadata (getf (metadata-of varray) :shape))
         (encoding (getf (rest (getf metadata :gen-meta)) :index-width))
         (cwidth (getf (rest (getf metadata :gen-meta)) :index-type))
         (etag (case encoding (8 :byte) (16 :word) (32 :dword) (64 :qword))))
    (case format
      (:lisp
       ))))

(defmethod shape-of ((varray vader-permute))
  (get-promised
   (varray-shape varray)
   (let* ((base-shape (shape-of (vader-base varray)))
          (argument (setf (vads-argument varray)
                          (render (vads-argument varray))))
          (arg (when argument
                 (if (vectorp argument)
                     (coerce (loop :for a :across argument :collect (max 0 (- a (vads-io varray))))
                             'vector)
                     (- argument (vads-io varray)))))
          (base-rank (length base-shape))
          (odims) (positions) (diagonals))
     (if (not argument)
         (reverse base-shape)
         (progn (setf odims (loop :for i :below base-rank :collect nil))
                (if (vectorp arg)
                    (loop :for i :across arg :for id :in base-shape :for ix :from 0
                          :do (setf (nth i odims) (if (nth i odims)
                                                      (min (nth i odims)
                                                           (nth ix base-shape))
                                                      (nth ix base-shape)))
                              ;; if a duplicate position is found, a diagonal section
                              ;; is being performed
                              (unless (member i positions)
                                (push i positions))
                              ;; collect possible diagonal indices into diagonal list
                              (if (assoc i diagonals)
                                  (push ix (rest (assoc i diagonals)))
                                  (push (list i ix) diagonals)))
                    (setf odims base-shape
                          positions (cons arg positions)))
                (setf (vaperm-is-diagonal varray)
                      (/= base-rank (length positions)))
                (remove nil odims))))))

(defmethod generator-of ((varray vader-permute) &optional indexers params)
  (case (getf params :base-format)
    (:encoded (let* ((enco-type (getf (getf params :gen-meta) :index-width))
                     (coord-type (getf (getf params :gen-meta) :index-type))
                     (argument (render (vads-argument varray)))
                     (assigning (getf params :for-selective-assign))
                     (indexer (indexer-permute
                               (shape-of (vader-base varray))
                               (shape-of varray)
                               (when argument
                                 (if (vectorp argument)
                                     (coerce (loop :for a :across argument
                                                   :collect (max 0 (- a (vads-io varray))))
                                             'vector)
                                     (- argument (vads-io varray))))
                               (and (vads-argument varray) (vaperm-is-diagonal varray))
                               enco-type coord-type assigning))
                     (all-indexers))
                (when indexer
                  (when (eq :linear (getf params :format))
                    (push (encode-rmi (get-dimensional-factors (shape-of varray) t)
                                      enco-type coord-type)
                          all-indexers)
                    (setf (getf params :format) :encoded))
                  (push indexer all-indexers)
                  (generator-of (vader-base varray) (append all-indexers indexers)
                                params))))
    (:linear)
    (t (let* ((argument (render (vads-argument varray)))
              (indexer (indexer-permute (shape-of (vader-base varray))
                                        (shape-of varray)
                                        (when argument
                                          (if (vectorp argument)
                                              (coerce (loop :for a :across argument
                                                            :collect (max 0 (- a (vads-io varray))))
                                                      'vector)
                                              (- argument (vads-io varray))))
                                        (and (vads-argument varray) (vaperm-is-diagonal varray))
                                        nil nil (getf params :for-selective-assign))))
         (if (getf params :for-selective-assign)
             indexer (generator-of (vader-base varray)
                                   (if (not indexer) indexers (cons indexer indexers))
                                   params))))))

(defmethod assign-rank ((varray vader-permute) rank)
  "Assign a rank at which the [⍉ permute] function shall operate, for its composition with the [⍤ rank] operator."
  (let ((degree (- (rank-of (vader-base varray)) rank))
        (dims (iota (rank-of (vader-base varray)) :start (vads-io varray))))
    (setf (vads-argument varray)
          (coerce (append (subseq dims 0 degree)
                          (if (vads-argument varray)
                              (loop :for a :across (render (vads-argument varray))
                                    :collect (+ a degree))
                              (reverse (nthcdr degree dims))))
                  'vector))
    varray))

(defclass vader-grade (varray-derived vad-with-argument vad-with-io vad-invertable)
  nil (:metaclass va-class)
  (:documentation "A graded array as from the [⍋⍒ grade up/down] functions."))

(defmethod etype-of ((varray vader-grade))
  (let ((this-shape (first (shape-of varray))))
    (list 'integer (vads-io varray)
          (+ this-shape (vads-io varray)))))

(defmethod prototype-of ((varray vader-grade))
  (declare (ignore varray))
  0)

(defmethod shape-of ((varray vader-grade))
  (get-promised (varray-shape varray)
                (let ((base-shape (shape-of (vader-base varray))))
                  (if base-shape (list (first base-shape))
                      (error "The [⍋ grade] function cannot take a scalar argument.")))))

(defmethod generator-of ((varray vader-grade) &optional indexers params)
  (declare (ignore indexers))
  (let* ((arg-rendered (render (vads-argument varray)))
         (iorigin (vads-io varray))
         (base-rendered (render (vader-base varray))))
    (setf (vader-content varray)
          (if arg-rendered (grade (if (vectorp arg-rendered)
                                      (index-of base-rendered arg-rendered iorigin)
                                      (array-grade arg-rendered base-rendered))
                                  iorigin (alpha-compare (if (vads-inverse varray)
                                                             #'> #'<)))
              (grade base-rendered iorigin (alpha-compare (if (vads-inverse varray)
                                                              #'>= #'<=)))))
    (case (getf params :base-format)
      (:encoded)
      (:linear)
      (t (lambda (index) (aref (vader-content varray) index))))))

(defclass vader-matrix-inverse (varray-derived)
  nil (:metaclass va-class)
  (:documentation "A matrix-inverse array as from the [⌹ matrix inverse] function."))

(defmethod etype-of ((varray vader-matrix-inverse))
  (declare (ignore varray))
  t)

(defmethod shape-of ((varray vader-matrix-inverse))
  (unless (vader-content varray) (generator-of varray))
  (shape-of (vader-content varray)))

(defmethod generator-of ((varray vader-matrix-inverse) &optional indexers params)
  (declare (ignore indexers))
  (let* ((content (when (shape-of (vader-base varray))
                    (or (vader-content varray)
                        (setf (vader-content varray)
                              (funcall (if (and (= 2 (rank-of (vader-base varray)))
                                                (reduce #'= (shape-of (vader-base varray))))
                                           #'invert-matrix #'left-invert-matrix)
                                       (render (vader-base varray)))))))
         (base-gen (unless content (generator-of (vader-base varray)))))
    (case (getf params :base-format)
      (:encoded)
      (:linear)
      (t (or (generator-of content)
             (if (not (functionp base-gen))
                 (if (numberp base-gen)
                     (/ base-gen)
                     (error "The argument to [⌹ matrix inverse] must be a number or array of numbers."))
                 (lambda (index) (/ (funcall base-gen index)))))))))

(defclass vader-matrix-divide (varray-derived vad-with-argument)
  nil (:metaclass va-class)
  (:documentation "A matrix-divided array as from the [⌹ matrix divide] function."))

(defmethod etype-of ((varray vader-matrix-divide))
  (declare (ignore varray))
  t)

(defmethod shape-of ((varray vader-matrix-divide))
  (unless (vader-content varray) (generator-of varray))
  (shape-of (vader-content varray)))

;; linear regression, least squares using matrix divide
;; coef ← (⍉⍵) +.× ⍵
;; cons ← ⍺ +.× ⍵
;; cons ⌹ coef

(defun linear-regression (omega alpha)
  "Linear regression for coefficient matrix ⍵ and constants ⍺. Used to implement dyadic [⌹ matrix divide] for an overspecified system"
  (let ((coef (array-inner-product (permute-axes omega nil)
                                   omega (lambda (a1 a2) (apply-scalar #'* a1 a2))
                                   #'+ t))
        (cons (array-inner-product alpha omega (lambda (a1 a2) (apply-scalar #'* a1 a2))
                                   #'+ t)))
    (matrix-divide coef cons)))

(defun matrix-divide (omega alpha)
  "Divide two matrices. Used to implement dyadic [⌹ matrix divide]."
  ;; if there are more rows than columns the system is over-specifed, use linear-regression
  ;; TODO: what does dyalog do for (⍴⍴⍵) > 2?
  (let ((rho (shape-of omega)))
    (if (> (nth 0 rho) (nth 1 rho))
	(linear-regression omega alpha)
	(array-inner-product (invert-matrix omega) alpha
			     (lambda (arg1 arg2) (apply-scalar #'* arg1 arg2))
			     #'+ t))))

(defmethod generator-of ((varray vader-matrix-divide) &optional indexers params)
  (declare (ignore indexers))
  (let* ((base-shape (shape-of (vader-base varray)))
         (content (when base-shape
                    (or (vader-content varray)
                        (setf (vader-content varray)
                              (matrix-divide (render (vader-base varray))
                                             (render (vads-argument varray))))))))
    (case (getf params :base-format)
      (:encoded)
      (:linear)
      (t (generator-of content)))))

(defclass vader-encode (varray-derived vad-with-argument vad-invertable)
  nil (:metaclass va-class)
  (:documentation "An encoded array as from the [⊤ encode] function."))

(defmethod etype-of ((varray vader-encode))
  (if (not (integerp (vader-base varray)))
      ;; output must be t-type if the base isn't an integer, otherwise
      ;; finding the type may be prohibitively expensive
      t (if (zerop (rank-of (vads-argument varray)))
            (let ((agen (generator-of (vads-argument varray))))
              (assign-element-type (if (not (functionp agen))
                                       agen (funcall agen 0))))
            ;; highest output value is the max of the highest encoding value and the floor of the
            ;; right argument and that highest encoded value
            (let ((max 0) (agen (generator-of (vads-argument varray))))
              (loop :for ax :below (size-of (vads-argument varray))
                    :do (let ((a (if (not (functionp agen))
                                     agen (funcall agen ax))))
                          (setf max (max max a))))
              (setf max (max max (floor (abs (vader-base varray)) max)))
              (list 'integer (if (minusp (vader-base varray)) (- max) 0)
                    max)))))

(defmethod shape-of ((varray vader-encode))
  (get-promised (varray-shape varray)
                (append (if (not (and (vads-inverse varray)
                                      (not (shape-of (vads-argument varray)))))
                            (shape-of (vads-argument varray))
                            (let* ((base (render (vader-base varray)))
                                   (max-base (unless (shape-of base) base)))
                              (unless max-base
                                (setf max-base 0)
                                (dotimes (i (size-of base))
                                  (when (< max-base (row-major-aref base i))
                                    (setf max-base (row-major-aref base i)))))
                              (list (if (zerop max-base)
                                        0 (1+ (floor (log max-base)
                                                     (log (render (vads-argument varray)))))))))
                        (shape-of (vader-base varray)))))

(defmethod generator-of ((varray vader-encode) &optional indexers params)
  (declare (ignore indexers))
  (let* ((out-dims (shape-of varray))
         (scalar-arg (not (shape-of (vads-argument varray))))
         (adims (if (vads-inverse varray)
                    (or (shape-of (vads-argument varray))
                        (butlast out-dims (rank-of (vader-base varray))))
                    (shape-of (vads-argument varray))))
         (base-shape (shape-of (vader-base varray)))
         (base-gen (generator-of (vader-base varray)))
         (arg-gen (generator-of (vads-argument varray)))
         (aifactors (get-dimensional-factors adims))
         (oifactors (get-dimensional-factors base-shape t))
         (ofactors (get-dimensional-factors out-dims t)))
    (case (getf params :base-format)
      (:encoded)
      (:linear)
      (t (lambda (index)
           (let ((remaining index) (base 1) (oindex 0) (afactor 0) (oix 0) (value))
             (loop :for af :in aifactors :for of :across ofactors :for ix :from 0
                   :do (multiple-value-bind (this-index remainder) (floor remaining of)
                         (incf oix)
                         (unless (zerop ix) (setf afactor (+ afactor (* af this-index))))
                         (setf remaining remainder)))
             (loop :for of :across oifactors
                   :do (multiple-value-bind (this-index remainder) (floor remaining (aref ofactors oix))
                         (incf oix)
                         (setf oindex (+ oindex (* of this-index))
                               remaining remainder)))
             (setf value (if (not (functionp base-gen))
                             base-gen (funcall base-gen oindex)))
             
             (if (and (not (vads-inverse varray)) (not (shape-of (vads-argument varray))))
                 (setf value (nth-value 1 (floor value arg-gen)))
                 (let ((last-base) (element) (aindex) (component 1)
                       (this-index (floor index (aref ofactors 0))))
                   (loop :for b :from (1- (first adims)) :downto this-index
                         :do (setq last-base base
                                   aindex (+ afactor (* b (first aifactors)))
                                   base (* base (if (and scalar-arg (vads-inverse varray))
                                                    arg-gen (if (not (functionp arg-gen))
                                                                arg-gen (funcall arg-gen aindex))))
                                   component (if (zerop base) value
                                                 (nth-value 1 (floor value base)))
                                   value (- value component)
                                   element (if (zerop last-base) 0
                                               (floor component last-base))))
                   (setf value element)))
             value))))))

(defclass vader-decode (varray-derived vad-with-argument)
  nil (:metaclass va-class)
  (:documentation "A decoded array as from the [⊥ decode] function."))

(defmethod etype-of ((varray vader-decode))
  (declare (ignore varray))
  t)

(defmethod shape-of ((varray vader-decode))
  (get-promised (varray-shape varray) (append (butlast (shape-of (vads-argument varray)))
                                              (rest (shape-of (vader-base varray))))))

(defmethod generator-of ((varray vader-decode) &optional indexers params)
  (declare (ignore indexers))
  (let* ((odims (shape-of (vader-base varray)))
         (adims (shape-of (vads-argument varray)))
         (osize (size-of (vader-base varray)))
         (asize (size-of (vads-argument varray)))
         (base-gen (generator-of (vader-base varray)))
         (arg-gen (generator-of (vads-argument varray)))
         (ovector (or (first odims) 1))
         (afactors (make-array (if (and (< 1 osize) (and adims (< 1 (first (last adims)))))
                                   adims (append (butlast adims 1) (list ovector)))
                               :initial-element 1))
         (asegments (reduce #'* (butlast adims)))
         (av2 (or (if (and (< 1 osize)
                           (or (not adims)
                               (= 1 (first (last adims)))))
                      (first odims)
                      (first (last adims)))
                  1))
         (out-section (reduce #'* (rest odims))))

    (when (shape-of varray)
      (dotimes (a asegments)
        (loop :for i :from (- (* av2 (1+ a)) 2) :downto (* av2 a)
              :do (setf (row-major-aref afactors i)
                        (* (if (not (functionp arg-gen))
                               arg-gen (funcall arg-gen (if (not (and (< 1 asize)
                                                                      (< 1 osize)
                                                                      (= 1 (first (last adims)))))
                                                            (1+ i)
                                                            (floor i (or (first odims) 1)))))
                           (row-major-aref afactors (1+ i)))))))
    
    (if (shape-of varray)
        (case (getf params :base-format)
          (:encoded)
          (:linear)
          (t (lambda (i)
               (let ((result 0))
                 (loop :for index :below av2
                       :do (incf result (* (if (not (functionp base-gen))
                                               base-gen (funcall base-gen (mod (+ (mod i out-section)
                                                                                  (* out-section index))
                                                                               osize)))
                                           (row-major-aref afactors
                                                           (+ index (* av2 (floor i out-section)))))))
                 result))))
        (case (getf params :base-format)
          (:encoded)
          (:linear)
          (t (let ((result 0) (factor 1))
               (loop :for i :from (1- (if (< 1 av2) av2 ovector)) :downto 0
                     :do (incf result (* factor (render (if (not (functionp base-gen))
                                                            base-gen (funcall base-gen
                                                                              (min i (1- ovector)))))))
                         (setq factor (* factor (render (if (not (functionp arg-gen))
                                                            arg-gen (funcall arg-gen
                                                                             (min i (1- av2))))))))
               result))))))

(defclass vader-identity (vad-nested varray-derived vad-maybe-shapeless vad-reindexing vad-invertable)
  nil (:metaclass va-class)
  (:documentation "The identity of an array as from the [⊢ identity] function."))

(defmethod prototype-of ((varray vader-identity))
  (prototype-of (vader-base varray)))

(defmethod etype-of ((varray vader-identity))
  (etype-of (vader-base varray)))

(defmethod shape-of ((varray vader-identity))
  (if (vads-shapeset varray)
      (varray-shape varray)
      (let ((shape (shape-of (vader-base varray))))
        (setf (vads-shapeset varray) t
              (varray-shape varray) shape))))

(defmethod generator-of ((varray vader-identity) &optional indexers params)
  (generator-of (vader-base varray) indexers params))

(defmethod initialize-instance :after ((varray vader-identity) &key)
  "Condense successive right and left identities (like ⊢⊣3 4⍴⍳9) into a single identity with the deferred rendering metadata flag activated. Successive right identities (like ⊢⊢3 4⍴⍳9) will force rendering."
  (let ((base (vader-base varray)))
    (when (typep base 'vader-identity)
      (let ((to-defer) (to-render))
        (labels ((get-sub-base (va)
                   (if (typep (vader-base va) 'vader-identity)
                       (progn (unless (vads-inverse va)
                                (if (vads-inverse (vader-base va))
                                    (setf to-defer t)
                                    (setf to-render t)))
                              (get-sub-base (vader-base va)))
                       (vader-base va))))
          (let ((sub-base (get-sub-base varray)))
            (setf (getf (varray-meta varray) :may-defer-rendering) to-defer
                  (vader-base varray) (funcall (if to-render #'render #'identity)
                                               sub-base))))))))

(defmethod render ((varray vader-identity) &rest params)
  "A special non-rendering render method for [⊢/⊣ identity], to be used when ⊢⊣X is invoked to defer rendering."
  (if (and (getf params :may-be-deferred)
           (getf (varray-meta varray) :may-defer-rendering))
      ;; a new vader-identity instance is created without the :may-defer-rendering flag
      (make-instance 'vader-identity :base (vader-base varray))
      (call-next-method)))

(defgeneric inverse-count-to (array index-origin)
  (:documentation "Invert an [⍳ index] function, returning the right argument passed to ⍳."))

(defmethod inverse-count-to ((item t) index-origin)
  (if (and (vectorp item) (loop :for v :across item :for i :from index-origin
                                :always (and (numberp v) (= v i))))
      (length item)
      (error "Attempted to invoke inverse [⍳ index] on something other than an integer progression vector.")))

(defmethod inverse-count-to ((varray varray-derived) index-origin)
  (inverse-count-to (render varray) index-origin))

(defmethod inverse-count-to ((varray vader-identity) index-origin)
  (inverse-count-to (vader-base varray) index-origin))

(defmethod inverse-count-to ((varray vapri-arith-provec) index-origin)
  ;; TODO: this does not get invoked by for instance ⍳⍣¯1⊢⍳9 because of the identity varray
  (if (and (= 1 (vapip-repeat varray))
           (= 1 (vapip-factor varray))
           (= index-origin (vapip-origin varray)))
      (+ (- (vapip-origin varray) index-origin)
         (vapip-number varray))
      (error "Attempted to invoke inverse [⍳ index] on an altered integer progression vector.")))
