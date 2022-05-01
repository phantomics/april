;;;; varray.lisp

(in-package #:varray)

(defclass varray ()
  ((%shape :accessor varray-shape
           :initform nil
           :initarg :shape
           :documentation "The array's shape - typically populated by a (shape-of) method."))
  (:documentation "Virtual array - the ancestor class for all virtual array objects."))

(defun varrayp (item)
  (typep item 'varray))

(defgeneric etype-of (varray)
  (:documentation "Get the element type of an array."))

(defgeneric prototype-of (varray)
  (:documentation "Get the prototype of an array."))

(defgeneric shape-of (varray)
  (:documentation "Get the shape of an array."))

(defgeneric rank-of (varray)
  (:documentation "Get the rank of an array."))

(defgeneric indexer-of (varray)
  (:documentation "Get an indexing function for an array."))

(defgeneric render (varray)
  (:documentation "Render an array into memory."))

(defmethod prototype-of ((item t))
  "The prototype representation of an item is returned by the (apl-array-prototype) function."
  (if (and (arrayp item)
           (array-displacement item)
           (vectorp (array-displacement item))
           (listp (aref (array-displacement item) 0))
           (member :empty-array-prototype (aref (array-displacement item) 0)))
      ;; if an empty array prototype has been stored, retrieve it
      (getf (aref (array-displacement item) 0) :empty-array-prototype)
      (apl-array-prototype item)))

(defmethod prototype-of ((varray varray))
  "The default prototype for a virtual array is 0."
  0)

(defmethod etype-of ((item t))
  "A literal array's element type is returned by the (array-element-type) function."
  (assign-element-type item))

(defmethod etype-of ((array array))
  "A literal array's element type is returned by the (array-element-type) function."
  (array-element-type array))

(defmethod etype-of ((varray varray))
  "The default element type for a virtual array is T."
  't)

(defmethod shape-of ((_ t))
  "Non-arrays have a nil shape."
  (declare (ignore _))
  nil)

(defmethod shape-of ((array array))
  "Literal array shapes are given by (array-dimensions)."
  (array-dimensions array))

(defmethod shape-of ((varray varray))
  "Virtual array shapes are referenced using the (varray-shape) method."
  (varray-shape varray))

(defmethod rank-of ((item t))
  "Non-arrays have a rank of 0."
  (declare (ignore item))
  0)

(defmethod rank-of ((array array))
  "Literal array ranks are given by (array-rank)."
  (array-rank array))

(defmethod rank-of ((varray varray))
  "A virtual array's rank is the length of its shape."
  (length (shape-of varray)))

(defmethod indexer-of ((item t))
  "The indexer for a non-array is its identity."
  item)

(defmethod indexer-of ((array array))
  (if (= 0 (array-rank array))
      array (if (= 0 (array-total-size array))
                (prototype-of array)
                (lambda (index)
                  (row-major-aref array index)))))

(defmethod render ((item t))
  item)

(defmethod render ((varray varray))
  ;; (print :abc)
  ;; (print (list :ss (shape-of varray)
  ;;              (prototype-of varray)))
  (let ((output-shape (shape-of varray))
        (prototype (prototype-of varray))
        (indexer (indexer-of varray)))
    (if output-shape
        (if (zerop (reduce #'* output-shape))
            (let ((this-prototype (prototype-of varray)))
              (let* ((out-meta (if (arrayp this-prototype)
                                   (make-array 1 :initial-contents
                                               (list (list :empty-array-prototype
                                                           (prototype-of varray))))))
                     (output (if out-meta (make-array (shape-of varray) :displaced-to out-meta)
                                 (make-array (shape-of varray) :element-type (assign-element-type
                                                                              this-prototype)))))
                output))
            (let ((output (make-array (shape-of varray) :element-type (etype-of varray))))
              ;; (print (list :out output (etype-of varray)))
              (dotimes (i (array-total-size output))
                (let ((indexed (funcall indexer i)))
                  (if indexed (setf (row-major-aref output i) indexed)
                      (setf (row-major-aref output i) prototype))))
              output))
        (funcall indexer 1))))

(defmacro get-or-assign-shape (object form)
  `(or (varray-shape ,object) (setf (varray-shape ,object) ,form)))

(defclass varray-primal (varray) nil
  (:documentation "A primal array: a virtual array defined wholly by its parameters, not derived from another array."))

(defclass varray-derived (varray)
  ((%base :accessor vader-base
          :initform nil
          :initarg :base
          :documentation "The array from which the array is derived."))
  (:documentation "A derived array: virtual array derived from another array."))

;; the default shape of a derived array is the same as its base array
(defmethod etype-of ((varray varray-derived))
  (if (arrayp (vader-base varray))
      (array-element-type (vader-base varray))
      (etype-of (vader-base varray))))

;; the default shape of a derived array is the same as its base array
(defmethod prototype-of ((varray varray-derived))
  (if (varrayp (vader-base varray))
      ;; (apl-array-prototype (funcall (indexer-of (vader-base varray)) 0))
      (prototype-of (vader-base varray))
      (prototype-of (vader-base varray))))

(defmethod shape-of ((varray varray-derived))
  "The default shape of a derived array is the same as the original array."
  (get-or-assign-shape varray (shape-of (vader-base varray))))

(defclass vvector-integer-progression (varray-primal)
  ((%number :accessor vvip-number
            :initform 1
            :initarg :number
            :documentation "The number of values.")
   (%origin :accessor vvip-origin
            :initform 0
            :initarg :origin
            :documentation "The origin point - by default, the index origin.")
   (%factor :accessor vvip-factor
            :initform 1
            :initarg :factor
            :documentation "Factor of values.")
   (%repeat :accessor vvip-repeat
            :initform 1
            :initarg :repeat
            :documentation "Instances of each value."))
  (:documentation "Integer progression vector - a series of numeric values generated by [⍳ index]."))

(defmethod etype-of ((vvector vvector-integer-progression))
  (if (floatp (vvip-factor vvector))
      'double-float (list 'integer (min 0 (vvip-origin vvector))
                          (+ (vvip-origin vvector)
                             (first (shape-of vvector))))))

(defmethod prototype-of ((vvector vvector-integer-progression))
  (declare (ignore vvector))
  0)

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

;; superclasses encompassing array derivations taking different types of parameters

(defclass vad-on-axis ()
  ((%axis :accessor vads-axis
          :initform :last
          :initarg :axis
          :documentation "The axis along which to transform."))
  (:documentation "Superclass of array transformations occuring along an axis."))

(defclass vad-with-argument ()
  ((%argument :accessor vads-argument
              :initform nil
              :initarg :argument
              :documentation "Parameters passed to an array transformation as an argument."))
  (:documentation "Superclass of array transformations occuring along an axis."))

(defclass vad-with-io ()
  ((%index-origin :accessor vads-io
                  :initform 0
                  :initarg :index-origin
                  :documentation "Parameter specifying the index origin for an array operation."))
  (:documentation "Superclass of array transformations taking index origin as an implicit argument."))

(defclass vad-invertable ()
  ((%inverse :accessor vads-inverse
             :initform nil
             :initarg :inverse
             :documentation "Parameters passed to an array transformation as an argument."))
  (:documentation "Superclass of array transformations that have an inverse variant as [↓ drop] is to [↑ take]."))
(defclass vader-shape (varray-derived)
  nil (:documentation "The shape of an array as from the [⍴ shape] function."))

(defmethod prototype-of ((varray vader-shape))
  (declare (ignore varray))
  0)

(defmethod etype-of ((varray vader-shape))
  (let ((shape (shape-of (vader-base varray)))
        (type))
    (apply #'type-in-common (loop :for dimension :in shape :collect (assign-element-type dimension)))))

(defmethod shape-of ((varray vader-shape))
  "The shape of a reshaped array is simply its argument."
  (get-or-assign-shape varray (list (length (shape-of (vader-base varray))))))

(defmethod indexer-of ((varray vader-shape))
  "Index a reshaped array."
  (let ((shape (coerce (shape-of (vader-base varray)) 'vector)))
    (lambda (index) (aref shape index))))

(defclass vader-reshape (varray-derived vad-with-argument)
  nil (:documentation "A reshaped array as from the [⍴ reshape] function."))

(defmethod prototype-of ((varray vader-reshape))
  (shape-of (vader-base varray)) ;; must get shape so that base array can be rendered
  (let ((indexer (indexer-of (vader-base varray))))
    ;; TODO: remove-disclose when [⍴ shape] is virtually implemented
    (aplesque::make-empty-array (disclose (if (not (functionp indexer))
                                              indexer ;; ←← remove
                                              (funcall (indexer-of (vader-base varray)) 0))))))

(defmethod shape-of ((varray vader-reshape))
  "The shape of a reshaped array is simply its argument."
  (get-or-assign-shape
   varray (let ((arg (setf (vads-argument varray)
                           (render (vads-argument varray)))))
            (if (typep arg 'sequence)
                (coerce arg 'list)
                (list arg)))))

(defmethod indexer-of ((varray vader-reshape))
  "Index a reshaped array."
  (let* ((input-size (reduce #'* (shape-of (vader-base varray))))
         (output-shape (shape-of varray))
         (output-size (reduce #'* output-shape))
         (base-indexer (indexer-of (vader-base varray))))
    (lambda (index)
      (if (zerop output-size)
          (prototype-of varray)
          (if (not (functionp base-indexer))
              (funcall (if (not (and (arrayp (vads-argument varray))
                                     (zerop (array-total-size (vads-argument varray)))))
                           #'disclose #'identity)
                       base-indexer)
              (funcall (if (not (and (arrayp (vads-argument varray))
                                     (zerop (array-total-size (vads-argument varray)))))
                           #'identity #'enclose)
                       (funcall base-indexer (if (not output-shape)
                                                 0 (mod index (max 1 input-size))))))))))
  
(defclass vader-section (varray-derived vad-on-axis vad-with-argument vad-with-io vad-invertable)
  nil (:documentation "A sectioned array as from the [↑ take] or [↓ drop] functions."))

(defmethod prototype-of ((varray vader-section))
  (let ((indexer (indexer-of (vader-base varray))))
    ;; TODO: remove-disclose when [⍴ shape] is virtually implemented
    (aplesque::make-empty-array (disclose (if (not (functionp indexer))
                                              indexer ;; ←← remove
                                              (funcall (indexer-of (vader-base varray)) 0))))))

(defmethod shape-of ((varray vader-section))
  "The shape of a sectioned array is the parameters (if not inverse, as for [↑ take]) or the difference between the parameters and the shape of the original array (if inverse, as for [↓ drop])."
  (get-or-assign-shape
   varray
   (let* ((arg-shape (shape-of (vads-argument varray)))
          (arg-indexer (indexer-of (setf (vads-argument varray)
                                         (render (vads-argument varray)))))
          (base-shape (copy-list (shape-of (vader-base varray))))
          (is-inverse (vads-inverse varray))
          (iorigin (vads-io varray))
          (axis (vads-axis varray))
          (pre-shape (loop :for b :below (max (length base-shape)
                                              (if (not arg-shape) 1 (first arg-shape)))
                           :collect (or (nth b base-shape) 1))))

     (if (vectorp axis)
         (loop :for x :across axis :for ix :from 0
               :do (setf (nth (- x iorigin) pre-shape)
                         (if is-inverse (- (nth (- x iorigin) pre-shape)
                                           (abs (funcall arg-indexer ix)))
                             (abs (funcall arg-indexer ix)))))
         (if (eq :last axis)
             (if (functionp arg-indexer)
                 (loop :for a :below (first arg-shape) :for ix :from 0
                       :do (setf (nth ix pre-shape)
                                 (if is-inverse (max 0 (- (nth ix pre-shape)
                                                          (abs (funcall arg-indexer a))))
                                     (abs (funcall arg-indexer a)))))
                 (setf (first pre-shape)
                       (if is-inverse (max 0 (- (first pre-shape) (abs arg-indexer)))
                           (abs arg-indexer))))
             (setf (nth (- axis iorigin) pre-shape)
                   (if is-inverse (max 0 (- (nth (- axis iorigin) pre-shape)
                                            (abs arg-indexer)))
                       (abs arg-indexer)))))
     
     pre-shape)))

(defmethod indexer-of ((varray vader-section))
  "Indexer for a sectioned array."
  (let* ((base-indexer (indexer-of (vader-base varray)))
         (iorigin (vads-io varray))
         (axis (vads-axis varray))
         (is-inverse (vads-inverse varray))
         (out-dims (if is-inverse (make-array (length (shape-of varray))
                                              :initial-element 0)
                       (coerce (shape-of varray) 'vector)))
         (arg-shape (shape-of (vads-argument varray)))
         (arg-indexer (indexer-of (vads-argument varray))))

    (if (vectorp axis)
        (loop :for x :across axis :for ix :from 0
              :do (setf (aref out-dims (- x iorigin))
                        (funcall arg-indexer ix)))
        (if (eq :last axis)
            (if (functionp arg-indexer)
                (loop :for a :below (first arg-shape) :for ix :from 0
                      :do (setf (aref out-dims ix)
                                (funcall arg-indexer a)))
                (setf (aref out-dims 0) arg-indexer))
            (setf (aref out-dims (- axis iorigin))
                  arg-indexer)))

    (let ((indexer (indexer-section (vads-inverse varray)
                                    (or (shape-of (vader-base varray)) '(1))
                                    out-dims nil)))
      (lambda (index)
        (let ((indexed (funcall indexer index)))
          (if indexed (if (not (functionp base-indexer))
                          (disclose base-indexer) ;; TODO: why is this disclose needed?
                          (funcall base-indexer indexed))
              (prototype-of (vader-base varray))))))))

(defclass vader-expand (varray-derived vad-on-axis vad-with-io vad-with-argument vad-invertable)
  nil (:documentation "An expanded (as from [\ expand]) or compressed (as from [/ compress]) array."))

(defmethod shape-of ((varray vader-expand))
  "The shape of an expanded or compressed array."
  (get-or-assign-shape
   varray
   (let* ((degrees-count (first (shape-of (vads-argument varray))))
          (degrees (setf (vads-argument varray)
                         (funcall (lambda (i)
                                    (if (arrayp i)
                                        (if (< 0 (array-rank i))
                                            i (vector (aref i)))
                                        (vector i)))
                                  (render (vads-argument varray)))))
          (base-shape (copy-list (shape-of (vader-base varray))))
          (base-rank (length base-shape))
          (is-inverse (vads-inverse varray))
          (axis (setf (vads-axis varray)
                      (max 0 (if (eq :last (vads-axis varray))
                                 (1- base-rank)
                                 (- (vads-axis varray)
                                    (vads-io varray)))))))
     
     (cond ((and base-shape (zerop (reduce #'* base-shape)))
            (if is-inverse
                (if (> axis (1- base-rank))
                    (error "This array does not have an axis ~a." axis)
                    (if (or (not (arrayp degrees))
                            (or (not degrees-count)
                                (= 1 degrees-count)))
                        (loop :for d :in base-shape :for dx :from 0
                              :collect (if (= dx axis) (* d (disclose-unitary degrees)) d))
                        (if (not degrees-count)
                            (loop :for d :below base-rank :for dx :from 0
                                  :collect (if (= dx axis) 0 d))
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
                          (zerop (aref degrees 0)))))
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
                              (loop :for degree :across degrees :when (< 0 degree)
                                    :counting degree :into dcount :finally (return dcount)))
                         degrees)
                     (nth axis base-shape)))
            (error "Attempting to expand elements across array but ~a"
                   "positive degrees are not equal to length of selected input axis."))
           (t (let* ((degrees (if (and (arrayp degrees)
                                      (not (= 1 (length degrees))))
                                 degrees (setf (vads-argument varray)
                                               (make-array (or (nth axis base-shape) 1)
                                                           :element-type 'fixnum
                                                           :initial-element (disclose-unitary degrees)))))
                    (c-degrees (make-array (length degrees)
                                           :element-type 'fixnum :initial-element 0))
                    (ex-dim))
                (loop :for degree :across degrees :for dx :from 0
                      :summing (max (abs degree) (if is-inverse 0 1)) :into this-dim
                      :do (setf (aref c-degrees dx) this-dim)
                      :finally (setq ex-dim this-dim))
                (loop :for dim :in (or base-shape '(1)) :for index :from 0
                      :collect (if (/= index axis) dim (* 1 ex-dim)))))))))

(defmethod indexer-of ((varray vader-expand))
  (let* ((arg-vector (coerce (vads-argument varray) 'vector))
         (base-indexer (indexer-of (vader-base varray)))
         (indexer (if (< 0 (length arg-vector))
                      (indexer-expand arg-vector (shape-of (vader-base varray))
                                      (vads-axis varray)
                                      (vads-inverse varray)))))
    (lambda (index)
      (if (not (functionp base-indexer))
          (if (funcall indexer index)
              (disclose base-indexer))
          (let ((indexed (funcall indexer index)))
            (if indexed (funcall base-indexer indexed)))))))

(defclass vader-turn (varray-derived vad-on-axis vad-with-io vad-with-argument)
  nil (:documentation "A rotated array as from the [⌽ rotate] function."))

(defun arg-process (argument)
  (if (or (listp argument) (numberp argument))
      argument (if (varrayp argument)
                   (render argument) ;; TODO: eliminate forced render here
                   
                   ;(if (vectorp argument)
                   ;    (coerce argument 'list)
                   (if (arrayp argument)
                       argument))))

(defmethod indexer-of ((varray vader-turn))
  "Indexer for a rotated or flipped array."
  (let* ((base-indexer (indexer-of (vader-base varray)))
         (indexer (if (functionp base-indexer)
                      (indexer-turn (if (eq :last (vads-axis varray))
                                        (1- (length (shape-of varray)))
                                        (- (vads-axis varray) (vads-io varray)))
                                    (shape-of varray)
                                    (arg-process (vads-argument varray))))))
    (lambda (index)
      (if (not indexer)
          base-indexer (funcall base-indexer (funcall indexer index))))))

(defclass vader-permute (varray-derived vad-with-io vad-with-argument)
  ((%is-diagonal :accessor vaperm-is-diagonal
                 :initform nil
                 :initarg :is-diagonal
                 :documentation "Whether this permutation is diagonal."))
  (:documentation "A permuted array as from the [⍉ permute] function."))

(defmethod shape-of ((varray vader-permute))
  "The shape of a permuted array."
  ;; (print (list :vv (shape-of (vads-argument varray))))
  (get-or-assign-shape
   varray
   (let* ((base-shape (shape-of (vader-base varray)))
          (argument (setf (vads-argument varray)
                          (render (vads-argument varray))))
          (arg (if argument
                   (if (vectorp argument)
                       (coerce (loop :for a :across argument :collect (max 0 (- a (vads-io varray))))
                               'vector)
                       (- argument (vads-io varray)))))
          (base-rank (length base-shape))
          (odims) (positions) (diagonals))
     ;; (print (list :ee odims argument (vader-base varray) (varray-shape varray)))
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
                              (if (not (member i positions))
                                  (push i positions))
                              ;; collect possible diagonal indices into diagonal list
                              (if (assoc i diagonals)
                                  (push ix (rest (assoc i diagonals)))
                                  (push (list i ix) diagonals)))
                    (setf odims base-shape
                          positions (cons arg positions)))
                (setf (vaperm-is-diagonal varray)
                      (= base-rank (length positions)))
                (remove nil odims))))))

(defmethod indexer-of ((varray vader-permute))
  "Indexer for a rotated or flipped array."
  (let* ((base-indexer (indexer-of (vader-base varray)))
         (argument (vads-argument varray))
         (indexer (if (functionp base-indexer)
                      (indexer-permute (shape-of (vader-base varray))
                                       (shape-of varray)
                                       ;; (vads-argument varray)
                                       (if argument
                                           (if (vectorp argument)
                                               (coerce (loop :for a :across argument
                                                             :collect (max 0 (- a (vads-io varray))))
                                                       'vector)
                                               (- argument (vads-io varray))))
                                       (not (or (not (vads-argument varray))
                                                (vaperm-is-diagonal varray)))))))
    (lambda (index)
      (if (not indexer)
          base-indexer (funcall base-indexer (funcall indexer index))))))


;; (1 2 3) (2 3 4)∘.⌽[1]⊂3 3⍴⍳9 NOT IN DYALOG?
