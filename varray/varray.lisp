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

(defgeneric sub-indexer-of (varray)
  (:documentation "Get a sub-indexing function for an array."))

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
      (if (and (arrayp item) (zerop (array-rank item)))
          (aplesque:make-empty-array (disclose item))
          (apl-array-prototype item))))

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
      ;; array
      ;; TODO: this causes tree printing test to fail
      (lambda (index) (row-major-aref array index))
      (if (= 0 (array-total-size array))
                (prototype-of array)
                (lambda (index)
                  (row-major-aref array index)))))

(defmethod render ((item t))
  "Rendering a non-virtual array object simply returns the object."
  item)

(defmethod render ((varray varray))
  (let ((output-shape (shape-of varray))
        (prototype (prototype-of varray))
        (indexer (indexer-of varray)))
    ;; (print (list :vv varray))
    (if output-shape
        (if (zerop (reduce #'* output-shape))
            (let* ((out-meta (if (arrayp prototype)
                                 (make-array 1 :initial-contents
                                             (list (list :empty-array-prototype
                                                         (prototype-of varray))))))
                   (output (if out-meta (make-array (shape-of varray) :displaced-to out-meta)
                               (make-array (shape-of varray) :element-type (assign-element-type
                                                                            prototype)))))
              output)
            (let ((output (make-array (shape-of varray) :element-type (etype-of varray))))
              (dotimes (i (array-total-size output))
                (let ((indexed (funcall indexer i)))
                  ;; (print (list :ind varray indexed prototype))
                  (if indexed (setf (row-major-aref output i)
                                    (funcall (if (not (typep indexed 'vad-subrendering))
                                                 #'identity #'render)
                                             indexed))
                      (setf (row-major-aref output i) prototype))))
              output))
        (funcall (if (not (typep varray 'vad-subrendering))
                     #'identity (lambda (item)
                                  (if (and (not (shape-of item))
                                           (not (or (arrayp item) (varrayp item))))
                                      item (make-array nil :initial-element (render item)))))
                 (funcall indexer 0)))))

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
;; (defmethod prototype-of ((varray varray-derived))
;;   (prototype-of (vader-base varray)))

(defmethod prototype-of ((varray varray-derived))
  (let ((shape (shape-of varray)))
    ;; (print (list :vd varray))
    (if (or (not shape) (loop :for dim :in shape :never (zerop dim)))
        (let* ((indexer (indexer-of varray))
               (indexed (if (not (functionp indexer))
                            indexer (funcall indexer 0))))
          ;; (print (list :in indexed (typep indexed 'varray) (type-of indexed)))
          (if indexed
              ;; TODO: remove-disclose when [⍴ shape] is virtually implemented
              (if (typep indexed 'varray)
                  (prototype-of indexed)
                  (aplesque::make-empty-array (disclose indexed)))
              (prototype-of (vader-base varray))))
        (prototype-of (vader-base varray)))))

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

(defclass vad-maybe-shapeless ()
  ((%determined :accessor vads-shapeset
                :initform nil
                :initarg :determined
                :documentation "Whether array's shape is determined."))
  (:documentation "Superclass of array transformations taking index origin as an implicit argument."))

(defclass vad-subrendering ()
  nil (:documentation "Superclass of derived arrays containing sub-arrays to be rendered."))

(defclass vad-invertable ()
  ((%inverse :accessor vads-inverse
             :initform nil
             :initarg :inverse
             :documentation "Parameters passed to an array transformation as an argument."))
  (:documentation "Superclass of array transformations that have an inverse variant as [↓ drop] is to [↑ take]."))

;; this is subrendering for the case of ≡↓↓2 3⍴⍳6
(defclass vader-subarray (varray-derived vad-subrendering)
  ((%prototype :accessor vasv-prototype
               :initform nil
               :initarg :prototype
               :documentation "Prototype value for subvector.")
   (%indexer :accessor vasv-indexer
             :initform nil
             :initarg :indexer
             :documentation "Indexer function for subvector."))
  (:documentation "Subvector."))

(defmethod prototype-of ((varray vader-subarray))
  (vasv-prototype varray))

(defmethod indexer-of ((varray vader-subarray))
  (vasv-indexer varray))

(defclass vader-shape (varray-derived)
  nil (:documentation "The shape of an array as from the [⍴ shape] function."))

(defmethod prototype-of ((varray vader-shape))
  (declare (ignore varray))
  0)

(defmethod etype-of ((varray vader-shape))
  (apply #'type-in-common (loop :for dimension :in (shape-of (vader-base varray))
                                :collect (assign-element-type dimension))))

(defmethod shape-of ((varray vader-shape))
  "The shape of a reshaped array is simply its argument."
  (get-or-assign-shape varray (list (length (shape-of (vader-base varray))))))

(defmethod indexer-of ((varray vader-shape))
  "Index a reshaped array."
  (let ((shape (coerce (shape-of (vader-base varray)) 'vector)))
    (lambda (index) (aref shape index))))

(defclass vader-reshape (varray-derived vad-with-argument)
  nil (:documentation "A reshaped array as from the [⍴ reshape] function."))

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
                       (let ((indexed (funcall base-indexer
                                               (if (not output-shape)
                                                   0 (mod index (max 1 input-size))))))
                         (if (not (typep indexed 'vad-subrendering))
                             indexed (render indexed)))))))))

(defclass vader-catenate (varray-derived vad-on-axis vad-with-argument vad-with-io)
  ((%laminating :accessor vacat-laminating
                :initform nil
                :initarg :laminating
                :documentation "Whether this catenation laminates arrays."))
  (:documentation "A catenated array as from the [, catenate] function."))

(defmethod etype-of ((varray vader-catenate))
  (apply #'type-in-common (loop :for array :across (vader-base varray) :collect (etype-of array))))

(defmethod shape-of ((varray vader-catenate))
  (get-or-assign-shape
   varray
   (let* ((ref-shape) (uneven)
          (each-shape (loop :for a :across (vader-base varray)
                            :collect (if (or (varrayp a) (arrayp a))
                                         (shape-of a))))
          (max-rank (reduce #'max (mapcar #'length each-shape)))
          (axis (disclose-unitary (render (vads-axis varray))))
          (axis (if (eq :last axis)
                        (max 0 (1- max-rank))
                        (- axis (vads-io varray))))
          (to-laminate (setf (vacat-laminating varray)
                             (or (typep axis 'ratio)
                                 (and (floatp axis)
                                      (< double-float-epsilon (nth-value 1 (floor axis)))))))
          (axis (setf (vads-axis varray) (ceiling axis))))

     (if to-laminate
         (loop :for shape :in each-shape
               :do (if shape (if ref-shape
                                 (if (not (and (= (length shape)
                                                  (length ref-shape))
                                               (loop :for r :in ref-shape :for a :in shape
                                                     :always (= a r))))
                                     (error "Mismatched array dimensions for laminate operation."))
                                 (setf ref-shape shape))))
         (loop :for shape :in each-shape
               :do (if (and shape (< (length ref-shape) (length shape)))
                       (if (or (not ref-shape)
                               (and (not uneven)
                                    (setf uneven (= 1 (- (length shape)
                                                         (length ref-shape))))))
                           (if (destructuring-bind (longer-dims shorter-dims)
                                   (funcall (if (>= (length ref-shape)
                                                    (length shape))
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
     (if (not ref-shape) (setf ref-shape (list (length (vader-base varray)))))
     
     (if to-laminate
         (if (zerop max-rank) ;; the ref-shape can be left as is if all elements are scalar
             ref-shape (loop :for dx :below (1+ (length ref-shape))
                             :collect (if (< dx axis) (nth dx ref-shape)
                                          (if (= dx axis) (length (vader-base varray))
                                              (nth (1- dx) ref-shape)))))
         (loop :for d :in ref-shape :for dx :from 0
               :collect (if (/= dx axis)
                            d (loop :for shape :in each-shape
                                    :summing (if (or (not shape)
                                                     (/= (length shape)
                                                         (length ref-shape)))
                                                 1 (nth axis shape)))))))))

(defmethod indexer-of ((varray vader-catenate))
  (let* ((out-shape (shape-of varray))
         (to-laminate (vacat-laminating varray))
         (axis (disclose-unitary (vads-axis varray)))
         (ofactors (get-dimensional-factors out-shape))
         (each-shape (loop :for a :across (vader-base varray)
                           :collect (if (or (varrayp a) (arrayp a))
                                        (shape-of a))))
         (increments (loop :for shape :in each-shape
                           :summing (if (or (not shape)
                                            (< (length shape) (length out-shape)))
                                        1 (nth axis shape))
                             :into total :collect total))
         (indexers (make-array (length (vader-base varray))
                               :initial-contents (loop :for a :across (vader-base varray)
                                                       :collect (indexer-of a))))
         (ifactors (make-array (length (vader-base varray))
                               :initial-contents (loop :for a :across (vader-base varray)
                                                       :collect (if (or (arrayp a) (varrayp a))
                                                                    (reverse (get-dimensional-factors
                                                                              (shape-of a))))))))
    
    (lambda (orig-index)
      (let ((remaining orig-index) (sum 0) (sub-indices) (array-index 0)
            (axis-offset (abs (- axis (1- (length (shape-of varray))))))
            (row-major-index) (source-array))
        (loop :for ofactor :in ofactors :for fx :from 0
              :do (multiple-value-bind (index remainder) (floor remaining ofactor)
                    (setf remaining remainder)
                    (push (if (/= fx axis)
                              index (loop :for i :in increments :for ix :from 0
                                          :while (>= index i)
                                          :do (progn (incf sum i) (incf array-index))
                                          :finally (return (- index sum))))
                          sub-indices)))
        
        (setf source-array (aref (vader-base varray) array-index)
              row-major-index
              (if (or (arrayp source-array)
                      (varrayp source-array))
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
        (if (not (functionp (aref indexers array-index)))
            (disclose (aref indexers array-index))
            (let ((indexed (funcall (aref indexers array-index) row-major-index)))
              (if (not (typep indexed 'vad-subrendering))
                  indexed (render indexed))))))))

(defclass vader-mix (varray-derived vad-on-axis vad-with-io)
  ((%shape-indices :accessor vamix-shape-indices
                   :initform nil
                   :initarg :shape-indices
                   :documentation "Indices of shape dimensions."))
  (:documentation "A mixed array as from the [↑ mix] function."))

(defmethod shape-of ((varray vader-mix))
  (get-or-assign-shape
   varray
   (let* ((axis (vads-axis varray))
          (base (vader-base varray))
          (base-shape (shape-of base))
          (base-indexer (indexer-of base))
          (max-rank 0) (each-shape))
     (if (not (functionp base-indexer))
         base-shape ;; handle the ↑⍬ case
         (progn
           (loop :for ix :below (reduce #'* base-shape)
                 :do (let ((member (funcall base-indexer ix)))
                       (setf max-rank (max max-rank (length (shape-of member))))
                       (push (shape-of member) each-shape)))
           
           (let ((out-shape) (shape-indices)
                 (max-shape (make-array max-rank :element-type 'fixnum :initial-element 0)))
             (loop :for shape :in each-shape
                   :do (loop :for d :in shape :for dx :from 0 :do (setf (aref max-shape dx)
                                                                        (max d (aref max-shape dx)))))

             (setf axis (setf (vads-axis varray)
                              (if (eq :last axis) (length base-shape)
                                  (ceiling (- axis (vads-io varray))))))
             
             ;; push the outer shape elements to the complete shape
             (loop :for odim :in base-shape :for ix :from 0
                   :do (if (= ix axis)
                           (loop :for ms :across max-shape :for mx :from 0
                                 :do (push ms out-shape)
                                     (push (+ mx (length base-shape)) shape-indices)))
                       (push odim out-shape)
                       (push ix shape-indices))
             
             (if (= axis (length base-shape)) ;; push the inner shape elements if for the last axis
                 (loop :for ms :across max-shape :for mx :from 0
                       :do (push ms out-shape)
                           (push (+ mx (length base-shape)) shape-indices)))

             (setf (vamix-shape-indices varray) (reverse shape-indices))
             
             ;; (print (list :si (reverse shape-indices)))
             
             (reverse out-shape)))))))

(defmethod indexer-of ((varray vader-mix))
  (let* (;; (base-shape (shape-of (vader-base varray)))
         (oshape (shape-of varray))
         (ofactors (get-dimensional-factors oshape))
         (oindexer (indexer-of (vader-base varray)))
         (dim-indices (vamix-shape-indices varray))
         (orank (length (shape-of (vader-base varray))))
         (outer-shape (loop :for i :in dim-indices :for s :in (shape-of varray)
                            :when (> orank i) :collect s))
         (inner-shape (loop :for i :in dim-indices :for s :in (shape-of varray)
                            :when (<= orank i) :collect s))
         (inner-rank (length inner-shape))
         (iofactors (get-dimensional-factors outer-shape)))
    (lambda (index)
      ;; TODO: add logic to simply return the argument if it's an array containing no nested arrays
      
      (if (not oshape)
          (if (not (functionp oindexer))
              (disclose oindexer) ;; TODO: change indexer-of for rank 0 arrays to obviate this1
              (funcall oindexer 0))
          (let ((remaining index) (row-major-index) (outer-indices) (inner-indices))

            (loop :for ofactor :in ofactors :for di :in dim-indices :for fx :from 0
                  :do (multiple-value-bind (this-index remainder) (floor remaining ofactor)
                        (setf remaining remainder)
                        (if (> orank di) (push this-index outer-indices)
                            (push this-index inner-indices))))

            (let* ((inner-indices (reverse inner-indices))
                   (oindex (loop :for i :in (reverse outer-indices)
                                 :for f :in iofactors :summing (* i f)))
                   (iarray (funcall oindexer oindex))
                   (ishape (copy-list (shape-of iarray)))
                   (iifactors (get-dimensional-factors ishape))
                   (iindexer (indexer-of iarray))
                   (irank (length ishape))
                   (doffset (- inner-rank irank))
                   (iindex 0))

              (if (not (functionp iindexer))
                  (if (zerop (reduce #'+ inner-indices)) iindexer)
                  (progn (loop :for i :in inner-indices :for ix :from 0 :while iindex
                               :do (if (< ix doffset) (if (not (zerop i))
                                                          (setf iindex nil))
                                       (if (< i (first ishape))
                                           (progn (incf iindex (* i (first iifactors)))
                                                  (setf ishape (rest ishape)
                                                        iifactors (rest iifactors)))
                                           (setf iindex nil))))
                         
                         (if iindex (funcall iindexer iindex))))))))))

(defclass vader-split (varray-derived vad-on-axis vad-with-io vad-subrendering vad-maybe-shapeless)
  nil (:documentation "A split array as from the [↓ split] function."))

(defmethod etype-of ((varray vader-split))
  "The [↓ split] function returns a nested array unless its argument is scalar."
  (let ((base (vader-base varray)))
    (if (or (arrayp base) (varrayp base))
        t (assign-element-type base))))

(defmethod shape-of ((varray vader-split))
  (get-or-assign-shape
   varray
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

(defmethod indexer-of ((varray vader-split))
  (let* ((output-shape (shape-of varray))
         (output-size (reduce #'* output-shape))
         (axis (vads-axis varray))
         (output-factors (get-dimensional-factors output-shape))
         (base-shape (shape-of (vader-base varray)))
         (base-indexer (indexer-of (vader-base varray)))
         (base-factors (get-dimensional-factors base-shape))
         (sv-factor (nth axis base-factors))
         (core-indexer (indexer-split axis (length output-shape) (coerce base-factors 'vector)
                                      (coerce output-factors 'vector)))
         (offset-indexer (lambda (offset)
                           (lambda (index) (funcall base-indexer (funcall core-indexer offset index)))))
         (subvectors
           (if (functionp base-indexer)
               (make-array output-size :initial-contents
                           (loop :for ix :below output-size
                                 :collect (let* ((sub-indexer (funcall offset-indexer ix))
                                                 (first-item (funcall sub-indexer 0))
                                                 (prototype (if (not output-shape)
                                                                (prototype-of (vader-base varray))
                                                                (if (varrayp first-item)
                                                                    (prototype-of first-item)
                                                                    (apl-array-prototype first-item)))))
                                            (make-instance
                                             'vader-subarray :base (vader-base varray)
                                                             :shape (if (nth axis base-shape)
                                                                        (list (nth axis base-shape)))
                                                             :indexer sub-indexer
                                                             :prototype prototype)))))))
    (lambda (index)
      (if (not (and subvectors (functionp base-indexer)))
          base-indexer (aref subvectors index)))))

(defclass vader-section (varray-derived vad-on-axis vad-with-argument vad-with-io vad-invertable)
  nil (:documentation "A sectioned array as from the [↑ take] or [↓ drop] functions."))

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
              (if (shape-of varray)
                  (prototype-of (vader-base varray)))
              ))))))

;; TODO: subrendering needed for vader-enclose?
(defclass vader-enclose (varray-derived vad-on-axis vad-with-io vad-with-argument 
                         vad-subrendering vad-maybe-shapeless)
  ((%inner-shape :accessor vaenc-inner-shape
                 :initform nil
                 :initarg :inner-shape
                 :documentation "Inner shape value for re-enclosed array."))
  (:documentation "An enclosed or re-enclosed array as from the [⊂ enclose] function."))

(defmethod etype-of ((varray vader-enclose))
  (let ((base (vader-base varray)))
    (if (or (arrayp base) (varrayp base)
            (vads-argument varray))
        t (assign-element-type base))))

(defmethod shape-of ((varray vader-enclose))
  (get-or-assign-shape
   varray (if (vads-shapeset varray)
              (varray-shape varray)
              (let* ((axis (setf (vads-axis varray)
                                 (if (vads-axis varray)
                                     (apply-scalar #'- (disclose (render (vads-axis varray)))
                                                   (vads-io varray)))))
                     (positions (if (vads-argument varray)
                                    (enclose-atom (vads-argument varray))))
                     (base-shape (shape-of (vader-base varray)))
                     (axis-size (if (and axis positions)
                                    (nth axis base-shape)
                                    (or (first (last base-shape)) 1)))
                     (input-offset 0) (intervals (list 0)))

                ;; (print (list :pos positions base-shape))
                
                (if positions
                    (dotimes (i (if (is-unitary positions) (or (first (last base-shape)) 1)
                                    (length positions)))
                      (let ((p (if (is-unitary positions) (disclose-unitary positions)
                                   (aref positions i))))
                        (if (zerop p) (progn (incf (first intervals))
                                             (incf input-offset))
                            (progn (setq intervals (append (loop :for i :below p :collect 0) intervals))
                                   (if (and axis-size (> axis-size input-offset))
                                       (progn (incf input-offset)
                                              (if (first intervals) (incf (first intervals))))))))))
                
                (if (and axis-size (second intervals))
                    (if (>= axis-size input-offset)
                        (incf (first intervals) (- axis-size input-offset))
                        (error "Size of partitions exceeds size of input array on axis ~w." axis)))
                
                (setf (vads-shapeset varray) (or (if positions (coerce (reverse intervals)
                                                                       'vector))
                                                 t))

                (if positions (list (1- (length intervals)))
                    (if axis (let ((base-shape (shape-of (vader-base varray)))
                                   (outer-shape) (inner-shape))
                               (loop :for d :in base-shape :for dx :from 0
                                     :do (if (if (integerp axis) (not (= dx axis))
                                                 (loop :for a :across axis :never (= a dx)))
                                             (push d outer-shape)
                                             (push d inner-shape)))
                               (setf (vaenc-inner-shape varray) (reverse inner-shape))
                               (reverse outer-shape))))))))

(defmethod indexer-of ((varray vader-enclose))
  (let* ((base-shape (shape-of (vader-base varray)))
         (output-shape (shape-of varray))
         (output-size (reduce #'* output-shape))
         (inner-shape (vaenc-inner-shape varray))
         (axis (or (vads-axis varray)
                   (max 0 (1- (length base-shape)))))
         (ofactors (get-dimensional-factors base-shape))
         (base-indexer (indexer-of (vader-base varray)))
         (outer-factors (get-dimensional-factors output-shape))
         (inner-factors (get-dimensional-factors inner-shape))
         (intervals (vads-shapeset varray))
         (offset-indexer
           (lambda (o)
             (lambda (i)
               (let ((orest o) (irest i) (index 0) (inner-dx 0) (outer-dx 0))
                 (loop :for f :in ofactors :for fx :from 0
                       :do (let ((in-outer (if (numberp axis) (/= fx axis)
                                               (loop :for a :across axis :never (= fx a)))))
                             (multiple-value-bind (factor remaining)
                                 (if in-outer (floor orest (nth outer-dx outer-factors))
                                     (floor irest (nth inner-dx inner-factors)))
                               (if in-outer (progn (incf outer-dx) (setf orest remaining))
                                   (progn (incf inner-dx) (setf irest remaining)))
                               (incf index (* f factor)))))
                 (funcall base-indexer index)))))
         (each-offset (if (vectorp intervals)
                          (coerce (loop :for i :across intervals :summing i :into s :collect s)
                                  'vector)))
         (section-size (if (vectorp intervals)
                           (reduce #'* (loop :for d :in base-shape :for dx :from 0
                                             :when (> dx axis) :collect d))))
         (iseg (if section-size (if (< 1 section-size)
                                    section-size (or (nth axis base-shape) 1))))
         (last-indim (first (last base-shape)))
         (partition-indexer
           (lambda (o shape)
             (let ((input-offset (aref each-offset o))
                   (last-idim (or (first (last shape)) 1))
                   (this-size (reduce #'* shape)))
               (lambda (i)
                 ;; (print (list :i i section-size this-size iseg last-idim input-offset))
                 (if (not (zerop this-size))
                     (let ((oseg (floor i iseg)) (ivix (mod i iseg)))
                       ;; (print (list :oos oseg ivix))
                       (if (not (functionp base-indexer))
                           base-indexer (funcall base-indexer
                                                 (if (= 1 section-size)
                                                     (+ input-offset (mod i last-idim)
                                                        (* last-indim (floor i last-idim)))
                                                     (+ ivix (* iseg (+ input-offset oseg)))))))))))))

    ;;(print (list :eo each-offset intervals section-size output-shape))
    
    (lambda (index)
      (if (vectorp intervals)
          (if (< 0 output-size)
              (let* ((this-shape (loop :for dim :in base-shape :for dx :from 0
                                       :collect (if (/= dx axis)
                                                    dim (aref intervals (1+ index)))))
                     (sub-indexer (funcall partition-indexer index this-shape))
                     (first-item (funcall sub-indexer 0))
                     (prototype (if (varrayp first-item)
                                    (prototype-of first-item)
                                    (apl-array-prototype first-item))))
                ;; (print (list :tt index this-shape))
                (make-instance 'vader-subarray :prototype prototype :indexer sub-indexer
                                               :shape (or this-shape '(1)) :base (vader-base varray)))
              (make-instance 'vader-subarray :prototype (prototype-of (vader-base varray))
                                             :shape output-shape :base (vader-base varray)))
          (if (not inner-shape)
              (if (vads-axis varray)
                  (if (not (functionp base-indexer))
                                         base-indexer (funcall base-indexer index))
                   (vader-base varray))
              (if (not (functionp base-indexer))
                  base-indexer (let* ((sub-indexer (funcall offset-indexer index))
                                      (first-item (funcall sub-indexer 0))
                                      (prototype (if (not output-shape)
                                                     (aplesque::make-empty-array
                                                      (vader-base varray))
                                                     (if (varrayp first-item)
                                                         (prototype-of first-item)
                                                         (apl-array-prototype first-item)))))
                                 (make-instance 'vader-subarray
                                                :prototype prototype
                                                :base (vader-base varray)
                                                :shape inner-shape :indexer sub-indexer))))))))

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
                                        i))
                                  (disclose-unitary (render (vads-argument varray))))))
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
                              (loop :for degree :across degrees :when (< 0 degree)
                                    :counting degree :into dcount :finally (return dcount)))
                         degrees)
                     (nth axis base-shape)))
            (error "Attempting to expand elements across array but ~a"
                   "positive degrees are not equal to length of selected input axis."))
           (t (let ((ex-dim))
                (if (not (arrayp degrees))
                    (setf ex-dim (* (abs degrees) (or (nth axis base-shape) 1)))
                    (loop :for degree :across degrees :for dx :from 0
                          :summing (max (abs degree) (if is-inverse 0 1)) :into this-dim
                          :finally (setf ex-dim this-dim)))
                (loop :for dim :in (or base-shape '(1)) :for index :from 0
                      :collect (if (/= index axis) dim (* 1 ex-dim)))))))))

(defmethod indexer-of ((varray vader-expand))
  (let* ((arg-vector (if (typep (vads-argument varray) 'sequence)
                         (coerce (vads-argument varray) 'vector)
                         (vads-argument varray)))
         (base-indexer (indexer-of (vader-base varray)))
         (indexer (if (or (integerp arg-vector)
                          (< 0 (length arg-vector)))
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
                   (if (arrayp argument)
                       argument))))

(defmethod indexer-of ((varray vader-turn))
  "Indexer for a rotated or flipped array."
  (let* ((base-indexer (indexer-of (vader-base varray)))
         (indexer (if (and (functionp base-indexer))
                      (indexer-turn (if (eq :last (vads-axis varray))
                                        (1- (length (shape-of varray)))
                                        (- (vads-axis varray) (vads-io varray)))
                                    (shape-of varray)
                                    (arg-process (vads-argument varray))))))
    (if (zerop (reduce #'+ (shape-of (vader-base varray))))
        (lambda (_)
          (declare (ignore _))
          (vader-base varray)) ;; handle the case of ⌽⍬
        (lambda (index)
          (if (not indexer)
              base-indexer (funcall base-indexer (funcall indexer index)))))))

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
         (argument (render (vads-argument varray)))
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
