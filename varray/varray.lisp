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

(defun subrendering-p (item)
  (and (typep item 'varray-derived)
       (vads-subrendering item)))

(defgeneric etype-of (varray)
  (:documentation "Get the element type of an array."))

(defgeneric prototype-of (varray)
  (:documentation "Get the prototype of an array."))

(defgeneric shape-of (varray)
  (:documentation "Get the shape of an array."))

(defgeneric size-of (varray)
  (:documentation "Get the size of an array."))

(defgeneric rank-of (varray)
  (:documentation "Get the rank of an array."))

(defgeneric indexer-of (varray &optional params)
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
  
(defmethod size-of ((item t))
  "Virtual array shapes are referenced using the (varray-shape) method."
  (if (not (arrayp item))
      1 (array-total-size item)))

(defmethod size-of ((varray varray))
  "Virtual array shapes are referenced using the (varray-shape) method."
  (reduce #'* (shape-of varray)))

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

(defmethod indexer-of ((item t) &optional params)
  "The indexer for a non-array is its identity."
  item)

(defmethod indexer-of ((array array) &optional params)
  (if (= 0 (array-rank array))
      ;; array
      ;; TODO: this causes tree printing test to fail
      (lambda (index)
        (if (< index (array-total-size array))
            (row-major-aref array index)))
      (if (= 0 (array-total-size array))
          (prototype-of array)
          ;; TODO: why does wrapping this in a (lambda) cause problems? like array-lib's (0↑⊂,⊂⍬) from 99
          (lambda (index)
            (if (< index (array-total-size array))
                (row-major-aref array index))))))

(defmethod render ((item t))
  "Rendering a non-virtual array object simply returns the object."
  item)

(defun subrendering-base (item)
  (if (typep item 'varray-derived)
      (or (subrendering-p (vader-base item))
          (subrendering-base (vader-base item)))))

(defmethod render ((varray varray))
  (let ((output-shape (shape-of varray))
        (prototype (prototype-of varray))
        (indexer (indexer-of varray))
        (to-subrender (or (subrendering-p varray)
                          (subrendering-base varray))))
    ;; (print (list :vv varray prototype
    ;;              (etype-of varray)
    ;;              (if (typep varray 'varray-derived)
    ;;                  (subrendering-p (vader-base varray)))))
    (if output-shape
        (if (zerop (reduce #'* output-shape))
            (let* ((out-meta (if (arrayp prototype)
                                 (make-array 1 :initial-contents
                                             (list (list :empty-array-prototype
                                                         (prototype-of varray))))))
                   (output (if out-meta (make-array (shape-of varray) :displaced-to out-meta)
                               (make-array (shape-of varray) :element-type ;; (assign-element-type
                                                                           ;;  prototype)
                                           (etype-of varray)))))
              output)
            (let ((output (make-array (shape-of varray) :element-type (etype-of varray))))
              (dotimes (i (array-total-size output))
              ;; (xdotimes output (i (array-total-size output))
                (let ((indexed (funcall indexer i)))
                  ;; (print (list :abc varray (shape-of varray) indexed prototype output))
                  (if indexed (setf (row-major-aref output i)
                                    (funcall (if (and (not to-subrender)
                                                      (not (subrendering-p indexed)))
                                                 #'identity #'render)
                                             indexed))
                      (setf (row-major-aref output i) prototype))))
              output))
        (funcall (if (not (subrendering-p varray))
                     #'render (lambda (item)
                                (let ((rendered (render item)))
                                  (if (not (arrayp rendered))
                                      rendered (make-array nil :initial-element rendered)))))
                 (if (not (functionp indexer))
                     indexer (funcall indexer 0))))))

(defmacro get-or-assign-shape (object form)
  `(or (varray-shape ,object) (setf (varray-shape ,object) ,form)))
  
(defclass varray-primal (varray) nil
  (:documentation "A primal array: a virtual array defined wholly by its parameters, not derived from another array."))

(defclass varray-derived (varray)
  ((%base :accessor vader-base
          :initform nil
          :initarg :base
          :documentation "The array from which the array is derived.")
   (%subrendering :accessor vads-subrendering
                  :initform nil
                  :initarg :subrendering
                  :documentation "Whether the array contains nested elements to be subrendered."))
  (:documentation "A derived array: virtual array derived from another array."))

;; the default shape of a derived array is the same as its base array
(defmethod etype-of ((varray varray-derived))
  (if (varrayp (vader-base varray))
      (etype-of (vader-base varray))
      (if (arrayp (vader-base varray))
          (array-element-type (vader-base varray))
          (assign-element-type (vader-base varray)))))

(defmethod prototype-of ((varray varray-derived))
  (let ((shape (shape-of varray)))
    ;; (print (list :vd varray (vader-base varray)
    ;;              (subrendering-p varray)
    ;;              (subrendering-p (vader-base varray))))
    (if (or (not shape) (loop :for dim :in shape :never (zerop dim)))
        (if (and (not (or (typep varray 'vader-mix)
                          ;; (typep varray 'vader-section)
                          (typep varray 'vader-expand)
                          (typep varray 'vader-catenate)))
                 ;; TODO: functions that combine an array of arguments shouldn't have base subrendering
                 ;; checked. Is there a better way to establish this rule?
                 (subrendering-p (vader-base varray)))
            (aplesque::make-empty-array (disclose (render (vader-base varray))))
            (if (subrendering-p varray)
                (aplesque::make-empty-array (disclose (render (vader-base varray))))
                (let* ((indexer (indexer-of varray))
                       (indexed (if (not (functionp indexer))
                                    indexer (funcall indexer 0))))
                  ;; (print (list :aaa indexed varray))
                  ;; (print (list :in indexed (typep indexed 'varray) (type-of indexed)
                  ;;              (if (varrayp indexed) (vader-base indexed))))
                  ;; TODO: remove-disclose when [⍴ shape] is virtually implemented
                  (if indexed (if (typep indexed 'varray)
                                  (prototype-of indexed)
                                  (aplesque::make-empty-array (disclose indexed)))
                      (prototype-of (vader-base varray))))))
        (prototype-of (vader-base varray)))))

(defmethod shape-of ((varray varray-derived))
  "The default shape of a derived array is the same as the original array."
  (get-or-assign-shape varray (shape-of (vader-base varray))))

(defmethod base-indexer-of ((varray varray-derived))
  "The default shape of a derived array is the same as the original array."
  (let ((this-shape (shape-of varray)))
    (indexer-of (vader-base varray)
                (list :shape-deriving (if (typep varray 'vader-reshape)
                                          (shape-of varray)
                                          (if (and (typep varray 'vader-section)
                                                   (= 1 (size-of (vads-argument varray))))
                                              (funcall (lambda (item)
                                                         (if (typep item 'sequence)
                                                             (coerce item 'list)
                                                             (list item)))
                                                       (render (vads-argument varray)))))))))

(defclass vapri-integer-progression (varray-primal)
  ((%number :accessor vapip-number
            :initform 1
            :initarg :number
            :documentation "The number of values.")
   (%origin :accessor vapip-origin
            :initform 0
            :initarg :origin
            :documentation "The origin point - by default, the index origin.")
   (%factor :accessor vapip-factor
            :initform 1
            :initarg :factor
            :documentation "Factor of values.")
   (%repeat :accessor vapip-repeat
            :initform 1
            :initarg :repeat
            :documentation "Instances of each value."))
  (:documentation "Integer progression vector - a series of numeric values generated by [⍳ index]."))

(defmethod etype-of ((vvector vapri-integer-progression))
  (if (floatp (vapip-factor vvector))
      'double-float (list 'integer (min 0 (vapip-origin vvector))
                          (+ (vapip-origin vvector)
                             (first (shape-of vvector))))))

(defmethod prototype-of ((vvector vapri-integer-progression))
  (declare (ignore vvector))
  0)

;; the shape of an IP vector is its number times its repetition
(defmethod shape-of ((vvector vapri-integer-progression))
  (get-or-assign-shape vvector (list (* (vapip-number vvector)
                                        (vapip-repeat vvector)))))

;; the IP vector's parameters are used to index its contents
(defmethod indexer-of ((vvector vapri-integer-progression) &optional params)
  (lambda (index)
    (if (< -1 index (vapip-number vvector))
        (* (+ (floor index (vapip-repeat vvector))
              (vapip-origin vvector))
           (vapip-factor vvector)))))

(defclass vapri-coordinate-identity (varray-primal)
  ((%shape :accessor vapci-shape
           :initform 1
           :initarg :number
           :documentation "The shape of the array.")
   (%origin :accessor vapci-origin
            :initform 0
            :initarg :origin
            :documentation "The origin point - by default, the index origin."))
  (:documentation "Coordinate identity array - an array of coordinate vectors generated by [⍳ index]."))

(defmethod etype-of ((varray vapri-coordinate-identity))
  "Being a nested array, the type is always t."
  (declare (ignore varray))
  t)

(defmethod prototype-of ((varray vapri-coordinate-identity))
  "Prototype is an array of zeroes with length equal to the array's rank."
  (make-array (length (vapci-shape varray)) :element-type 'bit :initial-element 0))

(defmethod shape-of ((varray vapri-coordinate-identity))
  "Shape is explicit."
  (vapci-shape varray))

(defmethod indexer-of ((varray vapri-coordinate-identity) &optional params)
  "Each index returns a coordinate vector."
  (let ((maximum (reduce #'max (vapci-shape varray)))
        (factors (get-dimensional-factors (vapci-shape varray) t)))
    (lambda (index)
      (let ((remaining index)
            (output (make-array (length factors)
                                :initial-element 0 :element-type (list 'integer 0 maximum))))
        (loop :for f :across factors :for ix :from 0
              :do (multiple-value-bind (item remainder) (floor remaining f)
                    (setf (aref output ix) (+ item (vapci-origin varray))
                          remaining remainder)))
        output))))
  
;; superclasses encompassing array derivations taking different types of parameters

(defclass vad-on-axis ()
  ((%axis :accessor vads-axis
          :initform nil
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

(defclass vad-with-ct ()
  ((%comp-tolerance :accessor vads-ct
                    :initform 0
                    :initarg :comparison-tolerance
                    :documentation "Parameter specifying the comparison tolerance for an array operation."))
  (:documentation "Superclass of array transformations taking comparison tolerance as an implicit argument."))

(defclass vad-with-rng ()
  ((%rng :accessor vads-rng
         :initform nil
         :initarg :rng
         :documentation "Random number generator specification for array transformations that use it."))
  (:documentation "Superclass of array transformations occuring along an axis."))

(defclass vad-maybe-shapeless ()
  ((%determined :accessor vads-shapeset
                :initform nil
                :initarg :determined
                :documentation "Whether array's shape is determined."))
  (:documentation "Superclass of array transformations taking index origin as an implicit argument."))

(defclass vad-subrendering ()
  ((%subrendering :accessor vads-subrendering
                  :initform t
                  :initarg :subrendering
                  :documentation "Whether the array contains nested elements to be subrendered."))
  (:documentation "Superclass of derived arrays containing sub-arrays to be rendered."))

(defclass vad-invertable ()
  ((%inverse :accessor vads-inverse
             :initform nil
             :initarg :inverse
             :documentation "Parameters passed to an array transformation as an argument."))
  (:documentation "Superclass of array transformations that have an inverse variant as [↓ drop] is to [↑ take]."))

(defclass vad-indefinite ()
  ((%content :accessor vads-content
             :initform nil
             :initarg :inverse
             :documentation "Cached rendered content of the array."))
  (:documentation "Superclass of array transformations whose output is indefinite until each element of the input has been processed."))

(defclass vad-limitable (vad-indefinite)
  nil (:documentation "Superclass of indefinite array transformations whose output can be dimensionally limited to avoid needless computation."))

;; this is subrendering for the case of ≡↓↓2 3⍴⍳6
(defclass vader-subarray (vad-subrendering varray-derived)
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

(defmethod indexer-of ((varray vader-subarray) &optional params)
  (vasv-indexer varray))

(defclass vader-operate (vad-subrendering varray-derived vad-on-axis vad-with-io)
  ((%params :accessor vaop-params
            :initform nil
            :initarg :params
            :documentation "Parameters for ")
   (%function :accessor vaop-function
              :initform nil
              :initarg :function
              :documentation "Function to be applied to derived array element(s).")
   (%sub-shape :accessor vaop-sub-shape
               :initform nil
               :initarg :sub-shape
               :documentation "Shape of a lower-rank array to be combined with a higher-rank array along given axes.")))

(defmethod etype-of ((varray vader-operate))
  (if (and (getf (vaop-params varray) :binary-output)
           (loop :for item :across (vader-base varray) :always (not (shape-of item))))
      'bit t))

(defmethod prototype-of ((varray vader-operate))
  (if (= 1 (length (vader-base varray)))
      (let ((first-shape (shape-of (aref (vader-base varray) 0)))
            (first-indexer (indexer-of (aref (vader-base varray) 0))))
        (if (zerop (reduce #'* first-shape))
            (prototype-of (render (aref (vader-base varray) 0)))
            (prototype-of (apply-scalar (vaop-function varray)
                                        (render (if (not (functionp first-indexer))
                                                    first-indexer (funcall first-indexer 0)))))))
      (let ((pre-proto))
        ;; TODO: more optimization is possible here
        (loop :for item :across (vader-base varray)
              :when (< 0 (size-of item))
                :do (let ((this-indexer (indexer-of item)))
                      (if (not pre-proto)
                          (setf pre-proto (render (if (not (functionp this-indexer))
                                                      this-indexer (funcall this-indexer 0))))
                          (setf pre-proto
                                (apply-scalar (vaop-function varray)
                                              pre-proto
                                              (render (if (not (functionp this-indexer))
                                                          this-indexer (funcall this-indexer 0))))))))
        (prototype-of pre-proto))))

(defmethod shape-of ((varray vader-operate))
  (get-or-assign-shape
   varray
   (let ((shape) (sub-shape)
         (axis (setf (vads-axis varray)
                     (if (vads-axis varray)
                         (funcall (lambda (ax)
                                    (if (numberp ax)
                                        (- ax (vads-io varray))
                                        (if (zerop (size-of ax))
                                            ax (if (= 1 (length ax)) ;; disclose 1-item axis vectors
                                                   (- (aref ax 0) (vads-io varray))
                                                   (loop :for a :across ax
                                                         :collect (- a (vads-io varray)))))))
                                  (disclose (render (vads-axis varray))))))))
     (flet ((shape-matches (a)
              (loop :for s1 :in shape :for s2 :in (shape-of a) :always (= s1 s2))))
       (loop :for a :across (vader-base varray)
             :do (let () ; ((a (disclose-unitary a)))
                   (if (shape-of a) ;; 1-element arrays are treated as scalars
                       (if (or (not shape)
                               (= 1 (reduce #'* shape)))
                           (setf shape (shape-of a))
                           (let ((rank (length (shape-of a))))
                             (if (or (not (= rank (length shape)))
                                     (and (not (shape-matches a))
                                          (not (= 1 (size-of a)))))
                                 (if axis (if (= (length shape)
                                                 (if (numberp axis) 1 (length axis)))
                                              (if (> rank (length shape))
                                                  (let ((ax-copy (if (listp axis) (copy-list axis)))
                                                        (shape-copy (copy-list shape))
                                                        (matching t))
                                                    (if (not (vaop-sub-shape varray))
                                                        (setf (vaop-sub-shape varray) shape))
                                                    (loop :for d :in (shape-of a) :for ix :from 0
                                                          :when (and (if ax-copy (= ix (first ax-copy))
                                                                         (= ix axis)))
                                                            :do (if (/= d (first shape-copy))
                                                                    (setf matching nil))
                                                                (setf ax-copy (rest ax-copy)
                                                                      shape-copy (rest shape-copy)))
                                                    (if matching (setf shape (shape-of a))
                                                        (error "Mismatched array dimensions.")))
                                                  (if (= rank (length shape))
                                                      (if (not (shape-matches a))
                                                          (error "Mismatched array dimensions."))
                                                      (error "Mismatched array dimensions.")))
                                              (if (= rank (if (numberp axis) 1 (length axis)))
                                                  (if (not (shape-matches a))
                                                      (or (and (numberp axis)
                                                               (= (first (shape-of a))
                                                                  (nth axis shape))
                                                               (setf (vaop-sub-shape varray)
                                                                     (shape-of a)))
                                                          (and (listp axis)
                                                               (loop :for ax :in axis :for sh :in shape
                                                                     :always (= sh (nth ax shape)))
                                                               (setf (vaop-sub-shape varray) shape))
                                                          (error "Mismatched array dimensions."))
                                                      (setf (vaop-sub-shape varray) (shape-of a)))
                                                  (error "Mismatched array dimensions.")))
                                     (error "Mismatched array dimensions."))))))))
       shape))))

(defmethod indexer-of ((varray vader-operate) &optional params)
  (let* ((out-shape (shape-of varray))
         (sub-shape (vaop-sub-shape varray))
         (out-rank (length out-shape))
         (axis (vads-axis varray))
         (shape-factors (if axis (get-dimensional-factors out-shape t)))
         (base-indexers (loop :for a :across (vader-base varray) :collect (indexer-of a)))
         (sub-factors (if axis (get-dimensional-factors sub-shape t))))
    (lambda (index)
      (let ((result) (subarrays) (sub-flag))
        (if (= 1 (size-of (vader-base varray)))
            (let ((base-indexer (indexer-of (aref (vader-base varray) 0))))
              ;; (print (list :fn (funcall base-indexer index)))
              (setf result (if (not (functionp base-indexer))
                               (funcall (vaop-function varray) base-indexer)
                               (let ((indexed (funcall base-indexer index)))
                                 (if (or (arrayp indexed) (varrayp indexed))
                                     (make-instance 'vader-operate
                                                    :base (vector indexed)
                                                    :function (vaop-function varray)
                                                    :index-origin (vads-io varray)
                                                    :params (vaop-params varray))
                                     (funcall (vaop-function varray)
                                              (funcall base-indexer index)))))))
            (loop :for a :across (vader-base varray)
                  :for ai :in base-indexers :for ix :from 0
                  :do (let* ((shape (shape-of a))
                             (size (size-of a))
                             (rank (length shape))
                             (item (if (and shape (< 1 size))
                                       (if (and axis (not (= rank out-rank)))
                                           (funcall ai
                                                    (if (numberp axis)
                                                        (mod (floor index (aref shape-factors axis))
                                                             size)
                                                        (let ((remaining index) (sub-index 0))
                                                          (loop :for f :across shape-factors :for fx :from 0
                                                                :do (multiple-value-bind (div remainder)
                                                                        (floor remaining f)
                                                                      (setf remaining remainder)
                                                                      (loop :for ax :in axis
                                                                            :for ix :from 0 :when (= ax fx)
                                                                            :do (incf sub-index
                                                                                      (* (aref sub-factors
                                                                                               ix)
                                                                                         div)))))
                                                          sub-index)))
                                           (or (funcall ai index)
                                               (prototype-of a)))
                                       (if (not (varrayp a))
                                           (if (not (and (arrayp a)
                                                         (= 1 (size-of a))))
                                               a (funcall ai 0))
                                           ;;(let ((indexer (indexer-of a)))
                                           (if (not (functionp ai))
                                               ai (funcall ai 0))))))
                        ;; (print (list :a item))
                        ;; (setf item (or item prototype)) ;; if item is nil, 
                        (push item subarrays) ;; TODO: this list appending is wasteful for simple ops like 1+2
                        ;; (print (list :su subarrays))
                        (if (or (arrayp item) (varrayp item))
                            (setf sub-flag t)
                            (setf result (if (not result)
                                             item (funcall (vaop-function varray)
                                                           result item)))))))
        ;; (print (list :eee))
        (if (not sub-flag)
            result (make-instance 'vader-operate :base (coerce (reverse subarrays) 'vector)
                                                 :function (vaop-function varray)
                                                 :index-origin (vads-io varray)
                                                 :params (vaop-params varray)))))))

(defclass vader-select (varray-derived vad-on-axis vad-with-io vad-with-argument)
  ((%function :accessor vasel-function
              :initform nil
              :initarg :function
              :documentation "Function to be applied to derived array element(s).")
   (%assign :accessor vasel-assign
            :initform nil
            :initarg :assign
            :documentation "Item(s) to be assigned to selected indices in array.")
   (%assign-shape :accessor vasel-assign-shape
                  :initform nil
                  :initarg :assign-shape
                  :documentation "Shape of area to be assigned, eliding 1-sized dimensions.")
   (%calling :accessor vasel-calling
             :initform nil
             :initarg :calling
             :documentation "Function to be called on original and assigned index values.")))

(defmethod etype-of ((varray vader-select))
  (if (vasel-assign varray)
      (type-in-common (etype-of (vader-base varray))
                      (etype-of (vasel-assign varray)))
      (call-next-method)))

(defmethod shape-of ((varray vader-select))
  (get-or-assign-shape
   varray
   (let* ((idims (shape-of (vader-base varray)))
          (set (vasel-assign varray))
          (indices (vads-argument varray))
          (naxes (< 1 (length indices)))
          (assign-shape (if set (setf (vasel-assign-shape varray)
                                      (loop :for i :in indices :for id :in idims
                                            :when (not i) :collect id
                                              :when (and (shape-of i)
                                                         (< 1 (size-of i)))
                                                :collect (size-of i)))))
          (s 0) (sdims (if set (shape-of set))))
     ;; (print (list :i indices sdims assign-shape))
     (if set (progn (if sdims (if (not (loop :for i :in assign-shape :for sd :in sdims
                                             :always (= sd i)))
                                  (error "Dimensions of assigned area don't match array to be assigned.")))
                    idims)
         (if naxes (loop :for i :in indices :for d :in idims
                         :append (let ((len (or (and (null i) (list d))
                                                (and (integerp i) nil)
                                                (shape-of i))))
                                   (if (and (not len) (not (integerp i)))
                                       (error "Invalid index."))
                                   ;; collect output dimensions according to indices;
                                   ;; this is necessary even when setting values
                                   ;; compatible with the input array in order
                                   ;; to catch invalid indices
                                   (if len (incf s))
                                   len))
             (let ((od (shape-of (or (first indices)
                                     (vader-base varray)))))
               od))))))

(defmethod indexer-of ((varray vader-select) &optional params)
  (let* ((indices)
         (iarray-factors)
         (base-indexer (base-indexer-of varray))
         (base-rank (length (shape-of (vader-base varray))))
         (set (vasel-assign varray))
         (set-indexer (indexer-of set))
         (ofactors (if (not set) (get-dimensional-factors (shape-of varray) t)))
         (ifactors (get-dimensional-factors (shape-of (vader-base varray)) t)))
    ;; (print (list :in indices ofactors ifactors :aa (vasel-assign-shape varray)))
    (setf indices (loop :for item :in (vads-argument varray)
                        :collect (let ((ishape (shape-of item)))
                                   (if (< 1 (length ishape))
                                       (push (get-dimensional-factors ishape)
                                             iarray-factors))
                                   (render item)))
          iarray-factors (reverse iarray-factors))
    ;; (print (list :arg (render (vader-base varray)) (vads-argument varray)))
    (lambda (index)
      (let* ((remaining index) (oindex 0) (ofix 0) (valid t) 
             (adims (shape-of (vasel-assign varray)))
             (afactors (if adims (get-dimensional-factors (vasel-assign-shape varray) t)))
             (choose-indexed) (assign-sub-index) (iafactors iarray-factors))
        ;; (print (list :cc indices ifactors of actors iarray-factors))
        (flet ((check-vindex (ind vector-index)
                 ;; (PRINT (LIST :III ind VECTOR-INDEX))
                 (let ((vector-indexer (indexer-of vector-index)))
                   (loop :for v :below (size-of vector-index) :for ix :from 0
                         :when (let* ((this-index (funcall vector-indexer v))
                                      (sub-index (funcall (indexer-of this-index) 0)))
                                 ;; (print (list :ti this-index sub-index))
                                 (cond ((numberp sub-index)
                                        (= ind (- sub-index (vads-io varray))))
                                       ((shape-of sub-index)
                                        (= ind (reduce #'* (loop :for n :across (render sub-index)
                                                                   :collect (- n (vads-io varray))))))
                                       (t nil)))
                           :return (funcall vector-indexer v)))))
          (loop :for in :in indices :for ifactor :across ifactors :while (not choose-indexed)
                :for ix :from 0 :while valid
                :do (let ((afactor (if (and afactors (< ix (length afactors)))
                                       (aref afactors ix))))
                      (if (numberp in) ;; handle numeric indices as for x[1;2]
                          (if set (multiple-value-bind (index remainder) (floor remaining ifactor)
                                    (if (/= index (- in (vads-io varray)))
                                        (setf valid nil)
                                        (if (= ix (1- (length ifactors)))
                                            (incf oindex)))
                                    (incf ofix)
                                    (setf remaining remainder))
                              (incf oindex (* ifactor (- in (vads-io varray)))))
                          ;; handle arrays as indices as for x[⍳3]
                          (if in (let ((matched-index) (sub-index 0))
                                   ;; (print (list :ii in))
                                   (if (or (vectorp in)
                                           (and (or (arrayp in) (varrayp in))
                                                (not (shape-of in))))
                                       (multiple-value-bind (index remainder)
                                           (floor remaining (if set ifactor (aref ofactors ofix)))
                                         ;; (print (list :fl index remainder))
                                         (if set (let* ((sub-indexer (indexer-of in))
                                                        (sub-indexed (funcall sub-indexer 0)))
                                                   ;; (print (list :ba in index))
                                                   (if (numberp sub-indexed)
                                                       (loop :for i :below (size-of in)
                                                             :while (not matched-index)
                                                             :do (if (/= index (- (funcall sub-indexer i)
                                                                                  (vads-io varray)))
                                                                     (if afactor (incf oindex afactor))
                                                                     (setf matched-index i)))
                                                       (let ((match (check-vindex index in)))
                                                         (if match (setf oindex match
                                                                         matched-index t
                                                                         assign-sub-index index))))
                                                   ;; (print (list :mm matched-index))
                                                   ;; (print (list :af index afactor matched-index oindex))
                                                   (if (not matched-index) (setf valid nil)))
                                             (setf sub-index index))
                                         (incf ofix)
                                         (setf remaining remainder))
                                       
                                       (progn (loop :for iafactor :in (first iafactors)
                                                    :do (multiple-value-bind (index remainder)
                                                            (floor remaining (aref ofactors ofix))
                                                          (incf sub-index (* iafactor index))
                                                          (incf ofix)
                                                          (setf remaining remainder)))
                                              (if (not (vectorp in))
                                                  (setf iafactors (rest iafactors)))))
                                   (if (not matched-index)
                                       ;; adjust indices if the index was not an array as for x[⍳3]←5
                                       (let ((indexed (funcall (indexer-of in) sub-index)))
                                         (if (numberp indexed)
                                             (incf oindex (* ifactor (- indexed (vads-io varray))))
                                             (setf oindex indexed)))))
                              ;; handle elided indices
                              (multiple-value-bind (index remainder)
                                  (floor remaining (if set ifactor (aref ofactors ofix)))
                                (if set (if (or (not adims)
                                                (< index (first adims)))
                                            (if afactor (incf oindex (* afactor index))
                                                (incf oindex index))
                                            (setf valid nil))
                                    (let ((indexed (if in (funcall (indexer-of in) index))))
                                      ;; if choose indexing is in use, set this object to subrender
                                      (if (or (not in) (numberp indexed))
                                          (incf oindex (* ifactor index)))))
                                (incf ofix)
                                (setf adims (rest adims)
                                      remaining remainder))))))
          ;; (print (list :val index valid oindex))
          ;; (if valid (print (list :oin oindex afactors)))
          (if (numberp oindex)
              (if set (if valid (if (vasel-calling varray)
                                    (let ((original (if (not (functionp base-indexer))
                                                        base-indexer (funcall base-indexer index))))
                                      (if (not (functionp set-indexer))
                                          (funcall (vasel-calling varray)
                                                   original (vasel-assign varray))
                                          (funcall (vasel-calling varray)
                                                   original (funcall set-indexer oindex))))
                                    (if (not (functionp set-indexer))
                                        (vasel-assign varray) (funcall set-indexer oindex)))
                          (if (not (functionp base-indexer))
                              base-indexer (funcall base-indexer index)))
                  (let ((indexed (if (not (functionp base-indexer))
                                     base-indexer (funcall base-indexer oindex))))
                    (if (not (shape-of varray))
                        (setf (vads-subrendering varray) t))
                    indexed))
              (let ((index-shape (first (shape-of oindex))))
                ;; (PRINT (LIST :aaa oindex (= index-shape (length (shape-of (vader-base varray))))))
                (setf (vads-subrendering varray) t)
                (if set
                    (if valid
                        (let* ((meta-indexer (indexer-of oindex))
                               (meta-index (funcall meta-indexer 0))
                               (assign-indexer (indexer-of (vasel-assign varray)))
                               (sub-base (make-instance 'vader-select
                                                        :base (vader-base varray)
                                                        :index-origin (vads-io varray)
                                                        :argument (if (numberp meta-index)
                                                                      (list meta-index)
                                                                      (coerce (render meta-index) 'list)))))
                          ;; (print (list :vaa (vasel-assign varray)))
                          (make-instance 'vader-select
                                         :base (disclose (render sub-base))
                                         ;; TODO: wrap this in disclose obj
                                         :index-origin (vads-io varray)
                                         :argument (rest (coerce (render oindex) 'list))
                                         :assign (if (not (functionp assign-indexer))
                                                     assign-indexer (funcall assign-indexer
                                                                             assign-sub-index))
                                         :assign-shape (vasel-assign-shape varray)
                                         :calling (vasel-calling varray)))
                        (if (not (functionp base-indexer))
                            base-indexer (funcall base-indexer index)))
                    (if (and (numberp (funcall base-indexer 0))
                             (= index-shape (length (shape-of (vader-base varray)))))
                        ;; if the length of the index vector is equal to the rank of the indexed array
                        ;; and the first element is a number, as for (5 5⍴⍳25)[(1 2)(3 4)]
                        (make-instance 'vader-select :base (vader-base varray)
                                                     :index-origin (vads-io varray)
                                                     :argument (coerce (render oindex) 'list))
                        ;; if the length of the index vector is not equal to the array's rank
                        ;; (as for (('JAN' 1)('FEB' 2)('MAR' 3))[(2 1)(1 2)])
                        ;; or the first element of the index vector is not a number
                        ;; (as for (2 3⍴('JAN' 1)('FEB' 2)('MAR' 3)('APR' 4)('MAY' 5)('JUN' 6))[((2 3)1)((1 1)2)])
                        (let* ((meta-indexer (indexer-of oindex))
                               (meta-index (funcall meta-indexer 0))
                               (sub-base (make-instance 'vader-select
                                                        :base (vader-base varray)
                                                        :index-origin (vads-io varray)
                                                        :argument (if (numberp meta-index)
                                                                      (list meta-index)
                                                                      (coerce (render meta-index) 'list)))))
                          (make-instance 'vader-select :base (disclose (render sub-base))
                                         ;; TODO: wrap this in disclose obj
                                         :index-origin (vads-io varray)
                                         :argument (rest (coerce (render oindex) 'list)))))))))))))

(defclass vader-random (varray-derived vad-with-rng vad-with-io)
  ((%cached :accessor varand-cached
            :initform nil
            :initarg :cached
            :documentation "Cached randomized array data."))
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

(defmethod indexer-of ((varray vader-random) &optional params)
  (let* ((rngs (vads-rng varray))
         (base-indexer (base-indexer-of varray))
         (scalar-base (not (or (varrayp (vader-base varray))
                               (arrayp (vader-base varray)))))
         (gen-name (getf (rest rngs) :rng))
         (generator (or (getf (rest rngs) gen-name)
                        (setf (getf (rest rngs) gen-name)
                              (if (eq :system gen-name)
                                  :system (random-state:make-generator gen-name)))))
         (seed (getf (rest rngs) :seed)))

    ;; randomized array content is generated synchronously
    ;; and cached in case a random seed is in use
    (if (and seed (not (varand-cached varray)))
        (progn (setf (varand-cached varray)
                     (make-array (size-of (vader-base varray))
                                 :element-type (etype-of varray)))
               (loop :for i :below (size-of (vader-base varray))
                     :do (setf (row-major-aref (varand-cached varray) i)
                               (apl-random-process (funcall base-indexer i)
                                                   (vads-io varray) generator)))))

    ;; (if seed (print (list seed (varand-cached varray))))
    
    (lambda (index)
      (if scalar-base (apl-random-process (funcall base-indexer index) (vads-io varray)
                                          generator)
          (if seed (row-major-aref (varand-cached varray) index)
              (apl-random-process (funcall base-indexer index) (vads-io varray)
                                  generator))))))

(defclass vader-deal (varray-derived vad-with-argument vad-with-rng vad-with-io)
  ((%cached :accessor vadeal-cached
            :initform nil
            :initarg :cached
            :documentation "Cached shuffled vector elements."))
  (:documentation "A shuffled index vector as from the [? deal] function."))

(defmethod etype-of ((varray vader-deal))
  (if (/= 1 (reduce #'* (shape-of (vads-argument varray))))
      (error "Both arguments to ? must be non-negative integers.")
      (let* ((base-indexer (indexer-of (vader-base varray)))
             (max (if (not (functionp base-indexer))
                      base-indexer (funcall base-indexer 0))))
        (list 'integer 0 (+ max (vads-io varray))))))

(defmethod prototype-of ((varray vader-deal))
  (declare (ignore varray))
  0)

(defmethod shape-of ((varray vader-deal))
  (if (/= 1 (reduce #'* (shape-of (vads-argument varray))))
      (error "Both arguments to ? must be non-negative integers.")
      (let* ((arg-indexer (indexer-of (vads-argument varray)))
             (length (if (not (functionp arg-indexer))
                         arg-indexer (funcall arg-indexer 0))))
        (if (integerp length)
            (list length)
            (error "Both arguments to ? must be non-negative integers.")))))

(defmethod indexer-of ((varray vader-deal) &optional params)
  (let* ((rngs (vads-rng varray))
         (base-indexer (indexer-of (vader-base varray)))
         (count (if (not (functionp base-indexer))
                    base-indexer (funcall base-indexer 0)))
         (gen-name (getf (rest rngs) :rng))
         (generator (or (getf (rest rngs) gen-name)
                        (setf (getf (rest rngs) gen-name)
                              (if (eq :system gen-name)
                                  :system (random-state:make-generator gen-name)))))
         (arg-indexer (indexer-of (vads-argument varray)))
         (length (if (not (functionp arg-indexer))
                     arg-indexer (funcall arg-indexer 0)))
         (vector (make-array count :element-type (etype-of varray))))
    
    (setf (vadeal-cached varray) vector)
    (xdotimes vector (x (length vector))
      (setf (aref vector x) (+ x (vads-io varray))))
    
    (loop :for i :from count :downto 2
          :do (rotatef (aref vector (if (eq :system generator) (random i)
                                        (random-state:random-int generator 0 (1- i))))
                       (aref vector (1- i))))
    
    (lambda (index) (aref (vadeal-cached varray) index))))

(defclass vader-without (varray-derived vad-with-argument vad-limitable)
  nil (:documentation "An array without given elements as from the [~ without] function."))

(defmethod etype-of ((varray vader-without))
  (type-in-common (etype-of (vader-base varray))
                  (etype-of (vads-argument varray))))

(defmethod shape-of ((varray vader-without))
  (get-or-assign-shape
   varray (let ((this-indexer (indexer-of varray)))
            (declare (ignore this-indexer))
            (shape-of (vads-content varray)))))

(defmethod indexer-of ((varray vader-without) &optional params)
  (if (not (vads-content varray))
      (flet ((compare (o a)
               (funcall (if (and (characterp a) (characterp o))
                            #'char= (if (and (numberp a) (numberp o))
                                        #'= (lambda (a o) (declare (ignore a o)))))
                        o a)))
        (let ((derivative-count (if (getf params :shape-deriving)
                                    (reduce #'* (getf params :shape-deriving))))
              (contents (render (vader-base varray)))
              (argument (render (vads-argument varray))))
          (if (and (arrayp argument)
                   (not (vectorp argument)))
              (error "The left argument to [~ without] must be a vector.")
              (let* ((included) (count 0)
                     (cont-vector (if (or (vectorp contents) (not (arrayp contents)))
                                      contents
                                      (make-array (size-of contents)
                                                  :displaced-to contents
                                                  :element-type (etype-of contents)))))
                ;; (print (list :cont contents cont-vector argument))
                (if (vectorp argument)
                    (loop :for element :across argument :while (or (not derivative-count)
                                                                   (< count derivative-count))
                          :do (let ((include t))
                                (if (vectorp cont-vector)
                                    (loop :for ex :across cont-vector
                                          :do (if (compare ex element) (setq include nil)))
                                    (if (compare cont-vector element) (setq include nil)))
                                (if include (progn (push element included)
                                                   (incf count)))))
                    (let ((include t))
                      (if (vectorp cont-vector)
                          (loop :for ex :across cont-vector
                                :do (if (compare ex argument) (setq include nil)))
                          (if (compare cont-vector argument) (setq include nil)))
                      (if include (progn (push argument included)
                                         (incf count)))))
                ;; (print (list :inc included derivative-count))
                (setf (vads-content varray)
                      (make-array (length included) :element-type (etype-of argument)
                                                    :initial-contents (reverse included))))))))
  (lambda (index) (aref (vads-content varray) index)))

(defclass vader-umask (varray-derived vad-limitable)
  nil (:documentation "The unique mask of an array as from the [≠ unique mask] function."))

(defmethod etype-of ((varray vader-umask))
  (declare (ignore varray))
  'bit)

(defmethod prototype-of ((varray vader-umask))
  (declare (ignore varray))
  0)

(defmethod shape-of ((varray vader-umask))
  (get-or-assign-shape
   varray (let ((this-indexer (indexer-of varray)))
            (declare (ignore this-indexer))
            (shape-of (vads-content varray)))))

(defmethod indexer-of ((varray vader-umask) &optional params)
  (if (not (vads-content varray))
      (let* ((derivative-count (if (getf params :shape-deriving)
                                   (reduce #'* (getf params :shape-deriving))))
             (base-shape (shape-of (vader-base varray)))
             (contents (render (vader-base varray)))
             (output-length (if (not derivative-count)
                                (first base-shape)
                                (min derivative-count (first base-shape))))
             (increment (reduce #'* (rest base-shape)))
             (displaced (if (< 1 (array-rank contents))
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
        (setf (vads-content varray) output)))
  (lambda (index) (aref (vads-content varray) index)))

(defclass vader-shape (varray-derived)
  nil (:documentation "The shape of an array as from the [⍴ shape] function."))

(defmethod prototype-of ((varray vader-shape))
  (declare (ignore varray))
  0)

(defmethod etype-of ((varray vader-shape))
  (or (apply #'type-in-common (loop :for dimension :in (shape-of (vader-base varray))
                                    :collect (assign-element-type dimension)))
      t))

(defmethod shape-of ((varray vader-shape))
  "The shape of a reshaped array is simply its argument."
  (get-or-assign-shape varray (list (length (shape-of (vader-base varray))))))

(defmethod indexer-of ((varray vader-shape) &optional params)
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

(defmethod indexer-of ((varray vader-reshape) &optional params)
  "Index a reshaped array."
  (let* ((base-indexer (base-indexer-of varray))
         (input-size (reduce #'* (shape-of (vader-base varray))))
         (output-shape (shape-of varray))
         (output-size (reduce #'* output-shape)))
    (lambda (index)
      (if (zerop output-size)
          (prototype-of varray)
          (if (not (functionp base-indexer))
              base-indexer
              (let ((indexed (funcall base-indexer (if (not output-shape)
                                                       0 (mod index (max 1 input-size))))))
                (setf (vads-subrendering varray) t)
                (if (not (subrendering-p indexed))
                    indexed (render indexed))))))))

(defclass vader-depth (varray-derived)
  nil (:documentation "The first dimension of an array as from the [≢ first dimension] function."))

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
  (let ((shape (shape-of input))
        (indexer (indexer-of input)))
    (flet ((vap (item) (or (arrayp item) (varrayp item))))
      (if (not (functionp indexer))
          1 (let* ((first-level (not layer))
                   (layer (if layer layer 1))
                   (new-layer layer))
              (dotimes (i (size-of input))
                (let ((item (funcall indexer i)))
                  (if (vap item)
                      (multiple-value-bind (next-layer new-uniform new-possible-depth)
                          (varray-depth item (1+ layer) uniform possible-depth)
                        (setq new-layer (max new-layer next-layer)
                              uniform new-uniform
                              possible-depth new-possible-depth))
                      (if (not possible-depth) (setq possible-depth new-layer)
                          (if (/= layer possible-depth) (setq uniform nil))))))
              (values (funcall (if (and first-level (not uniform)) #'- #'identity)
                               new-layer)
                      uniform possible-depth))))))

(defmethod indexer-of ((varray vader-depth) &optional params)
  "Index a reshaped array."
  (let ((shape (or (shape-of (vader-base varray))
                   (varrayp (vader-base varray))
                   (arrayp (vader-base varray)))))
    (if shape (lambda (index) (declare (ignore index))
                (varray-depth (vader-base varray)))
        (lambda (index) (declare (ignore index)) 0))))

(defclass vader-first-dim (varray-derived)
  nil (:documentation "The first dimension of an array as from the [≢ first dimension] function."))

(defmethod prototype-of ((varray vader-first-dim))
  (declare (ignore varray))
  0)

(defmethod etype-of ((varray vader-first-dim))
  (list 'integer 0 (first (shape-of (vader-base varray)))))

(defmethod shape-of ((varray vader-first-dim))
  "The shape of a reshaped array is simply its argument."
  (declare (ignore varray))
  nil)

(defmethod indexer-of ((varray vader-first-dim) &optional params)
  "Index a reshaped array."
  (let ((dimension (first (shape-of (vader-base varray)))))
    (lambda (index) (or dimension 1))))

(defun varray-compare (item1 item2 &optional comparison-tolerance)
  "Perform a deep comparison of two APL arrays, which may be multidimensional or nested."
  (flet ((vap (item) (or (arrayp item) (varrayp item))))
    (let ((shape1 (shape-of item1))
          (shape2 (shape-of item2))
          (indexer1 (indexer-of item1))
          (indexer2 (indexer-of item2)))
      (if (not shape1)
          (if (not shape2)
              (or (and comparison-tolerance (floatp indexer1) (floatp indexer2)
                       (> comparison-tolerance (abs (- indexer1 indexer2))))
                  (and (numberp indexer1)
                       (numberp indexer2)
                       (= item1 indexer2))
                  (and (characterp indexer1)
                       (characterp indexer2)
                       (char= item1 indexer2))))
          (if (and shape2 (= (length shape2) (length shape2))
                   (if (and (loop :for d :below (length shape1)
                                  :always (= (nth d shape1) (nth d shape2)))
                            ;; compared arrays must be either character or non-character to match
                            (or (< 0 (size-of item1)) ;; 0-size arrays must be of same type, as for ⍬≡''
                                (if (not (member (etype-of item1) '(character base-char)))
                                    (not (member (etype-of item2) '(character base-char)))
                                    (member (etype-of item2) '(character base-char)))))
                       (loop :for i :below (size-of item1)
                             :always (array-compare (funcall indexer1 i)
                                                    (funcall indexer2 i)))))
              (let ((match t))
                (dotimes (i (size-of item1))
                  (let ((item (funcall indexer1 i))
                        (alternate (funcall indexer2 i)))
                    (setq match (and match (or (and (vap item) (vap alternate)
                                                    (array-compare item alternate))
                                               (and (numberp item) (numberp alternate)
                                                    (= item alternate))
                                               (and (characterp item) (characterp alternate)
                                                    (char= item alternate)))))))
                match))))))

(defclass vader-compare (varray-derived vad-with-ct vad-invertable)
  nil (:documentation "The first dimension of an array as from the [≢ first dimension] function."))

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

(defmethod indexer-of ((varray vader-compare) &optional params)
  "Index a reshaped array."
  (let ((base-indexer (base-indexer-of varray)))
    (lambda (index) (if (funcall (if (vads-inverse varray) #'not #'identity)
                                 (varray-compare (funcall base-indexer 0)
                                                 (funcall base-indexer 1)
                                                 (vads-ct varray)))
                        1 0))))

(defclass vader-enlist (varray-derived vad-limitable)
  nil (:documentation "An array decomposed into a vector as from the [∊ enlist] function."))

(defmethod shape-of ((varray vader-enlist))
  (get-or-assign-shape
   varray (let ((this-indexer (indexer-of varray)))
            (declare (ignore this-indexer))
            (if (eq :raveled (vads-content varray))
                (list (size-of (vader-base varray)))
                (shape-of (vads-content varray))))))

(defmethod indexer-of ((varray vader-enlist) &optional params)
  (let ((base-indexer (indexer-of (vader-base varray))))
    (if (not (vads-content varray))
        (let ((first-item (if (not (functionp base-indexer))
                              base-indexer (funcall base-indexer 0))))
          (if (and (not (shape-of (vader-base varray)))
                   (not (or (arrayp first-item)
                            (varrayp first-item))))
              (setf (vads-content varray)
                    (make-array 1 :initial-element first-item :element-type (etype-of first-item)))
              (if (not (eq t (etype-of (vader-base varray))))
                  (setf (vads-content varray) :raveled)
                  (let ((index 0) (length 0)
                        (input (render (vader-base varray)))
                        (derivative-count (if (getf params :shape-deriving)
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
                        (setf (vads-content varray) output))))))))
    (lambda (index)
      (if (eq :raveled (vads-content varray))
          (funcall base-indexer index)
          (aref (vads-content varray) index)))))

(defclass vader-membership (varray-derived vad-with-argument)
  ((%to-search :accessor vamem-to-search
               :initform nil
               :initarg :to-search
               :documentation "Set of elements to be checked for membership in array."))
  (:documentation "A membership array as from the [∊ membership] function."))

(defmethod etype-of ((varray vader-membership))
  (declare (ignore varray))
  'bit)

(defmethod indexer-of ((varray vader-membership) &optional params)
  (let ((base-indexer (base-indexer-of varray)))
    (labels ((compare (item1 item2)
               (if (and (characterp item1) (characterp item2))
                   (char= item1 item2)
                   (if (and (numberp item1) (numberp item2))
                       (= item1 item2)
                       (if (and (arrayp item1) (arrayp item2))
                           (array-compare item1 item2))))))
      
      (if (not (vamem-to-search varray))
          (let ((argument (render (vads-argument varray))))
            ;; TODO: possible to optimize this?
            (if (functionp base-indexer)
                (setf (vamem-to-search varray) argument)
                (if (not (arrayp argument))
                    (setf (vamem-to-search varray)
                          (if (compare argument base-indexer) 1 0))
                    (setf (vamem-to-search varray)
                          (if (not (loop :for i :below (array-total-size argument)
                                         :never (compare base-indexer (row-major-aref argument i))))
                              1 0))))))
      (lambda (index)
        (if (arrayp (vamem-to-search varray))
            (let ((found))
              (loop :for ix :below (size-of (vamem-to-search varray)) :while (not found)
                    :do (setq found (compare (funcall base-indexer index)
                                             (row-major-aref (vamem-to-search varray) ix))))
              (if found 1 0))
            (if (functionp base-indexer)
                (if (compare (vamem-to-search varray)
                             (funcall base-indexer index))
                    1 0)
                (vamem-to-search varray)))))))

(defclass vader-find (varray-derived vad-with-argument)
  ((%pattern :accessor vafind-pattern
             :initform nil
             :initarg :pattern
             :documentation "Pattern to find.")
   (%cached :accessor vafind-cached
            :initform nil
            :initarg :cached
            :documentation "Cached data to search."))
  (:documentation "An array of search matches as from the [⍷ find] function."))

(defmethod etype-of ((varray vader-find))
  (declare (ignore varray))
  'bit)

(defmethod prototype-of ((varray vader-find))
  (declare (ignore varray))
  0)

(defmethod indexer-of ((varray vader-find) &optional params)
  (let* ((target (or (vafind-pattern varray)
                     (setf (vafind-pattern varray)
                           (render (vads-argument varray)))))
         (is-target-array (arrayp target))
         (target-shape (shape-of target))
         (target-rank (length target-shape))
         (target-size (size-of target))
         (target-factors (get-dimensional-factors target-shape t))
         (base (if is-target-array
                   (or (vafind-cached varray)
                       (setf (vafind-cached varray)
                             (render (vader-base varray))))))
         (base-indexer (if (not is-target-array)
                           (indexer-of (vader-base varray))))
         (base-shape (shape-of base))
         (base-factors (get-dimensional-factors base-shape t))
         (rank-delta (- (length base-factors) target-rank))
         (target-head (if (not is-target-array)
                          target (row-major-aref target 0))))
    
    (lambda (index)
      (if is-target-array
          (if (not (array-compare target-head (row-major-aref base index)))
              0 (let ((remaining index) (valid t) (shape-ref target-shape))
                  ;; determine whether the target array fits within the dimensions
                  ;; of the searched array from the row-major starting point
                  (loop :for f :across base-factors :for b :in base-shape :for ix :from 0
                        :do (multiple-value-bind (item remainder) (floor remaining f)
                              (if (>= ix rank-delta)
                                  (if (< b (+ item (first shape-ref)))
                                      (setf valid nil)
                                      (setf shape-ref (rest shape-ref))))
                              (setf remaining remainder)))
                  (if (not valid)
                      0 (progn
                          (loop :for it :below target-size :while valid
                                :do (let ((tremaining it) (bremaining index)
                                          (base-index 0) (tfactors target-factors))
                                      (loop :for b :across base-factors :for ix :from 0
                                            :do (multiple-value-bind (titem tremainder)
                                                    (if (>= ix rank-delta)
                                                        (floor tremaining
                                                               (aref tfactors (- ix rank-delta))))
                                                  (multiple-value-bind (bitem bremainder)
                                                      (floor bremaining b)
                                                    (incf base-index (* b (+ bitem (or titem 0))))
                                                    (setf tremaining (or tremainder it)
                                                          bremaining bremainder))))
                                      (setf valid (array-compare (row-major-aref target it)
                                                                 (row-major-aref base base-index)))))
                          (if valid 1 0)))))
          (if (array-compare target-head (if (not (functionp base-indexer))
                                             base-indexer (funcall base-indexer index)))
              1 0)))))

(defclass vader-where (varray-derived vad-with-io vad-limitable)
  nil (:documentation "The coordinates of array elements equal to 1 as from the [⍸ where] function."))

(defmethod etype-of ((varray vader-where))
  (declare (ignore varray))
  t)

(defmethod prototype-of ((varray vader-where))
  "Prototype is an array of zeroes with length equal to the base array's rank."
  (make-array (length (shape-of (vader-base varray))) :element-type 'bit :initial-element 0))

(defmethod shape-of ((varray vader-where))
  (get-or-assign-shape
   varray (let ((this-indexer (indexer-of varray))
                (base-indexer (indexer-of (vader-base varray))))
            (declare (ignore this-indexer))
            (or (shape-of (vads-content varray))
                (list (if (and (not (functionp base-indexer))
                               (integerp base-indexer)
                               (= 1 base-indexer))
                          1 0))))))

(defmethod indexer-of ((varray vader-where) &optional params)
  (let* ((base-shape (shape-of (vader-base varray)))
         (base-rank (length base-shape))
         (maximum (if base-shape (reduce #'max base-shape)))
         (factors (if base-shape (get-dimensional-factors base-shape t))))
    (if (and maximum (not (vads-content varray)))
        (let* ((derivative-count (if (getf params :shape-deriving)
                                     (reduce #'* (getf params :shape-deriving))))
               (base-indexer (indexer-of (vader-base varray)))
               (output-length (if derivative-count (min derivative-count (first base-shape))
                                  (first base-shape)))
               (indices) (match-count 0))
          (loop :for i :below (size-of (vader-base varray))
                :while (or (not derivative-count) (< match-count derivative-count))
                :do (let ((this-item (funcall base-indexer i)))
                      (if (and (integerp this-item) (= 1 this-item))
                          (progn (push i indices)
                                 (incf match-count)))))
          (let ((output (make-array match-count
                                    :element-type (list 'integer 0 (size-of (vader-base varray))))))
            (loop :for in :in indices :for ix :from (1- match-count) :downto 0
                  :do (setf (aref output ix) in))
            (setf (vads-content varray) output))))
    (lambda (index)
      (if (zerop base-rank) (make-array 0)
          (if (= 1 base-rank)
              (+ (vads-io varray) (aref (vads-content varray) index))
              (let ((remaining (aref (vads-content varray) index))
                    (output (make-array base-rank :initial-element 0
                                                  :element-type (list 'integer 0 maximum))))
                (loop :for f :across factors :for ix :from 0
                      :do (multiple-value-bind (item remainder) (floor remaining f)
                            (setf (aref output ix) (+ item (vads-io varray))
                                  remaining remainder)))
                output))))))

(defclass vader-interval-index (varray-derived vad-with-io vad-with-argument)
  nil (:documentation "Interval indices of value(s) as from the [⍸ interval index] function."))

(defmethod etype-of ((varray vader-interval-index))
  (list 'integer 0 (if (not (shape-of (vader-base varray)))
                       1 (+ (vads-io varray)
                            (reduce #'max (shape-of (vader-base varray)))))))

(defmethod prototype-of ((varray vader-interval-index))
  (declare (ignore varray))
  0)

(defmethod shape-of ((varray vader-interval-index))
  (let ((arg-rank (length (shape-of (vads-argument varray))))
        (base-shape (shape-of (vader-base varray))))
    (if (not base-shape)
        nil (butlast base-shape (1- arg-rank)))))

(defmethod indexer-of ((varray vader-interval-index) &optional params)
  (let* ((base-indexer (indexer-of (vader-base varray)))
         (argument (render (vads-argument varray)))
         (arg-shape (shape-of argument))
         (base-rendered (if (second arg-shape)
                            (render (vader-base varray))))
         (arg-indexer (indexer-of argument))
         (base-increment (if (second arg-shape)
                             (/ (reduce #'* (shape-of base-rendered))
                                (reduce #'* (shape-of varray)))))
         (arg-span (if (second arg-shape)
                       (/ (size-of argument) base-increment))))
    (lambda (index)
      (if (not (second arg-shape))
          (let ((count 0)
                (value (if (not (functionp base-indexer))
                           base-indexer (funcall base-indexer index))))
            (loop :for item :across argument :while (funcall (alpha-compare #'>) value item)
                  :do (incf count))
            count)
          (let ((count 0)
                (sub-base (make-array base-increment
                                      :element-type (etype-of base-rendered)
                                      :displaced-to base-rendered
                                      :displaced-index-offset (* index base-increment))))
            (loop :for ix :below arg-span
                  :while (vector-grade
                          (alpha-compare #'<)
                          (make-array base-increment
                                      :displaced-to argument :element-type (etype-of argument)
                                      :displaced-index-offset (* ix base-increment))
                          sub-base)
                  :do (incf count))
            count)))))

;; TODO: is subrendering needed here? Check render function
(defclass vader-pare (vader-reshape vad-on-axis vad-with-io)
  nil (:documentation "An array with a reduced shape as from the [, catenate] or [⍪ table] functions."))

(defmethod shape-of ((varray vader-pare))
  "The shape of a reshaped array is simply its argument."
  (get-or-assign-shape
   varray (let ((base-shape (shape-of (vader-base varray)))
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
                            base-shape (let ((output) (added)
                                             (real-axis (ceiling axis)))
                                         (loop :for s :in base-shape :for ix :from 0
                                               :do (if (= ix real-axis) (setf added (push 1 output)))
                                                   (push s output))
                                         (if (not added) (push 1 output))
                                         (reverse output)))
                        (if (zerop (size-of axis))
                            (append base-shape (list 1))
                            (let ((output) (reducing) (prev-axis)
                                  (axis (copy-list axis))
                                  (shape base-shape))
                              (loop :for s :in base-shape :for ix :from 0
                                    :do (if reducing
                                            (if axis (if (/= (first axis) (1+ prev-axis))
                                                         (error "Invalid axis for [, ravel].")
                                                         (if (= ix (first axis))
                                                             (setf (first output)
                                                                   (* s (first output))
                                                                   prev-axis (first axis)
                                                                   axis (rest axis))))
                                                (progn (push s output)
                                                       (setf reducing nil)))
                                            (progn (push s output)
                                                   (if (and axis (= ix (first axis)))
                                                       (setf reducing t
                                                             prev-axis (first axis)
                                                             axis (rest axis))))))
                              (reverse output)))))))))

(defclass vader-catenate (varray-derived vad-on-axis vad-with-argument vad-with-io)
  ((%laminating :accessor vacat-laminating
                :initform nil
                :initarg :laminating
                :documentation "Whether this catenation laminates arrays."))
  (:documentation "A catenated array as from the [, catenate] function."))

(defmethod etype-of ((varray vader-catenate))
  (apply #'type-in-common (loop :for array :across (vader-base varray)
                                :when (< 0 (size-of array))
                                :collect (etype-of array))))

(defmethod shape-of ((varray vader-catenate))
  (get-or-assign-shape
   varray
   (let* ((ref-shape) (uneven)
          (each-shape (loop :for a :across (vader-base varray)
                            :collect (let ((shape (shape-of a)))
                                       ;; (shape-of) must be called before checking whether an
                                       ;; array subrenders since (shape-of) determines whether a
                                       ;; catenated array subrenders
                                       (if (or (subrendering-p a)
                                               (subrendering-base a))
                                           (setf (vads-subrendering varray) t))
                                       (if (or (varrayp a) (arrayp a))
                                           shape))))
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

(defmethod indexer-of ((varray vader-catenate) &optional params)
  (let* ((out-shape (shape-of varray))
         (to-laminate (vacat-laminating varray))
         (axis (disclose-unitary (vads-axis varray)))
         (ofactors (get-dimensional-factors out-shape t))
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
        (loop :for ofactor :across ofactors :for fx :from 0
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
              (if (not (subrendering-p indexed))
                  indexed (render indexed))))))))

(defclass vader-mix (varray-derived vad-on-axis vad-with-io)
  ((%shape-indices :accessor vamix-shape-indices
                   :initform nil
                   :initarg :shape-indices
                   :documentation "Indices of shape dimensions."))
  (:documentation "A mixed array as from the [↑ mix] function."))

(defmethod etype-of ((varray vader-mix))
  (let ((base-indexer (base-indexer-of varray)))
    (apply #'type-in-common (loop :for aix :below (size-of (vader-base varray))
                                  :when (and (functionp base-indexer)
                                             (< 0 (size-of (funcall base-indexer aix))))
                                    :collect (etype-of (funcall base-indexer aix))
                                  :when (not (functionp base-indexer))
                                    :collect (assign-element-type base-indexer)))))

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
             
             (reverse out-shape)))))))

(defmethod indexer-of ((varray vader-mix) &optional params)
  (let* ((oshape (shape-of varray))
         (ofactors (get-dimensional-factors oshape t))
         (oindexer (base-indexer-of varray))
         (dim-indices (vamix-shape-indices varray))
         (orank (length (shape-of (vader-base varray))))
         (outer-shape (loop :for i :in dim-indices :for s :in (shape-of varray)
                            :when (> orank i) :collect s))
         (inner-shape (loop :for i :in dim-indices :for s :in (shape-of varray)
                            :when (<= orank i) :collect s))
         (inner-rank (length inner-shape))
         (iofactors (get-dimensional-factors outer-shape t)))
    (lambda (index)
      ;; TODO: add logic to simply return the argument if it's an array containing no nested arrays
      
      (if (not oshape)
          (if (not (functionp oindexer))
              (disclose oindexer) ;; TODO: change indexer-of for rank 0 arrays to obviate this
              (let* ((indexed (funcall oindexer 0))
                     (iindexer (indexer-of indexed))
                     (sub-index (if (not (functionp iindexer))
                                    iindexer (funcall (indexer-of indexed) 0))))
                (if (and (not (shape-of indexed)))
                    (setf (vads-subrendering varray) t))
                sub-index))
          
          (let ((remaining index) (row-major-index) (outer-indices) (inner-indices))
            (loop :for ofactor :across ofactors :for di :in dim-indices :for fx :from 0
                  :do (multiple-value-bind (this-index remainder) (floor remaining ofactor)
                        (setf remaining remainder)
                        (if (> orank di) (push this-index outer-indices)
                            (push this-index inner-indices))))

            (let* ((inner-indices (reverse inner-indices))
                   (oindex (loop :for i :in (reverse outer-indices)
                                 :for f :across iofactors :summing (* i f)))
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

(defclass vader-split (vad-subrendering varray-derived vad-on-axis vad-with-io vad-maybe-shapeless)
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

(defmethod indexer-of ((varray vader-split) &optional params)
  (let* ((output-shape (shape-of varray))
         (output-size (reduce #'* output-shape))
         (axis (vads-axis varray))
         (base-shape (shape-of (vader-base varray)))
         (base-indexer (base-indexer-of varray))
         (base-factors (get-dimensional-factors base-shape t))
         (sv-factor (if (< 0 (length base-factors)) (aref base-factors axis)))
         (core-indexer (indexer-split axis (length output-shape)
                                      base-factors (get-dimensional-factors output-shape t)))
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

(defmethod prototype-of ((varray vader-section))
  (let ((indexer (indexer-of varray))
        (size (size-of varray))
        (base-size (size-of (vader-base varray))))
    (if (or (zerop size) (zerop base-size))
        (prototype-of (vader-base varray))
        (aplesque::make-empty-array (render (if (not (functionp indexer))
                                                indexer (funcall indexer 0)))))))

;; TODO: delete this duplicate
(defmethod shape-of ((varray vader-section))
  "The shape of a sectioned array is the parameters (if not inverse, as for [↑ take]) or the difference between the parameters and the shape of the original array (if inverse, as for [↓ drop])."
  (get-or-assign-shape
   varray
   (let* ((arg-shape (shape-of (vads-argument varray)))
          (arg-indexer (indexer-of (setf (vads-argument varray)
                                         (render (vads-argument varray)))))
          (is-inverse (vads-inverse varray))
          (iorigin (vads-io varray))
          (axis (vads-axis varray))
          ;; the shape of the base is only needed for [↓ drop]
          (base-shape (copy-list (shape-of (vader-base varray))))
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

(defmethod shape-of ((varray vader-section))
  "The shape of a sectioned array is the parameters (if not inverse, as for [↑ take]) or the difference between the parameters and the shape of the original array (if inverse, as for [↓ drop])."
  (get-or-assign-shape
   varray
   (let* ((base (vader-base varray))
          (arg-shape (shape-of (vads-argument varray)))
          (arg-indexer (indexer-of (setf (vads-argument varray)
                                         (render (vads-argument varray)))))
          (is-inverse (vads-inverse varray))
          (iorigin (vads-io varray))
          (axis (vads-axis varray)))

     (if (and (not is-inverse)
              (eq :last axis)
              (typep (vader-base varray) 'vad-limitable)
              (= 1 (size-of (vads-argument varray))))
         (if (functionp arg-indexer)
             (loop :for a :below (first arg-shape)
                   :collect (abs (funcall arg-indexer a)))
             (list (abs arg-indexer)))
         (let* ((base-shape (copy-list (shape-of base)))
                ;; the shape of the base is only needed for [↓ drop]
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
           pre-shape)))))

(defmethod indexer-of ((varray vader-section) &optional params)
  "Indexer for a sectioned array."
  (let* ((base-indexer (base-indexer-of varray))
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
                          (funcall base-indexer indexed))))))))

(defclass vader-enclose (vad-subrendering varray-derived vad-on-axis vad-with-io
                         vad-with-argument vad-maybe-shapeless)
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
                                    (enclose-atom (render (vads-argument varray)))))
                     (base-shape (shape-of (vader-base varray)))
                     (axis-size (if (and axis positions)
                                    (nth axis base-shape)
                                    (or (first (last base-shape)) 1)))
                     (input-offset 0) (intervals (list 0)))

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

(defmethod indexer-of ((varray vader-enclose) &optional params)
  (let* ((base-shape (shape-of (vader-base varray)))
         (output-shape (shape-of varray))
         (output-size (reduce #'* output-shape))
         (inner-shape (vaenc-inner-shape varray))
         (axis (or (vads-axis varray)
                   (max 0 (1- (length base-shape)))))
         (ofactors (get-dimensional-factors base-shape))
         (base-indexer (base-indexer-of varray))
         (outer-factors (get-dimensional-factors output-shape t))
         (inner-factors (get-dimensional-factors inner-shape t))
         (intervals (vads-shapeset varray))
         (offset-indexer
           (lambda (o)
             (lambda (i)
               (let ((orest o) (irest i) (index 0) (inner-dx 0) (outer-dx 0))
                 (loop :for f :in ofactors :for fx :from 0
                       :do (let ((in-outer (if (numberp axis) (/= fx axis)
                                               (loop :for a :across axis :never (= fx a)))))
                             (multiple-value-bind (factor remaining)
                                 (if in-outer (floor orest (aref outer-factors outer-dx))
                                     (floor irest (aref inner-factors inner-dx)))
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
                 (if (not (zerop this-size))
                     (let ((oseg (floor i iseg)) (ivix (mod i iseg)))
                       (if (not (functionp base-indexer))
                           base-indexer (funcall base-indexer
                                                 (if (= 1 section-size)
                                                     (+ input-offset (mod i last-idim)
                                                        (* last-indim (floor i last-idim)))
                                                     (+ ivix (* iseg (+ input-offset oseg)))))))))))))
    
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

(defclass vader-partition (vad-subrendering varray-derived vad-on-axis vad-with-io
                           vad-with-argument vad-maybe-shapeless)
  ((%params :accessor vapart-params
            :initform nil
            :initarg :params
            :documentation "Parameters for partitioning."))
  (:documentation "A partitioned array as from the [⊆ partition] function."))

(defmethod etype-of ((varray vader-partition))
  (let ((base (vader-base varray)))
    (if (or (arrayp base) (varrayp base)
            (vads-argument varray))
        t (assign-element-type base))))

(defmethod shape-of ((varray vader-partition))
  (get-or-assign-shape
   varray
   (let* ((idims (shape-of (vader-base varray)))
          (arank (length idims))
          (positions (if (vads-argument varray)
                         (setf (vads-argument varray)
                               (funcall (lambda (i)
                                          (if (not (arrayp i))
                                              i (if (< 0 (array-rank i))
                                                    i (vector (aref i)))))
                                        (disclose-unitary (render (vads-argument varray)))))))
          (axis (setf (vads-axis varray)
                      (if (vads-axis varray)
                          (max 0 (if (eq :last (vads-axis varray))
                                     (1- arank)
                                     (- (vads-axis varray)
                                        (vads-io varray))))))))
     (if (not positions)
         (let ((base-type (etype-of (vader-base varray))))
           (if (eq t base-type) ;; array is simple if not t-type
               (let ((is-complex)
                     (base-indexer ;; (base-indexer-of varray)
                       (indexer-of (vader-base varray))
                       ))
                 (if base-indexer
                     (progn (loop :for i :below (size-of (vader-base varray))
                                  :do (if (shape-of (funcall base-indexer i))
                                          (setf is-complex t)))
                            (if is-complex (shape-of (vader-base varray))))))))
         (if (not (arrayp positions))
             (if (< 1 (size-of (vader-base varray)))
                 (progn (setf (vapart-params varray)
                              (list :partitions 1))
                        (list 1)))
             (let ((r-indices) (r-intervals) (indices) (intervals)
                   (interval-size 0)
                   (current-interval -1)
                   (partitions 0))
               (declare (dynamic-extent r-indices r-intervals indices intervals
                                        interval-size current-interval partitions))
               ;; find the index where each partition begins in the
               ;; input array and the length of each partition
               (loop :for pos :across positions :for p :from 0
                     :do (progn (if (not (zerop current-interval))
                                    (incf interval-size))
                                ;; if a position lower than the current interval index is encountered,
                                ;; decrement the current index to it, as for 1 1 1 2 1 1 2 1 1⊆⍳9
                                (if (and current-interval (< 0 pos current-interval))
                                    (setq current-interval pos)))
                     :when (or (< current-interval pos)
                               (and (zerop pos) (not (zerop current-interval))))
                       :do (setq r-indices (cons p r-indices)
                                 r-intervals (if (rest r-indices) (cons interval-size r-intervals)))
                           (incf partitions (if (zerop pos) 0 1))
                           (setq current-interval pos interval-size 0))
               ;; add the last entry to the intervals provided the
               ;; positions list didn't have a 0 value at the end
               (if (not (zerop (aref positions (1- (length positions)))))
                   (push (- (length positions) (first r-indices))
                         r-intervals))
               
               (if (/= (length r-indices) (length r-intervals))
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

(defmethod indexer-of ((varray vader-partition) &optional params)
  (if (not (or (vads-argument varray)
               (vapart-params varray)))
      (if (shape-of varray)
          (base-indexer-of varray)
          (lambda (index) (vader-base varray)))
      (let* ((idims (shape-of (vader-base varray)))
             (partitions (getf (vapart-params varray) :partitions))
             (intervals (getf (vapart-params varray) :intervals))
             (indices (getf (vapart-params varray) :indices))
             (base-indexer (base-indexer-of varray))
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
                     (funcall base-indexer input-index))))))
        
        (lambda (index)
          (if (not (functionp base-indexer))
              base-indexer (let* ((focus (mod (floor index section-size) partitions))
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
                                            :indexer sub-indexer)))))))

(defclass vader-expand (varray-derived vad-on-axis vad-with-io vad-with-argument vad-invertable)
  nil (:documentation "An expanded (as from [\ expand]) or compressed (as from [/ compress]) array."))

(defmethod shape-of ((varray vader-expand))
  "The shape of an expanded or compressed array."
  (get-or-assign-shape
   varray
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

(defmethod indexer-of ((varray vader-expand) &optional params)
  (let* ((arg-vector (funcall (lambda (item)
                                (if (not (typep item 'sequence))
                                    item (coerce item 'vector)))
                              (render (vads-argument varray))))
         (base-indexer (base-indexer-of varray))
         (indexer (if (or (integerp arg-vector)
                          (< 0 (size-of arg-vector)))
                      (indexer-expand arg-vector (shape-of (vader-base varray))
                                      (vads-axis varray)
                                      (vads-inverse varray)))))
    (lambda (index)
      (if (not (functionp base-indexer))
          (if (funcall indexer index)
              (disclose base-indexer))
          (let ((indexed (funcall indexer index)))
            (if indexed (funcall base-indexer indexed)))))))

(defclass vader-pick (varray-derived vad-with-argument vad-with-io)
  ((%reference :accessor vapick-reference
               :initform nil
               :initarg :reference
               :documentation "Reference to the array designated by this function."))
  (:documentation "An element from within an array as from the [⊃ disclose] or [⊃ pick] functions."))

(defgeneric fetch-reference (varray base &optional path path-index)
  (:documentation "Fetch the array targeted by an invocation of [⊃ disclose] or [⊃ pick]."))

(defmethod fetch-reference ((varray vader-pick) base &optional path path-index)
  (or (vapick-reference varray)
      (let* ((path (or path (vads-argument varray)))
             (path-indexer (indexer-of path))
             (path-length (size-of path))
             (base-indexer (indexer-of base)))
        (if path
            (let ((path-value
                    (funcall
                     (lambda (index)
                       (if (numberp index)
                           (- index (vads-io varray))
                           (let ((ix 0)
                                 (iindexer (if (shape-of index)
                                               (indexer-of index)
                                               (indexer-of (funcall (indexer-of index) 0))))
                                 (index-length (if (shape-of index)
                                                   (size-of index)
                                                   (size-of (funcall (indexer-of index) 0))))
                                 (factors (get-dimensional-factors (shape-of base))))
                             (loop :for f :in factors :for s :below index-length
                                   :do (incf ix (* f (- (funcall iindexer s) (vads-io varray)))))
                             ix)))
                     (if (not (functionp path-indexer))
                         path-indexer (funcall path-indexer (or path-index 0))))))
              ;; (print (list :p path-value path-length))
              (if path-index (if (= path-index (1- path-length))
                                 (let ((indexer (if (not (functionp base-indexer))
                                                    base-indexer (funcall base-indexer path-value))))
                                   (if (and (not (shape-of indexer))
                                            (or (arrayp indexer) (varrayp indexer)))
                                       (setf (vads-subrendering varray) t))
                                   indexer)
                                 (fetch-reference varray (funcall base-indexer path-value)
                                                  path (1+ (or path-index 0))))
                  (setf (vapick-reference varray)
                        (if (= 1 path-length)
                            (let ((indexer (if (not (functionp base-indexer))
                                                    base-indexer (funcall base-indexer path-value))))
                                   (if (and (not (shape-of indexer))
                                            (or (arrayp indexer) (varrayp indexer)))
                                       (setf (vads-subrendering varray) t))
                              indexer)
                            (fetch-reference varray (funcall base-indexer path-value)
                                             path (1+ (or path-index 0)))))))
            (setf (vapick-reference varray)
                  (let ((indexer (if (not (functionp base-indexer))
                                     base-indexer (funcall base-indexer 0))))
                    (if (and (shape-of base) (not (shape-of indexer))
                             (or (arrayp indexer)
                                 (varrayp indexer)))
                        (setf (vads-subrendering varray) t))
                    indexer))))))

(defmethod etype-of ((varray vader-pick))
  (etype-of (fetch-reference varray (vader-base varray))))

(defmethod prototype-of ((varray vader-pick))
  (prototype-of (identity (fetch-reference varray (vader-base varray)))))

(defmethod shape-of ((varray vader-pick))
  (print (shape-of (fetch-reference varray (vader-base varray)))))

(defmethod shape-of ((varray vader-pick))
  (let ((ref (fetch-reference varray (vader-base varray))))
    (if (not (zerop (size-of (vader-base varray))))
        (shape-of ref))))

(defmethod indexer-of ((varray vader-pick) &optional params)
  (indexer-of (fetch-reference varray (vader-base varray)) params))

(defclass vader-intersection (varray-derived vad-limitable)
  nil (:documentation "An array intersection as from the [∩ intersection] function."))

(defmethod etype-of ((varray vader-intersection))
  (apply #'type-in-common (loop :for array :across (vader-base varray) :collect (etype-of array))))

(defmethod shape-of ((varray vader-intersection))
  (get-or-assign-shape
   varray (let ((this-indexer (indexer-of varray)))
            (declare (ignore this-indexer))
            (shape-of (vads-content varray)))))

(defmethod indexer-of ((varray vader-intersection) &optional params)
  (if (not (vads-content varray))
      (let ((derivative-count (if (getf params :shape-deriving)
                                  (reduce #'* (getf params :shape-deriving))))
            (contents (loop :for a :across (vader-base varray) :collect (render a))))
        (if (not (loop :for a :across (vader-base varray) :always (not (second (shape-of a)))))
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
                                (if (loop :for c :in (rest contents)
                                          :always (if (arrayp c)
                                                      (find (first contents)
                                                            c :test #'array-compare)
                                                      (array-compare (first contents) c)))
                                    (progn (incf match-count)
                                           (list (first contents)))))))
              (setf (vads-content varray)
                    (make-array match-count :initial-contents matches
                                            :element-type (etype-of varray)))))))
  (lambda (index) (aref (vads-content varray) index)))

(defclass vader-unique (varray-derived vad-limitable)
  ((%indices :accessor vauni-indices
             :initform nil
             :initarg :indices
             :documentation "Parameters for partitioning."))
  (:documentation "An array intersection as from the [∩ intersection] function."))

(defmethod etype-of ((varray vader-unique))
  (declare (ignore varray))
  t)

(defmethod shape-of ((varray vader-unique))
  (get-or-assign-shape
   varray (let ((this-indexer (indexer-of varray)))
            (declare (ignore this-indexer))
            (if (vads-content varray)
                (cons (length (vauni-indices varray))
                      (rest (shape-of (vads-content varray))))
                (list (length (vauni-indices varray)))))))

(defmethod indexer-of ((varray vader-unique) &optional params)
  (let* ((base-shape (shape-of (vader-base varray)))
         (cell-size (reduce #'* (rest base-shape)))
         (base-size (size-of (vader-base varray)))
         (base-rank (length base-shape))
         (base-indexer (indexer-of (vader-base varray))))
    (if (not (vauni-indices varray))
        (let ((derivative-count (if (getf params :shape-deriving)
                                    (reduce #'* (getf params :shape-deriving)))))
          (if (not (functionp base-indexer))
              (setf (vauni-indices varray)
                    (if (not base-shape)
                        #*0 #()))
              (if (= 1 base-rank)
                  (let ((uniques) (indices) (unique-count 0))
                    (loop :for ix :below base-size
                          :while (or (not derivative-count) (< unique-count derivative-count))
                          :do (let ((item (funcall base-indexer ix)))
                                (if (not (find item uniques :test #'array-compare))
                                    (progn (push item uniques)
                                           (push ix indices)
                                           (incf unique-count)))))
                    (setf (vauni-indices varray)
                          (make-array unique-count :element-type (list 'integer 0 base-size)
                                                   :initial-contents (reverse indices))))
                  (let ((base (render (vader-base varray)))
                        (major-cells (make-array (first base-shape)))
                        (indices) (uniques) (unique-count 0))
                    ;; (print :gg)
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
                    (setf (vads-content varray) base
                          (vauni-indices varray)
                          (make-array unique-count :element-type (list 'integer 0 (first base-shape))
                                                   :initial-contents (reverse indices))))))))
    (lambda (index)
      ;; (print (list :inde index (etype-of varray) (shape-of varray)))
      (if (not (functionp base-indexer))
          base-indexer
          (if (= 1 base-rank)
              (funcall base-indexer (aref (vauni-indices varray) index))
              (multiple-value-bind (count remainder) (floor index cell-size)
                (row-major-aref (vads-content varray)
                                (+ remainder (* cell-size
                                                (aref (vauni-indices varray) count))))))))))

(defclass vader-union (varray-derived vad-limitable)
  nil (:documentation "An array intersection as from the [∩ union] function."))

(defmethod etype-of ((varray vader-union))
  (apply #'type-in-common (loop :for array :across (vader-base varray) :collect (etype-of array))))

(defmethod shape-of ((varray vader-union))
  (get-or-assign-shape
   varray (let ((this-indexer (indexer-of varray)))
            (declare (ignore this-indexer))
            (list (+ (or (first (shape-of (aref (vads-content varray) 0))) 1)
                     (or (first (shape-of (aref (vads-content varray) 1))) 1))))))

(defmethod indexer-of ((varray vader-union) &optional params)
  (if (not (vads-content varray))
      (let* ((derivative-count (if (getf params :shape-deriving)
                                   (reduce #'* (getf params :shape-deriving))))
             (contents (loop :for a :across (vader-base varray) :collect (render a)))
             (shapes (loop :for a :across (vader-base varray) :collect (shape-of a)))
             (indexers (loop :for a :across (vader-base varray) :collect (indexer-of a)))
             (first (first contents)))
        ;; (print (list :con contents))
        (if (not (loop :for shape :in shapes :always (not (second shape))))
            (error "Arguments to [∪ union] must be vectors.")
            (let ((matched) (appended))
              (loop :for this-item :in (rest contents)
                    :do (push nil matched)
                        (if (arrayp first)
                            (if (arrayp this-item)
                                (loop :for ti :across this-item
                                      :when (not (find ti first :test #'array-compare))
                                        :do (push ti (first matched)))
                                (if (not (find this-item first :test #'array-compare))
                                    (push this-item (first matched))))
                            (if (arrayp this-item)
                                (loop :for ti :across this-item
                                      :when (not (array-compare ti first))
                                        :do (push ti (first matched)))
                                (if (not (array-compare this-item first))
                                    (push this-item (first matched)))))
                        (loop :for m :in (rest matched)
                              :do (if (arrayp this-item)
                                      (loop :for ti :across this-item
                                            :when (not (find ti m :test #'array-compare))
                                              :do (push ti (first matched)))
                                      (if (not (find this-item m :test #'array-compare))
                                          (push this-item (first matched))))))
              (loop :for m :in matched
                    :do (loop :for subm :in m :do (push subm appended)))
              (setf (vads-content varray)
                    (vector first (make-array (length appended) :initial-contents appended)))
              ))))
  (lambda (index)
    ;; (print (list :in index (vads-content varray)))
    (if (not (arrayp (aref (vads-content varray) 0)))
        (if (zerop index)
            (aref (vads-content varray) 0)
            (aref (aref (vads-content varray) 1)
                  (1- index)))
        (if (< index (length (aref (vads-content varray) 0)))
            (aref (aref (vads-content varray) 0) index)
            (aref (aref (vads-content varray) 1)
                  (- index (length (aref (vads-content varray) 0)))))
            )))

(defclass vader-turn (varray-derived vad-on-axis vad-with-io vad-with-argument)
  nil (:documentation "A rotated array as from the [⌽ rotate] function."))

(defun arg-process (argument)
  (if (or (listp argument) (numberp argument))
      argument (if (varrayp argument)
                   (render argument) ;; TODO: eliminate forced render here
                   (if (arrayp argument)
                       argument))))

(defmethod indexer-of ((varray vader-turn) &optional params)
  "Indexer for a rotated or flipped array."
  (let* ((base-indexer (base-indexer-of varray))
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

(defmethod indexer-of ((varray vader-permute) &optional params)
  "Indexer for a rotated or flipped array."
  (let* ((base-indexer (base-indexer-of varray))
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

(defclass vader-grade (varray-derived vad-with-argument vad-with-io vad-invertable vad-indefinite)
  nil (:documentation "An encoded array as from the [⊤ encode] function."))

(defmethod etype-of ((varray vader-grade))
  (let ((this-shape (first (shape-of varray))))
    (list 'integer (vads-io varray)
          (+ this-shape (vads-io varray)))))

(defmethod shape-of ((varray vader-grade))
  (get-or-assign-shape
   varray (let ((base-shape (shape-of (vader-base varray))))
            (if base-shape (list (first base-shape))
                (error "The [⍋ grade] function cannot take a scalar argument.")))))

(defmethod indexer-of ((varray vader-grade) &optional params)
  (let* ((arg-rendered (render (vads-argument varray)))
         (iorigin (vads-io varray))
         (base-rendered (render (vader-base varray))))
    (setf (vads-content varray)
          (if arg-rendered (grade (if (vectorp arg-rendered)
                                      (index-of base-rendered arg-rendered iorigin)
                                      (array-grade arg-rendered base-rendered))
                                  iorigin (alpha-compare (if (vads-inverse varray)
                                                             #'> #'<)))
              (grade base-rendered iorigin (alpha-compare (if (vads-inverse varray)
                                                              #'>= #'<=)))))
    (lambda (index) (aref (vads-content varray) index))))

(defclass vader-encode (varray-derived vad-with-argument)
  nil (:documentation "An encoded array as from the [⊤ encode] function."))

(defmethod shape-of ((varray vader-encode))
  (get-or-assign-shape varray (append (shape-of (vads-argument varray))
                                      (shape-of (vader-base varray)))))

(defmethod indexer-of ((varray vader-encode) &optional params)
  (let* ((adims (shape-of (vads-argument varray)))
         (out-dims (shape-of varray))
         (base-shape (shape-of (vader-base varray)))
         (base-indexer (base-indexer-of varray))
         (arg-indexer (indexer-of (vads-argument varray)))
         (aifactors (get-dimensional-factors adims))
         (oifactors (get-dimensional-factors base-shape t))
         (ofactors (get-dimensional-factors out-dims t)))
    (lambda (index)
      (let ((remaining index) (base 1) (oindex 0) (afactor 0) (oix 0) (value))
        (loop :for af :in aifactors :for of :across ofactors :for ix :from 0
              :do (multiple-value-bind (this-index remainder) (floor remaining of)
                    (incf oix)
                    (if (not (zerop ix))
                        (setf afactor (+ afactor (* af this-index))))
                    (setf remaining remainder)))
        (loop :for of :across oifactors
              :do (multiple-value-bind (this-index remainder) (floor remaining (aref ofactors oix))
                    (incf oix)
                    (setf oindex (+ oindex (* of this-index))
                          remaining remainder)))
        (setf remaining index
              value (if (not (functionp base-indexer))
                        base-indexer (funcall base-indexer oindex)))
        
        (if (functionp arg-indexer)
            (let ((last-base) (element) (aindex) (component 1)
                  (index (floor index (aref ofactors 0))))
              (loop :for b :from (1- (first adims)) :downto index
                    :do (setq last-base base
                              aindex (+ afactor (* b (first aifactors)))
                              base (* base (funcall arg-indexer aindex))
                              component (if (zerop base) value
                                            (nth-value 1 (floor value base)))
                              value (- value component)
                              element (if (zerop last-base) 0
                                          (floor component last-base))))
              (setf value element))
            (setf value (nth-value 1 (floor value arg-indexer))))
        value))))

(defclass vader-decode (varray-derived vad-with-argument)
  nil (:documentation "A decoded array as from the [⊥ decode] function."))

(defmethod etype-of ((varray vader-decode))
  t)

(defmethod shape-of ((varray vader-decode))
  (get-or-assign-shape varray (append (butlast (shape-of (vads-argument varray)))
                                      (rest (shape-of (vader-base varray))))))

(defmethod indexer-of ((varray vader-decode) &optional params)
  (let* ((odims (shape-of (vader-base varray)))
         (adims (shape-of (vads-argument varray)))
         (osize (size-of (vader-base varray)))
         (asize (size-of (vads-argument varray)))
         (base-indexer (base-indexer-of varray))
         (arg-indexer (indexer-of (vads-argument varray)))
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

    (if (shape-of varray)
        (dotimes (a asegments)
          (loop :for i :from (- (* av2 (1+ a)) 2) :downto (* av2 a)
                :do (setf (row-major-aref afactors i)
                          (* (if (not (functionp arg-indexer))
                                 arg-indexer
                                 (funcall arg-indexer (if (not (and (< 1 asize)
                                                                    (< 1 osize)
                                                                    (= 1 (first (last adims)))))
                                                          (1+ i)
                                                          (floor i (or (first odims) 1)))))
                             (row-major-aref afactors (1+ i)))))))
    
    (lambda (i)
      (if (shape-of varray)
          (let ((result 0))
            (loop :for index :below av2
                  :do (incf result (* (if (not (functionp base-indexer))
                                          base-indexer
                                          (funcall base-indexer (mod (+ (mod i out-section)
                                                                        (* out-section index))
                                                                     osize)))
                                      (row-major-aref afactors
                                                      (+ index (* av2 (floor i out-section)))))))
            result)
          (let ((result 0) (factor 1))
            (loop :for i :from (1- (if (< 1 av2) av2 ovector)) :downto 0
                  :do (incf result (* factor (if (not (functionp base-indexer))
                                                 base-indexer (funcall base-indexer
                                                                       (min i (1- ovector))))))
                      (setq factor (* factor (if (not (functionp arg-indexer))
                                                 arg-indexer (funcall arg-indexer
                                                                      (min i (1- av2)))))))
            result)))))

;; (1 2 3) (2 3 4)∘.⌽[1]⊂3 3⍴⍳9 NOT IN DYALOG?
