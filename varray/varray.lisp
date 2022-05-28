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
                               (make-array (shape-of varray) :element-type (assign-element-type
                                                                            prototype)))))
              output)
            (let ((output (make-array (shape-of varray) :element-type (etype-of varray))))
              (dotimes (i (array-total-size output))
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
                     #'identity (lambda (item)
                                  (let ((rendered (render item)))
                                    (if (not (arrayp rendered))
                                        rendered (make-array nil :initial-element rendered)))))
                 (funcall indexer 0)))))

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
                (list :shape this-shape))))

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
(defmethod indexer-of ((vvector vvector-integer-progression) &optional params)
  (lambda (index)
    (if (< -1 index (vvip-number vvector))
        (* (+ (floor index (vvip-repeat vvector))
              (vvip-origin vvector))
           (vvip-factor vvector)))))

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
  nil (:documentation "Superclass of array transformations whose output is indefinite until each element of the input has been processed."))

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
      (prototype-of (apply-scalar (vaop-function varray)
                                  (aref (vader-base varray) 0)))
      (let ((pre-proto))
        (loop :for item :across (vader-base varray)
              :when (< 0 (size-of item))
                :do (let ((this-indexer (indexer-of item)))
                      (if (not pre-proto)
                          (setf pre-proto (if (not (functionp this-indexer))
                                              this-indexer (funcall this-indexer 0)))
                          (setf pre-proto
                                (apply-scalar (vaop-function varray)
                                              pre-proto (if (not (functionp this-indexer))
                                                            this-indexer (funcall this-indexer 0)))))))
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
       ;; (print (list :vv (vader-base varray)))
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
         (sub-factors (if axis (get-dimensional-factors sub-shape t))))
    ;; (print (list :ba (vader-base varray) axis shape-factors sub-factors))
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
            (loop :for a :across (vader-base varray) :for ix :from 0
                  :do (let* ((shape (shape-of a))
                             (size (size-of a))
                             (rank (length shape))
                             (item (if (and shape (< 1 size))
                                       (if (and axis (not (= rank out-rank)))
                                           (funcall (indexer-of a)
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
                                           (funcall (indexer-of a) index))
                                       (if (not (varrayp a))
                                           (if (not (and (arrayp a)
                                                         (= 1 (size-of a))))
                                               a (funcall (indexer-of a) 0))
                                           (let ((indexer (indexer-of a)))
                                             (if (not (functionp indexer))
                                                 indexer (funcall indexer 0)))))))
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
  (let* ((input-size (reduce #'* (shape-of (vader-base varray))))
         (output-shape (shape-of varray))
         (output-size (reduce #'* output-shape))
         (base-indexer (base-indexer-of varray)))
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
                         (if (not (subrendering-p indexed))
                             indexed (render indexed)))))))))

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
                (if (eq :tabulate axis) ; ,⍉↑⍬(⍳26)
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
              (disclose oindexer) ;; TODO: change indexer-of for rank 0 arrays to obviate this1
              (funcall oindexer 0))
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
     ;; (print (list :aa axis base-shape pre-shape axis arg-indexer))

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
     ;; (print (list :aa axis  arg-indexer))

     (if (and (not is-inverse)
              (eq :last axis)
              nil)
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
                          (funcall base-indexer indexed))
              (if (shape-of varray)
                  (prototype-of (vader-base varray)))
              ))))))

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
          (lambda (index)
            (vader-base varray)))
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

(defmethod indexer-of ((varray vader-expand) &optional params)
  (let* ((arg-vector (if (typep (vads-argument varray) 'sequence)
                         (coerce (vads-argument varray) 'vector)
                         (vads-argument varray)))
         (base-indexer (base-indexer-of varray))
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
         (base-rendered (render (vader-base varray)))
         (graded (if arg-rendered (grade (if (vectorp arg-rendered)
                                             (index-of base-rendered arg-rendered iorigin)
                                             (array-grade arg-rendered base-rendered))
                                         iorigin (alpha-compare (if (vads-inverse varray)
                                                                    #'> #'<)))
                     (grade base-rendered iorigin (alpha-compare (if (vads-inverse varray)
                                                                     #'>= #'<=))))))
    (lambda (index) (aref graded index))))

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
