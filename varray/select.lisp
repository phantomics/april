;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Varray -*-
;;;; select.lisp

(in-package #:varray)

"Definition of the virtual array manifesting selections and sub-assignments of arrays, used to implement indexing and selective assignment. This virtual array powers all forms of selection, by axes as with X[Y], using [⌷ index], selective assignment as with (X↑Y)←Z or using the [@ at] operator."

(defclass vader-select (varray-derived vad-on-axis vad-with-io vad-with-argument)
  ((%assign       :accessor vasel-assign
                  :initform nil
                  :initarg :assign
                  :documentation "Item(s) to be assigned to selected indices in array.")
   (%assign-if    :accessor vasel-assign-if
                  :initform nil
                  :initarg :assign-if
                  :documentation "Function to select items to be assigned as for ⌽@(<∘5)⊢⍳9.")
   (%assign-shape :accessor vasel-assign-shape
                  :initform nil
                  :initarg :assign-shape
                  :documentation "Shape of area to be assigned, eliding 1-sized dimensions.")
   (%calling      :accessor vasel-calling
                  :initform nil
                  :initarg :calling
                  :documentation "Function to be called on original and assigned index values.")
   (%selector     :accessor vasel-selector
                  :initform nil
                  :initarg :selector
                  :documentation "Object implementing selection function for array."))
  (:metaclass va-class))

(defmethod etype-of ((varray vader-select))
  (if (vasel-calling varray)
      t (if (vasel-assign varray)
            (type-in-common (etype-of (vader-base varray))
                            (etype-of (vasel-assign varray)))
            (call-next-method))))

(defmethod shape-of ((varray vader-select))
  (let* ((idims (shape-of (vader-base varray)))
         (set (vasel-assign varray))
         (indices (unless (typep (vads-argument varray) 'varray::varray)
                    (vads-argument varray)))
         (naxes (when indices (< 1 (length indices))))
         (assign-shape (when (and set indices)
                         (setf (vasel-assign-shape varray)
                               (if (and (= (length indices)
                                           (rank-of (vader-base varray))))
                                   (if (>= 1 (rank-of (first indices)))
                                       ;; take the shape of the first of the indices in cases like
                                       ;; (⌽2 3⍴⍳6)@(⍳2 3)⊢M
                                       (loop :for i :in indices :for id :in idims
                                             :when (not i) :collect id
                                               :when (and (shape-of i) (< 1 (size-of i)))
                                                 :collect (size-of i))
                                       (shape-of (first indices)))
                                   (when (= 1 (length indices))
                                     (shape-of (first indices)))))))
         (s 0) (sdims (when set (shape-of set))))
    (if (not indices)
        idims (if set (if (not (and sdims (not (loop :for i :in assign-shape :for sd :in sdims
                                                     :always (= sd i)))))
                          idims (error "Dimensions of assigned area don't ~a"
                                       "match array to be assigned."))
                  (if naxes (loop :for i :in indices :for d :in idims
                                  :append (let ((len (or (and (null i) (list d))
                                                         (and (integerp i) nil)
                                                         (shape-of i))))
                                            (when (and (not len) (not (integerp i)))
                                              (error "Invalid index."))
                                            ;; collect output dimensions according to indices;
                                            ;; this is necessary even when setting values
                                            ;; compatible with the input array in order
                                            ;; to catch invalid indices
                                            (when len (incf s))
                                            len))
                      (shape-of (or (first indices)
                                    (vader-base varray))))))))

(defmethod generator-of ((varray vader-select) &optional indexers params)
  (declare (ignore indexers))
  (let* ((indices)
         (iarray-factors)
         (base-gen (generator-of (vader-base varray)))
         (base-rank (rank-of (vader-base varray)))
         (set (vasel-assign varray))
         (set-indexer (generator-of set))
         (idims (shape-of varray))
         (selector-eindices (when (listp (vasel-selector varray))
                              (vasel-selector varray)))
         (eindexer (when selector-eindices (generator-of (getf selector-eindices :ebase))))
         (index-selector (unless selector-eindices (let ((is (vasel-selector varray)))
                                                     ;; must get shape of selector at this point
                                                     (shape-of is)
                                                     is)))
         (sub-selector (generator-of index-selector nil
                                     (list :for-selective-assign
                                           (or (shape-of (vasel-assign varray)) t)
                                           :assigning set
                                           :toggle-toplevel-nested
                                           (lambda ()
                                             (setf (vads-nested varray) t)))))
         (ofactors (unless set (strides-of idims t)))
         (ifactors (strides-of (shape-of (vader-base varray)) t))
         (adims (shape-of (vasel-assign varray)))
         (afactors (when adims (strides-of (vasel-assign-shape varray) t))))
    (setf indices (loop :for item :in (vads-argument varray)
                        :collect (let ((ishape (shape-of item)))
                                   (when (< 1 (length ishape))
                                     (push (strides-of ishape) iarray-factors))
                                   (render item)))
          iarray-factors (reverse iarray-factors))
    (flet ((verify-vindex (ind vector-index)
             (let ((vector-indexer (generator-of vector-index)))
               (loop :for v :below (size-of vector-index) :for ix :from 0
                     :when (let ((sub-index (funcall (generator-of (funcall vector-indexer v)) 0)))
                             (cond ((numberp sub-index)
                                    (= ind (- sub-index (vads-io varray))))
                                   ((shape-of sub-index)
                                    (= ind (reduce #'* (loop :for n :across (render sub-index)
                                                             :collect (- n (vads-io varray))))))
                                   (t nil)))
                       :return (funcall vector-indexer v))))
           (compare-path (pindex path)
             (let ((remaining pindex) (valid t))
               (loop :for if :across ifactors :for p :across path :while valid
                     :do (multiple-value-bind (dindex remainder) (floor remaining if)
                           (setf remaining remainder)
                           (when (/= dindex (- p (vads-io varray)))
                             (setf valid nil))))
               valid)))

      (if (not (or set (vasel-calling varray)))
          (case (getf params :base-format)
            (:encoded)
            (:linear)
            (t (lambda (index)
                 (let* ((remaining index) (oindex 0) (ofix 0) (valid t) (iafactors iarray-factors))
                   (loop :for in :in indices :for ifactor :across ifactors
                         :for ix :from 0 :while valid
                         :do (if (numberp in)
                                 (incf oindex (* ifactor (- in (vads-io varray))))
                                 (if in (let ((matched-index) (sub-index 0))
                                          (if (or (and (vectorp in) (< 0 (length in)))
                                                  (and (or (arrayp in) (varrayp in))
                                                       (not (shape-of in))))
                                              (multiple-value-bind (index remainder)
                                                  (floor remaining (if (zerop (length ofactors))
                                                                       1 (aref ofactors ofix)))
                                                (incf ofix)
                                                (setf sub-index index
                                                      remaining remainder))
                                              (progn (dolist (iafactor (first iafactors))
                                                       (multiple-value-bind (index remainder)
                                                           (floor remaining (aref ofactors ofix))
                                                         (incf sub-index (* iafactor index))
                                                         (incf ofix)
                                                         (setf remaining remainder)))
                                                     (unless (vectorp in)
                                                       (setf iafactors (rest iafactors)))))
                                          (unless matched-index
                                            ;; adjust indices if the index was not an array as for x[⍳3]←5
                                            (let ((indexed (if (not (arrayp in))
                                                               in (row-major-aref in sub-index))))
                                              (if (numberp indexed)
                                                  (incf oindex (* ifactor (- indexed (vads-io varray))))
                                                  (setf oindex indexed)))))
                                     (multiple-value-bind (index remainder)
                                         (floor remaining (aref ofactors ofix))
                                       (let ((indexed (when in (row-major-aref in index))))
                                         ;; if choose indexing is in use, set this object to subrender
                                         (when (or (not in) (numberp indexed))
                                           (incf oindex (* ifactor index))))
                                       (incf ofix)
                                       (setf remaining remainder))))
                             (setf adims (rest adims)))
                   
                   (if (numberp oindex)
                       (let ((indexed (if (not (functionp base-gen))
                                          base-gen (funcall base-gen oindex))))
                         (unless (shape-of varray)
                           (setf (vads-nested varray) t))
                         indexed)
                       (let ((index-shape (first (shape-of oindex))))
                         (setf (vads-nested varray) t)
                         (if (and (numberp (funcall base-gen 0))
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
                             (let* ((meta-indexer (generator-of oindex))
                                    (meta-index (funcall meta-indexer 0))
                                    (sub-base (make-instance 'vader-select
                                                             :base (vader-base varray)
                                                             :index-origin (vads-io varray)
                                                             :argument (if (numberp meta-index)
                                                                           (list meta-index)
                                                                           (coerce (render meta-index)
                                                                                   'list)))))
                               (make-instance 'vader-select :base (disclose (render sub-base))
                                              ;; TODO: wrap this in disclose obj
                                              :index-origin (vads-io varray)
                                              :argument (rest (coerce (render oindex) 'list)))))))))))
          (if (vasel-assign-if varray)
              (let* ((assign-indexer (generator-of (vasel-assign varray)))
                     (mask (funcall (vasel-assign-if varray) (vader-base varray)))
                     (mask-indexer (generator-of mask)))
                (if (vasel-calling varray)
                    ;; TODO: optimize types for mask indices
                    ;; the case of an array assigned depending on logical function result,
                    ;; for example 10 11 12 13@(<∘5)⍳9
                    (let ((mindex 0)
                          (mindices (make-array (shape-of (vader-base varray))
                                                :element-type 'fixnum :initial-element 0)))
                      ;; create an index mask for the assignments where each nonzero number corresponds
                      ;; to the index of the assigned value - 1
                      (dotimes (i (size-of mask))
                        (unless (zerop (funcall mask-indexer i))
                          (setf (row-major-aref mindices i) (incf mindex))))
                      ;; TODO: add case for scalar functions as with ÷@(≤∘5)⊢3 3⍴⍳9 so that
                      ;; the function can work element by element instead of needing displacement
                      (let ((displaced (make-array mindex :element-type (etype-of (vader-base varray)))))
                        (setf mindex 0)
                        (dotimes (i (size-of mindices))
                          (let ((mi (row-major-aref mindices i)))
                            (unless (zerop mi)
                              (setf (aref displaced (1- (incf mindex)))
                                    (funcall base-gen i)))))
                        (let ((processed (render (funcall (vasel-calling varray) displaced
                                                          (vasel-assign varray)))))
                          (case (getf params :base-format)
                            (:encoded)
                            (:linear)
                            (t (lambda (index) ;; the case of 10 11 12@(2∘|)⍳5
                                 (let ((this-mindex (row-major-aref mindices index)))
                                   (if (zerop this-mindex) (funcall base-gen index)
                                       (aref processed (1- this-mindex))))))))))
                    (if (functionp assign-indexer)
                        (case (getf params :base-format)
                          (:encoded)
                          (:linear)
                          (t (lambda (index) ;; the case of (⊃⍳3)@(<∘5)⍳9 ; scalar (virtual) array assigned value
                               (if (zerop (funcall mask-indexer index))
                                   (funcall assign-indexer 0) (funcall base-gen index)))))
                        (case (getf params :base-format)
                          (:encoded)
                          (:linear)
                          (t (lambda (index) ;; the case of 9@(<∘5)⍳9 ; scalar assigned value
                               (if (not (zerop (funcall mask-indexer index)))
                                   assign-indexer (funcall base-gen index))))))))
              
              (case (getf params :base-format)
                (:encoded)
                (:linear)
                (t (lambda (index)
                     (let* ((remaining index) (oindex 0) (ofix 0) (valid t) (tafix 0)
                            (assign-sub-index) (iafactors iarray-factors))
                       (loop :for in :in indices :for ifactor :across ifactors
                             :for ix :from 0 :while valid
                             :do (if (numberp in) ;; handle numeric indices as for x[1;2]
                                     (multiple-value-bind (index remainder) (floor remaining ifactor)
                                       (when (/= index (- in (vads-io varray)))
                                         (setf valid nil))
                                       (incf ofix)
                                       (setf remaining remainder))
                                     ;; handle arrays as indices as for x[⍳3]
                                     (if in (let ((matched-index) (sub-index 0) (aindex index))
                                              (if (or (and (vectorp in) (< 0 (length in)))
                                                      (and (or (arrayp in) (varrayp in))))
                                                  (multiple-value-bind (index remainder)
                                                      (floor remaining ifactor)
                                                    (let* ((sub-indexer (generator-of in))
                                                           (sub-indexed (funcall sub-indexer 0))
                                                           (ssindexer (generator-of sub-indexed))
                                                           (ssindexed (when (functionp ssindexer)
                                                                        (funcall ssindexer 0))))
                                                      (if (numberp sub-indexed)
                                                          (let ((toindex)
                                                                (tafactor (if (or (not afactors)
                                                                                  (zerop (length afactors)))
                                                                              1 (aref afactors tafix))))
                                                            (loop :for i :below (size-of in)
                                                                  :do (when (= index (- (funcall sub-indexer i)
                                                                                        (vads-io varray)))
                                                                        (setf matched-index i
                                                                              toindex (+ oindex
                                                                                         (* i tafactor)))))
                                                            (incf tafix)
                                                            (when toindex (setf oindex toindex)))
                                                          (if (and ssindexed (numberp ssindexed)
                                                                   (= base-rank (size-of sub-indexed)))
                                                              ;; the case of g←6 6⍴0 ⋄ g[(3 3)(4 4)]←5
                                                              ;; vectors the length of the base rank
                                                              ;; select an element in the base
                                                              (loop :for b :below (size-of in)
                                                                    :do (when (compare-path
                                                                               aindex (funcall
                                                                                       sub-indexer b))
                                                                          (setf oindex b
                                                                                matched-index t
                                                                                assign-sub-index index)))
                                                              ;; the case of toasn←(('JAN' 1)('FEB' 2)
                                                              ;; toasn[(2 1)(1 2)]←45 67
                                                              ;; reach indexing into subarrays of the base
                                                              (let ((match (verify-vindex index in)))
                                                                (when match (setf oindex match
                                                                                  matched-index t
                                                                                  assign-sub-index index)))))
                                                      (setf valid matched-index))
                                                    (incf ofix)
                                                    (setf remaining remainder))
                                                  
                                                  (progn (dolist (iafactor (first iafactors))
                                                           (multiple-value-bind (index remainder)
                                                               (floor remaining (aref ofactors ofix))
                                                             (incf sub-index (* iafactor index))
                                                             (incf ofix)
                                                             (setf remaining remainder)))
                                                         (unless (vectorp in)
                                                           (setf iafactors (rest iafactors)))))
                                              (if (zerop (size-of in)) ;; the case of ⍬@⍬⊢1 2 3
                                                  (setf valid nil)
                                                  (unless matched-index
                                                    ;; adjust indices if the index was not an array as for x[⍳3]←5
                                                    (let* ((iindexer (generator-of in))
                                                           (indexed (if (not (functionp iindexer))
                                                                        iindexer (funcall iindexer sub-index))))
                                                      (if (numberp indexed)
                                                          (incf oindex (* ifactor (- indexed (vads-io varray))))
                                                          (setf oindex indexed))))))
                                         ;; handle elided indices
                                         (multiple-value-bind (index remainder)
                                             (floor remaining ifactor)
                                           (if (or (not adims)
                                                   (< index (first adims)))
                                               (let ((tafactor (if (or (not afactors)
                                                                       (zerop (length afactors)))
                                                                   1 (aref afactors tafix))))
                                                 
                                                 (if tafactor (incf oindex (* tafactor index))
                                                     (incf oindex index)))
                                               (setf valid nil))
                                           (incf ofix)
                                           (incf tafix)
                                           (setf remaining remainder))))
                                 (setf adims (rest adims)))
                       ;; index-selector is used in the case of assignment by selection,
                       ;; for example {A←'RANDOM' 'CHANCE' ⋄ (2↑¨A)←⍵ ⋄ A} '*'
                       (when index-selector
                         (setf valid (when (and (or valid (not (vads-argument varray)))
                                                (setf oindex (funcall sub-selector (if (numberp valid)
                                                                                       valid index))))
                                       valid)))
                       ;; (print (list :val index valid oindex (shape-of oindex) selector-eindices))
                       ;; (print (list :se set (funcall (generator-of set) 0) set-indexer oindex base-gen))
                       (if (numberp oindex)
                           (if valid (if (vasel-calling varray)
                                         (let ((original (if (not (functionp base-gen))
                                                             base-gen (funcall base-gen index))))
                                           (funcall (vasel-calling varray)
                                                    original (if (functionp set-indexer)
                                                                 (funcall set-indexer oindex)
                                                                 (vasel-assign varray))))
                                         (if (not (functionp set-indexer))
                                             (if (and index-selector (typep index-selector 'vader-pick))
                                                 ;; build a pick array instance to derive from an indexed value,
                                                 ;; as for the case of {na←3⍴⊂⍳4 ⋄ (1↑⊃na)←⍵ ⋄ na} 99
                                                 (let ((indexed (if (not (functionp base-gen))
                                                                    base-gen (funcall base-gen oindex))))
                                                   (setf (vads-nested varray) t
                                                         (vader-base index-selector) indexed
                                                         (vapick-assign index-selector) (vasel-assign varray)
                                                         (vapick-reference index-selector) indexed)
                                                   index-selector)
                                                 (if selector-eindices
                                                     (let* ((bindex (if (not (functionp base-gen))
                                                                        base-gen (funcall base-gen index)))
                                                            (assign-indexer (generator-of (vasel-assign varray)))
                                                            (eelement (unless (arrayp bindex)
                                                                        (funcall eindexer index))))
                                                       (if eelement
                                                           (let* ((eindices (getf selector-eindices
                                                                                  :eindices))
                                                                  (egen (generator-of eindices)))
                                                             (if (loop :for e :below (size-of eindices)
                                                                       :never (= eelement (funcall egen e)))
                                                                 bindex (vasel-assign varray)))
                                                           (progn
                                                             (setf (vads-nested varray) t)
                                                             (make-instance
                                                              'vader-select
                                                              :base bindex :index-origin (vads-io varray)
                                                              :assign (if (not (functionp assign-indexer))
                                                                          assign-indexer
                                                                          (funcall assign-indexer
                                                                                   assign-sub-index))
                                                              :assign-shape (vasel-assign-shape varray)
                                                              :calling (vasel-calling varray)
                                                              :selector (list :eindices
                                                                              (getf selector-eindices
                                                                                    :eindices)
                                                                              :ebase (funcall eindexer
                                                                                              index))))))
                                                     set-indexer))
                                             (funcall set-indexer oindex)))
                               (if (not (functionp base-gen))
                                   base-gen (funcall base-gen index)))
                           
                           (progn
                             (setf (vads-nested varray) t)
                             (if valid
                                 (if (typep oindex 'varray::varray)
                                     ;; in the case of an [¨ each]-composed assignment by selection
                                     ;; like {A←'RANDOM' 'CHANCE' ⋄ (2↑¨A)←⍵ ⋄ A} '*'
                                     (let ((sub-indexer (generator-of (vader-base varray)))
                                           (assign-indexer (generator-of (vasel-assign varray))))
                                       (make-instance 'vader-select
                                                      :base (funcall sub-indexer index)
                                                      :index-origin (vads-io varray)
                                                      :selector oindex
                                                      :assign (if (not (functionp assign-indexer))
                                                                  assign-indexer (funcall assign-indexer
                                                                                          assign-sub-index))
                                                      :assign-shape (vasel-assign-shape varray)
                                                      :calling (vasel-calling varray)))
                                     (let* ((meta-indexer (generator-of oindex))
                                            (meta-index (funcall meta-indexer 0))
                                            (assign-indexer (generator-of (vasel-assign varray)))
                                            (sub-base (make-instance 'vader-select
                                                                     :base (vader-base varray)
                                                                     :index-origin (vads-io varray)
                                                                     :argument (if (not (vectorp meta-index))
                                                                                   (list meta-index)
                                                                                   (coerce (render meta-index)
                                                                                           'list)))))
                                       (make-instance 'vader-select
                                                      :base (disclose (render sub-base))
                                                      ;; TODO: wrap this in disclose obj
                                                      :index-origin (vads-io varray)
                                                      :argument (rest (coerce (render oindex) 'list))
                                                      :assign (if (not (functionp assign-indexer))
                                                                  assign-indexer (funcall assign-indexer
                                                                                          assign-sub-index))
                                                      :nested t ; needed for cases like 3⌈@(⊂1 3)⊢3⍴⊂5⍴1
                                                      :assign-shape (vasel-assign-shape varray)
                                                      :calling (vasel-calling varray))))
                                 (if (not (functionp base-gen))
                                     base-gen (funcall base-gen index))))))))))))))
