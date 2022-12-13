;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Varray -*-
;;;; composed.lisp

(in-package #:varray)

"Definitions of virtual arrays produced by compositions of functions, implementing APL operators."

(defun side-effect-free (function)
  "Use a function's metadata to check whether it has side effects. Needed for multithreaded operators - the functions composed with operators must be free of side effects for multithreading."
  (let ((fn-meta (handler-case (funcall function :get-metadata)
                   (error () nil))))
    ;; (print (list :ffnn fn-meta))
    (and fn-meta (listp fn-meta)
         (or (member :side-effects fn-meta)
             (member :lexical-reference fn-meta)
             (member :sefns-called fn-meta))
         (not (getf fn-meta :side-effects))
         (not (getf fn-meta :side-refs))
         (not (getf fn-meta :sefns-called))
         (or (not (getf fn-meta :symfns-called))
             (loop :for fn :in (getf fn-meta :symfns-called)
                   :always (or (not (fboundp fn))
                               (side-effect-free (symbol-function fn))))))))

(defclass vader-composing (varray-derived)
  ((%left  :accessor vacmp-left
           :initform nil
           :initarg :left
           :documentation "Left function composed with operator.")
   (%right :accessor vacmp-right
           :initform nil
           :initarg :right
           :documentation "Right function composed with operator.")
   (%omega :accessor vacmp-omega
           :initform nil
           :initarg :omega
           :documentation "Right argument to composed function.")
   (%alpha :accessor vacmp-alpha
           :initform nil
           :initarg :alpha
           :documentation "Left argument to composed function.")
   (%threadable :accessor vacmp-threadable
                :initform t
                :initarg :threadable
                :documentation "Whether this operation is threadable; i.e. operand functions lack side effects."))
  (:metaclass va-class)
  (:documentation "An array produced by an operator-composed function."))

(defmethod etype-of ((varray vader-composing))
  (declare (ignore varray))
  t)

(defun op-compose (type &rest args)
  (when (getf args :axis)
    (setf (getf args :axis) (first (getf args :axis))))
  (labels ((this (omega &optional alpha)
             (if (eq :reassign-axes omega)
                 (let ((last-key))
                   (setf args (loop :for a :in args :collect (if (not (eq :axis last-key))
                                                                 a (first alpha))
                                    :do (setf last-key a)))
                   #'this)
                 (if (not (eq :get-metadata omega))
                     (apply #'make-instance type :omega omega :alpha alpha args)
                     (if (getf args :right)
                         (append (list :operator-reference type)
                                 (list :left-meta (funcall (getf args :left) :get-metadata))
                                 (list :right-meta (funcall (getf args :right) :get-metadata)))
                         (append (list :operator-reference type)
                                 (funcall (getf args :left) :get-metadata)))))))
    #'this))

(defclass vacomp-reduce (vad-subrendering vader-composing vad-on-axis vad-with-io vad-with-default-axis)
  ((%unitary :accessor vacred-unitary
             :initform nil
             :initarg :unitary
             :documentation "Whether the array measures only one unit along the axis to be reduced."))
  (:metaclass va-class)
  (:documentation "A reduce-composed array as with the [/ reduce] operator."))

(defmethod prototype-of ((varray vacomp-reduce))
  0)

(defmethod shape-of ((varray vacomp-reduce))
  (get-promised (varray-shape varray)
                (progn
                  (when (and (vads-default-axis varray)
                             (not (vads-axis varray)))
                    (setf (vads-axis varray) (vads-default-axis varray)))
                  (let* ((base-shape (shape-of (vacmp-omega varray)))
                         ;; (base-size (size-of (vacmp-omega varray)))
                         (window (when (vacmp-alpha varray)
                                   (setf (vacmp-alpha varray)
                                         (disclose-unitary (render (vacmp-alpha varray))))))
                         (window (when window (abs window)))
                         (axis (setf (vads-axis varray)
                                     ;; TODO: what's sending an ⎕IO of 0 here for ⌊10000×+∘÷/40/1 ?
                                     (max 0 (if (vads-axis varray)
                                                (- (vads-axis varray) (vads-io varray))
                                                (1- (length base-shape))))))
                         (output))
                    (loop :for b :in base-shape :for ix :from 0 :when (/= ix axis) :collect b
                          :when (and (= ix axis) (= 1 b)) :do (setf (vacred-unitary varray) t)
                            :when (and window (= ix axis)) :collect (- b (1- window)))))))

(defclass vader-subarray-reduce (vader-subarray)
  ((%window :accessor vasbr-window
            :initform nil
            :initarg :window
            :documentation "Window length for subarray to reduce.")
   (%delta :accessor vasbr-delta
           :initform nil
           :initarg :delta
           :documentation "Delta for subarray to reduce.")
   (%reverse :accessor vasbr-reverse
             :initform nil
             :initarg :reverse
             :documentation "Is subarray to be traversed in reverse order?."))
  (:metaclass va-class)
  (:documentation "An element of a split array as from the [↓ split] function."))

(defmethod prototype-of ((varray vader-subarray-reduce))
  (declare (ignore params))
  (get-promised (varray-prototype varray)
                (let* ((generator (generator-of varray))
                       (first-item (funcall generator 0)))
                  (if (varrayp first-item) (prototype-of first-item)
                      (apl-array-prototype first-item)))))

(defmethod generator-of ((varray vader-subarray-reduce) &optional indexers params)
  (let ((base-indexer (generator-of (vader-base varray) indexers params)))
    (if (vasbr-reverse varray)
        (let ((this-length (1- (first (shape-of varray)))))
          (lambda (index)
            (funcall base-indexer (+ (* (- this-length index) (vasv-index varray))
                                     (vasbr-delta varray)))))
        (lambda (index)
          (funcall base-indexer (+ (* index (vasv-index varray))
                                   (vasbr-delta varray)))))))

(defmethod generator-of ((varray vacomp-reduce) &optional indexers params)
  "Reduce an array along by a given function along a given dimension, optionally with a window interval."
  (let ((irank (rank-of (vacmp-omega varray)))
        (osize (size-of (vacmp-omega varray))))
    (if (or (> 2 osize) (vacred-unitary varray))
        (if (zerop irank) (vacmp-omega varray)
            ;; return just the omega indexer in cases like +/2 2 1⍴'a'
            (if (zerop osize)
                (let ((fn-meta (funcall (vacmp-left varray) :get-metadata nil)))
                  (or (getf fn-meta :id)
                      (error "Attempted to [/ reduce] with a function that has no identity value.")))
                (generator-of (vacmp-omega varray))))
        (let* ((odims (shape-of (vacmp-omega varray)))
               (omega-indexer (generator-of (vacmp-omega varray)))
               (out-dims (shape-of varray))
               (axis (or (vads-axis varray) (1- (length odims))))
               (rlen (nth axis odims))
               (increment (reduce #'* (nthcdr (1+ axis) odims)))
               (window (vacmp-alpha varray))
               (window-reversed (and window (> 0 window)))
               (window (when window (abs window)))
               (wsegment)
               (non-nested (not (eq t (etype-of (vacmp-omega varray)))))
               (fn-meta (funcall (vacmp-left varray) :get-metadata nil))
               ;; check whether fn-meta is a list since train-composed functions and some
               ;; others won't return a metadata list; TODO: a better way to do this?
               (scalar-fn (and (listp fn-meta)
                               (getf fn-meta :scalar)
                               (not (getf fn-meta :operator-reference))))
               (catenate-fn (and (listp fn-meta)
                                 (getf fn-meta :lexical-reference)
                                 (not (getf fn-meta :operator-reference))
                                 (member (getf fn-meta :lexical-reference)
                                         '(#\, #\⍪ #\∩) :test #'char=)))
               (ax-interval (or window rlen))
               (increment-diff (- (* increment rlen) increment))
               ;; TODO: create a better way to determine the catenate function
               )
          (loop :for dim :in odims :for dx :from 0
                :when (and window (= dx axis))
                  :do (setq wsegment (- dim (1- window))))
          ;; (print (list :ws rlen (funcall (vacmp-left varray) :get-metadata nil)))
          (cond
            ((and scalar-fn (typep (vacmp-omega varray) 'vapri-integer-progression))
             (get-reduced (vacmp-omega varray) (vacmp-left varray)))
            ((and (or scalar-fn (and catenate-fn (not window)))
                  (not out-dims) (arrayp (vacmp-omega varray)))
             ;; reverse the argument vector in the case of a scalar function;
             ;; this also applies in the case of the next two clauses
             (lambda (i)
               (declare (optimize (safety 1)))
               (let* ((output (funcall (vacmp-left varray)
                                       :arg-vector (funcall (if scalar-fn #'reverse #'identity)
                                                            (vacmp-omega varray))))
                      (out-indexer (generator-of output)))
                 ;; pass the indexer through for a shapeless output as from +/⍳5;
                 ;; pass the output object through for an output with a shape as from +/(1 2 3)(4 5 6)
                 (if (shape-of output)
                     output (if (not (functionp out-indexer))
                                out-indexer (funcall out-indexer i))))))
            ((and (or scalar-fn (and catenate-fn (not window)))
                  (= (- axis (vads-io varray))
                     (length out-dims))
                  (arrayp (vacmp-omega varray)))
             ;; (print (list 11 axis out-dims (vacmp-omega varray)))
             (lambda (i)
               (declare (optimize (safety 1)))
               (funcall (vacmp-left varray)
                        :arg-vector (funcall (if scalar-fn #'reverse #'identity)
                                             (make-array
                                              rlen :element-type (etype-of (vacmp-omega varray))
                                              :displaced-to (vacmp-omega varray)
                                              :displaced-index-offset (* i rlen))))))
            (t (flet ((process-item (i ix delta)
                        (if (not (functionp omega-indexer))
                            omega-indexer (funcall omega-indexer (+ delta (* ix increment))))))
                 (if (or scalar-fn catenate-fn)
                     (lambda (i)
                       ;; in the case of a scalar function that can take entire vectors as an argument
                       (declare (optimize (safety 1)))
                       (let ((valix 0)
                             (value (make-array ax-interval))
                             (delta (+ (if window (* rlen (floor i wsegment))
                                           (if (= 1 increment)
                                               0 (* increment-diff (floor i increment))))
                                       (if (/= 1 increment) i
                                           (if window (if (>= 1 irank) i (mod i wsegment))
                                               (* i rlen))))))
                         ;; (print (list 13 axis out-dims (vacmp-omega varray) value valix))
                         
                         (if window-reversed
                             (loop :for ix :below window
                                   :do (setf (aref value (- ax-interval (incf valix)))
                                             (process-item i ix delta)))
                             (loop :for ix :from (1- ax-interval) :downto 0
                                   :do (setf (aref value (- ax-interval (incf valix)))
                                             (process-item i ix delta))))
                         (setf value (funcall (vacmp-left varray)
                                              :arg-vector (funcall (if scalar-fn #'reverse #'identity)
                                                                   value)))
                         value))
                     (lambda (i) ;; in the case of other functions
                       (declare (optimize (safety 1)))
                       (let ((value)
                             (delta (+ (if window (* rlen (floor i wsegment))
                                           (if (= 1 increment)
                                               0 (* (floor i increment)
                                                    (- (* increment rlen) increment))))
                                       (if (/= 1 increment) i
                                           (if window (if (>= 1 irank) i (mod i wsegment))
                                               (* i rlen))))))
                         ;; (print (list 13 axis out-dims (vacmp-omega varray) value valix))
                         ;; (print (list :vv value))
                         ;; (print (list :win window window-reversed))
                         (if window-reversed (loop :for ix :below window
                                                   :do (let ((item (process-item i ix delta)))
                                                         (setq value (if (not value) item
                                                                         (funcall (vacmp-left varray)
                                                                                  value item)))))
                             (loop :for ix :from (1- ax-interval) :downto 0
                                   :do (let ((item (process-item i ix delta)))
                                         (setq value (if (not value) item
                                                         (funcall (vacmp-left varray)
                                                                  value item))))))
                         (when (typep value 'vacomp-reduce)
                           (setf (vads-subrendering varray) nil))
                         value))))))))))

(defclass vacomp-each (vad-subrendering vader-composing)
  nil (:metaclass va-class)
  (:documentation "An each-composed array as with the [¨ each] operator."))

(defmethod prototype-of ((varray vacomp-each))
  0)

(defmethod shape-of ((varray vacomp-each))
  (get-promised (varray-shape varray)
                (let* ((oshape (shape-of (vacmp-omega varray)))
                       (ashape (shape-of (vacmp-alpha varray)))
                       (osize (reduce #'* oshape))
                       (asize (reduce #'* ashape)))
                  (if oshape (if (not ashape)
                                 oshape (if (= 1 osize)
                                            (if (zerop asize) oshape ashape)
                                            ;; if one size is 0, the shape is 0
                                            (if (zerop osize) (list 0)
                                                (if (or (= 1 asize)
                                                        (and (= (length oshape) (length ashape))
                                                             (loop :for o :in oshape :for a :in ashape
                                                                   :always (= o a))))
                                                    oshape (error "Shapes must match.")))))
                          ashape))))

(defmethod generator-of ((varray vacomp-each) &optional indexers params)
  (let ((this-shape (shape-of varray))
        (oshape (shape-of (vacmp-omega varray)))
        (ashape (shape-of (vacmp-alpha varray)))
        (oindexer (generator-of (vacmp-omega varray)))
        (aindexer (generator-of (vacmp-alpha varray)))
        (threaded (side-effect-free (vacmp-left varray))
                  ))
    ;; TODO: logic to determine threading needs work, then reenable it
    (unless threaded (setf (vacmp-threadable varray) nil))
    ;; (print (list :th threaded))
    (lambda (index)
      (if (vacmp-alpha varray)
          (funcall (vacmp-left varray)
                   (if (not (functionp oindexer))
                       oindexer (funcall oindexer (if oshape index 0)))
                   (if (not (functionp aindexer))
                       aindexer (funcall aindexer (if ashape index 0))))
          (funcall (vacmp-left varray)
                   (if (not (functionp oindexer))
                       oindexer (funcall oindexer (if oshape index 0))))))))

(defclass vacomp-produce (vad-subrendering vader-composing vad-with-io vad-with-default-axis)
  nil (:metaclass va-class)
  (:documentation "A reduce-composed array as with the [/ reduce] operator."))

(defmethod prototype-of ((varray vacomp-produce))
  (let ((this-indexer (generator-of varray)))
    (if (zerop (size-of varray))
        ;; TODO: should prototype reflect both arguments?
        (prototype-of (vacmp-alpha varray))
        (prototype-of (funcall this-indexer 0)))))

(defmethod shape-of ((varray vacomp-produce))
  (get-promised (varray-shape varray)
                (let ((left (vacmp-left varray))
                      (adims (shape-of (vacmp-alpha varray)))
                      (odims (shape-of (vacmp-omega varray))))
                  (if (eq :outer left) (append adims odims)
                      (append (butlast adims) (rest odims))))))

(defclass vader-subarray-displaced (vader-subarray)
  nil (:metaclass va-class)
  (:documentation "An element of a split array as from the [↓ split] function."))

(defmethod prototype-of ((varray vader-subarray-displaced))
  (declare (ignore params))
  (let* ((gen (generator-of varray))
         (first-item (if (not (functionp gen))
                         gen (funcall (generator-of varray) 0))))
    ;; (print (list :fi first-item))
    (if (varrayp first-item) (prototype-of first-item)
        (apl-array-prototype first-item))))

(defmethod generator-of ((varray vader-subarray-displaced) &optional indexers params)
  (declare (ignore params))
  (let ((interval (reduce #'* (shape-of varray)))
        (base-indexer (generator-of (vader-base varray) indexers params)))
    (if (not (functionp base-indexer))
        base-indexer (lambda (index)
                       (funcall base-indexer
                                (+ index (* interval (vasv-index varray))))))))

(defmethod generator-of ((varray vacomp-produce) &optional indexers params)
  (let* ((omega (vacmp-omega varray))
         (alpha (vacmp-alpha varray))
         (osize (size-of omega)) (asize (size-of alpha))
         (is-outer (eq :outer (vacmp-left varray)))
         (oindexer (generator-of omega))
         (aindexer (generator-of alpha))
         (orank (rank-of omega)) (arank (rank-of alpha))
         (oscalar (unless (shape-of omega)
                    (if (not (functionp oindexer))
                        oindexer (funcall oindexer 0))))
         (ascalar (unless (shape-of alpha)
                    (if (not (functionp aindexer))
                        aindexer (funcall aindexer 0)))))
    (if is-outer
        (lambda (index)
          ;; (print (list :ra (render (or oscalar (funcall oindexer (mod index osize))))
          ;;              (render (or ascalar (funcall aindexer (floor index osize))))))
          (funcall (vacmp-right varray) (or oscalar (funcall oindexer (mod index osize)))
                   (or ascalar (funcall aindexer (floor index osize)))))
        (let* ((odims (shape-of omega))
               (asegment (first (last (shape-of alpha))))
               (osegment (if (not (and asegment (first odims) (/= asegment (first odims))))
                             (first odims)
                             (error "To find an inner product the last dimension of the left array ~w"
                                    "must equal the first dimension of the right array.")))
               (ashape (when asegment (list asegment)))
               (oshape (when osegment (list osegment)))
               (ovectors (reduce #'* (rest odims))))
          ;; (print (list :om omega alpha (render omega) oshape ashape (shape-of varray)))
          (if (or (zerop osize) (zerop asize))
              (if (or (< 1 orank) (< 1 arank))
                  (lambda (i) (declare (ignore i)) (vector))
                  ;; inner product with an empty array of rank > 1 gives an empty vector
                  (or (let ((identity (getf (funcall (vacmp-left varray) :get-metadata nil)
                                            :id)))
                        (lambda (i)
                          (declare (ignore i))
                          (if (functionp identity) (funcall identity) identity)))
                      (error "Left operand given to [. inner product] has no identity.")))
              (lambda (index)
                (let ((avix (floor index ovectors))
                      (ovix (mod index ovectors)))
                  (if (= 0 orank arank)
                      (funcall (vacmp-right varray) omega alpha)
                      (make-instance
                       'vader-pick
                       :base (make-instance 'vacomp-reduce
                                            :left (vacmp-left varray) :index-origin 1
                                            :omega
                                            (make-instance
                                             'vacomp-each
                                             :left (vacmp-right varray)
                                             :omega (if (not (functionp oindexer))
                                                        oindexer
                                                        (make-instance
                                                         'vader-subarray-reduce
                                                         :delta ovix :index ovectors
                                                         :base omega :shape oshape))
                                             :alpha (if (not (functionp aindexer))
                                                        aindexer
                                                        (make-instance 'vader-subarray-displaced
                                                                       :base alpha :index avix
                                                                       :shape ashape)))))))))))))

(defclass vacomp-stencil (vad-subrendering vader-composing vad-with-io)
  ((%base-dims :accessor vacst-base-dims
               :initform nil
               :initarg :base-dims
               :documentation "Vector of base array's dimensions.")
   (%win-dims :accessor vacst-win-dims
              :initform nil
              :initarg :win-dims
              :documentation "Dimensions for windows within stencil.")
   (%win-offsets :accessor vacst-win-offsets
                 :initform nil
                 :initarg :win-offsets
                 :documentation "Space offsets for windows within stencil.")
   (%win-factors :accessor vacst-win-factors
                 :initform nil
                 :initarg :win-factors
                 :documentation "Dimensional factors for stencil window.")
   (%movement :accessor vacst-movement
              :initform nil
              :initarg :movement
              :documentation "Stencil's movement parameters.")
   (%in-factors :accessor vacst-in-factors
                :initform nil
                :initarg :in-factors
                :documentation "Dimensional factors for stencil input.")
   (%out-factors :accessor vacst-out-factors
                 :initform nil
                 :initarg :out-factors
                 :documentation "Dimensional factors for stencil output."))
  (:metaclass va-class)
  (:documentation "A stencil-composed array as with the [⌺ stencil] operator."))

(defmethod prototype-of ((varray vacomp-stencil))
  (let ((this-indexer (generator-of varray)))
    (if t ; (zerop (size-of varray))
        ;; TODO: should prototype reflect both arguments?
        (prototype-of (vacmp-omega varray))
        ;; (prototype-of (funcall this-indexer 0))
        )))

(defmethod shape-of ((varray vacomp-stencil))
  (get-promised
   (varray-shape varray)
   (let* ((right-value (render (vacmp-right varray)))
          (left-function (vacmp-left varray))
          (omega-shape (shape-of (vacmp-omega varray)))
          (omega-rank (rank-of (vacmp-omega varray))))
     (flet ((get-row (matrix index)
              (let ((row-length (first (last (shape-of matrix)))))
                (coerce (loop :for x :below row-length
                              :collect (row-major-aref matrix (+ x (* index row-length))))
                        'vector))))
       (if (not (or (and (< 2 (rank-of right-value))
                         (error "The right operand of [⌺ stencil] may not have more than 2 dimensions."))
                    (and (not left-function)
                         (error "The left operand of [⌺ stencil] must be a function."))))
           (let ((idims (shape-of (vacmp-omega varray)))
                 (window-dims (if (not (arrayp right-value))
                                  (vector right-value)
                                  (if (= 1 (rank-of right-value))
                                      right-value (get-row right-value 0))))
                 (movement (if (not (arrayp right-value))
                               (vector 1)
                               (if (= 2 (rank-of right-value)) (get-row right-value 1)
                                   (make-array (length right-value) :element-type 'fixnum
                                                                    :initial-element 1)))))
             (setf (vacst-movement varray)
                   (if (= omega-rank (length movement))
                       movement (coerce (loop :for s :in omega-shape :for i :from 0
                                              :collect (if (>= i (length window-dims))
                                                           0 (aref movement i)))
                                        'vector))
                   (vacst-win-dims varray)
                   (if (= omega-rank (length window-dims))
                       window-dims (coerce (loop :for s :in omega-shape :for i :from 0
                                                 :collect (if (>= i (length window-dims))
                                                              s (aref window-dims i)))
                                           'vector))
                   (vacst-base-dims varray) (coerce omega-shape 'vector))
             (loop :for dim :below (length window-dims) :for dx :from 0
                   :collect (ceiling (- (/ (nth dim idims) (aref movement dim))
                                        (if (and (evenp (aref window-dims dim))
                                                 (or (= 1 (aref movement dim))
                                                     (oddp (nth dim idims))))
                                            1 0))))))))))

(defclass vader-stencil-window (varray-derived)
  ((%index :accessor vastw-index
           :initform nil
           :initarg :index
           :documentation "Index of stencil window."))
  (:metaclass va-class)
  (:documentation "A stencil window, part of a stencil array produced by [⌺ stencil]."))

(defmethod prototype-of ((varray vader-stencil-window))
  (prototype-of (vacmp-omega (vader-base varray)))) ;; TODO: fix this to proto from individual frame

(defmethod generator-of ((varray vader-stencil-window) &optional indexers params)
  (let* ((base-indexer (generator-of (vacmp-omega (vader-base varray))))
         (idims (apply #'vector (shape-of (vacmp-omega (vader-base varray)))))
         (this-rank (rank-of varray))
         (prototype (prototype-of varray))
         (movement (vacst-movement (vader-base varray)))
         (in-factors (vacst-in-factors (vader-base varray)))
         (win-factors (vacst-win-factors (vader-base varray)))
         (win-offsets (vacst-win-offsets (vader-base varray)))
         (oindices (make-array this-rank :element-type 'fixnum :initial-element 0)))

    (let ((remaining (vastw-index varray)))
      (loop :for of :across (vacst-out-factors (vader-base varray)) :for i :from 0
            :do (multiple-value-bind (index remainder) (floor remaining of)
                  (setf (aref oindices i) index
                        remaining remainder))))
    
    (lambda (index)
      (let ((remaining index) (rmi 0) (valid t) (ox 0))
        (loop :while valid :for mv :across movement :for idim :across idims
              :for if :across in-factors :for wf :across win-factors :for wo :across win-offsets
              :do (multiple-value-bind (this-index remainder) (floor remaining wf)
                    (let* ((static (zerop mv))
                           (a-index (+ this-index (if static 0 (- (* mv (aref oindices ox)) wo)))))
                      ;; 0 if no borders needed
                      (incf rmi (* if a-index))
                      (setf remaining remainder)
                      (unless static
                        (incf ox)
                        (unless (< -1 a-index idim)
                          (setf valid nil))))))
        (if (not valid) prototype (funcall base-indexer rmi))))))

(defclass vader-stencil-margin (varray-derived vad-with-argument)
  ((%index :accessor vaste-index
           :initform nil
           :initarg :index
           :documentation "Index of stencil margin vector."))
  (:metaclass va-class)
  (:documentation "A stencil margin vector, enumerating the margin padding in a stencil window array produced by [⌺ stencil]."))

(defmethod prototype-of ((varray vader-stencil-margin))
  (declare (ignore varray))
  0)

(defmethod etype-of ((varray vader-stencil-margin))
  (declare (ignore varray))
  ;; '(signed-byte 8)
  'fixnum
  ) ;; 8-bit elements for efficiency - TODO: is a different type better?

(defmethod generator-of ((varray vader-stencil-margin) &optional indexers params)
  (let* ((base-dims (vacst-base-dims (vader-base varray)))
         (win-index (vaste-index varray))
         (win-dims (vacst-win-dims (vader-base varray)))
         (win-factors (vacst-win-factors (vader-base varray)))
         (win-offsets (vacst-win-offsets (vader-base varray))))
    (lambda (index)
      (let ((from-start (- (* (if (zerop index) (floor win-index (aref win-factors index))
                                  (mod win-index (aref win-dims index)))
                              (aref (vacst-movement (vader-base varray)) index))
                           (aref win-offsets index))))
        (- (if (not (zerop (min 0 from-start)))
               from-start (max 0 (- (+ from-start (aref win-dims index))
                                    (aref base-dims index)))))))))

(defmethod generator-of ((varray vacomp-stencil) &optional indexers params)
  (setf (vacmp-omega varray) (render (vacmp-omega varray)))
  (let* ((irank (rank-of (vacmp-omega varray)))
         (idims (apply #'vector (shape-of (vacmp-omega varray))))
         (this-shape (shape-of varray)) ;; must derive shape before fetching window-dims
         (this-rank (rank-of varray))
         (window-dims (vacst-win-dims varray)) (wrank (length window-dims))
         (win-offsets (or (vacst-win-offsets varray)
                          (setf (vacst-win-offsets varray)
                                (make-array wrank :element-type 'fixnum :initial-element 0))))
         (in-factors (or (vacst-in-factors varray)
                         (setf (vacst-in-factors varray)
                               (make-array irank :element-type 'fixnum :initial-element 0))))
         (out-factors (or (vacst-out-factors varray)
                          (setf (vacst-out-factors varray)
                                (make-array this-rank :element-type 'fixnum :initial-element 0))))
         (win-factors (or (vacst-win-factors varray)
                          (setf (vacst-win-factors varray)
                                (make-array wrank :element-type 'fixnum :initial-element 0))))
         (edge-shape (list (rank-of varray)))
         (wd-list) (last-dim))
    
    ;; generate factor vector for window dimensions
    (loop :for dx :below (length window-dims)
          :do (let ((d (aref window-dims (- wrank 1 dx))))
                (setf (aref win-factors (- wrank 1 dx))
                      (if (zerop dx) 1 (* last-dim (aref win-factors (- wrank dx))))
                      wd-list (cons d wd-list)
                      last-dim d)))

    ;; generate dimensional factors vector for input
    (loop :for dx :below irank
          :do (let ((d (aref idims (- irank 1 dx))))
                (setf (aref in-factors (- irank 1 dx))
                      (if (zerop dx) 1 (* last-dim (aref in-factors (- irank dx))))
                      last-dim d)))
    
    ;; generate dimensional factors vector for output
    (loop :for d :in (reverse this-shape) :for dx :from 0
          :do (setf (aref out-factors (- this-rank 1 dx))
                    (if (zerop dx) 1 (* last-dim (aref out-factors (- this-rank dx))))
                    last-dim d))

    ;; generate offsets determining 0-spacing at edges
    (loop :for wd :across window-dims :for i :from 0
          :do (setf (aref win-offsets i) (floor (- wd (if (evenp wd) 1 0)) 2)))

    (lambda (index)
      (funcall (vacmp-left varray) (make-instance 'vader-stencil-window
                                                  :shape wd-list :base varray :index index)
               (make-instance 'vader-stencil-margin :shape edge-shape :base varray :index index)))))

;; (1 2 3) (2 3 4)∘.⌽[1]⊂3 3⍴⍳9 NOT IN DYALOG?
