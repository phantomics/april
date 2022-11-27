;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; functions.lisp

(in-package #:april)

(defun count-to (index index-origin)
  "Implementation of APL's [⍳ index] function."
  (let ((index (disclose index)))
    (if (or (and (integerp index) (>= index 0))
            (and (vectorp index)
                 (= 1 (length index))))
        (let ((index (if (not (vectorp index)) index (row-major-aref index 0))))
          (if (zerop index) (vector)
              (let ((output (make-array index :element-type (list 'integer 0 (+ index-origin index)))))
                (xdotimes output (i index) (setf (aref output i) (+ i index-origin)))
                output)))
        (if (vectorp index)
            (let ((output (make-array (array-to-list index))))
              (across output (lambda (elem coords)
                               (declare (ignore elem))
                               (setf (apply #'aref output coords)
                                     (make-array (length index)
                                                 :element-type
                                                 (list 'integer 0 (+ index-origin (reduce #'max coords)))
                                                 :initial-contents
                                                 (if (zerop index-origin)
                                                     coords (loop :for c :in coords
                                                               :collect (+ c index-origin)))))))
              output)
            (error "The argument to [⍳ index] must be a positive integer, i.e. ⍳9, or a vector, i.e. ⍳2 3.")))))

(defun inverse-count-to (vector index-origin)
  "The [⍳ index] function inverted; it returns the length of a sequential integer array starting from the index origin or else throws an error."
  (if (not (vectorp vector))
      (error "Inverse [⍳ index] can only be invoked on a vector, at least for now.")
      (if (loop :for e :across vector :for i :from index-origin :always (= e i))
          (length vector) (error "The argument to inverse [⍳ index] is not an index vector."))))

(defun shape (omega)
  "Get the shape of an array, implementing monadic [⍴ shape]."
  (if (or (not (arrayp omega))
          (zerop (rank omega)))
      #() (if (and (listp (type-of omega))
                   (eql 'simple-array (first (type-of omega)))
                   (eq t (second (type-of omega)))
                   (eq nil (third (type-of omega))))
              0 (if (vectorp omega)
                    (make-array 1 :element-type (list 'integer 0 (length omega))
                                :initial-contents (list (length omega)))
                    (let* ((omega-dims (dims omega))
                           (max-dim (reduce #'max omega-dims)))
                      (make-array (length omega-dims)
                                  :initial-contents omega-dims :element-type (list 'integer 0 max-dim)))))))

(defun reshape-array ()
  "Wrap (aplesque:reshape-to-fit) so that dyadic [⍴ shape] can be implemented with the use of empty-array prototypes."
  (lambda (omega alpha)
    (let ((output (reshape-to-fit omega (if (arrayp alpha) (array-to-list alpha)
                                            (list alpha))
                                  :populator (build-populator omega))))
      (if (and (zerop (size output)) (arrayp omega)
               (< 0 (size omega)) (arrayp (row-major-aref omega 0)))
          (array-setting-meta output :empty-array-prototype
                              (make-prototype-of (row-major-aref omega 0)))
          output))))

(defun shape (omega)
  "Get the shape of an array, implementing monadic [⍴ shape]."
  (if (or (not (arrayp omega))
          (zerop (rank omega)))
      #() (if (and (listp (type-of omega))
                   (eql 'simple-array (first (type-of omega)))
                   (eq t (second (type-of omega)))
                   (eq nil (third (type-of omega))))
              0 (if (vectorp omega)
                    (make-array 1 :element-type (list 'integer 0 (length omega))
                                :initial-contents (list (length omega)))
                    (let* ((omega-dims (dims omega))
                           (max-dim (reduce #'max omega-dims)))
                      (make-array (length omega-dims)
                                  :initial-contents omega-dims :element-type (list 'integer 0 max-dim)))))))

(defun reshape-array ()
  "Wrap (aplesque:reshape-to-fit) so that dyadic [⍴ shape] can be implemented with the use of empty-array prototypes."
  (lambda (omega alpha)
    (let ((output (reshape-to-fit omega (if (arrayp alpha) (array-to-list alpha)
                                            (list alpha))
                                  :populator (build-populator omega))))
      (if (not (and (zerop (size output)) (arrayp omega)
                    (< 0 (size omega)) (arrayp (row-major-aref omega 0))))
          output (array-setting-meta output :empty-array-prototype
                                     (make-prototype-of (row-major-aref omega 0)))))))

(defun find-depth (omega)
  "Find the depth of an array, wrapping (aplesque:array-depth). Used to implement [≡ depth]."
  (if (not (arrayp omega))
      0 (array-depth omega)))

(defun array-compare-wrap (comparison-tolerance)
  "Wrap the array-compare function, providing a comparison tolerance."
  (lambda (omega alpha) (array-compare omega alpha comparison-tolerance)))

(defun find-first-dimension (omega)
  "Find the first dimension of an array. Used to implement [≢ first dimension]."
  (if (zerop (rank omega))
      1 (first (dims omega))))

(defun membership (omega alpha)
  "Determine if elements of alpha are present in omega. Used to implement dyadic [∊ membership]."
  (flet ((compare (item1 item2)
           (if (and (characterp item1) (characterp item2))
               (char= item1 item2)
               (if (and (numberp item1) (numberp item2))
                   (= item1 item2)
                   (if (and (arrayp item1) (arrayp item2))
                       (array-compare item1 item2))))))
    (let ((to-search (if (vectorp omega)
                         omega (if (arrayp omega)
                                   (make-array (array-total-size omega)
                                               :displaced-to omega :element-type (element-type omega))
                                   omega))))
      (if (not (arrayp alpha))
          (if (not (arrayp omega))
              (if (compare omega alpha) 1 0)
              (if (not (loop :for item :across to-search :never (compare item alpha)))
                  1 0))
          (let* ((output (make-array (dims alpha) :element-type 'bit :initial-element 0))
                 (to-search (enclose-atom to-search)))
            ;; TODO: this could be faster with use of a hash table and other additions
            (xdotimes output (index (array-total-size output))
              (let ((found))
                (loop :for item :across to-search :while (not found)
                   :do (setq found (compare item (row-major-aref alpha index))))
                (if found (setf (row-major-aref output index) 1))))
            output)))))

(defun where-equal-to-one (omega index-origin)
  "Return a vector of coordinates from an array where the value is equal to one. Used to implement [⍸ where]."
  (let* ((indices) (match-count 0)
         (orank (rank omega)))
    (if (zerop orank)
        (if (= 1 omega) #(#()) #())
        (progn (across omega (lambda (index coords)
                               ;; (declare (dynamic-extent index coords))
                               (if (= 1 index)
                                   (let* ((max-coord 0)
                                          (coords (mapcar (lambda (i)
                                                            (setq max-coord
                                                                  (max max-coord (+ i index-origin)))
                                                            (+ i index-origin))
                                                          coords)))
                                     (incf match-count)
                                     (push (if (< 1 orank)
                                               (make-array orank :element-type (list 'integer 0 max-coord)
                                                                 :initial-contents coords)
                                               (first coords))
                                           indices)))))
               (if (not indices)
                   #() (make-array match-count :element-type (if (< 1 orank)
                                                                 t (list 'integer 0 (reduce #'max indices)))
                                   :initial-contents (reverse indices)))))))

(defun inverse-where-equal-to-one (omega index-origin)
  "Return a binary array given a vector of coordinate sub-vectors or indices indicating where values in the array are equal to 1."
  (let* ((is-scalar (not (arrayp (aref omega 0))))
         (rank (if is-scalar 1 (length (aref omega 0))))
         (dims (if is-scalar 0 (make-array rank :initial-element 0))))
    (loop :for o :across omega :do (if is-scalar
                                       (if (arrayp o)
                                           (error "All coordinate vectors in the argument to ⍸⍣¯1 ~a"
                                                  "must be of the same length.")
                                           (setf dims (max o dims)))
                                       (if (or (not (arrayp o))
                                               (/= rank (length o)))
                                           (error "All coordinate vectors in the argument to ⍸⍣¯1 ~a"
                                                  "must be of the same length.")
                                           (loop :for oi :across o :for r :below rank
                                                 :do (setf (aref dims r) (max oi (aref dims r)))))))
    (let ((output (make-array (array-to-list dims) :element-type 'bit)))
      (loop :for o :across omega :do (setf (varef output o index-origin) 1))
      output)))

(defun tabulate (omega)
  "Return a two-dimensional array of values from an array, promoting or demoting the array if it is of a rank other than two. Used to implement [⍪ table]."
  (let ((omega (render-varrays omega)))
    (if (not (arrayp omega))
        omega (if (vectorp omega)
                  (let ((output (make-array (list (length omega) 1) :element-type (element-type omega))))
                    (loop :for i :below (length omega) :do (setf (row-major-aref output i) (aref omega i)))
                    output)
                  (let ((o-dims (dims omega)))
                    (make-array (list (first o-dims) (reduce #'* (rest o-dims)))
                                :element-type (element-type omega)
                                :displaced-to (copy-nested-array omega)))))))

(defun ravel-array (index-origin axes)
  "Wrapper for aplesque [, ravel] function incorporating index origin from current workspace."
  (lambda (omega)
    (ravel index-origin (render-varrays omega)
           (mapcar #'render-varrays axes))))

(defun catenate-arrays (index-origin axes)
  "Wrapper for [, catenate] incorporating (aplesque:catenate) and (aplesque:laminate)."
  (lambda (omega alpha)
    (let ((axis (disclose-atom *first-axis-or-nil*)))
      (if (or (typep axis 'ratio)
              (and (floatp axis)
                   (< double-float-epsilon (nth-value 1 (floor axis)))))
          ;; laminate in the case of a fractional axis argument
          (laminate alpha omega (ceiling axis))
          ;; simply stack the arrays if there is no axis argument or it's an integer
          (catenate alpha omega (or (if axis (floor axis))
                                    (max 0 (1- (max (rank alpha) (rank omega))))))))))

(defun catenate-on-first (index-origin axes)
  "Wrapper for [⍪ catenate first]; distinct from (catenate-arrays) because it does not provide the laminate functionality."
  (lambda (omega alpha)
    (if (and (vectorp alpha) (vectorp omega))
        (if (and *first-axis-or-nil* (< 0 *first-axis-or-nil*))
            (error (concatenate 'string "Specified axis is greater than 1, vectors"
                                " have only one axis along which to catenate."))
            (if (and axes (> 0 *first-axis-or-nil*))
                (error (format nil "Specified axis is less than ~a." index-origin))
                (catenate alpha omega 0)))
        (when (or (not axes) (integerp (first axes)))
          (catenate alpha omega (or *first-axis-or-nil* 0))))))

(defun mix-array (index-origin axes)
  "Wrapper for (aplesque:mix) used for [↑ mix]."
  (lambda (omega) ; &optional axes)
    (let ((omega (render-varrays omega))) ;; IPV-TODO: remove-render
    (mix-arrays (if axes (- (ceiling (first axes)) index-origin)
                    (rank omega))
                omega :populator (lambda (item)
                                   (let ((populator (build-populator item)))
                                     (when populator (funcall populator))))))))

(defun wrap-split-array (index-origin axes)
  "Wrapper for [↓ split]."
  (lambda (omega) (split-array (render-varrays omega) ;; IPV-TODO: remove-render
                               *last-axis*)))

(defun section-array (index-origin &optional inverse axes)
  "Wrapper for (aplesque:section) used for [↑ take] and [↓ drop]."
  (lambda (omega alpha)
    (let* ((alpha (if (arrayp alpha)
                      alpha (vector alpha)))
           (output (section omega
                            (if axes (let ((dims (make-array
                                                  (rank omega)
                                                  :initial-contents (if inverse (loop :for i :below (rank omega)
                                                                                   :collect 0)
                                                                        (dims omega))))
                                           (spec-axes (first axes)))
                                       (if (integerp spec-axes)
                                           (setf (aref dims (- spec-axes index-origin)) (aref alpha 0))
                                           (when (vectorp spec-axes)
                                             (loop :for ax :across spec-axes :for ix :from 0
                                                   :do (setf (aref dims (- ax index-origin))
                                                             (aref alpha ix)))))
                                       dims)
                                alpha)
                            :inverse inverse :populator (build-populator omega))))
      ;; if the resulting array is empty and the original array prototype was an array, set the
      ;; empty array prototype accordingly
      (if (and (zerop (size output)) (not inverse)
               (arrayp omega) (when (< 0 (size omega))
                                (arrayp (row-major-aref omega 0))))
          (array-setting-meta output :empty-array-prototype
                              (make-prototype-of (row-major-aref omega 0)))
          output))))

(defun enclose-array (index-origin axes)
  "Wrapper for [⊂ enclose]."
  (lambda (omega &optional alpha)
    (if alpha (let ((output (partitioned-enclose alpha omega *last-axis*)))
                (if (not (and (vectorp output) (zerop (length output))))
                    output (array-setting-meta output :empty-array-prototype (vector))))
        (if axes (re-enclose omega (aops:each (lambda (axis) (- axis index-origin))
                                              (if (arrayp (first axes))
                                                  (first axes)
                                                  (vector (first axes)))))
            (enclose omega)))))

(defun partition-array-wrap (index-origin axes)
  "Wrapper for [⊆ partition]."
  (lambda (omega alpha)
    (partition-array alpha omega *last-axis*)))

(defun pick (index-origin)
  "Fetch an array element, within successively nested arrays for each element of the left argument."
  (lambda (omega alpha)
    (labels ((pick-point (point input)
               (if (is-unitary point)
                   (let ((point (disclose point)))
                     ;; if this is the last level of nesting specified, fetch the element
                     (if (not (arrayp point))
                         (if (not (arrayp input))
                             (if (zerop point) input (error "Coordinates for a scalar can only be 0."))
                             (aref input (- point index-origin)))
                         (if (vectorp point)
                             (apply #'aref input (loop :for p :across point :collect (- p index-origin)))
                             (error "Coordinates for ⊃ must be expressed by scalars or vectors."))))
                   ;; if there are more elements of the left argument left to go,
                   ;; recurse on the element designated by the first element of the
                   ;; left argument and the remaining elements of the point
                   (pick-point (if (< 2 (length point))
                                   (make-array (1- (length point))
                                               :initial-contents (loop :for i :from 1 :to (1- (length point))
                                                                    :collect (aref point i)))
                                   (aref point 1))
                               (disclose (pick-point (aref point 0) input))))))
      ;; TODO: swap out the vector-based point for an array-based point
      (pick-point alpha omega))))

(defun array-intersection (omega alpha)
  "Return a vector of values common to two arrays. Used to implement [∩ intersection]."
  (let ((omega (enclose-atom omega))
        (alpha (enclose-atom alpha)))
    (if (or (not (vectorp alpha))
            (not (vectorp omega)))
        (error "Arguments to [∩ intersection] must be vectors.")
        (let* ((match-count 0)
               (matches (loop :for item :across alpha :when (find item omega :test #'array-compare)
                           :collect item :and :do (incf match-count))))
          (make-array (list match-count) :initial-contents matches
                      :element-type (type-in-common (element-type alpha) (element-type omega)))))))

(defun unique (omega)
  "Return a vector of unique values in an array. Used to implement [∪ unique]."
  (if (not (arrayp omega))
      (vector omega)
      (if (zerop (rank omega))
          (vector omega)
          (let ((vector (if (vectorp omega)
                            omega (re-enclose omega (make-array (max 0 (1- (rank omega)))
                                                                :element-type 'fixnum
                                                                :initial-contents
                                                                (loop :for i :from 1 :to (1- (rank omega))
                                                                   :collect i))))))
            (let ((uniques) (unique-count 0))
              (loop :for item :across vector :when (not (find item uniques :test #'array-compare))
                    :do (push item uniques)
                        (incf unique-count))
              (funcall (lambda (result) (if (vectorp omega) result (mix-arrays 1 result)))
                       (make-array unique-count :element-type (element-type vector)
                                   :initial-contents (reverse uniques))))))))

(defun array-union (omega alpha)
  "Return a vector of unique values from two arrays. Used to implement [∪ union]."
  (let ((omega (enclose-atom omega))
        (alpha (enclose-atom alpha)))
    (if (or (not (vectorp alpha))
            (not (vectorp omega)))
        (error "Arguments must be vectors.")
        (let* ((unique-count 0)
               (uniques (loop :for item :across omega :when (not (find item alpha :test #'array-compare))
                           :collect item :and :do (incf unique-count))))
          (catenate alpha (make-array unique-count :initial-contents uniques
                                      :element-type (type-in-common (element-type alpha)
                                                                    (element-type omega)))
                    0)))))

(defun unique-mask (array)
  "Return a 1 for each value encountered the first time in an array, 0 for others. Used to implement monadic [≠ unique mask]."
  (let ((output (make-array (first (dims array)) :element-type 'bit :initial-element 1))
        (displaced (if (< 1 (rank array)) (make-array (rest (dims array)) :displaced-to array
                                                      :element-type (element-type array))))
        (uniques) (increment (reduce #'* (rest (dims array)))))
    (dotimes (x (first (dims array)))
      (if (and displaced (< 0 x))
          (setq displaced (make-array (rest (dims array)) :element-type (element-type array)
                                      :displaced-to array :displaced-index-offset (* x increment))))
      (if (member (or displaced (aref array x)) uniques :test #'array-compare)
          (setf (aref output x) 0)
          (setf uniques (cons (if displaced (make-array (rest (dims array)) :displaced-to array
                                                        :element-type (element-type array)
                                                        :displaced-index-offset (* x increment))
                                  (aref array x))
                              uniques))))
    output))

(defun rotate-array (first-axis index-origin axes)
  "Wrapper for turn function, used to implement [⌽ rotate] and [⊖ rotate first]."
  (lambda (omega &optional alpha)
    (if first-axis (if alpha (turn omega *first-axis* alpha)
                       (turn omega *first-axis*))
        (if alpha (turn omega *last-axis* alpha)
            (turn omega *last-axis*)))))

(defun permute-array (index-origin)
  "Wraps (aplesque:permute-axes) to permute an array, rearranging the axes in a given order or reversing them if no order is given. Used to implement monadic and dyadic [⍉ permute]."
  (lambda (omega &optional alpha)
    (if (not (arrayp omega))
        omega (permute-axes
               omega (if alpha (if (not (arrayp alpha)) (- alpha index-origin)
                                   (if (vectorp alpha)
                                       (if (> (length alpha) (rank omega))
                                           (error "Length of left argument to ⍉ ~a"
                                                  "must be equal to rank of right argument.")
                                           (if (zerop index-origin)
                                               alpha (let ((adjusted (make-array (length alpha))))
                                                       (loop :for a :across alpha :for ax :from 0
                                                             :do (setf (aref adjusted ax) (- a index-origin)))
                                                       adjusted)))
                                       (error "Left argument to ⍉ must be a scalar or vector."))))))))

(defun expand-array (first-axis compress-mode index-origin axes)
  "Wrapper for (aplesque:expand) implementing [/ replicate] and [\ expand]."
  (lambda (omega alpha)
    (let* ((axis (if (first axes) (- (first axes) index-origin)
                     (if first-axis *first-axis* *last-axis*)))
           (output (expand alpha omega axis :compress-mode compress-mode
                                            :populator (build-populator omega))))
      (if (and (zerop (size output)) (arrayp omega) (not (zerop (size omega)))
               (arrayp (row-major-aref omega 0)))
          (array-setting-meta output :empty-array-prototype
                              (make-prototype-of (row-major-aref omega 0)))
          output))))

(defun matrix-inverse (omega)
  "Invert a matrix. Used to implement monadic [⌹ matrix inverse]."
  (if (not (arrayp omega))
      (/ omega)
      (if (< 2 (rank omega))
          (error "Matrix inversion only works on arrays of rank 2 or 1.")
          (funcall (if (and (= 2 (rank omega)) (reduce #'= (dims omega)))
                       #'invert-matrix #'left-invert-matrix)
                   omega))))

(defun matrix-divide (omega alpha)
  "Divide two matrices. Used to implement dyadic [⌹ matrix divide]."
  (array-inner-product (invert-matrix omega) alpha (lambda (arg1 arg2) (apply-scalar #'* arg1 arg2))
                       #'+ t))

(defun encode (omega alpha &optional inverse)
  "Encode a number or array of numbers as per a given set of bases. Used to implement [⊤ encode]."
  (if (and (vectorp alpha) (zerop (length alpha)))
      (make-array 0)
      (let* ((alpha (if (arrayp alpha)
                        alpha (if (not inverse)
                                  ;; if the encode is an inverted decode, extend a
                                  ;; scalar left argument to the appropriate degree
                                  alpha
                                  (let ((max-omega 0))
                                    (if (arrayp omega)
                                        (dotimes (i (size omega))
                                          (setq max-omega (max max-omega (row-major-aref omega i))))
                                        (setq max-omega omega))
                                    (if (zerop max-omega) #()
                                        (make-array (1+ (floor (log max-omega) (log alpha)))
                                                    :initial-element alpha))))))
             (odims (dims omega)) (adims (dims alpha))
             (osize (size omega)) (asize (size alpha))
             (out-dims (append (loop :for dim :in adims :collect dim)
                               (loop :for dim :in odims :collect dim)))
             ;; currently, the output is set to t because due to the cost of finding the highest array value
             (output (if out-dims (make-array out-dims)))
             (aseg-last (reduce #'* (butlast adims 1)))
             (aseg-first (reduce #'* (rest adims)))
             (ofactor (* osize aseg-first))
             (aifactors (get-dimensional-factors adims))
             (oifactors (get-dimensional-factors odims t))
             (ofactors (get-dimensional-factors out-dims t)))
        (dotimes (i (size output))
          (let ((remaining i) (base 1) (oindex 0) (afactor 0) (oix 0) (value))
            (loop :for af :in aifactors :for of :across ofactors :for ix :from 0
                  :do (multiple-value-bind (index remainder) (floor remaining of)
                        (incf oix)
                        (when (not (zerop ix))
                          (setf afactor (+ afactor (* af index))))
                        (setf remaining remainder)))
            (loop :for of :across oifactors
                  :do (multiple-value-bind (index remainder) (floor remaining (aref ofactors oix))
                        (incf oix)
                        (setf oindex (+ oindex (* of index))
                              remaining remainder)))
            
            (setf remaining i
                  value (if (not (arrayp omega))
                            omega (row-major-aref omega oindex)))
            
            (if adims (let ((last-base) (element) (aindex) (component 1)
                            (index (floor i (aref ofactors 0))))
                        (loop :for b :from (1- (first adims)) :downto index
                              :do (setq last-base base
                                        aindex (+ afactor (* b (first aifactors)))
                                        base (* base (row-major-aref alpha aindex))
                                        component (if (zerop base) value
                                                      (nth-value 1 (floor value base)))
                                        value (- value component)
                                        element (if (zerop last-base) 0
                                                    (floor component last-base))))
                        (setf value element))
                (setf value (nth-value 1 (floor value alpha))))
            (if output (setf (row-major-aref output i) value)
                (setf output value))))
        output)))

(defun decode (omega alpha)
  "Decode an array of numbers as per a given set of bases. Used to implement [⊥ decode]."
  (let* ((odims (dims omega)) (adims (dims alpha))
         (osize (size omega)) (asize (size alpha))
         (out-dims (append (butlast adims) (rest odims)))
         (output (if out-dims (make-array out-dims)))
         (ovector (or (first odims) 1))
         (afactors (make-array (if (and (< 1 osize) (and adims (< 1 (first (last adims)))))
                                   adims (append (butlast adims 1) (list ovector)))
                               :initial-element 1))
         (asegments (reduce #'* (butlast adims 1)))
         (av2 (or (if (and (< 1 osize)
                           (or (not adims)
                               (= 1 (first (last adims)))))
                      (first (dims omega))
                      (first (last adims)))
                  1))
         (out-section (reduce #'* (rest odims))))
    (if out-dims (progn (dotimes (a asegments)
                          (loop :for i :from (- (* av2 (1+ a)) 2) :downto (* av2 a)
                                :do (setf (row-major-aref afactors i)
                                          (* (if (not (arrayp alpha))
                                                 alpha
                                                 (row-major-aref
                                                  alpha (if (not (and (< 1 asize)
                                                                      (< 1 osize)
                                                                      (= 1 (first (last adims)))))
                                                            (1+ i)
                                                            (floor i (or (first odims) 1)))))
                                             (row-major-aref afactors (1+ i))))))
                        (xdotimes output (i (size output))
                          (let ((result 0))
                            (loop :for index :below av2
                                  :do (incf result (* (if (not (arrayp omega))
                                                          omega
                                                          (row-major-aref
                                                           omega (mod (+ (mod i out-section)
                                                                         (* out-section index))
                                                                      (size omega))))
                                                      (row-major-aref afactors
                                                                      (+ index
                                                                         (* av2 (floor i out-section)))))))
                            (setf (row-major-aref output i) result))))
        (let ((result 0) (factor 1))
          (loop :for i :from (1- (if (< 1 av2) av2 ovector)) :downto 0
                :do (incf result (* factor (if (not (arrayp omega))
                                               omega (row-major-aref omega (min i (1- ovector))))))
                    (setq factor (* factor (if (not (arrayp alpha))
                                               alpha (row-major-aref alpha (min i (1- av2)))))))
          (setq output result)))
    output))

(defun operate-reducing (function index-origin last-axis &key axis)
  "Reduce an array along a given axis by a given function, returning function identites when called on an empty array dimension. Used to implement the [/ reduce] operator."
  (lambda (omega &optional alpha environment)
    (declare (ignore environment))
    (setq omega (render-varrays omega)
          alpha (render-varrays alpha)
          axis (if axis (list (render-varrays (first axis)))))
    (let ((fn-rendered (lambda (o a) (render-varrays (funcall function o a)))))
      (if (not (arrayp omega))
          (if (eq :get-metadata omega)
              (list :on-axis (or axis (if last-axis :last :first))
                    :valence :ambivalent)
              (if (eq :reassign-axes omega)
                  (operate-reducing function index-origin last-axis :axis alpha)
                  (if alpha (error "Left argument (window) passed to reduce-composed ~a"
                                   "function when applied to scalar value.")
                      omega)))
          (if (zerop (size omega))
              (let* ((output-dims (loop :for d :in (dims omega) :for dx :from 0
                                        :when (/= dx (or axis (if (not last-axis)
                                                                  0 (1- (rank omega)))))
                                          :collect d))
                     (empty-output (loop :for od :in output-dims :when (zerop od) :do (return t)))
                     (identity (getf (funcall function :get-metadata nil) :id)))
                (if output-dims ;; if reduction produces an empty array just return that array
                    (if empty-output (make-array output-dims)
                        (if identity
                            (let ((output (make-array output-dims)))
                              ;; if reduction eliminates an empty axis to create a non-empty array,
                              ;; populate it with the function's identity value
                              (xdotimes output (i (size output))
                                (setf (row-major-aref output i)
                                      (enclose (if (functionp identity)
                                                   (funcall identity) identity))))
                              output)
                            (error "The operand of [/ reduce] has no identity value.")))
                    ;; if reduction produces a scalar, the result is the identity value
                    (or (and identity (if (functionp identity) (funcall identity) identity))
                        (error "The operand of [/ reduce] has no identity value."))))
              (render-varrays (reduce-array omega fn-rendered (when (first axis)
                                                                (- (first axis) index-origin))
                                            (side-effect-free function)
                                            last-axis alpha)))))))

(defun operate-at-rank (rank function)
  "Generate a function applying a function to sub-arrays of the arguments. Used to implement [⍤ rank]."
  (lambda (omega &optional alpha environment blank)
    (declare (ignore environment blank))
    (setq omega (render-varrays omega)
          alpha (render-varrays alpha))
    (let* ((odims (dims omega)) (adims (dims alpha))
           (orank (rank omega)) (arank (rank alpha))
           (fn-meta (funcall function :get-metadata alpha))
           ;; if alpha is nil the monadic metadata will be fetched, otherwise the dyadic data will be
           (rank (if (not (arrayp rank))
                     (if (> 0 rank) ;; handle a negative rank as for ,⍤¯1⊢2 3 4⍴⍳24
                         (make-array 3 :initial-contents (list (max 0 (+ rank orank))
                                                               (max 0 (+ rank (if alpha arank orank)))
                                                               (max 0 (+ rank orank))))
                         (make-array 3 :initial-element rank))
                     (if (= 1 (size rank))
                         (make-array 3 :initial-element (row-major-aref rank 0))
                         (if (= 2 (size rank))
                             (make-array 3 :initial-contents (list (aref rank 1) (aref rank 0) (aref rank 1)))
                             (if (= 3 (size rank))
                                 rank (if (or (< 1 (rank rank)) (< 3 (size rank)))
                                          (error "Right operand of [⍤ rank] must be a scalar integer or ~a"
                                                 "integer vector no more than 3 elements long.")))))))
           (ocrank (aref rank 2))
           (acrank (aref rank 1))
           (omrank (aref rank 0))
           (orankdelta (- orank (if alpha ocrank omrank)))
           (odivs (if (<= 0 orankdelta) (make-array (subseq odims 0 orankdelta))))
           (odiv-dims (if odivs (subseq odims orankdelta)))
           (odiv-size (if odivs (reduce #'* odiv-dims)))
           (arankdelta (- arank acrank))
           (adivs (if (and alpha (<= 0 arankdelta))
                      (make-array (subseq adims 0 arankdelta))))
           (adiv-dims (if adivs (subseq adims arankdelta)))
           (adiv-size (if alpha (reduce #'* adiv-dims))))
      (if (and alpha (eq :monadic (getf fn-meta :valence)))
          (error "Function composed with [⍤ rank] may not have a left argument."))
      (if (and (not alpha) (eq :dyadic (getf fn-meta :valence)))
          (error "Function composed with [⍤ rank] must have a left argument."))
      (if (eq omega :get-metadata)
          (append fn-meta (list :composed-by #\⍤))
          (if (and (getf fn-meta :on-axis)
                   (= 1 (if alpha ocrank omrank)))
              ;; if the composed function is directly equivalent to a function that operates
              ;; across an axis, as ⊖⍤1 and ⌽⍤1 are to ⌽, just reassign the axis
              (apply (if (eq :last (getf fn-meta :on-axis))
                         function (funcall function :reassign-axes (list (rank omega))))
                     omega (if alpha (list alpha)))
              (flet ((generate-divs (div-array ref-array div-dims div-size)
                       (xdotimes div-array (i (size div-array))
                         (setf (row-major-aref div-array i)
                               (if (zerop (rank div-array)) ref-array
                                   (if (not div-dims) (row-major-aref ref-array i)
                                       (make-array div-dims :element-type (element-type ref-array)
                                                            :displaced-to ref-array
                                                            :displaced-index-offset (* i div-size))))))))
                (if odivs (generate-divs odivs omega odiv-dims odiv-size))
                (if alpha (progn (if adivs (generate-divs adivs alpha adiv-dims adiv-size))
                                 (if (not (or odivs adivs))
                                     ;; if alpha and omega are scalar, just call the function on them
                                     (funcall function omega alpha)
                                     (let ((output (make-array (dims (or odivs adivs)))))
                                       (print (list :od odivs adivs))
                                       (xdotimes output (i (size output))
                                         (let ((this-odiv (if (not odivs)
                                                              omega (if (zerop (rank odivs))
                                                                        (aref odivs) (row-major-aref odivs i))))
                                               (this-adiv (if (not adivs)
                                                              alpha (if (zerop (rank adivs))
                                                                        (aref adivs) (row-major-aref adivs i)))))
                                           (setf (row-major-aref output i)
                                                 (disclose (render-varrays (funcall function this-odiv this-adiv))))))
                                       (print (list :ou output))
                                       (mix-arrays (max (rank odivs) (rank adivs))
                                                   output))))
                    (if (not odivs) ;; as above for an omega value alone
                        (funcall function omega)
                        (let ((output (make-array (dims odivs))))
                          (xdotimes output (i (size output) :synchronous-if (not (side-effect-free function)))
                            (setf (row-major-aref output i)
                                  (render-varrays (funcall function (row-major-aref odivs i)))))
                          (mix-arrays (rank output) output))))))))))

(defun operate-at (right left index-origin)
  "Generate a function applying a function at indices in an array specified by a given index or meeting certain conditions. Used to implement [@ at]."
  (let ((right (render-varrays right))
        (left (render-varrays left))
        (left-fn (when (functionp left)
                   (lambda (o &optional a)
                     (render-varrays (apply left o (when a (list a)))))))
        (right-fn (when (functionp right)
                    (lambda (o &optional a)
                      (render-varrays (apply right o (when a (list a))))))))
    (lambda (omega &optional alpha environment blank)
      (declare (ignorable alpha environment blank))
      (setq omega (render-varrays omega)
            alpha (render-varrays alpha))
      (if (and left-fn (or right-fn (or (vectorp right) (not (arrayp right)))))
          ;; if the right operand is a function, collect the right argument's matching elements
          ;; into a vector, apply the left operand function to it and assign its elements to their
          ;; proper places in the copied right argument array and return it
          (if right-fn
              (let ((true-indices (make-array (size omega) :initial-element 0))
                    (omega-copy (copy-array omega :element-type t)))
                (xdotimes true-indices (i (size omega))
                  (if (or (and right-fn (not (zerop (funcall right-fn (row-major-aref omega i)))))
                          (and (integerp right) (= i (- right index-origin)))
                          (and (arrayp right)
                               (not (loop :for r :below (size right) :never (= (row-major-aref right r)
                                                                               (+ i index-origin))))))
                      (incf (row-major-aref true-indices i))))
                (let ((tvix 0)
                      (true-vector (make-array (loop :for i :across true-indices :summing i)
                                               :element-type (element-type omega))))
                  (dotimes (i (size omega))
                    (if (not (zerop (row-major-aref true-indices i)))
                        (progn (setf (row-major-aref true-vector tvix)
                                     (row-major-aref omega i))
                               (incf (row-major-aref true-indices i) tvix)
                               (incf tvix))))
                  (let ((to-assign (if alpha (funcall left-fn true-vector alpha)
                                       (funcall left-fn true-vector))))
                    (xdotimes omega-copy (i (size omega))
                      (if (not (zerop (row-major-aref true-indices i)))
                          (setf (row-major-aref omega-copy i)
                                (if (= 1 (length true-vector))
                                    ;; if there is only one true element the to-assign value is
                                    ;; the value to be assigned, not a vector of values to assign
                                    (disclose to-assign)
                                    (row-major-aref to-assign (1- (row-major-aref true-indices i)))))))
                    omega-copy)))
              (let* ((omega-copy (copy-array omega :element-type t))
                     (indices-adjusted (if (not (arrayp right)) (- right index-origin)
                                           (apply-scalar (lambda (a) (- a index-origin)) right)))
                     (mod-array (choose omega (if (= 1 (rank omega)) (list indices-adjusted)
                                                  (cons indices-adjusted
                                                        (loop :for i :below (1- (rank omega))
                                                              :collect nil)))))
                     (out-sub-array (if alpha (funcall left-fn mod-array alpha)
                                        (funcall left-fn mod-array))))
                (choose omega-copy (if (= 1 (rank omega)) (list indices-adjusted)
                                       (cons indices-adjusted
                                             (loop :for i :below (1- (rank omega)) :collect nil)))
                        :modify-input t :set (funcall (if (and (not (arrayp right))
                                                               (> 2 (rank omega))
                                                               (not (zerop (rank out-sub-array))))
                                                          #'enclose #'identity)
                                                      out-sub-array))
                omega-copy))
          ;; if the right argument is an array of rank > 1, assign the left operand values or apply the
          ;; left operand function as per choose or reach indexing
          (if right-fn (let ((selections (funcall right-fn omega))
                             (output (make-array (dims omega))))
                         (if (/= (size omega) (size selections))
                             (error "Output of [@ at]'s right operand function must match ~n"
                                    " the shape of the left argument.")
                             (xdotimes output (i (size selections))
                               (setf (row-major-aref output i)
                                     (if (zerop (row-major-aref selections i))
                                         (row-major-aref omega i)
                                         (if left-fn
                                             (if alpha (funcall left-fn (row-major-aref omega i) alpha)
                                                 (funcall left-fn (row-major-aref omega i)))
                                             (if (not (arrayp left))
                                                 left (if (zerop (rank left))
                                                          (disclose left)
                                                          (row-major-aref left i))))))))
                         output)
              (nth-value
               1 (choose omega (append (list (apply-scalar #'- right index-origin))
                                       (loop :for i :below (- (rank omega) (array-depth right))
                                             :collect nil))
                         :set (if (not left-fn) left)
                         :set-by (if left-fn (lambda (old &optional new)
                                               (declare (ignorable new))
                                               (if alpha (funcall left-fn old alpha)
                                                   (funcall left-fn old)))))))))))
(defun operate-stenciling (right-value left-function)
  "Generate a function applying a function via (aplesque:stencil) to an array. Used to implement [⌺ stencil]."
  (lambda (omega &optional alpha environment blank)
    (declare (ignore alpha environment blank))
    (let ((stencil (make-instance 'vacomp-stencil :omega omega :right right-value :left left-function)))
      (make-instance 'vader-mix :base stencil :subrendering t :axis (varray::rank-of stencil)))))

;; From this point are optimized implementations of APL idioms.

(deftype fast-iota-sum-fixnum ()
  "The largest integer that can be supplied to fast-iota-sum without causing a fixnum overflow"
  '(integer 0 #.(isqrt (* 2 most-positive-fixnum))))

(declaim (ftype (function (fast-iota-sum-fixnum) fixnum) fast-iota-sum))
(defun fast-iota-sum (n)
  "Fast version of iota-sum for integers of type fast-iota-sum-fixnum"
  (declare (optimize (speed 3) (safety 0)))
  (if (oddp n)
      (* n (the fixnum (/ (1+ n) 2)))
    (let ((n/2 (the fixnum (/ n 2))))
      (+ (* n n/2) n/2))))

(defun iota-sum (n index-origin)
  "Fast implementation of +/⍳X."
  (cond ((< n 0)
	 (error "The argument to [⍳ index] must be a positive integer, i.e. ⍳9, or a vector, i.e. ⍳2 3."))
	((= n 0) 0)
	((= n 1) index-origin)
	((typep n 'fast-iota-sum-fixnum)
	 (if (= index-origin 1)
	     (fast-iota-sum n)
	     (fast-iota-sum (1- n))))
	(t (* n (/ (+ n index-origin index-origin -1) 2)))))

(defun iota-sum-array (array index-origin)
  (let* ((output (make-array (butlast (array-to-list array))))
	 (last (aref array (1- (length array))))
	 (last-sum (iota-sum last index-origin)))
    (across
     output
     (lambda (elm coords)
       (declare (ignore elm))
       (setf (apply #'aref output coords)
	     (concatenate 'vector
			  (map 'vector (lambda (x)
					 (* last (+ x index-origin)))
			       coords)
			  (vector last-sum)))))
    output))

(defun get-last-row-major (array)
  "Fast implementation of ⊃⌽,X."
  (if (not (arrayp array))
      array (row-major-aref array (1- (array-total-size array)))))

(defun get-rank (array)
  "Fast implementation of ⍴⍴X."
  (vector (rank array)))

(defun n-rank-uniques (array)
  "Fast implementation of ∪,X."
  (if (not (arrayp array))
      array (let ((raveled (make-array (size array) :element-type (element-type array)
                                                    :displaced-to array)))
              (unique raveled))))
