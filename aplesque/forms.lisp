;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Aplesque -*-
;;;; forms.lisp

(in-package #:aplesque)

"A set of functions defining the forms of arrays produced by the Aplesque array processing functions."

(defun indexer-split (axis orank input-factors output-factors)
  (lambda (outer inner)
    (let ((iindex 0) (remaining outer))
      (loop :for ofactor :across output-factors :for ix :from 0
            :do (multiple-value-bind (index remainder) (floor remaining ofactor)
                  (if (= ix axis)
                      (incf iindex (* inner (aref input-factors axis))))
                  (incf iindex (* index (aref input-factors (+ ix (if (< ix axis) 0 1)))))
                  (setq remaining remainder)))
      (if (= axis orank) (incf iindex inner))
      iindex)))

(defun indexer-section (inverse dims dimensions output-shorter)
  "Return indices of an array sectioned as with the [↑ take] or [↓ drop] functions."
  ;; (print (list :is inverse dims dimensions output-shorter))
  (let* ((isize (reduce #'* dims)) (irank (length dims))
         (rdiff (- irank (length dimensions)))
         (idims (make-array irank :element-type (if (zerop isize) t (list 'integer 0 isize))
                                  :initial-contents dims))
         (odims (loop :for odim :across dimensions :for idim :across idims
                      :collect (if (not inverse) (abs odim) (- idim (abs odim)))))
         (osize (reduce #'* odims))
         (last-dim)
         (id-factors (make-array irank :element-type 'fixnum))
         (od-factors (make-array irank :element-type 'fixnum)))
    ;; generate dimensional factors vectors for input and output
    (loop :for dx :below irank
          :do (let ((d (aref idims (- irank 1 dx))))
                (setf (aref id-factors (- irank 1 dx))
                      (if (zerop dx) 1 (* last-dim (aref id-factors (- irank dx))))
                      last-dim d)))

    (loop :for d :in (reverse odims) :for dx :from 0
          :do (setf (aref od-factors (- irank 1 dx))
                    (if (zerop dx) 1 (* last-dim (aref od-factors (- irank dx))))
                    last-dim d))
    (lambda (i)
      (if output-shorter
          ;; choose shorter path depending on whether input or output are larger, and
          ;; always iterate over output in the case of sub-7-bit arrays as this is necessary
          ;; to respect the segmentation of the elements
          (let ((oindex 0) (remaining i) (valid t))
            ;; calculate row-major offset for outer array dimensions
            (loop :for i :from 0 :to (- irank 1) :while valid
                  :for dim :across dimensions :for id :across idims :for od :in odims
                  :for ifactor :across id-factors :for ofactor :across od-factors
                  :do (multiple-value-bind (index remainder) (floor remaining ifactor)
                        (let ((adj-index (- index (if inverse (if (> 0 dim) 0 dim)
                                                      (if (< 0 dim) 0 (+ dim id))))))
                          (if (< -1 adj-index od)
                              (progn (incf oindex (* ofactor adj-index))
                                     (setq remaining remainder))
                              (setq valid nil)))))
            (if valid oindex))
          (let ((iindex 0) (remaining i) (valid t))
            ;; calculate row-major offset for outer array dimensions
            (loop :for i :from 0 :to (- irank 1) :while valid
                  :for dim :across dimensions :for id :across idims :for od :in odims
                  :for ifactor :across id-factors :for ofactor :across od-factors
                  :do (multiple-value-bind (index remainder) (floor remaining ofactor)
                        (let ((adj-index (+ index (if inverse (if (> 0 dim) 0 dim)
                                                      (if (< 0 dim) 0 (+ dim id))))))
                          (if (< -1 adj-index id)
                              (progn (incf iindex (* ifactor adj-index))
                                     (setq remaining remainder))
                              (setq valid nil)))))
            (if valid iindex))))))

(defun indexer-expand (degrees dims axis compress-mode is-inverse)
  "Return indices of an array expanded as with the [/ compress] or [\\ expand] functions."
  ;; TODO: more speedup is possible here in the case of a scalar degree argument
  (let* ((oned (if (not (arrayp degrees)) degrees))
         (degrees (if (arrayp degrees) degrees
                      (if (integerp degrees) (vector degrees))))
         (d-count (length degrees))
         (c-degrees (if degrees (make-array d-count :element-type 'fixnum :initial-element 0)))
         (positive-index-list (if (not compress-mode)
                                  (if oned (if (< 0 oned) (list 0))
                                      (loop :for degree :below d-count
                                            :when (< 0 (aref degrees degree)) :collect degree))))
         (positive-indices (if positive-index-list (make-array (length positive-index-list)
                                                               :element-type 'fixnum
                                                               :initial-contents positive-index-list)))
         (section-size (reduce #'* (loop :for d :in dims :for dx :from 0
                                         :when (> dx axis) :collect d))))
    (if degrees (loop :for degree :across degrees :for dx :from 0
                      :summing (max (abs degree) (if compress-mode 0 1))
                        :into this-dim :do (setf (aref c-degrees dx) this-dim)))
    (let* ((in-factors (if (and is-inverse (listp is-inverse))
                           (get-dimensional-factors dims t)))
           (asn-factors (if (and is-inverse (listp is-inverse))
                            (get-dimensional-factors is-inverse t)))
           (idiv-size (reduce #'* (loop :for d :in dims :for dx :from 0
                                        :when (>= dx axis) :collect d)))
           (odiv-size (reduce #'* (if dims (loop :for d :in dims :for dx :from 0
                                                 :when (> dx axis) :collect d :when (= dx axis)
                                                 :collect (if oned (* oned (nth axis dims))
                                                              (aref c-degrees (1- d-count))))
                                      (or (and oned (list oned))
                                          (loop :for d :across c-degrees :for dx :from 0
                                                :collect (abs d)))))))
      
      (lambda (i)
        ;; in compress-mode: degrees must = length of axis,
        ;; zeroes are omitted from output, negatives add zeroes
        ;; otherwise: zeroes pass through, negatives add zeroes, degrees>0 must = length of axis
        (if is-inverse
            (if (eq t is-inverse)
                (if (< 0 (aref degrees (mod i d-count))) i)
                (let ((remaining i) (aindex 0) (valid t))
                  ;; find assignment indices for valid input indices,
                  ;; as for x←6 8⍴⍳9 ⋄ ((30>+⌿x)/x)←6 4⍴10×⍳3 ⋄ x
                  (loop :for od :across in-factors
                        :for ad :across asn-factors :for ix :from 0 :while valid
                        :do (multiple-value-bind (item remainder) (floor remaining od)
                              (if (/= ix axis)
                                  (incf aindex (* ad item))
                                  (if (>= 0 (aref degrees item))
                                      (setf valid nil)
                                      (let ((dcount 0))
                                        (loop :for d :across degrees :for dx :below item
                                              :do (incf dcount (if (>= 0 d) 0 1)))
                                        (incf aindex (* ad dcount)))))
                              (setf remaining remainder)))
                  (if valid aindex)))
            (multiple-value-bind (oseg remainder) (floor i (max 1 odiv-size))
              (multiple-value-bind (oseg-index element-index) (floor remainder section-size)
                ;; dimension index
                (let ((dx (if oned (floor oseg-index (max 1 oned))
                              (loop :for d :across c-degrees :for di :from 0
                                    :when (> d oseg-index) :return di))))
                  (if (if oned (< 0 oned)
                          (< 0 (aref degrees dx)))
                      (+ element-index (* oseg idiv-size)
                         (* section-size (if (not positive-indices)
                                             dx (or (loop :for p :across positive-indices
                                                          :for px :from 0 :when (= p dx)
                                                          :return px)
                                                    1)))))))))))))

(defun indexer-enclose (axes input-dims)
  (let ((axis-list (array-to-list axes))
        (outer-dims) (inner-dims))
    (dotimes (axis (length input-dims))
      (if (find axis axis-list) (push axis inner-dims)
          (push axis outer-dims)))
    (setq inner-dims (reverse inner-dims)
          outer-dims (reverse outer-dims))
    ;; create a blank array of the outer dimensions containing blank arrays of the inner dimensions
    (let* ((ocoords (loop :for d :in outer-dims :collect (nth d input-dims)))
           (icoords (loop :for d :in inner-dims :collect (nth d input-dims)))
           (infactors (get-dimensional-factors input-dims))
           (inner-factors (get-dimensional-factors icoords))
           (outer-factors (get-dimensional-factors ocoords)))
      ;; iterate through the original array and for each element, apply the same separation
      ;; to their coordinates that was done to the original array's dimensions and apply the two sets
      ;; of coordinates to set each value in the nested output arrays to the corresponding values in
      ;; the original array
      (lambda (i)
        (let* ((rest i) (inner-index 0) (inner-dx 0) (outer-index 0) (outer-dx 0))
          (loop :for f :in infactors :for fx :from 0
                :do (multiple-value-bind (factor remaining) (floor rest f)
                      (setq rest remaining)
                      (if (loop :for a :across axes :never (= fx a))
                          (progn (incf outer-index (* factor (nth outer-dx outer-factors)))
                                 (incf outer-dx))
                          (progn (incf inner-index (* factor (nth inner-dx inner-factors)))
                                 (incf inner-dx)))))
          (list outer-index inner-index))))))

(defun indexer-turn (axis idims &optional degrees)
  "Return indices of an array rotated as with the [⌽ rotate] or [⊖ rotate first] functions."
  (declare (optimize (speed 3) (safety 0)))
  (let* ((axis (the (unsigned-byte 8) axis))
         (rlen (the (unsigned-byte 62) (nth axis idims)))
         (increment (the (unsigned-byte 62) (reduce #'* (nthcdr (1+ axis) idims))))
         (vset-size (the (unsigned-byte 64) (* increment rlen))))
    (lambda (i)
      (declare (type (unsigned-byte 62) i))
      (the (unsigned-byte 62)
           (+ (the (unsigned-byte 62) (mod i increment))
              (the (unsigned-byte 62) (* vset-size (floor i vset-size)))
              (the (unsigned-byte 62)
                   (* increment
                      (the fixnum
                           (funcall (if degrees #'identity
                                        (lambda (x)
                                          (declare (type (unsigned-byte 62) x))
                                          (abs (- x (1- rlen)))))
                                    (mod (+ (floor i increment)
                                            (the fixnum
                                                 (if (not degrees)
                                                     0 (if (integerp degrees)
                                                           degrees
                                                           (if (arrayp degrees)
                                                               (row-major-aref
                                                                degrees
                                                                (+ (the (unsigned-byte 62)
                                                                        (mod i increment))
                                                                   (the (unsigned-byte 62)
                                                                        (* increment
                                                                           (floor i vset-size)))))
                                                               0)))))
                                         rlen))))))))))

(defun indexer-permute (idims odims alpha is-diagonal &optional is-inverse)
  "Return indices of an array permuted as with the [⍉ permute] function."
  (let* ((irank (length idims))
         (positions) (diagonals) (idims-reduced) (idfactor 1) (odfactor 1)
         (id-factors (coerce (reverse (loop :for d :in (reverse idims)
                                            :collect idfactor :do (setq idfactor (* d idfactor))))
                             'vector))
         (indices (if alpha (progn (if (vectorp alpha)
                                       (loop :for i :across alpha :for id :in idims :for ix :from 0
                                             :do (if (not (member i positions))
                                                     ;; if a duplicate position is found,
                                                     ;; a diagonal section is being performed
                                                     (progn (push i positions)
                                                            (push id idims-reduced)))
                                                ;; collect possible diagonal indices into diagonal list
                                                (if (assoc i diagonals)
                                                    (push ix (rest (assoc i diagonals)))
                                                    (push (list i ix) diagonals))
                                             :collect i)
                                       (progn (setq odims idims
                                                    positions (cons alpha positions))
                                              (list alpha))))
                      (reverse (iota irank))))
         ;; remove indices not being used for diagonal section from diagonal list
         ;; the idims-reduced are a set of the original dimensions without dimensions being elided
         ;; for diagonal section, used to get the initial output array used for diagonal section
         (od-factors (make-array (length odims)))
         (s-factors (make-array irank)))
    (loop :for d :in (reverse odims) :for dx :from 0
          :do (setf (aref od-factors (- (length odims) 1 dx)) odfactor
                    odfactor (* d odfactor)))
    (loop :for i :across id-factors :for ix :from 0
          :do (setf (aref s-factors (nth ix indices)) i))
    (lambda (i)
      (if (not is-diagonal)
          ;; handle regular permutation cases
          (if is-inverse i ;; selective assignment assigns all elements in a regular permute case
              (let* ((remaining i) (oindex 0))
                (loop :for ix :in indices :for od :across od-factors :for s :across s-factors
                      :collect (multiple-value-bind (index remainder) (floor remaining od)
                                 (incf oindex (* index s))
                                 (setq remaining remainder)))
                oindex))
          ;; handle diagonal array traversals
          (if is-inverse
              (let ((remaining i) (valid t) (factor))
                ;; determine the presence of an input element in the
                ;; output, used for selective assignment i.e. (1 1⍉M)←0
                (loop :for ix :from 0 :for if :across id-factors :while valid
                      :do (multiple-value-bind (index remainder) (floor remaining if)
                            (if (and factor (/= index factor))
                                (setq valid nil)
                                (setq remaining remainder
                                      factor (or factor index)))))
                (if valid factor))
              (let ((remaining i) (iindex 0))
                (loop :for ox :from 0 :for of :across od-factors
                      :do (multiple-value-bind (index remainder) (floor remaining of)
                            (setq remaining remainder)
                            (loop :for a :in indices :for ax :from 0 :when (= a ox)
                                  :do (incf iindex (* index (aref id-factors ax))))))
                iindex))))))

;; a sub-package of Aplesque that provides the array formatting functions
(defpackage #:aplesque.forms
  (:import-from :aplesque #:indexer-split #:indexer-section #:indexer-expand
                #:indexer-turn #:indexer-permute)
  (:export #:indexer-split #:indexer-section #:indexer-expand #:indexer-turn #:indexer-permute))
