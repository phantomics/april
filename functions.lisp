;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; functions.lisp

(in-package #:april)

"This file contains the functions in April's 'standard library' that aren't provided by the aplesque package, mostly functions that are specific to the APL language and not generally applicable to array processing, as well as April-specific wrappers for aplesque functions."

(defun binary-not (bit)
  "Flip a binary value. Used to implement [~ not]."
  (case bit (0 1) (1 0) (t (error "Domain error: arguments to ~~ must be 1 or 0."))))

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

(defun apl-random (index-origin rngs)
  "Randomize an array or scalar value. This must run synchronously over arrays without threading so that the same seed will produce the same output. Used to implement [? random]."
  (lambda (omega)
    (let* ((gen-name (getf (rest rngs) :rng))
           (generator (or (getf (rest rngs) gen-name)
                          (setf (getf (rest rngs) gen-name)
                                (if (eq :system gen-name)
                                    :system (random-state:make-generator gen-name))))))
      (if (not (arrayp omega))
          (apl-random-process omega index-origin generator)
          (if (is-integer-array omega)
              (let* ((zeroes-present nil)
                     (nonzeroes-present nil))
                ;; TODO: this is fast if a zero is encountered soon, but slow if not because
                ;; it does not run in parallel, write loops running in parallel
                (loop :for i :below (size omega) :while (or (not zeroes-present)
                                                            (not nonzeroes-present))
                      :do (if (zerop (row-major-aref omega i)) (setq zeroes-present t)
                              (setq nonzeroes-present t)))
                (let ((output (make-array (dims omega)
                                          :element-type (if (not nonzeroes-present)
                                                            'double-float
                                                            (if (and zeroes-present nonzeroes-present)
                                                                t (element-type omega))))))
                  (dotimes (i (size omega))
                    (setf (row-major-aref output i)
                          (apl-random-process (row-major-aref omega i) index-origin generator)))
                  output))
              (let ((output (make-array (dims omega) :element-type (element-type omega))))
                (dotimes (i (size omega))
                  (setf (row-major-aref output i)
                        (apl-random-process (row-major-aref omega i) index-origin generator)))
                output))))))

(defun deal (index-origin rngs)
  "Return a function to randomly shuffle a finite sequence. Used to implement [? deal]."
  (lambda (omega alpha)
    (let* ((omega (disclose-unitary omega))
           (alpha (disclose-unitary alpha))
           (gen-name (getf (rest rngs) :rng))
           (generator (or (getf (rest rngs) gen-name)
                          (setf (getf (rest rngs) gen-name)
                                (if (eq :system gen-name)
                                    :system (random-state:make-generator gen-name))))))
      (if (or (not (integerp omega))
              (not (integerp alpha)))
          (error "Both arguments to ? must be single non-negative integers.")
          (if (> alpha omega)
              (error "The left argument to ? must be less than or equal to the right argument.")
              (let ((vector (count-to omega index-origin)))
                ;; perform Knuth shuffle of vector
                (loop :for i :from omega :downto 2
                      :do (rotatef (aref vector (if (eq :system generator) (random i)
                                                    (random-state:random-int generator 0 (1- i))))
                                   (aref vector (1- i))))
                (if (= alpha omega)
                    vector (make-array alpha :displaced-to vector :element-type (element-type vector)))))))))

(defun apl-divide (method)
  "Generate a division function according to the [⎕DIV division method] in use."
  (lambda (omega &optional alpha)
    (if (and alpha (zerop omega) (zerop alpha))
        (if (zerop method) 1 0)
        (if alpha (/ alpha omega)
            (if (and (< 0 method) (zerop omega))
                0 (/ omega))))))

(defun sb-rationalize (x)
  "This is a port of SBCL's (rationalize) function. It is needed for use in ABCL and ECL, whose (rationalize) implementations appear to simply pass through to (rational)."
  (if (realp x)
      (if (or (typep x 'single-float) (typep x 'double-float) (typep x 'long-float))
          (multiple-value-bind (frac expo sign)
              (integer-decode-float x)
            (cond ((or (zerop frac) (>= expo 0))
                   (if (minusp sign)
                       (- (ash frac expo))
                       (ash frac expo)))
                  (t (let ((a (/ (- (* 2 frac) 1) (ash 1 (- 1 expo))))
                           (b (/ (+ (* 2 frac) 1) (ash 1 (- 1 expo))))
                           (p0 0)
                           (q0 1)
                           (p1 1)
                           (q1 0))
                       ;; expo < 0 and (2*m-1) and (2*m+1) are coprime to 2^(1-e),
                       ;; so build the fraction up immediately, without having to do
                       ;; a gcd.
                       (do ((c (ceiling a) (ceiling a)))
                           ((< c b)
                            (let ((top (+ (* c p1) p0))
                                  (bot (+ (* c q1) q0)))
                              (/ (if (minusp sign)
                                     (- top)
                                     top)
                                 bot)))
                         (let* ((k (- c 1))
                                (p2 (+ (* k p1) p0))
                                (q2 (+ (* k q1) q0)))
                           (psetf a (/ (- b k))
                                  b (/ (- a k)))
                           (setf p0 p1
                                 q0 q1
                                 p1 p2
                                 q1 q2)))))))
          (if (rationalp x) x))))

(defun cmucl-complex-acos (z)
  "This (acos) implementation is used for ECL, whose stock (acos) is different from other CLs."
  (declare (number z))
  (let ((sqrt-1+z (sqrt (+ 1 z)))
        (sqrt-1-z (sqrt (- 1 z))))
    (complex (* 2 (atan (realpart sqrt-1-z) (realpart sqrt-1+z)))
             (asinh (imagpart (* (conjugate sqrt-1+z)
                                 sqrt-1-z))))))

(defun apl-exp (omega)
  "Power of e function that will always output a double-precision float as per APL standard."
  (exp (if (typep omega 'double-float)
           omega (coerce omega 'double-float))))

(defun apl-expt (omega alpha)
  "Exponent function that will always output a double-precision float as per APL standard."
  ;; in the case of one non-natural argument, both must be coerced to floats to avoid trouble in ABCL
  (expt (if (not (and (or (not (integerp omega))
                          (not (integerp alpha)))
                      (not (or (typep omega 'double-float)
                               (typep omega '(complex double-float))))))
            omega (coerce omega 'double-float))
        (if (not (and (or (not (integerp omega))
                          (not (integerp alpha)))
                      (not (or (typep alpha 'double-float)
                               (typep alpha '(complex double-float))))))
            alpha (coerce alpha 'double-float))))

(defun apl-log (omega &optional alpha)
  "Logarithm function that will always output a double-precision float as per APL standard."
  ;; like expt, both args must be coerced to doubles to avoid a problem in ABCL
  (if alpha (log (if (typep omega 'double-float)
                     omega (coerce omega 'double-float))
                 (if (typep alpha 'double-float)
                           alpha (coerce alpha 'double-float)))
      (log (if (typep omega 'double-float)
               omega (coerce omega 'double-float)))))

(defun complex-floor (number comparison-tolerance)
  "Find the floor of a complex number using Eugene McDonnell's algorithm."
  (let* ((rfloor (floor (realpart number)))
         (ifloor (floor (imagpart number)))
         (rpart (- (realpart number) rfloor))
         (ipart (- (+ (imagpart number)
                      (* comparison-tolerance (abs (imagpart number))))
                   ifloor)))
    (if (> 1 (+ rpart ipart)) (complex rfloor ifloor)
        (if (< rpart ipart) (complex rfloor (1+ ifloor))
            (1+ (complex rfloor ifloor))))))

(defun apl-floor (comparison-tolerance)
  "Find a number's floor using the complex floor algorithm if needed."
  (lambda (omega)
    (if (complexp omega) (complex-floor omega comparison-tolerance)
        (floor omega))))

(defun apl-ceiling (comparison-tolerance)
  "Find a number's ceiling deriving from the complex floor algorithm if needed."
  (lambda (omega)
    (if (complexp omega) (- (complex-floor (- omega) comparison-tolerance))
        (ceiling omega))))

(defun apl-residue (comparison-tolerance)
  "Implementation of residue extended to complex numbers based on the complex-floor function."
  (lambda (omega alpha)
    (if (or (complexp omega) (complexp alpha))
        (let ((ainput (complex (if (zerop (realpart alpha))
                                   1 (realpart alpha))
                               (if (zerop (imagpart alpha))
                                   1 (imagpart alpha)))))
          (- omega (* ainput (complex-floor (/ omega ainput) comparison-tolerance))))
        (if (zerop alpha)
            omega (mod omega alpha)))))

(defun apl-xcy (function)
  "Return a function to find the greatest common denominator or least common multiple of fractional as well as whole numbers. If one or both arguments are floats, the result is coerced to a double float."
  (lambda (omega alpha)
    (if (and (integerp omega) (integerp alpha))
        (funcall function omega alpha)
        (let* ((float-input)
               (omega (if (not (floatp omega))
                          omega (setf float-input #+(or abcl ecl) (sb-rationalize omega)
                                      #+(not (or abcl ecl)) (rationalize omega))))
               (alpha (if (not (floatp alpha))
                          alpha (setf float-input #+(or abcl ecl) (sb-rationalize alpha)
                                      #+(not (or abcl ecl)) (rationalize alpha)))))
          (funcall (if (not float-input) #'identity (lambda (number)
                                                      (if (not (typep number 'ratio))
                                                          number (coerce number 'double-float))))
                   (let ((d-product (* (denominator omega) (denominator alpha))))
                     (/ (funcall function (* d-product omega) (* d-product alpha))
                        d-product)))))))

(defun apl-gcd (comparison-tolerance)
  "Implementation of greatest common denominator extended to complex numbers based on the complex-floor function."
  (lambda (omega alpha)
    (if (or (complexp omega) (complexp alpha))
        (if (zerop (funcall (apl-residue comparison-tolerance)
                            omega alpha))
            alpha (if (or (not (integerp (realpart omega)))
                          (not (integerp (realpart alpha))))
                      (let* ((rlromega (rationalize (realpart omega)))
                             (rliomega (rationalize (imagpart omega)))
                             (oden (lcm (denominator rlromega)
                                        (denominator rliomega)))
                             (rlralpha (rationalize (realpart alpha)))
                             (rlialpha (rationalize (imagpart alpha)))
                             (aden (lcm (denominator rlralpha)
                                        (denominator rlialpha))))
                        (* 1.0d0 (/ (funcall (apl-gcd comparison-tolerance)
                                             (complex (* (numerator rlromega)
                                                         (/ oden (denominator rlromega)))
                                                      (* (numerator rliomega)
                                                         (/ oden (denominator rliomega))))
                                             (complex (* (numerator rlralpha)
                                                         (/ aden (denominator rlralpha)))
                                                      (* (numerator rlialpha)
                                                         (/ aden (denominator rlialpha)))))
                                    (lcm oden aden))))
                      (funcall (apl-gcd comparison-tolerance)
                               alpha (let ((residue (funcall (apl-residue comparison-tolerance)
                                                             omega alpha)))
                                       (if (< (- comparison-tolerance)
                                              (realpart residue)
                                              comparison-tolerance)
                                           residue (imagpart residue))))))
        (funcall (apl-xcy #'gcd) omega alpha))))

(defun apl-lcm (comparison-tolerance)
  "Implementation of lease common multiple extended to complex numbers based on the complex-floor function."
  (lambda (omega alpha)
    (if (or (complexp omega) (complexp alpha))
        (* alpha (/ omega (funcall (apl-gcd comparison-tolerance) omega alpha)))
        (funcall (apl-xcy #'lcm) omega alpha))))

(defun without (omega alpha)
  "Remove elements in omega from alpha. Used to implement dyadic [~ without]."
  (flet ((compare (o a)
           (funcall (if (and (characterp a) (characterp o))
                        #'char= (if (and (numberp a) (numberp o))
                                    #'= (lambda (a o) (declare (ignore a o)))))
                    o a)))
    (if (not (arrayp alpha))
        (setq alpha (vector alpha))
        (if (not (vectorp alpha))
            (error "The left argument to [~ without] must be a vector.")))
    (let ((included)
          (omega-vector (if (or (vectorp omega)(not (arrayp omega)))
                            (disclose omega)
                            (make-array (array-total-size omega)
                                        :displaced-to omega :element-type (element-type omega)))))
      (loop :for element :across alpha
         :do (let ((include t))
               (if (vectorp omega-vector)
                   (loop :for ex :across omega-vector
                      :do (if (compare ex element) (setq include nil)))
                   (if (compare omega-vector element) (setq include nil)))
               (if include (push element included))))
      (make-array (list (length included)) :element-type (element-type alpha)
                  :initial-contents (reverse included)))))

(defun scalar-compare (comparison-tolerance)
  "Compare two scalar values as appropriate for APL."
  (lambda (omega alpha)
    (funcall (if (and (characterp alpha) (characterp omega))
                 #'char= (if (and (numberp alpha) (numberp omega))
                             (if (not (or (floatp alpha) (floatp omega)))
                                 #'= (lambda (a o) (> comparison-tolerance (abs (- a o)))))
                             (lambda (a o) (declare (ignore a o)))))
             omega alpha)))

(defun compare-by (symbol comparison-tolerance &optional or-equal)
  "Generate a comparison function using the [⎕CT comparison tolerance]."
  (lambda (omega alpha)
    (funcall (if (and (numberp alpha) (numberp omega))
                 (if (not (or (floatp alpha) (floatp omega)))
                     (symbol-function symbol)
                     (lambda (a o)
                       (case symbol
                         (<  (< comparison-tolerance (- o a)))
                         (>  (< comparison-tolerance (- a o)))
                         (<= (or (> comparison-tolerance (abs (- a o)))
                                 (< comparison-tolerance (- o a))))
                         (>= (or (> comparison-tolerance (abs (- a o)))
                                 (< comparison-tolerance (- a o))))))))
             omega alpha)))

(defun count-to (index index-origin)
  "Implementation of APL's [⍳ index] function."
  (let ((index (disclose index)))
    (if (or (integerp index)
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
            (error "The argument to [⍳ index] must be an integer, i.e. ⍳9, or a vector, i.e. ⍳2 3.")))))

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

(defun at-index (index-origin axes)
  "Find the value(s) at the given index or indices in an array. Used to implement [⌷ index]."
  (lambda (omega alpha)
    (if (not (arrayp omega))
        (if (and (numberp alpha)
                 (= index-origin alpha))
            omega (error "Invalid index."))
        (multiple-value-bind (assignment-output assigned-array)
            (choose omega (let ((coords (funcall (if (arrayp alpha) #'array-to-list #'list)
                                                 (apply-scalar #'- alpha index-origin)))
                                ;; the inefficient array-to-list is used here in case of nested
                                ;; alpha arguments like (⊂1 2 3)⌷...
                                (axis (if axes (if (vectorp (first axes))
                                                   (loop :for item :across (first axes)
                                                      :collect (- item index-origin))
                                                   (if (integerp (first axes))
                                                       (list (- (first axes) index-origin)))))))
                            (if (not axis)
                                ;; pad coordinates with nil elements in the case of an elided reference
                                (append coords (loop :for i :below (- (rank omega) (length coords)) :collect nil))
                                (loop :for dim :below (rank omega)
                                   :collect (if (member dim axis) (first coords))
                                      :when (member dim axis) :do (setq coords (rest coords))))))
          (or assigned-array assignment-output)))))

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
  (if (not (arrayp omega))
      omega (if (vectorp omega)
                (let ((output (make-array (list (length omega) 1) :element-type (element-type omega))))
                  (loop :for i :below (length omega) :do (setf (row-major-aref output i) (aref omega i)))
                  output)
                (let ((o-dims (dims omega)))
                  (make-array (list (first o-dims) (reduce #'* (rest o-dims)))
                              :element-type (element-type omega)
                              :displaced-to (copy-nested-array omega))))))

(defun ravel-array (index-origin axes)
  "Wrapper for aplesque [, ravel] function incorporating index origin from current workspace."
  (lambda (omega)
    (ravel index-origin omega axes)))

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
        (if (or (not axes)
                (integerp (first axes)))
            (catenate alpha omega (or *first-axis-or-nil* 0))))))

(defun mix-array (index-origin axes)
  "Wrapper for (aplesque:mix) used for [↑ mix]."
  (lambda (omega) ; &optional axes)
    (mix-arrays (if axes (- (ceiling (first axes)) index-origin)
                    (rank omega))
                omega :populator (lambda (item)
                                   (let ((populator (build-populator item)))
                                     (if populator (funcall populator)))))))

(defun wrap-split-array (index-origin axes)
  "Wrapper for [↓ split]."
  (lambda (omega) (split-array omega *last-axis*)))

(defun section-array (index-origin &optional inverse axes)
  "Wrapper for (aplesque:section) used for [↑ take] and [↓ drop]."
  (lambda (omega alpha) ; &optional axes)
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
                                           (if (vectorp spec-axes)
                                               (loop :for ax :across spec-axes :for ix :from 0
                                                  :do (setf (aref dims (- ax index-origin))
                                                            (aref alpha ix)))))
                                       dims)
                                alpha)
                            :inverse inverse :populator (build-populator omega))))
      ;; if the resulting array is empty and the original array prototype was an array, set the
      ;; empty array prototype accordingly
      (if (and (zerop (size output)) (not inverse)
               (arrayp omega) (if (< 0 (size omega))
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
      #() (let* ((alpha (if (arrayp alpha)
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
                 (out-dims (append (loop :for dim :in adims :when t :collect dim)
                                   (loop :for dim :in odims :when t :collect dim)))
                 ;; currently, the output is set to t because due to the cost of finding the highest array value
                 (output (if out-dims (make-array out-dims)))
                 (aseg-last (reduce #'* (butlast adims 1)))
                 (aseg-first (reduce #'* (rest adims)))
                 (ofactor (* osize aseg-first)))
            (dotimes (i (size output))
              (multiple-value-bind (o a) (floor i asize)
                (let ((value (if (not (arrayp omega))
                                 omega (row-major-aref omega o)))
                      (last-base 1) (base 1) (component 1) (element 0)
                      (increment (if (/= 1 aseg-last) aseg-last asize)))
                  (loop :for index :from (1- increment) :downto (mod a increment)
                     :do (setq last-base base
                               base (* base (if (not (arrayp alpha))
                                                alpha (row-major-aref alpha (+ (* index aseg-first)
                                                                               (floor a increment)))))
                               component (if (zerop base) value (nth-value 1 (floor value base)))
                               value (- value component)
                               element (if (zerop last-base) 0 (floor component last-base))))
                  (if output (setf (row-major-aref output (+ o (* osize (floor a aseg-last))
                                                             (* ofactor (mod a aseg-last))))
                                   element)
                      (setq output element)))))
            output)))

(defun decode (omega alpha)
  "Decode an array of numbers as per a given set of bases. Used to implement [⊥ decode]."
  (let* ((omega (if (arrayp omega) omega (enclose-atom omega)))
         (alpha (if (arrayp alpha) alpha (enclose-atom alpha)))
         (odims (dims omega)) (adims (dims alpha))
         (osize (size omega)) (asize (size alpha))
         (out-dims (append (butlast adims 1) (rest odims)))
         (output (if out-dims (make-array out-dims)))
         (ovector (first odims))
         ;; extend a 1-column matrix to the length of the right argument's first dimension,
         ;; supporting use cases like (⍪5 10)⊥4 8⍴⍳9
         (alpha (if (< 1 asize) (if (not (and (< 1 osize) (= 1 (first (last adims)))))
                                    alpha (let ((out (make-array (append (butlast adims 1)
                                                                         (list ovector)))))
                                            (dotimes (a asize)
                                              (dotimes (i ovector)
                                                (setf (row-major-aref out (+ i (* a ovector)))
                                                      (row-major-aref alpha a))))
                                            out))
                    (make-array ovector :initial-element (row-major-aref alpha 0))))
         (afactors (make-array (if (and (< 1 osize) (< 1 (first (last adims))))
                                   adims (append (butlast adims 1) (list ovector)))
                               :initial-element 1))
         (asegments (reduce #'* (butlast adims 1)))
         (av2 (first (last (dims alpha))))
         (out-section (reduce #'* (rest odims))))
    (if out-dims (progn (dotimes (a asegments)
                          (loop :for i :from (- (* av2 (1+ a)) 2) :downto (* av2 a)
                             :do (setf (row-major-aref afactors i) (* (row-major-aref alpha (1+ i))
                                                                      (row-major-aref afactors (1+ i))))))
                        (xdotimes output (i (size output))
                          (let ((result 0))
                            (loop :for index :below av2
                                  :do (incf result (* (row-major-aref omega (mod (+ (mod i out-section)
                                                                                    (* out-section index))
                                                                                 (size omega)))
                                                      (row-major-aref afactors
                                                                      (+ index (* av2 (floor i out-section)))))))
                            (setf (row-major-aref output i) result))))
        (let ((result 0) (factor 1))
          (loop :for i :from (1- (if (< 1 av2) av2 ovector)) :downto 0
             :do (incf result (* factor (row-major-aref omega (min i (1- ovector)))))
               (setq factor (* factor (row-major-aref alpha (min i (1- av2))))))
          (setq output result)))
    output))

(defun left-invert-matrix (in-matrix)
  "Perform left inversion of matrix. Used to implement [⌹ matrix inverse]."
  (let* ((input (if (= 2 (rank in-matrix))
                    in-matrix (make-array (list (length in-matrix) 1))))
         (input-displaced (if (/= 2 (rank in-matrix))
                              (make-array (list 1 (length in-matrix)) :element-type (element-type input)
                                          :displaced-to input))))
    (if input-displaced (xdotimes input (i (length in-matrix)) (setf (row-major-aref input i)
                                                                     (aref in-matrix i))))
    (let ((result (array-inner-product (invert-matrix (array-inner-product (or input-displaced
                                                                               (aops:permute '(1 0) input))
                                                                           input #'* #'+ t))
                                       (or input-displaced (aops:permute '(1 0) input))
                                       #'* #'+ t)))
      (if (= 1 (rank in-matrix))
          (make-array (size result) :element-type (element-type result) :displaced-to result)
          result))))

(defun format-array (print-precision)
  "Use (aplesque:array-impress) to print an array and return the resulting character array, with the option of specifying decimal precision. Used to implement monadic and dyadic [⍕ format]."
  (lambda (omega &optional alpha)
    (if (and alpha (not (integerp alpha)))
        (error (concatenate 'string "The left argument to ⍕ must be an integer specifying"
                            " the precision at which to print floating-point numbers.")))
    (if (characterp omega)
        omega (array-impress
               omega :collate t
                     :segment (lambda (number &optional segments)
                                (count-segments number (if alpha (- alpha) print-precision)
                                                segments))
                     :format (lambda (number &optional segments rps)
                               (print-apl-number-string number segments print-precision alpha rps))))))

(defun format-array-uncollated (print-precision-default)
  "Generate a function using (aplesque:array-impress) to print an array in matrix form without collation. Used to implement ⎕FMT."
  (lambda (input &optional print-precision)
    (let ((print-precision (or print-precision print-precision-default))
          (is-not-nested t))
      (if (and print-precision (not (integerp print-precision)))
          (error (concatenate 'string "The left argument to ⍕ must be an integer specifying"
                              " the precision at which to print floating-point numbers.")))
      ;; only right-indent if this is a nested array; this is important for box-drawing functions
      (if (arrayp input) (xdotimes input (x (size input))
                           (if (arrayp (row-major-aref input x))
                               (setf is-not-nested nil))))
      (funcall (lambda (output)
                 (if (/= 1 (rank output))
                     output (array-promote output)))
               (array-impress input :unpadded is-not-nested
                              :segment (lambda (number &optional segments)
                                         (count-segments number print-precision segments))
                              :format (lambda (number &optional segments rps)
                                        (print-apl-number-string number segments
                                                                 print-precision print-precision rps)))))))

(defun generate-index-array (array &optional scalar-assigned ext-index)
  "Given an array, generate an array of the same shape whose each cell contains its row-major index."
  (let* ((index (or ext-index -1))
         (is-scalar (zerop (rank array)))
         (array (if (and is-scalar (not scalar-assigned))
                    (aref array) array))
         (output (make-array (dims array) :element-type (if (eq t (element-type array))
                                                            t (list 'integer 0 (size array))))))
    ;; TODO: can this be parallelized?
    (dotimes (i (size array))
      (if (or scalar-assigned (not (arrayp (row-major-aref array i))))
          (setf (row-major-aref output i) (incf index))
          (multiple-value-bind (out-array out-index)
              (generate-index-array (row-major-aref array i) scalar-assigned index)
            (setf (row-major-aref output i) out-array
                  index out-index))))
    (values (funcall (if (or scalar-assigned (not is-scalar))
                         #'identity (lambda (o) (make-array nil :initial-element o)))
                     output)
            (+ index (if ext-index 0 1)))))

(defun assign-by-selection (prime-function function value omega
                            &key assign-sym axes secondary-prime-fn by)
  "Assign to elements of an array selected by a function. Used to implement (3↑x)←5 etc."
  (let ((function-meta (handler-case (funcall prime-function :get-metadata nil) (error () nil))))
    (labels ((duplicate-t (array)
               (let ((output (make-array (dims array))))
                 (dotimes (i (size array))
                   (setf (row-major-aref output i)
                         (if (not (arrayp (row-major-aref array i)))
                             (row-major-aref array i)
                             (duplicate-t (row-major-aref array i)))))
                 output)))
      (if (getf function-meta :selective-assignment-compatible)
          (let* ((omega (if (and assign-sym
                                 (if (arrayp value)
                                     (and (or (not (getf function-meta :selective-assignment-enclosing))
                                              (eq t (element-type omega)))
                                          (subtypep (element-type value)
                                                    (element-type omega)))
                                     (subtypep (upgraded-array-element-type (assign-element-type value))
                                               (element-type omega))))
                            ;; array is duplicated if its element type is not a supertype of
                            ;; the assigned array's type, or if an enclosed array is
                            ;; being assigned and the array's type is not T
                            omega (duplicate-t omega)))
                 (assign-array (if (not axes) omega (choose omega axes :reference t)))
                 ;; assign reference is used to determine the shape of the area to be assigned,
                 ;; which informs the proper method for generating the index array
                 (assign-reference (disclose-atom (funcall function assign-array)))
                 (value (funcall (if (getf function-meta :selective-assignment-enclosing)
                                     #'enclose #'identity)
                                 value)))
            ;; TODO: this logic can be improved
            (if (arrayp value)
                (let* ((index-array (generate-index-array assign-array t))
                       (target-index-array (enclose-atom (funcall function index-array))))
                  (assign-by-vector assign-array index-array
                                    (vectorize-assigned target-index-array value (size assign-array))
                                    :by by)
                  assign-array)
                (multiple-value-bind (index-array assignment-size)
                    (generate-index-array assign-array (and (arrayp (disclose-atom assign-reference))
                                                            (not (< 1 (size (disclose-atom assign-reference))))
                                                            (not (arrayp value))))
                  (let ((target-index-array (enclose-atom (funcall function index-array))))
                    (assign-by-vector assign-array index-array
                                      (vectorize-assigned target-index-array value assignment-size)
                                      :by by)
                    omega))))
          (if (getf function-meta :selective-assignment-passthrough)
              (assign-by-selection secondary-prime-fn function value omega :axes axes :by by)
              (error "This function cannot be used for selective assignment."))))))

(defun vectorize-assigned (indices values vector-or-length)
  "Generate a vector of assigned values for use by (assign-by-selection)."
  (let ((vector (if (arrayp vector-or-length) vector-or-length
                    (make-array (list vector-or-length) :initial-element nil))))
    (if (and (arrayp values)
             (not (loop :for i :in (dims indices) :for v :in (dims values) :always (= i v))))
        (error "Area of array to be reassigned does not match shape of values to be assigned.")
        (progn (dotimes (i (size indices))
                 (if (not (arrayp (row-major-aref indices i)))
                     (setf (row-major-aref vector (row-major-aref indices i))
                           (if (not (arrayp values))
                               values (if (zerop (rank values))
                                          (aref values)
                                          (if (or (not (arrayp (row-major-aref values i)))
                                                  (= (size indices) (size values)))
                                              (row-major-aref values i)
                                              (error "Incompatible values to assign; nested array present ~a"
                                                     " where scalar value expected.")))))
                     (vectorize-assigned (row-major-aref indices i)
                                         (if (arrayp values) (row-major-aref values i)
                                             values)
                                         vector)))
               vector))))

(defun assign-by-vector (array indices vector &key by)
  "Assign elements of an array corresponding to an array of indices from a vector. For use with (assign-by-selection)."
  (dotimes (i (size array))
    (if (not (arrayp (row-major-aref array i)))
        (if (aref vector (row-major-aref indices i))
            (setf (row-major-aref array i)
                  (if by (funcall by (row-major-aref array i)
                                  (aref vector (row-major-aref indices i)))
                      (aref vector (row-major-aref indices i)))))
        (if (not (arrayp (row-major-aref indices i)))
            (if (aref vector (row-major-aref indices i))
                (setf (row-major-aref array i)
                      (if by (funcall by (row-major-aref array i)
                                      (aref vector (row-major-aref indices i)))
                          (aref vector (row-major-aref indices i)))))
            (assign-by-vector (row-major-aref array i)
                              (row-major-aref indices i)
                              vector :by by)))))

(defun operate-reducing (function index-origin last-axis &key axis)
  "Reduce an array along a given axis by a given function, returning function identites when called on an empty array dimension. Used to implement the [/ reduce] operator."
  (lambda (omega &optional alpha)
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
                                    (if (functionp identity) (funcall identity) identity)))
                            output)
                          (error "The operand of [/ reduce] has no identity value.")))
                  ;; if reduction produces a scalar, the result is the identity value
                  (or (and identity (if (functionp identity) (funcall identity) identity))
                      (error "The operand of [/ reduce] has no identity value."))))
            (reduce-array omega function (if (first axis) (- (first axis) index-origin))
                          (side-effect-free function)
                          last-axis alpha)))))

(defun operate-scanning (function index-origin last-axis inverse &key axis)
  "Scan a function across an array along a given axis. Used to implement the [\ scan] operator with an option for inversion when used with the [⍣ power] operator taking a negative right operand."
  (lambda (omega &optional alpha) ;; alpha is only used to pass an axis reassignment
    (if (not (arrayp omega))
        (if (eq :get-metadata omega)
            (list :inverse (let ((inverse-function (getf (funcall function :get-metadata nil) :inverse)))
                             (operate-scanning inverse-function index-origin last-axis t :axis axis))
                  :valence :monadic)
            (if (eq :reassign-axes omega)
                (operate-scanning function index-origin last-axis inverse :axis alpha)
                omega))
        (let* ((odims (dims omega))
               (axis (or (and (first axis) (- (first axis) index-origin))
                         (if (not last-axis) 0 (1- (rank omega)))))
               (rlen (nth axis odims))
               (increment (reduce #'* (nthcdr (1+ axis) odims)))
               (fn-meta (handler-case (funcall function :get-metadata nil) (error nil)))
               (output (make-array odims))
               (sao-copy))
          (if (getf fn-meta :scan-alternating)
              (progn (setq sao-copy (make-array (dims omega)))
                     (xdotimes sao-copy (i (size omega))
                       (let ((vector-index (mod (floor i increment) rlen))
                             (base (+ (mod i increment)
                                      (* increment rlen (floor i (* increment rlen))))))
                         (setf (row-major-aref sao-copy (+ base (* increment vector-index)))
                               (if (not (zerop (mod vector-index 2)))
                                   (apply-scalar (getf fn-meta :scan-alternating)
                                                 (row-major-aref
                                                  omega (+ base (* increment vector-index))))
                                   (row-major-aref omega (+ base (* increment vector-index)))))))))
          ;; (dotimes (i (size output)) ;; xdo
          (xdotimes output (i (size output) :synchronous-if (side-effect-free function))
            (declare (optimize (safety 1)))
            (let ((value) (vector-index (mod (floor i increment) rlen))
                  (base (+ (mod i increment) (* increment rlen (floor i (* increment rlen))))))
              (if inverse
                  (let ((original (disclose (row-major-aref
                                             omega (+ base (* increment vector-index))))))
                    (setq value (if (zerop vector-index)
                                    original
                                    (funcall function original
                                             (disclose
                                              (row-major-aref
                                               omega (+ base (* increment (1- vector-index)))))))))
                  ;; faster method for commutative functions
                  ;; NOTE: xdotimes will not work with this method
                  (if (or sao-copy (getf fn-meta :commutative))
                      (setq value (if (zerop vector-index)
                                      (row-major-aref omega base)
                                      (funcall (if sao-copy (getf fn-meta :inverse-right)
                                                   function)
                                               (row-major-aref
                                                output (+ base (* increment (1- vector-index))))
                                               (row-major-aref
                                                (or sao-copy omega)
                                                (+ base (* increment vector-index))))))
                      (loop :for ix :from vector-index :downto 0
                            :do (let ((original (row-major-aref omega (+ base (* ix increment)))))
                                  (setq value (if (not value) (disclose original)
                                                  (funcall function value (disclose original))))))))
              (setf (row-major-aref output i) value)))
          output))))

(defun operate-each (operand)
  "Generate a function applying a function to each element of an array. Used to implement [¨ each]."
  (lambda (omega &optional alpha)
    (let* ((oscalar (if (zerop (rank omega)) omega))
           (ascalar (if (zerop (rank alpha)) alpha))
           (ouvec (if (= 1 (size omega)) omega))
           (auvec (if (= 1 (size alpha)) alpha))
           (odims (dims omega)) (adims (dims alpha))
           (orank (rank omega)) (arank (rank alpha)))
      (flet ((disclose-any (item)
               (if (not (arrayp item))
                   item (row-major-aref item 0))))
        (if (not (or oscalar ascalar ouvec auvec (not alpha)
                     (and (= orank arank)
                          (loop :for da :in adims :for do :in odims :always (= da do)))))
            (error "Mismatched left and right arguments to [¨ each].")
            (let* ((output-dims (dims (if (or oscalar (and ouvec (arrayp alpha) (not ascalar)))
                                          alpha omega)))
                   (output (if (or (arrayp alpha) (arrayp omega))
                               (make-array output-dims))))
              (if alpha (if (and (or oscalar ouvec)
                                 (or ascalar auvec))
                            (if output (setf (row-major-aref output 0)
                                             (funcall operand (disclose-any omega)
                                                      (disclose-any alpha)))
                                (setf output (enclose (funcall operand omega alpha))))
                            (dotimes (i (size (if (or oscalar ouvec) alpha omega))) ;; xdo
                            ;; (xdotimes output (i (size (if (or oscalar ouvec) alpha omega))
                            ;;                     :synchronous-if (side-effect-free operand)) ;; xdo
                              (if output
                                  (setf (row-major-aref output i)
                                        (funcall operand (if (or oscalar ouvec) (disclose-any omega)
                                                             (row-major-aref omega i))
                                                 (if (or ascalar auvec)
                                                     (disclose-any alpha)
                                                     (row-major-aref alpha i))))
                                  (setf output (funcall operand omega alpha)))))
                  ;; if 0-rank array is passed, disclose its content and enclose the result of the operation
                  (if oscalar (setq output (enclose (funcall operand (disclose-any oscalar))))
                      (dotimes (i (size omega)) ;; xdo
                        (setf (row-major-aref output i)
                              (funcall operand (row-major-aref omega i))))))
              output))))))

(defun operate-commuting (operand)
  "Generate a function with reversed or mirrored arguments. Used to implement [⍨ commute]."
  (lambda (omega &optional alpha)
    (if (not (functionp operand))
        operand (if (eq :get-metadata omega)
                    (list :inverse (lambda (omega &optional alpha)
                                     (if (not alpha)
                                         (let* ((operand-meta (funcall operand :get-metadata nil))
                                                (inverse-commuted (getf operand-meta :inverse-commuted)))
                                           (if inverse-commuted (funcall inverse-commuted omega)
                                               (error "This commuted function cannot be inverted."))))))
                    (funcall operand (or alpha omega) omega)))))

(defun operate-grouping (function index-origin)
  "Generate a function applying a function to items grouped by a criterion. Used to implement [⌸ key]."
  (lambda (omega &optional alpha)
    (let* ((keys (or alpha omega))
           (key-test #'equalp)
           (increment (reduce #'* (rest (dims keys))))
           (indices-of (lambda (item vector)
                         (let ((collection))
                           (loop :for i :below (first (dims vector))
                                 :do (let ((section (if (= 1 increment) (aref vector i)
                                                        (make-array increment :element-type
                                                                    (element-type vector)
                                                                    :displaced-to vector
                                                                    :displaced-index-offset
                                                                    (* i increment)))))
                                       (if (funcall key-test item section)
                                           (push (+ index-origin i) collection))))
                           collection)))
           (key-table (make-hash-table :test key-test))
           (elisions (loop :for i :below (1- (rank omega)) :collect nil))
           (key-list))
      (dotimes (i (first (dims keys)))
        (let ((item (if (= 1 increment) (aref keys i)
                        (make-array increment :element-type (element-type keys) :displaced-to keys
                                              :displaced-index-offset (* i increment)))))
          (if (loop :for key :in key-list :never (funcall key-test item key))
              (push item key-list))
          (push i (gethash item key-table))))
      (let ((item-sets (loop :for key :in (reverse key-list)
                          :collect (funcall function
                                            (if alpha (choose omega
                                                              (cons (apply #'vector
                                                                           (reverse (gethash key key-table)))
                                                                    elisions))
                                                (let ((items (funcall indices-of key keys)))
                                                  (make-array (length items)
                                                              :initial-contents (reverse items))))
                                            key))))
        (mix-arrays 1 (apply #'vector item-sets))))))

(defun operate-producing-outer (operand)
  "Generate a function producing an outer product. Used to implement [∘. outer product]."
  (lambda (omega alpha)
    (if (eq :get-metadata omega)
        (let* ((operand-meta (funcall operand :get-metadata nil))
               (operand-inverse (getf operand-meta :inverse)))
          (list :inverse-right (lambda (omega alpha)
                                 (inverse-outer-product alpha operand-inverse omega
                                                        (side-effect-free operand)))
                :inverse (lambda (omega alpha)
                           (inverse-outer-product omega operand-inverse nil (side-effect-free operand)
                                                  alpha))))
        (array-outer-product omega alpha operand (side-effect-free operand)))))

(defun operate-producing-inner (right left)
  "Generate a function producing an inner product. Used to implement [. inner product]."
  (lambda (alpha omega)
    (if (or (zerop (size omega))
            (zerop (size alpha)))
        (if (or (< 1 (rank omega)) (< 1 (rank alpha)))
            (vector) ;; inner product with an empty array of rank > 1 gives an empty vector
            (or (let ((identity (getf (funcall left :get-metadata nil) :id)))
                  (if (functionp identity) (funcall identity) identity))
                (error "Left operand given to [. inner product] has no identity.")))
        (let ((is-scalar (handler-case (getf (funcall right :get-metadata nil) :scalar)
                           (error () nil))))
          (array-inner-product omega alpha right left (and (side-effect-free right)
                                                           (side-effect-free left))
                               (not is-scalar))))))

(defun operate-beside (right left)
  "Generate a function by linking together two functions or a function curried with an argument. Used to implement [∘ compose]."
  (let ((fn-right (and (functionp right) right))
        (fn-left (and (functionp left) left))
        (temp))
    (lambda (omega &optional alpha)
      (if (eq :get-metadata omega)
          (list :inverse (lambda (omega &optional alpha)
                           (if (and fn-right fn-left)
                               (setq temp fn-right
                                     fn-right fn-left
                                     fn-left temp))
                           (let* ((meta-right (if fn-right (apply fn-right :get-metadata
                                                                  (if (or alpha (not fn-left))
                                                                      (list nil)))))
                                  (meta-left (if fn-left (apply fn-left :get-metadata
                                                                (if (or alpha (not fn-right))
                                                                    (list nil)))))
                                  (fn-right (if fn-right (or (getf meta-right
                                                                   (if (or alpha (not fn-left))
                                                                       :inverse :inverse-right))
                                                             (getf meta-right :inverse))))
                                  (fn-left (if fn-left (if (and alpha fn-right)
                                                           fn-left
                                                           (or (getf meta-left :inverse-right)
                                                               (getf meta-left :inverse))))))
                             (if (and fn-right fn-left)
                                 (let ((processed (if alpha (funcall fn-right omega alpha)
                                                      (funcall fn-right omega))))
                                   (funcall fn-left processed))
                                 (if alpha (error "This function does not take a left argument.")
                                     (funcall (or fn-right fn-left)
                                              (if fn-right omega right)
                                              (if fn-left omega left)))))))
          (if (and fn-right fn-left)
              (let ((processed (funcall fn-right omega)))
                (if alpha (funcall fn-left processed alpha)
                    (funcall fn-left processed)))
              (if alpha (error "This function does not take a left argument.")
                  (funcall (or fn-right fn-left)
                           (if fn-right omega right)
                           (if fn-left omega left))))))))

(defun operate-at-rank (rank function)
  "Generate a function applying a function to sub-arrays of the arguments. Used to implement [⍤ rank]."
  (lambda (omega &optional alpha)
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
                                   (dotimes (i (size output)) ;; xdo
                                     (let ((this-odiv (if (not odivs)
                                                          omega (if (zerop (rank odivs))
                                                                    (aref odivs) (row-major-aref odivs i))))
                                           (this-adiv (if (not adivs)
                                                          alpha (if (zerop (rank adivs))
                                                                    (aref adivs) (row-major-aref adivs i)))))
                                       (setf (row-major-aref output i)
                                             (disclose (funcall function this-odiv this-adiv)))))
                                   (mix-arrays (max (rank odivs) (rank adivs))
                                               output))))
                (if (not odivs) ;; as above for an omega value alone
                    (funcall function omega)
                    (let ((output (make-array (dims odivs))))
                      (xdotimes output (i (size output) :synchronous-if (side-effect-free function))
                        (setf (row-major-aref output i) (funcall function (row-major-aref odivs i))))
                      (mix-arrays (rank output) output)))))))))

(defun operate-atop (right-fn left-fn)
  "Generate a function applying two functions to a value in succession. Used to implement [⍤ atop]."
  (lambda (omega &optional alpha)
    (if alpha (funcall left-fn (funcall right-fn omega alpha))
        (funcall left-fn (funcall right-fn omega)))))

(defun operate-to-power (fetch-determinant function)
  "Generate a function applying a function to a value and successively to the results of prior iterations a given number of times. Used to implement [⍣ power]."
  (lambda (omega &optional alpha)
    (if (eq omega :get-metadata)
        (list :inverse (let* ((determinant (funcall fetch-determinant))
                              (inverse-function (if (not (numberp determinant))
                                                    (getf (if alpha (funcall function :get-metadata nil)
                                                              (funcall function :get-metadata))
                                                          :inverse))))
                         (if (numberp determinant)
                             (operate-to-power (lambda () (- determinant)) function)
                             (operate-to-power fetch-determinant inverse-function))))
        (let ((determinant (funcall fetch-determinant)))
          (if (functionp determinant)
              ;; if the determinant is a function, loop until the result of its
              ;; evaluation with the current and prior values is zero
              (let ((arg omega) (prior-arg omega))
                (loop :for index :from 0 :while (or (zerop index)
                                                    (zerop (funcall determinant prior-arg arg)))
                   :do (setq prior-arg arg
                             arg (if alpha (funcall function arg alpha)
                                     (funcall function arg))))
                arg)
              ;; otherwise, run the operand function on the value(s) a number
              ;; of times equal to the absolute determinant value, inverting
              ;; the operand function if the determinant value is negative
              (let ((arg omega)
                    (function (if (<= 0 determinant)
                                  function (if alpha (getf (funcall function :get-metadata nil) :inverse)
                                               (getf (funcall function :get-metadata) :inverse)))))
                (dotimes (index (abs determinant))
                  (setq arg (if alpha (funcall function arg alpha)
                                (funcall function arg))))
                arg))))))

(defun operate-at (right left index-origin)
  "Generate a function applying a function at indices in an array specified by a given index or meeting certain conditions. Used to implement [@ at]."
  (let ((left-fn (if (functionp left) left))
        (right-fn (if (functionp right) right)))
    (lambda (omega &optional alpha)
      (declare (ignorable alpha))
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
  (lambda (omega)
    (flet ((iaxes (value index) (loop :for x :below (rank value) :for i :from 0
                                   :collect (if (= i 0) index nil))))
      (if (not (or (and (< 2 (rank right-value))
                        (error "The right operand of [⌺ stencil] may not have more than 2 dimensions."))
                   (and (not left-function)
                        (error "The left operand of [⌺ stencil] must be a function."))))
          (let ((window-dims (if (not (arrayp right-value))
                                 (vector right-value)
                                 (if (= 1 (rank right-value))
                                     right-value (choose right-value (iaxes right-value 0)))))
                (movement (if (not (arrayp right-value))
                              (vector 1)
                              (if (= 2 (rank right-value))
                                  (choose right-value (iaxes right-value 1))
                                  (make-array (length right-value) :element-type 'fixnum
                                              :initial-element 1)))))
            (mix-arrays (rank omega)
                        (stencil omega left-function window-dims movement
                                 (side-effect-free left-function))))))))

;;; From this point are optimized implementations of APL idioms.

(defun iota-sum (n)
  "Fast implementation of +/⍳X."
  (declare (type (integer 0 10000000) n)
           (optimize (speed 3) (safety 0)))
  (let ((total 0))
    (declare (type fixnum total))
    (loop :for i :of-type fixnum :from 0 :below n :do (incf total i))
    total))

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
