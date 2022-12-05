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
          (if (eq :system generator) (random item)
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
          (when (rationalp x) x))))

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
  (exp (if (typep omega 'integer)
           (coerce omega 'double-float) omega)))

(defun apl-expt (omega alpha)
  "Exponent function that will always output a double-precision float except if both arguments are integers, as per APL standard."
  ;; in the case of one non-natural argument, both must be coerced to floats to avoid trouble in ABCL
  (expt (if (and (typep omega 'integer) (not (typep alpha 'integer)))
            (coerce omega 'double-float) omega)
        (if (and (typep alpha 'integer) (not (typep omega 'integer)))
            (coerce alpha 'double-float) alpha)))

(defun apl-log (omega &optional alpha)
  "Logarithm function that will always output a double-precision float as per APL standard."
  ;; like expt, both args must be coerced to doubles to avoid a problem in ABCL
  (if alpha (log (if (typep omega 'integer)
                     (coerce omega 'double-float) omega)
                 (if (typep alpha 'integer)
                     (coerce alpha 'double-float) alpha))
      (log (if (typep omega 'integer)
               (coerce omega 'double-float) omega))))

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
                          omega (setf float-input #+(or abcl ecl clasp) (sb-rationalize omega)
                                      #+(not (or abcl ecl clasp)) (rationalize omega))))
               (alpha (if (not (floatp alpha))
                          alpha (setf float-input #+(or abcl ecl clasp) (sb-rationalize alpha)
                                      #+(not (or abcl ecl clasp)) (rationalize alpha)))))
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
                      (let* ((rlromega #+(or abcl ecl clasp) (sb-rationalize (realpart omega))
                                       #+(not (or abcl ecl clasp)) (rationalize (realpart omega)))
                             (rliomega #+(or abcl ecl clasp) (sb-rationalize (imagpart omega))
                                       #+(not (or abcl ecl clasp)) (rationalize (imagpart omega)))
                             (oden (lcm (denominator rlromega)
                                        (denominator rliomega)))
                             (rlralpha #+(or abcl ecl clasp) (sb-rationalize (realpart alpha))
                                       #+(not (or abcl ecl clasp)) (rationalize (realpart alpha)))
                             (rlialpha #+(or abcl ecl clasp) (sb-rationalize (imagpart alpha))
                                       #+(not (or abcl ecl clasp)) (rationalize (imagpart alpha)))
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
        (unless (vectorp alpha)
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
                         :do (when (compare ex element) (setq include nil)))
                   (when (compare omega-vector element) (setq include nil)))
               (when include (push element included))))
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
  (let ((index (disclose-atom (render-varrays index))))
    (if (or (integerp index)
            (and (vectorp index)
                 (= 1 (length index))))
        (make-instance 'vapri-integer-progression :number index :origin index-origin)
        (make-instance 'vapri-coordinate-identity :shape (coerce index 'list)
                                                  :index-origin index-origin))))

(defun at-index (index-origin axes)
  "Find the value(s) at the given index or indices in an array. Used to implement [⌷ index]."
  (lambda (omega alpha)
    (setf omega (render-varrays omega))
    (if (not (arrayp omega))
        (if (and (numberp alpha)
                 (= index-origin alpha))
            omega (error "Invalid index."))
        (make-virtual
         'vader-select
         :base omega :index-origin index-origin
         :argument (let ((alpha (render-varrays alpha))
                         (axes (render-varrays axes))
                         (axis (if axes (if (vectorp (first axes))
                                            ;; the inefficient array-to-list is used here in case of nested
                                            ;; alpha arguments like (⊂1 2 3)⌷...
                                            (coerce (first axes) 'list)
                                            (if (integerp (first axes))
                                                (list (first axes)))))))
                     ;; (print (list :ax axis coords))
                     (if axis (let ((cx 0))
                                (loop :for dim :below (length (shape-of omega))
                                      :collect (if (member (+ dim index-origin) axis)
                                                   (let ((c (if (not (arrayp alpha))
                                                                alpha (if (zerop (rank alpha))
                                                                          (aref alpha)
                                                                          (aref alpha cx)))))
                                                     (incf cx)
                                                     c))))
                         ;; pad coordinates with nil elements in the case of an elided reference
                         (append (if (not (arrayp alpha))
                                     (list alpha)
                                     (if (zerop (rank alpha))
                                         (list (aref alpha))
                                         (loop :for a :across alpha :collect a)))
                                 (loop :for i :below (- (length (shape-of omega))
                                                        (if (or (not (arrayp alpha))
                                                                (zerop (rank alpha)))
                                                            1 (length alpha)))
                                       :collect nil))))))))

(defun format-array (print-precision)
  "Use (aplesque:array-impress) to print an array and return the resulting character array, with the option of specifying decimal precision. Used to implement monadic and dyadic [⍕ format]."
  (lambda (omega &optional alpha)
    (let ((omega (render-varrays omega))
          (alpha (render-varrays alpha)))
      (when (and alpha (not (integerp alpha)))
        (error (concatenate 'string "The left argument to ⍕ must be an integer specifying"
                            " the precision at which to print floating-point numbers.")))
      (if (characterp omega)
          omega (array-impress
                 omega :collate t
                       :segment (lambda (number &optional segments)
                                  (count-segments number (if alpha (- alpha) print-precision)
                                                  segments))
                       :format (lambda (number &optional segments rps)
                                 (print-apl-number-string number segments print-precision alpha rps)))))))

(defun format-array-uncollated (print-precision-default)
  "Generate a function using (aplesque:array-impress) to print an array in matrix form without collation. Used to implement ⎕FMT."
  (lambda (input &optional print-precision)
    (let ((input (render-varrays input))
          (print-precision (or (render-varrays print-precision)
                               print-precision-default))
          (is-not-nested t))
      (when (and print-precision (not (integerp print-precision)))
        (error "The left argument to ⍕ must be an integer specifying ~a"
               "the precision at which to print floating-point numbers."))
      ;; only right-indent if this is a nested array; this is important for box-drawing functions
      (when (arrayp input)
        (xdotimes input (x (size input))
          (when (arrayp (row-major-aref input x))
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
         (array (if (not (and is-scalar (not scalar-assigned)))
                    array (aref array)))
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

(defun invert-assigned-varray (object &optional order)
  "Generate the inverted deferred computation object that serves to verify indices in a selection array implementing assignment by selection, like the one expressed by {na←3⍴⊂⍳4 ⋄ (1↑⊃na[1])←⍵ ⋄ na} 99."
  (if (varrayp object)
      (invert-assigned-varray (typecase object (vacomp-each (varray::vacmp-omega object))
                                (t (varray::vader-base object)))
                              (typecase object (vader-identity order)
                                ;; omit identity objects, this is for selective
                                ;; assignment cases like ⍺←⊢ ⋄ (⍺ ⍺⍺ X)←Y
                                (vader-select (append order (list object)))
                                (vader-pick (append order (list object)))
                                ;; pick and select objects are shifted to the end of the list
                                (t (cons object order))))
      (let ((output) (ivec))
        (loop :for o :in order
              :do (typecase o (vacomp-each (setf (varray::vacmp-omega o) (or output object)))
                            (vader-select (setf (varray::vasel-selector o) (or output object)))
                            ;; in the case of an enlist object, generate the index array and pass it
                            ;; back via the second value, needed for cases like
                            ;; 'a' {names←'Kent' 'Alan' 'Ryan' ⋄ ((⍺=∊names)/∊names)←⍵ ⋄ names} '*'
                            (t (setf (varray::vader-base o)
                                     (or output (if (not (typep o 'vader-enlist))
                                                    object (setf ivec (generate-index-array object)))))))
                  (setf output o))
        (values (or output object) ivec))))

(defun assign-by-selection (prime-function function value omega &key index-origin)
  "Assign to elements of an array selected by a function. Used to implement (3↑x)←5 etc."
  (let ((function-meta (handler-case (funcall prime-function :get-metadata nil) (error () nil))))
    ;; (setf ggi (invert-assigned-varray (funcall function omega)))
    (multiple-value-bind (base-object ivec) (invert-assigned-varray (funcall function omega))
      (typecase base-object
        (varray::vader-select
         (setf (varray::vasel-assign base-object) value)
         base-object)
        (varray::vader-pick
         (setf (varray::vapick-assign base-object) value
               (varray::vapick-selector base-object)
               ;; assign the selector if the omega is a virtual array, this excludes
               ;; cases like x←⍳4 ⋄ (⊃x)←2 2⍴⍳4 ⋄ x
               ;; TODO: normalize this check for full lazy operation
               (when (typep (varray::vader-base base-object) 'varray::varray)
                 (varray::vader-base base-object))
               (varray::vader-base base-object) omega)
         base-object)
        ;; In the case of an index vector returned as the second value from invert-assigned-varray,
        ;; assignment is being done according to processing of an enlist of the input array, thus
        ;; selection is done using a vector of matching enlisted indices. Thus a vector of the indices
        ;; and a nested index array must be passed to the select object indexer for use indexing.
        (t (make-instance 'vader-select :base omega :index-origin index-origin :assign value
                                        :selector (if ivec (list :ebase ivec
                                                                 :eindices (render-varrays base-object))
                                                      (funcall function omega))))))))

(defun operate-scanning (function index-origin last-axis inverse &key axis)
  "Scan a function across an array along a given axis. Used to implement the [\ scan] operator with an option for inversion when used with the [⍣ power] operator taking a negative right operand."
  (lambda (omega &optional alpha environment) ;; alpha is only used to pass an axis reassignment
    (declare (ignore environment))
    (setq omega (render-varrays omega)
          alpha (render-varrays alpha))
    (if (not (arrayp omega))
        (if (eq :get-metadata omega)
            (list :inverse (let ((inverse-function (getf (funcall function :get-metadata nil) :inverse)))
                             (operate-scanning inverse-function index-origin last-axis t :axis axis))
                  :valence :monadic)
            (if (eq :reassign-axes omega)
                (operate-scanning function index-origin last-axis inverse :axis alpha)
                omega))
        (let* ((odims (dims omega))
               (fn-rendered (lambda (o a) (render-varrays (funcall function o a))))
               (axis (when axis (list (render-varrays (first axis)))))
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
          (xdotimes output (i (size output) :synchronous-if (or (not (side-effect-free function))
                                                                sao-copy (getf fn-meta :commutative)))
            (declare (optimize (safety 1)))
            (let ((value) (vector-index (mod (floor i increment) rlen))
                  (base (+ (mod i increment) (* increment rlen (floor i (* increment rlen))))))
              (if inverse
                  (let ((original (disclose (row-major-aref
                                             omega (+ base (* increment vector-index))))))
                    (setq value (if (zerop vector-index)
                                    original
                                    (funcall fn-rendered original
                                             (disclose
                                              (row-major-aref
                                               omega (+ base (* increment (1- vector-index)))))))))
                  ;; faster method for commutative functions
                  ;; NOTE: xdotimes will not work with this method
                  (if (or sao-copy (getf fn-meta :commutative))
                      (setq value (if (zerop vector-index)
                                      (row-major-aref omega base)
                                      (render-varrays
                                       (funcall (if sao-copy (getf fn-meta :inverse-right)
                                                    fn-rendered)
                                                (row-major-aref
                                                 output (+ base (* increment (1- vector-index))))
                                                (row-major-aref
                                                 (or sao-copy omega)
                                                 (+ base (* increment vector-index)))))))
                      (loop :for ix :from vector-index :downto 0
                            :do (let ((original (row-major-aref omega (+ base (* ix increment)))))
                                  (setq value (if (not value) (disclose original)
                                                  (funcall fn-rendered value (disclose original))))))))
              (setf (row-major-aref output i) value)))
          (render-varrays output)))))

(defun operate-each (operand)
  (op-compose 'vacomp-each :left operand))

(defun operate-commuting (operand)
  "Generate a function with reversed or mirrored arguments. Used to implement [⍨ commute]."
  (lambda (omega &optional alpha environment)
    (declare (ignore environment))
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
  (lambda (omega &optional alpha environment)
    (declare (ignore environment))
    (let* ((keys (or alpha omega))
           (key-test #'equalp)
           (keys-dims (varray::shape-of keys))
           (increment (reduce #'* (rest keys-dims)))
           (indices-of (lambda (item vector)
                         (let ((collection))
                           (loop :for i :below (first (varray::shape-of vector))
                                 :do (let ((section (if (= 1 increment) (aref vector i)
                                                        (make-array increment :element-type
                                                                    (varray::etype-of vector)
                                                                    :displaced-to (render-varrays vector)
                                                                    :displaced-index-offset
                                                                    (* i increment)))))
                                       (if (funcall key-test item section)
                                           (push (+ index-origin i) collection))))
                           collection)))
           (key-table (make-hash-table :test key-test))
           (elisions (loop :for i :below (1- (varray::rank-of omega)) :collect nil))
           (key-list)
           (key-indexer (varray::generator-of keys)))
      (dotimes (i (first keys-dims))
        (let ((item (if (= 1 increment) (funcall key-indexer i)
                        (make-array increment :element-type (varray::etype-of keys)
                                              :displaced-to (render-varrays keys)
                                              :displaced-index-offset (* i increment)))))
          (when (loop :for key :in key-list :never (funcall key-test item key))
            (push item key-list))
          (push i (gethash item key-table))))
      (let ((item-sets (loop :for key :in (reverse key-list)
                             :collect (funcall function
                                               (if alpha (choose omega
                                                                 (cons (apply #'vector
                                                                              (reverse
                                                                               (gethash key key-table)))
                                                                       elisions))
                                                   (let ((items (funcall indices-of key keys)))
                                                     (make-array (length items)
                                                                 :initial-contents (reverse items))))
                                               key))))
        (make-instance 'vader-mix :base (apply #'vector item-sets) :subrendering t :axis 1)))))

(defun operate-producing-outer (operand)
  "Generate a function producing an outer product. Used to implement [∘. outer product]."
  (lambda (omega alpha &optional environment blank)
    (declare (ignore environment blank))
    (if (eq :get-metadata omega)
        (let* ((operand-meta (funcall operand :get-metadata nil))
               (operand-inverse (getf operand-meta :inverse))
               ;; TODO: create lazy inverse outer product
               (operand-irendering (lambda (o a) (render-varrays (funcall operand-inverse o a)))))
          (list :inverse-right (lambda (omega alpha)
                                 (inverse-outer-product (render-varrays alpha) operand-irendering
                                                        (render-varrays omega)
                                                        (side-effect-free operand)))
                :inverse (lambda (omega alpha)
                           (inverse-outer-product (render-varrays omega) operand-irendering
                                                  nil (side-effect-free operand)
                                                  alpha))))
        (make-instance 'vacomp-produce :right operand :left :outer :omega omega :alpha alpha))))

(defun operate-producing-inner (right left)
  "Generate a function producing an inner product. Used to implement [. inner product]."
  (lambda (alpha omega &optional environment blank)
    (declare (ignore environment blank))
    (if (or (zerop (varray::size-of omega))
            (zerop (varray::size-of alpha)))
        (if (or (< 1 (varray::rank-of omega)) (< 1 (varray::rank-of alpha)))
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
  (let ((fn-right (when (functionp right) right))
        (fn-left (when (functionp left) left))
        (temp))
    (lambda (omega &optional alpha environment blank)
      (declare (ignore environment blank)) ;; blank allows the case of :get-metadata nil arguments
      (if (eq :get-metadata omega)
          (list :inverse (lambda (omega &optional alpha)
                           (when (and fn-right fn-left)
                             (setq temp fn-right
                                   fn-right fn-left
                                   fn-left temp))
                           (let* ((meta-right (when fn-right (apply fn-right :get-metadata
                                                                    (when (or alpha (not fn-left))
                                                                      (list nil)))))
                                  (meta-left (when fn-left (apply fn-left :get-metadata
                                                                  (when (or alpha (not fn-right))
                                                                    (list nil)))))
                                  (fn-right (when fn-right (or (getf meta-right
                                                                     (if (or alpha (not fn-left))
                                                                         :inverse :inverse-right))
                                                               (getf meta-right :inverse))))
                                  (fn-left (when fn-left
                                             (if (and alpha fn-right)
                                                 fn-left (or (getf meta-left :inverse-right)
                                                             (getf meta-left :inverse))))))
                             (if (and fn-right fn-left)
                                 (let ((processed (if alpha (funcall fn-right omega alpha)
                                                      (funcall fn-right omega))))
                                   (funcall fn-left processed))
                                 (if alpha (error "This function does not take a left argument.")
                                     (funcall (or fn-right fn-left)
                                              (if fn-right omega right)
                                              (if fn-left omega left)))))))
          (if (and fn-right fn-left) ;; TODO: the force render is needed for i.e. ⌊10_000×+∘÷/40/1
              (let ((processed (render-varrays (funcall fn-right omega))))
                (if alpha (funcall fn-left processed alpha)
                    (funcall fn-left processed)))
              (if alpha (error "This function does not take a left argument.")
                  (funcall (or fn-right fn-left)
                           (if fn-right omega right)
                           (if fn-left omega left))))))))

(defun operate-before (right-fn left-fn)
  "Generate a function by linking together two functions with the left one called first. Used to implement [⍛ reverse compose]."
  (lambda (omega &optional alpha)
    (if alpha (funcall right-fn omega (funcall left-fn alpha))
        (error "A function composed with [⍛ reverse compose] must have a left argument."))))

(defun operate-at-rank (rank function)
  "Generate a function applying a function to sub-arrays of the arguments. Used to implement [⍤ rank]."
  (lambda (omega &optional alpha environment blank)
    (declare (ignore environment blank))
    (let* ((rank (render-varrays rank))
           (odims (shape-of omega)) (adims (shape-of alpha))
           (orank (varray::rank-of omega)) (arank (varray::rank-of alpha))
           (fn-meta (funcall function :get-metadata nil))
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
                             (make-array 3 :initial-contents (list (aref rank 1)
                                                                   (aref rank 0) (aref rank 1)))
                             (if (= 3 (size rank))
                                 rank (when (or (< 1 (rank rank)) (< 3 (size rank)))
                                        (error "Right operand of [⍤ rank] must be a scalar integer or ~a"
                                               "integer vector no more than 3 elements long.")))))))
           (ocrank (aref rank 2))
           (acrank (aref rank 1))
           (omrank (aref rank 0))
           (orankdelta (- orank (if alpha ocrank omrank)))
           (odivs (when (<= 0 orankdelta) (make-array (subseq odims 0 orankdelta))))
           (odiv-dims (when odivs (subseq odims orankdelta)))
           (odiv-size (when odivs (reduce #'* odiv-dims)))
           (arankdelta (- arank acrank))
           (adivs (when (and alpha (<= 0 arankdelta))
                    (make-array (subseq adims 0 arankdelta))))
           (adiv-dims (when adivs (subseq adims arankdelta)))
           (adiv-size (when alpha (reduce #'* adiv-dims))))

      (when (or (not (and (integerp ocrank) (or (zerop ocrank) (plusp ocrank))))
                (not (and (integerp acrank) (or (zerop acrank) (plusp acrank))))
                (not (and (integerp omrank) (or (zerop omrank) (plusp omrank)))))
        (error "Right operand to rank may only be a scalar or vector of 2-3 dimensional indices."))
      (when (and alpha (eq :monadic (getf fn-meta :valence)))
        (error "Function composed with [⍤ rank] may not have a left argument."))
      (when (and (not alpha) (eq :dyadic (getf fn-meta :valence)))
        (error "Function composed with [⍤ rank] must have a left argument."))

      (if (eq omega :get-metadata)
          (append fn-meta (list :composed-by #\⍤))
          (if (and (getf fn-meta :on-axis)
                   (= 1 (if alpha ocrank omrank)))
              ;; if the composed function is directly equivalent to a function that operates
              ;; across an axis, as ⊖⍤1 and ⌽⍤1 are to ⌽, just reassign the axis
              (apply (if (eq :last (getf fn-meta :on-axis))
                         function (funcall function :reassign-axes (list (max orank arank))))
                     omega (when alpha (list alpha)))
              (flet ((generate-divs (div-array ref-array div-dims div-size)
                       (let ((ref-indexer (varray::generator-of ref-array)))
                         (dotimes (i (size div-array))
                           (setf (row-major-aref div-array i)
                                 (if (zerop (rank div-array)) ref-array
                                     (if (not div-dims) (funcall ref-indexer i)
                                         (make-instance 'varray::vader-subarray-displaced
                                                        :shape div-dims :index i
                                                        :base ref-array))))))))
                (when odivs (generate-divs odivs omega odiv-dims odiv-size))
                (if alpha (progn (when adivs (generate-divs adivs alpha adiv-dims adiv-size))
                                 (if (not (or odivs adivs))
                                     ;; if alpha and omega are scalar, just call the function on them
                                     (funcall function omega alpha)
                                     (let ((output (make-array (shape-of (if (> (rank-of odivs)
                                                                                (rank-of adivs))
                                                                             odivs adivs)))))
                                       (dotimes (i (size output))
                                         (let ((this-odiv (if (not odivs)
                                                              omega (if (zerop (rank odivs))
                                                                        (aref odivs)
                                                                        (row-major-aref odivs i))))
                                               (this-adiv (if (not adivs)
                                                              alpha (if (zerop (rank adivs))
                                                                        (aref adivs)
                                                                        (row-major-aref adivs i)))))
                                           (setf (row-major-aref output i)
                                                 (funcall function this-odiv this-adiv))))
                                       (make-instance 'vader-mix :base output :subrendering t
                                                                 :axis (max (rank odivs) (rank adivs))))))
                    (if (not odivs) ;; as above for an omega value alone
                        (funcall function omega)
                        (let ((output (make-array (dims odivs))))
                          (dotimes (i (size output))
                            (setf (row-major-aref output i)
                                  (funcall function (row-major-aref odivs i))))
                          (make-instance 'vader-mix :base output :subrendering t
                                         :axis (varray::rank-of output)))))))))))

(defun operate-atop (right-fn left-fn)
  "Generate a function applying two functions to a value in succession. Used to implement [⍤ atop]."
  (lambda (omega &optional alpha environment blank)
    (declare (ignore environment blank))
    (if alpha (funcall left-fn (funcall right-fn omega alpha))
        (funcall left-fn (funcall right-fn omega)))))

(defun operate-to-power (fetch-determinant function)
  "Generate a function applying a function to a value and successively to the results of prior iterations a given number of times. Used to implement [⍣ power]."
  (lambda (omega &optional alpha environment blank)
    (declare (ignore environment blank))
    (if (eq omega :get-metadata)
        (list :inverse (let* ((determinant (render-varrays (funcall fetch-determinant)))
                              (inverse-function (if (not (numberp determinant))
                                                    (getf (if alpha (funcall function :get-metadata nil)
                                                              (funcall function :get-metadata))
                                                          :inverse))))
                         (if (numberp determinant)
                             (operate-to-power (lambda () (- determinant)) function)
                             (operate-to-power fetch-determinant inverse-function))))
        (let ((determinant (render-varrays (funcall fetch-determinant))))
          (if (functionp determinant)
              ;; if the determinant is a function, loop until the result of its
              ;; evaluation with the current and prior values is zero
              (let ((arg omega) (prior-arg omega))
                (loop :for index :from 0 :while (or (zerop index)
                                                    (zerop (render-varrays
                                                            (funcall determinant prior-arg arg))))
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
  (lambda (omega &optional alpha environment blank)
    (declare (ignorable alpha environment blank))
    (let ((orank (varray::rank-of omega))
          (left-fn (when (functionp left) left))
          (right-fn (when (functionp right) right)))
      (if (and left-fn (or right-fn (= 1 (varray::rank-of right))
                           (not (or (arrayp right) (varray::varrayp right)))))
          (if right-fn (make-instance 'vader-select
                                      :base omega :index-origin index-origin
                                      :assign alpha :calling left-fn :assign-if right-fn)
              (let* ((mod-array (make-instance
                                 'vader-select :base omega :index-origin index-origin
                                               :argument (cons right (loop :for i :below (1- orank)
                                                                           :collect nil))))
                     (out-sub-array (if alpha (funcall left-fn mod-array alpha)
                                        (funcall left-fn mod-array))))
                (make-instance 'vader-select
                               :base omega :index-origin index-origin
                               :assign (funcall (if (and (not (arrayp right)) (> 2 orank)
                                                         (not (zerop (varray::rank-of out-sub-array))))
                                                    #'enclose #'identity)
                                                (render-varrays out-sub-array))
                               ;; IPV-TODO: removing this force render causes a bug, figure it out
                               :argument (cons right (loop :for i :below (1- orank) :collect nil)))))
          (if right-fn (make-instance 'vader-select
                                      :base omega :index-origin index-origin :assign-if right-fn
                                      :calling left-fn :assign (if left-fn alpha left))
              (make-instance 'vader-select
                             :base omega :index-origin index-origin
                             :calling left-fn :assign (if left-fn alpha left)
                             :argument (cons right (loop :for i :below (- orank (rank-of right))
                                                         :collect nil))))))))

(defun operate-stenciling (right-value left-function)
  "Generate a function applying a function via (aplesque:stencil) to an array. Used to implement [⌺ stencil]."
  (lambda (omega &optional alpha environment blank)
    (declare (ignore alpha environment blank))
    (setq omega (render-varrays omega)
          right-value (render-varrays right-value))
    (let ((left-fn-mod (lambda (o a) (render (funcall left-function o a)))))
      (flet ((iaxes (value index) (loop :for x :below (rank-of value) :for i :from 0
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
                          (stencil omega left-fn-mod window-dims movement
                                   (side-effect-free left-function)))))))))
