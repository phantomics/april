;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; library.lisp

(in-package #:april)

"This file contains the functions in April's 'standard library' that aren't provided by the aplesque package, mostly functions that are specific to the APL language and not generally applicable to array processing."

(defun without (omega alpha)
  "Remove elements in omega from alpha. Used to implement dyadic [~ without]."
  (flet ((compare (o a)
	   (funcall (if (and (characterp a) (characterp o))
			#'char= (if (and (numberp a) (numberp o))
				    #'= (lambda (a o) (declare (ignore a o)))))
		    o a)))
    (let ((included)
	  (omega-vector (if (or (vectorp omega)	(not (arrayp omega)))
			    (disclose omega)
			    (make-array (list (array-total-size omega))
					:displaced-to omega :element-type (element-type omega)))))
      (loop :for element :across alpha
	 :do (let ((include t))
	       (if (vectorp omega-vector)
		   (loop :for ex :across omega-vector
		      :do (if (compare ex element) (setq include nil)))
		   (if (compare omega-vector element) (setq include nil)))
	       (if include (setq included (cons element included)))))
      (make-array (list (length included)) :element-type (element-type alpha)
		  :initial-contents (reverse included)))))

(defun scalar-compare (omega alpha)
  "Compare two scalar values as appropriate for APL."
  (funcall (if (and (characterp alpha) (characterp omega))
	       #'char= (if (and (numberp alpha) (numberp omega))
			   #'= (lambda (a o) (declare (ignore a o)))))
	   omega alpha))

(defun count-to (index index-origin)
  "Implementation of APL's ⍳ function."
  (let ((index (disclose index)))
    (if (integerp index)
	(if (= 0 index) (vector)
	    (let ((output (make-array (list index) :element-type (list 'integer 0 index))))
	      (loop :for ix :below index :do (setf (aref output ix) (+ ix index-origin)))
	      output))
	(if (vectorp index)
	    (let ((output (make-array (array-to-list index))))
	      (across output (lambda (elem coords)
			       (declare (ignore elem))
			       (setf (apply #'aref output coords)
				     (make-array (length index)
						 :element-type
						 (list 'integer 0 (+ index-origin
								     (reduce #'max coords)))
						 :initial-contents
						 (if (= 0 index-origin)
						     coords (loop :for c :in coords
							       :collect (+ c index-origin)))))))
	      output)
	    (error "The argument to ⍳ must be an integer, i.e. ⍳9, or a vector, i.e. ⍳2 3.")))))

(defun shape (omega)
  "Get the shape of an array, implementing monadic [⍴ shape]."
  (if (or (not (arrayp omega))
	  (= 0 (rank omega)))
      #() (if (and (eql 'simple-array (first (type-of omega)))
		   (eq t (second (type-of omega)))
		   (eq nil (third (type-of omega))))
	      0 (if (vectorp omega)
		    (make-array 1 :element-type (list 'integer 0 (length omega))
				:initial-contents (list (length omega)))
		    (let* ((omega-dims (dims omega))
			   (max-dim (reduce #'max omega-dims)))
		      (make-array (list (length omega-dims))
				  :initial-contents omega-dims :element-type (list 'integer 0 max-dim)))))))

(defun reshape-array (metadata-symbol)
  "Wrap (aplesque:reshape-to-fit) so that dyadic [⍴ shape] can be implemented with the use of empty-array prototypes."
  (lambda (omega alpha)
    (let ((output (reshape-to-fit omega (if (arrayp alpha) (array-to-list alpha)
					    (list alpha))
				  :populator (build-populator metadata-symbol omega))))
      (if (and (= 0 (size output)) (arrayp (row-major-aref omega 0)))
	  (set-workspace-item-meta metadata-symbol output
				   :eaprototype
				   (make-prototype-of (funcall (if (= 0 (rank omega)) #'identity #'aref)
							       (row-major-aref omega 0)))))
      output)))

(defun at-index (omega alpha axes index-origin)
  "Find the value(s) at the given index or indices in an array. Used to implement [⌷ index]."
  (if (not (arrayp omega))
      (if (and (numberp alpha)
	       (= index-origin alpha))
	  omega (error "Invalid index."))
      (choose omega (let ((coords (funcall (if (arrayp alpha) #'array-to-list #'list)
					   (apply-scalar #'- alpha index-origin)))
			  ;; the inefficient array-to-list is used here in case of nested
			  ;; alpha arguments like (⊂1 2 3)⌷...
			  (axis (if (first axes) (loop :for item :across (first axes)
						    :collect (- item index-origin)))))
		      (if (not axis)
			  ;; pad coordinates with nil elements in the case of an elided reference
			  (append coords (loop :for i :below (- (rank omega) (length coords)) :collect nil))
			  (loop :for dim :below (rank omega)
			     :collect (if (member dim axis) (first coords))
			     :when (member dim axis) :do (setq coords (rest coords))))))))

(defun find-depth (omega)
  "Find the depth of an array, wrapping (aplesque:array-depth). Used to implement [≡ depth]."
  (if (not (arrayp omega))
      0 (array-depth omega)))

(defun find-first-dimension (omega)
  "Find the first dimension of an array. Used to implement [≢ first dimension]."
  (if (not (arrayp omega))
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
    (if (not (arrayp alpha))
	(if (not (arrayp omega))
	    (if (compare omega alpha) 1 0)
	    (if (not (loop :for item :across omega :never (compare item alpha)))
		1 0))
	(let* ((output (make-array (dims alpha) :element-type 'bit :initial-element 0))
	       (omega (enclose omega))
	       (to-search (if (vectorp omega)
			      omega (make-array (array-total-size omega)
						:displaced-to omega :element-type (element-type omega)))))
	  ;; TODO: this could be faster with use of a hash table and other additions
	  (dotimes (index (array-total-size output))
	    (let ((found))
	      (loop :for item :across to-search :while (not found)
		 :do (setq found (compare item (row-major-aref alpha index))))
	      (if found (setf (row-major-aref output index) 1))))
	  output))))
  
(defun where-equal-to-one (omega index-origin)
  "Return a vector of coordinates from an array where the value is equal to one. Used to implement [⍸ where]."
  (let* ((indices) (match-count 0)
	 (orank (rank omega)))
    (if (= 0 orank)
	(if (= 1 omega) 1 0)
	(progn (across omega (lambda (index coords)
			       (declare (dynamic-extent index coords))
			       (if (= 1 index)
				   (let* ((max-coord 0)
					  (coords (mapcar (lambda (i)
							    (setq max-coord
								  (max max-coord (+ i index-origin)))
							    (+ i index-origin))
							  coords)))
				     (incf match-count)
				     (setq indices (cons (if (< 1 orank)
							     (nest (make-array
								    orank :element-type (list 'integer 0 max-coord)
								    :initial-contents coords))
							     (first coords))
							 indices))))))
	       (if (not indices)
		   0 (make-array (list match-count)
				 :element-type (if (< 1 orank)
						   t (list 'integer 0 (reduce #'max indices)))
				 :initial-contents (reverse indices)))))))

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
			      :displaced-to (copy-array omega))))))

(defun ravel-array (index-origin)
  "Wrapper for aplesque [,ravel] function incorporating index origin from current workspace."
  (lambda (omega &optional axes)
    (ravel index-origin omega axes)))

(defun catenate-arrays (index-origin)
  "Wrapper for [, catenate] incorporating (aplesque:catenate) and (aplesque:laminate)."
  (lambda (omega alpha &optional axes)
    (let ((axis *first-axis-or-nil*))
      (if (floatp axis)
	  ;; laminate in the case of a fractional axis argument
	  (laminate alpha omega (ceiling axis))
	  ;; simply stack the arrays if there is no axis argument or it's an integer
	  (catenate alpha omega (or axis (max 0 (1- (max (rank alpha) (rank omega))))))))))

(defun catenate-on-first (index-origin)
  "Wrapper for [⍪ catenate first]; distinct from (catenate-arrays) because it does not provide the laminate functionality."
  (lambda (omega alpha &optional axes)
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

(defun section-array (index-origin metadata-symbol &optional inverse)
  "Wrapper for (aplesque:section) used for [↑ take] and [↓ drop]."
  (lambda (omega alpha &optional axes)
    (let* ((alpha-index alpha)
	   (alpha (if (arrayp alpha)
		      alpha (vector alpha)))
	   (output (section omega (if axes (make-array (rank omega)
						       :initial-contents
						       (loop :for axis :below (rank omega)
							  :collect (if inverse
								       (if (/= axis (- (first axes) index-origin))
									   0 alpha-index)
								       (if (= axis (- (first axes) index-origin))
									   alpha-index (nth axis (dims omega))))))
				      alpha)
			    :inverse inverse :populator (build-populator metadata-symbol omega))))
      ;; if the resulting array is empty and the original array prototype was an array, set the
      ;; empty array prototype accordingly
      (if (and (= 0 (size output))
	       (not inverse) (arrayp (row-major-aref omega 0)))
	  (set-workspace-item-meta metadata-symbol output
				   :eaprototype
				   (make-prototype-of (funcall (if (= 0 (rank omega)) #'identity #'aref)
							       (row-major-aref omega 0)))))
      output)))

(defun pick (index-origin)
  "Fetch an array element, within successively nested arrays for each element of the left argument."
  (lambda (omega alpha)
    (labels ((pick-point (point input)
	       (if (is-unitary point)
		   (let ((point (disclose point)))
		     ;; if this is the last level of nesting specified, fetch the element
		     (if (not (arrayp point))
			 (aref input (- point index-origin))
			 (if (vectorp point)
			     (apply #'aref input (loop :for p :across point :collect (- p index-origin)))
			     (error "Coordinates for ⊃ must be expressed by scalars or vectors."))))
		   ;; if there are more elements of the left argument left to go, recurse on the element designated
		   ;; by the first element of the left argument and the remaining elements of the point
		   (pick-point (if (< 2 (length point))
				   (make-array (1- (length point))
					       :initial-contents (loop :for i :from 1 :to (1- (length point))
								    :collect (aref point i)))
				   (aref point 1))
			       (disclose (pick-point (aref point 0) input))))))
      ;; TODO: swap out the vector-based point for an array-based point
      (if (= 1 (array-total-size omega))
	  (error "Right argument to dyadic ⊃ may not be unitary.")
	  (disclose (pick-point alpha omega))))))

(defun expand-array (degrees input axis metadata-symbol &key (compress-mode))
  "Wrapper for (aplesque:expand) implementing [/ replicate] and [\ expand]."
  (let ((output (expand degrees input axis :compress-mode compress-mode
			:populator (build-populator metadata-symbol input))))
    (if (and (= 0 (size output)) (arrayp input) (arrayp (row-major-aref input 0)))
	(set-workspace-item-meta metadata-symbol output
				 :eaprototype
				 (make-prototype-of (funcall (if (= 0 (rank input)) #'identity #'aref)
							     (row-major-aref input 0)))))
    output))

(defun array-intersection (omega alpha)
  "Return a vector of values common to two arrays. Used to implement [∩ intersection]."
  (let ((omega (enclose omega))
	(alpha (enclose alpha)))
    (if (or (not (vectorp alpha))
	    (not (vectorp omega)))
	(error "Arguments must be vectors.")
	(let* ((match-count 0)
	       (matches (loop :for item :across alpha :when (find item omega :test #'array-compare)
			   :collect item :and :do (incf match-count))))
	  (if (= 1 match-count)
	      (first matches)
	      (make-array (list match-count) :initial-contents matches
			  :element-type (type-in-common (element-type alpha) (element-type omega))))))))

(defun unique (omega)
  "Return a vector of unique values in an array. Used to implement [∪ unique]."
  (if (not (arrayp omega))
      omega (let ((vector (if (vectorp omega)
			      omega (re-enclose omega (make-array (1- (rank omega))
								  :element-type 'fixnum
								  :initial-contents
								  (loop :for i :from 1 :to (1- (rank omega))
								     :collect i))))))
	      (let ((uniques) (unique-count 0))
		(loop :for item :across vector :when (not (find item uniques :test #'array-compare))
		   :do (setq uniques (cons item uniques)
			     unique-count (1+ unique-count)))
		(if (= 1 unique-count)
		    (disclose (first uniques))
		    (make-array unique-count :element-type (element-type vector)
				:initial-contents (reverse uniques)))))))

(defun array-union (omega alpha)
  "Return a vector of unique values from two arrays. Used to implement [∪ union]."
  (let ((omega (enclose omega))
	(alpha (enclose alpha)))
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
	(displaced (if (< 1 (rank array)) (make-array (rest (dims array))
						      :displaced-to array
						      :element-type (element-type array))))
	(uniques) (increment (reduce #'* (rest (dims array)))))
    (loop :for x :below (first (dims array))
       :do (if (and displaced (< 0 x))
	       (adjust-array displaced (rest (dims array))
			     :displaced-to array :element-type (element-type array)
			     :displaced-index-offset (* x increment)))
       (if (member (or displaced (aref array x)) uniques :test #'array-compare)
	   (setf (aref output x) 0)
	   (setf uniques (cons (if displaced (make-array (rest (dims array)) :displaced-to array
							 :element-type (element-type array)
							 :displaced-index-offset (* x increment))
				   (aref array x))
			       uniques))))
    output))

(defun permute-array (index-origin)
  "Wraps (aops:permute) to permute an array, rearranging the axes in a given order or reversing them if no order is given. Used to implement monadic and dyadic [⍉ permute]."
  (lambda (omega &optional alpha)
    (if (not (arrayp omega))
	omega (aops:permute (if alpha (loop :for i :across (enclose alpha) :collect (- i index-origin))
				(loop :for i :from (1- (rank omega)) :downto 0 :collect i))
			    omega))))

(defun matrix-inverse (omega)
  "Invert a matrix. Used to implement monadic [⌹ matrix inverse]."
  (if (not (arrayp omega))
      (/ omega)
      (if (< 2 (rank omega))
	  (error "Matrix inversion only works on arrays of rank 2 or 1.")
	  (if (and (= 2 (rank omega)) (reduce #'= (dims omega)))
	      (invert-matrix omega)
	      (left-invert-matrix omega)))))

(defun matrix-divide (omega alpha)
  "Divide two matrices. Used to implement dyadic [⌹ matrix divide]."
  (each-scalar t (array-inner-product (invert-matrix omega)
				      alpha (lambda (arg1 arg2) (apply-scalar #'* arg1 arg2))
				      #'+)))

(defun encode (omega alpha)
  "Encode a number or array of numbers as per a given set of bases. Used to implement [⊤ encode]."
  (let* ((omega (if (arrayp omega)
		    omega (enclose omega)))
	 (alpha (if (arrayp alpha)
		    alpha (enclose alpha)))
	 (odims (dims omega)) (adims (dims alpha))
	 (last-adim (first (last adims)))
	 (out-coords (loop :for i :below (+ (- (rank alpha) (count 1 adims))
					    (- (rank omega) (count 1 odims))) :collect 0))
	 (output (make-array (or (append (loop :for dim :in adims :when (< 1 dim) :collect dim)
					 (loop :for dim :in odims :when (< 1 dim) :collect dim))
				 '(1))))
	 (dxc))
    (flet ((rebase (base-coords number)
	     (let ((operand number) (last-base 1)
		   (base 1) (component 1) (element 0))
	       (loop :for index :from (1- last-adim) :downto (first (last base-coords))
		  :do (setq last-base base
			    base (* base (apply #'aref alpha (append (butlast base-coords 1)
								     (list index))))
			    component (if (= 0 base)
					  operand (* base (nth-value 1 (floor (/ operand base)))))
			    operand (- operand component)
			    element (/ component last-base)))
	       element)))
      (across alpha (lambda (aelem acoords)
		      (declare (ignore aelem) (dynamic-extent acoords))
		      (across omega (lambda (oelem ocoords)
				      (declare (dynamic-extent oelem ocoords))
				      (setq dxc 0)
				      (if out-coords
					  (progn (loop :for dx :below (length acoords) :when (< 1 (nth dx adims))
						    :do (setf (nth dxc out-coords) (nth dx acoords)
							      dxc (1+ dxc)))
						 (loop :for dx :below (length ocoords) :when (< 1 (nth dx odims))
						    :do (setf (nth dxc out-coords) (nth dx ocoords)
							      dxc (1+ dxc)))))
				      (setf (apply #'aref output (or out-coords '(0)))
					    (rebase acoords oelem))))))
      (if (is-unitary output)
	  (disclose output)
	  (each-scalar t output)))))

(defun decode (omega alpha)
  "Decode an array of numbers as per a given set of bases. Used to implement [⊥ decode]."
  (let* ((omega (if (arrayp omega)
		    omega (enclose omega)))
	 (alpha (if (arrayp alpha)
		    alpha (enclose alpha)))
	 (odims (dims omega)) (adims (dims alpha))
	 (last-adim (first (last adims)))
	 (rba-coords (loop :for i :below (rank alpha) :collect 0))
	 (rbo-coords (loop :for i :below (rank omega) :collect 0))
	 (out-coords (loop :for i :below (max 1 (+ (1- (rank alpha)) (1- (rank omega)))) :collect 0))
	 (output (make-array (or (append (butlast adims 1) (rest odims)) '(1))))
	 (dxc))
    (flet ((rebase (base-coords number-coords)
	     (let ((base 1) (result 0) (bclen (length base-coords)))
	       (loop :for i :from 0 :to (- bclen 2)
		  :do (setf (nth i rba-coords) (nth i base-coords)))
	       (loop :for i :from 1 :to (1- (length number-coords))
		  :do (setf (nth i rbo-coords) (nth i number-coords)))
	       (if (and (not (is-unitary base-coords))
			(not (is-unitary number-coords))
			(/= (first odims) (first (last adims))))
		   (error (concatenate 'string "If neither argument to ⊥ is scalar, the first dimension"
				       " of the right argument must equal the last "
				       "dimension of the left argument."))
		   (loop :for index :from (if (< 1 last-adim) (1- last-adim) (1- (first odims)))
		      :downto 0 :do (setf (nth 0 rbo-coords) (if (< 1 (first odims)) index 0)
					  (nth (1- bclen) rba-coords) (if (< 1 last-adim) index 0))
			(incf result (* base (apply #'aref omega rbo-coords)))
			(setq base (* base (apply #'aref alpha rba-coords)))))
	       result)))
      (across alpha (lambda (aelem acoords)
		      (declare (ignore aelem) (dynamic-extent acoords))
		      (across omega (lambda (oelem ocoords)
				      (declare (ignore oelem) (dynamic-extent ocoords))
				      (setq dxc 0)
				      (loop :for dx :below (1- (length acoords))
					 :do (setf (nth dxc out-coords) (nth dx acoords)
						   dxc (1+ dxc)))
				      (if ocoords (loop :for dx :from 1 :to (1- (length ocoords))
						     :do (setf (nth dxc out-coords) (nth dx ocoords)
							       dxc (1+ dxc))))
				      (setf (apply #'aref output (or out-coords '(0)))
					    (rebase acoords ocoords)))
			      :elements (loop :for i :below (rank omega) :collect (if (= i 0) 0)))))
      :elements (loop :for i :below (rank alpha) :collect (if (= i (1- (rank alpha))) 0)))
    (if (is-unitary output) (disclose output) (each-scalar t output))))

(defun left-invert-matrix (in-matrix)
  "Perform left inversion of matrix. Used to implement [⌹ matrix inverse]."
  (let* ((input (if (= 2 (rank in-matrix))
		    in-matrix (make-array (list (length in-matrix) 1))))
	 (input-displaced (if (/= 2 (rank in-matrix))
			      (make-array (list 1 (length in-matrix)) :element-type (element-type input)
					  :displaced-to input))))
    (if input-displaced (loop :for i :below (length in-matrix) :do (setf (row-major-aref input i)
									 (aref in-matrix i))))
    (let ((result (array-inner-product (invert-matrix (array-inner-product (or input-displaced
									       (aops:permute '(1 0) input))
									   input #'* #'+))
				       (or input-displaced (aops:permute '(1 0) input))
				       #'* #'+)))
      (if (= 1 (rank in-matrix))
	  (make-array (size result) :element-type (element-type result) :displaced-to result)
	  result))))

(defun format-array (print-precision)
  "Use (aplesque:array-impress) to print an array and return the resulting character array, with the option of specifying decimal precision. Used to implement monadic and dyadic [⍕ format]."
  (lambda (omega &optional alpha)
    (if (and alpha (not (integerp alpha)))
	(error (concatenate 'string "The left argument to ⍕ must be an integer specifying"
			    " the precision at which to print floating-point numbers.")))
    (array-impress omega :collate t
		   :segment (lambda (number &optional segments)
			      (aplesque::count-segments number (if alpha (- alpha) print-precision)
							segments))
		   :format (lambda (number &optional segments rps)
			     (print-apl-number-string number segments print-precision alpha rps)))))

(defun generate-index-array (array)
  "Given an array, generate an array of the same shape whose each cell contains its row-major index."
  (let ((output (make-array (dims array) :element-type (list 'integer 0 (size array)))))
    (dotimes (i (size array)) (setf (row-major-aref output i) i))
    output))

(defun assign-selected (array indices values)
  "Assign array values selected using one of the functions [↑ take], [↓ drop], [\ expand] or [⊃ pick]."
  (if (or (= 0 (rank values))
	  (and (= (rank indices) (rank values))
	       (loop :for i :in (dims indices) :for v :in (dims values) :always (= i v))))
      ;; if the data to be assigned is not a scalar value, a new array must be created to ensure
      ;; that the output array will be compatible with all assigned and original values
      (let* ((to-copy-input (not (and (= 0 (rank values))
				      (or (eq t (element-type array))
					  (and (listp (type-of array))
					       (or (eql 'simple-vector (first (type-of array)))
						   (and (eql 'simple-array (first (type-of array)))
							(typep values (second (type-of array))))))))))
	     (output (if (not to-copy-input)
			 array (make-array (dims array) :element-type (if (/= 0 (rank values))
									  t (assign-element-type values)))))
	     (assigned-indices (if to-copy-input
				   (make-array (size array) :element-type 'bit :initial-element 0))))
	;; TODO: is assigning bits slow?
	;; iterate through the items to be assigned and, if an empty array has been initialized for
	;; the output, store the indices that have been assigned to new data
	(dotimes (i (size indices))
	  (setf (row-major-aref output (row-major-aref indices i))
		(if (= 0 (rank values)) values (row-major-aref values i)))
	  (if to-copy-input (setf (aref assigned-indices (row-major-aref indices i)) 1)))
	;; if the original array was assigned to just return it, or if a new array was created
	;; iterate through the old array and copy the non-assigned data to the output
	(if to-copy-input (dotimes (i (size array))
			    (if (= 0 (aref assigned-indices i))
				(setf (row-major-aref output i) (row-major-aref array i)))))
	output)
      (error "Area of array to be reassigned does not match shape of values to be assigned.")))

(defmacro apply-reducing (operation-symbol operation axes &optional first-axis)
  "Generate a function reducing an array along an axis by a function. Used to implement [/ reduce]."
  (let ((omega (gensym)) (o (gensym)) (a (gensym)) (symstring (string operation-symbol)))
    `(lambda (,omega)
       (if (= 0 (size ,omega))
	   (if (/= 1 (rank ,omega))
	       ;; if reducing an empty vector, return the identity operator for compatible lexical functions
	       ;; higher-dimensional empty arrays will yield an [⍬ empty vector]
	       #() ,(or (if (= 1 (length symstring))
			    (second (assoc (aref symstring 0)
					   '((#\+ 0) (#\- 0) (#\× 1) (#\÷ 1) (#\⋆ 1) (#\* 1) (#\! 1) 
					     (#\< 0) (#\≤ 1) (#\= 1) (#\≥ 1) (#\> 0) (#\≠ 0) (#\| 0)
					     (#\^ 1) (#\∧ 1) (#\∨ 0) (#\⌈ most-negative-long-float)
					     (#\⌊ most-positive-long-float))
					   :test #'char=)))
			'(error "Invalid function for reduction of empty array.")))
	   (if (= 1 (size ,omega))
	       ,omega (disclose-atom
		       (do-over ,omega (lambda (,o ,a) (apl-call ,operation-symbol ,operation ,a ,o))
				,(if axes `(- ,(first axes) index-origin)
				     (if first-axis 0 `(1- (rank ,omega))))
				:reduce t :in-reverse t)))))))

(defmacro apply-scanning (operation-symbol operation axes &optional first-axis)
  "Generate a function scanning a function over an array. Used to implement [\ scan]."
  (let ((omega (gensym)) (o (gensym)) (a (gensym)))
    `(lambda (,omega)
       (do-over ,omega (lambda (,o ,a) (apl-call ,operation-symbol ,operation ,o ,a))
		,(if axes `(- ,(first axes) index-origin)
		     (if first-axis 0 `(1- (rank ,omega))))))))

(defmacro apply-to-each (symbol operation-monadic operation-dyadic)
  "Generate a function applying a function to each element of an array. Used to implement [¨ each]."
  (let ((index (gensym)) (coords (gensym)) (output (gensym)) (item (gensym))
	(omega (gensym)) (alpha (gensym)) (a (gensym)) (o (gensym))
	(monadic-op (if (and (listp operation-monadic)
			     (eql 'with-properties (first operation-monadic)))
			(third operation-monadic) operation-monadic))
	(dyadic-op (if (and (listp operation-dyadic)
			    (eql 'with-properties (first operation-dyadic)))
		       (third operation-monadic) operation-dyadic)))
    (flet ((expand-dyadic (a1 a2 &optional reverse)
	     ;; the enclose-clause here and the (arrayp ,a1) clause below are added just so that the compiled
	     ;; clause will not cause problems when expanding with an explicit scalar argument, as with 3/¨⍳3
	     (let ((call (if reverse `(nest (apl-call ,symbol ,dyadic-op ,index ,a2))
			     `(nest (apl-call ,symbol ,dyadic-op ,a2 ,index)))))
	       `(let ((,output (make-array (dims ,a1))))
		  (across ,a1 (lambda (,index ,coords)
				(declare (dynamic-extent ,index ,coords))
				(setf (apply #'aref ,output ,coords)
				      (each-scalar t ,call))))
		  ,output))))
      `(lambda (,omega &optional ,alpha)
	 (declare (ignorable ,alpha))
	 (each-scalar
	  t ,(if (or (not (listp dyadic-op))
		     (not (listp (second dyadic-op)))
		     (< 1 (length (second dyadic-op))))
		 ;; don't create the dyadic clauses if the function being passed is monadic-only
		 `(if ,alpha (cond ((not (arrayp ,omega))
				    ,(expand-dyadic alpha omega))
				   ((not (arrayp ,alpha))
				    ,(expand-dyadic omega alpha t))
				   ((is-unitary ,omega)
				    ,(expand-dyadic alpha `(disclose ,omega)))
				   ((is-unitary ,alpha)
				    ,(expand-dyadic omega `(disclose ,alpha) t))
				   ((and (= (size ,alpha) (size ,omega))
					 (= (rank ,alpha) (rank ,omega))
					 (loop :for ,a :in (dims ,alpha) :for ,o :in (dims ,omega)
					      :always (= ,a ,o)))
				    (aops:each (lambda (,o ,a)
						 (nest (apl-call ,symbol ,dyadic-op ,o ,a)))
					       ,omega ,alpha))
				   (t (error "Mismatched argument shapes to ¨.")))
		      (aops:each (lambda (,item) (nest (apl-call ,symbol ,monadic-op (disclose ,item))))
				 ,omega))
		 `(aops:each (lambda (,item) (nest (apl-call ,symbol ,monadic-op (disclose ,item))))
			     ,omega)))))))

(defmacro apply-commuting (symbol operation-dyadic)
  "Generate a function applying a function to arguments in reverse order, or duplicating a single argument. Used to implement [⍨ commute]."
  (let ((omega (gensym)) (alpha (gensym)))
    `(lambda (,omega &optional ,alpha)
       (apl-call ,symbol ,operation-dyadic (if ,alpha ,alpha ,omega)
		 ,omega))))

(defmacro apply-to-grouped (symbol operation-dyadic)
  "Generate a function applying a function to items grouped by a criterion. Used to implement [⌸ key]."
  ;; TODO: eliminate consing here
  (let ((key (gensym)) (keys (gensym)) (key-test (gensym)) (indices-of (gensym))
	(key-table (gensym)) (key-list (gensym)) (item-sets (gensym)) (li (gensym))
	(item (gensym)) (items (gensym)) (vector (gensym)) (coords (gensym))
	(alpha (gensym)) (omega (gensym)))
    `(lambda (,omega &optional ,alpha)
       (let* ((,keys (if ,alpha ,alpha ,omega))
	      (,key-test #'equalp)
	      (,indices-of (lambda (,item ,vector)
			     (loop :for ,li :below (length ,vector)
				:when (funcall ,key-test ,item (aref ,vector ,li))
				:collect (+ index-origin ,li))))
	      (,key-table (make-hash-table :test ,key-test))
	      (,key-list))
	 (across ,keys (lambda (,item ,coords)
			 (declare (dynamic-extent ,item ,coords))
			 (if (loop :for ,key :in ,key-list :never (funcall ,key-test ,item ,key))
			     (setq ,key-list (cons ,item ,key-list)))
			 (setf (gethash ,item ,key-table)
			       (cons (apply #'aref (cons ,omega ,coords))
				     (gethash ,item ,key-table)))))
	 (let* ((,item-sets (loop :for ,key :in (reverse ,key-list)
			       :collect (apl-call ,symbol ,operation-dyadic
						  (let ((,items (if ,alpha (gethash ,key ,key-table)
								    (funcall ,indices-of
									     ,key ,keys))))
						    (make-array (list (length ,items))
								:initial-contents
								(reverse ,items)))
						  ,key))))
	   (mix-arrays 1 (apply #'vector ,item-sets)))))))

(defmacro apply-producing-inner (right-symbol right-operation left-symbol left-operation)
  "Generate a function finding the inner product of an array using two functions. Used to implement [. inner/outer product]."
  (let* ((op-right `(lambda (alpha omega) (apl-call ,right-symbol ,right-operation omega alpha)))
	 (op-left `(lambda (alpha omega) (apl-call ,left-symbol ,left-operation omega alpha)))
	 (result (gensym)) (alpha (gensym)) (omega (gensym)))
    `(lambda (,omega ,alpha)
       (if (and (not (arrayp ,omega))
		(not (arrayp ,alpha)))
	   (funcall (lambda (,result)
		      (if (not (and (arrayp ,result) (< 1 (rank ,result))))
			  ,result (vector ,result)))
		    ;; enclose the result in a vector if its rank is > 1
		    ;; to preserve the rank of the result
		    (reduce ,op-left (aops:each (lambda (e) (aops:each #'disclose e))
						(apply-scalar ,op-right ,alpha ,omega))))
	   (each-scalar t (array-inner-product ,alpha ,omega ,op-right ,op-left))))))

(defmacro apply-producing-outer (right-symbol right-operation)
  "Generate a function finding the outer product of an array using a functions. Used to implement [. inner/outer product]."
  (let* ((op-right `(lambda (alpha omega) (apl-call ,right-symbol ,right-operation omega alpha)))
	 (alpha (gensym)) (omega (gensym)))
    `(lambda (,omega ,alpha)
       (if (and (not (arrayp ,omega)) (not (arrayp ,alpha)))
	   (funcall ,op-right ,alpha ,omega)
	   (each-scalar t (array-outer-product ,alpha ,omega ,op-right))))))

(defmacro apply-composed (right-symbol right-value right-function-monadic right-function-dyadic
			  left-symbol left-value left-function-monadic left-function-dyadic is-confirmed-monadic)
  "Generate a function by linking together two functions or a function curried with an argument. Used to implement [∘ compose]."
  (let* ((alpha (gensym)) (omega (gensym)) (processed (gensym))
	 (fn-right (or right-function-monadic right-function-dyadic))
	 (fn-left (or left-function-monadic left-function-dyadic)))
    `(lambda (,omega &optional ,alpha)
       (declare (ignorable ,alpha))
       ,(if (and fn-right fn-left)
	    (let ((clauses (list `(apl-call ,left-symbol ,left-function-dyadic ,processed ,alpha)
				 `(apl-call ,left-symbol ,left-function-monadic ,processed))))
	      `(let ((,processed (apl-call ,right-symbol ,right-function-monadic ,omega)))
		 ,(if is-confirmed-monadic (second clauses)
		      `(if ,alpha ,@clauses))))
	    `(apl-call :fn ,(or right-function-dyadic left-function-dyadic)
		       ,(if (not fn-right) right-value omega)
		       ,(if (not fn-left) left-value omega))))))

(defmacro apply-over (right-symbol right-function-monadic
		      left-symbol left-function-monadic left-function-dyadic)
  "Generate a function combining two functions, with the second called on the results of the monadic first function called on the argument(s). Used to implement [⍥ over]."
  (let ((alpha (gensym)) (omega (gensym)))
    `(lambda (,omega &optional ,alpha)
       (if ,alpha (apl-call ,left-symbol ,left-function-dyadic
			    (apl-call ,right-symbol ,right-function-monadic ,omega)
			    (apl-call ,right-symbol ,right-function-monadic ,alpha))
	   (apl-call ,left-symbol ,left-function-monadic
		     (apl-call ,right-symbol ,right-function-monadic ,omega))))))

(defmacro apply-at-rank (right-value left-symbol left-function-monadic left-function-dyadic)
  "Generate a function applying a function to sub-arrays of the arguments. Used to implement [⍤ rank]."
  (let ((rank (gensym)) (orank (gensym)) (arank (gensym)) (fn (gensym))
	(romega (gensym)) (ralpha (gensym)) (alpha (gensym)) (omega (gensym))
	(o (gensym)) (a (gensym)) (r (gensym)))
    ;; TODO: eliminate consing here
    `(lambda (,omega &optional ,alpha)
       (let* ((,rank (disclose ,right-value))
	      (,orank (rank ,omega))
	      (,arank (rank ,alpha))
	      (,fn (if (not ,alpha)
		       (lambda (,o) (apl-call ,left-symbol ,left-function-monadic ,o))
		       (lambda (,o ,a) (apl-call ,left-symbol ,left-function-dyadic ,o ,a))))
	      (,romega (if (and ,omega (< ,rank ,orank))
			   (re-enclose ,omega (each (lambda (,r) (- ,r index-origin))
						    (make-array (list ,rank)
								:initial-contents
								(nthcdr (- ,orank ,rank)
									(iota ,orank :start index-origin)))))))
	      (,ralpha (if (and ,alpha (< ,rank ,arank))
			   (re-enclose ,alpha (each (lambda (,r) (- ,r index-origin))
						    (make-array (list ,rank)
								:initial-contents
								(nthcdr (- ,arank ,rank)
									(iota ,arank :start index-origin))))))))
	 (if ,alpha (merge-arrays (if ,romega (if ,ralpha (each ,fn ,romega ,ralpha)
						  (each ,fn ,romega
							(make-array (dims ,romega)
								    :initial-element ,alpha)))
				      (if ,ralpha (each ,fn (make-array (dims ,ralpha)
									:initial-element ,omega)
							,ralpha)
					  (funcall ,fn ,omega ,alpha))))
	     (if ,romega (merge-arrays (each ,fn ,romega) :nesting nil)
		 (funcall ,fn ,omega)))))))

(defmacro apply-to-power (op-right sym-left left-function-monadic left-function-dyadic)
  "Generate a function applying a function to a value and successively to the results of prior iterations a given number of times. Used to implement [⍣ power]."
  (let ((alpha (gensym)) (omega (gensym)) (arg (gensym)) (index (gensym)))
    `(lambda (,omega &optional ,alpha)
       (let ((,arg (disclose ,omega)))
	 (loop :for ,index :below (disclose ,op-right)
	    :do (setq ,arg (if ,alpha (apl-call ,sym-left ,left-function-dyadic ,arg ,alpha)
			       (apl-call ,sym-left ,left-function-monadic ,arg))))
	 ,arg))))

(defmacro apply-until (sym-right op-right sym-left op-left)
  "Generate a function applying a function to a value and successively to the results of prior iterations until a condition is net. Used to implement [⍣ power]."
  (let ((alpha (gensym)) (omega (gensym)) (arg (gensym)) (prior-arg (gensym)))
    `(lambda (,omega &optional ,alpha)
       (declare (ignorable ,alpha))
       (let ((,arg ,omega)
	     (,prior-arg ,omega))
	 (loop :while (= 0 (apl-call ,sym-right ,op-right ,prior-arg ,arg))
	    :do (setq ,prior-arg ,arg
		      ,arg (if ,alpha (apl-call ,sym-left ,op-left ,arg ,alpha)
			       (apl-call ,sym-left ,op-left ,arg))))
	 ,arg))))

(defmacro apply-at (right-symbol right-value right-function-monadic
		    left-symbol left-value left-function-monadic left-function-dyadic)
  "Generate a function applying a function at indices in an array specified by a given index or meeting certain conditions. Used to implement [@ at]."
  (let* ((index (gensym)) (omega-var (gensym)) (output (gensym)) (item (gensym))
	 (coord (gensym)) (coords (gensym)) (result (gensym)) (alen (gensym))
	 (alpha (gensym)) (omega (gensym)))
    (cond (right-function-monadic
	   `(lambda (,omega &optional ,alpha)
	      (declare (ignorable ,alpha))
	      (each-scalar (lambda (,item ,coords)
			     (declare (ignore ,coords))
			     (let ((,result (disclose (apl-call ,right-symbol ,right-function-monadic ,item))))
			       (if (= 1 ,result)
				   (disclose ,(cond ((or left-function-monadic left-function-dyadic)
						     `(if ,alpha (apl-call ,left-symbol ,left-function-dyadic
									   ,item ,alpha)
							  (apl-call ,left-symbol ,left-function-monadic ,item)))
						    (t left-value)))
				   (if (= 0 ,result)
				       ,item (error ,(concatenate
						      'string "Domain error: A right function operand"
						      " of @ must only return 1 or 0 values."))))))
			   ,omega)))
	  (t `(lambda (,omega)
		(let* ((,omega-var (apply-scalar #'- ,right-value index-origin))
		       (,output (make-array (dims ,omega)))
		       (,coord))
		  ;; make copy of array without type constraint; TODO: is there a more
		  ;; efficient way to do this?
		  (across ,omega (lambda (,item ,coords)
				   (declare (dynamic-extent ,item ,coords))
				   (setf (apply #'aref (cons ,output ,coords))
					 ,item)))
		  (loop :for ,index :below (length ,omega-var)
		     :do (setq ,coord (aref ,omega-var ,index))
		       (choose ,output (append (if (arrayp ,coord)
						   (mapcar #'list (array-to-list ,coord))
						   (list (list ,coord)))
					       ;; pad choose value with nils to elide
					       (loop :for i :below (1- (rank ,output)) :collect nil))
			       :set ,@(cond (left-function-monadic (list left-function-monadic))
					    (t `((if (is-unitary ,left-value)
						     (disclose ,left-value)
						     (lambda (,item ,coords)
						       (declare (ignore ,item))
						       (let ((,alen (if (not (listp ,coord))
									1 (length ,coord))))
							 (choose ,left-value
								 (mapcar #'list
									 (append (list ,index)
										 (nthcdr ,alen ,coords)))))))
						 :set-coords t)))))
		  ,output))))))

(defmacro apply-stenciled (right-value left-symbol left-function-dyadic)
  "Generate a function applying a function via (aplesque:stencil) to an array. Used to implement [⌺ stencil]."
  (let* ((omega (gensym)) (window-dims (gensym)) (movement (gensym)) (o (gensym)) (a (gensym))
	 (op-left `(lambda (,o ,a) (apl-call ,left-symbol ,left-function-dyadic ,o ,a)))
	 (iaxes (gensym)))
    `(lambda (,omega)
       (flet ((,iaxes (value index) (loop :for x :below (rank value) :for i :from 0
				       :collect (if (= i 0) index nil))))
	 (cond ((< 2 (rank ,right-value))
		(error "The right operand of ⌺ may not have more than 2 dimensions."))
	       ((not ,left-function-dyadic)
		(error "The left operand of ⌺ must be a function."))
	       (t (let ((,window-dims (if (not (arrayp ,right-value))
					  (vector ,right-value)
					  (if (= 1 (rank ,right-value))
					      ,right-value (choose ,right-value (,iaxes ,right-value 0)))))
			(,movement (if (not (arrayp ,right-value))
				       (vector 1)
				       (if (= 2 (rank ,right-value))
					   (choose ,right-value (,iaxes ,right-value 1))
					   (make-array (length ,right-value)
						       :element-type 'fixnum
						       :initial-element 1)))))
		    (merge-arrays (stencil ,omega ,op-left ,window-dims ,movement)))))))))
