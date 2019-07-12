;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Aplesque -*-
;;;; aplesque.lisp

(in-package #:aplesque)

(defun is-unitary (value)
  "Check whether this array has only one member, returning true if the argument is not an array."
  (or (not (arrayp value))
      (loop :for dim :in (dims value) :always (= 1 dim))))

(defun enclose (item)
  "Enclose non-array values, passing through arguments that are already arrays."
  (if (arrayp item)
      item (make-array '(1) :element-type (assign-element-type item)
		       :initial-element item)))

(defun disclose (item &key if-array)
  "If the argument is an array with only one member, disclose it, otherwise do nothing."
  (if (and (arrayp item)
	   (is-unitary item)
	   (or (not if-array)
	       (arrayp (row-major-aref item 0))))
      (row-major-aref item 0)
      item))

(defun disclose-unitary-array (item)
  "Disclose an array if it's unitary, otherwise pass it back unchanged."
  (if (and (arrayp item)
	   (is-unitary item)
	   (arrayp (row-major-aref item 0)))
      (row-major-aref item 0)
      item))

(defun scale-array (unitary to-match &optional axis)
  "Scale up a 1-element array to fill the dimensions of the given array."
  (let ((match-dims (dims to-match)))
    (make-array (if axis (loop :for this-dim :in match-dims :counting this-dim :into tdix
			    :collect (if (= tdix (1+ axis))
					 1 this-dim))
		    match-dims)
		:element-type (element-type unitary)
		:initial-element (aref unitary 0))))

(defun array-promote (input)
  "Promote an array to the next rank. The existing content will occupy 1 unit of the new dimension."
  (let ((output (make-array (cons 1 (dims input))
			    :element-type (element-type input))))
    (across input (lambda (elem coords)
		    (setf (apply #'aref output 0 coords)
			  elem)))
    output))

(defun array-to-list (input)
  "Convert array to list."
  (if (not (arrayp input))
      (list input)
      (let* ((dimensions (dims input))
	     (depth (1- (length dimensions)))
	     (indices (make-list (1+ depth) :initial-element 0)))
	(labels ((recurse (n)
		   (loop :for j :below (nth n dimensions)
		      :do (setf (nth n indices) j)
		      :collect (if (= n depth)
				   ;; (let ((item (apply #'aref input indices)))
				   ;;   (if (arrayp item)
				   ;; 	 (array-to-list item)
				   ;; 	 item))
				   (apply #'aref input indices)
				   (recurse (1+ n))))))
	  (recurse 0)))))

(defmacro apl-default-element (array)
  "Returns the default element for an array based on that array's type; blank spaces for character arrays and zeroes for others."
  `(if (or (eql 'character (element-type ,array))
	   (eql 'base-char (element-type ,array)))
       #\  0))

(defun assign-element-type (item)
  "Find a type suitable for an APL array to hold a given item."
  (typecase item
    (bit 'bit)
    (character 'character)
    (integer (list 'integer (min 0 item)
		   (max 0 item)))
    (single-float 'single-float)
    (double-float 'double-float)
    (t t)))

(defun type-in-common (&rest types)
  "Find a type for an array that may hold elements from arrays of a set of given types; effectively the most efficient compatible type among the array types."
  (let ((type))
    (loop :for a :in types
       :do (let ((this-type a))
	     (setq type (if type (if (listp type)
				     (cond ((eql 'bit this-type)
					    type)
					   ((eql 'fixnum this-type)
					    'fixnum)
					   ((eql 'bignum this-type)
					    'bignum)
					   ((not (listp this-type))
					    t)
					   ((eql 'integer (first type))
					    (cond ((eql 'integer (first this-type))
						   (list 'integer (min 0 (second this-type)
								       (second type))
							 (max 0 (third this-type)
							      (third type))))
						  (t t)))
					   ((eql 'unsigned-byte (first type))
					    (cond ((eql 'unsigned-byte (first this-type))
						   (list 'unsigned-byte (max (second type)
									     (second this-type))))
						  ((eql 'signed-byte (first this-type))
						   'fixnum)
						  (t t)))
					   ((eql 'signed-byte (first type))
					    (cond ((eql 'signed-byte (first this-type))
						   (list 'signed-byte (max (second type)
									   (second this-type))))
						  ((eql 'unsigned-byte (first this-type))
						   'fixnum)
						  (t t)))
					   (t t))
				     (cond ((eql type this-type)
					    type)
					   ((eql 'bit type)
					    (cond ((eql 'fixnum this-type)
						   'fixnum)
						  ((eql 'bignum this-type)
						   'bignum)
						  ((listp this-type)
						   (if (eql 'integer (first this-type))
						       (list (first this-type)
							     (second this-type)
							     (max 1 (third this-type)))
						       this-type))
						  (t t)))
					   ((eql 'fixnum type)
					    (cond ((or (eql 'bit this-type)
						       (listp this-type))
						   'fixnum)
						  ((eql 'bignum this-type)
						   'bignum)
						  (t t)))
					   ((eql 'bignum type)
					    (cond ((or (eql 'bit this-type)
						       (eql 'fixnum this-type)
						       (listp this-type))
						   'bignum)
						  (t t)))
					   ((eql 'single-float type)
					    (cond ((eql 'double-float this-type)
						   'double-float)
						  (t t)))
					   ((eql 'double-float type)
					    (cond ((eql 'single-float this-type)
						   'double-float)
						  (t t)))
					   (t t)))
			    this-type))))
    type))

(defun each-boolean (test omega)
  "Iterate over an array/arrays of scalar values, performing operations upon them that will result in boolean values to be returned in an array with the same shape as the input array(s)."
  (let ((output (make-array (dims omega) :element-type 'bit :initial-element 0)))
    (across omega (lambda (elem coords)
		    ;; (print (list :fn (funcall test elem coords) output))
		    (if (= 1 (funcall test elem coords))
			(setf (apply #'aref output coords)
			      1))))
    output))

(defun each-scalar (function omega)
  "Iterate over an array/arrays of scalar values, operating upon them and returning the output in the most efficiently-stored array capable of holding said output."
  (let ((type)
	(output (make-array (dims omega))))
    (if (not (arrayp omega))
	omega (progn (across omega (lambda (elem coords)
				     (let ((result (if (eq t function)
						       elem (funcall function elem coords))))
				       (if type (if (not (typep result type))
						    (setq type (type-in-common type (assign-element-type result))))
					   (setq type (assign-element-type result)))
				       (setf (apply #'aref output coords)
					     result)
				       ;; output second value to halt traversal of output array;
				       ;; if the type will be t and no change is being made to the array values,
				       ;; and thus the function value is t as well, end the traversal
				       ;; since there's no longer a point
				       (values nil (and (eq t type)
							(eq t function))))))
		     (if (eq t type)
			 (if (eq t function)
			     ;; if the traversal was terminated and thus the proper output is the original input,
			     ;; return that instead of the output
			     omega output)
			 (let ((true-output (make-array (dims omega) :element-type type)))
			   (across output (lambda (elem coords)
					    (setf (apply #'aref true-output coords)
						  elem)))
			   true-output))))))

(defun array-compare (item1 item2)
  "Perform a deep comparison of two APL arrays, which may be multidimensional or nested."
  (if (and (not (arrayp item1))
	   (not (arrayp item2)))
      (or (and (numberp item1)
	       (numberp item2)
	       (= item1 item2))
	  (and (characterp item1)
	       (characterp item2)
	       (char= item1 item2)))
      (if (and (= (rank item1) (rank item2))
	       (let ((dims1 (dims item1))
		     (dims2 (dims item2)))
		 (loop :for d :below (length dims1)
		    :always (= (nth d dims1) (nth d dims2)))))
	  (let ((match t))
	    (across item1 (lambda (item coords)
			    (let ((alternate (apply #'aref item2 coords)))
			      (setq match (and match (or (and (arrayp item) (arrayp alternate)
							      (array-compare item alternate))
							 (and (numberp item) (numberp alternate)
							      (= item alternate))
							 (and (characterp item) (characterp alternate)
							      (char= item alternate))))))))
	    match))))

(defun array-depth (input &optional layer (uniform t) possible-depth)
  "Find the maximum depth of nested arrays within an array."
  (let* ((first-level (not layer))
	 (layer (if layer layer 1))
	 (new-layer layer))
    (aops:each (lambda (item)
		 (if (arrayp item)
		     (multiple-value-bind (next-layer new-uniform new-possible-depth)
			 (array-depth item (1+ layer) uniform possible-depth)
		       (setq new-layer (max new-layer next-layer)
			     uniform new-uniform
			     possible-depth new-possible-depth))
		     (if (not possible-depth)
			 (setq possible-depth new-layer)
			 (if (/= layer possible-depth)
			     (setq uniform nil)))))
	       input)
    (values (funcall (if (and first-level (not uniform))
			 #'- #'identity)
		     new-layer)
	    uniform possible-depth)))

(defun section (input dimensions &key (inverse nil) (fill-with nil))
  "Take a subsection of an array of the same rank and given dimensions as per APL's ↑ function, or invert the function as per APL's ↓ function to take the elements of an array excepting a specific dimensional range."
  (if (and inverse (< 0 (reduce #'+ (mapcar (lambda (i) (- (min 0 i)))
					    (mapcar #'- (dims input)
						    dimensions)))))
      #0A0
      (let* ((idims (dims input))
	     (input-smaller (< (array-total-size input)
			       (abs (reduce #'* dimensions))))
	     ;; if the input array is smaller than the output will be, iterate over the cells of input and copy to
	     ;; the appropriate output cells; otherwise do the inverse, iterating over output and copying from the
	     ;; corresponding input cells
	     (output (make-array (mapcar (lambda (outdim indim)
					   (if (not inverse)
					       (abs outdim) (- indim (abs outdim))))
					 dimensions idims)
				 :initial-element (if fill-with fill-with (apl-default-element input))
				 :element-type (element-type input))))
	(across (if input-smaller input output)
		(lambda (element coords)
		  (declare (ignore element))
		  (let* ((coord t)
			 (target (loop :for c :below (length coords) :while coord
				    :collect (let ((cx (nth c coords))
						   (ix (nth c idims))
						   (ox (nth c dimensions)))
					       (declare (dynamic-extent cx ix ox))
					       (setq coord (cond ((and inverse (> 0 ox))
								  (if (< cx (+ ox ix))
								      cx))
								 (inverse (if (> ix (+ cx ox))
									      (+ cx ox)))
								 ((> 0 ox)
								  (if input-smaller
								      (if (>= cx (+ ox ix))
									  (- cx (+ ox ix)))
								      (if (<= cx (+ ox ix))
									  (+ cx ix ox))))
								 (t (if (and (< cx ox)
									     (< cx ix))
									cx))))))))
		    (declare (dynamic-extent coord target))
		    (if coord (setf (apply #'aref output (if (or inverse (not input-smaller))
							     coords target))
				    (apply #'aref input (if (or inverse (not input-smaller))
							    target coords)))))))
	(disclose output))))

(defun scan-back (function input &optional output)
  "Scan a function backwards across an array."
  (if (not input)
      output (if output (scan-back function (rest input)
				   (disclose (funcall function (first input) output)))
		 (scan-back function (cddr input)
 			    (disclose (funcall function (second input) (first input)))))))

(defun make-back-scanner (function)
  "Build a function to scan across an array, modifying each value as determined by prior values."
  (lambda (sub-array)
    (let ((args (list (aref sub-array 0))))
      (loop :for index :from 1 :to (1- (length sub-array))
	 :do (setf args (cons (aref sub-array index)
			      args)
		   (aref sub-array index)
		   (scan-back function args)))
      sub-array)))

(defun catenate (a1 a2 axis)
  "Join two arrays together along the specified axis."
  (flet ((upgrade (array)
	   (make-array (append (if (= 0 axis) '(1))
			       (loop :for dim :in (dims array) :counting dim :into dx
				  :append (cons dim (if (= dx axis) '(1)))))
		       :element-type (element-type array)
		       :displaced-to array))
	 (scalar-to-array (item to-match &optional axis)
	   (let ((match-dims (dims to-match)))
	     (make-array (if axis (loop :for this-dim :in match-dims :counting this-dim :into tdix
				     :collect (if (= tdix (1+ axis))
						  1 this-dim))
			     match-dims)
			 :element-type (type-of item)
			 :initial-element item)))
	 (join (array1 array2)
	   (let ((output (make-array (loop :for dim :in (dims array1) :counting dim :into dx
					:collect (if (/= axis (1- dx))
						     dim (+ dim (nth (1- dx) (dims array2)))))
				     :element-type (type-in-common (element-type array1)
								   (element-type array2)))))
	     (across array1 (lambda (elem coords)
			      (setf (apply #'aref output coords)
				    elem)))
	     (across array2 (lambda (elem coords)
			      (setf (apply #'aref output
					   (loop :for coord :in coords :counting coord :into cx
					      :collect (if (/= axis (1- cx))
							   coord (+ coord (nth axis (dims array1))))))
				    elem)))
	     output)))
    (if (and (not (arrayp a1))
	     (not (arrayp a2)))
	(make-array '(2) :element-type (type-in-common (assign-element-type a1)
						       (assign-element-type a2))
		    :initial-contents (list a1 a2))
	(let* ((a1 (if (arrayp a1)
		       a1 (scalar-to-array a1 a2 axis)))
	       (a2 (if (arrayp a2)
		       a2 (scalar-to-array a2 a1 axis))))
	  (if (= (rank a1) (rank a2))
	      (join a1 a2)
	      (let* ((lesser-first (< (rank a1) (rank a2)))
		     (lesser (if lesser-first a1 a2))
		     (greater (if lesser-first a2 a1))
		     (gdims (dims greater))
		     (grank (rank greater))
		     (ldims (dims lesser))
		     (lrank (rank lesser)))
		(declare (dynamic-extent lesser-first lesser greater gdims grank ldims lrank))
		(if (and (= 1 (- grank lrank))
			 (if (= 1 lrank)
			     (= (first ldims) (nth (- 1 axis) gdims))
			     (let ((compare-dims (loop :for gdim :in gdims :counting gdim :into gdix
						    :when (/= gdix (1+ axis)) :collect gdim)))
			       (loop :for ix :below lrank :always (or (= ix axis)
								      (= (nth ix ldims)
									 (nth ix compare-dims)))))))
		    (join (if lesser-first (upgrade a1) a1)
			  (if lesser-first a2 (upgrade a2)))
		    (error "Incompatible arrays."))))))))

(defun laminate (a1 a2 axis)
  "Join the two arrays along a new axis inserted before the specified axis, the new axis having a length of 2."
  (flet ((rotate-right (n l)
	   "Rotate an array n units to the right."
	   (funcall (lambda (n l) (append (nthcdr n l) (butlast l (- (length l) n))))
		    (- (length l) n) l)))
    (let* ((permute-dims (alexandria:iota (1+ (rank a1))))
	   (pa1 (if (not (is-unitary a1))
		    (aops:permute (rotate-right axis permute-dims)
				  (array-promote a1))))
	   (pa2 (if (not (is-unitary a2))
		    (aops:permute (rotate-right axis permute-dims)
				  (array-promote a2)))))
      (declare (dynamic-extent permute-dims pa1 pa2))
      ;; a 1-element array argument to laminate is scaled to
      ;; match the other array's dimensions
      (catenate (if (is-unitary a1)
		    (scale-array a1 pa2)
		    pa1)
		(if (is-unitary a2)
		    (scale-array a2 pa1)
		    pa2)
		axis))))

(defun expand-array (degrees input axis &key (compress-mode nil))
  "Expand an input array as per a vector of degrees, with the option to manifest zero values in the degree array as zeroes in the output in place of the original input values or to omit the corresponding values altogether if the :compress-mode option is used."
  (cond ((and (not (arrayp input))
	      (not (arrayp degrees)))
	 (make-array (list degrees) :element-type (type-of input) :initial-element input))
	((and compress-mode (not (is-unitary input))
	      (and (arrayp degrees)
		   (/= (length degrees)
		       (nth axis (dims input)))))
	 (error (concatenate 'string "Attempting to replicate elements across array but "
			     "degrees are not equal to length of selected input axis.")))
	((and (not compress-mode)
	      (and (arrayp input)
		   (< 1 (array-total-size input)))
	      (/= (loop :for degree :across degrees :when (< 0 degree)
		     :counting degree :into dcount :finally (return dcount))
		  (nth axis (dims input))))
	 (error (concatenate 'string "Attempting to expand elements across array but "
			     "positive degrees are not equal to length of selected input axis.")))
	(t (let* ((degrees (if (arrayp degrees)
			       degrees (vector degrees)))
		  (input (if (arrayp input)
			     input (vector input)))
		  ;; TODO: is there a more elegant way to handle scalar degrees or input when both aren't scalar?
		  (c-degrees (make-array (list (length degrees))
					 :element-type 'fixnum :initial-element 0))
		  (positive-index-list (if (not compress-mode)
					   (loop :for degree :below (length degrees)
					      :when (< 0 (aref degrees degree)) :collect degree)))
		  (positive-indices (if positive-index-list (make-array (list (length positive-index-list))
									:element-type 'fixnum
									:initial-contents positive-index-list)))
		  (ex-dim))
	     (declare (dynamic-extent c-degrees positive-index-list positive-indices ex-dim))
	     (loop :for degree :across degrees :counting degree :into dx
		:summing (max (abs degree)
			      (if compress-mode 0 1))
		:into this-dim :do (setf (aref c-degrees (1- dx)) this-dim)
		:finally (setq ex-dim this-dim))
	     (let ((output (make-array (if (= 1 (length degrees))
					   (list (* (aref degrees 0)
						    (nth axis (dims input))))
					   (loop :for dim :in (dims input) :counting dim :into index
					      :collect (if (not (or (= index (1+ axis))
								    (is-unitary input)))
							   dim ex-dim)))
				       :element-type (element-type input)
				       :initial-element (apl-default-element input))))
	       ;; in compress-mode: degrees must = length of axis,
	       ;; zeroes are omitted from output, negatives add zeroes
	       ;; otherwise: zeroes pass through, negatives add zeroes, degrees>0 must = length of axis
	       (if (is-unitary input)
		   ;; if the input is a unitary value, just expand or replicate with that value
		   (let ((value (row-major-aref input 0)))
		     (loop :for degree :below (length degrees)
			:do (let ((this-degree (aref degrees degree)))
			      (loop :for ix :below this-degree
				 :do (setf (aref output (+ ix (if (= 0 degree)
								  0 (aref c-degrees (1- degree)))))
					   value)))))
		   (across input
			   (lambda (elem coords)
			     (let* ((exc (nth axis coords))
				    (dx (if compress-mode exc (aref positive-indices exc)))
				    (this-degree (if (= 1 (length degrees))
						     (aref degrees 0) (aref degrees dx))))
			       (declare (dynamic-extent exc dx this-degree))
			       (loop :for ix :below this-degree
				  :do (setf (apply #'aref output
						   (loop :for coord :in coords :counting coord :into cix
						      :collect
						      (if (not (= cix (1+ axis)))
							  coord (+ ix (if (= 0 exc)
									  0 (if (= 1 (length degrees))
										(* exc (aref c-degrees 0))
										(aref c-degrees
										      (1- dx))))))))
					    (if (> 0 this-degree)
						0 elem)))))))
	       output)))))

(defun partitioned-enclose (positions input axis)
  "Enclose parts of an input array partitioned according to the 'positions' argument."
  (let* ((indices) (intervals) (interval-size 0)
	 (input (enclose input))
	 (positions (enclose positions)))
    (declare (dynamic-extent indices intervals interval-size))
    (loop :for p :below (length positions)
       :do (incf interval-size)
       :when (= 1 (aref positions p))
       :do (setq indices (cons p indices)
		 intervals (if (rest indices)
			       (cons interval-size intervals)
			       intervals)
		 interval-size 0))
    (setq intervals (reverse (cons (- (length positions) (first indices))
				   intervals))
	  indices (reverse indices))
    (let* ((idims (dims input))
	   (output (make-array (list (length indices))
			       :initial-contents (loop :for intv :in intervals
						    :collect (make-array (loop :for dim :in idims
									    :counting dim :into dx
									    :collect (if (= dx (1+ axis))
											 intv dim))
									 :element-type (element-type input))))))
      (declare (dynamic-extent idims))
      (loop :for out :across output :for oix :below (length output)
	 :do (across out (lambda (elem coords)
			   (declare (ignore elem))
			   (setf (apply #'aref out coords)
				 (apply #'aref input (loop :for c :in coords :counting c :into cix
							:collect (if (= cix (1+ axis))
								     (+ c (nth oix indices))
								     c)))))))
      output)))

(defun partition-array (positions input axis)
  "Split an array into an array of vectors divided according to an array of positions."
  (if (not (arrayp positions))
      (if (< 1 (array-total-size input))
	  (vector input)
	  (error "Rank error."))
      (let ((r-indices) (r-intervals) (indices) (intervals)
	    (interval-size 0)
	    (current-interval -1)
	    (partitions 0)
	    (idims (dims input))
	    (arank (rank input)))
	(declare (dynamic-extent r-indices r-intervals indices intervals interval-size
				 current-interval partitions idims arank))
	;; find the index where each partition begins in the input array and the length of each partition
	(loop :for pos :across positions :for p :below (length positions)
	   :do (if (/= 0 current-interval)
		   (incf interval-size))
	   :when (or (< current-interval pos)
		     (and (= 0 pos) (/= 0 current-interval)))
	   :do (setq r-indices (cons p r-indices)
		     r-intervals (if (rest r-indices) (cons interval-size r-intervals)))
	   (incf partitions (if (/= 0 pos) 1 0))
	   (setq current-interval pos interval-size 0))
	;; add the last entry to the intervals provided the positions list didn't have a 0 value at the end
	(if (/= 0 (aref positions (1- (length positions))))
	    (setq r-intervals (cons (- (length positions) (first r-indices))
				    r-intervals)))
	;; collect the indices and intervals into lists the right way around, dropping indices with 0-length
	;; intervals corresponding to zeroes in the positions list
	(loop :for rint :in r-intervals :for rind :in r-indices :when (/= 0 rint)
	   :do (setq intervals (cons rint intervals)
		     indices (cons rind indices)))
	(let ((output (make-array (loop :for dim :in idims :for dx :below arank
				     :collect (if (= dx axis) partitions dim)))))
	  (across output (lambda (elem coords)
			   (declare (ignore elem))
			   (let* ((focus (nth axis coords))
				  (this-index (nth focus indices))
				  (this-interval (nth focus intervals)))
			     (declare (dynamic-extent focus this-index this-interval))
			     (setf (apply #'aref output coords)
				   (make-array (list this-interval)
					       :element-type (element-type input)
					       :initial-contents
					       (loop :for ix :below this-interval
						  :collect (apply #'aref input
								  (loop :for coord :in coords
								     :for dx :below arank
								     :collect (if (= dx axis)
										  (+ ix this-index)
										  coord)))))))))
	  output))))

(defun enlist (input &optional internal output (output-length 0))
  "Create a vector containing all elements of the input array in ravel order, breaking down nested and multidimensional arrays."
  (if (or (not (arrayp input))
	  (= 1 (array-total-size input)))
      input (let ((raveled (make-array (list (array-total-size input))
				       :element-type (element-type input)
				       :displaced-to input)))
	      (loop :for item :across raveled :do (if (arrayp item)
						      (multiple-value-bind (out new-length)
							  (enlist item t output output-length)
							(setq output out output-length new-length))
						      (setq output (cons item output)
							    output-length (1+ output-length))))
	      (if internal (values output output-length)
		  (make-array (list output-length) :element-type (element-type input)
			      :initial-contents (reverse output))))))

(defun reshape-to-fit (input output-dims)
  "Reshape an array into a given set of dimensions, truncating or repeating the elements in the array until the dimensions are satisfied if the new array's size is different from the old."
  (if (not (arrayp input))
      (make-array output-dims :element-type (assign-element-type input) :initial-element input)
      (let* ((input-length (array-total-size input))
	     (output-length (reduce #'* output-dims))
	     (input-index 0)
	     (input-displaced (if (vectorp input)
				  input (make-array (list input-length)
						    :displaced-to input :element-type (element-type input))))
	     (output (make-array output-dims :element-type (element-type input)))
	     (output-displaced (if (not (rest output-dims))
				   output (make-array (list output-length)
						      :displaced-to output :element-type (element-type input)))))
	(declare (dynamic-extent input-length output-length input-index input-displaced output-displaced)
		 (optimize (safety 0) (speed 3)))
	(loop :for index :below output-length
	   :do (setf (aref output-displaced index)
		     (aref input-displaced input-index)
		     input-index (if (= input-index (1- input-length))
				     0 (1+ input-index))))
	output)))

(defun sprfact (n)
  "Recursive factorial-computing function. Based on P. Luschny's code."
  (let ((p 1) (r 1) (NN 1) (log2n (floor (log n 2)))
	(h 0) (shift 0) (high 1) (len 0))
    (declare (dynamic-extent p r NN log2n h shift high len))
    (labels ((prod (n)
	       (declare (fixnum n))
	       (let ((m (ash n -1)))
		 (cond ((= m 0) (incf NN 2))
		       ((= n 2) (* (incf NN 2)
				   (incf NN 2)))
		       (t (* (prod (- n m))
			     (prod m)))))))
      (loop :while (/= h n)
	 :do (incf shift h)
	 (setf h (ash n (- log2n)))
	 (decf log2n)
	 (setf len high)
	 (setf high (if (oddp h)
			h (1- h)))
	 (setf len (ash (- high len) -1))
	 (cond ((> len 0)
		(setf p (* p (prod len)))
		(setf r (* r p)))))
      (ash r shift))))

(defun binomial (n k)
  "Find a binomial using the above sprfact function."
  (labels ((prod-enum (s e) (do ((i s (1+ i)) (r 1 (* i r))) ((> i e) r)))
	   (sprfact (n) (prod-enum 1 n)))
    (/ (prod-enum (- (1+ n) k) n) (sprfact k))))

(defun array-inner-product (operand1 operand2 function1 function2)
  "Find the inner product of two arrays with two functions."
  (funcall (lambda (result)
	     ;; disclose the result if the right argument was a vector and there is
	     ;; a superfluous second dimension
	     (if (vectorp operand1)
		 (aref (aops:split result 1) 0)
		 (if (vectorp operand2)
		     (let ((nested-result (aops:split result 1)))
		       (make-array (list (length nested-result))
				   :initial-contents (loop :for nrelem :across nested-result
							:collect (aref nrelem 0))))
		     result)))
	   (aops:each (lambda (sub-vector)
			(disclose (if (vectorp sub-vector)
				      (reduce function2 sub-vector)
				      (funcall function2 sub-vector))))
		      (aops:outer function1 (if (vectorp operand1)
						;; enclose the argument if it is a vector
						(vector operand1)
						(aops:split (aops:permute (iota (rank operand1))
									  operand1)
							    1))
				  (if (vectorp operand2)
				      (vector operand2)
				      (aops:split (aops:permute (reverse (iota (rank operand2)))
								operand2)
						  1))))))

(defun index-of (to-search set count-from)
  "Find occurrences of members of one set in an array and create a corresponding array with values equal to the indices of the found values in the search set, or one plus the maximum possible found item index if the item is not found in the search set."
  (if (not (or (numberp set) (characterp set) (vectorp set)))
      (error "Rank error.")
      (if (and (not (arrayp set))
	       (not (arrayp to-search)))
	  (if (equalp set to-search)
	      1 2)
	  (let* ((to-find (if (vectorp set)
			      (remove-duplicates set :from-end t)
			      (vector set)))
		 (to-search (if (arrayp to-search)
				to-search (vector to-search)))
		 (maximum (+ count-from (length to-find)))
		 (results (make-array (dims to-search) :element-type 'number)))
	    (dotimes (index (array-total-size results))
	      (let* ((search-index (row-major-aref to-search index))
		     (found (position search-index to-find)))
		(setf (row-major-aref results index)
		      (if found (+ count-from found)
			  maximum))))
	    results))))

(defun alpha-compare (atomic-vector compare-by)
  "Compare the contents of a vector according to their positions in an array, as when comparing an array of letters by their positions in the alphabet."
  (lambda (item1 item2)
    (flet ((assign-char-value (char)
	     (let ((vector-pos (position char atomic-vector)))
	       (if vector-pos vector-pos (length atomic-vector)))))
      (if (numberp item1)
	  (or (characterp item2)
	      (if (= item1 item2)
		  :equal (funcall compare-by item1 item2)))
	  (if (characterp item1)
	      (if (characterp item2)
		  (if (char= item1 item2)
		      :equal (funcall compare-by (assign-char-value item1)
				      (assign-char-value item2)))))))))

(defun vector-grade (compare-by vector1 vector2 &optional index)
  "Compare two vectors by the values of each element, giving priority to elements proportional to their position in the array, as when comparing words by the alphabetical order of the letters."
  (let ((index (if index index 0)))
    (cond ((>= index (length vector1))
	   (not (>= index (length vector2))))
	  ((>= index (length vector2)) nil)
	  (t (let ((compared (funcall compare-by (aref vector1 index)
				      (aref vector2 index))))
	       (if (eq :equal compared)
		   (vector-grade compare-by vector1 vector2 (1+ index))
		   compared))))))

(defun grade (input count-from compare-by)
  "Grade an array, using vector grading if 1-dimensional or decomposing the array into vectors and comparing those if multidimensional."
  (if (is-unitary input)
      count-from (let* ((input (if (= 1 (rank input))
				   input (aops:split input 1)))
			(input-length (length input))
			(vector (make-array (list input-length)))
			(graded-array (make-array (list input-length)
						  :element-type (list 'integer 0 input-length)
						  :initial-contents (mapcar (lambda (item) (+ item count-from))
									    (alexandria:iota input-length)))))
		   (loop :for vix :below (length vector)
		      :do (setf (aref vector vix)
				(if (and (arrayp (aref input vix))
					 (< 1 (rank (aref input vix))))
				    (grade (aref input vix)
					   count-from compare-by)
				    (aref input vix))))
		   (stable-sort graded-array (lambda (1st 2nd)
					       (let ((val1 (aref vector (- 1st count-from)))
						     (val2 (aref vector (- 2nd count-from))))
						 (cond ((not (arrayp val1))
							(if (arrayp val2)
							    (funcall compare-by val1 (aref val2 0))
							    (let ((output (funcall compare-by val1 val2)))
							      (and output (not (eq :equal output))))))
						       ((not (arrayp val2))
							(funcall compare-by (aref val1 0) val2))
						       (t (vector-grade compare-by val1 val2))))))
		   graded-array)))

(defun array-grade (compare-by input)
  "Grade an array."
  (aops:each (lambda (item)
	       (let ((coords))
		 (across compare-by (lambda (found indices)
				      (if (char= found item)
					  (setq coords indices))))
		 (make-array (list (length coords))
			     :initial-contents (reverse coords))))
	     input))

(defun interval-index (atomic-vector)
  "Return a function to find the locations of indices of an array between the indices of a reference array."
  (lambda (items reference)
    (labels ((interval-compare (ref)
	       (lambda (oitem)
		 (let ((match 0))
		   (aops:each (lambda (item)
				(if (funcall (alpha-compare atomic-vector #'>)
					     oitem item)
				    (incf match)))
			      ref)
		   match))))
      (each-scalar
       t
       (if (vectorp reference)
	   (disclose (aops:each (interval-compare reference) (enclose items)))
	   (if (and (<= (rank reference) (rank items))
		    (reduce #'eq (mapcar #'= (reverse (rest (dims reference)))
					 (reverse (dims items)))))
	       (let* ((items (enclose items))
		      (ref-cells (re-enclose reference (make-array (list (1- (rank reference)))
								   :initial-contents (rest (iota (rank reference)
												 :start 0)))))
		      (sub-dims (- (rank items) (1- (rank reference))))
		      (sub-arrays (re-enclose items (make-array (list (- (rank items) sub-dims))
								:initial-contents
								(nthcdr sub-dims (iota (rank items) :start 0))))))
		 (aops:each (lambda (sub-array)
			      (let ((match 0))
				(aops:each (lambda (ref)
					     (let ((ref (if (vectorp ref)
							    ref (make-array (list (array-total-size ref))
									    :element-type (element-type ref)
									    :displaced-to ref)))
						   (sub-array (if (vectorp sub-array)
								  sub-array
								  (make-array (list (array-total-size sub-array))
									      :element-type (element-type sub-array)
									      :displaced-to sub-array))))
					       (if (vector-grade (alpha-compare atomic-vector #'<)
								 ref sub-array)
						   (incf match))))
					   ref-cells)
				match))
			    sub-arrays))
	       (error "Rank of left argument may not be greater than rank of right argument.")))))))

(defun find-array (input target)
  "Find instances of an array within a larger array."
  (let* ((source-dims (dims input))
	 (output (make-array source-dims :element-type 'bit :initial-element 0)))
    (if (not (arrayp target))
	(if (not (arrayp input))
	    (if (characterp input)
		(if (characterp target)
		    (if (char= target input)
			1 0)
		    0)
		(if (= target input)
		    1 0))
	    (progn (across input (lambda (element coords)
				   (setf (apply #'aref output coords)
					 (if (funcall (if (eql 'character (element-type input))
							  #'char= #'=)
						      target element)
					     1 0))))
		   output))
	(let ((target-head (row-major-aref target 0))
	      (target-dims (append (if (< (rank target) (rank input))
				       (loop :for index :below (- (rank input) (rank target))
					  :collect 1))
				   (dims target))))
	  (loop :for match :in (let ((match-coords))
				 (across input (lambda (element coords)
						 (if (equal element target-head)
						     (setq match-coords (cons coords match-coords)))))
				 match-coords)
	     :do (let ((target-index 0)
		       (target-matched t)
		       (target-displaced (make-array (list (array-total-size target))
						     :element-type (element-type target)
						     :displaced-to target)))
		   (across input (lambda (element coords)
				   (declare (ignore coords))
				   (if (and (< target-index (length target-displaced))
					    (not (equal element (aref target-displaced target-index))))
				       (setq target-matched nil))
				   (incf target-index))
			   :elements (mapcar (lambda (start extent limit)
					       (iota (min extent (- limit start))
						     :start start))
					     match target-dims source-dims))
		   ;; check the target index in case the elements in the searched array ran out
		   (if (and target-matched (= target-index (length target-displaced)))
		       (incf (apply #'aref output match)))))
	  output))))

(defun across (input function &key elements indices reverse-axes (dimensions (dims input)))
  "Iterate across a range of elements in an array, with the option of specifying which elements within each dimension to process."
  (let* ((proceeding t)
	 (first-of-elements (first elements))
	 (this-range (if (listp first-of-elements)
			 first-of-elements (list first-of-elements))))
    (loop :for elix :in (funcall (if (and reverse-axes (member (or (length indices) 0) reverse-axes))
				     #'reverse #'identity)
				 (if this-range this-range (iota (nth (length indices) dimensions))))
       :while proceeding
       :do (let ((coords (append indices (list elix))))
	     ;; if the halt-if-true value is output by the function, traversal across the array will end
	     ;; by means of nullifying the proceeding variable; this will result in a nil return value
	     ;; from the across function which will stop its recursive parents
	     (if (< (length indices) (1- (length dimensions)))
		 (if (not (across input function :dimensions dimensions :elements (rest elements)
				  :indices coords :reverse-axes reverse-axes))
		     (setq proceeding nil))
		 (multiple-value-bind (output halt-if-true)
		     (funcall function (apply #'aref input coords)
			      coords)
		   (declare (ignore output))
		   (if halt-if-true (setq proceeding nil))))))
    proceeding))

(defun choose (input aindices &key (fn #'identity) (set nil) (set-coords nil))
  "Retrieve and/or change elements of an array allowing elision, returning a new array whose shape is determined by the elision and number of indices selected unless indices for just one value are passed."
  (let* ((idims (dims input))
	 (output (let ((dims-out (if (or (and (listp (first aindices))
					      (arrayp (caar aindices)))
					 (and (arrayp (first aindices))
					      (arrayp (row-major-aref (first aindices) 0))))
				     (dims (first aindices))
				     ;; if the first if the first indices is an array, that means that choose
				     ;; indexing is being used, so the shape of the first index will be
				     ;; the shape of the output array. Otherwise, measure the shapes of the indices
				     ;; to determine the shape of the output array
				     (loop :for dim :in idims :counting dim :into dx
					:append (let ((index (nth (1- dx) aindices)))
				     		  (if (and (or (listp index) (vectorp index))
				     			   (< 1 (length index)))
				     		      (list (length index))
				     		      (if (and (arrayp index) (< 1 (array-total-size index)))
				     			  (dims index)
				     			  (if (not index)
				     			      (list dim)))))))))
		   (if (not dims-out)
		       :unitary (make-array dims-out :element-type t))))
	 (input-compatible (typep set (element-type input)))
	 (true-input (if (not input-compatible)
			 (make-array (dims input) :element-type (type-in-common (element-type input)
										(assign-element-type set))))))
    (if true-input (progn (across input (lambda (elem coords) (setf (apply #'aref true-input coords)
								    elem)))
			  (setq input true-input)))
    (labels ((process (indices &optional out-path in-path)
	       (symbol-macrolet
		   ((source-cell (apply #'aref input (reverse in-path)))
		    (target-cell (apply #'aref output (reverse out-path)))
		    (set-cell (apply #'aref set (reverse out-path)))
		    (apply-set-function (apply set (cons source-cell (if set-coords (list (reverse in-path)))))))
		 (cond ((and (not indices) (= (rank input) (length in-path)))
			;; if the output is not a unitary value, set the target cell appropriately
			(if (and out-path (not (eq :unitary output)))
			    (setf target-cell (cond ((arrayp set)
						     ;; if assigning a whole array, set the value of the source
						     ;; cell to the value of the corresponding cell in
						     ;; the array of values to be set
						     (setf source-cell set-cell))
						    (set (setf source-cell (disclose (if (functionp set)
											 apply-set-function set))))
						    (t (funcall fn source-cell))))
			    (setq output
				  (if set (setf source-cell (disclose (if (functionp set) apply-set-function set)))
				      source-cell))))
		       ((and (not indices) (vectorp (first in-path)) (vectorp (aref (first in-path) 0)))
		       	;; if using reach indexing, recurse on the sub-array specified
			;; by the first coordinate vector
		       	(setf target-cell
		       	      (disclose (choose (apply #'aref input (array-to-list (aref (first in-path) 0)))
						(loop :for ix :from 1 :to (1- (length (first in-path)))
						   :collect (aref (first in-path) ix))
						:set set :fn fn :set-coords set-coords))))
		       ((and (not indices) (vectorp (first in-path)))
		       	;; if using choose indexing, recurse using the index as the coordinates
		       	(setf target-cell (disclose (choose input (array-to-list (first in-path))
							    :set set :fn fn :set-coords set-coords))))
		       (t (let ((this-index (first indices)))
			    (cond ((and (not (eq :unitary output))
					(arrayp set)
					(or (not (= (rank output)
						    (rank set)))
					    (not (loop :for dx :below (rank output)
						    :always (= (nth dx (dims output))
							       (nth dx (dims set)))))))
				   ;; if assigning a whole array, make sure the input and output area
				   ;; have the same shape
				   (error (concatenate 'string "When assigning from an array, the shape of the"
						       " area to be assigned and the shape of the array to"
						       " assign from must be the same.")))
				  ((arrayp this-index)
				   ;; iterate over the index if it's an array, unless it's a unitary vector
				   (if (and (vectorp this-index)
					    (= 1 (length this-index)))
				       (process (rest indices)
						out-path (cons (aref this-index 0) in-path))
				       (across this-index (lambda (value coords)
							    (process (rest indices)
								     (append (reverse coords) out-path)
								     (cons value in-path))))))
				  ((not this-index)
				   ;; if there is no index, elide it by iterating over this dimension of the array
				   (let ((count 0))
				     (loop :for ix :below (nth (length in-path) idims)
					:do (process (rest indices) (cons count out-path)
						     (cons ix in-path))
					(incf count 1))))
				  ((listp this-index)
				   ;; iterate over the index if it's a list, same as with an array
				   (if (= 1 (length this-index))
				       (process (rest indices)
						out-path (cons (first this-index) in-path))
				       (let ((count 0))
					 (loop :for ix :in this-index :do (process (rest indices)
										   (cons count out-path)
										   (cons ix in-path))
					    (incf count 1)))))
				  (t (process (rest indices)
					      out-path (cons this-index in-path))))))))))
      (process aindices)
      (apply #'values (cons (each-scalar t output)
			    (if true-input (list (if (not (functionp set))
						     input (each-scalar t input)))))))))

(defun mix-arrays (axis input)
  "Combine an array of nested arrays into a higher-rank array, removing a layer of nesting."
  (if (or (not (arrayp input))
	  (and (= 1 (array-total-size input))
	       (not (arrayp (row-major-aref input 0)))))
      input
      (flet ((sort-dimensions (outer inner)
	       (let ((axis-index 0))
		 (loop :for odix :from 0 :to (length outer)
		    :append (append (if (and axis (= odix (- axis (min 1 axis-index))))
					(progn (incf axis-index)
					       (if axis inner (list (nth (1- axis-index) inner)))))
				    (if (< odix (length outer))
					(list (nth odix outer))))))))
	(let* ((max-rank 1)
	       (each-type)
	       (input-vector (make-array (list (array-total-size input))
					 :displaced-to input :element-type (element-type input)))
	       (each-dims (let ((dims))
			    (loop :for elem :across input-vector
			       :do (setq each-type (cons (if (arrayp elem)
							     (element-type elem)
							     (type-of elem))
							 each-type)
					 dims (cons (if (arrayp elem)
							(dims elem)
							'(1))
						    dims)
					 max-rank (max max-rank (if (arrayp elem)
								    (rank elem)
								    0))))
			    dims))
	       (type (apply #'type-in-common each-type))
	       (output (make-array (sort-dimensions (dims input)
						    (loop :for dim :below (length (first each-dims))
						       :collect (reduce #'max (mapcar (lambda (d) (or (nth dim d)
												      1))
										      each-dims))))
				   :element-type type :initial-element (if (eql 'character type)
									   #\  0))))
	  (across input (lambda (oelem ocoords)
			  (if (arrayp oelem)
			      (across oelem (lambda (ielem icoords)
					      (if (> max-rank (length icoords))
						  (loop :for x :below (- max-rank (length icoords))
						     :do (setq icoords (cons 0 icoords))))
					      (setf (apply #'aref output (sort-dimensions ocoords icoords))
						    ielem)))
			      (setf (apply #'aref output (append ocoords '(0)))
				    oelem))))
	  output))))

(defun merge-arrays (input)
  "Merge a set of arrays with the same rank and shape into a larger array."
  (if (= 1 (array-total-size input))
      ;; if the input array is of size one, like #(#(2 4 6 8 10)), simply promote the sole element
      (array-promote (row-major-aref input 0))
      (let* ((first-sub-array (row-major-aref input 0))
	     (inner-dims (dims first-sub-array))
	     (input-vector (make-array (list (array-total-size input))
				       :displaced-to input :element-type (element-type input)))
	     (each-type (let ((types))
			  (loop :for elem :across input-vector
			     :do (setq types (cons (element-type elem) types)))
			  types))
	     (output (make-array (append (remove 1 (dims input))
					 (remove 1 inner-dims))
				 :element-type (apply #'type-in-common each-type)))
	     (dims-match t))
	(across input (lambda (elem coords)
			(if (and dims-match (reduce #'eq (mapcar #'= inner-dims (dims elem))))
			    (if (is-unitary elem)
				;; if the element is a unitary array, just assign its element to the appropriate
				;; output coordinates
				(setf (apply #'aref output coords)
				      (row-major-aref elem 0))
				;; otherwise, iterate across the element and assing the element to the output
				;; coordinates derived from the combined outer and inner array coordinates
				(across elem (lambda (sub-elem sub-coords)
					       (setf (apply #'aref output (append coords sub-coords))
						     sub-elem))))
			    (setq dims-match nil))))
	(if dims-match output))))

(defun split-array (input &optional axis)
  "Split an array into a set of sub-arrays."
  (if (is-unitary input)
      input (let* ((axis (if axis axis (1- (rank input))))
		   (idims (dims input))
		   (output (aops:each (lambda (elem)
					(declare (ignore elem))
					(make-array (list (nth axis idims)) :element-type (type-of input)))
				      (make-array (loop :for dim :in idims :counting dim :into dx
						     :when (not (= dx (1+ axis)))
						     :collect dim)))))
	      (across input (lambda (elem coords)
			      (setf (aref (apply #'aref output (loop :for coord :in coords
								  :counting coord :into cix
								  :when (not (= cix (1+ axis)))
								  :collect coord))
					  (nth axis coords))
				    elem)))
	      output)))

(defun ravel (count-from input &optional axes)
  "Produce a vector from the elements of a multidimensional array."
  (if (and (not axes) (= 1 (rank input)))
      input (if axes (cond ((and (numberp (first axes))
				 (not (integerp (first axes))))
			    (make-array (if (not (first axes))
					    (append (dims input)
						    '(1))
					    (funcall (lambda (lst index)
						       (if (= 0 index)
							   (setq lst (cons 1 lst))
							   (push 1 (cdr (nthcdr (1- index) lst))))
						       lst)
						     (dims input)
						     (- (ceiling (first axes))
							count-from)))
					:element-type (element-type input)
					:displaced-to (copy-array input)))
			   ;; TODO: fix these clauses
			   ;; ((and (< 1 (length (first axes)))
			   ;; 	 (or (> 0 (aref (first axes) 0))
			   ;; 	     (> (aref (first axes) (1- (length (first axes))))
			   ;; 		(rank input))
			   ;; 	     (not (loop :for index :from 1 :to (1- (length (first axes)))
			   ;; 		     :always (= (aref (first axes) index)
			   ;; 				(1+ (aref (first axes) (1- index))))))))
			   ;;  (error (concatenate 'string "Dimension indices must be consecutive and within "
			   ;; 			"the array's number of dimensions.")))
			   ((< 1 (length (first axes)))
			    (let* ((axl (mapcar (lambda (item) (- item count-from))
						(array-to-list (first axes))))
				   (collapsed (apply #'* (mapcar (lambda (index) (nth index (dims input)))
								 axl))))
			      (labels ((dproc (dms &optional index output)
					 (let ((index (if index index 0)))
					   (if (not dms)
					       (reverse output)
					       (dproc (if (= index (first axl))
							  (nthcdr (length axl) dms)
							  (rest dms))
						      (1+ index)
						      (cons (if (= index (first axl))
								collapsed (first dms))
							    output))))))
				(make-array (dproc (dims input))
					    :element-type (element-type input)
					    :displaced-to (copy-array input))))))
		(make-array (list (array-total-size input))
			    :element-type (element-type input)
			    :displaced-to (copy-array input)))))

(defun re-enclose (matrix axes)
  "Convert an array into a set of sub-arrays within a larger array. The dimensions of the containing array and the sub-arrays will be some combination of the dimensions of the original array. For example, a 2 x 3 x 4 array may be composed into a 3-element vector containing 2 x 4 dimensional arrays."
  (labels ((make-enclosure (inner-dims type dimensions)
	     (loop :for d :below (first dimensions)
		:collect (if (= 1 (length dimensions))
			     (make-array inner-dims :element-type type)
			     (make-enclosure inner-dims type (rest dimensions))))))
    (cond ((= 1 (length axes))
	   ;; if there is only one axis just split the array, with permutation first if not splitting
	   ;; along the last axis
	   (if (= (1- (rank matrix))
		  (aref axes 0))
	       (aops:split matrix (1- (rank matrix)))
	       (aops:split (aops:permute (sort (alexandria:iota (rank matrix))
					       (lambda (a b)
						 (declare (ignore a))
						 (= b (aref axes 0))))
					 matrix)
			   (1- (rank matrix)))))
	  ((not (apply #'< (array-to-list axes)))
	   (error "Elements in an axis argument to the enclose function must be in ascending order."))
	  ((= (length axes)
	      (rank matrix))
	   ;; if the number of axes is the same as the matrix's rank, just pass it back
	   matrix)
	  ((let ((indices (mapcar (lambda (item) (+ item (aref axes 0)))
				  (alexandria:iota (- (rank matrix)
						      (- (rank matrix)
							 (length axes)))))))
	     (and (= (first (last indices))
		     (1- (rank matrix)))
		  (loop :for index :in indices :counting index :into iix
		     :always (= index (aref axes (1- iix))))))
	   ;; if there are multiple indices in the axis argument leading up to the last axis,
	   ;; all that's needed is to split the array along the first of the indices
	   (if (> (rank matrix)
		  (length axes))
	       (aops:split matrix (aref axes 0))
	       (make-array '(1) :initial-element matrix)))
	  (t (let* ((matrix-dims (dims matrix))
		    (axis-list (array-to-list axes))
		    (outer-dims)
		    (inner-dims))
	       ;; otherwise, start by separating the dimensions of the original array into sets of dimensions
	       ;; for the output array and each of its enclosed arrays
	       (loop :for axis :below (rank matrix)
		  :do (if (find axis axis-list)
			  (setq inner-dims (cons axis inner-dims))
			  (setq outer-dims (cons axis outer-dims))))
	       (setq inner-dims (reverse inner-dims)
		     outer-dims (reverse outer-dims))
	       ;; create a new blank array of the outer dimensions containing blank arrays of the inner dimensions
	       (let ((new-matrix (make-array (loop :for dm :in outer-dims :collect (nth dm matrix-dims))
					     :initial-contents
					     (make-enclosure (loop :for dm :in inner-dims
								:collect (nth dm matrix-dims))
							     (element-type matrix)
							     (loop :for dm :in outer-dims
								:collect (nth dm matrix-dims))))))
		 ;; iterate through the original array and for each element, apply the same separation
		 ;; to their coordinates that was done to the original array's dimensions and apply the two sets
		 ;; of coordinates to set each value in the nested output arrays to the corresponding values in
		 ;; the original array
		 (across matrix (lambda (item coords)
				  (setf (apply #'aref (apply #'aref new-matrix (loop :for d :in outer-dims
										  :collect (nth d coords)))
					       (loop :for d :in inner-dims :collect (nth d coords)))
					item)))
		 new-matrix))))))

(defun turn (input axis &optional degrees)
  "Rotate an array on a given axis by a given number of degrees or an array containing degree values for each sub-vector."
  (if (not (arrayp input))
      input
      (let* ((idims (dims input))
	     (output (make-array idims :element-type (element-type input)))
	     (rdimension (nth axis idims)))
	(across input (lambda (item coords)
			(let ((degree (if (integerp degrees)
					  degrees (if degrees
						      (apply #'aref degrees (loop :for coord :in coords
									       :counting coord :into this-axis
									       :when (/= axis (1- this-axis))
									       :collect coord))))))
			  (setf (apply #'aref output (loop :for coord :in coords :counting coord :into this-axis
							:collect (if (or (/= axis (1- this-axis))
									 (and degree (= 0 degree)))
								     coord (if degree (mod (- coord degree)
											   rdimension)
									       (- rdimension 1 coord)))))
				item))))
	output)))

(defun invert-matrix (in-matrix)
  "Find the inverse of a square matrix."
  (let ((dim (array-dimension in-matrix 0))   ;; dimension of matrix
	(det 1)                               ;; determinant of matrix
	(l)                                   ;; permutation vector
	(m)                                   ;; permutation vector
	(temp 0)
	(out-matrix (make-array (dims in-matrix))))

    (if (not (equal dim (array-dimension in-matrix 1)))
	(error "invert-matrix () - matrix not square"))

    ;; (if (not (equal (array-dimensions in-matrix)
    ;;                 (array-dimensions out-matrix)))
    ;;     (error "invert-matrix () - matrices not of the same size"))

    ;; copy in-matrix to out-matrix if they are not the same
    (when (not (equal in-matrix out-matrix))
      (do ((i 0 (1+ i)))
	  ((>= i dim))    
	(do ((j 0 (1+ j)))
	    ((>= j dim)) 
	  (setf (aref out-matrix i j) (aref in-matrix i j)))))

    ;; allocate permutation vectors for l and m, with the 
    ;; same origin as the matrix
    (setf l (make-array `(,dim)))
    (setf m (make-array `(,dim)))

    (do ((k 0 (1+ k))
	 (biga 0)
	 (recip-biga 0))
	((>= k dim))

      (setf (aref l k) k)
      (setf (aref m k) k)
      (setf biga (aref out-matrix k k))

      ;; find the biggest element in the submatrix
      (do ((i k (1+ i)))
	  ((>= i dim))    
	(do ((j k (1+ j)))
	    ((>= j dim)) 
	  (when (> (abs (aref out-matrix i j)) (abs biga))
	    (setf biga (aref out-matrix i j))
	    (setf (aref l k) i)
	    (setf (aref m k) j))))

      ;; interchange rows
      (if (> (aref l k) k)
	  (do ((j 0 (1+ j))
	       (i (aref l k)))
	      ((>= j dim)) 
	    (setf temp (- (aref out-matrix k j)))
	    (setf (aref out-matrix k j) (aref out-matrix i j))
	    (setf (aref out-matrix i j) temp)))

      ;; interchange columns 
      (if (> (aref m k) k)
	  (do ((i 0 (1+ i))
	       (j (aref m k)))
	      ((>= i dim)) 
	    (setf temp (- (aref out-matrix i k)))
	    (setf (aref out-matrix i k) (aref out-matrix i j))
	    (setf (aref out-matrix i j) temp)))

      ;; divide column by minus pivot (value of pivot 
      ;; element is in biga)
      (if (equalp biga 0) 
	  (return-from invert-matrix 0))
      (setf recip-biga (/ 1 biga))
      (do ((i 0 (1+ i)))
	  ((>= i dim)) 
	(if (not (equal i k))
	    (setf (aref out-matrix i k) 
		  (* (aref out-matrix i k) (- recip-biga)))))

      ;; reduce matrix
      (do ((i 0 (1+ i)))
	  ((>= i dim)) 
	(when (not (equal i k))
	  (setf temp (aref out-matrix i k))
	  (do ((j 0 (1+ j)))
	      ((>= j dim)) 
	    (if (not (equal j k))
		(incf (aref out-matrix i j) 
		      (* temp (aref out-matrix k j)))))))

      ;; divide row by pivot
      (do ((j 0 (1+ j)))
	  ((>= j dim)) 
	(if (not (equal j k))
	    (setf (aref out-matrix k j)
		  (* (aref out-matrix k j) recip-biga))))

      (setf det (* det biga)) ;; product of pivots
      (setf (aref out-matrix k k) recip-biga)) ;; k loop

    ;; final row & column interchanges
    (do ((k (1- dim) (1- k)))
	((< k 0))
      (if (> (aref l k) k)
	  (do ((j 0 (1+ j))
	       (i (aref l k)))
	      ((>= j dim))
	    (setf temp (aref out-matrix j k))
	    (setf (aref out-matrix j k) 
		  (- (aref out-matrix j i)))
	    (setf (aref out-matrix j i) temp)))
      (if (> (aref m k) k)
	  (do ((i 0 (1+ i))
	       (j (aref m k)))
	      ((>= i dim))
	    (setf temp (aref out-matrix k i))
	    (setf (aref out-matrix k i) 
		  (- (aref out-matrix j i)))
	    (setf (aref out-matrix j i) temp))))
    det ;; return determinant
    out-matrix))

(defun stencil (input process window-dims movement)
  "Apply a given function to sub-arrays of an array with specified dimensions sampled according to a given pattern of movement across the array."
  (let* ((idims (apply #'vector (dims input)))
	 (output-dims (loop :for dim :below (length window-dims)
			 :collect (ceiling (- (/ (aref idims dim) (aref movement dim))
					      (if (and (evenp (aref window-dims dim))
						       (or (= 1 (aref movement dim))
							   (oddp (aref idims dim))))
						  1 0)))))
	 (output (make-array output-dims)))
    (across output (lambda (elem coords)
		     (declare (ignore elem))
		     (let ((window (make-array (array-to-list window-dims) :element-type (element-type input))))
		       (across window (lambda (welem wcoords)
					(declare (ignore welem))
					(let ((ref-coords
					       (loop :for cix :below (length wcoords)
						  :collect (let ((melem (aref movement cix))
								 (wdim (aref window-dims cix)))
							     (+ (nth cix wcoords)
								(- (* melem (nth cix coords))
								   (floor (/ (- wdim (if (evenp wdim) 1 0))
									     2))))))))
					  (setf (apply #'aref window wcoords)
						(if (loop :for coord :in ref-coords :counting coord :into cix
						       :always (<= 0 coord (1- (aref idims (1- cix)))))
						    (apply #'aref input ref-coords)
						    0)))))
		       (setf (apply #'aref output coords)
			     (funcall process window
				      (make-array (list (length coords))
						  :element-type '(signed-byte 8)
						  :initial-contents
						  (loop :for coord :in coords :counting coord :into cix
						     :collect (* (aref movement (1- cix))
								 (if (= 0 coord)
								     1 (if (= coord (1- (nth (1- cix) output-dims)))
									   -1 0))))))))))
    output))

(defun array-impress (input &key (prepend nil) (append nil) (collate nil) (format nil))
  "Render the contents of an array into a character matrix or, if the collate option is taken, an array with sub-matrices of characters."
  (cond ((not (arrayp input))
	 (funcall (if format format #'write-to-string)
		  input))
	;; if indenting with a character, prepend it to the string; strings are otherwise passed back as-is
	((stringp input) (if (not prepend)
			     input (concatenate 'string (list prepend) input)))
	((equalp #0A0 input)
	 "")
	(t (let* ((idims (dims input))
		  ;; the x-offset and y-offset for each column and row; each array has an extra element to
		  ;; represent the total width and height of the output array
		  (x-offsets (make-array (list (1+ (first (last idims))))
					 :initial-element 0 :element-type 'fixnum))
		  (y-offsets (make-array (list (1+ (reduce #'* (rest (reverse idims)))))
					 :initial-element 0 :element-type 'fixnum))
		  (col-widths (make-array (list (first (last idims)))
					  :initial-element 1 :element-type 'fixnum))
		  (col-types (make-array (list (first (last idims))) :initial-element nil))
		  ;; an array of decimal point positions for each column; first row holds the position of the
		  ;; decimal point, second holds the overall length of each printed number
		  (col-decimals (make-array (list 3 (first (last idims)))
					    :initial-element 0 :element-type '(unsigned-byte 32)))
		  (strings (make-array idims))
		  (output-default-char #\ )
		  (row) (empty-rows))
	     (declare (dynamic-extent idims x-offsets y-offsets col-widths col-types col-decimals
				      strings output-default-char row empty-rows))
	     (symbol-macrolet ((this-string (apply #'aref strings coords))
			       (this-col-width (aref col-widths last-coord))
			       (this-col-type (aref col-types last-coord))
			       (last-col-type (aref col-types (1- last-coord)))
			       (decimal-place (aref col-decimals 0 last-coord))
			       (decimal-length (aref col-decimals 1 last-coord))
			       (trailing (aref col-decimals 2 last-coord)))
	       (across input (lambda (elem coords)
			       (let* ((last-coord (first (last coords))))
				 (flet ((add-column-types (&rest types)
					  (loop :for type :in types
					     :do (if (not (member type this-col-type))
						     (setf this-col-type (cons type this-col-type))))))
				   (cond ((or (characterp elem)
					      (and (stringp elem) (= 1 (length elem))))
					  ;; characters are simply passed through,
					  ;; 1-item strings are rendered the same way
					  (add-column-types :character)
					  (setf this-string (if (characterp elem)
								elem (aref elem 0))))
					 ((arrayp elem)
					  ;; recurse to handle nested arrays, passing back the rendered character
					  ;; array and adjust the offsets to allow for its height and width
					  (let ((rendered (array-impress elem :format format
									 :prepend output-default-char)))
					    ;; in the case a 1D array (string) is passed back, height defaults to 1
					    (setf this-string rendered)
					    ;; TODO: improve element type-checking here
					    (if (eq 'character (element-type elem))
						(add-column-types :array :character-array)
						(if (or (eq 'fixnum (element-type elem))
							(eq 'bit (element-type elem)))
						    (add-column-types :array :number-array)
						    (add-column-types :array :mixed-array)))))
					 ((numberp elem)
					  ;; use the number-formatting function to make strings out of numbers,
					  ;; and adjust the next x-offset to allow for their width
					  (let* ((elem-string (funcall (if format format #'write-to-string)
								       elem))
						 (is-float (or (floatp elem) (and (not (integerp elem))
										  (rationalp elem)))))
					    (setf this-string elem-string)
					    (if is-float (add-column-types :number :float)
						(add-column-types :number)))))))))
	       (across input (lambda (elem coords)
			       (let* ((last-coord (first (last coords)))
				      (next-elem (if (< last-coord (- (length x-offsets) 2))
						     (apply #'aref input (append (butlast coords)
										 (list (1+ last-coord))))))
				      (last-elem (if (not (= 0 last-coord))
						     (apply #'aref input (append (butlast coords)
										 (list (1- last-coord))))))
				      (elem-width 1)
				      (elem-height 1)
				      (decimals 0)
				      (is-first-col (not last-elem))
				      (is-last-col (not next-elem))
				      (rendered (apply #'aref strings coords)))
				 (flet ((is-pure-character-column (&optional index)
					  (and (eq :character (first (aref col-types (or index last-coord))))
					       (not (rest (aref col-types (or index last-coord)))))))
				   (cond ((and (arrayp elem)
					       (not (characterp rendered)))
					  (let ((rdims (reverse (dims rendered))))
					    (setf elem-height (or (second rdims) 1)
						  elem-width (first rdims))))
					 ((numberp elem)
					  (setf decimals (+ (if (> 0 elem) 2 1)
							    ;; add an extra space for the ¯ if negative
							    (if (= 0 elem)
								0 (max 0 (floor (log (abs elem) 10)))))
						;; increment the decimal point position if it's further right
						;; than in other rows of this column; negative values occupy
						;; an extra space due to the minus sign
						decimal-place (max decimals decimal-place)
						trailing (max trailing (- (length rendered) decimals))
						elem-width (+ decimal-place trailing)
						decimal-length (max elem-width decimal-length))))
				   ;; if this is the beginning of a new row, increment the row's y-offset
				   ;; by the number of preceding empty rows
				   (if (= 0 last-coord)
				       (setf row (reduce #'+ (mapcar #'* (rest (reverse coords))
								     (let ((current 1))
								       (loop :for dim
									  :in (cons 1 (rest (reverse (rest idims))))
									  :collect (setq current
											 (* current dim))))))
					     ;; find the total number of empty lines preceding this row by encoding
					     ;; the coordinates excepting the last two with a series of number bases
					     ;; found by multiplying each dimension going backwards excepting the
					     ;; last 2 by the previous base and adding 1
					     empty-rows
					     (reduce #'+ (mapcar #'* (cddr (reverse coords))
								 (cons 1 (let ((last 1))
									   (loop :for dim
									      :in (reverse (rest (butlast idims 2)))
									      :collect (setq last
											     (1+ (* dim last))))))))
					     (aref y-offsets row)
					     (+ empty-rows (aref y-offsets row))))
				   (setf this-col-width
					 (max this-col-width
					      ;; don't add a left buffer space if 1. It's the first column
					      ;; 2. It's an array column and the previous column was also an array
					      ;; (thus preserving the 2-space margin between arrays)
					      ;; or 3. It's an array column and the previous column held only chars
					      (+ elem-width
						 (if (or is-first-col
							 (and (characterp elem) (characterp last-elem))
							 (and (member :array this-col-type)
							      ;; note: the indentation is still done if this is
							      ;; an array column but there's a numeric value
							      ;; that's bigger than any of the array values thus
							      ;; far; otherwise there will be no space between the
							      ;; number and the previous column
							      (not (and (numberp elem)
									(> elem-width this-col-width)))
							      (or (member :array last-col-type)
								  (is-pure-character-column
								   (1- last-coord)))))
						     0 1)
						 ;; add right buffer space if this column is 1. Not the last and
						 ;; 2. Contains an array
						 (if (and (not is-last-col)
							  (member :array this-col-type)
							  (not (is-pure-character-column (1+ last-coord))))
						     1 0)))
					 (aref x-offsets (1+ last-coord))
					 ;; set as x-offset by the width of the element or the maximum width
					 ;; of this column (whichever is greater) minus the prior x-offset
					 (max (aref x-offsets (1+ last-coord))
					      (+ (max elem-width this-col-width)
						 (aref x-offsets last-coord)))
					 (aref y-offsets (1+ row))
					 (max (aref y-offsets (1+ row))
					      (+ elem-height (aref y-offsets row) (- empty-rows))
					      (if (and (= 2 (length y-offsets))
						       (< 1 (aref y-offsets (1+ row))))
						  ;; don't increment the next row if it is the last, there's only
						  ;; one row in the output and its value is higher than one;
						  ;; this mainly applies to vectors containing nested arrays
						  ;; of rank>1
						  (aref y-offsets (1+ row))
						  (if (= row (- (length y-offsets) 2))
						      (+ elem-height (aref y-offsets row))
						      (+ row (if (and (= 0 last-coord)
								      (/= last-coord (1- (length y-offsets))))
								 1 0))))))))))
	       ;; (print (list :xoyo x-offsets y-offsets col-widths col-decimals))
	       ;; (princ #\Newline)
	       ;; collated output is printed to a multidimensional array whose sub-matrices are character-rendered
	       ;; versions of the original sub-matrices, as per APL's [⍕ format] function. If a prepend
	       ;; character is set, the output array has an extra element in its last dimension to hold the
	       ;; indenting character
	       (let ((output (if collate (make-array (append (butlast idims)
							     (list (+ (if (or prepend append) 1 0)
								      (aref x-offsets (1- (length x-offsets))))))
						     :element-type 'character :initial-element output-default-char)
				 (make-array (list (aref y-offsets (1- (length y-offsets)))
						   (+ (if (or prepend append) 1 0)
						      (aref x-offsets (1- (length x-offsets)))))
					     :element-type 'character :initial-element output-default-char))))
		 (across strings
			 (lambda (chars coords)
			   ;; calculate the row of output currently being produced
			   (let* ((row (reduce #'+ (mapcar #'* (rest (reverse coords))
							   (let ((current 1))
							     (loop :for dim
								:in (cons 1 (rest (reverse (rest idims))))
								:collect (setq current (* current dim)))))))
				  (original (apply #'aref input coords))
				  (last-coord (first (last coords)))
				  (chars-width (first (last (dims chars))))
				  (array-col (member :array this-col-type))
				  (array-last-col (if (not (= 0 last-coord))
						      (member :array last-col-type)))
				  (numeric-col (member :number this-col-type))
				  (float-col (member :float this-col-type))
				  (is-first-col (= 0 last-coord))
				  (is-last-col (= last-coord (1- (length col-types)))))
			     (flet ((is-pure-character-column (&optional index)
				      (and (eq :character (first (aref col-types (or index last-coord))))
					   (not (rest (aref col-types (or index last-coord))))))
				    (is-character-array-column (&optional index)
				      (let ((type-list (aref col-types (or index last-coord))))
					(and (not (member :number type-list))
					     (not (member :mixed-array type-list))
					     (not (member :number-array type-list))))))
			       (if (arrayp chars)
				   ;; print a string or sub-matrix of characters; the coordinate conversion
				   ;; is different depending on whether collated output is being produced
				   (across chars
					   (lambda (element ecoords)
					     (let* ((d-indent
						     ;; derive this cell's decimal indentation; negative
						     ;; values are indented 1 space less to allow for
						     ;; the minus sign
						     (if (and float-col (numberp original))
							 (max 0 (- decimal-place
								   (+ (if (> 0 original) 2 1)
								      (if (> 1 (abs original))
									  0 (floor (log (abs original) 10))))))))
						    ;; pad arrays with a space to the right, unless
						    ;; they're at the last column or the next column is a
						    ;; character column
						    (right-space (if (and (not is-last-col)
									  array-col (not (is-pure-character-column
											  (1+ last-coord))))
								     -1 0))
						    (x-coord (+ (if (second ecoords)
								    (second ecoords) (first ecoords))
								(aref x-offsets last-coord)
								;; if this array is prepended, shift
								;; printed output right by 1 space
								(if prepend 1 0)
								;; left-justify printed items unless the
								;; column contains numbers, in which case
								;; everything is right-justified
								(if numeric-col
								    (+ right-space
								       (- (if (and float-col (numberp original))
									      (- (- decimal-length chars-width)
										 (if d-indent d-indent 0))
									      0))
								       (- (aref x-offsets (1+ last-coord))
									  (aref x-offsets last-coord)
									  chars-width))
								    ;; add a right padding space if this is
								    ;; a character array column and 
								    (if (and (not is-first-col)
									     (not array-last-col))
									1 0)))))
					       (if collate (setf (apply #'aref output (append (butlast coords 1)
											      (list x-coord)))
								 element)
						   (setf (aref output (+ (aref y-offsets row)
									 (if (not (second ecoords))
									     0 (first ecoords)))
							       x-coord)
							 element)))))
				   ;; print a single character
				   (let ((x-coord (+ (if prepend 1 0)
						     ;; add a space of padding to the left if 1. This is a
						     ;; single character in a character array column, or
						     ;; 2. This is a character column but the prior column
						     ;; is not, or 3. This is an array column, the prior column
						     ;; was an array column and this is not a numeric column
						     ;; (which would mean characters are right-aligned)
						     (if (or (and (is-character-array-column)
								  (not (is-pure-character-column)))
							     (and (not is-first-col)
								  (is-pure-character-column)
								  (not (is-pure-character-column (1- last-coord))))
							     (and array-col array-last-col (not numeric-col)))
							 1 0)
						     (aref x-offsets last-coord)
						     ;; right-justify the character if this column holds
						     ;; numbers as well
						     (max 0 (if (not numeric-col)
								0 (- this-col-width
								     ;; add 2 spaces to the right if this column
								     ;; holds an array, it isn't the last column
								     ;; and the next column holds a character
								     (if (and array-col (not is-last-col)
									      (not (is-pure-character-column
										    (1+ last-coord))))
									 2 1)))))))
				     (if collate (setf (apply #'aref output (append (butlast coords 1)
										    (list x-coord)))
						       chars)
					 (setf (aref output (aref y-offsets row)
						     x-coord)
					       chars))))))))
		 ;; (print (list :cd col-decimals col-widths col-types))
		 ;; if prepending or appending a character, it is placed in the array here;
		 ;; this is more complicated for a collated array and it is not needed if the
		 ;; character is the same as the default character for the array
		 (if (or (and append (not (char= append output-default-char)))
			 (and prepend (not (char= prepend output-default-char))))
		     (let ((last-dim (first (last (dims output)))))
		       (if collate (across output (lambda (elem coords)
						    (declare (ignore elem))
						    (setf (apply #'aref output coords)
							  (if prepend prepend append))))
	     		   (if prepend (loop :for row :below (first (dims output))
	     				  :do (setf (aref output row 0) prepend))
	     		       (loop :for row :below (first (dims output))
	     			  :do (setf (aref output row (1- last-dim)) append))))))
		 output))))))

(defmacro matrix-print (input &rest options)
  "Print a character matrix generated by array-impress."
  (let ((rendered (gensym)))
    `(let ((,rendered (array-impress ,input ,@options)))
       (if (stringp ,rendered)
	   ,rendered (make-array (list (array-total-size ,rendered))
				 :element-type 'character :displaced-to ,rendered)))))
