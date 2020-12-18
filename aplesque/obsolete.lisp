;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Aplesque -*-
;;;; obsolete.lisp

(in-package #:aplesque)

;; an archive of old implementations, not as fast as current ones but historically interesting

(defun catenate (a1 a2 axis)
  "Join two arrays along the specified axis."
  (let* ((rank1 (rank a1)) (rank2 (rank a2))
	 (max-rank (max rank1 rank2)) (uneven (/= rank1 rank2))
	 (offset (if (and uneven (< rank1 rank2)) 1 (nth axis (dims a1))))
	 (output-type (type-in-common (if (arrayp a1) (element-type a1) (assign-element-type a1))
				      (if (arrayp a2) (element-type a2) (assign-element-type a2)))))
    (if (and (> axis (1- max-rank))
	     (not (and (= 0 axis max-rank))))
	(error "Invalid axis (~a) for array with ~a dimensions." (1+ axis) max-rank))
    (if (loop :for a :in (list a1 a2) :always (= 0 (rank a)))
	(make-array 2 :element-type output-type :initial-contents (list a1 a2))
	find the shape of the output using the higher-rank array as reference
	(let ((output (make-array (destructuring-bind (ref-array second-array)
				      (funcall (if (>= rank1 rank2) #'identity #'reverse)
					       (list a1 a2))
				    (if (and uneven (arrayp second-array)
					     (< 1 (abs (- rank1 rank2))))
					(error "Catenated arrays must be at most one rank apart.")
					iterate through the reference array dimensions
					and increment the axis dimension by one (if uneven) or the
					axis dimension of the other array if both are the same rank
					(loop :for i :from 0 :for rdim :in (dims ref-array)
					   :collect (if (= i axis)
							(+ rdim (if uneven 1 (nth i (dims second-array))))
							(if (or (not (dims second-array))
								(= rdim (nth (- i (if (and uneven (> i axis))
										      1 0))
									     (dims second-array))))
							    rdim (error "Mismatched array dimensions."))))))
				  :element-type output-type)))
	  (across output (lambda (elem coords)
			   (declare (ignore elem))
			   (let* ((first-array)
				  (in-coords (loop :for x :from 0 :for c :in coords
						:when (and (= x axis)
							   (if (setq first-array (< c offset))
							       (not (arrayp a1)) (not (arrayp a2))))
						:do (return nil)
						:when (not (and uneven (= x axis)
								(if first-array (< rank1 rank2)
								    (> rank1 rank2))))
						:collect (if (or first-array (/= x axis))
							     c (- c offset))))
				  (in-array (if first-array a1 a2)))
	  		     (setf (apply #'aref output coords)
	  			   (if (not in-coords)
				       in-array (apply #'aref in-array in-coords))))))
	  output))))

(defun turn (input axis &optional degrees)
  "Rotate an array on a given axis by a given number of degrees or an array containing degree values for each sub-vector."
  (if (not (arrayp input))
      input
      (let* ((idims (dims input))
	     (ocoords (loop :for i :below (rank input) :collect 0))
	     (dcoords (if (not (integerp degrees)) (loop :for i :below (1- (rank input)) :collect 0)))
	     (output (make-array idims :element-type (element-type input)))
	     (rdimension (nth axis idims)))
	(across input (lambda (item coords)
			(let ((degree (if (integerp degrees)
					  degrees (if degrees (let ((dcix 0))
								(loop :for coord :in coords
								   :for this-axis :from 0
								   :when (/= axis this-axis)
								   :do (setf (nth dcix dcoords) coord
									     dcix (1+ dcix)))
								(apply #'aref degrees dcoords))))))
			  (loop :for coord :in coords :for this-axis :from 0
			     :do (setf (nth this-axis ocoords)
				       (if (or (/= axis this-axis)
					       (and degree (= 0 degree)))
					   coord (if degree (mod (- coord degree) rdimension)
						     (- rdimension 1 coord)))))
			  (setf (apply #'aref output ocoords)
				item))))
	output)))

(defun each-scalar (function omega)
  "Iterate over an array/arrays of scalar values, operating upon them and returning the output in the most efficiently-stored array capable of holding said output."
  (let ((type)
	(output (make-array (dims omega))))
    (if (not (arrayp omega))
	omega (progn (across omega (lambda (elem coords)
				     (declare (dynamic-extent elem coords))
				     (let ((result (if (eq t function)
						       elem (funcall function elem coords))))
				       (if type (if (not (typep result type))
						    (setq type (type-in-common type (assign-element-type result))))
					   (setq type (assign-element-type result)))
				       (if coords (setf (apply #'aref output coords) result)
					   (setf (aref output) result))
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
					    (declare (dynamic-extent elem coords))
					    (setf (apply #'aref true-output coords)
						  elem)))
			   true-output))))))
