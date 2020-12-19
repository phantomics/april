;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; parallel.lisp

(in-package #:april)

"An implementation of multithreading in April. NOTE: This functionality is still in an alpha state and thus is not implemented by default, therefore this file must be manually compiled."

;; (defun system-command-exists (command-string &optional prefix)
;;   (if (not prefix) (setq prefix ""))
;;   (= 0 (multiple-value-bind (1st 2nd error-code)
;; 	   (uiop:run-program (format nil "~acommand -v ~a" prefix command-string)
;; 			     :ignore-error-status t)
;; 	 (declare (ignore 1st 2nd))
;; 	 error-code)))

;; (defvar *april-thread-count*
;;   (with-open-stream (cmd-out (make-string-output-stream))
;;     (uiop:run-program (case (uiop:operating-system)
;; 			((:linux :linux-target)
;; 			 (if (system-command-exists "nproc") "nproc" ""))
;; 			((:macosx :darwin)
;; 			 (if (system-command-exists "sysctl") "sysctl -n hw.logicalcpu" ""))
;; 			((:bsd :freebsd :openbsd :netbsd)
;; 			 (if (system-command-exists "sysctl") "sysctl -n hw.ncpu" "")))
;; 		      :output cmd-out)
;;     (let ((output (get-output-stream-string cmd-out)))
;;       (if (= 0 (length output))
;; 	  1 (read-from-string output)))))

(defvar *april-parallel-kernel* (lparallel:make-kernel (1- (cl-cpus:get-number-of-processors))
						       :name "april-language-kernel"))

(setf lparallel:*kernel* *april-parallel-kernel*)

;; (set-system-meta this-idiom :thread-count *april-thread-count*)

;; (defun rmi-convert (dims index)
;;   "Convert row-major [index] into a list of the corresponding array coordinates according to [dims] dimensions of array."
;;   (flet ((rebase (nth-coord number)
;; 	   (let ((operand number) (last-base 1)
;; 		 (base 1) (component 1) (element 0))
;; 	     (loop :for i :from (1- (length dims)) :downto nth-coord
;; 		:do (setq last-base base
;; 			  base (* base (nth i dims))
;; 			  component (if (= 0 base)
;; 					operand (* base (nth-value 1 (floor (/ operand base)))))
;; 			  operand (- operand component)
;; 			  element (/ component last-base)))
;; 	     element)))
;;     (loop :for d :below (length dims) :collect (rebase d index))))

;; (defun subs-convert (dims subs)
;;   (let ((result 0) (factor 1) (dim-index 0))
;;     (loop :for s :in subs :counting s :into sx
;;        :do (if (>= s (aref dims dim-index))
;; 	       (error "Invalid index for dimension ~W." sx)
;; 	       (setq result (+ result (* factor s))
;; 		     factor (* factor (aref dims dim-index))
;; 		     dim-index (1+ dim-index))))
;;     result))

;; (defun get-indices (dims indices &optional output (dim-index 0) (out-index 0) path)
;;   (let* ((to-return (null output))
;; 	 (olen (if (not output) (reduce #'* (loop :for i :in indices :for d :across dims
;; 					       :collect (if i (length i) d)))))
;; 	 (output (or output (make-array olen :element-type (list 'integer 0 (reduce #'* dims))))))
;;     (print (list :in dims indices output))
;;     (if (first indices)
;; 	(loop :for i :in (if (listp (first indices))
;; 			     (first indices) (list (first indices)))
;; 	   :do (if (not (rest indices))
;; 		   (setf (aref output out-index) (subs-convert dims (reverse (cons i path)))
;; 			 out-index (1+ out-index))
;; 		   (setq out-index (get-indices dims (rest indices)
;; 						output (1+ dim-index) out-index (cons i path)))))
;; 	(loop :for i :below (aref dims dim-index)
;; 	   :do (if (not (rest indices))
;; 		   (setf (aref output out-index) (subs-convert dims (reverse (cons i path)))
;; 			 out-index (1+ out-index))
;; 		   (setq out-index (get-indices dims (rest indices)
;; 						output (1+ dim-index) out-index (cons i path))))))
;;     (values (if to-return output out-index))))

;; (defun segment-area (size section-count)
;;   (let* ((section-count (min section-count size))
;; 	 (division-size (/ size section-count))
;; 	 (start-points (make-array (list section-count)))
;; 	 (section-lengths (make-array (list section-count))))
;;     (loop :for i :below section-count :do (setf (aref start-points i) (floor (* i division-size))))
;;     (loop :for i :below section-count :do (setf (aref section-lengths i)
;; 						(- (if (= i (1- section-count))
;; 						       size (aref start-points (1+ i)))
;; 						   (aref start-points i))))
;;     (values start-points section-lengths section-count)))


;; (defun pacross (array function &key (indices))
;;   "Move across an array and perform operations on each element and its coordinates, operating on sections of the array in parallel via pdotimes."
;;   (let* ((dims (aops:dims array)) (size (aops:size array))
;; 	 ;; (displaced-vector (make-array size :element-type (element-type array) :displaced-to array))
;; 	 (output (make-array dims))
;; 	 (count-vector (make-array size :element-type t :displaced-to output))
;; 	 (input-vector (make-array size :initial-contents (loop :for i :below size :collect i))))
;;     (loop :for i :below size :do (setf (aref count-vector i) i))
;;     (lparallel:pmap-into count-vector function input-vector)
;;     output))

;; (defun pturn (input axis &optional degrees)
;;   "Rotate an array on a given axis by a given number of degrees or an array containing degree values for each sub-vector."
;;   (if (not (arrayp input))
;;       input
;;       (let* ((ocoords (loop :for i :below (aops:rank input) :collect 0))
;; 	     (dcoords (if (not (integerp degrees)) (loop :for i :below (1- (aops:rank input)) :collect 0)))
;; 	     (dims (aops:dims input))
;; 	     (rdimension (nth axis dims)))
;; 	(pacross input (lambda (item)
;; 			 (declare (dynamic-extent item))
;; 			 (let* ((coords (rmi-convert dims item))
;; 				(degree (if (integerp degrees)
;; 					    degrees (if degrees (let ((dcix 0))
;; 								  (loop :for coord :in coords
;; 								     :counting coord :into this-axis
;; 								     :when (/= axis (1- this-axis))
;; 								     :do (setf (nth dcix dcoords) coord
;; 									       dcix (1+ dcix)))
;; 								  (apply #'aref degrees dcoords))))))
;; 			   (loop :for coord :in coords :counting coord :into this-axis
;; 			      :do (setf (nth (1- this-axis) ocoords)
;; 					(if (or (/= axis (1- this-axis))
;; 						(and degree (= 0 degree)))
;; 					    coord (if degree (mod (+ coord degree) rdimension)
;; 						      (+ rdimension 1 coord)))))
;; 			   (apply #'aref input ocoords)))))))
