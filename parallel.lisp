;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; parallel.lisp

(in-package #:april)

"An implementation of multithreading in April. NOTE: This functionality is still in an alpha state and thus is not implemented by default, therefore this file must be manually compiled."

(defun system-command-exists (command-string &optional prefix)
  (if (not prefix) (setq prefix ""))
  (= 0 (multiple-value-bind (1st 2nd error-code)
	   (uiop:run-program (format nil "~acommand -v ~a" prefix command-string)
			     :ignore-error-status t)
	 (declare (ignore 1st 2nd))
	 error-code)))

(defvar *april-thread-count*
  (with-open-stream (cmd-out (make-string-output-stream))
    (case (uiop:operating-system)
      ((:macosx :darwin)
       (if (system-command-exists "sysctl")
	   (progn (uiop:run-program "sysctl -n hw.logicalcpu" :output cmd-out)
		  (*read-from-string (get-output-stream-string cmd-out)))
	   1))
      ((:linux :linux-target)
       (if (system-command-exists "nproc")
	   (progn (uiop:run-program "nproc" :output cmd-out)
		  (read-from-string (get-output-stream-string cmd-out)))
	   1))
      ((:bsd :freebsd :openbsd :netbsd)
       (if (system-command-exists "sysctl")
	   (progn (uiop:run-program "sysctl -n hw.ncpu" :output cmd-out)
		  (read-from-string (get-output-stream-string cmd-out)))
	   1))
      (otherwise 1))))

(defvar *april-parallel-kernel* (lparallel:make-kernel *april-thread-count* :name "april-language-kernel"))

(setf lparallel:*kernel* *april-parallel-kernel*)

(set-system-meta this-idiom :thread-count *april-thread-count*)

(initialize-for-environment
 :across (lambda (original-function)
      	   (lambda (input function &rest args)
      	     (if (and (< 1 *april-thread-count*) (not (getf args :singlethreaded))
      		      (not (getf args :count)) (not (getf args :elements)))
      		 (let ((idims (dims input))
      		       (proms (make-array (list (1- *april-thread-count*)))))
      		   (loop :for i :below (1- *april-thread-count*) :do (setf (aref proms i) (promise)))
      		   (multiple-value-bind (start-points counts)
      		       (segment-area idims (1- *april-thread-count*))
      		     (pdotimes (i (length start-points))
      		       (apply original-function input function
      			      (append (list :count (aref counts i)
					    :start-at (rmi-convert idims (aref start-points i)))
      				      args))
      		       (fulfill (aref proms i) t))
		     (loop :for p :across proms :do (force p))))
      		 (apply original-function input function args)))))

;; (initialize-for-environment :across #'identity)

(defun segment-area (dims section-count)
  (let* ((total-size (reduce #'* dims))
	 (section-count (min section-count total-size))
	 (division-size (/ total-size section-count))
	 (start-points (make-array (list section-count)))
	 (section-lengths (make-array (list section-count))))
    
    (loop :for i :below section-count :do (setf (aref start-points i) (floor (* i division-size))))
    (loop :for i :below section-count :do (setf (aref section-lengths i)
						(- (if (= i (1- section-count))
						       total-size (aref start-points (1+ i)))
						   (aref start-points i))))
    (values start-points section-lengths)))

(defun rmi-convert (dims index)
  "Convert row-major [index] into a list of the corresponding array coordinates according to [dims] dimensions of array."
  (flet ((rebase (nth-coord number)
	   (let ((operand number) (last-base 1)
		 (base 1) (component 1) (element 0))
	     (loop :for i :from (1- (length dims)) :downto nth-coord
		:do (setq last-base base
			  base (* base (nth i dims))
			  component (if (= 0 base)
					operand (* base (nth-value 1 (floor (/ operand base)))))
			  operand (- operand component)
			  element (/ component last-base)))
	     element)))
    (loop :for d :below (length dims) :collect (rebase d index))))

(defun lp-test (&optional width)
  (let* ((thread-count 24) (width (or width 50))
	 (output (make-array (list thread-count width) :initial-element 0)))
    (pdotimes (i thread-count)
      (loop :for x :below width :do (setf (row-major-aref output (+ x (* i width))) 1)))
    :complete))

#|
(defun divide-volume (dims section-count &optional factor idims divisions)
  (let* ((is-root (not factor))
	 (section-count (min section-count (reduce #'* dims)))
	 (divisions (or divisions (make-array (list section-count) :initial-element nil)))
	 (idims (or idims (sort (mapcar #'list dims (iota (length dims)))
				(lambda (a b) (> (first a) (first b))))))
	 (last-end -1) (this-dim (caar idims)) (factor (* this-dim (or factor 1)))
	 (int-count (loop :for i :below (min section-count this-dim) :collect 0)))
    (flet ((set-interval (interval)
	     (setf (nth interval int-count) (1+ (nth interval int-count)))
	     interval))
      (loop :for c :below section-count
	 :do (setf (aref divisions c)
		   (cons (if (= 0 (mod section-count factor))
			     (set-interval (floor (* c (/ this-dim section-count))))
			     (if (> 1 (floor (* 1 (/ this-dim section-count))))
				 (set-interval (min (1- this-dim)
						    (floor (* c (/ this-dim (1- section-count))))))
				 (let* ((base-start (floor (* c (/ this-dim section-count))))
					(start (max base-start (1+ last-end))))
				   (cons start (setq last-end (min (1- this-dim)
								   (floor (* (+ 1/2 c) (/ this-dim
											  section-count)))))))))
			 (aref divisions c))))
      ;; (print (list :ii divisions idims int-count))
      (if (< factor section-count)
	  (loop :for i :in int-count :summing i :into isum
	     :do (divide-volume (rest dims) i factor (rest idims)
				(make-array (list i) :element-type t :displaced-to divisions
					    :displaced-index-offset (- isum i)))))
      (if is-root (let* ((address-length (length (aref divisions 0)))
			 (ranges (make-array (list section-count)))
			 (max-range (reduce #'max (mapcar #'second (subseq idims 0 address-length)))))
		    (loop :for i :below section-count
		       :do (setf (aref ranges i) (loop :for n :below (1+ max-range) :collect nil))
			 (loop :for x :below address-length :do (setf (nth (- max-range (second (nth x idims)))
									   (aref ranges i))
								      ;; flip the indices to set
								      (nth x (aref divisions i)))))
		    ranges)))))
|#
