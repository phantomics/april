;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Varray -*-
;;;; plex.lisp

(in-package #:varray)

;; (defmethod plex-of ((varray vader-calculate) &optional params)) ;; performs a computation of arrays

;; (defmethod plex-of ((varray vader-index) &optional params)) ;; ⍳ finds indices of elements in array

(defmethod plex-of ((varray vader-shape) &optional params)
  "Find the shape of an array as from [⍴ shape]."
  (declare (ignore params))
  (petalisp:lazy-array-shape (vader-base varray)))

(defmethod plex-of ((varray vader-reshape) &optional params)
  "Find the shape of an array as from [⍴ shape]."
  (declare (ignore params))
  (petalisp:lazy-reshape (vader-base varray)
                         (apply #'~ (shape-of varray))))

;; note: a plex method for vader-pare is not needed since it derives from vader-reshape

;; (defmethod plex-of ((varray vader-compare) &optional params)) ;; ≡ deeply compares arrays

;; (defmethod plex-of ((varray vader-depth) &optional params)) ;; ≢ finds array depth

;; (defmethod plex-of ((varray vader-membership) &optional params)) ;; ∊ checks for membership in arrays

;; (defmethod plex-of ((varray vader-find) &optional params)) ;; ⍷ searches arrays

(defmethod plex-of ((varray vader-catenate) &optional params)
  "Catenate arrays as with [, catenate]."
  (declare (ignore params))
  (let ((axis (if (eq :last (vads-axis varray))
                  (1- (rank-of varray))
                  (vads-axis varray)))
        (base-gen (generator-of (vader-base varray))))
    (apply #'petalisp:lazy-stack axis (print (funcall base-gen 0))
           (loop :for ix :from 1 :below (first (shape-of (vader-base varray)))
                 :collect (plex-of (funcall base-gen ix))))))

;; (defmethod plex-of ((varray vader-mix) &optional params)) ;; ↑ mixes arrays

;; (defmethod plex-of ((varray vader-split) &optional params)) ;; ↓ splits arrays

;; (defmethod plex-of ((varray vader-section) &optional params)) ;; ↑↓ sections arrays

;; (defmethod plex-of ((varray vader-enclose) &optional params)) ;; ⊂ encloses arrays

;; (defmethod plex-of ((varray vader-partition) &optional params)) ;; ⊆ partitions arrays

;; (defmethod plex-of ((varray vader-expand) &optional params)) ;; /\ expands arrays

;; (defmethod plex-of ((varray vader-expand) &optional params)
;;   "Expand or compress an array as with [\ expand] or [/ compress]."
;;   (declare (ignore params))
;;   (let* ((arg (render (vads-argument varray)))
;;          (scalings (if (vectorp arg) arg (vector arg)))
;;          (transformation (petalisp:make-transformation :scalings scalings)))
;;     ;; (print (list :sc scalings))
;;     (petalisp:lazy-reshape (vader-base varray) (vader-base varray) transformation)))

(defmethod plex-of ((varray vapri-apro-vector) &optional params)
  (declare (ignore params))
  (with-accessors ((origin vapip-origin)
                   (offset vapip-offset)
                   (number vapip-number)
                   (factor vapip-factor)
                   (repeat vapip-repeat)) varray
    (let* ((start (* (+ origin offset) factor))
           (range (petalisp:range start (+ start (* factor number)) factor)))
      (petalisp:lazy-reshape
       (petalisp:lazy-index-components (~* range))
       (~* range ~ repeat)
       (petalisp:flattening-reshaper)))))

(defmethod plex-of ((varray vapri-onehot-vector) &optional params)
  (declare (ignore params))
  (destructuring-bind (length) (varray-shape varray)
    (let ((index (vaohv-index varray)))
      (petalisp:lazy-overwrite
       (petalisp:lazy-reshape 0 (~ length))
       (petalisp:lazy-reshape 1 (~ index (1+ index)))))))

(defmethod plex-of ((varray vader-turn) &optional params)
  (declare (ignore params))
  (let* ((array (petalisp:lazy-array (render (vader-base varray))))
         (amount (setf (vads-argument varray) (arg-process varray)))
         (axis (max 0 (if (eq :last (vads-axis varray))
                          (1- (rank-of varray))
                          (- (vads-axis varray)
                             (vads-io varray)))))
         (shape (petalisp:lazy-array-shape array))
         (rank (petalisp:shape-rank shape))
         (range (petalisp:shape-range shape axis))
         (start (petalisp:range-start range))
         (size (petalisp:range-size range))
         (step (petalisp:range-step range))
         (shift (if (minusp amount)
                    (* (+ size amount) step)
                    (* amount step)))
         (position (+ start shift))
         (offsets (let ((v (make-array rank :initial-element 0)))
                    (setf (aref v axis) (- shift)) v))
         (transformation (petalisp:make-transformation :offsets offsets)))
    (multiple-value-bind (lo hi)
        (petalisp:split-shape shape axis position)
      (petalisp:lazy-stack
       axis
       (petalisp:lazy-reshape array hi transformation)
       (petalisp:lazy-reshape array lo)))))
