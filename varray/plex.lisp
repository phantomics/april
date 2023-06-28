;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Varray -*-
;;;; plex.lisp

(in-package #:varray)

(defmethod plex-of :around ((varray vapri-apro-vector) &optional params)
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

(defmethod plex-of :around ((varray vapri-onehot-vector) &optional params)
  (declare (ignore params))
  (destructuring-bind (length) (varray-shape varray)
    (let ((index (vaohv-index varray)))
      (petalisp:lazy-overwrite
       (petalisp:lazy-reshape 0 (~ length))
       (petalisp:lazy-reshape 1 (~ index (1+ index)))))))

(defmethod plex-of :around ((varray vader-turn) &optional params)
  (declare (ignore params))
  (let* ((array (petalisp:lazy-array (render varray)))
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
