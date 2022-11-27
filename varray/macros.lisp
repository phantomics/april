;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Varray -*-
;;;; macros.lisp

(in-package #:varray)

"Macros supporting the implementation of virtual arrays."

(defmacro intraverser (params function)
  (let ((widths '(:lindex-width (8 16 32 64)
                  :eindex-width (16 32 64)
                  :cindex-width (8 16 32)
                  :rank-width (4)))
        (default-form) (output)
        (table (gensym)) (variants (gensym)) (default (gensym)))
    (labels ((process-form (form var-widths)
               (loop :for item :in form :for ix :from 0
                     :collect (if (listp item) (process-form item var-widths)
                                  (or (loop :for (key value) :on var-widths :by #'cddr
                                            :when (eql item key) :return value)
                                      item))))
             (process-var-range (form vars &optional params key-ints var-widths)
               (if vars
                   (destructuring-bind (var-type var-symbol &rest rest-vars) vars
                     ;; (print (list :vt var-type var-symbol rest-vars (getf widths var-type)))
                     (case var-type
                       (:rank
                        (loop :for i :below (floor (getf params :base-width)
                                                   (getf params :coordinate-width))
                              :do (process-var-range form rest-vars params (cons i key-ints)
                                                     (append (list var-symbol i) var-widths))))
                       (:rank-plus
                        (loop :for i :below (floor (getf params :base-width)
                                                   (getf params :coordinate-width))
                              :do (process-var-range form rest-vars params (cons (1+ i) key-ints)
                                                     (append (list var-symbol (1+ i))
                                                             var-widths))))
                       (:address-fraction
                        ;; address fractions are traversed based on the ratio of coordinate
                        ;; width to base width, as needed for functions like ⌽⊖↑↓, where index
                        ;; transformations are applied to a specific axis
                        (loop :for i :below (floor (getf params :base-width)
                                                   (getf params :coordinate-width))
                              :do (let ((width (* i (getf params :coordinate-width))))
                                    (process-var-range form rest-vars params (cons i key-ints)
                                                       (append (list var-symbol width) var-widths)))))
                       (t
                        (loop :for width :in (or (getf widths var-type) '(0))
                              :when (or (not (member var-type '(:cindex-width)))
                                        (not (getf params :base-width))
                                        (< width (getf params :base-width)))
                                :do (let ((sub-params (copy-tree params))
                                          (sub-base-width (when (eq :sub-base-width var-type)
                                                            (1- (getf params :base-width)))))
                                      
                                      (when (member var-type '(:lindex-width :eindex-width))
                                        (setf (getf sub-params :base-width) width))

                                      (when (member var-type '(:cindex-width))
                                        (setf (getf sub-params :coordinate-width) width))
                                      ;; these variable types impose a width limit on subordinate
                                      ;; variables; i.e. the sub-byte values of an integer must
                                      ;; be half or less of that integer's width
                                      (process-var-range
                                       form (cddr vars) sub-params
                                       (if (not (member var-type '(:lindex-width :eindex-width
                                                                   :cindex-width)))
                                           key-ints (cons width key-ints))
                                       (append (list var-symbol (or sub-base-width width))
                                               var-widths)))))))
                   (progn (push (process-form form var-widths) output)
                          ;; the sub-base-width is not included in the key list
                          (push `(gethash ',(reverse key-ints) ,table)
                                output)))))
      ;; (loop :for form :in forms :do (let ((vars (first form)))
      ;;                                 (process-var-range (second form) vars)))
      (process-var-range function params)
      `(let ((,table (make-hash-table :test #'equalp)))
         (setf ,@output)
         ,table))))
