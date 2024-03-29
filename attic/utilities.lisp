;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; utilities.lisp

(in-package #:april)

(defun join-fns (form &optional wrap)
  "Compose multiple successive scalar functions into a larger scalar function. Used to expand (a-call)."
  (when (and (listp form) (eql 'a-call (first form)))
    (destructuring-bind (_ function &rest args) form
      (declare (ignore _))
      (if (and (listp function) (eql 'apl-fn-s (first function)))
          (if (or (and (numberp (first args)) (listp (second args))
                       (eql 'a-call (first (second args)))
                       (listp (second (second args))) (eql 'apl-fn-s (first (second (second args)))))
                  (and (numberp (second args)) (listp (first args))
                       (eql 'a-call (first (first args)))
                       (listp (second (first args))) (eql 'apl-fn-s (first (second (first args)))))
                  (and (not (second args)) (listp (first args))
                       (eql 'a-call (first (first args)))
                       (listp (second (first args))) (eql 'apl-fn-s (first (second (first args))))))
              ;; need condition for one argument
              (let ((to-wrap (lambda (fform)
                               (if (not wrap) (let ((arg-sym (gensym)))
                                                (values `(lambda (,arg-sym)
                                                           (funcall ,function ,(if (numberp (first args))
                                                                                   (first args) fform)
                                                                    ,@(when (second args)
                                                                        (list (if (numberp (second args))
                                                                                  (second args) fform)))))
                                                        arg-sym))
                                   (funcall wrap `(funcall ,function ,(if (numberp (first args))
                                                                          (first args) fform)
                                                           ,@(if (second args)
                                                                 (list (if (numberp (second args))
                                                                           (second args) fform)))))))))
                (join-fns (if (numberp (first args))
                              (second args) (first args))
                          to-wrap))
              (if (and wrap (numberp (first args)) (not (second args)))
                  (multiple-value-bind (wrapped arg-symbol)
                      (funcall wrap (list 'funcall function (if (numberp (first args))
                                                                (first args) :arg)))
                    `(apply-scalar ,(subst arg-symbol :arg wrapped) ,(first args)))
                  (let ((argument (if (numberp (first args))
                                      (if (second args) (second args) (first args))
                                      (when (numberp (second args))
                                        (first args)))))
                    (when (and argument wrap)
                      (multiple-value-bind (wrapped arg-symbol)
                          (funcall wrap (append (list 'funcall function (if (numberp (first args))
                                                                            (first args) :arg))
                                                (when (second args) (list (if (numberp (second args))
                                                                              (second args) :arg)))))
                        `(apply-scalar ,(subst arg-symbol :arg wrapped) ,argument))))))))))
