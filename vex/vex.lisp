;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Vex -*-
;;;; vex.lisp

(in-package #:vex)

"A framework for building vector languages; its current use case is the implementation of the April dialect of APL."

;; The idiom object defines a vector language instance with glyph lexicons and a set of processing utilities.
(defclass idiom ()
  ((name :accessor idiom-name
         :initarg :name)
   (system :accessor idiom-system
           :initform nil
           :initarg :system)
   (symbols :accessor idiom-symbols
            :initform nil
            :initarg :symbols)
   (utilities :accessor idiom-utilities
              :initform nil
              :initarg :utilities)
   (lexicons :accessor idiom-lexicons
             :initform nil
             :initarg :lexicons)))

(defmacro local-idiom (symbol)
  "Shorthand macro to output the name of a Vex idiom in the local package."
  (let ((sym (intern (format nil "*~a-IDIOM*" (string-upcase symbol))
                     (string-upcase symbol))))
    (if (boundp sym)
        sym `(progn (defvar ,sym) ,sym))))

(defgeneric get-system-meta (idiom property))
(defmethod get-system-meta ((idiom idiom) property)
  "Retrieve a property of the idiom's system."
  (getf (idiom-system idiom) property))

(defgeneric set-system-meta (idiom &rest pairs))
(defmethod set-system-meta ((idiom idiom) &rest pairs)
  "Set a property of the idiom's system."
  (loop :for (key value) :on pairs :by #'cddr
        :do (setf (getf (idiom-system idiom) key) value)))

(defgeneric of-utilities (idiom utility))
(defmethod of-utilities ((idiom idiom) utility)
  "Retrieve one of the idiom's utilities used for parsing and language processing."
  (getf (idiom-utilities idiom) utility))

(defgeneric of-lexicons (idiom glyph &rest lexicons))
(defmethod of-lexicons ((idiom idiom) glyph &rest lexicons)
  "Check whether a character belongs to a given Vex lexicon."
  (loop :for lexicon :in lexicons :always (member glyph (getf (idiom-lexicons idiom) lexicon))))

(defmacro boolean-op (operation)
  "Wrap a boolean operation for use in a vector language, converting the t or nil it returns to 1 or 0."
  (let ((omega (gensym)) (alpha (gensym)) (outcome (gensym)))
    `(lambda (,omega &optional ,alpha)
       (let ((,outcome (funcall ,(if (symbolp operation) `(function ,operation)
                                     (macroexpand operation))
                                ,alpha ,omega)))
         (if ,outcome 1 0)))))

(defmacro reverse-op (is-dyadic &optional operation)
  "Wrap a function so as to reverse the arguments passed to it and (- 5 10) will thus result in 5."
  (let ((is-dyadic (when operation is-dyadic))
        (operation (if operation operation is-dyadic))
        (omega (gensym)) (alpha (gensym)))
    `(lambda (,omega &optional ,alpha)
       ,(if is-dyadic `(funcall (function ,operation) ,alpha ,omega)
            `(if ,alpha (funcall (function ,operation) ,alpha ,omega)
                 (funcall (function ,operation) ,omega))))))

(defun count-symbol-in-spec (symbol limbs)
  "Count occurrences of a symbol in the spec. Used to plan the number of Prove tests to run."
  (let ((results 0))
    (loop :for limb :in limbs :do (if (listp limb)
                                      (incf results (count-symbol-in-spec symbol limb))
                                      (when (and (symbolp limb) (eql symbol limb))
                                        (incf results))))
    results))

(defun process-lex-tests-for (symbol operator &key (mode :test))
  "Process a set of tests for Vex functions or operators."
  (let* ((tests (rest (assoc (intern "TESTS" (package-name *package*))
                             (rest operator))))
         (props (rest (assoc (intern "HAS" (package-name *package*))
                             (rest operator))))
         (heading (format nil "[~a] ~a~a~%" (first operator)
                          (if (getf props :title)
                              (getf props :title)
                              (when (getf props :titles)
                                (first (getf props :titles))))
                          (if (not (getf props :titles))
                              "" (concatenate 'string " / " (second (getf props :titles)))))))
    (labels ((for-tests (tests &optional output)
               (if tests (for-tests (rest tests)
                                    (append output (unless (eq :time mode)
                                                     `((format t "  _ ~a" ,(cadr (first tests)))))
                                            (cond ((and (eq :test mode)
                                                        (eql 'is (caar tests)))
                                                   `((is (,(intern (string-upcase symbol)
                                                                   (package-name *package*))
                                                           ,(cadar tests))
                                                         ,(third (first tests))
                                                         :test #'equalp)))
                                                  ((and (eq :time mode)
                                                        (eql 'is (caar tests)))
                                                   `((,(intern (string-upcase symbol)
                                                               (package-name *package*))
                                                       ,(cadar tests))))
                                                  ((and (eq :demo mode)
                                                        (eql 'is (caar tests)))
                                                   `((princ #\Newline)
                                                     (let ((output
                                                            (,(intern (string-upcase symbol)
                                                                      (package-name *package*))
                                                              (with (:state :output-printed :only))
                                                              ,(cadar tests))))
                                                       (princ (concatenate
                                                               'string "    "
                                                               (regex-replace-all
                                                                "[\\n]" output
                                                                ,(concatenate 'string '(#\Newline) "    "))))
                                                       (when (or (zerop (length output))
                                                                 (not (char= #\Newline
                                                                             (aref output (1- (length
                                                                                               output))))))
                                                         (princ #\Newline)))
                                                     ,@(when (rest tests)
                                                         `((princ #\Newline))))))))
                   output)))
      (if tests (append (unless (eq :time mode) `((format t "~%~a" ,heading)))
                        (for-tests tests))))))

;; TODO: this is also April-specific, move it into spec
(defun process-general-tests-for (symbol test-set &key (mode :test))
  "Process specs for general tests not associated with a specific function or operator."
  (append (unless (eq :time mode)
            `((princ ,(format nil "~%~a~a" (cond ((string= "FOR" (string-upcase (first test-set)))
                                                  "⍎ ")
                                                 ((string= "FOR-PRINTED" (string-upcase (first test-set)))
                                                  (if (eq :test mode) "⎕ Printed: " "⎕ ")))
                              (second test-set)))
              (format t "~%  _ ~a~%" ,(third test-set))))
          (list (cond ((and (eq :test mode)
                            (string= "FOR" (string-upcase (first test-set))))
                       `(is (,(intern (string-upcase symbol) (package-name *package*))
                              ,(third test-set))
                            ,(fourth test-set)
                            :test #'equalp))
                      ((and (eq :time mode)
                            (string= "FOR" (string-upcase (first test-set))))
                       `(,(intern (string-upcase symbol) (package-name *package*))
                          ,(third test-set)))
                      ((and (eq :demo mode)
                            (string= "FOR" (string-upcase (first test-set))))
                       `(let ((output (,(intern (string-upcase symbol) (package-name *package*))
                                        (with (:state :output-printed :only))
                                        ,(third test-set))))
                          (princ (concatenate 'string "    "
                                              (regex-replace-all "[\\n]" output
                                                                 ,(concatenate 'string '(#\Newline)  "    "))))
                          (when (or (zerop (length output))
                                    (not (char= #\Newline (aref output (1- (length output))))))
                            (princ #\Newline))))
                      ((and (eq :test mode)
                            (string= "FOR-PRINTED" (string-upcase (first test-set))))
                       `(is (,(intern (string-upcase symbol) (package-name *package*))
                              (with (:state :output-printed :only))
                              ,(third test-set))
                            ,(fourth test-set)
                            :test #'string=))
                      ((and (eq :time mode)
                            (string= "FOR-PRINTED" (string-upcase (first test-set))))
                       `(,(intern (string-upcase symbol) (package-name *package*))
                          (with (:state :output-printed :only))
                          ,(third test-set)))
                      ((and (eq :demo mode)
                            (string= "FOR-PRINTED" (string-upcase (first test-set))))
                       `(let ((output (,(intern (string-upcase symbol) (package-name *package*))
                                        (with (:state :output-printed :only))
                                        ,(third test-set))))
                          (princ (concatenate 'string "    "
                                              (regex-replace-all "[\\n]" output
                                                                 ,(concatenate 'string '(#\Newline)  "    "))))
                          (when (or (zerop (length output))
                                    (not (char= #\Newline (aref output (1- (length output))))))
                            (princ #\Newline))))))))

(defun process-arbitrary-tests-for (symbol test-set &key (mode :test))
  "Process arbitrary tests within a spec containing expressions that are evaluated without being wrapped in an (april ...) form."
  (declare (ignore symbol mode))
  (loop :for test :in test-set :append (append '((format t "~%"))
                                               (list test))))

(defmacro specify-vex-idiom (symbol &rest subspecs)
  "Wraps the idiom-spec macro for an initial specification of a Vex idiom."
  `(vex-idiom-spec ,symbol nil ,@subspecs))

(defmacro extend-vex-idiom (symbol &rest subspecs)
  "Wraps the idiom-spec macro for an extension of a Vex idiom."
  `(vex-idiom-spec ,symbol t ,@subspecs))

(defun merge-options (source target)
  "Merge options from multiple Vex specifiction sections into a single set."
  (let ((output (loop :for section :in target
                   :collect (let ((source-items (rest (assoc (first section) source)))
                                  (pair-index 0)
                                  ;; the osection values are copied from (rest section), otherwise it's
                                  ;; effectively a pass-by-reference and changing osection will change section
                                  (osection (loop :for item :in (rest section) :collect item)))
                              (loop :for (item-name item-value) :on source-items :while item-value
                                    :do (when (evenp pair-index)
                                          (setf (getf osection item-name) item-value))
                                        (incf pair-index))
                              (cons (first section)
                                    osection)))))
    (loop :for section :in source :when (not (assoc (first section) target))
       :do (push section output))
    output))

(defun build-profile (symbol spec mode section-names)
  "Build a documentation or test profile from a set of section names in a Vex idiom specification."
  (let ((specs (loop :for subspec :in spec
                  :when (or (string= "FUNCTIONS" (string-upcase (first subspec)))
                            (string= "OPERATORS" (string-upcase (first subspec)))
                            (string= "STATEMENTS" (string-upcase (first subspec)))
                            (string= "ARBITRARY-TEST-SET" (string-upcase (first subspec)))
                            (string= "TEST-SET" (string-upcase (first subspec))))
                  :collect subspec)))
    (loop :for name :in section-names
       :append (let* ((subspec (find name specs :test (lambda (id form)
                                                        (eq id (second (assoc :name (rest (second form)))))))))
                 (append (cond ((eq :demo mode)
                                `((format t "~%~%∘○( ~a~%  ( ~a~%"
                                          ,(getf (rest (assoc :demo-profile (cdadr subspec)))
                                                 :title)
                                          ,(getf (rest (assoc :demo-profile (cdadr subspec)))
                                                 :description))))
                               ((eq :test mode)
                                `((format t "~%~%∘○( ~a )○∘~%"
                                          ,(getf (rest (assoc :tests-profile (cdadr subspec)))
                                                 :title)))))
                         (loop :for test-set :in (cddr subspec)
                            :append (funcall (cond ((or (string= "FUNCTIONS" (string-upcase (first subspec)))
                                                        (string= "OPERATORS" (string-upcase (first subspec)))
                                                        (string= "STATEMENTS" (string-upcase (first subspec))))
                                                    #'process-lex-tests-for)
                                                   ((string= "TEST-SET" (string-upcase (first subspec)))
                                                    #'process-general-tests-for)
                                                   ((string= "ARBITRARY-TEST-SET"
                                                             (string-upcase (first subspec)))
                                                    #'process-arbitrary-tests-for))
                                             symbol test-set :mode mode)))))))

(defmacro vex-idiom-spec (symbol extension &rest subspecs)
  "Process the specification for a vector language and build functions that generate the code tree."
  (macrolet ((of-subspec (symbol-string)
               `(rest (assoc ',symbol-string subspecs :test (lambda (x y) (string= (string-upcase x)
                                                                                   (string-upcase y)))))))
    (let* ((symbol-string (string-upcase symbol))
           (idiom-symbol (intern (format nil "*~a-IDIOM*" symbol-string)
                                 (symbol-package symbol)))
           (demo-forms (build-profile symbol subspecs :demo (rest (assoc :demo (of-subspec profiles)))))
           (test-forms (build-profile symbol subspecs :test (rest (assoc :test (of-subspec profiles)))))
           (timed-forms (build-profile symbol subspecs :time (rest (assoc :time (of-subspec profiles)))))
           (atest-forms (build-profile symbol subspecs :test
                                       (rest (assoc :arbitrary-test (of-subspec profiles)))))
           ;; note: the pattern specs are processed and appended in reverse order so that their ordering in the
           ;; spec is intuitive, with more specific pattern sets such as optimization templates being included
           ;; after less specific ones like the baseline grammar
           (idiom-definition `(make-instance 'idiom :name ,(intern symbol-string "KEYWORD")))
           (printout-sym (concatenate 'string symbol-string "-F"))
           (inline-sym (concatenate 'string symbol-string "-C"))
           (options (gensym)) (input-string (gensym)) (body (gensym)) (args (gensym))
           (input-path (gensym)) (process (gensym)) (form (gensym)) (item (gensym)) (pathname (gensym))
           (ws-name (gensym)) (ws-fullname (gensym)))
      (multiple-value-bind (idiom-list assignment-form idiom-data)
          (funcall (second (getf (of-subspec utilities) :process-fn-op-specs))
                   (reverse (loop :for subspec :in subspecs
                                  :when (or (string= "FUNCTIONS" (string-upcase (first subspec)))
                                            (string= "OPERATORS" (string-upcase (first subspec)))
                                            (string= "STATEMENTS" (string-upcase (first subspec))))
                                    :collect subspec)))
        `(progn ,@(unless extension `((proclaim '(special ,idiom-symbol))
                                      (setf (symbol-value (quote ,idiom-symbol)) ,idiom-definition)))
                ,@assignment-form
                (setf (idiom-system ,idiom-symbol)
                      (append (idiom-system ,idiom-symbol)
                              ,(cons 'list (of-subspec system)))
                      (idiom-utilities ,idiom-symbol)
                      (append (idiom-utilities ,idiom-symbol)
                              ,(cons 'list (of-subspec utilities)))
                      (idiom-symbols ,idiom-symbol)
                      (append (idiom-symbols ,idiom-symbol)
                              ,(list 'quote (of-subspec symbols)))
                      (idiom-lexicons ,idiom-symbol)
                      (quote ,idiom-list))
                ,@(if (not extension)
                      `((defmacro ,(intern symbol-string (symbol-package symbol))
                            (,options &optional ,input-string)
                          ;; this macro is the point of contact between users and the language, used to
                          ;; evaluate expressions and control properties of the language instance
                          (cond ((and ,options (listp ,options)
                                      (string= "TEST" (string-upcase (first ,options))))
                                 ;; the (test) setting is used to run tests
                                 `(progn (setq prove:*enable-colors* nil)
                                         (plan ,(+ (loop :for exp :in ',test-forms
                                                         :counting (eql 'is (first exp)))
                                                   (count-symbol-in-spec 'prove:is ',atest-forms)))
                                         (,',(intern (concatenate 'string "WITH-" symbol-string "-CONTEXT")
                                                     (symbol-package symbol))
                                          ,,(getf (of-subspec utilities) :test-parameters)
                                          ,@',test-forms ,@',atest-forms (finalize))
                                         (setq prove:*enable-colors* t)))
                                ((and ,options (listp ,options)
                                      (string= "TIME-TESTS" (string-upcase (first ,options))))
                                 `(progn (time (progn ,@',timed-forms))
                                         ,(format nil "Timed evaluation of ~d tests." (length ',timed-forms))))
                                ((and ,options (listp ,options)
                                      (string= "DEMO" (string-upcase (first ,options))))
                                 ;; the (demo) setting is used to print demos of the language
                                 `(progn ,@',demo-forms "Demos complete!"))
                                (t (if (or (and ,input-string (or (stringp ,input-string)
                                                                  (listp ,input-string)))
                                           (and (not ,input-string)
                                                (stringp ,options)))
                                       (vex-program ,idiom-symbol
                                                    (when ,input-string
                                                      (if (or (string= "WITH" (string (first ,options)))
                                                              (string= "SET" (string (first ,options))))
                                                          (rest ,options)
                                                          (error "Incorrect option syntax.")))
                                                    (if ,input-string ,input-string ,options))
                                       ;; this clause results in compilation at runtime of an
                                       ;; evaluated string value
                                       `(eval (vex-program
                                               ,',idiom-symbol
                                               ,(when ,input-string
                                                  (if (or (string= "WITH" (string (first ,options)))
                                                          (string= "SET" (string (first ,options))))
                                                      `(quote ,(rest ,options))
                                                      (error "Incorrect option syntax.")))
                                               ,(if ,input-string ,input-string ,options)))))))
                        (defmacro ,(intern printout-sym (symbol-package symbol))
                            (&rest ,options)
                          ;; an alternate evaluation macro that prints formatted evaluation results
                          ;; as well as returning them
                          (cons ',(intern symbol-string (symbol-package symbol))
                                (append (if (second ,options)
                                            (list (cons (caar ,options)
                                                        (merge-options `((:state :print t))
                                                                       (cdar ,options))))
                                            `((with (:state :print t))))
                                        (last ,options))))
                        (defmacro ,(intern inline-sym (symbol-package symbol))
                            (,options &rest ,args)
                          ;; an alternate evaluation macro that calls a function on arguments passed inline;
                          ;; makes for more compact invocations of the language
                          ;; TODO: can this be made to work with code passed in string-referencing variables?
                          (let ((,args (if (stringp ,options) ,args (rest ,args)))
                                (,input-string (when (listp ,options) (first ,args))))
                            (apply #'vex-program ,idiom-symbol
                                   (when ,input-string
                                     (if (or (string= "WITH" (string (first ,options)))
                                             (string= "SET" (string (first ,options))))
                                         (rest ,options)
                                         (error "Incorrect option syntax.")))
                                   (if ,input-string ,input-string ,options)
                                   ,args)))
                        (defmacro ,(intern (concatenate 'string symbol-string "-LOAD")
                                           (symbol-package symbol))
                            (,options &optional ,input-path)
                          ;; an evaluation macro that loads code from a file,
                          ;; evaluating the path expression
                          `(progn ,(let ((,pathname (if ,input-path (eval ,input-path)
                                                        (eval ,options))))
                                     (if (pathnamep ,pathname)
                                         (vex-program ,idiom-symbol
                                                      (when ,input-path
                                                        (if (or (string= "WITH" (string (first ,options)))
                                                                (string= "SET" (string (first ,options))))
                                                            (rest ,options)
                                                            (error "Incorrect option syntax.")))
                                                      ,pathname)
                                         (error "Argument to be loaded was not a pathname.")))))
                        (defmacro ,(intern (concatenate 'string "WITH-" symbol-string "-CONTEXT")
                                           (symbol-package symbol))
                            (,options &rest ,body)
                          ;; this macro creates a context enclosure within which evaluations have a default
                          ;; context; use this to evaluate many times with the same (with) expression
                          (labels ((,process (,form)
                                     (loop :for ,item :in ,form
                                           :collect (if (and (listp ,item)
                                                             (or (eql ',(intern symbol-string
                                                                                (symbol-package symbol))
                                                                      (first ,item))
                                                                 (eql ',(intern (concatenate 'string
                                                                                             symbol-string
                                                                                             "-LOAD")
                                                                                (symbol-package symbol))
                                                                      (first ,item))
                                                                 (eql ',(intern printout-sym
                                                                                (symbol-package symbol))
                                                                      (first ,item))))
                                                        (list (first ,item)
                                                              (if (third ,item)
                                                                  (cons (caadr ,item)
                                                                        (merge-options (cdadr ,item)
                                                                                       ,options))
                                                                  (cons 'with ,options))
                                                              (first (last ,item)))
                                                        (if (not (listp ,item))
                                                            ,item (,process ,item))))))
                            (cons 'progn (,process ,body))))
                        (defmacro ,(intern (concatenate 'string symbol-string "-CREATE-WORKSPACE")
                                           (symbol-package symbol))
                            (,ws-name)
                          ;; this macro creates a context enclosure within which evaluations have a default
                          ;; context; use this to evaluate many times with the same (with) expression
                          (let ((,ws-fullname (concatenate 'string ,(string-upcase symbol)
                                                           "-WORKSPACE-" (string-upcase ,ws-name))))
                            (if (string= "-LEX" (subseq (string ,ws-name) (- (length (string ,ws-name)) 4)))
                                (error "Workspace names may not end with \"-LEX\", this suffix is reserved.")
                                `(progn
                                   (if (find-package ,,ws-fullname)
                                       (format nil "A workspace called ｢~a｣ already exists." ',,ws-name)
                                       (progn (make-package ,,ws-fullname)
                                              (make-package ,(concatenate 'string ,ws-fullname "-LEX"))))
                                   (proclaim (list 'special (intern "*SYSTEM*" ,,ws-fullname)
                                                   (intern "*BRANCHES*" ,,ws-fullname)
                                                   (intern "*NS-POINT*" ,,ws-fullname)
                                                   ,@(loop :for (key val)
                                                             :on ,(getf (of-subspec system) :variables)
                                                           :by #'cddr
                                                           :collect `(intern ,(string-upcase val)
                                                                             ,,ws-fullname))))
                                   (unless (boundp (intern "*SYSTEM*" ,,ws-fullname))
                                     (set (intern "*SYSTEM*" ,,ws-fullname)
                                          ,',(cons 'list (of-subspec system)))
                                     ;; TODO: following is APL-specific, move into spec
                                     (set (intern "*BRANCHES*" ,,ws-fullname) nil)
                                     (set (intern "*NS-POINT*" ,,ws-fullname) nil)
                                     ,@(loop :for (key val)
                                               :on ,(getf (of-subspec system) :variables) :by #'cddr
                                             :collect `(set (intern ,(string-upcase val) ,,ws-fullname)
                                                            ,(getf ',(second (getf (of-subspec system)
                                                                                   :workspace-defaults))
                                                                   key)))
                                     (format nil "Successfully created workspace ｢~a｣." ',,ws-name))))))
                        (defmacro ,(intern (concatenate 'string symbol-string "-CLEAR-WORKSPACE")
                                           (symbol-package symbol))
                            (,ws-name)
                          ;; this macro creates a context enclosure within which evaluations have a default
                          ;; context; use this to evaluate many times with the same (with) expression
                          (let ((,ws-fullname (concatenate 'string ,(string-upcase symbol)
                                                           "-WORKSPACE-" (string-upcase ,ws-name))))
                            `(if (find-package ,,ws-fullname)
                                 (progn (delete-package ,,ws-fullname)
                                        (,',(intern (concatenate 'string symbol-string "-CREATE-WORKSPACE")
                                                    (symbol-package symbol))
                                         ,,ws-name)
                                        ,(format nil "The workspace ｢~a｣ has been cleared." ,ws-name))
                                 (progn (,',(intern (concatenate 'string symbol-string "-CREATE-WORKSPACE")
                                                    (symbol-package symbol))
                                         ,,ws-name)
                                        ,(format nil "No workspace called ｢~a｣ was found to clear; ~a"
                                                 ,ws-name "the workspace has been created..")))))
                        (defun ,(intern (concatenate 'string symbol-string "-REF")
                                        (symbol-package symbol))
                            (,ws-name &optional ,form)
                          ;; this macro creates a context enclosure within which evaluations have a default
                          ;; context; use this to evaluate many times with the same (with) expression
                          (let* ((,ws-fullname (if ,form (concatenate 'string ,(string-upcase symbol)
                                                                      "-WORKSPACE-" (string ,ws-name))
                                                   (concatenate 'string ,(string-upcase symbol)
                                                                "-WORKSPACE-COMMON")))
                                 (,form (or ,form ,ws-name))
                                 (,item (intern (string ,form) (string ,ws-fullname))))
                            (if (fboundp ,item) (symbol-function ,item)
                                (if (boundp ,item) (symbol-value ,item)
                                    (error "The symbol ｢~a｣ is not bound in workspace ｢~a｣."
                                           ,item ,ws-name)))))))
                ;; print a summary of the idiom as it was specified or extended
                (let ((items 0)
                      (set-index 0)
                      (output "")
                      (sets (list (list "basic grammar element"
                                        ,(if (not (assoc :elements (of-subspec grammar)))
                                             0 (* 1/2 (length (cadadr (assoc :elements
                                                                             (of-subspec grammar)))))))
                                  (list "opening grammar pattern"
                                        (+ ,@(loop :for pset :in (reverse (rest (assoc :opening-patterns
                                                                                       (of-subspec grammar))))
                                                   :collect `(length ,pset))))
                                  (list "following grammar pattern"
                                        (+ ,@(loop :for pset :in (reverse (rest (assoc :following-patterns
                                                                                       (of-subspec grammar))))
                                                   :collect `(length ,pset))))
                                  (list "lexical function" ,(getf idiom-data :fn-count))
                                  (list "lexical operator" ,(getf idiom-data :op-count))
                                  (list "utility function" ,(* 1/2 (length (of-subspec utilities))))
                                  (list "unit test" ,(+ (loop :for exp :in test-forms
                                                              :counting (eql 'is (first exp)))
                                                        (count-symbol-in-spec 'prove:is atest-forms))))))
                  (loop :for set-values :in sets
                        :do (destructuring-bind (set-name set) set-values
                              (setq output
                                    (if (zerop set) output
                                        (format nil "~a~a~a ~a~a"
                                                output (if (and (< 0 items)
                                                                (or (= set-index (1- (length sets)))
                                                                    (zerop (loop :for sx :from (1+ set-index)
                                                                                   :to (1- (length sets))
                                                                                 :summing (second
                                                                                           (nth sx sets))))))
                                                           " and " (if (< 0 items) ", " ""))
                                                set set-name (if (< 1 set) "s" "")))
                                    set-index (1+ set-index)
                                    items (+ set items))))
                  (format t "~%~a idiom ｢~a｣ with ~a.~%~%" ,(if extension "Extended" "Specified")
                          ,(string-upcase symbol)
                          output))
                ,(format nil "Idiom ~a complete." (if extension "extension" "specification")))))))

(defun derive-opglyphs (glyph-list &optional output)
  "Extract a list of function/operator glyphs from part of a Vex language specification."
  (if (not glyph-list)
      output (derive-opglyphs (rest glyph-list)
                              (let ((glyph (first glyph-list)))
                                (if (characterp glyph)
                                    (cons glyph output)
                                    (when (stringp glyph)
                                      (append output (loop :for char :below (length glyph)
                                                           :collect (aref glyph char)))))))))

(defun =vex-string (idiom &optional output special-precedent)
  "Parse a string of text, converting its contents into nested lists of Vex tokens."
  (let ((string-found) (olnchar) (symbols) (is-function-closure)
        ;; the olnchar variable is needed to handle characters that may be functional or part
        ;; of a number based on their context; in APL it's the . character, which may begin a number like .5
        ;; or may work as the inner/outer product operator, as in 1 2 3+.×4 5 6.
        (uniform-char) (arg-rooted-path) (fix 0))
    (labels ((?blank-character   () (?satisfies (of-utilities idiom :match-blank-character)))
             (?newline-character () (?satisfies (of-utilities idiom :match-newline-character)))
             (?numeric-character () (?satisfies (of-utilities idiom :match-numeric-character)))
             (?token-character   () (?satisfies (of-utilities idiom :match-token-character)))
             (numeric-string-p (item) (funcall (of-utilities idiom :format-number) item))
             (pjoin-char-p     (item) (funcall (of-utilities idiom :match-path-joining-character) item))
             (utoken-p         (item) (funcall (of-utilities idiom :match-uniform-token-character) item))
             (p-or-u-char-p (is-path uniform-char)
               (if is-path (lambda (item) (funcall (of-utilities idiom :match-token-character) item))
                   (lambda (item) (char= uniform-char item))))
             (=string (&rest delimiters)
               (let ((lastc) (delimiter) (escape-indices) (char-index 0))
                 (=destructure (_ content)
                     (=list (?satisfies (lambda (c) (when (member c delimiters)
                                                      (setq delimiter c))))
                            ;; note: nested quotes must be checked backwards; to determine whether a delimiter
                            ;; indicates the end of the quote, look at previous character to see whether it is a
                            ;; delimiter, then check whether the current character is an escape character #\\
                            (=subseq (%any (?satisfies (lambda (char)
                                                         (when (or (not lastc)
                                                                   (not (char= lastc delimiter))
                                                                   (char= char delimiter))
                                                           (setq lastc (if (and lastc (char= char delimiter)
                                                                                (char= lastc delimiter))
                                                                           (progn (push (1- char-index)
                                                                                        escape-indices)
                                                                                  #\ )
                                                                           char)
                                                                 char-index (1+ char-index))))))))
                   (setq string-found t)
                   ;; the string-found variable is set to true
                   ;; TODO: is there a better way to do this?
                   (when (or (not (char= delimiter (aref content (1- (length content)))))
                             (and escape-indices (= (first escape-indices)
                                                    (- (length content) 2))))
                     (error "Syntax error: unbalanced quotes."))
                   (if escape-indices (let ((offset 0)
                                            (outstr (make-array (list (- (length content)
                                                                         1 (length escape-indices)))
                                                                :element-type 'character)))
                                        (loop :for x :below (1- (length content))
                                           :when (member x escape-indices) :do (incf offset)
                                           :when (not (member x escape-indices))
                                           :do (setf (aref outstr (- x offset)) (aref content x)))
                                        outstr)
                       (if (= 2 (length content))
                           (aref content 0)
                           (if (= 1 (length content))
                               (make-array 0 :element-type 'character)
                               (make-array (1- (length content)) :element-type 'character
                                           :displaced-to content)))))))
             (=vex-closure (boundary-chars &key (transform-by) (disallow-linebreaks)
                                           (symbol-collector) (if-confirmed))
               (let* ((balance 1)
                      (char-index 0)
                      (quoted) (quote-delimiter #\')
                      ;; disallow linebreak overriding opening and closing characters
                      (dllen (when (stringp disallow-linebreaks) (length disallow-linebreaks)))
                      (dlbor-opening-chars (when dllen (subseq disallow-linebreaks 0 (/ dllen 2))))
                      (dlbor-closing-chars (when dllen (subseq disallow-linebreaks (/ dllen 2) dllen)))
                      (dlb-overriding-balance 0))
                 (=destructure (_ enclosed _)
                     (=list (?eq (aref boundary-chars 0))
                            ;; for some reason, the first character in the string is iterated over twice here,
                            ;; so the character index is checked and nothing is done for the first character
                            ;; TODO: fix this
                            (=transform (=subseq
                                         (%some (?satisfies
                                                 (lambda (char)
                                                   ;; have to do the zerop check to avoid the double-
                                                   ;; iteration mentioned above
                                                   (when (and (not (zerop char-index))
                                                              (char= char quote-delimiter))
                                                     (setf quoted (not quoted)))
                                                   (when (and disallow-linebreaks
                                                              (zerop dlb-overriding-balance)
                                                              (funcall (of-utilities
                                                                        idiom :match-newline-character)
                                                                       char))
                                                     (error "Newlines cannot occur within a ~a closure."
                                                            boundary-chars))
                                                   (unless quoted
                                                     (if (and (char= char (aref boundary-chars 0))
                                                              (< 0 char-index))
                                                         (incf balance)
                                                         (when (and (char= char (aref boundary-chars 1))
                                                                    (< 0 char-index))
                                                           (decf balance)))
                                                     (when dlbor-opening-chars
                                                       (if (and (< 0 char-index)
                                                                (not (loop :for c :across dlbor-opening-chars
                                                                           :never (char= char c))))
                                                           (incf dlb-overriding-balance)
                                                           (unless (loop :for c :across dlbor-closing-chars
                                                                         :never (char= char c))
                                                             (decf dlb-overriding-balance)))))
                                                   (incf char-index)
                                                   (< 0 balance)))))
                                        (or transform-by
                                            (lambda (string-content)
                                              (destructuring-bind (parsed remaining meta)
                                                  (parse string-content (=vex-string idiom))
                                                (declare (ignore remaining))
                                                (when symbol-collector (funcall symbol-collector meta))
                                                parsed))))
                            (?eq (aref boundary-chars 1)))
                   (if (zerop balance)
                       (progn (when if-confirmed (funcall if-confirmed))
                              enclosed)
                       (error "No closing ~a found for opening ~a."
                              (aref boundary-chars 1) (aref boundary-chars 0))))))
             (=vex-errant-axis-separating-character ()
               (let ((errant-char))
                 (=destructure (_ _)
                     (=list (?satisfies (lambda (char)
                                          (when (funcall (of-utilities
                                                          idiom :match-axis-separating-character)
                                                         char)
                                            (setq errant-char char))))
                            (=subseq (%any (?satisfies 'characterp))))
                   (error "Misplaced axis delimiter ~a." errant-char))))
             (=vex-errant-closing-character (boundary-chars)
               (let ((errant-char) (matching-char)
                     (chars-count (/ (length boundary-chars) 2)))
                 (=destructure (_ _)
                     (=list (?satisfies (lambda (char)
                                          (loop :for i :across boundary-chars
                                                :for x :from 0 :to chars-count :when (char= char i)
                                                :do (setq errant-char i
                                                          matching-char (aref boundary-chars
                                                                              (+ x chars-count))))
                                          errant-char))
                            (=subseq (%any (?satisfies 'characterp))))
                   (error "Mismatched enclosing characters; each closing ~a must be preceded by an opening ~a."
                          errant-char matching-char))))
             (process-lines (lines &optional output meta)
               (if (or (zerop (length lines))
                       (loop :for c :across lines :always (char= c #\ )))
                   (list output meta)
                   (destructuring-bind (out remaining meta)
                       (parse lines (=vex-string idiom nil meta))
                     (process-lines remaining (append output (when out (list out)))
                                    meta))))
             (handle-axes ()
               (lambda (input-string)
                 (let* ((each-axis (funcall (of-utilities idiom :process-axis-string)
                                            input-string))
                        (each-axis-code (loop :for axis :in each-axis :collect
                                                                      (let ((output (process-lines axis)))
                                                                        (first output)))))
                   (cons :ax each-axis-code))))
             (handle-function (input-string)
               (destructuring-bind (content meta) (process-lines input-string)
                 (list :fn (cons :meta meta) content)))
             (handle-symbol (string)
               (multiple-value-bind (formatted is-symbol)
                   (funcall (of-utilities idiom :format-value)
                            (string-upcase (idiom-name idiom))
                            ;; if there's an overloaded token character, do as above
                            (idiom-symbols idiom)
                            (if (getf special-precedent :overloaded-num-char)
                                (format nil "~a~a" (getf special-precedent :overloaded-num-char)
                                        string)
                                string))
                 (values formatted is-symbol)))
             (functional-character-matcher (char)
               (when (and (> 2 fix)
                          (funcall (of-utilities idiom :match-overloaded-numeric-character)
                                   char))
                 (setq olnchar char))
               (when (and olnchar (= 2 fix) (not (digit-char-p char)))
                 (setq olnchar nil))
               (incf fix 1)
               (and (not (< 2 fix))
                    (or (of-lexicons idiom char :functions)
                        (of-lexicons idiom char :operators)))))
      (=destructure (_ item _ break rest)
          (=list (%any (?blank-character))
                 (%or (=vex-closure "()" :transform-by nil :disallow-linebreaks "{}")
                      (=vex-closure "[]" :transform-by (handle-axes))
                      (=vex-closure "{}" :transform-by #'handle-function
                                         :if-confirmed (lambda () (setq is-function-closure t)))
                      (=vex-errant-axis-separating-character)
                      (=vex-errant-closing-character ")]}([{")
                      (=string #\' #\")
                      (=transform (=subseq (%some (?satisfies #'functional-character-matcher)))
                                  (lambda (string)
                                    (let ((char (character string)))
                                      (if (not olnchar)
                                          (append (list (if (of-lexicons idiom char :statements)
                                                            :st (if (of-lexicons idiom char :operators)
                                                                    :op (when (of-lexicons idiom char
                                                                                           :functions)
                                                                          :fn))))
                                                  (if (of-lexicons idiom char :operators)
                                                      (list (if (of-lexicons idiom char :operators-pivotal)
                                                                :pivotal
                                                                (if (of-lexicons idiom char
                                                                                 :operators-lateral)
                                                                    :lateral :unitary))))
                                                  (list char))))))
                      (=transform (%and (?test (#'numeric-string-p)
                                            (=subseq (%some (?numeric-character))))
                                        (=subseq (%some (?numeric-character))))
                                  (lambda (string)
                                    (funcall (of-utilities idiom :format-number)
                                             ;; if there's an overloaded token character passed in
                                             ;; the special precedent, prepend it to the token
                                             ;; being processed
                                             (if (getf special-precedent :overloaded-num-char)
                                                 (format nil "~a~a" (getf special-precedent
                                                                          :overloaded-num-char)
                                                         string)
                                                 string))))
                      ;; matches symbols like APL's ⍺, ∇∇, and ⍵⍵ that must be homogenous - however,
                      ;; homogenous symbols on the list of argument symbols may be part of a larger
                      ;; symbol that references a namespace path like ⍵.path.to
                      (=transform (=subseq (?seq (=transform (=subseq (?test (#'utoken-p) (=element)))
                                                             (lambda (c)
                                                               (if c (setq uniform-char (aref c 0)))))
                                                 (=transform
                                                  (=subseq (%any (?test (#'pjoin-char-p) (=element))))
                                                  (lambda (c)
                                                    (when (< 0 (length c)) (setq arg-rooted-path t))))
                                                 (=subseq (%any (?test ((p-or-u-char-p
                                                                         arg-rooted-path
                                                                         uniform-char)))))))
                                  (lambda (string)
                                    (multiple-value-bind (formatted is-symbol) (handle-symbol string)
                                      (when is-symbol (push formatted symbols))
                                      formatted)))
                      (=transform (=subseq (%some (?token-character)))
                                  (lambda (string)
                                    (multiple-value-bind (formatted is-symbol) (handle-symbol string)
                                      (when is-symbol (push formatted symbols))
                                      formatted)))
                      ;; this last clause returns the remainder of the input in case the input has either no
                      ;; characters or only blank characters before the first line break
                      (=subseq (%any (?satisfies 'characterp))))
                 (%any (?blank-character))
                 (=subseq (%any (?newline-character)))
                 (=subseq (%any (?satisfies 'characterp))))
        (if (and (not output) (stringp item) (< 0 (length item))
                 (funcall (of-utilities idiom :match-newline-character)
                          (aref item 0)))
            ;; if the string is passed back (minus any leading whitespace) because the string began with
            ;; a line break, parse again omitting the line break character
            (parse (subseq item 1) (=vex-string idiom nil special-precedent))
            (if (and (zerop (length break)) (< 0 (length rest)))
                (parse rest (=vex-string idiom (if output (if (not item) output (cons item output))
                                                   (when item (list item)))
                                         (append (when olnchar (list :overloaded-num-char olnchar))
                                                 (list :symbols nil))))
                (list (if (or (not item)
                              (and (typep item 'sequence)
                                   (zerop (length item)) (not string-found)))
                          ;; return nothing if only an empty sequence results from parsing
                          ;; unless an explicit empty string was parsed
                          output (cons item output))
                      rest special-precedent)))))))

(defun vex-program (idiom options &optional string &rest inline-arguments)
  "Compile a set of expressions, optionally drawing external variables into the program and setting configuration parameters for the system."
  (let* ((state (rest (assoc :state options)))
         (print-tokens (assoc :print-tokens options))
         (space (concatenate 'string (string-upcase (idiom-name idiom))
                             "-WORKSPACE-" (if (not (second (assoc :space options)))
                                               "COMMON" (string-upcase (second (assoc :space options))))))
         (state-to-use) (system-to-use))

    (labels ((assign-from (source dest)
               (if (not source)
                   dest (progn (setf (getf dest (first source)) (second source))
                               (assign-from (cddr source) dest))))
             (validate-var-symbol (symbol)
               (let ((string-sym (if (stringp symbol) symbol (lisp->camel-case symbol))))
                 (loop :for c :across string-sym
                    :always (funcall (of-utilities idiom :match-token-character) c))))
             (process-lines (string &optional space params output)
               (if (zerop (length string))
                   (funcall (of-utilities idiom :compile-form)
                            (reverse output) :space space :params params)
                   (let ((result (funcall (of-utilities idiom :lexer-postprocess)
                                          (parse string (=vex-string idiom))
                                          idiom space)))
                     (when print-tokens (print (first result)))
                     (process-lines (second result) space params (cons (first result) output)))))
             (get-item-refs (items-to-store &optional storing-functions)
               ;; Function or variable names passed as a string may be assigned literally as long as there are
               ;; no dashes present in them, so the variable name "iD" becomes iD within the idiom, whereas a
               ;; variable named |iD| will become id within the idiom. Strings are used instead of pipe-quoting
               ;; because there's no way to tell the difference between symbols ABC and |ABC| after they
               ;; pass the reader and the uppercase symbol names are converted to lowercase by default.
               (loop :for item :in items-to-store
                  :collect (list (if storing-functions (of-utilities idiom :assign-fun-sym)
                                     (of-utilities idiom :assign-val-sym))
                                 (if (validate-var-symbol (first item))
                                     (let ((symbol (if (and (stringp (first item))
                                                            (loop :for c :across (first item)
                                                               :never (char= #\- c)))
                                                       (string (first item))
                                                       (lisp->camel-case (first item)))))
                                       ;; if functions or variables are to be stored in their workspace,
                                       ;; initialize their values so compilation will proceed correctly
                                       (funcall (of-utilities idiom :process-stored-symbol)
                                                symbol space storing-functions)
                                       (intern symbol))
                                     (error "Invalid characters present in symbol ~a passed to :~a."
                                            (first item) (if storing-functions :store-fun :store-val)))
                                 (second item)))))

      (symbol-macrolet ((ws-system (symbol-value (intern "*SYSTEM*" space))))

        (setq state         (funcall (of-utilities idiom :preprocess-state-input) state)
              state-to-use  (assign-from (getf ws-system :base-state) state-to-use)
              state-to-use  (assign-from (getf ws-system :state) state-to-use)
              state-to-use  (assign-from state state-to-use)
              system-to-use (assign-from ws-system system-to-use)
              system-to-use (assign-from state system-to-use))

        (if string
            (let* ((string (if (stringp string)
                               ;; just pass the string through if it's not a pathname;
                               ;; if it is a pathname, evaluate it in case something like
                               ;; (asdf:system-relative-pathname ...) was passed
                               string (with-open-file (stream (eval string))
                                        (apply #'concatenate
                                               (cons 'string (loop :for line := (read-line stream nil)
                                                                :while line
                                                                :append (list line '(#\Newline))))))))
                   (input-vars (getf state-to-use :in))
                   (output-vars (getf state-to-use :out))
                   (stored-refs (append (get-item-refs (rest (assoc :store-val options)))
                                        (get-item-refs (rest (assoc :store-fun options)) t)))
                   (system-vars (funcall (of-utilities idiom :system-lexical-environment-interface)
                                         state-to-use))
                   (vars-declared (funcall (of-utilities idiom :build-variable-declarations)
                                           input-vars space))
                   (iv-list (mapcar (lambda (return-var) (intern (lisp->camel-case (first return-var))
                                                                 (string (idiom-name idiom))))
                                    input-vars))
                   (ov-list (mapcar (lambda (return-var) (intern (lisp->camel-case return-var)
                                                                 (string (idiom-name idiom))))
                                    output-vars)))
              (funcall (of-utilities idiom :build-compiled-code)
                       (append (funcall (if output-vars #'values
                                            (apply (of-utilities idiom :postprocess-compiled)
                                                   (append (when (assoc :unrendered options)
                                                             (list :unrendered t))
                                                           system-to-use)
                                                   inline-arguments))
                                        (process-lines
                                         (funcall (of-utilities idiom :prep-code-string) string)
                                         space (list :call-scope (list :input-vars iv-list
                                                                       :output-vars ov-list))))
                               ;; if multiple values are to be output, add the (values) form at bottom
                               (when output-vars
                                 (funcall (of-utilities idiom :process-multiple-outputs)
                                          output-vars space (not (assoc :unrendered options)))))
                       (loop :for (key value) :on (getf (idiom-system idiom) :workspace-defaults)
                          :by #'cddr :collect (string-upcase key))
                       options system-vars vars-declared stored-refs space)))))))
