;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Vex -*-
;;;; vex.lisp

(in-package #:vex)

"A framework for building vector languages; its current use case is the implementation of the April dialect of APL."

;; The idiom object defines a vector language instance with glyph lexicons and a set of processing utilities.
(defclass idiom ()
  ((name      :accessor idiom-name
              :initarg :name)
   (system    :accessor idiom-system
              :initform nil
              :initarg :system)
   (symbols   :accessor idiom-symbols
              :initform nil
              :initarg :symbols)
   (utilities :accessor idiom-utilities
              :initform nil
              :initarg :utilities)
   (lexicons  :accessor idiom-lexicons
              :initform nil
              :initarg :lexicons)))

(defgeneric get-system-meta (idiom property))
(defmethod get-system-meta ((idiom idiom) property)
  "Retrieve a property of the idiom's system."
  (getf (idiom-system idiom) property))

(defgeneric set-system-meta (idiom &rest pairs))
(defmethod set-system-meta ((idiom idiom) &rest pairs)
  "Set a property of the idiom's system."
  (loop :for (key value) :on pairs :by #'cddr
        :do (setf (getf (idiom-system idiom) key) value)))

(defgeneric of-system (idiom property))
(defmethod of-system ((idiom idiom) property)
  "Retrieve one of the idiom's system properties."
  (getf (idiom-system idiom) property))

(defgeneric of-utilities (idiom utility))
(defmethod of-utilities ((idiom idiom) utility)
  "Retrieve one of the idiom's utilities used for parsing and language processing."
  (getf (idiom-utilities idiom) utility))

(defgeneric of-lexicons (idiom glyph &rest lexicons))
(defmethod of-lexicons ((idiom idiom) glyph &rest lexicons)
  "Check whether a character belongs to a given Vex lexicon."
  (loop :for lexicon :in lexicons
        :always (position glyph (getf (idiom-lexicons idiom) lexicon) :test #'char=)))

(defmacro with-open-vex-file ((filename path &rest options) &body body)
  "Wraps with-open-file forms, designates the format for Vex-compatible input files as UTF-8."
  `(with-open-file (,filename ,path :external-format :utf-8 ,@options) ,@body))

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
  (let* ((tests (rest (assoc (find-symbol "TESTS" (package-name *package*))
                             (rest operator))))
         (props (rest (assoc (find-symbol "HAS" (package-name *package*))
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
  (destructuring-bind (type description expression expected) test-set
    (let ((type-sym (intern (string-upcase (first test-set)) "KEYWORD")))
      (append (unless (eq :time mode)
                `((princ ,(format nil "~%~a~a" (case type-sym (:for "⍎ ")
                                                     (:for-printed (if (eq mode :test)
                                                                       "⎕ Printed: " "⎕ ")))
                                  description))
                  (format t "~%  _ ~a~%" ,expression)))
              (let ((idiom-symbol (find-symbol (string-upcase symbol) (package-name *package*))))
                (list (case type-sym
                        (:for
                         (case mode
                           (:test `(is (,idiom-symbol ,expression) ,expected :test #'equalp))
                           (:time (list idiom-symbol expression))
                           (:demo (let ((output (gensym)))
                                    `(let ((,output (,idiom-symbol (with (:state :output-printed :only))
                                                                   ,expression)))
                                       (princ (concatenate 'string "    "
                                                           (regex-replace-all
                                                            "[\\n]" ,output
                                                            ,(concatenate 'string '(#\Newline)
                                                                          "    "))))
                                       (when (or (zerop (length ,output))
                                                 (not (char= #\Newline (aref ,output (1- (length ,output))))))
                                         (princ #\Newline)))))))
                        (:for-printed
                         (case mode
                           (:test `(is (,idiom-symbol (with (:state :output-printed :only))
                                                      ,expression)
                                       ,expected :test #'string=))
                           (:time `(,idiom-symbol (with (:state :output-printed :only))
                                                  ,expression))
                           (:demo (let ((output (gensym)))
                                    `(let ((,output (,idiom-symbol (with (:state :output-printed :only))
                                                                   ,expression)))
                                       (princ (concatenate 'string "    "
                                                           (regex-replace-all
                                                            "[\\n]" ,output
                                                            ,(concatenate 'string '(#\Newline)
                                                                          "    "))))
                                       (when (or (zerop (length ,output))
                                                 (not (char= #\Newline (aref ,output (1- (length ,output))))))
                                         (princ #\Newline))))))))))))))
  
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
                   :collect (let ((pair-index 0)
                                  (source-items (rest (assoc (first section) source)))
                                  ;; the osection values are copied from (rest section), otherwise it's
                                  ;; effectively a pass-by-reference and changing osection will change section
                                  (osection (loop :for item :in (rest section) :collect item)))
                              (loop :for (item-name item-value) :on source-items :while item-value
                                    :do (when (evenp pair-index)
                                          (setf (getf osection item-name) item-value))
                                        (incf pair-index))
                              (cons (first section) osection)))))
    (loop :for section :in source :when (not (assoc (first section) target))
       :do (push section output))
    output))

(defun build-profile (symbol spec mode section-names)
  "Build a documentation or test profile from a set of section names in a Vex idiom specification."
  (let ((specs (loop :for subspec :in spec
                     :when (position (string-upcase (first subspec))
                                     #("FUNCTIONS" "OPERATORS" "STATEMENTS" "ARBITRARY-TEST-SET" "TEST-SET")
                                     :test #'string=)
                       :collect subspec)))
    (loop :for name :in section-names
          :append (let* ((subspec (find name specs :test (lambda (id form)
                                                           (eq id (second (assoc :name (rest (second form))))))))
                         (spec-type (intern (string-upcase (first subspec)) "KEYWORD")))
                    (append (case mode
                              (:demo (let ((profile-spec (rest (assoc :demo-profile (cdadr subspec)))))
                                       `((format t "~%~%∘○( ~a~%  ( ~a~%" ,(getf profile-spec :title)
                                                 ,(getf profile-spec :description)))))
                              (:test `((format t "~%~%∘○( ~a )○∘~%"
                                               ,(getf (rest (assoc :tests-profile (cdadr subspec)))
                                                      :title)))))
                            (loop :for test-set :in (cddr subspec)
                                  :append (funcall (case spec-type
                                                     (:functions #'process-lex-tests-for)
                                                     (:operators #'process-lex-tests-for)
                                                     (:statements #'process-lex-tests-for)
                                                     (:test-set #'process-general-tests-for)
                                                     (:arbitrary-test-set #'process-arbitrary-tests-for))
                                                   symbol test-set :mode mode)))))))

(defmacro vex-idiom-spec (symbol extension &rest subspecs)
  "Process the specification for a vector language and build functions that generate the code tree."
  (macrolet ((of-subspec (symbol-string)
               `(rest (assoc ',symbol-string subspecs :test (lambda (x y) (string= (string-upcase x)
                                                                                   (string-upcase y)))))))
    (let* ((symbol-string (string-upcase symbol))
           (idiom-symbol (find-symbol (format nil "*~a-IDIOM*" symbol-string)
                                      (symbol-package symbol)))
           (lexicons-form (list 'idiom-lexicons idiom-symbol))
           (demo-forms (build-profile symbol subspecs :demo (rest (assoc :demo (of-subspec profiles)))))
           (test-forms (build-profile symbol subspecs :test (rest (assoc :test (of-subspec profiles)))))
           (timed-forms (build-profile symbol subspecs :time (rest (assoc :time (of-subspec profiles)))))
           (atest-forms (build-profile symbol subspecs :test
                                       (rest (assoc :arbitrary-test (of-subspec profiles)))))
           (entity-defs (rest (assoc 'entities subspecs)))
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
                   symbol (loop :for subspec :in subspecs
                                :when (position (intern (string-upcase (first subspec)) "KEYWORD")
                                                #(:functions :operators :statements))
                                  :collect subspec))
        `(progn ,@(unless extension `((proclaim '(special ,idiom-symbol))
                                      (setf (symbol-value (quote ,idiom-symbol)) ,idiom-definition)))
                ,@assignment-form
                (setf (idiom-system ,idiom-symbol)
                      (append ,(cons 'list (of-subspec system))
                              (idiom-system ,idiom-symbol))
                      (idiom-utilities ,idiom-symbol)
                      (append ,(cons 'list (of-subspec utilities))
                              (idiom-utilities ,idiom-symbol))
                      (idiom-symbols ,idiom-symbol)
                      (append ,(list 'quote (of-subspec symbols))
                              (idiom-symbols ,idiom-symbol))
                      ;; assign each part of the lexicon, appending to an existing lexicon if present
                      ;; so the spec may either create or append to an idiom (as for (extend-vex-idiom))
                      ,@(reverse (loop :for (key val) :on idiom-list :by #'cddr :when val
                                       :append `((concatenate 'string (reverse (coerce ',val 'string))
                                                              (or (getf ,lexicons-form ,key) ""))
                                                 ;; reversed order, needed to order the lexicon list
                                                 (getf ,lexicons-form ,key)))))

                (setf (idiom-utilities ,idiom-symbol)
                      (append (list :map-sections
                                    (specify-mappers
                                     ,@(loop :for entd :in entity-defs
                                             :collect (cons 'list (cons (intern (string (first entd))
                                                                                "KEYWORD")
                                                                        (rest entd))))))
                              (idiom-utilities ,idiom-symbol)))
                
                ,@(if (not extension)
                      `((defmacro ,(find-symbol symbol-string (symbol-package symbol))
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
                          (cons ',(find-symbol symbol-string (symbol-package symbol))
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
                                                             (or (eql ',(find-symbol symbol-string
                                                                                     (symbol-package symbol))
                                                                      (first ,item))
                                                                 (eql ',(find-symbol
                                                                         (concatenate 'string
                                                                                      symbol-string
                                                                                      "-LOAD")
                                                                         (symbol-package symbol))
                                                                      (first ,item))
                                                                 (eql ',(find-symbol printout-sym
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
                                `(eval-when (:execute :load-toplevel :compile-toplevel)
                                   (if (find-package ,,ws-fullname)
                                       (format nil "A workspace called ｢~a｣ already exists." ',,ws-name)
                                       (progn (make-package ,,ws-fullname)
                                              (make-package ,(concatenate 'string ,ws-fullname "-LEX"))))
                                   (proclaim (list 'special (intern "*SYSTEM*" ,,ws-fullname)
                                                   (intern "*BRANCHES*" ,,ws-fullname)
                                                   (intern "*NS-POINT*" ,,ws-fullname)
                                                   (intern "*IDIOM*" ,,ws-fullname)
                                                   ,@(loop :for (key val)
                                                             :on ,(getf (of-subspec system) :variables)
                                                           :by #'cddr
                                                           :collect `(intern ,(string-upcase val)
                                                                             ,,ws-fullname))))
                                   (set (find-symbol "*IDIOM*" ,,ws-fullname)
                                        ,(intern (format nil "*~a-IDIOM*" ,symbol-string)
                                                 ,(package-name *package*)))
                                   (unless (boundp (find-symbol "*SYSTEM*" ,,ws-fullname))
                                     (set (find-symbol "*SYSTEM*" ,,ws-fullname)
                                          ,',(cons 'list (of-subspec system)))
                                     ;; TODO: following is APL-specific, move into spec
                                     (set (find-symbol "*BRANCHES*" ,,ws-fullname) nil)
                                     (set (find-symbol "*NS-POINT*" ,,ws-fullname) nil)
                                     ,@(loop :for (key val)
                                               :on ,(getf (of-subspec system) :variables) :by #'cddr
                                             :collect `(set (find-symbol ,(string-upcase val) ,,ws-fullname)
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
                                        (,',(find-symbol (concatenate 'string symbol-string
                                                                      "-CREATE-WORKSPACE")
                                                         (symbol-package symbol))
                                         ,,ws-name)
                                        ,(format nil "The workspace ｢~a｣ has been cleared." ,ws-name))
                                 (progn (,',(find-symbol (concatenate 'string symbol-string
                                                                      "-CREATE-WORKSPACE")
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
                                  (list "aliased function" ,(getf idiom-data :afn-count))
                                  (list "aliased operator" ,(getf idiom-data :aop-count))
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

;; (defun specify-mappers (&rest specs)
;;   (let ((open-chars) (closing-chars) (open-matchers) (dividers) (close-matchers) (formatters))
;;     (loop :for spec in specs
;;           :do (let ((delimiters (getf spec :delimit)) (divider (getf spec :divide))
;;                     (start (getf spec :start)) (end (getf spec :end)) (format (getf spec :format)))
;;                 (when delimiters
;;                   (let* ((bclen (length delimiters))
;;                          (hbclen (ash bclen -1)))
;;                     (push (lambda (char)
;;                             (position char delimiters :end hbclen :test #'char=))
;;                           open-matchers)
;;                     (loop :for d :across delimiters :for dx :below hbclen :do (push d open-chars))
;;                     (push (lambda (char) (position char delimiters :start hbclen :test #'char=))
;;                           close-matchers)
;;                     (loop :for dx :from hbclen :below (length delimiters)
;;                           :do (push (aref delimiters dx) open-chars))))
;;                 (when start
;;                   (loop :for trm :in (list start end)
;;                         :for matcher :in (list open-matchers close-matchers)
;;                         :do (typecase terminal
;;                               (character
;;                                (push (lambda (char) (char= char trm)) matcher))
;;                               (list
;;                                (push (lambda (char) (member char trm :test #'char=)) matcher))
;;                               (function (push trm open-matchers)))))
;;                 (if (not divider)
;;                     (push nil dividers)
;;                     (push (typecase divider
;;                             (character
;;                              (print (list :dd divider))
;;                              (lambda (char) (char= char divider)) ))
;;                           dividers))
;;                 (push format formatters)))
;;     (print (list :oo open-matchers dividers close-matchers specs))
;;     (lambda (idiom input)
;;       (let ((matched nil))
;;         (print (list :oo))
;;         (loop :until matched :for open-matcher :in open-matchers :for divider :in dividers
;;               :for close-matcher :in close-matchers :for formatter :in formatters
;;               :do (parse input
;;                          (=destructure
;;                              (_ enclosed terminal)
;;                              (=list (?satisfies open-matcher)
;;                                     ;; for some reason, the first character in the string is
;;                                     ;; iterated over twice here, so the character index is
;;                                     ;; checked and nothing is done for the first character
;;                                     ;; TODO: fix this
;;                                     (=transform (=subseq
;;                                                          (%some (?not (%or (?satisfies open-matcher)
;;                                                                            (?satisfies divider)
;;                                                                            (?satisfies close-matcher)))))
;;                                                         (funcall formatter
;;                                                                  (lambda (str)
;;                                                                    (parse str (=vex-string idiom)))))
;;                                     (%or (?satisfies close-matcher)
;;                                          (?satisfies divider)))
;;                            (print (list :tr enclosed terminal))
;;                            (when enclosed (setf matched enclosed)))))
;;         matched))))

;; (let ((delimiters) (axis-separators) (full-len) (half-len) (nesting (vector 0 0 0)))
;;   (lambda (string idiom)
;;     (unless delimiters
;;       (setf delimiters (reverse (funcall (of-utilities idiom :collect-delimiters) idiom))
;;             full-len (length delimiters)
;;             half-len (/ full-len 2)
;;             axis-separators (of-system idiom :axis-separators)))
;;     (let ((indices) (last-index) (quoted))
;;       (loop :for i :below (length nesting) :do (setf (aref nesting i) 0))
;;       (loop :for char :across string :counting char :into charix
;;             :do (let ((mx (or (loop :for d :across delimiters :counting d :into dx
;;                                     :when (char= d char) :do (return (- full-len -1 dx)))
;;                               0)))
;;                   (if (position char (of-system idiom :string-delimiters) :test #'char=)
;;                       (setf quoted (not quoted))
;;                       (unless quoted
;;                         (if (< half-len mx) (incf (aref nesting (- full-len mx)))
;;                             (if (<= 1 mx half-len)
;;                                 (if (< 0 (aref nesting (- half-len mx)))
;;                                     (decf (aref nesting (- half-len mx)))
;;                                     (error "Each closing ~a must match with an opening ~a."
;;                                            (aref delimiters mx)
;;                                            (aref delimiters (- half-len mx))))
;;                                 (when (and (position char axis-separators :test #'char=)
;;                                            (zerop (loop :for ncount :across nesting
;;                                                         :summing ncount)))
;;                                   (setq indices (cons (1- charix) indices)))))))))
;;       (loop :for index :in (reverse (cons (length string) indices))
;;             :counting index :into iix
;;             :collect (make-array (- index (if last-index 1 0)
;;                                     (or last-index 0))
;;                                  :element-type 'character :displaced-to string
;;                                  :displaced-index-offset (if last-index (1+ last-index) 0))
;;             :do (setq last-index index)))))

;; (defun process-enclosed (string )
;;   (let ((indices) (last-index) (quoted) (nesting 0))
;;     (loop :for i :below (length nesting) :do (setf (aref nesting i) 0))
;;     (loop :for char :across string :counting char :into charix
;;           :do (let ((mx (or (loop :for d :across delimiters :counting d :into dx
;;                                   :when (char= d char) :do (return (- full-len -1 dx)))
;;                             0)))
;;                 (if (position char (of-system idiom :string-delimiters) :test #'char=)
;;                     (setf quoted (not quoted))
;;                     (unless quoted
;;                       (if (< half-len mx) (incf (aref nesting (- full-len mx)))
;;                           (if (<= 1 mx half-len)
;;                               (if (< 0 (aref nesting (- half-len mx)))
;;                                   (decf (aref nesting (- half-len mx)))
;;                                   (error "Each closing ~a must match with an opening ~a."
;;                                          (aref delimiters mx)
;;                                          (aref delimiters (- half-len mx))))
;;                               (when (and (position char axis-separators :test #'char=)
;;                                          (zerop (loop :for ncount :across nesting
;;                                                       :summing ncount)))
;;                                 (setq indices (cons (1- charix) indices)))))))))
;;     (loop :for index :in (reverse (cons (length string) indices))
;;           :counting index :into iix
;;           :collect (make-array (- index (if last-index 1 0)
;;                                   (or last-index 0))
;;                                :element-type 'character :displaced-to string
;;                                :displaced-index-offset (if last-index (1+ last-index) 0))
;;           :do (setq last-index index))))

;; (defun specify-mappers (&rest specs)
;;   (let ((open-chars) (closing-chars) (open-matchers) (dividers) (close-matchers) (formatters)
;;         (nest-counters))
;;     (loop :for spec in specs
;;           :do (let ((delimiters (getf spec :delimit)) (divider (getf spec :divide))
;;                     (start (getf spec :start)) (end (getf spec :end)) (format (getf spec :format)))
;;                 (when delimiters
;;                   (let* ((bclen (length delimiters))
;;                          (hbclen (ash bclen -1)))
;;                     (push (lambda (char)
;;                             (position char delimiters :end hbclen :test #'char=))
;;                           open-matchers)
;;                     (loop :for d :across delimiters :for dx :below hbclen :do (push d open-chars))
;;                     (push (lambda (char) (position char delimiters :start hbclen :test #'char=))
;;                           close-matchers)
;;                     (loop :for dx :from hbclen :below (length delimiters)
;;                           :do (push (aref delimiters dx) open-chars))))
;;                 (when start
;;                   (loop :for trm :in (list start end)
;;                         :for matcher :in (list open-matchers close-matchers)
;;                         :for collection :in (list open-chars closing-chars)
;;                         :do (typecase terminal
;;                               (character
;;                                (push (lambda (char) (char= char trm)) matcher)
;;                                (push char collection))
;;                               (list
;;                                (push (lambda (char) (member char trm :test #'char=)) matcher)
;;                                (loop :for i :in terminal :do (push i collection)))
;;                               (function (push trm open-matchers)))))
;;                 (if (not divider)
;;                     (push nil dividers)
;;                     (push (typecase divider
;;                             (character
;;                              (print (list :dd divider))
;;                              (lambda (char) (char= char divider)) ))
;;                           dividers))
;;                 (push format formatters)))
    
;;     (setf nest-counters (make-array (length open-matchers) :element-type 'fixnum :initial-element 0))
    
;;     (print (list :oo open-matchers dividers close-matchers specs nest-counters
;;                  open-chars closing-chars))
;;     (lambda (idiom input)
;;       (let ((matched nil))
;;         (print (list :oo))
;;         (loop :until matched :for open-matcher :in open-matchers
;;               :do (when (funcall open-matcher (aref input 0))
;;                     (setf matched (process-enclosed idiom input
;;                                                     open-matchers close-matchers dividers))))
;;         ;; (loop :until matched :for open-matcher :in open-matchers :for divider :in dividers
;;         ;;       :for close-matcher :in close-matchers :for formatter :in formatters
;;         ;;       :do (parse input
;;         ;;                  (=destructure
;;         ;;                      (_ enclosed terminal)
;;         ;;                      (=list (?satisfies open-matcher)
;;         ;;                             ;; for some reason, the first character in the string is
;;         ;;                             ;; iterated over twice here, so the character index is
;;         ;;                             ;; checked and nothing is done for the first character
;;         ;;                             ;; TODO: fix this
;;         ;;                             (=transform (=subseq
;;         ;;                                                  (%some (?not (%or (?satisfies open-matcher)
;;         ;;                                                                    (?satisfies divider)
;;         ;;                                                                    (?satisfies close-matcher)))))
;;         ;;                                                 (funcall formatter
;;         ;;                                                          (lambda (str)
;;         ;;                                                            (parse str (=vex-string idiom)))))
;;         ;;                             (%or (?satisfies close-matcher)
;;         ;;                                  (?satisfies divider)))
;;         ;;                    (print (list :tr enclosed terminal))
;;         ;;                    (when enclosed (setf matched enclosed)))))
;;         matched))))

(defun specify-mappers (&rest specs)
  (let ((open-chars) (closing-chars) (open-matchers) (dividers) (close-matchers) (formatters)
        (div-formatters) (nest-counters))
    (loop :for spec-list in specs 
          :do (destructuring-bind (spec-type spec-name &rest spec) spec-list
                ;; (print (list :sp spec-type))
                (case spec-type
                  (:series
                   (let ((delimiters (getf spec :delimit)) (divider (getf spec :divide))
                         (start (getf spec :start)) (end (getf spec :end)) (format (getf spec :format)))
                     (when delimiters
                       (let* ((bclen (length delimiters))
                              (hbclen (ash bclen -1)))
                         (push (lambda (char)
                                 (position char delimiters :end hbclen :test #'char=))
                               open-matchers)
                         (loop :for d :across delimiters :for dx :below hbclen :do (push d open-chars))
                         (push (lambda (char) (position char delimiters :start hbclen :test #'char=))
                               close-matchers)
                         (loop :for dx :from hbclen :below (length delimiters)
                               :do (push (aref delimiters dx) open-chars))))
                     (when start
                       (loop :for trm :in (list start end)
                             :for matcher :in (list open-matchers close-matchers)
                             :for collection :in (list open-chars closing-chars)
                             :do (typecase terminal
                                   (character
                                    (push (lambda (char) (char= char trm)) matcher)
                                    (push char collection))
                                   (list
                                    (push (lambda (char) (member char trm :test #'char=)) matcher)
                                    (loop :for i :in terminal :do (push i collection)))
                                   (function (push trm open-matchers)))))
                     ;; (push (typecase divider
                     ;;         (character
                     ;;          (print (list :dd divider))
                     ;;          (lambda (char) (char= char divider))))
                     ;;       dividers)
                     (push format formatters)))
                  (:divider
                   (let ((matcher (getf spec :match)) (format (getf spec :format)))
                     (push (typecase matcher
                             (character (lambda (char) (char= char matcher)))
                             (string (lambda (char) (position char matcher :test #'char=)))
                             (list (lambda (char) (member char matcher :test #'char=)))
                             (function matcher))
                           dividers)
                     (push (typecase format
                             (symbol format))
                           div-formatters))))))
    
    (setf nest-counters (make-array (length open-matchers) :element-type 'fixnum :initial-element 0))
    
    ;; (print (list :oo open-matchers dividers close-matchers specs nest-counters
    ;;              open-chars closing-chars))
    (lambda (idiom string)
      (let ((code 0) (denoted) (returned) (divider-list)
            (dl-indices (make-array (length string) :element-type '(signed-byte 8)
                                 :initial-element 0)))
        (loop :for char :across string :for cx :from 0
              :do (loop :for om :in open-matchers :for cm :in close-matchers
                        :for ix :from 0 :while (zerop code)
                        :do (when (funcall om char)
                              (incf code (1+ ix)))
                            (when (funcall cm char)
                              (decf code (1+ ix))))
                  (loop :for dv :in dividers :for ix :from (length open-matchers) :while (zerop code)
                        :do (when (and dv (funcall dv char))
                              (incf code (1+ ix))))
                  (unless (zerop code)
                    (setf (aref dl-indices cx) code
                          code              0)))

        (print (list :ggg formatters dl-indices dividers div-formatters))
        
        (loop :for index :across dl-indices :for char :across string :for ix :from 0
              :do (when (not (zerop index))
                    (when (plusp index)
                      (if (<= index (length formatters))
                          (progn (incf code (ash 1 (ash index 3)))
                                 (push (list nil ix index) returned))
                          (push (list (nth (- index 1 (length formatters))
                                           div-formatters)
                                      ix)
                                returned)))
                    (when (minusp index)
                      (decf code (ash 1 (ash (abs index) 3)))
                      (setf divider-list (rest divider-list))
                      (let ((found) (formatter (nth (1- (abs index)) formatters)))
                        (loop :for r :in returned :for rx :from 0
                              :until found :when (third r)
                              :do (when (= (abs index) (third r))
                                    (setf found (setf (nth rx returned)
                                                      (list formatter (second r) ix)))))))))
      (values (reverse returned) dl-indices)))))

(defun construct (map) ;; &optional collected (start 0))
  (print (list :mm map))
  (destructuring-bind (&optional type start end) (first map)
    (if end (let* ((collected) (remaining (rest map))
                   (next-element (if (and (cadar remaining) (> end (cadar remaining)))
                                     (first remaining)))
                   (span (list start (min end (or (second next-element) end))))
                   (next-end (and next-element (third next-element))))
              (print (list :st start end next-element))
              (loop :while span
                    :do (push :a collected)
                        (print (list :cc collected))
                        (multiple-value-bind (constructed yet-remaining) (construct remaining)
                          (print (list :c remaining constructed yet-remaining collected))
                          (when constructed (push constructed collected))
                          (setf remaining    (print yet-remaining)
                                next-element (if (and (cadar remaining) (> end (cadar remaining)))
                                                 (print (first remaining)))
                                span         (if next-element
                                                 (list start (min end (or (second next-element) end)))))))
              (print :ee)
              ;; (sleep 1)
              (values (list type collected) remaining)))))

;; (defun construct (map)
;;   (print (list :m map))
;;   (let ((index 0) (close) (output (list nil nil)))
;;     (loop :for spec :in map
;;           :do (destructuring-bind (type start &optional end) spec
;;                 (print (list :b index start end :a (- start index)))
;;                 (when (< index start)
;;                   (push (list :a (- start index)) (first output)))
;;                 (setf index (1+ start))
;;                 (print (list :cl close output))
;;                 (when (and close (> start (first close)))
;;                   (print (list :cl close))
;;                   (push (reverse (first output)) (second output))
;;                   (setf close (rest close)))
                
;;                 (case type
;;                   ((:as :br)
;;                    (push (reverse (first output)) (second output))
;;                    (setf output (rest output)
;;                          index  (1+ start))
;;                    (push nil output))
;;                   (:ax (push (list :ax) output)
;;                    (push end close))
;;                   (t (push nil output)
;;                    (push end close))))
;;           (print (list :out output)))
;;     ;; (push (list :a 99) (first output))
;;     ;; (push (reverse (first output))
;;     ;;       (second output))
;;     (setf output (rest output))
;;     output
;;     ;; (reverse (cons (reverse (first output))
;;     ;;                (second output)))
;;     ))

(defun construct2 (map string)
  (print (list :m map))
  (let ((bounds) (formats) (index 0)
        (output (list nil nil)))
    (loop :for spec :in map
          :do (print :ee)
              (destructuring-bind (type start &optional end) spec
                (loop :while (and bounds (> start (first bounds)))
                      :do (push (list :a (- (first bounds) index)) (first output))
                          (print (list :gg output (rest output)))
                          (push (cons (first formats) (reverse (first output))) (second output))
                          (print (list :o output))
                          (setf index   (1+ (first bounds))
                                bounds  (rest bounds)
                                formats (rest formats)
                                output  (print (cons nil (rest output)))))
                (when (< index start)
                  (push (list :a (- start index)) (first output))
                  (print (list :ex index start))
                  (setf index (1+ start)))
                (print (list :b spec index start end :a (- start index)
                             :ff formats output type))
                (case type
                  ((:as :br)
                   (push (reverse (first output)) (second output))
                   (setf output (rest output)))
                  (:ax (push :ax formats)
                   (setf index (1+ start))
                   (push nil output)
                   (push end bounds))))
              (print (list :out output)))
    (when (< index (1- (length string)))
      (print (list :io output))
      (push (list :a (- (length string) 1 index))
            (first output)))
    ;; (loop :for b :in bounds :do
    ;;   (push (list :a (- b index)) (first output))
    ;;   (push (cons (first formats) (reverse (first output))) (second output))
    ;;   (setf index (1+ b)))
    ;; (push (list :a 99) (first output))
    ;; (push (reverse (first output))
    ;;       (second output))
    ;; (setf output (rest output))
    output
    ;; (reverse (cons (reverse (first output))
    ;;                (second output)))
    ))

'((:ax (:a 1) (:a 2)))

'((:ax (:a 1) (:ax (:a 1) (:a 2)) (:a 1)))

;; (april "'[{(]})'{⍺{(⍵×~I)+-0⌈(2÷⍨≢⍺)-⍨⍵×I←⍵>2÷⍨≢⍺}⍺{⍵×⍵<1+≢⍺}⍺⍳⍵}'( [  () ] )'")
;; (april "'[{(]})'{{(×⍵)×256*1-⍨|⍵}⍺{E←-0⌈(2÷⍨≢⍺)-⍨⍵×I←⍵>2÷⍨≢⍺ ⋄ E+⍵×~I}⍺{⍵×⍵<1+≢⍺}⍺⍳⍵}'( [  () ] )'")
;; (april "'[{(]})'{{255*|⍵}⍺{E←-0⌈(2÷⍨≢⍺)-⍨⍵×I←⍵>2÷⍨≢⍺ ⋄ E+⍵×~I}⍺{⍵×⍵<1+≢⍺}⍺⍳⍵}'( [  () ] )'")

;; (specify-mappers
;;  (series :comment  :start "⍝" :end (coerce '(#\Newline #\Return) 'string))
;;  (series :closure  :delimit "()" :without :break)
;;  (series :function :delimit "{}")
;;  (series :axes     :delimit "[]" :divide #\; :without :break)))

(defun =vex-closure (idiom boundary-chars
                     &key transform-by disallow-linebreaks symbol-collector if-confirmed)
  (let* ((quoted) (balance 1) (char-index 0)
         (string-delimiters (of-system idiom :string-delimiters))
         (bclen (length boundary-chars))
         (hbclen (floor bclen 2))
         ;; disallow linebreak overriding opening and closing characters
         (dllen (when (stringp disallow-linebreaks) (length disallow-linebreaks)))
         (dlbor-opening-chars (when dllen (subseq disallow-linebreaks 0 (floor dllen 2))))
         (dlbor-closing-chars (when dllen (subseq disallow-linebreaks (floor dllen 2) dllen)))
         (dlb-overriding-balance 0))
    (=destructure
        (_ enclosed _)
        (=list (?satisfies (lambda (char) (position char boundary-chars
                                                    :end hbclen :test #'char=)))
               ;; for some reason, the first character in the string is iterated over twice here,
               ;; so the character index is checked and nothing is done for the first character
               ;; TODO: fix this
               (=transform (=subseq
                            (%some (?satisfies
                                    (lambda (char)
                                      ;; have to do the zerop check to avoid the double-
                                      ;; iteration mentioned above
                                      (when (and (not (zerop char-index))
                                                 (position char string-delimiters :test #'char=))
                                        (setf quoted (not quoted)))
                                      (when (and disallow-linebreaks
                                                 (zerop dlb-overriding-balance)
                                                 (funcall (of-utilities idiom :match-newline-character)
                                                          char))
                                        (error "Newlines cannot occur within a ~a closure."
                                               boundary-chars))
                                      (unless quoted
                                        (if (and (< 0 char-index)
                                                 (position char boundary-chars
                                                           :end hbclen :test #'char=))
                                            (incf balance)
                                            (when (and (position char boundary-chars
                                                                 :start hbclen :test #'char=)
                                                       (< 0 char-index))
                                              (decf balance)))
                                        (when dlbor-opening-chars
                                          (if (and (< 0 char-index)
                                                   (position char dlbor-opening-chars
                                                             :test #'char=))
                                              (incf dlb-overriding-balance)
                                              (when (position char dlbor-closing-chars
                                                              :test #'char=)
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
               (?satisfies (lambda (char) (position char boundary-chars
                                                    :start hbclen :test #'char=))))
      (if (zerop balance)
          (progn (when if-confirmed (funcall if-confirmed))
                 enclosed)
          (error "No closing ~a found for opening ~a."
                 (aref boundary-chars 1) (aref boundary-chars 0))))))

(let ((collected-matched-closing-chars))
  (defun =vex-string (idiom &optional output precedent)
    "Parse a string of text, converting its contents into nested lists of Vex tokens."
    (let ((string-found) (olnchar) (symbols) (is-function-closure)
          ;; the olnchar variable is needed to handle characters that may be functional or part
          ;; of a number based on their context; in APL it's the . character, which may begin a number like .5
          ;; or may work as the inner/outer product operator, as in 1 2 3+.×4 5 6.
          (uniform-char) (arg-rooted-path) (fix 0))
      (unless collected-matched-closing-chars
        (setf collected-matched-closing-chars (funcall (of-utilities idiom :collect-delimiters) idiom)))
      
      (labels ((?blank-character   () (?satisfies (of-utilities idiom :match-blank-character)))
               (?newline-character () (?satisfies (of-utilities idiom :match-newline-character)))
               (?numeric-character () (?satisfies
                                         (lambda (i) (funcall (of-utilities idiom :match-numeric-character)
                                                              i idiom))))
               (?token-character   () (?satisfies
                                         (lambda (i) (funcall (of-utilities idiom :match-token-character)
                                                              i idiom))))
               (numeric-string-p  (i) (funcall (of-utilities idiom :number-formatter) i))
               (pjoin-char-p      (i) (funcall (of-utilities idiom :match-path-joining-character)
                                              i idiom))
               (utoken-p          (i) (funcall (of-utilities idiom :match-uniform-token-character) i))
               (p-or-u-char-p (is-path uniform-char)
                 (if is-path (lambda (item) (funcall (of-utilities idiom :match-token-character)
                                                     item idiom))
                     (lambda (item) (char= uniform-char item))))
               (=string (delimiters)
                 (let ((lastc) (delimiter) (escape-indices) (char-index 0))
                   (=destructure (_ content)
                                 (=list (?satisfies (lambda (c) (when (position c delimiters :test #'char=)
                                                                  (setq delimiter c))))
                                        ;; note: nested quotes must be checked backwards; to determine
                                        ;; whether a delimiter indicates the end of the quote, look at
                                        ;; previous character to see whether it is a delimiter, then
                                        ;; check whether the current character is an escape character #\\
                                        (=subseq (%any (?satisfies
                                                        (lambda (char)
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
                                                      (+ -2 (length content)))))
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
               (=vex-errant-axis-separating-character ()
                 (let ((errant-char))
                   (=destructure (_ _)
                                 (=list (?satisfies
                                         (lambda (char)
                                           (when (funcall (of-utilities
                                                           idiom :match-axis-separating-character)
                                                          char idiom)
                                             (setq errant-char char))))
                                        (=subseq (%any (?satisfies 'characterp))))
                     (error "Misplaced axis delimiter ~a." errant-char))))
               (=vex-errant-closing-character (boundary-chars)
                 (let ((errant-char) (matching-char)
                       (chars-count (floor (length boundary-chars) 2)))
                   (=destructure (_ _)
                                 (=list (?satisfies
                                         (lambda (char)
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
               (handle-axes (input-string)
                 (let* ((each-axis (funcall (of-utilities idiom :process-axis-string)
                                            input-string idiom))
                        (each-axis-code (loop :for axis :in each-axis
                                              :collect (first (process-lines axis)))))
                   (cons :ax each-axis-code)))
               (handle-function (input-string)
                 (destructuring-bind (content meta) (process-lines input-string)
                   (list :fn (cons :meta meta) content)))
               (handle-symbol (string)
                 (multiple-value-bind (formatted is-symbol)
                     (funcall (of-utilities idiom :format-value)
                              (string-upcase (idiom-name idiom))
                              ;; if there's an overloaded token character, do as above
                              (idiom-symbols idiom)
                              (if (getf precedent :overloaded-num-char)
                                  (format nil "~a~a" (getf precedent :overloaded-num-char)
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
                          (of-lexicons idiom char :operators)
                          (of-lexicons idiom char :statements)))))
        (=destructure (leading-space item trailing-space break rest)
                      (=list (=transform (=subseq (%any (?blank-character))) #'length)
                             (%or (=vex-closure idiom (of-system idiom :closure-wrapping)
                                                :transform-by nil
                                                :disallow-linebreaks
                                                (of-system idiom :function-wrapping))
                                  (=vex-closure idiom (of-system idiom :axis-wrapping)
                                                :transform-by #'handle-axes)
                                  (=vex-closure idiom (of-system idiom :function-wrapping)
                                                :transform-by #'handle-function
                                                :if-confirmed (lambda () (setq is-function-closure t)))
                                  (=vex-errant-axis-separating-character)
                                  (=vex-errant-closing-character collected-matched-closing-chars)
                                  (=string (of-system idiom :string-delimiters))
                                  (=transform (=subseq (%some (?satisfies #'functional-character-matcher)))
                                              (lambda (string)
                                                (let ((*print-case* :upcase)
                                                      (char (character string)))
                                                  ;; the print case is needed since otherwise the (format)
                                                  ;; here will search using a lowercase idiom name
                                                  (unless olnchar
                                                    (or (and (not (of-lexicons idiom char :operators))
                                                             (of-lexicons idiom char :symbolic-forms)
                                                             (symbol-value
                                                              (find-symbol (format nil "~a-LEX-SY-~a"
                                                                                   (idiom-name idiom)
                                                                                   char)
                                                                           (string (idiom-name idiom)))))
                                                        (append (list (if (of-lexicons idiom char :statements)
                                                                          :st (if (of-lexicons idiom char
                                                                                               :operators)
                                                                                  :op (when (of-lexicons
                                                                                             idiom char
                                                                                             :functions)
                                                                                        :fn))))
                                                                (if (of-lexicons idiom char :operators)
                                                                    (list (if (of-lexicons idiom char
                                                                                           :operators-pivotal)
                                                                              :pivotal
                                                                              (if (of-lexicons
                                                                                   idiom char
                                                                                   :operators-lateral)
                                                                                  :lateral :unitary))))
                                                                (list char)))))))
                                  (=transform (%and (?test (#'numeric-string-p)
                                                        (=subseq (%some (?numeric-character))))
                                                    (=subseq (%some (?numeric-character))))
                                              (lambda (string)
                                                (funcall (of-utilities idiom :number-formatter)
                                                         ;; if there's an overloaded token character passed in
                                                         ;; the special precedent, prepend it to the token
                                                         ;; being processed
                                                         (if (getf precedent :overloaded-num-char)
                                                             (format nil "~a~a" (getf precedent
                                                                                      :overloaded-num-char)
                                                                     string)
                                                             string))))
                                  ;; matches symbols like APL's ⍺, ∇∇, and ⍵⍵ that must be homogenous - however,
                                  ;; homogenous symbols on the list of argument symbols may be part of a larger
                                  ;; symbol that references a namespace path like ⍵.path.to
                                  (=transform
                                   (=subseq (?seq (=transform (=subseq (?test (#'utoken-p) (=element)))
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
                                  ;; this last clause returns the remainder of the input in case the
                                  ;; input has either no characters or only blank characters
                                  ;; before the first line break
                                  (=subseq (%any (?satisfies 'characterp))))
                             (=transform (=subseq (%any (?blank-character))) #'length)
                             (=subseq (%any (?newline-character)))
                             (=subseq (%any (?satisfies 'characterp))))
          ;; (print (list :ll leading-space trailing-space item precedent))
          ;; (setf (getf precedent :whitespace) trailing-space)
          (let ((lspace-total (+ leading-space (or (and (getf precedent :pspace)
                                                        (second (getf precedent :pspace)))
                                                   0))))
            ;; (when (and (listp item) (member (first item) '(:fn :op))
            ;;            (not (zerop lspace-total)))
            ;;   (setf item (cons (first item) (cons (list :meta :whitespace lspace-total)
            ;;                                       (rest item)))))
            (if (and (not output) (stringp item) (< 0 (length item))
                     (funcall (of-utilities idiom :match-newline-character)
                              (aref item 0)))
                ;; if the string is passed back (minus any leading whitespace) because the string began with
                ;; a line break, parse again omitting the line break character
                (parse (subseq item 1) (=vex-string idiom nil precedent))
                (if (and (zerop (length break)) (< 0 (length rest)))
                    (parse rest (=vex-string idiom (if output (if (not item) output (cons item output))
                                                       (when item (list item)))
                                             (append (when olnchar (list :overloaded-num-char olnchar))
                                                     (list :symbols nil ;; peripheral space
                                                           :pspace (list lspace-total trailing-space)))))
                    (list (if (or (not item)
                                  (and (typep item 'sequence)
                                       (zerop (length item)) (not string-found)))
                              ;; return nothing if only an empty sequence results from parsing
                              ;; unless an explicit empty string was parsed
                              output (cons item output))
                          rest precedent)))))))))

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
                       :always (funcall (of-utilities idiom :match-token-character) c idiom))))
             (process-lines (string &optional space params output)
               (if (zerop (length string))
                   (funcall (of-utilities idiom :compile-form)
                            (reverse output) :space space :params params)
                   (let ((result (funcall (or (of-utilities idiom :lexer-postprocess)
                                              (lambda (&rest args) (first args)))
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

      (symbol-macrolet ((ws-system (symbol-value (find-symbol "*SYSTEM*" space))))

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
                               string (with-open-vex-file (stream (eval string))
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
                                    output-vars))
                   (string-prep (funcall (of-utilities idiom :prep-code-string)
                                         idiom)))
              (funcall (of-utilities idiom :build-compiled-code)
                       (append (funcall (if output-vars #'values
                                            (apply (of-utilities idiom :postprocess-compiled)
                                                   (append (when (assoc :unrendered options)
                                                             (list :unrendered t))
                                                           system-to-use)
                                                   inline-arguments))
                                        (process-lines
                                         (funcall string-prep string)
                                         space (list :call-scope (list :input-vars iv-list
                                                                       :output-vars ov-list))))
                               ;; if multiple values are to be output, add the (values) form at bottom
                               (if output-vars
                                   (funcall (of-utilities idiom :process-multiple-outputs)
                                            output-vars space (not (assoc :unrendered options)))))
                       (loop :for (key value) :on (getf (idiom-system idiom) :workspace-defaults)
                             :by #'cddr :collect (string-upcase key))
                       options system-vars vars-declared stored-refs space)))))))
