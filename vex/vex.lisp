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
                                                                        (rest entd)))))
                                    :entity-specs
                                    ,(cons 'list
                                           (loop :for entd :in entity-defs
                                                 :append (destructuring-bind (type name &rest pairs) entd
                                                           (list name (cons 'list pairs)))))
                                    :section-builders
                                    ,(cons 'list
                                           (loop :for entd :in entity-defs
                                                 :append (destructuring-bind (type name &rest pairs) entd
                                                           (list name (getf pairs :build)))))
                                    :section-formatters
                                    ,(cons 'list
                                           (loop :for entd :in entity-defs
                                                 :append (destructuring-bind (type name &rest pairs) entd
                                                           (list name (getf pairs :format)))))
                                    :section-dividers
                                    ,(cons 'list
                                           (loop :for entd :in entity-defs
                                                 :append (destructuring-bind (type name &rest pairs) entd
                                                           (list name (getf pairs :divide)))))
                                    :section-renderers
                                    ,(cons 'list
                                           (loop :for entd :in entity-defs
                                                 :append (destructuring-bind (type name &rest pairs) entd
                                                           (list name (getf pairs :render))))))
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

(defun specify-mappers (&rest specs)
  (let ((open-chars) (closing-chars) (open-matchers) (dividers) (close-matchers) (exclusive-specs)
        (spec-names) (div-spec-names) (section-names) (divider-names)
        (formatters) (div-formatters) (nest-counters))
    (macrolet ((ixchar (&optional str-index) `(aref string (+ index ,(or str-index 0)))))
      (loop :for spec-list in specs
            :do (destructuring-bind (spec-type spec-name &rest spec) spec-list
                  (case spec-type
                    (:section
                     (let ((delimiters (getf spec :delimit)) (divider (getf spec :divide))
                           (start (getf spec :start)) (end (getf spec :end)) (format (getf spec :format)))
                       (push spec-name spec-names)
                       ;; (print (list :sp spec))
                       (push (getf spec :exclusive) exclusive-specs)
                       (when delimiters ;; in the case of sections with paired delimiters
                         (let* ((bclen (length delimiters))
                                (hbclen (ash bclen -1)))
                           (push (lambda (string index)
                                   (let ((pos (position (ixchar) delimiters :end hbclen :test #'char=)))
                                     (when pos (lambda (string index)
                                                 ;; (print (list :sx string index delimiters
                                                 ;;              (+ hbclen -1 (- hbclen pos))
                                                 ;;              (ixchar)))
                                                 (char= (ixchar)
                                                        (aref delimiters (+ hbclen -1 (- hbclen pos))))))))
                                 open-matchers)
                           (loop :for d :across delimiters :for dx :below hbclen :do (push d open-chars))
                           (push (lambda (string index)
                                   (position (ixchar) delimiters :start hbclen :test #'char=))
                                 close-matchers)
                           (loop :for dx :from hbclen :below (length delimiters)
                                 :do (push (aref delimiters dx) open-chars))))
                       (when start ;; in the case of sections with start and end qualifiers
                         (typecase start
                           (character (push (lambda (string index) (char= (ixchar) start))
                                            open-matchers)
                            (push start collection))
                           (string    (push (lambda (string index) (position (ixchar) start :test #'char=))
                                            open-matchers)
                            (loop :for c :across start :do (push c open-chars)))
                           (list      (push (lambda (string index) (member (ixchar) start :test #'char=))
                                            open-matchers)
                            (loop :for i :in start :do (push i open-chars)))
                           (function (push start open-matchers)))
                         (typecase end
                           (character (push (lambda (string index) (char= (ixchar) end))
                                            open-matchers)
                            (push end collection))
                           (string    (push (lambda (string index) (position (ixchar) end :test #'char=))
                                            close-matchers)
                            (loop :for c :across end :do (push c closing-chars)))
                           (list      (push (lambda (string index) (member (ixchar) end :test #'char=))
                                            close-matchers)
                            (loop :for i :in end :do (push i closing-chars)))
                           (function (push end close-matchers))
                           (t (error "Section specification ~a has a start qualifier but no end qualifier."
                                     spec-name))))
                       (push format formatters)))
                    (:divider
                     (let ((matcher (getf spec :match)) (format (getf spec :format)))
                       (push spec-name div-spec-names)
                       (push (typecase matcher
                               (character (lambda (string index) (char= (ixchar) matcher)))
                               (string    (lambda (string index) (position (ixchar) matcher :test #'char=)))
                               (list      (lambda (string index) (member (ixchar) matcher :test #'char=)))
                               (function matcher))
                             dividers)
                       (push (typecase format
                               (symbol format))
                             div-formatters)))))))
    
    (setf nest-counters  (make-array (length open-matchers) :element-type 'fixnum :initial-element 0))

    ;; (print (list :dn spec-names))
    
    ;; (print (list :oo open-matchers dividers close-matchers specs nest-counters
    ;;              open-chars closing-chars))
    
    (lambda (idiom string)
      (let ((code 0) (open-stack) (confirmer-stack)
            (returned) (divider-list) (exclusive-index) (set-mirroring)
            (dl-indices (make-array (length string) :element-type '(signed-byte 8) :initial-element 0)))
        (loop :for char :across string :for cx :from 0
              :do (loop :for dv :in dividers :for ix :from (length open-matchers) :while (zerop code)
                        :do (when (and dv (funcall dv string cx))
                              (incf code (1+ ix))))
                  (loop :for om :in open-matchers :for cm :in close-matchers
                        :for ix :from 0 :while (zerop code)
                        :do (setf set-mirroring nil)
                            (let ((matcher-output (and (not exclusive-index)
                                                       (funcall om string cx))))
                              ;; (print (list :mo char matcher-output))
                              (when matcher-output
                                (unless nil ; exclusive-index
                                  (incf code (1+ ix))
                                  (push ix open-stack))
                                (when (nth ix exclusive-specs)
                                  (setf exclusive-index ix
                                        set-mirroring   t)
                                  ;; (push ix open-stack)
                                  ))
                              ;; (print (list :ch char matcher-output
                              ;;              set-mirroring open-stack ix))
                              ;; (push matcher-output confirmer-stack)
                              (when (and ;; (funcall cm string cx)
                                     (not set-mirroring)
                                     open-stack (= ix (first open-stack))
                                     confirmer-stack
                                     (or (not (functionp (first confirmer-stack)))
                                         (funcall (first confirmer-stack) string cx))
                                     (or (not exclusive-index)
                                         (= ix exclusive-index)))
                                ;; (print (list :p open-stack))
                                (when exclusive-index (setf exclusive-index nil))
                                (decf code (1+ ix))
                                (pop open-stack)
                                (pop confirmer-stack))
                              (when matcher-output (push matcher-output confirmer-stack))))
                 (unless (zerop code)
                    (setf (aref dl-indices cx) code
                          code                 0)))

        ;; (print (list :ggg formatters dl-indices dividers div-formatters spec-names
        ;;              open-matchers))
        
        (loop :for index :across dl-indices :for char :across string :for ix :from 0
              :do (when (not (zerop index))
                    (when (plusp index)
                      (if (< index (length spec-names))
                          (progn (incf code (ash 1 (ash index 3)))
                                 (push (list nil ix index) returned))
                          (push (list (nth (- index (length spec-names)) div-spec-names)
                                      ix)
                                returned)))
                    ;; (print (list :in index returned exclusive-specs
                    ;;              (= index (third (first returned)))))
                    (when (minusp index)
                      (decf code (ash 1 (ash (abs index) 3)))
                      (pop divider-list)
                      (let ((found) (formatter (nth (1- (abs index)) spec-names)))
                        (loop :for r :in returned :for rx :from 0
                              :until found :when (third r)
                              :do (when (and (null (first r))
                                             (= (abs index) (third r)))
                                    (setf found (setf (nth rx returned)
                                                      (list formatter (second r) ix)))))))))
        ;; (print (list :rr returned))
        (values (reverse returned) dl-indices)))))

(defun construct (string idiom workspace)
  (let ((bounds (list (length string)))
        (formats) (index 0) (output (list nil nil))
        (cl-meta (list :meta :aa 0)) ;; placeholder meta
        (map (funcall (getf (idiom-utilities idiom) :map-sections) idiom string))
        (base-divider (loop :for (key val) :on (getf (idiom-utilities idiom) :entity-specs)
                            :by #'cddr :when (getf val :base) :return (getf val :divide)))
        (postprocessor (or (of-utilities idiom :lexer-postprocess)
                           (lambda (&rest args) (first args)))))
    ;; (print (list :m map bounds))
    (labels ((lex-chars (start end)
               (let* ((substring (make-array (- end start)
                                             :element-type 'character :displaced-to string
                                             :displaced-index-offset start))
                      ;; (sst (print (list :sss substring)))
                      (parsed (parse substring (=vex-string idiom))))
                 ;; (loop :for c :from start :below end :for i :from 0
                 ;;       :do (setf (aref in-string i) (aref string c)))
                 (setf cl-meta (cons :meta (third parsed))
                       (first output)
                       (append ;; (first (funcall (or (of-utilities idiom :lexer-postprocess)
                               ;;                     (lambda (&rest args) (first args)))
                               ;;                 parsed idiom workspace cl-meta))
                               (first parsed)
                               (first output)))))
             (close-bound ()
               ;; (print (list :ggg index (first bounds)))
               (lex-chars index (first bounds))
               ;; (push (list :a (- (first bounds) index)) (first output))
               ;; (push (list :a (lex-chars index (first bounds))) (first output))
               ;; (print (list :gg output)) ;; (rest output)))
               ;; (push (first output) (second output))
               (when (first formats)
                 (let ((this-format (getf (getf (getf (idiom-utilities idiom) :entity-specs)
                                                (first formats))
                                          :format)))
                   ;; (print (list :tf this-format output))
                   (typecase this-format
                     (symbol
                      (setf output (cons (cons (cons this-format
                                                     (cons (first output)
                                                           (second output)))
                                               (third output))
                                         (cdddr output))))
                     (function
                      (setf output (funcall this-format (lambda (item)
                                                          (funcall postprocessor item idiom workspace))
                                            output))))
                   (pop formats)))
               ;; (print (list :o output bounds))
               (setf index  (1+ (first bounds))
                     bounds (rest bounds))))
      
      (loop :for spec :in map
            :do (destructuring-bind (type start &optional end) spec
                  ;; (print (list :bb bou start))
                  (loop :while (and bounds (> start (first bounds))) :do (close-bound))
                  ;; (print (list :ty type index start output))
                  (when (< index start)
                    (lex-chars index start)
                    ;; (push (list :a (lex-chars index start)) (first output))
                    ;; (push (list :a (- start index)) (first output))
                    ;; (print (list :ex type index start))
                    ;; (print (list :eoo output))
z                    (setf index (1+ start))
                    )

                  (when (= index start)
                  ;;   (push nil (first output))
                    (setf index (1+ start)))

                  
                  ;; (print (list :b spec index start end :a (- start index)
                  ;;                                      :ff formats output type))
                  (if end ;; an entity is a section if it has an end, a divider if not
                      (let ((this-builder (getf (getf (getf (idiom-utilities idiom) :entity-specs)
                                                      type)
                                                :build))
                            (this-renderer (getf (getf (getf (idiom-utilities idiom) :entity-specs)
                                                       type)
                                                 :render)))
                        ;; (print (list :tr this-renderer))
                        (if this-builder
                            (progn (push type formats)
                                   (push end bounds)
                                   (setf index (1+ start)
                                         output (funcall this-builder output)
                                         ;; output (funcall postprocessor (funcall this-builder output)
                                         ;;                 idiom workspace)
                                         ))
                            (if this-renderer
                                (progn (push (funcall this-renderer string start end) (first output))
                                       (setf index (1+ end)))
                                (setf index (1+ end)))))
                      ;; dividers are handled based on the containing section type
                      (setf output (funcall (if (first formats)
                                                (getf (getf (getf (idiom-utilities idiom) :entity-specs)
                                                            (first formats))
                                                      :divide)
                                                base-divider)
                                            type (lambda (item)
                                                   (funcall postprocessor item idiom workspace))
                                            output)))))

      ;; (print (list :bo bounds))
      
      (loop :while bounds :do (close-bound))
      (mapcar (lambda (item) (funcall postprocessor item idiom workspace))
              (reverse (cons (first output) (second output)))))))

;; push a nil when you're inside a closure after a break

;;; MAIN THING TO SOLVE:
;;; (april "aa←3 ⋄ bob←{aa+←⍵ ⋄ aa} ⋄ bob 5")
;;; LOOK AT THE TOKENS:

;; (((:FN (:META :ARG-SYMS (⍵))
;;    ((⍵ :SPECIAL-LEXICAL-FORM-ASSIGN (:FN #\+) |aa|) (|aa|)))
;;   (3 :SPECIAL-LEXICAL-FORM-ASSIGN |aa|))
;;  (5 |bob|))

;;; THE ASSIGN AA IS IMPROPERLY NESTED

#|
INFINITE: {(⍵=1)∨⍵=2 : 1 ⋄ (∇ ⍵-2)+∇ ⍵-1}¨⍳12
' ' { A W←{(⍵≠(≢⍵)⍴' ')/⍵}¨⍺ ⍵ ⋄ ((⍴A)=⍴W)∧∧/(+/A∘.=W)=+/A∘.=A } 'dog'
('*'@2)⍳5
{$[⍵>5;G←3⋄H←5⋄G+H;C←8⋄D←2⋄C×D]}¨3 7
{$[⍵<3;5;e←⍵+2⋄-{⍺⍺ ⍵} e]}¨⍳9
'''abc'''

    f1←{⍵+3} ⋄ f2←{⍵×2} ⍝ A comment after the functions are defined.
    ⍝ This is another comment.
    v←⍳3 ⋄ f2 f1 v,4 5

(april (vex::with (:space unit-test-staging)) "fun←{
      5+⍵
      ⍝ comment
    }
    fun 3")


{e←⍳⍵ ⋄ g←⌷ ⋄ (3 g e)←5 ⋄ e} 9
aa←3 ⋄ bob←{aa+←⍵ ⋄ aa} ⋄ bob 5
3 (+{⍺⍺ 2 ⋄ ⍺ ⍵⍵ ⍵}÷) 4
{ ee←{↑⍪/(⊂⍺),⍶,⊂⍵} ⋄ ⍵⊃⊃↑{⍺ ee⌿⍵}/9⍴⊂⍳9 } 22
+{op←⍺⍺ ⋄ ⊃op{(⊂⍺ op⊃⍬⍴⍵),⍵}/1↓{⍵,⊂⍬⍴⍵}¯1⌽⍵}⍳4


(1 2 3 4) 15[1]

x←3 3⍴⍳9 ⋄ y←1 ⋄ x[;y]
(3 3 3⍴⍳27)[1 2;2 2⍴⍳3;]


a←3 4⍴⍳12 ⋄ ⍴a[⍬;]
(3 4⍴⍳12)[ ; 4 3 ]


 ('(','asdf')⍳'('
{$[⍵<3;5;e←⍵+2⋄-{⍺⍺ ⍵} e]}¨⍳9
$[0;2;3]
('*'@2)⍳5
x←2 3 4⍴⍳9 ⋄ x[;1;]←7 ⋄ x
('a',⍬)≡1↑'amy'
{x←⊂[2] ⋄ x ⍵} 2 3 4⍴⍳9

{x←⊂[2] ⋄ x ⍵} 2 3 4⍴⍳9
⊃(⊂'test'),3
1 ¯2 3 ¯4 5\'.'
st←'aodjeignwug' ⋄ st[⍋st]

{(⊂⍋⍵)⌷⍵}⍤1⊢3 4 5⍴⍳9
{⍵[⍋⍵]}'abcABC012xyzXYZ789'
rr←-∘⌽[1] ⋄ rr 3 3⍴⍳9
x←1 ⋄ →three          ⋄ x×←11 ⋄ one→⎕ ⋄ x×←3 ⋄ two→⎕ ⋄ x×←5 ⋄ three→⎕ ⋄ x×←7

{⍵+5}⍣$[3>2;4;5]⊢2
+/,{+/,⍵}⌺3 3⊢6 5 ⍴ ⍳5
⊃,/{m←× ⋄ s←⍴ ⋄ i←⍳ ⋄ 5 m 2 3 s i ⍵}¨2 6
x←1

'((:ax (:a 1) (:a 2)))

'((:ax (:a 1) (:ax (:a 1) (:a 2)) (:a 1)))

(let ((ss "[  [ ( ) ;  ] ]")) (vex::construct4 (funcall (getf (vex::idiom-utilities *april-idiom*) :map-sections) *april-idiom* ss) ss))

(vex::construct3 (funcall (getf (vex::idiom-utilities *april-idiom*) :map-sections) *april-idiom* "[ ;  ]
 ") "[ ;  ]
 ")

"[  [ ( ) ;  ] 
]"

(((:A 1)) ((:AX ((:A 1)) ((:A 2)))))

(defvar str1 "[  [ ( ) ;  ] 
]")

(defvar str2 "[  [ ; ] 
]")

(defvar str3 "[ [ ; ] ]")

(defvar str4 "[ [   ] ]")

(((:ax (:a 1) (:ax (:a 3))))

(vex::construct3 (funcall (getf (vex::idiom-utilities *april-idiom*) :map-sections) *april-idiom* str1) str1)

(NIL (((:A 1) :AX (:A 2) ((:A 3) (:A 1)) ((:A 2)))) NIL)

|#

;; (april "'[{(]})'{⍺{(⍵×~I)+-0⌈(2÷⍨≢⍺)-⍨⍵×I←⍵>2÷⍨≢⍺}⍺{⍵×⍵<1+≢⍺}⍺⍳⍵}'( [  () ] )'")
;; (april "'[{(]})'{{(×⍵)×256*1-⍨|⍵}⍺{E←-0⌈(2÷⍨≢⍺)-⍨⍵×I←⍵>2÷⍨≢⍺ ⋄ E+⍵×~I}⍺{⍵×⍵<1+≢⍺}⍺⍳⍵}'( [  () ] )'")
;; (april "'[{(]})'{{255*|⍵}⍺{E←-0⌈(2÷⍨≢⍺)-⍨⍵×I←⍵>2÷⍨≢⍺ ⋄ E+⍵×~I}⍺{⍵×⍵<1+≢⍺}⍺⍳⍵}'( [  () ] )'")

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
               (if t ; nil
                   (funcall (of-utilities idiom :compile-form)
                        (vex::construct string idiom space)
                        :space space :params params)
                   (if (zerop (length string))
                       (funcall (of-utilities idiom :compile-form)
                                (reverse output) :space space :params params)
                       (let ((result (funcall (or (of-utilities idiom :lexer-postprocess)
                                                  (lambda (&rest args) (first args)))
                                              (print (parse string (=vex-string idiom)))
                                              idiom space)))
                         (print (list :res result))
                         (when print-tokens (print (first result)))
                         (process-lines (second result) space params (cons (first result) output))))))
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
