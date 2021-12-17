;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; utilities.lisp

(in-package #:april)

"Utility functions for April. It's important to understand the difference between the functions and macros provided here and the ones that come from the aplesque package. The faculties provided by aplesque reflect features of APL, but they have uses other than implementing APL. The material here is specifically for use in implementing APL, with uses aside from an APL implementation not accounted for in its design. The functions here are used to implement language mechanics as opposed to functions in the language's standard library; the latter are implemented in library.lisp."

(defvar *april-idiom*)

(define-symbol-macro this-idiom *april-idiom*)
(define-symbol-macro *apl-timestamp* (apl-timestamp))
(define-symbol-macro *first-axis* (if (not axes) 0 (apply-scalar #'- (first axes) index-origin)))
(define-symbol-macro *last-axis* (if axes (- (first axes) index-origin)
                                     (max 0 (1- (rank omega)))))
(define-symbol-macro *first-axis-or-nil* (if axes (apply-scalar #'- (first axes) index-origin)))
(define-symbol-macro *branches* (symbol-value (intern "*BRANCHES*" space)))

(defvar *function-identities* nil)

;; the names of library functions that curry functions having axes with index-origin, needed for the λχ macro
(defparameter *io-currying-function-symbols-monadic* '(ravel-arrays))
(defparameter *io-currying-function-symbols-dyadic* '(catenate-arrays catenate-on-first section-array))
(defparameter *package-name-string* (package-name *package*))

(defvar *april-parallel-kernel*)

(defvar *demo-packages*
  (append #.(if (or (not (string= "Armed Bear Common Lisp" (lisp-implementation-type)))
                    (with-open-stream (cmd-out (make-string-output-stream))
                      (uiop:run-program "mvn -v" :output cmd-out :ignore-error-status t)
                      (< 0 (length (get-output-stream-string cmd-out)))))
                ''(april-demo.cnn) nil)
          ;; tree demo is disabled for ABCL, Lispworks because its large functions cannot be
          ;; compiled using the JVM, while the functions cause LispWorks to freeze
          '(april-demo.dfns.array april-demo.dfns.string april-demo.dfns.power
            #+(not (or abcl lispworks)) april-demo.dfns.tree
            april-demo.dfns.graph april-demo.dfns.numeric)))

(defvar ∇ nil)
(defvar ∇∇ nil)
;; set ∇ and ∇∇ to nil; this prevents errors when they are seen in operator compositions

(defvar *digit-vector* "0123456789")

(defvar *alphabet-vector* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defvar *idiom-native-symbols* '(⍺ ⍵ ⍶ ⍹ ⍺⍺ ⍵⍵ ∇ ∇∇ index-origin print-precision *digit-vector*
                                 *alphabet-vector* *apl-timestamp* to-output output-stream))

(defvar *system-variables* '(:index-origin *index-origin* :print-precision *print-precision*
                             :comparison-tolerance *comparison-tolerance* :division-method *division-method*
                             :rngs *rngs*))

(defvar *rng-names* #(:linear-congruence :mersenne-twister-64 :system))

(defun system-command-exists (command-string &optional prefix)
  "Check for the existence of a shell command under the host operating system."
  (if (not prefix) (setq prefix ""))
  (= 0 (multiple-value-bind (1st 2nd error-code)
	   (uiop:run-program (format nil "~acommand -v ~a" prefix command-string)
			     :ignore-error-status t)
	 (declare (ignore 1st 2nd))
	 error-code)))

(defun count-cpus ()
  "Count the available threads in the system, accounting for different CL implementations."
  (with-open-stream (cmd-out (make-string-output-stream))
    (uiop:run-program
     (case (uiop:operating-system)
       ((:linux :linux-target)
	(if (system-command-exists "nproc") "nproc" ""))
       ((:bsd :freebsd :openbsd :netbsd)
	(if (system-command-exists "sysctl") "sysctl -n hw.ncpu" ""))
       ((:macosx :darwin)
	(if (system-command-exists "sysctl") "sysctl -n hw.logicalcpu" ""))
       ((:windows)
        "echo %NUMBER_OF_PROCESSORS%"))
     :output cmd-out)
    (let ((output (get-output-stream-string cmd-out)))
      (if (= 0 (length output))
	  1 (read-from-string output)))))

(defun make-threading-kernel-if-absent ()
  "Create a kernel for multithreaded executuion via lparallel if none is present."
  (if (not lparallel:*kernel*)
      (setq lparallel:*kernel* (setq *april-parallel-kernel*
                                     (lparallel:make-kernel (count-cpus) :name "april-language-kernel")))))

(let ((this-package (package-name *package*)))
  (defmacro in-april-workspace (name &body body)
    "Macro that interns symbols in the current workspace; works in tandem with ⊏ reader macro."
    (let* ((space-name (concatenate 'string "APRIL-WORKSPACE-" (string-upcase name)))
           (lex-space-name (concatenate 'string space-name "-LEX"))
           ;; build list of values assigned in the (april) call; these are stored as dynamic vars
           (top-level-instrs (mapcar (lambda (item) (string (cadar item))) (cdadar body))))
      (labels ((replace-symbols (form &optional inside-function)
                 (loop :for item :in form :for ix :from 0
                       :collect (cond ((listp item)
                                       (if (and (second item) (not (third item))
                                                (symbolp (second item)) (member (first item) '(inws inwsd)))
                                           (let ((istring (string (second item))))
                                             (intern (string (second item))
                                                     (if (and inside-function
                                                              (not (eql 'inwsd (first item)))
                                                              (not (char= #\* (aref istring 0)))
                                                              (loop :for str :in top-level-instrs
                                                                    :never (string= str istring)))
                                                         lex-space-name space-name)))
                                           ;; don't lex-intern functions like #'⊏|fn|
                                           (replace-symbols item (and (not (eql 'function (first item)))
                                                                      (or inside-function
                                                                          (member (first item)
                                                                                  '(alambda olambda)))))))
                                      ((and (symbolp item) (string= "+WORKSPACE-NAME+" (string-upcase item)))
                                       (list 'quote (intern (string-upcase name) this-package)))
                                      (t item)))))
        (funcall (lambda (form)
                   (push '(make-threading-kernel-if-absent) (cdddr form))
                   form)
                 (replace-symbols (first body)))))))

;; this reader macro expands to (inws symbol) for reader-friendly printing of compiled code
(set-macro-character #\⊏ (lambda (stream character)
                           (declare (ignore character))
                           (list 'inws (read stream t nil t))))

;; this reader macro expands to (inws symbol) for reader-friendly printing of compiled code
(set-macro-character #\⊑ (lambda (stream character)
                           (declare (ignore character))
                           (list 'inwsd (read stream t nil t))))

;; printer extension to use the ⊏ reader macro
(set-pprint-dispatch '(cons (member inws))
                     #'(lambda (s list)
                         (if (and (symbolp (second list)) (not (third list)))
                             (funcall (formatter "⊏~W") s (second list))
                             (pprint-fill s list))))

;; printer extension to use the ⊑ reader macro
(set-pprint-dispatch '(cons (member inwsd))
                     #'(lambda (s list)
                         (if (and (symbolp (second list)) (not (third list)))
                             (funcall (formatter "⊑~W") s (second list))
                             (pprint-fill s list))))

(defun load-demos ()
  "Load the April demo packages."
  (loop :for package-symbol :in *demo-packages* :do (asdf:load-system package-symbol)))

(defmacro run-demo-tests ()
  "Run the tests for each April demo package."
  (cons 'progn
        (loop :for package-symbol :in *demo-packages*
              :append (if (asdf:registered-system package-symbol)
                          (let ((run-function-symbol (intern "RUN-TESTS" (string-upcase package-symbol))))
                            (if (fboundp run-function-symbol)
                                ;; (funcall (symbol-function run-function-symbol))
                                (list (list run-function-symbol))
                                ))
                          (format t "~% Warning: demo system ｢~a｣ not loaded. Did you evaluate (load-demos) before trying to run the demo tests?~%"
                                   package-symbol)))))

(defun disclose-atom (item)
  "If the argument is a non-nested array with only one member, disclose it, otherwise do nothing."
  (if (not (and (not (stringp item)) (arrayp item) (is-unitary item)
                (not (arrayp (row-major-aref item 0)))))
      item (row-major-aref item 0)))

(defmacro insym (symbol)
  "Macro used in grammar.lisp to intern value-referencing symbols in appropriate workspace package."
  `(if (or (not (symbolp ,symbol))
           (member ,symbol *idiom-native-symbols*))
       ,symbol (intern (string ,symbol) space)))

(defmacro alambda (params options &body body)
  "Generate a lambda with a self-reference for use with APL's ∇ character for self-reference in a defn."
  (let* ((options (rest options))
         (system-vars (rest (assoc :sys-vars options)))
         (meta (rest (assoc :meta options))))
    `(symbol-macrolet ((%in-function-p% t))
       (labels ((∇self ,params
                  (declare (ignorable ,@(loop :for var :in params :when (not (eql '&optional var))
                                              :collect var)))
                  (if (eq :get-metadata ,(first params))
                      ,(cons 'list meta)
                      (let ,(loop :for var :in system-vars :collect (list var var))
                        (declare (ignorable ,@system-vars))
                        ,@body))))
         #'∇self))))

(defmacro olambda (params &body body)
  "Generate a lambda with a self-reference for use with APL's ∇∇ symbol for self-reference in a defined operator."
  `(symbol-macrolet ((%in-function-p% t))
     (labels ((∇oself ,params ,@body))
       #'∇oself)))

(defmacro fn-meta (function &rest meta)
  "Wrap a function in another function so that it may carry metadata."
  (let ((args (gensym)))
    `(lambda (&rest ,args)
       (if (eq :get-metadata (first ,args))
           ,(cons 'list meta)
           (apply ,(if (not (symbolp function))
                       function `(function ,function))
                  ,args)))))

(defmacro inv-fn (function &optional is-dyadic inverse-type)
  "Wrap a function to be inverted; returns an error if the function has no inverse form."
  (let ((metadata (gensym)) (inverse (gensym)))
    `(let* ((,metadata (funcall ,function :get-metadata ,@(if is-dyadic (list nil))))
            (,inverse (if (listp ,metadata) (getf ,metadata ,(or inverse-type :inverse)))))
       (or ,inverse (error "Cannot invert function ~a." (quote ,function))))))

(defmacro achoose (item indices &rest rest-params)
  "Wrapper for the choose function."
  (let ((indices-evaluated (gensym)))
    `(let ((,indices-evaluated ,indices))
       (choose ,item ,indices-evaluated ,@rest-params))))

(defun dummy-nargument-function (first &rest rest)
  "Placeholder function to be assigned to newly initialized function symbols."
  (declare (ignorable rest))
  first)

(defun dummy-operator (first &rest rest)
  "Placeholder function to be assigned to newly initialized operator symbols."
  (declare (ignorable rest))
  first)

;; keep legacy april-p macro in place and usable in place of april-f
(defmacro april-p (&rest args)
  (cons 'april-f args))

;; these macros are shorthand for lambda definitions used in the spec; they make April's compiled code
;; more compact and comfortable to read
(defmacro λω (&rest body)
  `(lambda (omega) ,@body))

(defmacro λωα (&rest body)
  `(lambda (omega alpha) ,@body))

(defmacro λωαχ (&rest body)
  `(lambda (omega alpha &optional axes) ,@body))

(defmacro λωχ (&rest body)
  `(lambda (omega &optional axes) ,@body))

(defmacro λχ (body axes)
  "Curry a function with axes for use with an operator."
  (let ((function-type (if (eql 'fn-meta (first body))
                           (caadr body) (first body))))
    (if (member function-type (cons 'λωαχ *io-currying-function-symbols-dyadic*))
        `(λωα (funcall ,body omega alpha ,(cons 'list axes)))
        (if (member function-type (cons 'λωχ *io-currying-function-symbols-monadic*))
            `(λω (funcall ,body omega ,(cons 'list axes)))
            body))))

(defun of-meta-hierarchy (meta-form key symbol)
  "Fetch a combined list of symbols of a given type at each level of a closure metadata hierarchy. Used to query data collected as part of lexer postprocessing."
  (or (and (getf meta-form key) (member symbol (getf meta-form key) :test #'eql))
      (and (getf meta-form :parent) (of-meta-hierarchy (rest (getf meta-form :parent)) key symbol))))

(defmacro is-workspace-value (item)
  "Checks if a variable is present in the current workspace as a value."
  `(and (boundp (intern (string ,item) space))
        (not (fboundp (intern (string ,item) space)))))

(defmacro is-workspace-function (item)
  "Checks if a variable is present in the current workspace as a function."
  `(and (fboundp (intern (string ,item) space))
        (or (not (boundp (intern (string ,item) space)))
            (not (listp (symbol-value (intern (string ,item) space))))
            (not (getf (rest (symbol-value (intern (string ,item) space))) :valence)))))

(defmacro is-workspace-operator (item)
  "Checks if a variable is present in the current workspace as an operator."
  `(and (fboundp (intern (string ,item) space))
        (boundp (intern (string ,item) space))
        (listp (symbol-value (intern (string ,item) space)))
        (getf (rest (symbol-value (intern (string ,item) space))) :valence)))

(defun get-array-meta (array &rest keys)
  "Gets one or more metadata of an array using the displacement reference technique."
  (let ((metadata-holder (array-displacement array)))
    (if metadata-holder
        (apply #'values (loop :for key :in keys :collect (getf (aref metadata-holder 0) key))))))

(defun set-array-meta (array &rest data)
  "Sets one or more metadata of an array using the displacement reference technique."
  (let ((metadata-holder (array-displacement array)))
    (if metadata-holder (progn (loop :for (key value) :on data :by #'cddr
                                  :do (setf (getf (aref metadata-holder 0) key) value))
                               data))))

(defun array-setting-meta (array &rest data)
  "Sets one or more metadata of an array using the displacement reference technique, returning the displaced array."
  (let ((metadata-holder (array-displacement array)))
    (if metadata-holder (progn (loop :for (key value) :on data :by #'cddr
                                  :do (setf (getf (aref metadata-holder 0) key) value))
                               array)
        (let ((output)
              (meta-array (make-array (1+ (size array)) :element-type t)))
          (setf (aref meta-array 0) data
                output (make-array (dims array) :displaced-to meta-array
                                   :displaced-index-offset 1 :element-type t))
          output))))

(defun follow-path (space path &optional item)
  "Follow a path through a namespace and fetch the function or value there."
  (if (not path)
      item (if item (follow-path space (rest path)
                                 (getf item (intern (string (first path)) "KEYWORD")))
               (if (boundp (intern (string (first path)) space))
                   (follow-path space (rest path)
                                (symbol-value (intern (string (first path)) space)))))))

(let ((minimal-anchars (concatenate 'string "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                    "abcdefghijklmnopqrstuvwxyzÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑ"
                                    "ÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ")))
  (defun minimal-alphanumericp (character)
    (not (null (position character minimal-anchars)))))

(defmacro amb-ref (fn-monadic fn-dyadic &optional axes)
  "Generate a function aliasing a lexical function which may be monadic or dyadic; an ambivalent reference."
  (flet ((wrap-curried-axes (form)
           (if (not axes) form `(λχ ,form ,@axes))))
    (let ((args (gensym)) (reduced-args (gensym))
          (m-meta (if (member (first fn-monadic) '(fn-meta scalar-function))
                      (cddr fn-monadic)))
          (d-meta (if (member (first fn-dyadic) '(fn-meta scalar-function))
                      (cddr fn-dyadic))))
      `(lambda (&rest ,args)
         (if (eq :get-metadata (first ,args))
             (if (= 1 (length ,args)) ,(if m-meta (cons 'list m-meta))
                 ,(if d-meta (cons 'list d-meta)))
             (if (= 2 (length ,args))
                 (if (null (second ,args)) (a-call ,(wrap-curried-axes fn-monadic) (first ,args))
                     (a-call ,(wrap-curried-axes fn-dyadic) (first ,args) (second ,args)))
                 (if (= 3 (length ,args))
                     (if (null (second ,args))
                         (let ((,reduced-args (cons (first ,args) (cddr ,args))))
                           (apply ,(wrap-curried-axes fn-monadic) ,reduced-args))
                         (apply ,(wrap-curried-axes fn-dyadic) ,args))
                     (a-call ,(wrap-curried-axes fn-monadic) (first ,args)))))))))

(defun build-populator (array)
  "Generate a function that will populate array elements with an empty array prototype."
  (if (= 0 (size array))
      (let ((found (get-array-meta array :empty-array-prototype)))
        (if found (lambda () (copy-nested-array found))))))

(defun make-prototype-of (array)
  "Make a prototype version of an array; all values in the array will be blank spaces for character arrays or zeroes for other types of arrays."
  (if (not (eq t (element-type array)))
      (make-array (dims array) :element-type (element-type array)
                  :initial-element (if (member (element-type array) '(base-char character)) #\  0))
      (let ((output (make-array (dims array))))
        (dotimes (i (size output)) (setf (row-major-aref output i)
                                         (if (not (arrayp (row-major-aref array i)))
                                             0 (make-prototype-of (row-major-aref array i)))))
        output)))

(defmacro print-and-run (form)
  "Print a formatted code string and then run the code; used in april's arbitrary evaluation tests."
  `(let ((*print-case* :downcase))
     (princ (indent-code (write-to-string (quote ,form))))
     ,form))

(defun indent-code (string)
  "Indent a code string produced by (print-and-run) as appropriate for April's test output."
  (concatenate 'string "  * " (regex-replace-all "[\\n]" string (format nil "~%    "))))

(defun process-path (item key-list &optional processor value)
  "Generate appropriate code to fetch or change elements nested within (arrays of) namespaces."
  (if (not key-list)
      (funcall (or processor (lambda (a b) (declare (ignore b)) a))
               item value)
      (if (symbolp (first key-list))
          (if (and (listp item) (eql 'achoose (first item)))
              (let ((arg (gensym)) (key-sym (intern (string (first key-list)) "KEYWORD")))
                ;; if multiple namespaces have been fetched from an array, the function to fetch the
                ;; rest of the path must be applied over the output array with (apply-scalar)
                `(apply-scalar (lambda (,arg) ,(process-path `(getf ,arg ,key-sym)
                                                             (rest key-list) processor value))
                               ,item))
              (process-path `(getf ,item ,(intern (string (first key-list)) "KEYWORD"))
                            (rest key-list) processor value))
          (let ((this-item (gensym)) (other (gensym)))
            (if processor ;; if there's a processor, this path is being used for assignment
                (enclose-axes item (first key-list)
                              :set value :set-nil (not value)
                              :set-by `(lambda (,this-item ,other)
                                         ,(process-path this-item (rest key-list) processor other)
                                         ,this-item))
                (process-path (enclose-axes item (first key-list))
                              (rest key-list) processor value))))))

(defmacro nspath (list &rest keys)
  "Macro to expedite the fetching of values from nested namespaces."
  `(at-path ,(if (not (and (listp list) (eql 'fn-ref (first list))))
                 list (second list))
            ,(cons 'list (loop :for k :in keys
                               :collect (if (symbolp k) (intern (string k) "KEYWORD")
                                            `(mapcar (lambda (array)
                                                       (if array
                                                           (apply-scalar #'- array index-origin)))
                                                     ,(cons 'list (first k))))))))

(defun format-nspath (items &optional output)
  "Create a string representation of a namespace path from the symbol list implementing that path."
  (if (not items)
      output (let ((this-item (if (and (listp (first items))
                                       (member (caar items) '(inws inwsd)))
                                  (cadar items) (if (symbolp (first items))
                                                    (first items)))))
               (if this-item (format-nspath (rest items) (if output (format nil "~a.~a" output this-item)
                                                             (string this-item)))))))

(defun at-path (object path &key (value) (value-nil) (set-by))
  "Get or set values within a namespace (structured as a ptree), handling arrays within the namespace according to array indices within the namespace path or eliding arrays in the absence of specific coordinates."
  (if (and (not value) (not value-nil) (arrayp object) (symbolp (first path)))
      (apply-scalar (lambda (item) (at-path item path))
                    object)
      (if (= 1 (length path)) ;; path is one element long, as for a
          (if (symbolp (first path))
              (if (or value value-nil) ;; path is assigned a value, as for a←X
                  (progn (if (and object (symbolp object))
                             (if set-by (setf (getf (symbol-value object) (first path))
                                              (funcall set-by value
                                                       (getf (symbol-value object) (first path))))
                                 (setf (getf (symbol-value object) (first path)) value))
                             (if (arrayp object) ;; handle elided assignment of array elements
                                 (progn (dotimes (i (size object))
                                          (setf (getf (row-major-aref object i) (first path))
                                                (if set-by (funcall set-by value
                                                                    (getf (row-major-aref object i)
                                                                          (first path)))
                                                    value)))
                                        object)
                                 (setf (getf object (first path))
                                       (if set-by (funcall set-by value (getf object (first path)))
                                           value))))
                         object)
                  ;; path is just referenced, not assigned, as for a
                  (if (arrayp object) ;; handle elision of arrays
                      (let ((output (make-array (dims object))))
                        (dotimes (i (size output))
                          (let ((original (row-major-aref object i)))
                            (setf (row-major-aref output i)
                                  (getf (row-major-aref original i) (first path)))))
                        output)
                      (getf (if (not (symbolp object)) object (symbol-value object))
                            (first path))))
              (nth-value 1 (achoose object (first path) ;; path is a set of coordinates, as for [1]
                                    :set value :set-nil value-nil :modify-input t
                                    :set-by (or set-by (lambda (a b) (declare (ignore a)) b)))))
          (let ((object (if (symbolp object) (symbol-value object)
                            object)))
            (if (or value value-nil)
                (if (= 2 (length path)) ;; path is 2 long and assigned a value
                    (if (symbolp (first path))
                        (if (symbolp (second path)) ;; elements 1 and 2 are symbols, as for a.b←X
                            (if (arrayp object)
                                (progn (dotimes (i (size object)) ;; handle elision
                                         (setf (getf (row-major-aref object i) (first path))
                                               (at-path (getf (row-major-aref object i) (first path))
                                                        (rest path) :value value :value-nil value-nil
                                                                    :set-by set-by)))
                                       object)
                                (let ((this-object (getf object (first path))))
                                  (if (arrayp this-object) ;; handle elision
                                      (progn (at-path this-object (rest path)
                                                      :value value :value-nil value-nil :set-by set-by)
                                             this-object)
                                      (setf (getf this-object (second path)) value
                                            (getf object (first path)) this-object))))
                            (if (arrayp object) ;; second element is coords, as for a[1]←x
                                (let ((this-object object))
                                  (dotimes (i (size this-object))
                                    (setf (row-major-aref this-object i)
                                          (at-path (row-major-aref this-object i)
                                                   path :value value :value-nil value-nil
                                                        :set-by set-by)))
                                  this-object)
                                (let ((this-object (at-path (getf object (first path))
                                                            (rest path) :value value :value-nil value-nil
                                                                        :set-by set-by)))
                                  (setf (getf object (first path)) this-object)
                                  object)))
                        ;; first element is coords and second element may be symbol or coords,
                        ;; as for [1].b←X or [1][2]←X
                        (nth-value 1 (achoose object (first path)
                                              :set value :set-nil value-nil :modify-input t
                                              :set-by
                                              (lambda (a b) (at-path a (rest path) :value b
                                                                                   :set-by set-by)))))
                    ;; path is more than two elements long and assigned a value
                    (if (not (symbolp (first path)))
                        ;; first elem is array coordinates, as for [1]...←X
                        (progn (achoose object (first path)
                                        :set value :set-nil value-nil :modify-input t
                                        :set-by (if (rest path)
                                                    (lambda (a b)
                                                      (at-path a (rest path) :value b :value-nil value-nil
                                                                             :set-by set-by))))
                               object)
                        ;; first elem is a symbol, as for a...←X
                        (if (arrayp object)
                            (progn (dotimes (i (size object)) ;; handle elision
                                     (setf (getf (row-major-aref object i) (first path))
                                           (at-path (getf (row-major-aref object i) (first path))
                                                    (rest path) :value value :value-nil value-nil
                                                                :set-by set-by)))
                                   object)
                            (if (not (symbolp (second path)))
                                (setf (getf object (first path))
                                      (at-path (getf object (first path)) (rest path)
                                               :value value :value-nil value-nil :set-by set-by))
                                (at-path (getf object (first path)) (rest path)
                                         :value value :value-nil value-nil :set-by set-by)))))
                ;; path is over 1 element long and not assigned a value, like a.b, a.b.c, a[1].c, etc...
                (if (symbolp (first path)) ;; first element is a symbol as for a...
                    (if (arrayp object) (dotimes (i (size object)) ;; handle elision
                                          (at-path (getf (row-major-aref object i) (first path))
                                                   (rest path)))
                        (at-path (getf object (first path)) (rest path)))
                    ;; first element is coordinates like [1]...
                    (at-path (achoose object (first path))
                             (rest path))))))))

(defmacro a-set (symbol value &key (by) (axes))
  "This is macro is used to build variable assignment forms and includes logic for strand assignment."
  (labels ((follow-path (item path)
             (if (not path)
                 item (follow-path `(getf ,item ,(intern (string (first path)) "KEYWORD"))
                                   (rest path)))))
    (if (not (listp symbol))
        (let* ((is-symbol-value (or (symbolp value)
                                    (and (listp value)
                                         (or (member (first value) '(inws inwsd))
                                             ;; remember to duplicate an assigned symbol as well
                                             (and (eql 'a-set (first value))
                                                  (or (symbolp (second value))
                                                      (and (listp (second value))
                                                           (member (second value) '(inws inwsd)))))))))
               (ns-sym (intern "*NS-POINT*" (package-name (symbol-package symbol))))
               (namespace (if (boundp ns-sym) (symbol-value ns-sym)))
               (set-to (if (not is-symbol-value) value `(duplicate ,value))))
          ;; handle assignment of ⍺ or ⍵; ⍺-assignment sets its default value if no right argument is
          ;; present; ⍵-assignment is an error. This is handled below for strand assignments.
          (if axes (enclose-axes symbol axes :set value :set-by by)
              (if (eql '⍺ symbol) `(or ⍺ (setf ⍺ ,set-to))
                  (if (eql '⍵ symbol) `(error "The [⍵ right argument] cannot have a default assignment.")
                      (if (string= "*RNGS*" (string symbol))
                          ;; handle random seed assignments
                          (let ((valsym (gensym)) (seed (gensym))
                                (rngindex (gensym)) (rngname (gensym)))
                            `(let ((,valsym ,value))
                               (if (or (integerp ,valsym)
                                       (and (vectorp ,valsym) (= 1 (length ,valsym))))
                                   (let ((,seed (disclose-atom ,valsym)))
                                     (if (not (or (integerp ,seed)
                                                  (and (vectorp ,seed) (= 0 (length ,seed)))))
                                         (error "Random seeds set by ⎕RL←X must be integers or empty vectors.")
                                         (let ((,rngname (getf (rest ,symbol) :rng)))
                                           (setf (getf (rest ,symbol) ,rngname)
                                                 (if (eq :system ,rngname)
                                                       :system
                                                       (if (and (vectorp ,seed) (= 0 (length ,seed)))
                                                           (random-state:make-generator ,rngname)
                                                           (random-state:make-generator
                                                            ,rngname ,seed)))))))
                                   (if (and (vectorp ,valsym) (= 2 (length ,valsym)))
                                       (let* ((,seed (aref ,valsym 0)) (,rngindex (aref ,valsym 1))
                                              (,rngname (aref *rng-names* ,rngindex)))
                                         (if (not (or (integerp ,seed)
                                                  (and (vectorp ,seed) (= 0 (length ,seed)))))
                                             (error "Random seeds set by ⎕RL←X must be ~a"
                                                    "integers or empty vectors.")
                                             (setf (getf (rest ,symbol) :rng)
                                                   ,rngname
                                                   (getf (rest ,symbol) ,rngname)
                                                   (if (eq :system ,rngname)
                                                       :system
                                                       (if (and (vectorp ,seed) (= 0 (length ,seed)))
                                                           (random-state:make-generator ,rngname)
                                                           (random-state:make-generator
                                                            ,rngname ,seed))))))
                                       (error "The [⎕RL random link] value can only be set as an ~a"
                                              "integer or a 2-element vector.")))))
                          (let ((sym-package (package-name (symbol-package symbol))))
                            (if (and (listp value) (eql 'a-call (first value))
                                     (listp (second value)) (eql 'function (caadr value))
                                     (member (cadadr value) '(external-workspace-function
                                                              external-workspace-operator))
                                     (not (string= "LEX" (subseq sym-package (+ -3 (length sym-package))
                                                                 (length sym-package)))))
                                (let ((args (gensym))
                                      (other-space (concatenate 'string "APRIL-WORKSPACE-"
                                                                (first (last value)))))
                                  `(progn
                                     (proclaim '(special ,symbol))
                                     (setf (symbol-function ',symbol)
                                           (lambda (&rest ,args)
                                             (let ,(loop :for (key val) :on *system-variables* :by #'cddr
                                                         :collect (list (intern (string val) other-space)
                                                                        (intern (string val) sym-package)))
                                               (apply ,value ,args)))
                                           ,@(if (eql 'external-workspace-operator (cadadr value))
                                                 `((symbol-value ',symbol)
                                                   (symbol-value ',(intern (third value) other-space)))))))
                                (if namespace `(setf (getf ,(if (symbolp namespace)
                                                                namespace (follow-path (first namespace)
                                                                                       (rest namespace)))
                                                           ,(intern (string symbol) "KEYWORD"))
                                                     ,set-to)
                                    `(setf ,symbol ,set-to)))))))))
        (if (and (listp symbol) (eql 'nspath (first symbol)))
            ;; handle assignments within namespaces, using process-path to handle the paths
            (let ((val (gensym)))
              `(let ((,val ,value))
                 ,(if (= 3 (length symbol))
                      `(setf ,(second symbol) ,(append (macroexpand (append symbol (if axes (list axes))))
                                                       (list :value val :value-nil `(null ,val))
                                                       (if by (list :set-by by))))
                      (append (macroexpand (append symbol (if axes (list axes))))
                              (list :value val :value-nil `(null ,val))
                              (if by (list :set-by by))))))
            (if (and (listp symbol) (eql 'symbol-function (first symbol)))
                `(setf ,symbol ,value)
                (let ((symbols (if (not (eql 'avec (first symbol)))
                                   symbol (rest symbol))))
                  ;; handle multiple assignments like a b c←1 2 3
                  (labels ((process-symbols (sym-list values)
                             (let* ((this-val (gensym))
                                    (assigning-xfns (and (listp values) (eql 'a-call (first values))
                                                         (listp (second values))
                                                         (eql 'function (caadr values))
                                                         (member (cadadr values)
                                                                 '(external-workspace-function
                                                                   external-workspace-operator))))
                                    (other-space (and assigning-xfns
                                                      (concatenate 'string "APRIL-WORKSPACE-"
                                                                   (first (last values)))))
                                    (sym-package (and assigning-xfns
                                                      (package-name (symbol-package (first sym-list))))))
                               `(let ((,this-val ,values))
                                  ,@(loop :for sym :in (if (not (eql 'avec (first sym-list)))
                                                           sym-list (rest sym-list))
                                          :for sx :from 0
                                          :append
                                          (if (and (listp sym) (not (eql 'inws (first sym))))
                                              (list (process-symbols
                                                     sym `(if (not (vectorp ,this-val))
                                                              ,this-val (aref ,this-val ,sx))))
                                              (if (eql '⍺ sym)
                                                  `((or ⍺ (setf ⍺ (if (not (vectorp ,this-val))
                                                                      (disclose ,this-val)
                                                                      (aref ,this-val sx)))))
                                                  (if (eql '⍵ sym)
                                                      `(error "The [⍵ right argument] cannot ~a"
                                                              "have a default assignment.")
                                                      (if assigning-xfns
                                                          ;; handle assignment of multiple functions
                                                          ;; from another WS like fn1 fn2 ← ⎕XWF 'fn1' 'fn2'
                                                          (let ((args (gensym)))
                                                            `((proclaim '(special ,sym))
                                                              (setf (symbol-function ',sym)
                                                                    (lambda (&rest ,args)
                                                                      (let ,(loop :for (key val)
                                                                                    :on *system-variables*
                                                                                  :by #'cddr
                                                                                  :collect
                                                                                  (list (intern (string val)
                                                                                                other-space)
                                                                                        (intern (string val)
                                                                                                sym-package)))
                                                                        (apply (aref ,this-val ,sx)
                                                                               ,args))))))
                                                          `((setf ,sym (if (not (vectorp ,this-val))
                                                                           (disclose ,this-val)
                                                                           (aref ,this-val ,sx)))))))))
                                  ,this-val))))
                    (process-symbols symbols value))))))))

(defmacro a-out (form &key (print-to) (output-printed)
                             (print-assignment) (print-precision) (with-newline))
  "Generate code to output the result of APL evaluation, with options to print an APL-formatted text string expressing said result and/or return the text string as a result."
  (let ((result (gensym)) (printout (gensym))
        ;; get the symbol referencing a function passed as the output
        (function-name-value (if (and (listp form) (eql 'function (first form)))
                                 `(string (quote ,(second form)))))
        (form (if (not (and (characterp form) (of-lexicons this-idiom form :functions)))
                  form (build-call-form form))))
    `(let* ((,result ,form)
            (,printout ,(if (and (or print-to output-printed))
                            ;; don't print the results of assignment unless the :print-assignment option is set,
                            ;; as done when compiling a ⎕← expression
                            (or (and function-name-value
                                     `(concatenate 'string "∇" ,function-name-value))
                                ;; if a bare function name is to be output, prefix it with ∇
                                (and (listp form)
                                     (eql 'a-set (first form))
                                     (not print-assignment)
                                     "")
                                `(matrix-print ,result :append #\Newline
                                               :segment (lambda (n &optional s)
                                                          (count-segments n ,print-precision s))
                                               :format (lambda (n &optional s r)
                                                         (print-apl-number-string
                                                          n s ,print-precision nil r)))))))
       (declare (ignorable ,result ,printout))
       ;; TODO: add printing rules for functions like {⍵+1}
       ,(if print-to (let ((string-output `(progn (write-string ,printout ,print-to))))
                       `(progn (if (arrayp ,result)
                                   ,string-output (concatenate 'string ,string-output (list #\Newline)))
                               ,@(if with-newline
                                     `((if (not (char= #\Newline (aref ,printout (1- (size ,printout)))))
                                           (write-char #\Newline ,print-to)))))))
       ,(if output-printed (if (eq :only output-printed) printout `(values ,result ,printout))
            result))))

(defun array-to-nested-vector (array)
  "Convert an array to a nested vector. Useful for applications such as JSON conversion where multidimensional arrays must be converted to nested vectors."
  (aops:each (lambda (member) (if (not (and (arrayp member) (< 1 (rank member))))
                                  member (array-to-nested-vector member)))
             (aops:split array 1)))

(defmacro avec (&rest items)
  "This macro returns an APL vector, disclosing data within that are meant to be individual atoms."
  (let ((type))
    (loop :for item :in items :while (not (eq t type))
       :do (setq type (type-in-common type (assign-element-type (if (or (not (integerp item))
                                                                        (> 0 item))
                                                                    item (max 16 item))))))
    `(make-array (list ,(length items)) ;; enclose each array included in an APL vector
                 :element-type (quote ,type) :initial-contents (list ,@items))))

(defun parse-apl-number-string (number-string &optional component-of)
  "Parse an APL numeric string into a Lisp value, handling high minus signs, J-notation for complex numbers and R-notation for rational numbers."
  (ignore-errors ;; if number parsing fails, just return nil
    (let ((nstring (string-upcase (regex-replace-all "[_]" number-string ""))))
      (if (and (not (eql 'complex component-of))
               (find #\J nstring))
          (let ((halves (cl-ppcre:split #\J nstring)))
            (if (and (= 2 (length halves))
                     (< 0 (length (first halves)))
                     (< 0 (length (second halves))))
                (complex (parse-apl-number-string (first halves) 'complex)
                         (parse-apl-number-string (second halves) 'complex))))
          (if (find #\E nstring)
              (let ((exp-float (parse-number:parse-number (regex-replace-all "[¯]" nstring "-")
                                                          :float-format 'double-float)))
                (if (< double-float-epsilon (nth-value 1 (floor exp-float)))
                    exp-float (let ((halves (mapcar #'parse-apl-number-string (cl-ppcre:split #\E nstring))))
                                (floor (* (first halves) (expt 10 (second halves)))))))
              (if (and (not (eql 'rational component-of))
                       (find #\R nstring))
                  (let ((halves (cl-ppcre:split #\R nstring)))
                    (/ (parse-apl-number-string (first halves) 'rational)
                       (parse-apl-number-string (second halves) 'rational)))
                  ;; the macron character is converted to the minus sign
                  (parse-number:parse-number (regex-replace-all "[¯]" nstring "-")
                                             :float-format 'double-float)))))))

(defun print-apl-number-string (number &optional segments precision decimals realpart-multisegment)
  "Format a number as appropriate for APL, using high minus signs and J-notation for complex numbers, optionally at a given precision and post-decimal length for floats."
  (cond ((complexp number)
         (format nil "~aJ~a" (print-apl-number-string (realpart number)
                                                      (list (first segments)
                                                            (if (or realpart-multisegment
                                                                    (not (integerp (realpart number))))
                                                                (if (not (third segments))
                                                                    0 (- (second segments)))))
                                                      precision nil t)
                 (print-apl-number-string (imagpart number) (if (third segments)
                                                                (list (- (third segments))
                                                                      (or (fourth segments) 0))
                                                                (list (second segments) 0))
                                          precision)))
        ((integerp number)
         (let ((output (format nil (format nil "~~~d,'~ad~a" (abs (first segments))
                                           ;; for negative values, empty space to the left must initially
                                           ;; be filled with ¯ characters; the reasoning is explained below
                                           (if (> 0 number) #\¯ (if (> 0 (first segments)) #\_ #\ ))
                                           (if (not (and (second segments)
                                                         (or (> 0 (first segments))
                                                             (> 0 (second segments)))))
                                               "" (make-array (1+ (abs (second segments)))
                                                              :element-type 'base-char :initial-element
                                                              (if (and (< 0 (first segments))
                                                                       (second segments)
                                                                       (> 0 (second segments)))
                                                                  #\_ #\ ))))
                               (abs number)))
               (number-found))
           (if (> 0 number)
               ;; replace ¯ padding with zeroes or spaces as appropriate; this strange system
               ;; of initially padding with ¯ is needed because ¯ is an extended unicode character
               ;; and strings of numeric characters are rendered as base-char arrays by (format),
               ;; making it impossible to assign ¯ to their elements; unicode characters must be generated
               ;; by (format) so that the output string is of type 'character; this is also done for floats
               (loop :for i :from 1 :to (1- (length output)) :while (not number-found)
                  :do (if #+allegro (minimal-alphanumericp (aref output i))
                          #+(not allegro) (alphanumericp (aref output i))
                          (setq number-found t)
                          (setf (aref output (1- i)) #\ ))))
           output))
        ((rationalp number)
         (format nil "~ar~a" (print-apl-number-string (numerator number) (list (first segments)) precision)
                 (print-apl-number-string (denominator number) (list (- (abs (second segments)))) precision)))
        (t (let* ((number-string (first (cl-ppcre:split #\D (string-upcase (write-to-string number)))))
                  (number-sections (cl-ppcre:split #\. number-string))
                  (right-padding (if (not (and (second segments) (< 0 (second segments))))
                                     0 (max 0 (- (second segments) (length (second number-sections))))))
                  ;; space to left of decimal is expressed by the first segment
                  (left-space (abs (first segments)))
                  ;; space to right of decimal can be explicitly specified by decimal argument
                  ;; or expressed by second segment length, whose length may be expanded if the digits
                  ;; on the left don't fill out the precision, as with ⎕pp←6 ⋄ ⍪3005 0.125
                  (right-space (+ (or decimals (max 1 (- (max (abs (second segments))
                                                              (min (length (second number-sections))
                                                                   (abs (second segments))))
                                                         right-padding)))))
                  ;; total number length is left space + right space + one for decimal dot
                  (total-length (+ 1 left-space right-space))
                  (output (format nil (format nil "~~~d,~d,,,'~af~a" total-length right-space
                                              (if (> 0 number)
                                                  #\¯ (if (< 0 (first segments)) #\  #\_))
                                              (if (not (and right-padding (< 0 right-padding)))
                                                  "" (make-array right-padding :element-type 'base-char
                                                                 :initial-element #\ )))
                                  (abs number))))
             (if (> 0 number)
                 (let ((start-at (if (< 0 (first segments)) 0 1)))
                   (loop :for i :from start-at :while (char= #\¯ (aref output i))
                      :when (or (= 1 start-at) (char= #\¯ (aref output (1+ i))))
                      :do (setf (aref output i) (aref " 0" start-at)))))
             output))))

(defun format-value (idiom-name symbols element)
  "Convert a token string into an APL value, paying heed to APL's native ⍺, ⍵ and ⍬ variables."
  (cond ((string= element "⍬")
         ;; APL's "zilde" character yields a keyword the compiler translates to an empty vector
         :empty-array)
        ((or (and (char= #\" (aref element 0))
                  (char= #\" (aref element (1- (length element)))))
             (and (char= #\' (aref element 0))
                  (char= #\' (aref element (1- (length element))))))
         ;; strings are converted to Lisp strings and passed through,
         ;; unless they're one element in which case the character is disclosed
         (if (= 3 (length element))
             (aref element 1) (subseq element 1 (1- (length element)))))
        ((member element '("⍺" "⍵" "⍶" "⍹" "⍺⍺" "⍵⍵" "∇" "∇∇") :test #'string=)
         ;; alpha and omega characters are directly changed to symbols in the April package
         (values (intern element idiom-name) t))
        (t (or (parse-apl-number-string element)
               (and (char= #\⎕ (aref element 0))
                    (or (getf (rest (assoc :variable symbols))
                              (intern (string-upcase element) *package-name-string*))
                        (getf (rest (assoc :constant symbols))
                              (intern (string-upcase element) *package-name-string*))))
               (values (intern element) t)))))

(defun apl-timestamp ()
  "Generate an APL timestamp, a vector of the current year, month, day, hour, minute, second and millisecond."
  (let ((now (now)))
    (make-array 7 :element-type '(integer 0 16384)
                :initial-contents (list (year-of now) (month-of now) (day-of now) (hour-of now)
                                        (minute-of now) (second-of now) (millisecond-of now)))))

(defun process-output-vector (items)
  "Process items in a vector to be generated by the compiler, wrapping any array references in aplSymbol so that they are disclosed. This does not apply if the output vector is unitary (length 1)."
  (loop :for item :in items :collect (if (and (< 1 (length items))
                                              (listp item) (eql 'achoose (first item)))
                                         (list 'disclose item)
                                         item)))

(defun resolve-function (reference)
  "Return a function form if it's valid as a function within compiled April code."
  (if (and (listp reference)
           (or (eql 'lambda (first reference))
               (and (symbolp (first reference))
                    (macro-function (first reference))
                    (not (member (first reference)
                                 ;; TODO: this will cause a problem if a function is passed and assigned
                                 '(avec a-call apl-if a-out a-set))))))
      reference))

(defun extract-axes (process tokens &optional axes)
  "Given a list of tokens starting with axis specifications, build the code for the axis specifications to be applied to the subsequent function or value."
  (labels ((process-axis (axis)
             (multiple-value-bind (item item-props remaining)
                 (funcall process axis)
               (declare (ignore remaining))
               ;; allow either a null item (representing an elided axis) or an array
               (if (or (not item) (eq :array (first (getf item-props :type))))
                   item (error "Invalid axis.")))))
    (if (and (listp (first tokens))
             (eq :axes (caar tokens)))
        (extract-axes process (rest tokens)
                      (cons (loop :for axis :in (cdar tokens)
                               :collect (if (= 1 (length axis))
                                            (process-axis axis)
                                            (cons 'progn (mapcar #'process-axis axis))))
                            axes))
        (values axes (first tokens)
                (rest tokens)))))

(defun adjust-axes-for-index-origin (io axis-list)
  "Adjust axes passed to a function to account for the given index origin."
  (if (integerp (first axis-list))
      (- (first axis-list) io)
      (if (vectorp (first axis-list))
          (let ((ix 0)
                (output (make-array (list (length (first axis-list))))))
            (loop :for i :across (first axis-list) :do (setf (aref output ix) (- i io)
                                                             ix (1+ ix)))
            output))))

(defmacro a-call (function &rest arguments)
  "Call an APL function with one or two arguments. Compose successive scalar functions into bigger functions for more efficiency."
  (let ((arg-list (gensym "A")))
    (let ((is-scalar (or (and (second arguments)
                              (listp function) (eql 'apl-fn-s (first arguments))
                              (of-lexicons *april-idiom* (character (second arguments))
                                           :functions-scalar-dyadic))
                         (and (listp function) (eql 'apl-fn-s (first arguments))
                              (of-lexicons *april-idiom* (character (second arguments))
                                           :functions-scalar-monadic))))
          (axes-present (and (listp function) (eql 'apl-fn-s (first arguments))
                             (third function) (listp (third function))
                             (eql 'apply-scalar (first (third function))))))
      (or (join-fns `(a-call ,function ,@arguments))
          (if (and (listp function)
                   (eql 'function (first function))
                   (eql 'change-namespace (second function)))
              `(identity t))
          (progn (if (and (listp function)
                          (eql 'nspath (first function)))
                     (let* ((ns-sym (intern "*NS-POINT*" (package-name (symbol-package (second function)))))
                            (namespace (if (boundp ns-sym) (symbol-value ns-sym))))
                       (if namespace (setq function
                                           (cons 'nspath (append (if (listp namespace) namespace
                                                                     (list namespace))
                                                                 (list (intern (string (second function))
                                                                               "KEYWORD"))
                                                                 (cddr function)))))))
                 `(let ((,arg-list (list ,@arguments ,@(if axes-present (list (third function))))))
                    (apply ,@(if is-scalar (list '#'apply-scalar))
                           ,(if (not axes-present) function (butlast function 1))
                           ,arg-list)))))))

(defun join-fns (form &optional wrap)
  "Compose multiple successive scalar functions into a larger scalar function. Used to expand (a-call)."
  (if (and (listp form) (eql 'a-call (first form)))
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
                                                                      ,@(if (second args)
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
                                        (if (numberp (second args))
                                            (first args)))))
                      (if (and argument wrap)
                          (multiple-value-bind (wrapped arg-symbol)
                              (funcall wrap (append (list 'funcall function (if (numberp (first args))
                                                                                (first args) :arg))
                                                    (if (second args) (list (if (numberp (second args))
                                                                                (second args) :arg)))))
                            `(apply-scalar ,(subst arg-symbol :arg wrapped) ,argument))))))))))

#|
This is a minimalistic implementation of (a-call) that doesn't perform any function composition.
It remains here as a standard against which to compare methods for composing APL functions.

(defmacro a-call (function &rest arguments)
  `(,(if (and (listp function)
              (eql 'scalar-function (first function)))
         'apply-scalar 'funcall)
    ,function  ,@arguments))
|#

(defmacro ac-wrap (type form)
  "Wrap a function form in a function that calls it via (a-call). Used for specification of inverse scalar functions."
  (list (if (eq :m type) 'λω 'λωα)
        `(a-call ,form omega ,@(if (eq :d type) (list 'alpha)))))

(defmacro apl-fn (glyph &rest initial-args)
  "Wrap a glyph referencing a lexical function, and if more parameters are passed, use them as a list of implicit args for the primary function represented by that glyph, the resulting secondary function to be called on the argumants passed in the APL code."
  (let ((symbol (intern (concatenate 'string "APRIL-LEX-FN-" (string glyph)) *package-name-string*)))
    (if initial-args (cons symbol initial-args)
        (list 'function symbol))))

(defmacro apl-fn-s (glyph &rest initial-args)
  "Wrap a glyph referencing a scalar lexical function, with axes handled appropriately and defaulting to the (apl-fn) handling of ."
  (let ((symbol (intern (concatenate 'string "APRIL-LEX-FN-" (string glyph)) *package-name-string*))
        (args (gensym)) (axes-sym (gensym))
        (axes (if (listp (first initial-args))
                  (first initial-args))))
    (if axes `(let ((,axes-sym ,@(if axes (list axes))))
                (lambda (&rest ,args)
                  (if (eq :get-metadata (first ,args))
                      ,(append '(list :scalar t))
                      (apply-scalar ,(if (fboundp symbol) `(function ,symbol) glyph)
                                    (first ,args) (second ,args)
                                    ,axes-sym))))
        (cons 'apl-fn (cons glyph initial-args)))))

(defun build-call-form (glyph-char &optional args axes)
  "Format a function to be called within generated APL code."
  (let* ((fn-meta (handler-case (funcall (symbol-function (intern (format nil "APRIL-LEX-FN-~a" glyph-char)
                                                                  *package-name-string*))
                                         :get-metadata)
                    (error () nil)))
         (is-scalar (of-lexicons *april-idiom* glyph-char
                                 (if (eq :dyadic args) :functions-scalar-dyadic
                                     :functions-scalar-monadic))))
    (append (list (if is-scalar 'apl-fn-s 'apl-fn)
                  (intern (string glyph-char) *package-name-string*))
            (getf fn-meta :implicit-args)
            (if (and axes (or (getf fn-meta :axes)
                              (eq :dyadic args)))
                (list (if is-scalar `(apply-scalar #'- ,(caar axes) index-origin)
                          (cons 'list (first axes))))))))

(defmacro value-meta-process (form)
  "Assign array metadata appropriately to arrays resulting from scalar operations along with newly assigned arrays. Currently this is used to migrate array prototypes, as for operations like 1+0↑⊂3 3⍴⍳9."
  (let ((omega (gensym)) (prototype (gensym)) (result (gensym)))
    (cond ((eql 'setf (first form))
           (if (and (listp (third form))
                    (eql 'duplicate (first (third form))))
               (let ((to-assign (gensym)))
                 `(let ((to-assign ,(third form)))
                    (if (and (arrayp ,to-assign)
                             (= 0 (size ,omega))
                             (= 0 (size ,result)))
                        (let ((,prototype (get-array-meta ,omega :empty-array-prototype)))
                          (if ,prototype (array-setting-meta ,result :empty-array-prototype ,prototype)
                              ,result)))))))
          ((eql 'apply-scalar (first form))
           (let ((omega (gensym)) (alpha (gensym)) (result (gensym)) (prototype (gensym)))
             `(let* ((,omega ,(third form))
                     (,alpha ,(fourth form))
                     (,result ,(append (list (first form) (second form) omega alpha)
                                       (nthcdr 4 form))))
                (if (and (arrayp ,omega)
                         (= 0 (size ,omega))
                         (= 0 (size ,result)))
                    (let ((,prototype (get-array-meta ,omega :empty-array-prototype)))
                      (if ,prototype (array-setting-meta ,result :empty-array-prototype ,prototype)
                          ,result))
                    (if (and (arrayp ,alpha)
                             (= 0 (size ,alpha))
                             (= 0 (size ,result)))
                        (let ((,prototype (get-array-meta ,alpha :empty-array-prototype)))
                          (if ,prototype (array-setting-meta ,result :empty-array-prototype ,prototype)
                              ,result))
                        ,result))))))))

(defmacro a-comp (symbol &rest body)
  "A wrapper macro for macros that implement April's operators; functionally this macro does nothing but it improves the readability of April's compiled code."
  (declare (ignore symbol))
  (let ((expanded (macroexpand body)))
    (if (or (and (not (listp (first expanded)))
                 (or (not (symbolp (first expanded)))
                     (eql '∇oself (first expanded))
                     (fboundp (first expanded))))
            (and (listp (first expanded))
                 (not (eql 'olambda (caar expanded)))))
        expanded (cons 'funcall expanded))))

(defmacro apl-if (&rest each-clause)
  "Expands to an APL-style if-statement where clauses are evaluated depending on whether given expressions evaluate to 1 or 0."
  (let ((condition (gensym)))
    (labels ((build-clauses (clauses)
               `(let ((,condition (disclose-atom ,(first clauses))))
                  (if (not (is-unitary ,condition))
                      (error "Predicates within an [$ if] statement must be unitary or scalar.")
                      (if (/= 0 (disclose-atom ,condition))
                          ,(second clauses)
                          ,(if (third clauses)
                               (if (fourth clauses)
                                   (build-clauses (cddr clauses))
                                   (third clauses))
                               (make-array nil)))))))
      (build-clauses each-clause))))

(defmacro scalar-function (function &rest meta)
  "Wrap a scalar function. This is a passthrough macro used by the scalar composition system in (a-call)."
  (let ((args (gensym))
        (function (if (or (not (listp function))
                          (not (eql 'apl-fn (first function)))
                          (>= 2 (length function)))
                      function (list (first function) (second function))))
        (axes (and (listp function) (eql 'apl-fn (first function))
                   (third function))))
    `(lambda (&rest ,args)
       (if (eq :get-metadata (first ,args))
           ,(append '(list :scalar t) meta)
           (apply-scalar ,(if (not (symbolp function)) function `(function ,function))
                         (first ,args) (second ,args)
                         ,@(if axes (list axes)))))))

(defun validate-arg-unitary (value)
  "Verify that a form like (vector 5) represents a unitary value."
  (or (symbolp value)
      (numberp value)
      (and (listp value)
           (or (not (eql 'vector (first value)))
               (not (third value))))))

(defmacro or-functional-character (reference symbol)
  "Return a symbol representing a functional character or, if the passed value is not a character, an arbitrary fallback symbol. Used to derive the initial symbol argument for (a-call)."
  `(if (not (characterp ,reference))
       ,symbol (intern (string-upcase ,reference) ,*package-name-string*)))

(defun enclose-axes (body axis-sets &key (set) (set-by) (set-nil) (reference))
  "Apply axes to an array, with the ability to handle multiple sets of axes as in (6 8 5⍴⍳9)[1 4;;2 1][1;2 4 5;]."
  (let ((axes (first axis-sets))
        (assignment-output (gensym)) (assigned-array (gensym)) (to-set (gensym)))
    (if (not axis-sets)
        body (enclose-axes
              (if set `(let ((,to-set ,set))
                         (multiple-value-bind (,assignment-output ,assigned-array)
                             (achoose ,body (mapcar (lambda (array)
                                                      (if array (apply-scalar #'- array index-origin)))
                                                    (list ,@axes))
                                      :set-nil ,set-nil :set ,to-set ,@(if set-by (list :set-by set-by))
                                      ;; setting the modify-input parameter so that the original value
                                      ;; is modified in place if possible
                                      ,@(if reference (list :reference reference))
                                      :modify-input t)
                           (if ,assigned-array (setf ,body ,assigned-array))
                           ,assignment-output))
                  `(achoose ,body (mapcar (lambda (array) (if array (apply-scalar #'- array index-origin)))
                                          (list ,@axes))
                            ,@(if reference (list :reference reference))))
              (rest axis-sets)))))

(defun make-namespace (contents)
  "Create a namespace. Used to implement ⎕NS. A namespace is just a plist, so this returns nil."
  (declare (ignore contents))
  nil)

(defun change-namespace (ns-path workspace-symbol)
  "Change the current 'namespace point' within the workspace. Used to implement ⎕CS."
  (if (and (symbolp ns-path)
           (string= "_" (string ns-path)))
      (setf (symbol-value workspace-symbol) nil)
      (labels ((follow-path (item path)
                 (if path (let ((head (member (intern (string (first path)) "KEYWORD")
                                              item)))
                            (if head (follow-path (second head) (rest path))))
                     (or item t))))
        (let ((namespace (if (symbolp ns-path)
                             (if (boundp ns-path) (list ns-path)
                                 (error "Namespace does not exist."))
                             (if (listp ns-path)
                                 (or (and (follow-path (symbol-value (first ns-path)) (rest ns-path))
                                          (cons (first ns-path)
                                                (loop :for i :in (rest ns-path)
                                                      :collect (intern (string i) "KEYWORD"))))
                                     (error "Namespace does not exist."))))))
          (if namespace (setf (symbol-value workspace-symbol) namespace)
              (error "Not a valid namespace."))))))

(defun coerce-or-get-type (array &optional type-index)
  "Create an array with a numerically designated type holding the contents of the given array. Used to implement ⎕DT."
  (let ((types '((0 t) (-1 bit) (1 (unsigned-byte 2)) (2 (unsigned-byte 4))
                 (-3 (unsigned-byte 7)) (3 (unsigned-byte 8)) (-4 (unsigned-byte 15))
                 (4 (unsigned-byte 16)) (-5 (unsigned-byte 31)) (5 (unsigned-byte 32))
                 (-6 (unsigned-byte 63)) (6 (unsigned-byte 64))
                 (13 (signed-byte 8)) (14 (signed-byte 16)) (15 (signed-byte 32))
                 (-16 (signed-byte 63)) (16 (signed-byte 64)) (21 fixnum)
                 (31 short-float) (32 single-float) (34 double-float) (35 long-float)
                 (48 base-char) (49 character)))
        (type))
    (if type-index
        (progn (loop :for item :in types :when (= type-index (first item)) :do (setf type (second item)))
               (if (or (not (arrayp array))
                       (equalp type (element-type array)))
                   array (let ((output (make-array (dims array) :element-type type)))
                           (xdotimes output (i (size array)) (setf (row-major-aref output i)
                                                                   (row-major-aref array i)))
                           output)))
        (loop :for item :in types :when (equalp (element-type array) (second item)) :return (first item)))))

(defun scalar-code-char (input) ;; TODO: add left arg? 'UTF-8', 16 or 32
  "Convert Unicode characters into integers and vice versa. Used to implement ⎕UCS."
  (if (characterp input) (char-code input)
      (if (and (arrayp input) (eql 'character (element-type input)))
          (apply-scalar #'char-code input)
          (apply-scalar #'code-char input))))

(defun external-workspace-value (symbol-string &optional space-string)
  (let ((space (concatenate 'string "APRIL-WORKSPACE-" (or space-string "COMMON"))))
    (flet ((process-string (string)
             (if (boundp (intern string space))
                 (symbol-value (intern string space)))))
      (if (stringp symbol-string)
          (process-string symbol-string)
          (if (vectorp symbol-string)
              (apply #'vector (loop :for item :across symbol-string :collect (process-string item))))))))
      
(defun external-workspace-function (symbol-string &optional space-string)
  (let ((space (concatenate 'string "APRIL-WORKSPACE-" (or space-string "COMMON"))))
    (flet ((process-string (string)
             (if (fboundp (intern string space))
                 (symbol-function (intern string space)))))
      (if (stringp symbol-string)
          (process-string symbol-string)
          (if (vectorp symbol-string)
              (apply #'vector (loop :for item :across symbol-string :collect (process-string item))))))))

(defun external-workspace-operator (symbol-string &optional space-string)
  (let ((space (concatenate 'string "APRIL-WORKSPACE-" (or space-string "COMMON"))))
    (if (fboundp (intern symbol-string space))
        (symbol-function (intern symbol-string space)))))

(defun output-value (space form &optional properties closure-meta)
  "Express an APL value in the form of an explicit array specification or a symbol representing an array, supporting axis arguments."
  (labels ((enclose-symbol (item)
             ;; enclose the symbol in an (inws) form for interning in the workspace
             ;; if it isn't one of the designated idiom-native symbols
             (if (or (not (symbolp item))
                     (member item *idiom-native-symbols*))
                 item (if (or (of-meta-hierarchy closure-meta :var-syms item)
                              (not (boundp (intern (string item) space))))
                          ;; if a symbol does not represent a lexical variable within the given scope,
                          ;; it must be a dynamic variable so wrap it with ⊑
                          `(inws ,item) `(inwsd ,item))))
           (apply-props (item form-props)
             (let ((form-props (if (not (listp (first form-props)))
                                   form-props (first form-props))))
               (if (getf form-props :axes)
                   (enclose-axes (enclose-symbol item)
                                 (getf form-props :axes))
                   (enclose-symbol item)))))
    (let ((properties (reverse properties)))
      (if form (if (listp form)
                   (let ((initial (if (not (and (listp (first form))
                                                (member (first form) '(inws inwsd))))
                                      (first form) (first form))))
                     (if (member (first form) '(avec achoose inws inwsd))
                         form (if (not (or (numberp initial)
                                           (listp initial)
                                           (stringp initial)
                                           (characterp initial)
                                           (and (arrayp initial)
                                                (= 0 (size initial)))
                                           (member initial '(⍺ ⍵ ⍶ ⍹ ⍺⍺ ⍵⍵) :test #'eql)
                                           (and (not (fboundp initial))
                                                (and (symbolp initial)
                                                     (and (or (of-meta-hierarchy closure-meta
                                                                                 :var-syms initial)
                                                              (not (fboundp (intern (string initial)
                                                                                    space)))))))))
                                  (if (= 1 (length properties))
                                      (apply-props form (first properties))
                                      (mapcar #'apply-props form properties))
                                  (if (getf (first properties) :vector-axes)
                                      (enclose-axes `(avec ,@(mapcar #'apply-props form properties))
                                                    (getf (first properties) :vector-axes))
                                      `(avec ,@(mapcar #'apply-props form properties))))))
                   (if (not (numberp form))
                       (apply-props form properties)
                       form))))))

(defmacro f-lex (symbol-sets &body body)
  "Specify the lexicon for a function - this means wrapping the function's contents in a (let) form referencing external variables as appropriate."
  (destructuring-bind (ref-symbols symbols) symbol-sets
    (let ((ref-strings (loop :for sym :in ref-symbols :collect (string sym)))
          (modifier-symbols))
      `(let* ,(loop :for sym :in symbols
                 :append (let* ((membership (member (string sym) ref-strings :test #'string=))
                                (dynamic-sym (if membership (nth (- (length ref-symbols)
                                                                    (length membership))
                                                                 ref-symbols))))
                           `((,sym ,(if (not membership)
                                        sym `(if (boundp ',dynamic-sym) (symbol-value ',dynamic-sym)))))))
         (declare (ignorable ,@(append symbols modifier-symbols)))
         ,@body))))

(defun output-function (form &optional arguments closure-meta)
  "Express an APL inline function like {⍵+5}."
  (let ((arg-symbols (getf closure-meta :arg-syms))
        (assigned-vars (loop :for sym :in (getf closure-meta :var-syms)
                             :when (not (or (member sym '(⍺ nil))
                                            (member sym arguments)))
                               :collect `(inws ,sym)))
        (assigned-fns (loop :for sym :in (getf closure-meta :fn-syms)
                            :collect `(inws ,sym)))
        (assigned-ops (append (loop :for sym :in (getf closure-meta :lop-syms)
                                    :when (not (of-meta-hierarchy (rest (getf closure-meta :parent))
                                                                  :lop-syms sym))
                                      :collect `(inws ,(intern (string sym))))
                              (loop :for sym :in (getf closure-meta :pop-syms)
                                    :when (not (of-meta-hierarchy (rest (getf closure-meta :parent))
                                                                  :pop-syms sym))
                                      :collect `(inws ,(intern (string sym))))))
        (context-vars (loop :for sym :in (getf closure-meta :var-syms)
                            :when (and (not (member sym '(⍺ nil)))
                                       (not (member sym arguments))
                                       (not (of-meta-hierarchy (rest (getf closure-meta :parent))
                                                               :var-syms sym)))
                              :collect `(inwsd, sym)))
        (context-fns (loop :for sym :in (getf closure-meta :fn-syms)
                           :when (not (of-meta-hierarchy (rest (getf closure-meta :parent))
                                                         :var-syms sym))
                             :collect `(inwsd, sym)))
        (context-ops (append (loop :for sym :in (getf closure-meta :lop-syms)
                                   :when (not (of-meta-hierarchy (rest (getf closure-meta :parent))
                                                                 :lop-syms sym))
                                     :collect `(inwsd ,(intern (string sym))))
                             (loop :for sym :in (getf closure-meta :pop-syms)
                                   :when (not (of-meta-hierarchy (rest (getf closure-meta :parent))
                                                                 :pop-syms sym))
                                     :collect `(inwsd ,(intern (string sym))))))
        (arguments (if arguments (mapcar (lambda (item) `(inws ,item)) arguments))))
    ;; (print (list :as arg-symbols))
    (if (getf closure-meta :variant-niladic)
        ;; produce the plain (progn) forms used to implement function variant implicit statements
        (cons 'progn form)
        (funcall (if (not (intersection arg-symbols '(⍶ ⍹ ⍺⍺ ⍵⍵)))
                     ;; the latter case wraps a user-defined operator
                     #'identity (lambda (form) `(olambda (,(if (member '⍶ arg-symbols) '⍶ '⍺⍺)
                                                          &optional ,(if (member '⍹ arg-symbols) '⍹
                                                                         (if (member '⍵⍵ arg-symbols)
                                                                             '⍵⍵ '_)))
                                                  (declare (ignorable ,(if (member '⍶ arg-symbols) '⍶ '⍺⍺)
                                                                      ,(if (member '⍹ arg-symbols) '⍹
                                                                           (if (member '⍵⍵ arg-symbols)
                                                                               '⍵⍵ '_))))
                                                  ,form)))
                 `(alambda ,(if arguments arguments `(⍵ &optional ⍺))
                      (with (:meta :inverse (alambda ,(if arguments arguments `(⍵ &optional ⍺)) (with nil)
                                              ,(if (= 1 (length form))
                                                   (if (listp (first form))
                                                       (invert-function (first form))
                                                       '(error "This function cannot be inverted."))
                                                   '(error "This function cannot be inverted as it ~a"
                                                     "contains more than one statement.")))))
                    ,@(if (not (or context-vars context-fns context-ops
                                   assigned-vars assigned-fns assigned-ops))
                          form `((f-lex ,(list (append context-vars context-fns context-ops)
                                               (append assigned-vars assigned-fns assigned-ops))
                                   ,@form))))))))

(defmacro function-variant (assignment-clause variable-clause function-clause)
  "Evaluates one form if the initial clause evaluates to a function, the other if it's a variable. Used to implement ⍺←function cases within defns, most often ⍺←⊢."
  `(if (functionp ,assignment-clause)
       ,function-clause ,variable-clause))

(defun build-variable-declarations (input-vars space)
  "Create the set of variable declarations that begins April's compiled code."
  (loop :for var-entry :in input-vars
        :collect (let ((symbol (lisp->camel-case (first var-entry))))
                   ;; unbind functions assigned to input symbols within the workspace
                   (fmakunbound (intern symbol space))
                   (list `(inws ,(intern symbol *package-name-string*))
                         (second var-entry)))))

(defun build-compiled-code (exps workspace-symbols options system-vars vars-declared stored-refs space)
  "Return a set of compiled April expressions within the proper context."
  (let* ((branch-index (gensym "A")) (branches-sym (intern "*BRANCHES*" space))
         (tags-found (loop :for exp :in exps :when (symbolp exp) :collect exp))
         (tags-matching (loop :for tag :in (symbol-value branches-sym)
                           :when (or (and (listp tag) (member (second tag) tags-found))) :collect tag)))
    ;; create an lparallel kernel if none is present; this is done at runtime so April's compilation
    ;; doesn't entail the creation of a kernel
    (flet ((process-tags (form)
             (loop :for sub-form :in form
                :collect (if (not (and (listp sub-form) (eql 'go (first sub-form))
                                       (not (symbolp (second sub-form)))))
                             sub-form (if (integerp (second sub-form))
                                          (if (assoc (second sub-form) tags-matching)
                                              (list 'go (second (assoc (second sub-form) tags-matching))))
                                          (if (third sub-form)
                                              `(let ((,branch-index ,(third sub-form)))
                                                 (cond ,@(loop :for tag :in (second sub-form)
                                                            :counting tag :into tix
                                                            :collect `((= ,branch-index ,tix)
                                                                       (go ,tag)))))
                                              `(let ((,branch-index ,(second sub-form)))
                                                 (cond ,@(loop :for tag :in tags-matching
                                                            :when (and (listp tag)
                                                                       (member (second tag) tags-found))
                                                            :collect `((= ,branch-index ,(first tag))
                                                                       (go ,(second tag))))))))))))
      (funcall (lambda (code) (if (not (assoc :compile-only options))
                                  code `(quote ,code)))
               (if (or system-vars vars-declared)
                   `(in-april-workspace ,(or (second (assoc :space options)) 'common)
                      (let (,@(loop :for var :in system-vars
                                 :when (not (member (string-upcase (first var)) workspace-symbols
                                                    :test #'string=))
                                 :collect var)
                            ,@vars-declared)
                        (declare (ignorable ,@(loop :for var :in system-vars
                                                 :when (not (member (string-upcase (first var))
                                                                    workspace-symbols :test #'string=))
                                                 :collect (first var))))
                        (symbol-macrolet ,(loop :for var :in system-vars
                                             :when (member (string-upcase (first var)) workspace-symbols
                                                           :test #'string=)
                                             :collect var)
                          ,@(loop :for ref :in stored-refs
                               :collect (list (first ref)
                                              (list 'inws (second ref)) (third ref)))
                          ,@(if (or (not tags-found) (not (boundp branches-sym)))
                                exps `((tagbody ,@(butlast (process-tags exps) 1))
                                       ,(first (last exps)))))))
                   (if (< 1 (length exps))
                       (cons 'progn exps) (first exps)))))))

(defun lexer-postprocess (tokens idiom space &optional closure-meta-form)
  "Process the output of the lexer, assigning values in the workspace and closure metadata as appropriate. Mainly used to process symbols naming functions and variables."
  ;; this function is used to initialize function and variable references in the workspace and tabulate
  ;; those references for each closure, along with generating implicit statements for guards and ⍺←function
  (labels ((implicit-statement-process (form-content form-meta)
             ;; reconstruct function content implementing implicit statements, like the if-statements
             ;; implied by guards and the type-dependent forking structure implied by ⍺←function
             (let ((processed) (guard-encountered) (agets-encountered))
               (loop :for item :in form-content :for ix :from 0 :while (and (not guard-encountered)
                                                                            (not agets-encountered))
                     :do (let ((guard-sym-index)
                               (processed-form (lexer-postprocess item idiom space form-meta)))
                           (loop :for d :in processed-form :for dx :from 0
                                 :when (and (listp d) (eq :fn (first d))
                                            (characterp (second d)) (char= #\: (second d)))
                                   :do (setq guard-sym-index dx)
                                 :when (and (= dx (- (length processed-form) 2))
                                            (and (listp d) (eq :fn (first d))
                                                 (characterp (second d)) (char= #\← (second d))
                                                 (not (and (= 3 (length processed-form))
                                                           (numberp (nth (1- dx) processed-form))))
                                                 (let ((assigned (nth (1- dx) processed-form))
                                                       (isyms (idiom-symbols idiom)))
                                                   (or (and (symbolp assigned)
                                                            (not (keywordp assigned))
                                                            (not (member assigned
                                                                         (rest (assoc :variable isyms))))
                                                            (not (member assigned
                                                                         (rest (assoc :constant isyms)))))
                                                       (and (listp assigned) (eql :fn (first assigned)))))))
                                   :do (setq agets-encountered t)
                                 :when (and agets-encountered (= dx (1- (length processed-form)))
                                            (not (eql '⍺ d)))
                                   :do (setq agets-encountered nil))
                           (if agets-encountered ;; handle ⍺← implicit statements
                               (push (list (list :axes (list processed-form)
                                                 (list (list (list :fn (list :meta :variant-niladic t
                                                                                   :parent form-meta)
                                                                   (implicit-statement-process
                                                                    (nthcdr (1+ ix) form-content)
                                                                    form-meta))))
                                                 (list (list (list :fn (list :meta :fn-syms '(⍺) 
                                                                                   :variant-niladic t
                                                                                   :parent form-meta)
                                                                   (implicit-statement-process
                                                                    (nthcdr (1+ ix) form-content)
                                                                    form-meta)))))
                                           (list :st :unitary #\⍢))
                                     processed)
                               (if (not guard-sym-index) (push processed-form processed)
                                   ;; handle guard composition
                                   (progn (setq guard-encountered t)
                                          (push (list (list :axes (list (nthcdr (1+ guard-sym-index)
                                                                                processed-form))
                                                            (list (loop :for e :in processed-form
                                                                        :for ex :from 0
                                                                        :while (< ex guard-sym-index)
                                                                        :collect e))
                                                            (implicit-statement-process
                                                             (nthcdr (1+ ix) form-content)
                                                             form-meta))
                                                      (list :st :unitary #\$))
                                                processed))))))
               (reverse processed)))
           (process-split-sym (symbol)
             (let ((split-segments (cl-ppcre:split "[.]" (string symbol))))
               (if (and (= 2 (length split-segments))
                        (or (fboundp (intern (first split-segments) space))
                            (member (intern (first split-segments) *package-name-string*)
                                    '(⍺⍺ ⍵⍵ ∇ ∇∇))
                            (member (intern (first split-segments) *package-name-string*)
                                    (getf (rest closure-meta-form) :fn-syms))))
                   (list (intern (second split-segments) *package-name-string*)
                         '(:op :pivotal #\.)
                         (intern (first split-segments) *package-name-string*))
                   (cons :pt (mapcar (lambda (item) (intern item *package-name-string*))
                                     split-segments)))))
           (is-product-operator (form)
             (and (listp form) (eq :op (first form)) (eq :pivotal (second form))
                  (char= #\. (third form)))))

    ;; search each list of tokens for elements of namespace paths like aaa.bb.ccc[1].dd.e
    ;; and combine them into path tokens of the form (:pt ...) if found
    (if (listp tokens)
        (let* ((found) (path-contents) (in-path) (new-tokens)
               (processed (loop :for rest-t :on tokens :by #'rest
                                :collect (let ((i (first rest-t)))
                                           (if (and (symbolp i) (position #\. (string i)))
                                               (setq found (process-split-sym i))
                                               (if (and (symbolp i) (is-product-operator (second rest-t))
                                                        (listp (third rest-t))
                                                        (eq :axes (first (third rest-t))))
                                                   (setq found i)))))))
          (labels ((process-token-sets (rest-t rest-p)
                     (if rest-t
                         (let ((tk (first rest-t)) (pr (first rest-p)) (skip-next))
                           (if pr (if (and (listp pr) (eq :pt (first pr)))
                                      (let ((next-p (second rest-p)))
                                        (setq in-path t)
                                        (loop :for pelem :in (reverse (rest pr))
                                              :do (push pelem path-contents))
                                        ;; finish processing this path if the next token is also a path,
                                        ;; i.e. for successive namespace refs like 5×myns.a myns.b
                                        (if (and (listp next-p) (eq :pt (first next-p)))
                                            (progn (setq in-path nil)
                                                   (push (cons :pt path-contents) new-tokens)
                                                   (setq path-contents nil))))
                                      (progn (setq in-path t)
                                             (push pr path-contents)))
                               (if in-path (if (and (is-product-operator tk)
                                                    (listp (second rest-t)) (eq :axes (caadr rest-t)))
                                               (progn (push (second rest-t) path-contents)
                                                      (setq skip-next t))
                                               (progn (setq in-path nil)
                                                      (push (cons :pt path-contents) new-tokens)
                                                      (setq path-contents nil)
                                                      (push tk new-tokens)))
                                   (push tk new-tokens)))
                           (if skip-next (process-token-sets (cddr rest-t) (cddr rest-p))
                               (process-token-sets (rest rest-t) (rest rest-p)))))))
            (if found (progn (process-token-sets tokens processed)
                             (if in-path (push (cons :pt path-contents) new-tokens))
                             (setq tokens (reverse new-tokens)))))))

    (symbol-macrolet ((closure-meta (rest closure-meta-form)))
      (match tokens
        ((list (guard axes-form (and (listp axes-form) (eq :axes (first axes-form))))
               (guard fn-form (and (listp fn-form) (member (first fn-form) '(:fn :op))))
               '(:fn #\←) (guard symbol (and (symbolp symbol) (not (member symbol '(⍺⍺ ⍵⍵))))))
         ;; handle function currying with axes, like ax←,[1.5]
         (if (eq :op (first fn-form))
             (let ((valence (second fn-form)))
               (if closure-meta (if (not (member symbol (getf closure-meta
                                                              (if (eq :lateral valence)
                                                                  :lop-syms :pop-syms))))
                                    (push symbol (getf closure-meta (if (eq :lateral valence)
                                                                        :lop-syms :pop-syms))))
                   (progn (if (is-workspace-value symbol)
                              (makunbound (intern (string symbol) space)))))
               (list axes-form fn-form '(:fn #\←) symbol))
             (progn (if closure-meta (push symbol (getf closure-meta :fn-syms))
                        (progn (if (is-workspace-value symbol)
                                   (makunbound (intern (string symbol) space)))
                               (setf (symbol-function (intern (string symbol) space))
                                     #'dummy-nargument-function)))
                    (let ((each-axis (rest axes-form)))
                      ;; if the symbol is already bound as a regular function, unbind it
                      (list (cons :axes (loop :for item :in each-axis
                                              :collect (lexer-postprocess item idiom
                                                                          space closure-meta-form)))
                            fn-form '(:fn #\←) symbol)))))
        ((list (guard fn-form (and (listp fn-form) (member (first fn-form) '(:fn :op))))
               '(:fn #\←) (guard symbol (and (symbolp symbol) (not (member symbol '(⍺ ⍺⍺ ⍵⍵))))))
         ;; handle function assignments like fn←{⍵+5} or aliasing like fn←+
         (if (characterp (second fn-form))
             (progn (if closure-meta (push symbol (getf closure-meta :fn-syms))
                        (progn (if (is-workspace-value symbol)
                                   (makunbound (intern (string symbol) space)))
                               (setf (symbol-function (intern (string symbol) space))
                                     #'dummy-nargument-function)))
                    (list fn-form '(:fn #\←) symbol))
             (if (eq :op (first fn-form)) ;; handle operator aliases like x←⍤
                 (let ((valence (second fn-form)))
                   (if closure-meta (if (not (member symbol (getf closure-meta
                                                                  (if (eq :lateral valence)
                                                                      :lop-syms :pop-syms))))
                                        (push symbol (getf closure-meta (if (eq :lateral valence)
                                                                            :lop-syms :pop-syms))))
                       (progn (if (is-workspace-value symbol)
                                  (makunbound (intern (string symbol) space)))))
                   (list fn-form '(:fn #\←) symbol))
                 (if (listp (second fn-form))
                     (let ((fn-meta (second fn-form)))
                       (if (and fn-meta closure-meta-form)
                           (setf (getf (rest fn-meta) :parent) closure-meta-form))
                       (let* ((form-content (if (listp (third fn-form)) (third fn-form)))
                              (processed (implicit-statement-process form-content fn-meta))
                              (is-operator (intersection (getf (rest fn-meta) :arg-syms)
                                                         '(⍶ ⍹ ⍺⍺ ⍵⍵)))
                              (valence (if is-operator (if (intersection is-operator '(⍹ ⍵⍵))
                                                           :pivotal :lateral)))
                              (int-symbol (if is-operator (intern (string symbol) space))))
                         (if is-operator (progn (setf (getf (rest fn-meta) :valence) valence)
                                                (if (getf (rest fn-meta) :valence-setters)
                                                    (loop :for setter :in (getf (rest fn-meta)
                                                                                :valence-setters)
                                                          :do (funcall setter valence)))
                                                (setf (getf (rest fn-meta) :valence-setters) nil)))
                         (if closure-meta (if is-operator
                                              (if (not (member symbol (getf closure-meta
                                                                            (if (eq :lateral valence)
                                                                                :lop-syms :pop-syms))))
                                                  (push symbol (getf closure-meta (if (eq :lateral valence)
                                                                                      :lop-syms :pop-syms))))
                                              (if (not (member symbol (getf closure-meta :fn-syms)))
                                                  (push symbol (getf closure-meta :fn-syms))))
                             (progn (if (is-workspace-value symbol)
                                        (makunbound (intern (string symbol) space)))
                                    (if int-symbol (progn (if (not (fboundp int-symbol))
                                                              (setf (symbol-function int-symbol)
                                                                    #'dummy-operator))
                                                          (set (intern (string int-symbol) space) fn-meta))
                                        (if (not (fboundp (intern (string symbol) space)))
                                            (setf (symbol-function (intern (string symbol) space))
                                                  #'dummy-nargument-function)))))
                         (list (list (if is-operator :op :fn) fn-meta processed)
                               '(:fn #\←) (lexer-postprocess symbol idiom space closure-meta-form))))))))
        ((list form '(:fn #\←) (guard symbol (and (symbolp symbol) (not (member symbol '(⍵ ⍺))))))
         ;; handle other types of function assignment
         (if (symbolp form)
             (if (or (and closure-meta (member form '(⍺⍺ ⍵⍵)))
                     (and closure-meta (of-meta-hierarchy closure-meta :fn-syms form))
                     (fboundp (intern (string form) space)))
                 (if closure-meta (push symbol (getf closure-meta :fn-syms))
                     (progn (setf (symbol-function (intern (string symbol) space))
                                  #'dummy-nargument-function)
                            (set (intern (string symbol) space) (list :meta))))
                 ;; TODO: add cases for dynamically bound operators
                 (if (and closure-meta (of-meta-hierarchy closure-meta :lop-syms form))
                     (if closure-meta (push symbol (getf closure-meta :lop-syms))
                         (progn (setf (symbol-function (intern (string symbol) space))
                                      #'dummy-operator)
                                (set (intern (string symbol) space) (list :meta :valence :lateral))))
                     (if (and closure-meta (of-meta-hierarchy closure-meta :pop-syms form))
                         (if closure-meta (push symbol (getf closure-meta :pop-syms))
                             (progn (setf (symbol-function (intern (string symbol) space))
                                          #'dummy-operator)
                                    (set (intern (string symbol) space) (list :meta :valence :pivotal))))
                         (if closure-meta (push symbol (getf closure-meta :var-syms))
                             (fmakunbound symbol)))))
             (if closure-meta (if (not (member symbol (getf closure-meta :var-syms)))
                                  (push symbol (getf closure-meta :var-syms)))
                 (fmakunbound symbol)))
         (list (lexer-postprocess form idiom space closure-meta-form)
               '(:fn #\←) (lexer-postprocess symbol idiom space closure-meta-form)))
        ((guard fn-form (and (listp fn-form) (eq :fn (first fn-form))))
         ;; handle function forms like {⍵+5} by themselves, as when called inline
         (let* ((form-meta (if (listp (second fn-form)) (second fn-form)))
                (form-content (if (listp (third fn-form)) (third fn-form)))
                (processed (implicit-statement-process form-content form-meta))
                (is-operator (intersection (getf (rest form-meta) :arg-syms)
                                           '(⍶ ⍹ ⍺⍺ ⍵⍵))))
           (if (and form-meta closure-meta-form)
               (setf (getf (rest form-meta) :parent) closure-meta-form))
           (if (not (and form-meta form-content))
               tokens (list (if is-operator :op :fn)
                            (second fn-form) processed))))
        ((guard fn-form (and (listp fn-form) (eq :op (first fn-form))))
         ;; handle operators like {⍺⍺/3×⍵} by themselves, as when called inline
         (let ((form-meta (if (listp (second fn-form)) (second fn-form)))
               (form-content (if (listp (third fn-form)) (third fn-form))))
           (if (and form-meta closure-meta-form)
               (setf (getf (rest form-meta) :parent) closure-meta-form))
           (if (not (and form-meta form-content))
               tokens (list (first fn-form) (second fn-form)
                            (loop :for item :in form-content
                                  :collect (lexer-postprocess item idiom space form-meta))))))
        ((list (guard axes-form (and (listp axes-form) (eq :axes (first axes-form))))
               axes-of)
         ;; handle sets of axes like [1;2;3]
         (let ((each-axis (rest axes-form)))
           ;; if the symbol is already bound as a regular function, unbind it
           (list (cons :axes (loop :for item :in each-axis
                                   :collect (lexer-postprocess item idiom space closure-meta-form)))
                 axes-of)))
        ((guard list (and (listp list) (not (member (first list) '(inws inwsd)))))
         ;; handle closures like (1,⍳5)
         (labels ((process-symbols (possible-symbols &optional top-level)
                    (let ((symbols-valid t))
                      (if (or top-level (loop :for item :in possible-symbols
                                              :always (or (symbolp item) (listp item))))
                          (progn (loop :for item :in possible-symbols :while symbols-valid
                                       :do (if (and (symbolp item) (not (member item '(⍺ ⍵ ⍶ ⍹ ⍺⍺ ⍵⍵ ∇ ∇∇))))
                                               (if closure-meta
                                                   (if (not (member item (getf closure-meta :var-syms)))
                                                       (push item (getf closure-meta :var-syms))))
                                               (if (listp item) (or (process-symbols item)
                                                                    (setq symbols-valid nil))
                                                   (setq symbols-valid nil))))
                                 symbols-valid)))))
           (let* ((assignment-indices)
                  (items (loop :for item :in list :for ix :from 0
                               :do (if (and (listp item) (eq :fn (first item)) 
                                            (characterp (second item)) (char= #\← (second item)))
                                       (push ix assignment-indices))
                               :collect (lexer-postprocess item idiom space closure-meta-form))))
             (loop :for index :in (reverse assignment-indices)
                   :do (process-symbols (nthcdr (1+ index) list) t))
             items)))
        ((guard symbol (eql symbol '∇∇))
         ;; handle the ∇∇ symbol, assigning its valence as appropriate
         (let ((this-form (list :op :valence '∇∇)))
           (push (lambda (valence) (setf (second this-form) valence))
                 (getf closure-meta :valence-setters))
           this-form))
        ((guard symbol (member symbol '(⍺ ⍵ ⍶ ⍹ ⍺⍺ ⍵⍵)))
         ;; handle argument symbols, adding them to the closure-meta list
         (if (and closure-meta (not (member symbol (getf closure-meta :arg-syms))))
             (push symbol (getf closure-meta :arg-syms)))
         symbol)
        ;; handle any other token
        (token-or-tokens token-or-tokens)))))

(defun get-assigned-symbols (tokens space &optional token-list is-nested in-assignment-context)
  "Find a list of symbols within a token list which are assigned with the [← gets] lexical function. Used to find lists of variables to hoist in lambda forms."
  (let ((previous-token)
        (token-list (or token-list (list :tokens))))
    (loop :for token :in tokens
       :do (if (and previous-token (listp previous-token)
                    (eql :fn (first previous-token))
                    (characterp (second previous-token))
                    (char= #\← (second previous-token)))
               ;; once a ← is encountered, we're in an assignment context; symbols found after this point may
               ;; be added to the list to hoist if eligible
               (setq in-assignment-context t))
         (if (and (listp token) (member (first token) '(:fn :op)))
             (setq in-assignment-context nil))
         (if (and (listp token) (not (member (first token) '(:fn :op))))
             ;; recurse into lists, but not functions contained within a function, otherwise something
             ;; like {÷{⍺⍺ ⍵}5} will be read as an operator because an inline operator is within it;
             ;; if an assignment context, that will be passed down to the next level of recursion
             ;; as for a (b c)←⍵
             (get-assigned-symbols token space token-list t in-assignment-context)
             (if (and in-assignment-context (symbolp token) (not (keywordp token)))
                 (cond ((and (not (member token *idiom-native-symbols*))
                             (not (member token token-list)))
                        (setf (rest token-list)
                              (cons token (rest token-list)))))))
         (setq previous-token token))
    ;; TODO: write tests to ensure variable hoisting is done properly?
    (if is-nested token-list (remove-duplicates (rest token-list)))))

(defun invert-function (form &optional (to-wrap #'identity))
  "Invert a function expression. For use with the [⍣ power] operator taking a negative right operand."
  (match form
    ((list* 'a-call function-form arg1 arg2-rest)
     (if (listp function-form)
         (let ((arg2 (first arg2-rest)))
           (if (not arg2) (if (and (listp arg1) (eql 'a-call (first arg1)))
                              (invert-function arg1 (lambda (form)
                                                      `(a-call (inv-fn ,function-form)
                                                               ,(funcall to-wrap form))))
                              `(a-call (inv-fn ,function-form) ,(funcall to-wrap arg1)))
               (if (not (or (and (listp arg1) (eql 'a-call (first arg1)))
                            (and (listp arg2) (eql 'a-call (first arg2)))))
                   `(a-call (inv-fn ,function-form ,@(if arg2 (list t))
                                    ,(if (and (eql arg2 '⍵) (not (member arg1 '(⍵ ⍺))))
                                         :inverse-right :inverse))
                            ,@(list (if (not (member arg1 '(⍵ ⍺)))
                                        arg1 (funcall to-wrap arg1))
                                    (if (not (member arg2 '(⍵ ⍺)))
                                        arg2 (funcall to-wrap arg2))))
                   (if (and (listp arg1) (eql 'a-call (first arg1)))
                       (if (and (listp arg2) (eql 'a-call (first arg2)))
                           `(a-call (inv-fn ,function-form t :inverse)
                                    ,(invert-function arg1)
                                    ,(invert-function arg2))
                           (if (eql '⍵ arg2)
                               `(a-call (inv-fn ,function-form t :inverse-right)
                                        ,(invert-function arg1) ,arg2)
                               (invert-function
                                arg1 (lambda (form)
                                       `(a-call (inv-fn ,function-form ,@(if arg2 (list t))
                                                        ,(if (and (eql arg2 '⍺)
                                                                  (not (member form '(⍵ ⍺))))
                                                             :inverse-right :inverse))
                                                ,(funcall to-wrap form)
                                                ,arg2)))))
                       (if (and (listp arg2) (eql 'a-call (first arg2)))
                           (if (eql '⍵ arg1)
                               `(a-call (inv-fn ,function-form t :inverse-right)
                                        ,arg1 ,(invert-function arg2))
                               (invert-function
                                arg1 (lambda (form)
                                       `(a-call (inv-fn ,function-form ,@(if arg2 (list t))
                                                        ,(if (and (member form '(⍵ ⍺))
                                                                  (not (eql arg1 '⍺)))
                                                             :inverse-right :inverse))
                                                ,arg1 ,(funcall to-wrap form))))))))))))))

(defun process-fnspecs (spec-sets)
  "Process a set of function and operator specs, generating lists of their referring characters, recording counts of functions and operators and building their assignment forms."
  (let ((assignment-forms) (symbol-set)
        (fn-count 0) (op-count 0) (args (gensym))
        (lexicons (list :functions nil :functions-monadic nil :functions-dyadic nil :functions-symbolic nil
                        :functions-scalar-monadic nil :functions-scalar-dyadic nil
                        :operators nil :operators-lateral nil :operators-pivotal nil
                        :operators-unitary nil :statements nil)))
    (flet ((wrap-meta (type form metadata &optional is-not-ambivalent)
             (if (not metadata) form
                 (if (and (listp form) (eql 'scalar-function (first form)))
                     (funcall (if (not (and is-not-ambivalent (eql 'scalar-function (first form))))
                                  ;; enclose non-ambivalent scalar functions in an (a-call)
                                  ;; wrapper so they are correctly applied over arrays
                                  #'identity (lambda (fn-form)
                                               `(lambda (&rest ,args)
                                                  (a-call ,fn-form (first ,args)
                                                          ,@(if (eq :dyadic type) `((second ,args)))))))
                              (append form metadata))
                     `(fn-meta ,form ,@metadata))))
           (wrap-implicit (implicit-args optional-implicit-args primary-meta form)
             (let ((axis-arg (getf primary-meta :axes)))
               (if (not (or implicit-args optional-implicit-args axis-arg))
                   form `(lambda ,(cons (first implicit-args)
                                        (cons '&optional (append (rest implicit-args)
                                                                 (if axis-arg (list axis-arg))
                                                                 optional-implicit-args)))
                           (if (eq :get-metadata ,(first implicit-args))
                               (quote ,primary-meta)
                               ,form))))))
      (macrolet ((push-char-and-aliases (&rest lexicons)
                   `(progn ,@(loop :for lexicon :in lexicons
                                   :collect `(push glyph-char (getf lexicons ,lexicon)))
                           (if (getf props :aliases)
                               (loop :for alias :in (getf props :aliases)
                                     :do (let ((a-char (aref (string alias) 0)))
                                           ,@(loop :for lexicon :in lexicons
                                                   :collect `(push a-char (getf lexicons ,lexicon)))))))))
        (loop :for each-spec :in spec-sets
              :do (loop :for spec :in (reverse (cddr each-spec))
                        :do (destructuring-bind (glyph-sym props implementation &rest rest) spec
                              (let* ((spec-type (intern (string (first each-spec))))
                                     (props (rest props))
                                     (glyph-char (aref (string glyph-sym) 0))
                                     (item-type (intern (string (first implementation))))
                                     (spec-meta (rest (assoc 'meta rest)))
                                     (primary-metadata (rest (assoc 'primary spec-meta)))
                                     (implicit-args (getf primary-metadata :implicit-args))
                                     (optional-implicit-args (getf primary-metadata :optional-implicit-args))
                                     (fn-symbol (intern (format nil "APRIL-LEX-~a-~a"
                                                                (if (eql 'statements spec-type)
                                                                    "ST" (if (eql 'operators spec-type)
                                                                             "OP" "FN"))
                                                                glyph-sym)))
                                     (assigned-form))
                                (push fn-symbol symbol-set)
                                (if (getf props :aliases)
                                    (loop :for alias :in (getf props :aliases)
                                          :do (let ((alias-symbol
                                                      (intern (format nil "APRIL-LEX-~a-~a"
                                                                      (if (eql 'operators spec-type) "OP" "FN")
                                                                      alias))))
                                                (push alias-symbol symbol-set))))
                                (case item-type
                                  (monadic (incf fn-count)
                                   (push-char-and-aliases :functions :functions-monadic)
                                   (if (eql 'scalar-function (caadr implementation))
                                       (push-char-and-aliases :functions-scalar-monadic))
                                   (push (setq assigned-form
                                               (funcall
                                                (lambda (form)
                                                  (wrap-implicit implicit-args
                                                                 optional-implicit-args
                                                                 primary-metadata form))
                                                (wrap-meta :monadic (second implementation)
                                                           (rest (assoc 'monadic spec-meta)) t)))
                                         assignment-forms))
                                  (dyadic (incf fn-count)
                                   (push-char-and-aliases :functions :functions-dyadic)
                                   (if (eql 'scalar-function (caadr implementation))
                                       (push-char-and-aliases :functions-scalar-dyadic))
                                   (push (setq assigned-form
                                               (funcall
                                                (lambda (form)
                                                  (wrap-implicit implicit-args
                                                                 optional-implicit-args
                                                                 primary-metadata form))
                                                (wrap-meta :dyadic (second implementation)
                                                           (rest (assoc 'dyadic spec-meta)) t)))
                                         assignment-forms))
                                  (ambivalent (incf fn-count 2)
                                   (push-char-and-aliases :functions :functions-monadic :functions-dyadic)
                                   (if (eql 'scalar-function (caadr implementation))
                                       (push-char-and-aliases :functions-scalar-monadic))
                                   (if (eql 'scalar-function (caaddr implementation))
                                       (push-char-and-aliases :functions-scalar-dyadic))
                                   (push (setq assigned-form
                                               (funcall
                                                (lambda (form)
                                                  (wrap-implicit implicit-args
                                                                 optional-implicit-args
                                                                 primary-metadata form))
                                                `(amb-ref ,(wrap-meta
                                                            :monadic (second implementation)
                                                            (rest (assoc 'monadic spec-meta)))
                                                          ,(wrap-meta :dyadic (third implementation)
                                                                      (rest (assoc 'dyadic spec-meta))))))
                                         assignment-forms))
                                  (symbolic (incf fn-count)
                                   (push-char-and-aliases :functions :functions-symbolic)
                                   (if (getf props :aliases)
                                       (loop :for alias :in (getf props :aliases)
                                             :do (let ((a-char (aref (string alias) 0)))
                                                   (push a-char (getf lexicons :functions))
                                                   (push a-char (getf lexicons :functions-symbolic)))))
                                   (push (setq assigned-form (second implementation))
                                         assignment-forms))
                                  (lateral (incf op-count)
                                   (push-char-and-aliases :operators :operators-lateral)
                                   (push (second implementation) assignment-forms))
                                  (pivotal (incf op-count)
                                   (push-char-and-aliases :operators :operators-pivotal)
                                   (push (second implementation) assignment-forms))
                                  (unitary (incf op-count)
                                   (push-char-and-aliases :operators :operators-unitary :statements)
                                   (push (second implementation) assignment-forms)))
                                (push `(,(if (and (eql 'functions spec-type)
                                                  (eql 'symbolic item-type))
                                             'symbol-value 'symbol-function)
                                        (quote ,fn-symbol))
                                      assignment-forms)
                                (if (getf props :aliases)
                                    (loop :for alias :in (getf props :aliases)
                                          :do (push assigned-form assignment-forms)
                                              (push `(,(if (and (eql 'functions spec-type)
                                                                (eql 'symbolic item-type))
                                                           'symbol-value 'symbol-function)
                                                      (quote ,(intern (format nil "APRIL-LEX-FN-~a" alias))))
                                                    assignment-forms)))))))
        (values lexicons (list `(proclaim '(special ,@symbol-set))
                               (cons 'setf assignment-forms))
                (list :fn-count fn-count :op-count op-count))))))

(defmacro specify-demo (title params &rest sections)
  "This macro is used to specify a set of information and tests for an April demo package, currently used for some of those found in the /demos folder."
  (let ((params (rest params)))
    `(defmacro ,(intern "RUN-TESTS" (package-name *package*)) ()
       '(progn (format t "~a ｢~a｣" ,title ,(package-name *package*))
         (princ #\Newline)
         ,@(if (getf params :description)
               `((format t "~a~%~%" ,(getf params :description))))
         ,@(if (assoc :tests sections)
               (let* ((test-count 0)
                      (items (loop :for item :in (rest (assoc :tests sections))
                                   :append (case (intern (string-upcase (first item)) "KEYWORD")
                                             (:provision `((format t "  ] ~a~%" ,(second item))
                                                           (april ,(second item))))
                                             (:is (incf test-count)
                                              `((format t "  _ ~a" ,(second item))
                                                (is (april ,(second item))
                                                    ,(third item) :test #'equalp)))))))
                 `((setq prove:*enable-colors* nil)
                   (plan ,test-count)
                   (with-april-context ((:space ,(getf params :space)))
                     ,@items)
                   (finalize)
                   (setq prove:*enable-colors* t)
                   (format t "~%~%"))))))))

;; a secondary package containing tools for the extension of April idioms
(defpackage #:april.idiom-extension-tools
  (:import-from :april #:extend-vex-idiom #:process-fnspecs #:scalar-function)
  (:export #:extend-vex-idiom #:process-fnspecs #:scalar-function #:λω #:λωα #:λωχ #:λωαχ))

;; a secondary package containing tools for specifying April demo packages
(defpackage #:april.demo-definition-tools
  (:import-from :april #:specify-demo)
  (:export #:specify-demo))
