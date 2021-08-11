;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:April -*-
;;;; utilities.lisp

(in-package #:april)

"Utility functions for April. It's important to understand the difference between the functions and macros provided here and the ones that come from the aplesque package. The faculties provided by aplesque reflect features of APL, but they have uses other than implementing APL. The material here is specifically for use in implementing APL, with uses aside from an APL implementation not accounted for in its design. The functions here are used to implement language mechanics as opposed to functions in the language's standard library; the latter are implemented in library.lisp."

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

(defvar *demo-packages* '(april-demo.cnn april-demo.dfns.array
                          april-demo.dfns.graph april-demo.dfns.numeric))

(defvar ∇ nil)
(defvar ∇∇ nil)
;; set ∇ and ∇∇ to nil; this prevents errors when they are seen in operator compositions

(defvar *digit-vector* "0123456789")

(defvar *alphabet-vector* "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defvar *idiom-native-symbols* '(⍺ ⍵ ⍶ ⍹ ⍺⍺ ⍵⍵ ∇ ∇∇ index-origin print-precision *digit-vector*
                                 *alphabet-vector* *apl-timestamp* to-output output-stream))

(defvar *system-variables* '(:index-origin *index-origin* :print-precision *print-precision*
                             :comparison-tolerance *comparison-tolerance* :division-method *division-method*))

(defun make-threading-kernel-if-absent ()
  (if (not lparallel:*kernel*)
      (setq lparallel:*kernel* (setq *april-parallel-kernel*
                                     (lparallel:make-kernel (1- (cl-cpus:get-number-of-processors))
                                                            :name "april-language-kernel")))))

(let ((this-package (package-name *package*)))
  (defmacro in-april-workspace (name &body body)
    "Macro that interns symbols in the current workspace; works in tandem with 𝕊 reader macro."
    (let* ((space-name (concatenate 'string "APRIL-WORKSPACE-" (string-upcase name)))
           (lex-space-name (concatenate 'string space-name "-LEX")))
      (labels ((replace-symbols (form &optional inside-function)
                 (loop :for item :in form :for ix :from 0
                    :do (if (listp item)
                            (if (and (second item) (not (third item))
                                     (symbolp (second item)) (member (first item) '(inws inwsd)))
                                (setf (nth ix form)
                                      (intern (string (second item))
                                              (if (and inside-function (not (eql 'inwsd (first item)))
                                                       (not (char= #\* (aref (string (second item)) 0))))
                                                  lex-space-name space-name)))
                                ;; don't lex-intern functions like #'𝕊|fn|
                                (replace-symbols item (and (not (eql 'function (first item)))
                                                           (or inside-function
                                                               (member (first item) '(alambda olambda))))))
                            (if (and (symbolp item)
                                     (string= "+WORKSPACE-NAME+" (string-upcase item)))
                                (setf (nth ix form) (intern (string-upcase name)
                                                            this-package)))))))
        (replace-symbols body)
        (setf (cdddr (first body))
              (cons '(make-threading-kernel-if-absent)
                    (cdddr (first body))))
        (first body)))))

;; this reader macro expands to (inws symbol) for reader-friendly printing of compiled code
(set-macro-character #\𝕊 (lambda (stream character)
                           (declare (ignore character))
                           (list 'inws (read stream t nil t))))

;; this reader macro expands to (inws symbol) for reader-friendly printing of compiled code
(set-macro-character #\𝔻 (lambda (stream character)
                           (declare (ignore character))
                           (list 'inwsd (read stream t nil t))))

;; printer extension to use the 𝕊 reader macro
(set-pprint-dispatch '(cons (member inws))
                     #'(lambda (s list)
                         (if (and (symbolp (second list)) (not (third list)))
                             (funcall (formatter "𝕊~W") s (second list))
                             (pprint-fill s list))))

;; printer extension to use the 𝔻 reader macro
(set-pprint-dispatch '(cons (member inwsd))
                     #'(lambda (s list)
                         (if (and (symbolp (second list)) (not (third list)))
                             (funcall (formatter "𝔻~W") s (second list))
                             (pprint-fill s list))))

(defun load-demos ()
  "Load the April demo packages."
  (loop :for package-symbol :in *demo-packages* :do (asdf:load-system package-symbol)))

(defun run-demo-tests ()
  "Run the tests for each April demo package."
  (loop :for package-symbol :in *demo-packages*
     :do (if (asdf:registered-system package-symbol)
             (let ((run-function-symbol (intern "RUN-TESTS" (string-upcase package-symbol))))
               (if (fboundp run-function-symbol)
                   (funcall (symbol-function run-function-symbol))))
             (format t "~% Warning: demo system ｢~a｣ not loaded. Did you evaluate (load-demos) before trying to run the demo tests?~%"
                     package-symbol))))

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
         (system-vars (rest (assoc :sys-vars options))))
    `(labels ((∇self ,params
                (declare (ignorable ,@(loop :for var :in params :when (not (eql '&optional var))
                                         :collect var)))
                (let ,(loop :for var :in system-vars :collect (list var var))
                  (declare (ignorable ,@system-vars))
                  ,@body)))
       #'∇self)))

(defmacro olambda (params &body body)
  `(labels ((∇oself ,params ,@body))
     #'∇oself))

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
  (if (member (first body) (cons 'λωαχ *io-currying-function-symbols-dyadic*))
      `(λωα (funcall ,body omega alpha ,(cons 'list axes)))
      (if (member (first body) (cons 'λωχ *io-currying-function-symbols-monadic*))
          `(λω (funcall ,body omega ,(cons 'list axes)))
          body)))

(defmacro is-workspace-value (item)
  "Checks if a variable is present in the current workspace as a value."
  `(and (boundp (intern (string ,item) space))
        (not (fboundp (intern (string ,item) space)))))

(defmacro is-workspace-function (item)
  "Checks if a variable is present in the current workspace as a function."
  `(fboundp (intern (string ,item) space)))

(defmacro is-workspace-operator (item)
  "Checks if a variable is present in the current workspace as a function."
  `(or (fboundp (intern (concatenate 'string "𝕆𝕃∇" (string ,item))
                        space))
       (fboundp (intern (concatenate 'string "𝕆ℙ∇" (string ,item))
                        space))))

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

;; (defun set-workspace-item-meta (table item &rest data)
;;   "Sets one or more metadata for an item in a workspace."
;;   (loop :for (key value) :on data :by #'cddr
;;      :do (setf (getf (gethash item table) key) value)))

;; (defun get-workspace-item-meta (table item &rest keys)
;;   "Gets one or more metadata for an item in a workspace."
;;   (let ((data (gethash item table)))
;;     (apply #'values (loop :for key :in keys :collect (getf data key)))))

(defun get-workspace-alias (space symbol)
  "Find an existing alias of a lexical function in a workspace."
  (let ((aliases-symbol (intern "*LEXICAL-FUNCTION-ALIASES*" space))
        (ws-symbol (intern (string symbol) space)))
    (if (boundp aliases-symbol) (gethash ws-symbol (symbol-value aliases-symbol)))))

(defun set-workspace-alias (space symbol glyph)
  "Set an alias for a lexical function in a workspace, as when compiling f←+."
  (let ((aliases-symbol (intern "*LEXICAL-FUNCTION-ALIASES*" space))
        (ws-symbol (intern (string symbol) space)))
    (if (not (boundp aliases-symbol))
        (setf (symbol-value aliases-symbol) (make-hash-table :test #'eq)))
    (let ((ws-aliases (symbol-value aliases-symbol)))
      (if (or (and (null glyph)
                   (gethash ws-symbol ws-aliases))
              (resolve-function :monadic glyph)
              (resolve-function :dyadic glyph))
          (setf (gethash ws-symbol ws-aliases) glyph)))))

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

(defmacro apl-assign (symbol value);; &optional is-toplevel)
  "This is macro is used to build variable assignment forms and includes logic for strand assignment."
  (if (or (not (listp symbol))
          (eql 'inws (first symbol)))
      (let* ((assign-val (gensym))
             (is-symbol-value (or (symbolp value)
                                  (and (listp value)
                                       (or (member (first value) '(inws inwsd))
                                           ;; remember to duplicate an assigned symbol as well
                                           (and (eql 'apl-assign (first value))
                                                (or (symbolp (second value))
                                                    (and (listp (second value))
                                                         (member (second value) '(inws inwsd)))))))))
             (set-to (if (not is-symbol-value) value `(duplicate ,value))))
        ;; handle assignment of ⍺ or ⍵; ⍺-assignment sets its default value if no right argument is
        ;; present; ⍵-assignment is an error. This is handled below for strand assignments.
        (if (eql '⍺ symbol) `(or ⍺ (setf ⍺ ,set-to))
            (if (eql '⍵ symbol) `(error "The [⍵ right argument] cannot have a default assignment.")
                `(setf ,symbol ,set-to))))
      (let (;; (values (gensym "A"))
            (symbols (if (not (eql 'avector (first symbol)))
                         symbol (rest symbol))))
        (labels ((process-symbols (sym-list values &optional path)
                   (let ((this-val (gensym)) (assign-val (gensym)))
                     `(let ((,this-val ,values))
                        ,@(loop :for sym :in sym-list :for sx :from 0
                             :append (let ((path-to (cons sx path)))
                                       (if (and (listp sym) (not (eql 'inws (first sym))))
                                           (list (process-symbols sym `(if (not (vectorp ,this-val))
                                                                           ,this-val (aref ,this-val ,sx))))
                                           (if (eql '⍺ sym)
                                               `((or ⍺ (setf ⍺ (if (not (vectorp ,this-val))
                                                                   ,this-val (aref ,this-val sx)))))
                                               (if (eql '⍵ sym) `(error "The [⍵ right argument] cannot ~a"
                                                                        "have a default assignment.")
                                                   `((setf ,sym (if (not (vectorp ,this-val))
                                                                    ,this-val (aref ,this-val ,sx)))))))))
                        ,this-val))))
          (process-symbols symbols value)))))

(defmacro apl-output (form &key (print-to) (output-printed)
                             (print-assignment) (print-precision) (with-newline))
  "Generate code to output the result of APL evaluation, with options to print an APL-formatted text string expressing said result and/or return the text string as a result."
  (let ((result (gensym)) (printout (gensym))
        ;; get the symbol referencing a function passed as the output
        (function-name-value (if (and (listp form) (eql 'function (first form)))
                                 `(string (quote ,(second form))))))
    `(let* ((,result ,form)
            (,printout ,(if (and (or print-to output-printed))
                            ;; don't print the results of assignment unless the :print-assignment option is set,
                            ;; as done when compiling a ⎕← expression
                            (or (and function-name-value
                                     `(concatenate 'string "∇" ,function-name-value))
                                ;; if a bare function name is to be output, prefix it with ∇
                                (and (listp form)
                                     (eql 'apl-assign (first form))
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

(defmacro avector (&rest items)
  "This macro returns an APL vector, disclosing data within that are meant to be individual atoms."
  (let ((type))
    (loop :for item :in items :while (not (eq t type))
       :do (setq type (type-in-common type (assign-element-type (if (or (not (integerp item))
                                                                        (> 0 item))
                                                                    item (max 16 item))))))
    `(make-array (list ,(length items))
                 :element-type (quote ,type)
                 ;; enclose each array included in an APL vector
                 :initial-contents (list ,@items))))

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
                  :do (if (alphanumericp (aref output i))
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

(defmacro with-derived-operands (operand-specs &rest body)
  "Derive references to data to be passed to an operator so they can be used by the macro implementing the operator."
  (let* ((first-op (gensym)) (first-axes (gensym)) (second-op (gensym)) (second-axes (gensym))
         (ignorables) (to-ignore))
    (loop :for symbol :in '(right left)
       :do (if (not (member symbol operand-specs)) (setq to-ignore (cons symbol to-ignore))))
    `(lambda (,first-op ,first-axes &optional ,second-op ,second-axes)
       (declare (ignorable ,second-op ,second-axes))
       (let (,@(loop :for symbol :in operand-specs
                  :when (and (not (member symbol '(right left axes)))
                             (or (symbolp symbol) (characterp symbol)))
                  :collect (list symbol (case symbol
                                          (left-op first-op)
                                          (left-axes first-axes)
                                          (left-glyph (setq ignorables (cons 'left-glyph ignorables))
                                                      `(or-functional-character ,first-op :fn))
                                          (left-fn-monadic
                                           `(if (resolve-function :monadic ,first-op ,first-axes)
                                                `(λω (apl-call ,(or-functional-character ,first-op :fn)
                                                               ,(resolve-function :monadic ,first-op ,first-axes)
                                                               omega))
                                                (if (listp ,first-op)
                                                    (if (eql 'function (first ,first-op))
                                                        ,first-op (if (eql 'wrap-fn-ref (first ,first-op))
                                                                      (second ,first-op)))
                                                    (if (eql '⍺⍺ ,first-op) ,first-op))))
                                          (left-fn-monadic-inverse
                                           `(if (resolve-function :monadic-inverse ,first-op ,first-axes)
                                                `(λω (apl-call ,(or-functional-character ,first-op :fn)
                                                               ,(resolve-function :monadic-inverse
                                                                                  ,first-op ,first-axes)
                                                               omega))))
                                          (left-fn-dyadic
                                           `(if (resolve-function :dyadic ,first-op ,first-axes)
                                                `(λωα (apl-call ,(or-functional-character ,first-op :fn)
                                                                ,(resolve-function :dyadic ,first-op ,first-axes)
                                                                omega alpha))
                                                (if (listp ,first-op)
                                                    (if (eql 'function (first ,first-op))
                                                        ,first-op (if (eql 'wrap-fn-ref (first ,first-op))
                                                                      (second ,first-op)))
                                                    (if (eql '⍺⍺ ,first-op) ,first-op))))
                                          (left-fn-dyadic-inverse
                                           `(if (resolve-function :dyadic-inverse ,first-op)
                                                `(λωα (apl-call ,(or-functional-character ,first-op :fn)
                                                                ,(getf
                                                                  (resolve-function :dyadic-inverse
                                                                                    ,first-op)
                                                                  :plain)
                                                                omega alpha))))
                                          (left-fn-symbolic `(resolve-function :symbolic ,first-op ,first-axes))
                                          (right-op second-op)
                                          (right-axes second-axes)
                                          (right-glyph (setq ignorables (cons 'right-glyph ignorables))
                                                       `(or-functional-character ,second-op :fn))
                                          (right-fn-monadic
                                           `(if (resolve-function :monadic ,second-op ,second-axes)
                                                `(λω (apl-call ,(or-functional-character ,second-op :fn)
                                                               ,(resolve-function :monadic
                                                                                  ,second-op ,second-axes)
                                                               omega))
                                                (if (listp ,second-op)
                                                    (if (eql 'function (first ,second-op))
                                                        ,second-op (if (eql 'wrap-fn-ref (first ,second-op))
                                                                       (second ,second-op)))
                                                    (if (eql '⍵⍵ ,second-op) ,second-op))))
                                          (right-fn-dyadic
                                           `(if (resolve-function :dyadic ,second-op ,second-axes)
                                                `(λωα (apl-call ,(or-functional-character ,second-op :fn)
                                                                ,(resolve-function :dyadic ,second-op ,second-axes)
                                                                omega alpha))
                                                (if (listp ,second-op)
                                                    (if (eql 'function (first ,second-op))
                                                        ,second-op (if (eql 'wrap-fn-ref (first ,second-op))
                                                                       (second ,second-op)))
                                                    (if (eql '⍵⍵ ,second-op) ,second-op))))
                                          (right-fn-symbolic `(resolve-function :symbolic
                                                                                ,second-op ,second-axes))))))
         ,@(if ignorables `((declare (ignorable ,@ignorables))))
         (lambda (,@(if (member 'axes operand-specs)
                        (list 'axes)
                        (list 'right 'left)))
           (declare ,@(if (member 'axes operand-specs) `((ignorable axes))
                          `((ignore ,@to-ignore))))
           ;; if bare character values were passed in wrapped in (:char) forms,
           ;; they are removed from those forms here
           ,@(if (not (member 'axes operand-specs))
                 `((if (and (listp right) (eq :char (first right)))
                       (setq right (second right)))
                   (if (and (listp left) (eq :char (first left)))
                       (setq left (second left)))))
           ,@body)))))

(defun resolve-function (mode reference &optional axes)
  "Retrieve the function corresponding to a given character or symbol in a given mode (monadic or dyadic)."
  (if (characterp reference)
      (if axes `(λχ ,(of-functions this-idiom reference mode) ,axes)
          (of-functions this-idiom reference mode))
      (if (symbolp reference)
          (if (or (fboundp reference))
              `(function ,reference)
              (if (eql '∇ reference)
                  '#'∇self (if (eql '∇∇ reference)
                               '#'∇∇oself (if (member reference '(⍺⍺ ⍵⍵))
                                              reference))))
          ;; TODO: can the logic determining if something is not a function be improved?
          (if (and (listp reference)
                   (or (eql 'lambda (first reference))
                       (and (symbolp (first reference))
                            (macro-function (first reference))
                            (not (member (first reference)
                                         ;; TODO: this will cause a problem if a function is passed and assigned
                                         '(avector apl-call apl-if apl-output apl-assign))))))
              reference))))

(defmacro resolve-operator (mode reference)
  "Retrieve an operator's composing function."
  `(of-operators this-idiom ,reference ,mode))

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

(defmacro apl-call (symbol function &rest arguments)
  "Call an APL function with one or two arguments. Compose successive scalar functions into bigger functions for more efficiency."
  (declare (ignore symbol))
  (let ((arg (gensym "A")))
    (flet ((is-scalar (form) (and (listp form) (eql 'scalar-function (first form))))
           (is-boolean (form) (and (listp form) (listp (second form))
                                   (eql 'boolean-op (caadr form))))
           (expand-monadic (fn argument)
             (let ((arg-expanded (macroexpand argument)))
               (if (and (listp arg-expanded)
                        (eql 'apply-scalar (first arg-expanded))
                        (not (fourth arg-expanded)))
                   (let ((innerfn (second arg-expanded)))
                     (list (if (not (eql 'lambda (first innerfn)))
                               `(lambda (,arg) (funcall ,fn (funcall ,innerfn ,arg)))
                               (list (first innerfn) (second innerfn)
                                     `(funcall ,fn ,(third innerfn))))
                           (third arg-expanded)))
                   (list fn argument))))
           (expand-dyadic (fn is-first arg1 arg2)
             (let* ((arg-expanded (macroexpand (if is-first arg1 arg2))))
               (if (and (listp arg-expanded)
                        (eql 'apply-scalar (first arg-expanded))
                        ;; extract the sub-arguments within the expanded argument to the function; if one
                        ;; is a scalar value, the function may be merged into the containing closure
                        (let ((sub-arg1 (if (and (listp (second arg-expanded))
                                                 (eql 'lambda (caadr arg-expanded)))
                                            (third (third (second arg-expanded)))
                                            (third arg-expanded)))
                              (sub-arg2 (if (and (listp (second arg-expanded))
                                                 (eql 'lambda (caadr arg-expanded)))
                                            (fourth (third (second arg-expanded)))
                                            (fourth arg-expanded))))
                          ;; one of the sub-arguments must be a number - or if there is no second argument,
                          ;; the inner function is monadic and the decomposition can proceed
                          (or (numberp sub-arg1) (not sub-arg2) (numberp sub-arg2))))
                   (let ((innerfn (second arg-expanded)))
                     (list (if (not (eql 'lambda (first innerfn)))
                               `(lambda (,arg) (funcall ,fn ,@(if (not is-first) (list arg1))
                                                        (funcall ,innerfn ,arg
                                                                 ;; include the inner function's
                                                                 ;; second argument if present
                                                                 ,@(if (fourth arg-expanded)
                                                                       (list (fourth arg-expanded))))
                                                        ,@(if is-first (list arg2))))
                               (list (first innerfn) (second innerfn)
                                     `(funcall ,fn ,@(if (not is-first) (list arg1))
                                               ,(third innerfn) ,@(if is-first (list arg2)))))
                           (third arg-expanded)))))))
      (let* ((scalar-fn (is-scalar function))
             (fn-body (cond ((and scalar-fn (not (second arguments)))
                             ;; compose monadic functions if the argument is the output of another scalar function
                             (expand-monadic function (first arguments)))
                            ((and scalar-fn (second arguments)
                                  (numberp (first arguments)))
                             ;; compose dyadic functions if the first argument is a scalar numeric value
                             ;; and the other argument is the output of a scalar function
                             (let ((expanded (expand-dyadic function nil (first arguments) (second arguments))))
                               (or expanded `((lambda (,arg) (funcall ,function ,(first arguments) ,arg))
                                              ,(macroexpand (second arguments))
                                              nil))))
                            ((and scalar-fn (second arguments)
                                  (numberp (second arguments)))
                             ;; same as above if the numeric argument is reversed
                             (let ((expanded (expand-dyadic function t (first arguments) (second arguments))))
                               (or expanded `((lambda (,arg) (funcall ,function ,arg ,(second arguments)))
                                              ,(macroexpand (first arguments))
                                              nil))))
                            ;; otherwise, just list the function and its arguments
                            (t (cons function arguments)))))
        (funcall (lambda (form)
                   (if (not scalar-fn)
                       form (list 'value-meta-process form)))
                 ;; wrap (apply-scalar) forms in the (value-meta-process) macro
                 (append (list (if scalar-fn 'apply-scalar 'funcall))
                         (if (and scalar-fn (= 4 (length fn-body)))
                             ;; if the function is scalar and an axis argument is present,
                             ;; adjust the numerical axis values according to the index origin
                             (append (butlast fn-body 1)
                                     `((adjust-axes-for-index-origin index-origin ,(fourth fn-body))))
                             fn-body)
                         (if (and scalar-fn (= 2 (length fn-body)))
                             '(nil))
                         (if (and scalar-fn (is-boolean function))
                             '(nil t))))))))

(defmacro value-meta-process (form)
  "Assign array metadata appropriately to arrays resulting from scalar operations along with newly assigned arrays. Currently this is used to migrate array prototypes, as for operations like 1+0↑⊂3 3⍴⍳9."
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
                      ,result)))))))

#|
This is a minimalistic implementation of (apl-call) that doesn't perform any function composition.
It remains here as a standard against which to compare methods for composing APL functions.

(defmacro apl-call (symbol function &rest arguments)
(declare (ignore symbol))
`(,(if (and (listp function)
(eql 'scalar-function (first function)))
'apply-scalar 'funcall)
,function  ,@arguments))
|#

(defmacro apl-compose (symbol &rest body)
  "A wrapper macro for macros that implement April's operators; functionally this macro does nothing but it improves the readability of April's compiled code."
  (declare (ignore symbol))
  (let ((expanded (macroexpand body)))
    (if (or (not (listp (first expanded)))
            (not (eql 'olambda (caar expanded))))
        expanded (cons 'funcall expanded))))

(defmacro apl-if (&rest each-clause)
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

(defmacro scalar-function (function)
  "Wrap a scalar function. This is a passthrough macro used by the scalar composition system in (apl-call)."
  (if (symbolp function)
      `(function ,function)
      function))

(defun validate-arg-unitary (value)
  "Verify that a form like (vector 5) represents a unitary value."
  (or (symbolp value)
      (numberp value)
      (and (listp value)
           (or (not (eql 'vector (first value)))
               (not (third value))))))

(defmacro or-functional-character (reference symbol)
  "Return a symbol representing a functional character or, if the passed value is not a character, an arbitrary fallback symbol. Used to derive the initial symbol argument for (apl-call)."
  `(if (not (characterp ,reference))
       ,symbol (intern (string-upcase ,reference) ,*package-name-string*)))

(defun enclose-axes (body axis-sets &key (set) (set-by))
  "Apply axes to an array, with the ability to handle multiple sets of axes as in (6 8 5⍴⍳9)[1 4;;2 1][1;2 4 5;]."
  (let ((axes (first axis-sets))
        (assignment-output (gensym)) (assigned-array (gensym)))
    (if (not axis-sets)
        body (enclose-axes
              (if set `(multiple-value-bind (,assignment-output ,assigned-array)
                           (achoose ,body (mapcar (lambda (array)
                                                    (if array (apply-scalar #'- array index-origin)))
                                                  (list ,@axes))
                                    :set ,set ,@(if set-by (list :set-by set-by))
                                    ;; setting the modify-input parameter so that the original value
                                    ;; is modified in place if possible
                                    :modify-input t)
                         (if ,assigned-array (setf ,body ,assigned-array))
                         ,assignment-output)
                  `(achoose ,body (mapcar (lambda (array) (if array (apply-scalar #'- array index-origin)))
                                          (list ,@axes))))
              (rest axis-sets)))))

(defun coerce-or-get-type (array &optional type-index)
  "Create an array with a numerically designated type holding the contents of the given array."
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

(defun output-value (space form &optional properties lexical-symbols)
  "Express an APL value in the form of an explicit array specification or a symbol representing an array, supporting axis arguments."
  (labels ((enclose-symbol (item)
             ;; enclose the symbol in an (inws) form for interning in the workspace
             ;; if it isn't one of the designated idiom-native symbols
             (if (or (not (symbolp item))
                     (member item *idiom-native-symbols*))
                 item (if (or (member item lexical-symbols)
                              (not (boundp (intern (string item) space))))
                          ;; if a symbol does not represent a lexical variable within the given scope,
                          ;; it must be a dynamic variable so wrap it with 𝔻
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
                   (if (member (first form) '(avector inws inwsd))
                       form (if (not (or (numberp (first form))
                                         (listp (first form))
                                         (stringp (first form))
                                         (characterp (first form))
                                         (and (arrayp (first form))
                                              (= 0 (size (first form))))
                                         (member (first form) '(⍺ ⍵ ⍶ ⍹ ⍺⍺ ⍵⍵)
                                                 :test #'eql)
                                         (and (not (fboundp (first form)))
                                              (and (symbolp (first form))
                                                   (and (not (fboundp (intern (string (first form))
                                                                              space))))))))
                                (if (= 1 (length properties))
                                    (apply-props form (first properties))
                                    (mapcar #'apply-props form properties))
                                (if (getf (first properties) :vector-axes)
                                    (enclose-axes `(avector ,@(mapcar #'apply-props form properties))
                                                  (getf (first properties) :vector-axes))
                                    `(avector ,@(mapcar #'apply-props form properties)))))
                   (if (not (numberp form))
                       (apply-props form properties)
                       form))))))

(defmacro f-lex (symbol-sets &body body)
  (destructuring-bind (ref-symbols symbols) symbol-sets
    (let ((arg-sym (gensym))
          (ref-strings (loop :for sym :in ref-symbols :collect (string sym)))
          (package-string (if (first symbols) (package-name (symbol-package (first symbols)))))
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
                          :when (not (member sym arguments)) :collect `(inws ,sym)))
        (assigned-fns (loop :for sym :in (getf closure-meta :fn-syms)
                         :append `((inws ,sym) (inws ,(intern (format nil "𝕚∇~a" sym))))))
        (assigned-ops (append (loop :for sym :in (getf closure-meta :lop-syms)
                                 :collect `(inws ,(intern (format nil "𝕆𝕃∇~a" sym))))
                              (loop :for sym :in (getf closure-meta :pop-syms)
                                 :collect `(inws ,(intern (format nil "𝕆ℙ∇~a" sym))))))
        (context-vars (loop :for sym :in (getf closure-meta :var-syms)
                         :when (and (not (member sym arguments))
                                    (not (member sym (getf (getf closure-meta :env) :var-syms))))
                         :collect `(inwsd, sym)))
        (context-fns (loop :for sym :in (getf closure-meta :fn-syms)
                        :when (not (member sym (getf (getf closure-meta :env) :var-syms)))
                        :append `((inwsd, sym) (inwsd ,(intern (format nil "𝕚∇~a" sym))))))
        (context-ops (append (loop :for sym :in (getf closure-meta :lop-syms)
                                :when (not (member sym (getf (getf closure-meta :env) :lop-syms)))
                                :collect `(inwsd ,(intern (format nil "𝕆𝕃∇~a" sym))))
                             (loop :for sym :in (getf closure-meta :pop-syms)
                                :when (not (member sym (getf (getf closure-meta :env) :pop-syms)))
                                :collect `(inwsd ,(intern (format nil "𝕆ℙ∇~a" sym))))))
        (arguments (if arguments (mapcar (lambda (item) `(inws ,item)) arguments))))
    ;; (print (list :form form closure-meta))
    (funcall (if (not (intersection arg-symbols '(⍶ ⍹ ⍺⍺ ⍵⍵)))
                 ;; the latter case wraps a user-defined operator
                 #'identity (lambda (form) `(olambda (,(if (member '⍶ arg-symbols) '⍶ '⍺⍺)
                                                       &optional ,(if (member '⍹ arg-symbols) '⍹ '⍵⍵))
                                              (declare (ignorable ,(if (member '⍶ arg-symbols) '⍶ '⍺⍺)
                                                                  ,(if (member '⍹ arg-symbols) '⍹ '⍵⍵)))
                                              ,form)))
             `(alambda ,(if arguments arguments `(⍵ &optional ⍺))
                  (with (:sys-vars ,@(loop :for (key value) :on *system-variables* :by #'cddr
                                        :collect `(inws ,value))))
                ,@(if (not (or context-vars context-fns context-ops assigned-vars assigned-fns assigned-ops))
                      form `((f-lex ,(list (append context-vars context-fns context-ops)
                                           (append assigned-vars assigned-fns assigned-ops))
                               ,@form)))))))

(defun output-function2 (form &optional arguments assigned-symbols arg-symbols to-reference closure-meta)
  "Express an APL inline function like {⍵+5}."
  (declare (ignore closure-meta))
  (let ((assigned-symbols (loop :for sym :in assigned-symbols
                             :when (not (member (string-upcase sym)
                                                '("*INDEX-ORIGIN*" "*COMPARISON-TOLERANCE*")
                                                :test #'string=))
                             :collect `(inws ,sym)))
        (to-reference (loop :for sym :in to-reference :collect `(inwsd ,sym)))
        (arguments (if arguments (mapcar (lambda (item) `(inws ,item)) arguments))))
    (funcall (if (not (intersection arg-symbols '(⍶ ⍹ ⍺⍺ ⍵⍵)))
                 ;; the latter case wraps a user-defined operator
                 #'identity (lambda (form) `(olambda (,(if (member '⍶ arg-symbols) '⍶ '⍺⍺)
                                                       &optional ,(if (member '⍹ arg-symbols) '⍹ '⍵⍵))
                                              (declare (ignorable ,(if (member '⍶ arg-symbols) '⍶ '⍺⍺)
                                                                  ,(if (member '⍹ arg-symbols) '⍹ '⍵⍵)))
                                              ,form)))
             `(alambda ,(if arguments arguments `(⍵ &optional ⍺))
                  (with (:sys-vars ,@(loop :for (key value) :on *system-variables* :by #'cddr
                                        :collect `(inws ,value))))
                ,@(if (not assigned-symbols)
                      form `((f-lex (,to-reference ,assigned-symbols) ,@form)))))))

(defun build-variable-declarations (input-vars space)
  "Create the set of variable declarations that begins April's compiled code."
  (loop :for var-entry :in input-vars :collect (list (intern (lisp->camel-case (first var-entry)) space)
                                                     (second var-entry))))

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

(defun generate-function-retriever (operand function-monadic function-dyadic axes)
  "This function is used at compile time to generate the functon invoked by the [⍣ power] operator at runtime to fetch a regular or inverse function depending on the right operand passed to it."
  (let ((is-dyadic (gensym)) (is-inverse (gensym)))
    (if (or (symbolp operand) (characterp operand))
        (let* ((dyinv-forms (resolve-function :dyadic-inverse operand axes))
               (left-fn-monadic
                `(λω (apl-call ,operand ,(resolve-function :monadic operand axes) omega)))
               (left-fn-dyadic
                `(λωα (apl-call ,operand ,(resolve-function :dyadic operand axes) omega alpha)))
               (left-fn-monadic-inverse
                `(λω (apl-call ,operand ,(resolve-function :monadic-inverse operand axes) omega)))
               (left-fn-dyadic-inverse
                `(λωα (apl-call ,operand ,(or (getf dyinv-forms :plain)) omega alpha))))
          `(lambda (,is-dyadic ,is-inverse)
             (if (not ,is-inverse) (if ,is-dyadic ,left-fn-dyadic ,left-fn-monadic)
                 (if ,is-dyadic ,left-fn-dyadic-inverse ,left-fn-monadic-inverse))))
        (or (match operand ((list 'function (list 'inws (guard symbol (symbolp symbol))))
                            (let ((inverse-operand `(function (inws ,(intern (format nil "𝕚∇~a" symbol))))))
                              `(lambda (,is-dyadic ,is-inverse)
                                 (declare (ignore ,is-dyadic))
                                 (if ,is-inverse ,inverse-operand ,operand))))
                   ;; handle (wrap-fn-ref) for locally-scoped functions
                   ((list 'wrap-fn-ref (list 'inws (guard symbol (symbolp symbol))))
                    (let ((inverse-operand `(inws ,(intern (format nil "𝕚∇~a" symbol)))))
                      `(lambda (,is-dyadic ,is-inverse)
                         (declare (ignore ,is-dyadic))
                         (if ,is-inverse ,inverse-operand ,(second operand))))))
            (match function-monadic ((list 'inws (guard symbol (symbolp symbol)))
                                     `(lambda (,is-dyadic ,is-inverse)
                                        (declare (ignore ,is-dyadic))
                                        (if ,is-inverse nil ,function-monadic))))
            (let ((inverted (invert-function operand)))
              `(lambda (,is-dyadic ,is-inverse)
                 (declare (ignore ,is-dyadic))
                 ,function-monadic ,function-dyadic
                 (if ,is-inverse ,inverted ,operand)))))))

;; (defun lexer-postprocess (tokens idiom space &optional closure-meta-form)
;;   ;; currently, this function is used to initialize function and variable references
;;   ;; in the workspace before compilation is performed so that recursive
;;   ;; functions will work correctly as with fn←{A←⍵-1 ⋄ $[A≥0;A,fn A;0]} ⋄ fn 5
;;   ;; (print (list :tok tokens))
;;   (symbol-macrolet ((closure-meta (rest closure-meta-form)))
;;     (match tokens
;;       ((list (guard fn-form (and (listp fn-form)
;;                                  (eq :fn (first fn-form))
;;                                  (listp (second fn-form))))
;;              '(:fn #\←) (guard symbol (and (symbolp symbol) (not (member symbol '(⍺⍺ ⍵⍵))))))
;;        ;; (print (list :gg closure-meta))
;;        (let* ((fn-meta (second fn-form))
;;               (processed (lexer-postprocess (third fn-form) idiom space fn-meta))
;;               (is-operator (intersection (getf (rest fn-meta) :symbols)
;;                                          '(⍶ ⍹ ⍺⍺ ⍵⍵)))
;;               (valence (if is-operator (if (intersection is-operator '(⍹ ⍵⍵))
;;                                            :pivotal :lateral)))
;;               (int-symbol (if is-operator (intern (concatenate 'string (if (eq valence :lateral) "𝕆𝕃∇" "𝕆ℙ∇")
;;                                                                (string symbol))
;;                                                   space))))
;;          (if is-operator (progn (setf (getf (rest fn-meta) :valence) valence)
;;                                 (if (getf (rest fn-meta) :valence-setters)
;;                                     (loop :for setter :in (getf (rest fn-meta) :valence-setters)
;;                                        :do (funcall setter valence)))
;;                                 (setf (getf (rest fn-meta) :valence-setters) nil)))
;;          (if closure-meta (if is-operator (push symbol (getf closure-meta
;;                                                              (if (eq :lateral valence) :lop-syms :pop-syms)))
;;                               (push symbol (getf closure-meta :fn-syms)))
;;              (progn (if (is-workspace-value symbol)
;;                         (makunbound (intern (string symbol) space)))
;;                     ;; (if (string= "rlop" (string symbol))
;;                     ;;     (print (list :in int-symbol symbol is-operator valence fn-meta)))
;;                     (if int-symbol (progn (if (not (fboundp int-symbol))
;;                                               (setf (symbol-function int-symbol) #'dummy-operator))
;;                                           (set (intern (string int-symbol) space) fn-meta))
;;                         (if (not (fboundp (intern (string symbol) space)))
;;                             (setf (symbol-function (intern (string symbol) space))
;;                                   #'dummy-nargument-function)))))
;;          ;; (print (list :pro fn-meta))
;;          (list (list (if is-operator :op :fn) fn-meta processed)
;;                '(:fn #\←) symbol)))
;;       ;; ((list (guard op-form (and (listp op-form)
;;       ;;                            (eq :op (first op-form))
;;       ;;                            (listp (second op-form))))
;;       ;;        '(:fn #\←) (guard symbol (or (and (symbolp symbol) (not (member symbol '(⍺⍺ ⍵⍵)))))))
;;       ;;  (let ((fn-symbol (intern (string symbol) space))
;;       ;;        (op-meta (second op-form))
;;       ;;        (int-symbol (intern (concatenate 'string (if (eq :lateral (getf (cdadr op-form) :valence))
;;       ;;                                                     "𝕆𝕃∇" "𝕆ℙ∇")
;;       ;;                                         (string symbol))
;;       ;;                            space)))
;;       ;;    (if closure-meta (push symbol (getf closure-meta (if (eq :lateral (getf (cdadr op-form) :valence))
;;       ;;                                                         :lop-syms :pop-syms)))
;;       ;;        ;; (print (list :op (getf op-meta :valence)))
;;       ;;        ;; if the symbol is already bound as a regular function, unbind it
;;       ;;        (progn (if (fboundp fn-symbol) (fmakunbound fn-symbol))
;;       ;;               (if (not (fboundp int-symbol))
;;       ;;                   (setf (symbol-function int-symbol) #'dummy-nargument-function))))
;;       ;;    (list (list (first op-form) (second op-form)
;;       ;;                (lexer-postprocess (third op-form) idiom space op-meta))
;;       ;;          '(:fn #\←) symbol)))
;;       ((list form '(:fn #\←) (guard symbol (symbolp symbol)))
;;        (if closure-meta (push symbol (getf closure-meta :symbols))
;;            (if (fboundp symbol) (fmakunbound symbol)))
;;        (list (lexer-postprocess form idiom space closure-meta-form)
;;              '(:fn #\←) symbol))
;;       ((guard fn-form (and (listp fn-form) (eq :fn (first fn-form))))
;;        (let* ((form-meta (if (listp (second fn-form)) (second fn-form)))
;;               (form-content (if (listp (third fn-form)) (third fn-form)))
;;               (processed (loop :for item :in form-content
;;                             :collect (lexer-postprocess item idiom space form-meta)))
;;               (is-operator (intersection (getf (rest form-meta) :symbols)
;;                                          '(⍶ ⍹ ⍺⍺ ⍵⍵))))
;;          ;; (print (list :frr form-meta processed is-operator))
;;          (if (not (and form-meta form-content))
;;              tokens (list (if is-operator :op :fn)
;;                           (second fn-form)
;;                           processed))))
;;       ((guard fn-form (and (listp fn-form) (eq :op (first fn-form))))
;;        ;; (print (list :ff fn-form))
;;        (let ((form-meta (if (listp (second fn-form)) (second fn-form)))
;;              (form-content (if (listp (third fn-form)) (third fn-form))))
;;          (if (not (and form-meta form-content))
;;              tokens (list (first fn-form) (second fn-form)
;;                           (loop :for item :in form-content
;;                              :collect (lexer-postprocess item idiom space form-meta))))))
;;       ((list (guard axes-form (and (listp axes-form) (eq :axes (first axes-form))))
;;              axes-of)
;;        (let ((each-axis (rest axes-form)))
;;          ;; (print (list :op (getf op-meta :valence)))
;;          ;; if the symbol is already bound as a regular function, unbind it
;;          (list (cons :axes (loop :for item :in each-axis
;;                               :collect (lexer-postprocess item idiom space closure-meta-form)))
;;                axes-of)
;;          ;; (print (list :opt tokens each-axis))
;;          ))
;;       ((guard list (and (listp list) (not (member (first list) '(inws inwsd)))))
;;        (loop :for item :in list :collect (lexer-postprocess item idiom space closure-meta-form))
;;        ;; (print (list :lout tokens))
;;        )
;;       ((guard symbol (eql symbol '∇∇))
;;        (let ((this-form (list :op :valence '∇∇)))
;;          (push (lambda (valence) (setf (second this-form) valence))
;;                (getf closure-meta :valence-setters))
;;          this-form))
;;       ((guard symbol (member symbol '(⍺ ⍵ ⍶ ⍹ ⍺⍺ ⍵⍵)))
;;        (if (and closure-meta (not (member symbol (getf closure-meta :symbols))))
;;            (push symbol (getf closure-meta :symbols)))
;;        symbol)
;;       (token-or-tokens token-or-tokens)
;;       )
;;     ))

(defun get-external-scope-symbols (key meta)
  (if (and (getf meta :parent)
           (getf (rest (getf meta :parent)) key))
      (append (loop :for sym :in (getf (rest (getf meta :parent)) key) :collect sym)
              (get-external-scope-symbols key (getf (rest (getf meta :parent)) :parent)))))

(defun lexer-postprocess (tokens idiom space &optional closure-meta-form)
  ;; currently, this function is used to initialize function and variable references
  ;; in the workspace before compilation is performed so that recursive
  ;; functions will work correctly as with fn←{A←⍵-1 ⋄ $[A≥0;A,fn A;0]} ⋄ fn 5
  (symbol-macrolet ((closure-meta (rest closure-meta-form)))
    (match tokens
      ((list (guard axes-form (and (listp axes-form) (eq :axes (first axes-form))))
             (guard fn-form (and (listp fn-form) (eq :fn (first fn-form))
                                 (characterp (second fn-form))))
             '(:fn #\←) (guard symbol (and (symbolp symbol) (not (member symbol '(⍺⍺ ⍵⍵))))))
       (if closure-meta (push symbol (getf closure-meta :fn-syms))
           (progn (if (is-workspace-value symbol)
                      (makunbound (intern (string symbol) space)))
                  (setf (symbol-function (intern (string symbol) space))
                        #'dummy-nargument-function)))
       (let ((each-axis (rest axes-form)))
         ;; if the symbol is already bound as a regular function, unbind it
         (list (cons :axes (loop :for item :in each-axis
                              :collect (lexer-postprocess item idiom space closure-meta-form)))
               fn-form '(:fn #\←) symbol)))
      ((list (guard fn-form (and (listp fn-form)
                                 (eq :fn (first fn-form))))
             '(:fn #\←) (guard symbol (and (symbolp symbol) (not (member symbol '(⍺⍺ ⍵⍵))))))
       (if (characterp (second fn-form))
           (progn (if closure-meta (push symbol (getf closure-meta :fn-syms))
                      (progn (if (is-workspace-value symbol)
                                 (makunbound (intern (string symbol) space)))
                             (setf (symbol-function (intern (string symbol) space))
                                   #'dummy-nargument-function)))
                  (list fn-form '(:fn #\←) symbol))
           (if (listp (second fn-form))
               (let* ((fn-meta (second fn-form))
                      (processed (lexer-postprocess (third fn-form) idiom space fn-meta))
                      (is-operator (intersection (getf (rest fn-meta) :arg-syms)
                                                 '(⍶ ⍹ ⍺⍺ ⍵⍵)))
                      (valence (if is-operator (if (intersection is-operator '(⍹ ⍵⍵))
                                                   :pivotal :lateral)))
                      (int-symbol (if is-operator (intern (concatenate 'string (if (eq valence :lateral)
                                                                                   "𝕆𝕃∇" "𝕆ℙ∇")
                                                                       (string symbol))
                                                          space))))
                 (if is-operator (progn (setf (getf (rest fn-meta) :valence) valence)
                                        (if (getf (rest fn-meta) :valence-setters)
                                            (loop :for setter :in (getf (rest fn-meta) :valence-setters)
                                               :do (funcall setter valence)))
                                        (setf (getf (rest fn-meta) :valence-setters) nil)))
                 (if closure-meta (if is-operator
                                      (if (not (member symbol (getf closure-meta
                                                                    (if (eq :lateral valence)
                                                                        :lop-syms :pop-syms))))
                                          (push symbol (getf closure-meta
                                                             (if (eq :lateral valence) :lop-syms :pop-syms))))
                                      (if (not (member symbol (getf closure-meta :fn-syms)))
                                          (push symbol (getf closure-meta :fn-syms))))
                     (progn (if (is-workspace-value symbol)
                                (makunbound (intern (string symbol) space)))
                            (if int-symbol (progn (if (not (fboundp int-symbol))
                                                      (setf (symbol-function int-symbol) #'dummy-operator))
                                                  (set (intern (string int-symbol) space) fn-meta))
                                (if (not (fboundp (intern (string symbol) space)))
                                    (setf (symbol-function (intern (string symbol) space))
                                          #'dummy-nargument-function)))))
                 (if (and fn-meta closure-meta-form)
                     (progn (setf (getf (rest fn-meta) :parent) closure-meta-form
                                  (getf (rest fn-meta) :env) nil)
                            (loop :for key :in '(:var-syms :fn-syms :lop-syms :pop-syms)
                               :do (setf (getf (getf (rest fn-meta) :env) key)
                                         (get-external-scope-symbols key (rest fn-meta))))))
                 (list (list (if is-operator :op :fn) fn-meta processed)
                       '(:fn #\←) symbol)))))
      ((list form '(:fn #\←) (guard symbol (symbolp symbol)))
       ;; (print (list :ff form closure-meta))
       (if (symbolp form)
           (if (or (and closure-meta (member form '(⍺⍺ ⍵⍵)))
                   (and closure-meta (member form (getf closure-meta :fn-syms)))
                   (and closure-meta (member form (getf (getf closure-meta :env) :fn-syms)))
                   (fboundp (intern (string form) space)))
               (if closure-meta (push symbol (getf closure-meta :fn-syms))
                   (progn (setf (symbol-function (intern (string symbol) space))
                                #'dummy-nargument-function)
                          (set (intern (string symbol) space) (list :meta))))
               ;; TODO: add cases for dynamically bound operators
               (if (or (and closure-meta (member form (getf closure-meta :lop-syms)))
                       (and closure-meta (member form (getf (getf closure-meta :env) :lop-syms))))
                   (if closure-meta (push symbol (getf closure-meta :lop-syms))
                       (progn (setf (symbol-function (intern (string symbol) space))
                                    #'dummy-operator)
                              (set (intern (string symbol) space) (list :meta :valence :lateral))))
                   (if (or (and closure-meta (member form (getf closure-meta :pop-syms)))
                           (and closure-meta (member form (getf (getf closure-meta :env) :pop-syms))))
                       (if closure-meta (push symbol (getf closure-meta :pop-syms))
                           (progn (setf (symbol-function (intern (string symbol) space))
                                        #'dummy-operator)
                                  (set (intern (string symbol) space) (list :meta :valence :pivotal))))
                       (if closure-meta (push symbol (getf closure-meta :var-syms))
                           (progn ;; (set (intern (string symbol) space) nil)
                                  (fmakunbound symbol))))))
           (if closure-meta (if (not (member symbol (getf closure-meta :var-syms)))
                                (push symbol (getf closure-meta :var-syms)))
               (progn ;; (set (intern (string symbol) space) nil)
                      (fmakunbound symbol))))
       (list (lexer-postprocess form idiom space closure-meta-form)
             '(:fn #\←) symbol))
      ((guard fn-form (and (listp fn-form) (eq :fn (first fn-form))))
       (let* ((form-meta (if (listp (second fn-form)) (second fn-form)))
              (form-content (if (listp (third fn-form)) (third fn-form)))
              (processed (loop :for item :in form-content
                            :collect (lexer-postprocess item idiom space form-meta)))
              (is-operator (intersection (getf (rest form-meta) :arg-syms)
                                         '(⍶ ⍹ ⍺⍺ ⍵⍵))))
         (if (and form-meta closure-meta-form)
             (progn (setf (getf (rest form-meta) :parent) closure-meta-form
                          (getf (rest form-meta) :env) nil)
                    (loop :for key :in '(:var-syms :fn-syms :lop-syms :pop-syms)
                       :do (setf (getf (getf (rest form-meta) :env) key)
                                 (get-external-scope-symbols key (rest form-meta))))))
         (if (not (and form-meta form-content))
             tokens (list (if is-operator :op :fn)
                          (second fn-form)
                          processed))))
      ((guard fn-form (and (listp fn-form) (eq :op (first fn-form))))
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
       (let ((each-axis (rest axes-form)))
         ;; if the symbol is already bound as a regular function, unbind it
         (list (cons :axes (loop :for item :in each-axis
                              :collect (lexer-postprocess item idiom space closure-meta-form)))
               axes-of)))
      ;; ((guard list (and (listp list) (not (member (first list) '(inws inwsd)))))
      ;;  (loop :for item :in list :collect (lexer-postprocess item idiom space closure-meta-form)))
      ((guard list (and (listp list) (not (member (first list) '(inws inwsd)))))
       ;; (print (list :ll list))
       (labels ((process-symbols (possible-symbols &optional top-level)
                  (let ((symbols-valid t))
                    (if (or top-level (loop :for item :in possible-symbols :always (symbolp item)))
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
                          :collect (lexer-postprocess item idiom space closure-meta-form)))
                (assignment-index (first (last assignment-indices))))
           (loop :for index :in (reverse assignment-indices)
              :do (process-symbols (nthcdr (1+ index) list) t))
           items)))
      ((guard symbol (eql symbol '∇∇))
       (let ((this-form (list :op :valence '∇∇)))
         (push (lambda (valence) (setf (second this-form) valence))
               (getf closure-meta :valence-setters))
         this-form))
      ((guard symbol (member symbol '(⍺ ⍵ ⍶ ⍹ ⍺⍺ ⍵⍵)))
       (if (and closure-meta (not (member symbol (getf closure-meta :arg-syms))))
           (push symbol (getf closure-meta :arg-syms)))
       symbol)
      (token-or-tokens token-or-tokens))))

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

(defun invert-function (form &optional to-wrap)
  "Invert a function expression. For use with the [⍣ power] operator taking a negative right operand."
  (match form
    ((list* 'apl-compose '⍣ 'operate-to-power degree rest)
     ;; invert a [⍣ power] operation - all that needs be done is negate the right operand
     (let ((inverse-degree `(lambda () (- ,(third degree)))))
       ;; the degree is manifested by a function,
       ;; so a function generating the inverse of that degree is built
       `(apl-compose ⍣ operate-to-power ,inverse-degree ,@rest)))
    ((list* 'apl-compose '∘ 'operate-composed
            (list 'apl-compose '\. 'lambda '(o a)
                  (list 'array-outer-product 'o 'a (guard opfn (and (eql 'λωα (first opfn))
                                                                    (eql 'apl-call (caadr opfn))))))
            _ _ rest)
     ;; invert a right-composition of an [∘. outer product] operation
     `(apl-compose ∘ operate-composed :inverted-op nil
                     (λωα (inverse-outer-product omega (λωα ,(invert-function (second opfn)))
                                                 nil alpha))
                     ,@rest))
    ((list* 'apl-compose '∘ 'operate-composed op1 nil nil
            (list 'apl-compose '\. 'lambda '(o a)
                  (list 'array-outer-product 'o 'a (guard opfn (and (eql 'λωα (first opfn))
                                                                    (eql 'apl-call (caadr opfn))))))
            _ _ rest)
     ;; invert a left-composition of an [∘. outer product] operation
     `(apl-compose ∘ operate-composed ,op1 nil nil :inverted-op nil
                     (λωα (inverse-outer-product alpha (λωα ,(invert-function (second opfn)))
                                                 omega))
                     ,@rest))
    ((list* 'apl-compose '∘ 'operate-composed (guard op1 (not (characterp op1)))
            nil nil op2-sym _ _ remaining)
     ;; invert a [∘ compose] operation with a value on the left
     (let ((dyinv-forms (resolve-function :dyadic-inverse op2-sym)))
       `(apl-compose ∘ operate-composed ,op1 nil nil ,op2-sym nil
                       (λωα (apl-call ,op2-sym ,(getf dyinv-forms :right-composed) omega alpha))
                       ,@remaining)))
    ((list* 'apl-compose '∘ 'operate-composed op1-sym _ _ (guard op2 (not (characterp op2)))
            nil nil remaining)
     ;; invert a [∘ compose] operation with a value on the right
     (let ((dyinv-forms (resolve-function :dyadic-inverse op1-sym)))
       `(apl-compose ∘ operate-composed ,op1-sym nil
                       (λωα (apl-call ,op1-sym ,(getf dyinv-forms :plain) omega alpha))
                       ,op2 nil nil ,@remaining)))
    ((list* 'apl-compose '∘ 'operate-composed right-fn-sym right-fn-form-monadic right-fn-form-dyadic
            left-fn-sym left-fn-form-monadic left-fn-form-dyadic remaining)
     ;; invert a [∘ compose] operation with two function operands
     (let ((left-clause
            (if (or (eq :fn left-fn-sym)
                    (not (symbolp left-fn-sym)))
                (list left-fn-sym (invert-function left-fn-form-monadic)
                      (invert-function left-fn-form-dyadic))
                (let ((fn-glyph (aref (string left-fn-sym) 0)))
                  (list left-fn-sym
                        (if (resolve-function :monadic-inverse fn-glyph)
                            `(λω (apl-call ,left-fn-sym ,(resolve-function :monadic-inverse fn-glyph)
                                           omega)))
                        (if (resolve-function :dyadic-inverse fn-glyph)
                            `(λωα (apl-call ,left-fn-sym ,(resolve-function :dyadic-inverse fn-glyph)
                                            omega alpha)))))))
           (right-clause
            (if (or (eq :fn right-fn-sym)
                    (not (symbolp right-fn-sym)))
                (list right-fn-sym (invert-function right-fn-form-monadic)
                      (invert-function right-fn-form-dyadic))
                (let ((fn-glyph (aref (string right-fn-sym) 0)))
                  (list right-fn-sym
                        (if (resolve-function :monadic-inverse fn-glyph)
                            `(λω (apl-call ,right-fn-sym ,(resolve-function :monadic-inverse fn-glyph)
                                           omega)))
                        (if (resolve-function :dyadic-inverse fn-glyph)
                            `(λωα (apl-call ,right-fn-sym ,(resolve-function :dyadic-inverse fn-glyph)
                                            omega alpha))))))))
       (if (and (listp (first right-clause))
                (listp (first left-clause)))
           `(apl-compose ∘ operate-composed ,@left-clause ,@right-clause ,@remaining)
           `(lambda (omega &optional alpha)
              (if (not alpha)
                  (funcall (apl-compose ∘ operate-composed ,@left-clause ,@right-clause ,@remaining)
                           omega)
                  (apl-call ,(if (listp (first right-clause))
                                 :fn (first right-clause))
                            ,(if (listp (first right-clause))
                                 (third (second (second right-clause)))
                                 (resolve-function :monadic-inverse (first right-clause)))
                            ,(if (listp (first left-clause))
                                 (third (third (second (third left-clause))))
                                 `(apl-call ,(if (listp (first left-clause))
                                                 :fn (first left-clause))
                                            ,(getf (resolve-function :dyadic-inverse (first left-clause))
                                                   :plain)
                                            omega alpha))))))))
    ((list* 'apl-compose '\\ 'operate-scanning operand remaining)
     ;; invert a [\ scan] operation
     `(apl-compose \\ operate-scanning ,(invert-function operand) ,@remaining t))
    ((list 'apl-compose '\¨ 'operate-each op-monadic op-dyadic)
     ;; invert an [¨ each] operation
     `(apl-compose \¨ operate-each ,(invert-function op-monadic)
                   ,(invert-function op-dyadic)))
    ((list 'apl-compose '⍨ 'lambda args funcall-form)
     ;; invert a [⍨ commute] operation
     (or (match funcall-form ((list* 'funcall (guard sub-lambda (eql 'λωα (first sub-lambda)))
                                     _)
                              (let* ((fn-glyph (second (second sub-lambda)))
                                     (dyinv-forms (resolve-function
                                                   :dyadic-inverse (aref (string fn-glyph) 0))))
                                `(apl-compose ⍨ lambda ,args
                                                (funcall (λω (apl-call ,fn-glyph
                                                                       ,(getf dyinv-forms :commuted)
                                                                       omega alpha))
                                                         omega)))))
         (error "Composition with ⍨ not invertable.")))
    ((list (guard first (member first '(λω λωα))) second)
     ;; invert a λω or λωα macro lambda expression
     (list first (invert-function second)))
    ((list* 'alambda args options
            (guard declare-form (and (listp declare-form) (eql 'declare (first declare-form))))
            first-form rest-forms)
     ;; invert an arbitrary lambda
     (if rest-forms `(lambda ,args ,declare-form
                             (error "This function has more than one statement and thus cannot be inverted."))
         `(alambda ,args ,options ,declare-form ,(invert-function first-form))))
    ((list* 'alambda args options first-form rest-forms)
     ;; invert an arbitrary lambda
     (if rest-forms `(lambda ,args (declare (ignore ⍵ ⍺))
                             (error "This function has more than one statement and thus cannot be inverted."))
         `(alambda ,args ,options ,(invert-function first-form))))
    ((list* 'apl-call function-symbol function-form arg1 arg2-rest)
     (destructuring-bind (&optional arg2 &rest rest) arg2-rest
       ;; invert an apl-call expression - WIP
       (let* ((function-char (aref (string function-symbol) 0))
              (dyinv-forms (resolve-function :dyadic-inverse function-char))
              (to-invert (or (member arg1 '(⍵ ⍺ omega alpha))
                             (and (listp arg1) (eql 'apl-call (first arg1)))))
              (arg1-var (if to-invert arg1))
              (arg2-var (if (or (member arg2 '(⍵ ⍺ omega alpha))
                                (and (listp arg2) (eql 'apl-call (first arg2))))
                            arg2))
              (last-layer (not (or (and (listp arg1) (eql 'apl-call (first arg1)))
                                   (and (listp arg2) (eql 'apl-call (first arg2))))))
              (to-wrap (or to-wrap #'identity)))
         (flet ((wrapper (item)
                  `(apl-call ,function-symbol ,(if (eq :fn function-symbol)
                                                   (invert-function function-form)
                                                   (if arg2 (or (if to-invert (getf dyinv-forms :plain)
                                                                    (or (getf dyinv-forms :right-composed)
                                                                        (getf dyinv-forms :plain)))
                                                                `(λωα (declare (ignore omega alpha))
                                                                      (error "No dyadic inverse for ~a."
                                                                             ,function-char)))
                                                       (or (resolve-function :monadic-inverse function-char)
                                                           `(λω (declare (ignore omega))
                                                                (error "No monadic inverse for ~a."
                                                                       ,function-char)))))
                             ,@(append (funcall (if (or to-invert (getf dyinv-forms :right-composed))
                                                    #'identity #'reverse)
                                                (append (list (if (or (listp arg1)
                                                                      (and arg1-var (not arg2-var)))
                                                                  (funcall to-wrap item) arg1))
                                                        (if (and (not (member arg2 '(⍵ ⍺)))
                                                                 (and arg2-var (not arg1-var)))
                                                            (list (funcall to-wrap item))
                                                            (if arg2 (list arg2)))))
                                       rest))))
           (if last-layer (wrapper (or arg1-var arg2-var))
               (invert-function (or arg1-var arg2-var) #'wrapper))))))))

(defun april-function-glyph-processor (type glyph spec &optional inverse-spec)
  "Convert a Vex function specification for April into a set of lexicon elements, forms and functions that will make up part of the April idiom object used to compile the language."
  (let ((type (intern (string-upcase type) "KEYWORD"))
        (function-type (intern (string-upcase (first spec)) "KEYWORD"))
        (spec-body (rest spec))
        (inverse-spec-body (rest inverse-spec))
        (inverse-function-type (if inverse-spec (intern (string-upcase (first inverse-spec)) "KEYWORD"))))
    (cond ((eq :symbolic function-type)
           `(,glyph :lexicons (:functions :symbolic-functions)
                    :functions (:symbolic ,(first spec-body))))
          ((keywordp (first spec-body))
           ;; if this is a simple scalar declaration passing through another function
           `(,glyph :lexicons (:functions :scalar-functions :monadic-functions :scalar-monadic-functions
                                          ,@(if (not (eq :monadic function-type))
                                                '(:dyadic-functions :scalar-dyadic-functions))
                                          ,@(if (and inverse-function-type
                                                     (not (eq :dyadic inverse-function-type)))
                                                '(:inverse-monadic-functions
                                                  :scalar-inverse-monadic-functions))
                                          ,@(if (or (eq :dyadic inverse-function-type)
                                                    (eq :ambivalent inverse-function-type))
                                                '(:inverse-dyadic-functions
                                                  :scalar-inverse-dyadic-functions)))
                    :functions ,(append (if (or (eq :ambivalent function-type)
                                                (eq :monadic function-type))
                                            (list :monadic `(scalar-function ,(second spec-body))))
                                        (if (or (eq :ambivalent function-type)
                                                (eq :dyadic function-type))
                                            (list :dyadic `(scalar-function ,(first (last spec-body)))))
                                        (if (or (eq :ambivalent inverse-function-type)
                                                (eq :monadic inverse-function-type))
                                            (list :monadic-inverse
                                                  `(scalar-function ,(first inverse-spec-body))))
                                        (if (or (eq :ambivalent inverse-function-type)
                                                (eq :dyadic inverse-function-type))
                                            (list :dyadic-inverse
                                                  (funcall (lambda (spec)
                                                             (loop :for (key val) :on spec :by #'cddr
                                                                :append (list key `(scalar-function ,val))))
                                                           (if (eq :dyadic inverse-function-type)
                                                               inverse-spec-body (rest inverse-spec-body)))))
                                        )))
          (t `(,glyph :lexicons ,(cond ((eq :functions type)
                                        `(:functions ,@(if (eq :ambivalent function-type)
                                                           '(:monadic-functions :dyadic-functions)
                                                           (list (intern (string-upcase
                                                                          (concatenate 'string
                                                                                       (string function-type)
                                                                                       "-" (string type)))
                                                                         "KEYWORD")))
                                                     ,@(if (eq :ambivalent inverse-function-type)
                                                           '(:inverse-monadic-functions
                                                             :inverse-dyadic-functions)
                                                           (list (intern (string-upcase
                                                                          (concatenate
                                                                           'string
                                                                           (string inverse-function-type)
                                                                           "-" (string type)))
                                                                         "KEYWORD")))
                                                     ,@(if (and (or (eq :ambivalent function-type)
                                                                    (eq :monadic function-type))
                                                                (eql 'scalar-function (caar spec-body)))
                                                           '(:scalar-functions :scalar-monadic-functions))
                                                     ,@(if (or (and (eq :dyadic function-type)
                                                                    (eql 'scalar-function (caar spec-body)))
                                                               (and (eq :ambivalent function-type)
                                                                    (eql 'scalar-function (caadr spec-body))))
                                                           '(:scalar-functions :scalar-dyadic-functions))
                                                     ,@(if (and (or (eq :ambivalent inverse-function-type)
                                                                    (eq :monadic inverse-function-type))
                                                                (eql 'scalar-function
                                                                     (caar inverse-spec-body)))
                                                           '(:inverse-scalar-functions
                                                             :inverse-scalar-monadic-functions))
                                                     ,@(if (or (and (eq :dyadic inverse-function-type)
                                                                    (listp (car inverse-spec-body))
                                                                    (eql 'scalar-function
                                                                         (caar inverse-spec-body)))
                                                               (and (eq :ambivalent function-type)
                                                                    (listp (cadr inverse-spec-body))
                                                                    (eql 'scalar-function
                                                                         (caadr inverse-spec-body))))
                                                           '(:inverse-scalar-functions
                                                             :inverse-scalar-dyadic-functions))))
                                       ((eq :operators type)
                                        `(:operators ,(if (eq :lateral function-type)
                                                          :lateral-operators
                                                          (if (eq :pivotal function-type)
                                                              :pivotal-operators :unitary-operators)))))
                      ,@(cond ((eq :functions type)
                               `(:functions ,(append (if (or (eq :ambivalent function-type)
                                                             (eq :monadic function-type))
                                                         (list :monadic (first spec-body)))
                                                     (if (eq :ambivalent function-type)
                                                         (list :dyadic (second spec-body))
                                                         (if (eq :dyadic function-type)
                                                             (list :dyadic (first spec-body))))
                                                     (if (or (eq :ambivalent inverse-function-type)
                                                             (eq :monadic inverse-function-type))
                                                         (list :monadic-inverse
                                                               (first inverse-spec-body)))
                                                     (if (eq :ambivalent inverse-function-type)
                                                         (list :dyadic-inverse (rest inverse-spec-body))
                                                         (if (eq :dyadic inverse-function-type)
                                                             (list :dyadic-inverse inverse-spec-body))))))
                              ((eq :operators type)
                               `(:operators ,(first spec-body)))))))))

(defmacro specify-demo (title params &rest sections)
  (let ((params (rest params)))
    `(progn (defun ,(intern "RUN-TESTS" (package-name *package*)) ()
              (format t "~a ｢~a｣" ,title ,(package-name *package*))
              (princ #\Newline)
              ,@(if (getf params :description)
                    `((princ ,(getf params :description))
                      (princ #\Newline)
                      (princ #\Newline)))
              ,@(if (assoc :tests sections)
                    (let* ((test-count 0)
                           (items (loop :for item :in (rest (assoc :tests sections))
                                     :append (case (intern (string-upcase (first item)) "KEYWORD")
                                               (:provision `((format t "  ] ~a~%" ,(second item))
                                                             (april (with (:space ,(getf params :space)))
                                                                    ,(second item))))
                                               (:is (incf test-count)
                                                    `((format t "  _ ~a" ,(second item))
                                                      (is (april (with (:space ,(getf params :space)))
                                                                 ,(second item))
                                                          ,(third item) :test #'equalp)))))))
                      `((progn (setq prove:*enable-colors* nil)
                               (plan ,test-count)
                               ,@items (finalize)
                               (setq prove:*enable-colors* t)
                               (format t "~%~%")))))))))

;; a secondary package containing tools for the extension of April idioms
(defpackage #:april.idiom-extension-tools
  (:import-from :april #:extend-vex-idiom #:april-function-glyph-processor #:scalar-function)
  (:export #:extend-vex-idiom #:april-function-glyph-processor #:scalar-function
           #:λω #:λωα #:λωχ #:λωαχ))

;; a secondary package containing tools for specifying April demo packages
(defpackage #:april.demo-definition-tools
  (:import-from :april #:specify-demo)
  (:export #:specify-demo))
