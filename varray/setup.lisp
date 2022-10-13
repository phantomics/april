;;;; setup.lisp

(in-package #:varray)

(defun system-command-exists (command-string &optional prefix)
  "Check for the existence of a shell command under the host operating system."
  (if (not prefix) (setq prefix ""))
  (zerop (multiple-value-bind (1st 2nd error-code)
	     (uiop:run-program (format nil "~acommand -v ~a" prefix command-string)
			       :ignore-error-status t)
	   (declare (ignore 1st 2nd))
	   error-code)))

(defun count-cpus ()
  "Count the available threads in the system, accounting for different operating systems."
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
      (if (zerop (length output))
	  1 (read-from-string output)))))

;; specialized types for April virtual arrays
(deftype ava-worker-count () `(integer 0 ,(max 1 (1- (count-cpus)))))
(deftype ava-rank () `(integer 0 ,(1- array-rank-limit)))
(deftype ava-dimension () `(integer 0 ,(1- array-dimension-limit)))
(deftype ava-size () `(integer 0 ,(1- array-total-size-limit)))

(defparameter *workers-count* (max 1 (1- (count-cpus))))

;; (defun cpro-clause2 (ctype symbols &rest body)
;;   (let ((factors (getf symbols :factors))
;;         (factor-index (getf symbols :factor-index))
;;         (f (getf symbols :factor))
;;         (i (getf symbols :index))
;;         (d (getf symbols :dimension))
;;         (thix (getf symbols :this-index))
;;         (remaining (getf symbols :remaining))
;;         (loop-clauses (getf symbols :loop-clauses))
;;         (remainder (gensym))
;;         (varray (getf symbols :varray)))
;;     (case ctype
;;       (:integer
;;        `(loop :for ,factor-index :in ,factors :for ,i :from 0
;;               ,@(if d `(:for ,d :in (shape-of ,varray)))
;;               ,@loop-clauses
;;               :do (multiple-value-bind (,thix ,remainder) (floor ,remaining ,factor-index)
;;                     ,@body (setf ,remaining ,remainder))))
;;       (:encoded
;;        `(loop :for ,i :below (rank-of ,varray)
;;               :do (let ((,this-index (ldb (byte byte (* byte ,i)) ,index)))
;;                     ,@body)))
;;       (t `(loop :for ,factor-index :in ,factors :for ,i :from 0
;;                 ,@(if d `(:for ,d :in (shape-of ,varray)))
;;                 :do (multiple-value-bind (,thix ,remainder) (floor ,remaining ,factor-index)
;;                       (print (list :ti ,thix))
;;                       ,@body (setf ,remaining ,remainder)))))))

;; `(intraverser (:varray varray :factors factors :dimension dimension :this-index this-index
;;                :type-assign type :ctype-assign ctype :ctype ctype)
;;    (lambda (i)
;;      (let ((remaining (the +index-type+ i)) (valid t) (factor)
;;            (oindex (the +index-type+ 0)))
;;        ;; determine the presence of an input element in the
;;        ;; output, used for selective assignment i.e. (1 1⍉M)←0
;;        (print (list :ii i))
;;        (cpro-clause (:factor f :index i :varray #() :factors '(12 3 1)
;;                      :dimension d :this-index thix :remaining remaining)
;;          (incf oindex (* i thix)))
;;        oindex
;;        )))

;; `(varray::intraverser (:varray varray :factors factors :dimension dimension :this-index this-index) (do-stuff))

;; (defmacro intraverser (symbols &body body)
;;   (let ((widths '(16 32 64))
;;         (typekey (getf symbols :typekey))
;;         (varray (getf symbols :varray))
;;         (factors (getf symbols :factors))
;;         (dimension (getf symbols :dimension))
;;         (this-index (getf symbols :this-index))
;;         (i (gensym)) (f (gensym)) (remainder (gensym))
;;         (index (gensym)) (remaining (gensym)))
;;     (labels ((cpro-clause (ctype symbols &rest body)
;;                (let ((factors (getf symbols :factors))
;;                      (factor-index (getf symbols :factor-index))
;;                      (f (getf symbols :factor))
;;                      (i (getf symbols :index))
;;                      (d (getf symbols :dimension))
;;                      (thix (getf symbols :this-index))
;;                      (remaining (getf symbols :remaining))
;;                      (loop-clauses (getf symbols :loop-clauses))
;;                      (remainder (gensym))
;;                      (varray (getf symbols :varray)))
;;                  (case ctype
;;                    (:integer
;;                     `(loop :for ,factor-index :in ,factors :for ,i :from 0
;;                            ,@(if d `(:for ,d :in (shape-of ,varray)))
;;                            ,@loop-clauses
;;                            :do (multiple-value-bind (,thix ,remainder) (floor ,remaining ,factor-index)
;;                                  ,@body (setf ,remaining ,remainder))))
;;                    (:encoded
;;                     `(loop :for ,i :below (rank-of ,varray)
;;                            :do (let ((,this-index (ldb (byte byte (* byte ,i)) ,index)))
;;                                  ,@body)))
;;                    (t `(loop :for ,factor-index :in ,factors :for ,i :from 0
;;                              ,@(if d `(:for ,d :in (shape-of ,varray)))
;;                              :do (multiple-value-bind (,thix ,remainder) (floor ,remaining ,factor-index)
;;                                    (print (list :ti ,thix))
;;                                    ,@body (setf ,remaining ,remainder)))))))
;;              (replace-symbols (form size ctype)
;;                (loop :for item :in form :for ix :from 0
;;                      :collect (cond ((listp item)
;;                                      ;; assign sublexicon based on symbol macros for invocation
;;                                      (if (eql 'cpro-clause (first item))
;;                                          (apply #'cpro-clause (cons ctype (rest item)))
;;                                          (replace-symbols item size ctype)))
;;                                     ((eql '+index-type+ item)
;;                                      (if (not size)
;;                                          t `(unsigned-byte ,size)))
;;                                     (t item))))
;;              (wrap (form size itype)
;;                `(symbol-macrolet ((,(getf symbols :ctype) ,itype))
;;                   (the (function ((unsigned-byte ,size)) (unsigned-byte ,size))
;;                        ,@(replace-symbols form size itype)))))
;;       `(case ,typekey
;;          ,@(loop :for b :in widths :collect (list (intern (format nil "I~a" b) "KEYWORD")
;;                                                   (wrap body b :integer)))
;;          ;; ,@(loop :for b :in widths :append (list (intern (format nil "E~a" b) "KEYWORD")
;;          ;;                                         (encode-clause b)))
;;          (t ,@(replace-symbols body nil t))
;;          ))))

(defmacro intraverser-ex (&rest forms)
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
             (process-var-range (form types vars &optional params key-ints var-widths)
               (if vars
                   (destructuring-bind (var-type var-symbol &rest rest-vars) vars
                     ;; (print (list :vt var-type var-symbol rest-vars (getf widths var-type)))
                     (case var-type
                       (:rank
                        (loop :for i :below (floor (getf params :base-width)
                                                   (getf params :coordinate-width))
                              :do (process-var-range form types rest-vars params (cons i key-ints)
                                                     (append (list var-symbol i) var-widths))))
                       (:rank-plus
                        (loop :for i :below (floor (getf params :base-width)
                                                   (getf params :coordinate-width))
                              :do (process-var-range form types rest-vars params (cons (1+ i) key-ints)
                                                     (append (list var-symbol (1+ i))
                                                             var-widths))))
                       (:address-fraction
                        ;; address fractions are traversed based on the ratio of coordinate
                        ;; width to base width, as needed for functions like ⌽, where movement along an axis
                        (loop :for i :below (floor (getf params :base-width)
                                                   (getf params :coordinate-width))
                              :do (let ((width (* i (getf params :coordinate-width))))
                                    (process-var-range form types rest-vars params (cons i key-ints)
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
                                      ;; these variable types impose a width limit on subordinate variables;
                                      ;; i.e. the sub-byte values of an integer must be half or less of
                                      ;; that integer's width
                                      (process-var-range
                                       form types (cddr vars)
                                       sub-params
                                       (if (not (member var-type '(:lindex-width :eindex-width
                                                                   :cindex-width)))
                                           key-ints (cons width key-ints))
                                       (append (list var-symbol (or sub-base-width width))
                                               var-widths)))))))
                   (progn (push (process-form form var-widths) output)
                          ;; the sub-base-width is not included in the key list
                          (push `(gethash ',(list types (reverse key-ints)) ,table)
                                output)))))
      (loop :for form :in forms
            :do (let* ((types (caar form))
                       (base-type (first types))
                       (vars (cadar form)))
                  (process-var-range (second form) types vars)))
      `(let ((,table (make-hash-table :test #'equalp)))
         (setf ,@output)
         ,table))))

(let ((default-function
        (lambda (increment vset-size degrees rlen)
          (lambda (i)
            (+ (mod i increment)
               (* vset-size (floor i vset-size))
               (let ((degree (if (integerp degrees)
                                 degrees
                                 (if (not (arrayp degrees))
                                     0 (row-major-aref
                                        degrees
                                        (+ (mod i increment)
                                           (* increment (floor i vset-size))))))))
                 (* increment (mod (+ degree (floor i increment))
                                   rlen)))))))
      (function-table
        (intraverser-ex
         ;; (((:integer) (:lindex-width +lindex-width+ :rank-width +rank-width+
         ;;               :sub-base-width +sub-base-width+))
         ;;  (the (function ((unsigned-byte +lindex-width+) (unsigned-byte +rank-width+)
         ;;                                                 (unsigned-byte +sub-base-width+))
         ;;                 function)
         ;;       (lambda (increment vset-size degrees rlen)
         ;;         (declare (optimize (speed 3) (safety 0))
         ;;                  (type (unsigned-byte +lindex-width+) increment vset-size)
         ;;                  (type (unsigned-byte +sub-base-width+) degrees rlen))
         ;;         ;; (print (list :cc degrees))
         ;;         (if (integerp degrees)
         ;;             (the (function ((unsigned-byte +lindex-width+)) (unsigned-byte +lindex-width+))
         ;;                  (lambda (i)
         ;;                    (declare (type (unsigned-byte +lindex-width+) i))
         ;;                    (the (unsigned-byte +lindex-width+)
         ;;                         (+ (the (unsigned-byte +sub-base-width+)
         ;;                                 (mod i increment))
         ;;                            (the (unsigned-byte +sub-base-width+)
         ;;                                 (* vset-size (floor i vset-size)))
         ;;                            (the (unsigned-byte +sub-base-width+)
         ;;                                 (* increment (the (unsigned-byte +sub-base-width+)
         ;;                                                   (mod (the (unsigned-byte +sub-base-width+)
         ;;                                                             (+ degrees (floor i increment)))
         ;;                                                        rlen))))))))
         ;;             (the (function ((unsigned-byte +lindex-width+)) (unsigned-byte +lindex-width+))
         ;;                  (lambda (i)
         ;;                    (declare (type (unsigned-byte +lindex-width+) i))
         ;;                    (the (unsigned-byte +lindex-width+)
         ;;                         (+ (the (unsigned-byte +sub-base-width+)
         ;;                                 (mod i increment))
         ;;                            (the (unsigned-byte +sub-base-width+)
         ;;                                 (* vset-size (floor i vset-size)))
         ;;                            (let ((degree (the (unsigned-byte +lindex-width+)
         ;;                                               (if (not (arrayp degrees))
         ;;                                                   0 (row-major-aref
         ;;                                                      degrees
         ;;                                                      (+ (the (unsigned-byte +lindex-width+)
         ;;                                                              (mod i increment))
         ;;                                                         (the (unsigned-byte +lindex-width+)
         ;;                                                              (* increment
         ;;                                                                 (floor i vset-size)))))))))
         ;;                              (the (unsigned-byte +lindex-width+)
         ;;                                   (* increment (the (unsigned-byte +sub-base-width+)
         ;;                                                     (mod (the (unsigned-byte
         ;;                                                                +sub-base-width+)
         ;;                                                               (+ degree
         ;;                                                                  (floor i increment)))
         ;;                                                          rlen)))))))))))))
         (((:encoded) (:eindex-width +eindex-width+ :cindex-width +cindex-width+ :rank-width +rank-width+
                       :sub-base-width +sub-base-width+ :address-fraction +address-fraction+))
          (the (function ((unsigned-byte +sub-base-width+) (unsigned-byte +sub-base-width+))
                         function)
               (lambda (degrees rlen)
                 (declare (optimize (speed 3) (safety 0))
                          (type (unsigned-byte +sub-base-width+) degrees rlen))
                 ;; (let ((byte-offset (* +cindex-width+ dindex)))
                 ;; (print (list :fn degrees rlen +cindex-width+ +eindex-width+ +address-fraction+))
                 (the (function ((unsigned-byte +eindex-width+)) (unsigned-byte +eindex-width+))
                      (lambda (i)
                        (declare (type (unsigned-byte +eindex-width+) i))
                        (let ((iindex (the (unsigned-byte +cindex-width+)
                                           (ldb (byte +cindex-width+ +address-fraction+)
                                                i))))
                          (the (unsigned-byte +eindex-width+)
                               (dpb (the (unsigned-byte +cindex-width+)
                                         (mod (+ iindex degrees) rlen))
                                    (byte +cindex-width+ +address-fraction+)
                                    i)))))))))))
  
  (defun indexer-turn (axis idims typekey iwidth itype &optional degrees)
    "Return indices of an array rotated as with the [⌽ rotate] or [⊖ rotate first] functions."
    ;; (declare (optimize (speed 3) (safety 0)))
    (let* ((irank (length idims))
           (rlen (nth axis idims))
           (increment (reduce #'* (nthcdr (1+ axis) idims)))
           (vset-size (the t (* increment rlen))))
      (if degrees
          ;; TODO: implement a system for accelerated rotation when degrees are an array
          (if (not (integerp degrees))
              (lambda (i)
                (the (unsigned-byte 62)
                     (+ (the (unsigned-byte 62) (mod i increment))
                        (the (unsigned-byte 62) (* vset-size (floor i vset-size)))
                        (let ((degree (the fixnum
                                           (if (not (arrayp degrees))
                                               0 (row-major-aref
                                                  degrees
                                                  (+ (the (unsigned-byte 62)
                                                          (mod i increment))
                                                     (the (unsigned-byte 62)
                                                          (* increment (floor i vset-size)))))))))
                          (the (unsigned-byte 62)
                               (* increment (the fixnum (mod (the fixnum (+ degree (floor i increment)))
                                                             rlen))))))))
              (list (let ((match (gethash (list '(:encoded)
                                                (list iwidth itype (- irank 1 axis)))
                                          function-table)))
                      (when match (funcall match degrees rlen)))
                    (funcall default-function increment vset-size degrees rlen)))
          (lambda (i)
            (+ (mod i increment)
               (* vset-size (floor i vset-size))
               (* increment
                  (the fixnum (abs (- (mod (the fixnum (floor i increment))
                                           rlen)
                                      (1- rlen)))))))
          ;; (intraverser (:typekey typekey)
          ;;   (:integer
          ;;    (the +function-type+
          ;;         (lambda (i) +optimize-for-type+
          ;;           (the +index-type+
          ;;                (+ (the +index-type+ (mod i increment))
          ;;                   (the +index-type+ (* vset-size (floor i vset-size)))
          ;;                   (the +index-type+
          ;;                        (* increment
          ;;                           (the fixnum (abs (- (mod (the fixnum (floor i increment))
          ;;                                                    rlen)
          ;;                                               (1- rlen)))))))))))
          ;;   (:encoded))
          ;; (funcall default-function increment vset-size degrees rlen)
          ))))

(defmacro intraverser (symbols &rest forms)
  (let (;; (widths '(16)) ; '(8 16 32 64))
        (widths '(8 16 32 64))
        (typekey (getf symbols :typekey))
        (linear (getf symbols :linear))
        (variants (gensym)) (default (gensym)))
    (labels ((replace-symbols (form &optional size)
               (loop :for item :in form :for ix :from 0
                     :collect (if (listp item) (replace-symbols item size)
                                  ;; assign sublexicon based on symbol macros for invocation
                                  (case item
                                    (+index-width+ size)
                                    (+index-type+ (if (not size) t `(unsigned-byte ,size)))
                                    (+function-type+ (if (not size) t `(function ((unsigned-byte ,size))
                                                                                 (unsigned-byte ,size))))
                                    (+root-function-type+
                                     (if (not size) t `(function ((unsigned-byte ,size))
                                                                 function)))
                                    (+optimize-for-type+
                                     (when size `(declare (optimize (speed 3) (safety 0)))))
                                    (t item))))))
      `(let ((,variants (make-hash-table :test #'eq))
             (,default ,(when (not linear)
                          (replace-symbols (second (assoc :integer forms))))))
         ,@(if (assoc :integer forms)
               (loop :for b :in widths
                     :collect `(setf (gethash ,(intern (format nil "I~a" b) "KEYWORD") ,variants)
                                     ,(replace-symbols (second (assoc :integer forms)) b))))
         ,@(if (assoc :encoded forms)
               (loop :for b :in widths
                     :collect `(setf (gethash ,(intern (format nil "E~a" b) "KEYWORD") ,variants)
                                     ,(replace-symbols (second (assoc :encoded forms)) b))))
         ,(if linear `(gethash ,typekey ,variants)
              `(list (gethash ,typekey ,variants)
                     ,default))))))

;; (defun indexer-turn (axis idims typekey &optional degrees)
;;   "Return indices of an array rotated as with the [⌽ rotate] or [⊖ rotate first] functions."
;;   ;; (declare (optimize (speed 3) (safety 0)))
;;   (let* ((axis (the t axis))
;;          (rlen (the t (nth axis idims)))
;;          (increment (the t (reduce #'* (nthcdr (1+ axis) idims))))
;;          (irank (length idims))
;;          (vset-size (the t (* increment rlen))))
;;     ;; (print (list :ty typekey))
;;     (if degrees
;;         ;; TODO: implement a system for accelerated rotation when degrees are an array
;;         (if (not (integerp degrees))
;;             (lambda (i)
;;               (the (unsigned-byte 62)
;;                    (+ (the (unsigned-byte 62) (mod i increment))
;;                       (the (unsigned-byte 62) (* vset-size (floor i vset-size)))
;;                       (let ((degree (the fixnum
;;                                          (if (not (arrayp degrees))
;;                                              0 (row-major-aref
;;                                                 degrees
;;                                                 (+ (the (unsigned-byte 62)
;;                                                         (mod i increment))
;;                                                    (the (unsigned-byte 62)
;;                                                         (* increment (floor i vset-size)))))))))
;;                         (the (unsigned-byte 62)
;;                              (* increment (the fixnum (mod (the fixnum (+ degree (floor i increment)))
;;                                                            rlen))))))))
;;             (intraverser (:typekey typekey)
;;               (:integer
;;                (the +function-type+
;;                     (lambda (i) +optimize-for-type+
;;                       (the +index-type+
;;                            (+ (the +index-type+ (mod i increment))
;;                               (the +index-type+ (* vset-size (floor i vset-size)))
;;                               (let ((degree (the fixnum
;;                                                  (if (integerp degrees)
;;                                                      degrees
;;                                                      (if (arrayp degrees)
;;                                                          (row-major-aref
;;                                                           degrees
;;                                                           (+ (the +index-type+ (mod i increment))
;;                                                              (the +index-type+
;;                                                                   (* increment (floor i vset-size)))))
;;                                                          0)))))
;;                                 (the +index-type+
;;                                      (* increment (the fixnum (mod (the fixnum (+ degree (floor i increment)))
;;                                                                    rlen))))))))))
;;               (:encoded
;;                (let* ((fraction (floor +index-width+ irank))
;;                       (dindex (- irank 1 axis))
;;                       (byte (loop :for w :in '(8 16 32 64)
;;                                   :when (< fraction w) :return (floor w 2))))
;;                  (the +function-type+
;;                       (lambda (i) +optimize-for-type+
;;                         (let ((iindex (the +index-type+ (ldb (byte byte (* byte dindex)) i))))
;;                           (dpb (mod (+ iindex degrees) rlen)
;;                                (byte byte (* byte dindex))
;;                                i))))))))
;;         ;; (lambda (i)
;;         ;;   (declare (type (unsigned-byte 62) i))
;;         ;;   (the (unsigned-byte 62)
;;         ;;        (+ (the (unsigned-byte 62) (mod i increment))
;;         ;;           (the (unsigned-byte 62) (* vset-size (floor i vset-size)))
;;         ;;           (the (unsigned-byte 62)
;;         ;;                (* increment
;;         ;;                   (the fixnum (abs (- (mod (the fixnum (floor i increment))
;;         ;;                                            rlen)
;;         ;;                                       (1- rlen)))))))))
;;         (intraverser (:typekey typekey)
;;           (:integer
;;            (the +function-type+
;;                 (lambda (i) +optimize-for-type+
;;                   (the +index-type+
;;                        (+ (the +index-type+ (mod i increment))
;;                           (the +index-type+ (* vset-size (floor i vset-size)))
;;                           (the +index-type+
;;                                (* increment
;;                                   (the fixnum (abs (- (mod (the fixnum (floor i increment))
;;                                                            rlen)
;;                                                       (1- rlen)))))))))))
;;           (:encoded))
;;         )))

#|

(THE (FUNCTION ((SIMPLE-ARRAY (UNSIGNED-BYTE 8) (4))) FUNCTION)
     (LAMBDA (INDICES)
       (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0))
                (TYPE (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (4)) INDICES))
       (PRINT (LIST :IN INDICES))
       (THE (FUNCTION ((UNSIGNED-BYTE 32)) (UNSIGNED-BYTE 32))
            (LAMBDA (I)
              (DECLARE (TYPE (UNSIGNED-BYTE 32) I))
              (LET ((IINDEX (THE (UNSIGNED-BYTE 32) 0)))
                (LOOP :FOR A :of-type (unsigned-byte 8) :ACROSS INDICES
                      :FOR N :of-type (unsigned-byte 8) :FROM 0
                      :DO (SETF IINDEX
                                (DPB (LDB (BYTE 8 (* N 8)) I)
                                     (BYTE 8 (* A 8)) IINDEX)))
                IINDEX)))))

(THE (FUNCTION ((SIMPLE-ARRAY (UNSIGNED-BYTE 8) (4))) FUNCTION)
               (LAMBDA (INDICES)
                 (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0))
                          (TYPE (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (4)) INDICES))
                 (THE (FUNCTION ((UNSIGNED-BYTE 64)) (UNSIGNED-BYTE 64))
                      (LAMBDA (I)
                        (DECLARE (TYPE (UNSIGNED-BYTE 64) I))
                        (LET ((IINDEX (THE (UNSIGNED-BYTE 64) 0)))
                          (LOOP :FOR A :OF-TYPE (UNSIGNED-BYTE
                                                 16) :ACROSS INDICES
                                :FOR N :OF-TYPE (UNSIGNED-BYTE 8) :FROM 0
                                :DO (SETF IINDEX
                                            (DPB (LDB (BYTE 16 (* N 16)) I)
                                                 (BYTE 16 (* A 16)) IINDEX)))
                          IINDEX)))))

|#

(let (;; (default-function
      ;;   (lambda (increment vset-size degrees rlen)
      ;;     (lambda (i)
      ;;       (+ (mod i increment)
      ;;          (* vset-size (floor i vset-size))
      ;;          (let ((degree (if (integerp degrees)
      ;;                            degrees
      ;;                            (if (not (arrayp degrees))
      ;;                                0 (row-major-aref
      ;;                                   degrees
      ;;                                   (+ (mod i increment)
      ;;                                      (* increment (floor i vset-size))))))))
      ;;            (* increment (mod (+ degree (floor i increment))
      ;;                              rlen)))))))
      (regular-handler-table
        (intraverser-ex
         ;; (((:integer) (:lindex-width +lindex-width+ :sub-base-width +sub-base-width+
         ;;               ;; :rank +rank+
         ;;               :rank-width +rank-width+))
         ;;  (the (function (;; (simple-array (unsigned-byte +sub-base-width+) (+rank+))
         ;;                  ;; (simple-array (unsigned-byte +sub-base-width+) (+rank+))
         ;;                  vector vector)
         ;;                 function)
         ;;       (lambda (od-factors s-factors)
         ;;         (declare (optimize (speed 3) (safety 0)))
         ;;         (the (function ((unsigned-byte +lindex-width+)) (unsigned-byte +lindex-width+))
         ;;              (lambda (i)
         ;;                (declare (type (unsigned-byte +lindex-width+) i))
         ;;                (let* ((remaining (the (unsigned-byte +lindex-width+) i))
         ;;                       (oindex (the (unsigned-byte +lindex-width+) 0)))
         ;;                  (loop :for od :across od-factors :for s :across s-factors
         ;;                        :collect (multiple-value-bind (index remainder) (floor remaining od)
         ;;                                   (incf oindex (* index s))
         ;;                                   (setq remaining remainder)))
         ;;                  oindex))))))
         (((:encoded) (:eindex-width +eindex-width+ :cindex-width +cindex-width+
                       :rank-plus +rank-plus+ :rank-width +rank-width+))
          (the (function ((simple-array (unsigned-byte 8) (+rank-plus+)))
                         function)
               (lambda (indices)
                 (declare (optimize (speed 3) (safety 0))
                          (type (simple-array (unsigned-byte 8) (+rank-plus+))
                                indices)) ;; TODO: fix hardcoded type
                 ;; (print (list :in indices))
                 (the (function ((unsigned-byte +eindex-width+)) (unsigned-byte +eindex-width+))
                      (lambda (i)
                        (declare (type (unsigned-byte +eindex-width+) i))\
                        (let ((iindex (the (unsigned-byte +eindex-width+) 0)))
                          (loop :for a :of-type (unsigned-byte +cindex-width+) :across indices
                                :for n :of-type (unsigned-byte 8) :from 0
                                :do (setf iindex (dpb (ldb (byte +cindex-width+ (* n +cindex-width+)) i)
                                                      (byte +cindex-width+ (* a +cindex-width+))
                                                      iindex)))
                          iindex))))))))
      (diagonal-handler-table
        (intraverser-ex
         ;; (((:integer) (:lindex-width +lindex-width+ :sub-base-width +sub-base-width+ 
         ;;                             ;; :rank +rank+
         ;;               :rank-width +rank-width+))
         ;;  (the (function (;; (simple-array (unsigned-byte +sub-base-width+) (+rank+))
         ;;                  ;; (simple-array (unsigned-byte +rank-width+) (+rank+))
         ;;                  vector vector)
         ;;                 function)
         ;;       (lambda (id-factors indices)
         ;;         (the (function ((unsigned-byte +lindex-width+)) (unsigned-byte +lindex-width+))
         ;;              (lambda (i)
         ;;                (declare (optimize (speed 3) (safety 0))
         ;;                         (type (unsigned-byte +lindex-width+) i))
         ;;                (let ((iindex (the (unsigned-byte +lindex-width+) 0))
         ;;                      (remaining (the (unsigned-byte +lindex-width+) i)))
         ;;                  (loop :for ox :from 0 :for of :across od-factors
         ;;                        :do (multiple-value-bind (index remainder) (floor remaining of)
         ;;                              (loop :for a :of-type (unsigned-byte +rank-width+)
         ;;                                      :across indices
         ;;                                    :for ax :from 0 :when (= a ox)
         ;;                                    :do (incf iindex (* index (aref id-factors ax))))
         ;;                              (setq remaining remainder)))
         ;;                  (the (unsigned-byte +lindex-width+) iindex)))))))
         (((:encoded) (:eindex-width +eindex-width+ :cindex-width +cindex-width+
                       :rank +rank+ :rank-width +rank-width+))
          (the (function ((unsigned-byte +rank-width+)
                          (simple-array (unsigned-byte +rank-width+) (+rank+)))
                         function)
               (lambda (indices)
                 (let ((input-rank (length indices)))
                   (the (function ((unsigned-byte +eindex-width+)) (unsigned-byte +eindex-width+))
                        (lambda (i)
                          (declare (optimize (speed 3) (safety 0))
                                   (type (unsigned-byte +eindex-width+) i))
                          (let ((iindex (the (unsigned-byte +eindex-width+) 0)))
                            (loop :for d :below input-rank :for ox :from 0
                                  :do (let ((this-index (ldb (byte +cindex-width+
                                                                   (* d +cindex-width+))
                                                             i)))
                                        (loop :for a :in indices :for ax :from 0 :when (= a ox)
                                              :do (setf iindex (dpb this-index
                                                                    (byte +cindex-width+
                                                                          (* ax +cindex-width+))
                                                                    iindex)))))
                            (the (unsigned-byte +eindex-width+) iindex)))))))))))
  
  (defun indexer-permute (idims odims alpha is-diagonal iwidth itype &optional is-inverse)
    "Return indices of an array rotated as with the [⌽ rotate] or [⊖ rotate first] functions."
    ;; (declare (optimize (speed 3) (safety 0)))
    (let* ((irank (length idims))
           (positions) (diagonals) (idims-reduced) (idfactor 1) (odfactor 1)
           (id-factors (make-array irank :element-type '(unsigned-byte 62) ;; TODO: remove hard-coding
                                   :initial-contents (reverse (loop :for d :in (reverse idims)
                                                                    :collect idfactor
                                                                    :do (setq idfactor
                                                                              (* d idfactor))))))
           (indices (if alpha (progn (if (vectorp alpha)
                                         (loop :for i :across alpha :for id :in idims :for ix :from 0
                                               :do (when (not (member i positions))
                                                     ;; if a duplicate position is found,
                                                     ;; a diagonal section is being performed
                                                     (push i positions)
                                                     (push id idims-reduced))
                                                   ;; collect possible diagonal indices into diagonal list
                                                   (if (assoc i diagonals)
                                                       (push ix (rest (assoc i diagonals)))
                                                       (push (list i ix) diagonals))
                                               :collect i)
                                         (progn (setq odims idims
                                                      positions (cons alpha positions))
                                                (list alpha))))
                        (reverse (iota irank))))
           ;; TODO: hardcoded 8-bit rank
           (indices-vector (make-array (length indices) :element-type '(unsigned-byte 8)
                                       :initial-contents indices))
           ;; remove indices not being used for diagonal section from diagonal list
           ;; the idims-reduced are a set of the original dimensions without dimensions being elided
           ;; for diagonal section, used to get the initial output array used for diagonal section
           (od-factors (make-array (length odims)))
           (s-factors (make-array irank)))
      
      (loop :for d :in (reverse odims) :for dx :from 0
            :do (setf (aref od-factors (- (length odims) 1 dx)) odfactor
                      odfactor (* d odfactor)))

      (loop :for i :across id-factors :for ix :from 0
            :do (setf (aref s-factors (nth ix indices)) i))
      
      (if (not is-diagonal)
          ;; handle regular permutation cases
          (if is-inverse #'identity ;; selective assignment assigns all elements in a regular permute case
              (let ((match (gethash (list '(:encoded) (list iwidth itype irank))
                                    regular-handler-table)))
                ;; (print (list :ma idims match iwidth itype irank indices-vector))
                (list (when nil ; match
                        ;; (print (list :mm idims iwidth itype irank indices-vector))
                        (funcall match indices-vector))
                      (lambda (i)
                        (let* ((remaining i) (oindex 0))
                          (loop :for od :across od-factors :for s :across s-factors
                                :collect (multiple-value-bind (index remainder) (floor remaining od)
                                           (incf oindex (* index s))
                                           (setq remaining remainder)))
                          oindex)))))
          ;; handle diagonal array traversals
          (if is-inverse
              (lambda (i)
                (let ((remaining i) (valid t) (factor))
                  ;; determine the presence of an input element in the
                  ;; output, used for selective assignment i.e. (1 1⍉M)←0
                  (loop :for if :across id-factors :while valid
                        :do (multiple-value-bind (index remainder) (floor remaining if)
                              (if (and factor (/= index factor))
                                  (setq valid nil)
                                  (setq remaining remainder
                                        factor (or factor index)))))
                  (when valid factor)))
              (let ((match (gethash (list '(:encoded) (list iwidth itype (1- irank)))
                                    diagonal-handler-table)))
                (list (when match (funcall match indices-vector))
                      (lambda (i)
                        (let ((remaining i) (iindex 0))
                          (loop :for ox :from 0 :for of :across od-factors
                                :do (multiple-value-bind (index remainder) (floor remaining of)
                                      (setq remaining remainder)
                                      (loop :for a :in indices :for ax :from 0 :when (= a ox)
                                            :do (incf iindex (* index (aref id-factors ax))))))
                          iindex)))))))))

;; (defun indexer-permute (idims odims alpha is-diagonal a1 a2 &optional is-inverse)
;;   "Return indices of an array permuted as with the [⍉ permute] function."
;;   (declare (ignore a1 a2))
;;   (let* ((irank (length idims))
;;          (positions) (diagonals) (idims-reduced) (idfactor 1) (odfactor 1)
;;          (id-factors (coerce (reverse (loop :for d :in (reverse idims)
;;                                             :collect idfactor :do (setq idfactor (* d idfactor))))
;;                              'vector))
;;          (indices (if alpha (progn (if (vectorp alpha)
;;                                        (loop :for i :across alpha :for id :in idims :for ix :from 0
;;                                              :do (when (not (member i positions))
;;                                                      ;; if a duplicate position is found,
;;                                                      ;; a diagonal section is being performed
;;                                                    (push i positions)
;;                                                    (push id idims-reduced))
;;                                                 ;; collect possible diagonal indices into diagonal list
;;                                                 (if (assoc i diagonals)
;;                                                     (push ix (rest (assoc i diagonals)))
;;                                                     (push (list i ix) diagonals))
;;                                              :collect i)
;;                                        (progn (setq odims idims
;;                                                     positions (cons alpha positions))
;;                                               (list alpha))))
;;                       (reverse (iota irank))))
;;          ;; remove indices not being used for diagonal section from diagonal list
;;          ;; the idims-reduced are a set of the original dimensions without dimensions being elided
;;          ;; for diagonal section, used to get the initial output array used for diagonal section
;;          (od-factors (make-array (length odims)))
;;          (s-factors (make-array irank)))
;;     (loop :for d :in (reverse odims) :for dx :from 0
;;           :do (setf (aref od-factors (- (length odims) 1 dx)) odfactor
;;                     odfactor (* d odfactor)))
;;     (loop :for i :across id-factors :for ix :from 0
;;           :do (setf (aref s-factors (nth ix indices)) i))
;;     (if (not is-diagonal)
;;         ;; handle regular permutation cases
;;         (if is-inverse #'identity ;; selective assignment assigns all elements in a regular permute case
;;             (lambda (i)
;;               (let* ((remaining i) (oindex 0))
;;                 (loop :for ix :in indices :for od :across od-factors :for s :across s-factors
;;                       :collect (multiple-value-bind (index remainder) (floor remaining od)
;;                                  (incf oindex (* index s))
;;                                  (setq remaining remainder)))
;;                 ;; (print (list :oi oindex))
;;                 oindex)))
;;         ;; handle diagonal array traversals
;;         (if is-inverse
;;             (lambda (i)
;;               (let ((remaining i) (valid t) (factor))
;;                 ;; determine the presence of an input element in the
;;                 ;; output, used for selective assignment i.e. (1 1⍉M)←0
;;                 (loop :for ix :from 0 :for if :across id-factors :while valid
;;                       :do (multiple-value-bind (index remainder) (floor remaining if)
;;                             (if (and factor (/= index factor))
;;                                 (setq valid nil)
;;                                 (setq remaining remainder
;;                                       factor (or factor index)))))
;;                 (when valid factor)))
;;             (lambda (i)
;;               (let ((remaining i) (iindex 0))
;;                 (loop :for ox :from 0 :for of :across od-factors
;;                       :do (multiple-value-bind (index remainder) (floor remaining of)
;;                             (setq remaining remainder)
;;                             (loop :for a :in indices :for ax :from 0 :when (= a ox)
;;                                   :do (incf iindex (* index (aref id-factors ax))))))
;;                 iindex))))))

;; array - (2 1)(2 1)(2 1)(2 1) from ta4
