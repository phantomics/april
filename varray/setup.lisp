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

;; `(varray::intraverser (:varray varray :factors factors :dimension dimension :this-index this-index) (do-stuff))

(defmacro intraverser (symbols &rest forms)
  (let ((widths '(8 16 32 64))
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
      ;; `(case ,typekey
      ;;    ,@(loop :for b :in widths :collect (list (intern (format nil "I~a" b) "KEYWORD")
      ;;                                             (replace-symbols (second (assoc :integer forms)) b)))
      ;;    ,@(loop :for b :in widths :collect (list (intern (format nil "E~a" b) "KEYWORD")
      ;;                                             (replace-symbols (second (assoc :encoded forms)) b)))
      ;;    (t ,(replace-symbols (second (assoc :integer forms)))))
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
                     ,default))
         ))))



(defun indexer-turn (axis idims typekey &optional degrees)
  "Return indices of an array rotated as with the [⌽ rotate] or [⊖ rotate first] functions."
  (declare (optimize (speed 3) (safety 0)))
  (let* ((axis (the t axis))
         (rlen (the t (nth axis idims)))
         (increment (the t (reduce #'* (nthcdr (1+ axis) idims))))
         (irank (length idims))
         (vset-size (the t (* increment rlen))))
    ;; (print (list :ty typekey))
    (if degrees
        ;; (lambda (i)
        ;;   (the (unsigned-byte 62)
        ;;        (+ (the (unsigned-byte 62) (mod i increment))
        ;;           (the (unsigned-byte 62) (* vset-size (floor i vset-size)))
        ;;           (let ((degree (the fixnum
        ;;                              (if (integerp degrees)
        ;;                                  degrees
        ;;                                  (if (arrayp degrees)
        ;;                                      (row-major-aref
        ;;                                       degrees
        ;;                                       (+ (the (unsigned-byte 62)
        ;;                                               (mod i increment))
        ;;                                          (the (unsigned-byte 62)
        ;;                                               (* increment (floor i vset-size)))))
        ;;                                      0)))))
        ;;             (the (unsigned-byte 62)
        ;;                  (* increment (the fixnum (mod (the fixnum (+ degree (floor i increment)))
        ;;                                                rlen))))))))
        (intraverser (:typekey typekey)
          (:integer
           (the +function-type+
                (lambda (i) +optimize-for-type+
                  (the +index-type+
                       (+ (the +index-type+ (mod i increment))
                          (the +index-type+ (* vset-size (floor i vset-size)))
                          (let ((degree (the fixnum
                                             (if (integerp degrees)
                                                 degrees
                                                 (if (arrayp degrees)
                                                     (row-major-aref
                                                      degrees
                                                      (+ (the +index-type+ (mod i increment))
                                                         (the +index-type+
                                                              (* increment (floor i vset-size)))))
                                                     0)))))
                            (the +index-type+
                                 (* increment (the fixnum (mod (the fixnum (+ degree (floor i increment)))
                                                               rlen))))))))))
          (:encoded
           (let* ((fraction (floor +index-width+ irank))
                  (dindex (- irank 1 axis))
                  (byte (loop :for w :in '(8 16 32 64)
                              :when (< fraction w) :return (floor w 2))))
             (the +function-type+
                  (lambda (i) +optimize-for-type+
                    (let ((iindex (the +index-type+ (ldb (byte byte (* byte dindex)) i))))
                      ;; (print (list :ii i iindex axis))
                      (dpb (mod (+ iindex (if (integerp degrees)
                                              degrees
                                              (if (arrayp degrees)
                                                  (row-major-aref
                                                   degrees
                                                   (+ (the +index-type+ (mod i increment))
                                                      (the +index-type+
                                                           (* increment (floor i vset-size)))))
                                                  0)))
                                rlen)
                           (byte byte (* byte dindex))
                           i))))))
          )
        ;; (lambda (i)
        ;;   (declare (type (unsigned-byte 62) i))
        ;;   (the (unsigned-byte 62)
        ;;        (+ (the (unsigned-byte 62) (mod i increment))
        ;;           (the (unsigned-byte 62) (* vset-size (floor i vset-size)))
        ;;           (the (unsigned-byte 62)
        ;;                (* increment
        ;;                   (the fixnum (abs (- (mod (the fixnum (floor i increment))
        ;;                                            rlen)
        ;;                                       (1- rlen)))))))))
        (intraverser (:typekey typekey)
          (:integer
           (the +function-type+
                (lambda (i) +optimize-for-type+
                  (the +index-type+
                       (+ (the +index-type+ (mod i increment))
                          (the +index-type+ (* vset-size (floor i vset-size)))
                          (the +index-type+
                               (* increment
                                  (the fixnum (abs (- (mod (the fixnum (floor i increment))
                                                           rlen)
                                                      (1- rlen)))))))))))
          (:encoded))
        )))

(defun indexer-permute (idims odims alpha is-diagonal typekey &optional is-inverse)
  "Return indices of an array permuted as with the [⍉ permute] function."
  (let* ((irank (length idims))
         (positions) (diagonals) (idims-reduced) (idfactor 1) (odfactor 1)
         (id-factors (coerce (reverse (loop :for d :in (reverse idims)
                                            :collect idfactor :do (setq idfactor (* d idfactor))))
                             'vector))
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
            ;; (lambda (i)
            ;;   (let* ((remaining i) (oindex 0))
            ;;     (loop :for od :across od-factors :for s :across s-factors
            ;;           :collect (multiple-value-bind (index remainder) (floor remaining od)
            ;;                      (incf oindex (* index s))
            ;;                      (setq remaining remainder)))
            ;;     oindex))
            (intraverser (:typekey typekey)
              (:integer
               (the +function-type+
                    (lambda (i) +optimize-for-type+
                      (let* ((remaining (the +index-type+ i))
                             (oindex (the +index-type+ 0)))
                        (loop :for od :across od-factors :for s :across s-factors
                              :collect (multiple-value-bind (index remainder) (floor remaining od)
                                         (incf oindex (* index s))
                                         (setq remaining remainder)))
                        oindex))))
              (:encoded
               (let* ((fraction (floor +index-width+ irank))
                      (byte (loop :for w :in '(8 16 32 64)
                                  :when (< fraction w) :return (floor w 2))))
                 (the +function-type+
                      (lambda (i) +optimize-for-type+
                        (let ((iindex (the +index-type+ 0)))
                          (loop :for a :in indices :for n :from 0
                                :do (dpb (ldb (byte byte (* byte n)) i)
                                         (byte byte (* byte a))
                                         iindex))
                          iindex))))))
            )
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
            ;; (lambda (i)
            ;;   (let ((remaining i) (iindex 0))
            ;;     (loop :for ox :from 0 :for of :across od-factors
            ;;           :do (multiple-value-bind (index remainder) (floor remaining of)
            ;;                 (setq remaining remainder)
            ;;                 (loop :for a :in indices :for ax :from 0 :when (= a ox)
            ;;                       :do (incf iindex (* index (aref id-factors ax))))))
            ;;     iindex))
            (intraverser (:typekey typekey)
              (:integer
               (the +function-type+
                    (lambda (i) +optimize-for-type+
                      (let ((iindex (the +index-type+ 0))
                            (remaining (the +index-type+ i)))
                        (loop :for ox :from 0 :for of :across od-factors
                              :do (multiple-value-bind (index remainder) (floor remaining of)
                                    (loop :for a :in indices :for ax :from 0 :when (= a ox)
                                          :do (incf iindex (* index (aref id-factors ax))))
                                    (setq remaining remainder)))
                        iindex))))
              (:encoded
               (let* ((fraction (floor +index-width+ irank))
                      (byte (loop :for w :in '(8 16 32 64)
                                  :when (< fraction w) :return (floor w 2))))
                 (the +function-type+
                      (lambda (i) +optimize-for-type+
                        (let ((iindex (the +index-type+ 0)))
                          (loop :for d :below irank :for ox :from 0
                                :do (let ((this-index (ldb (byte byte (* byte d)) i)))
                                      (loop :for a :in indices :for ax :from 0 :when (= a ox)
                                            :do (setf iindex (dpb this-index (byte byte (* byte ax))
                                                                  iindex)))))
                          iindex))))))))))

;; (defmacro intraverser (symbols &body body)
;;   (let ((widths '(16 32 64))
;;         (varray (getf symbols :varray))
;;         (factors (getf symbols :factors))
;;         (dimension (getf symbols :dimension))
;;         (this-index (getf symbols :this-index))
;;         (i (gensym)) (f (gensym)) (remainder (gensym))
;;         (index (gensym)) (remaining (gensym)))
;;     (flet ((wrap (form)
;;              `(the (function ((unsigned-byte ,size)) (unsigned-byte ,size))
;;                    ,form))
;;            (int-clause (size)
;;              `(the (function ((unsigned-byte ,size)) (unsigned-byte ,size))
;;                    (lambda (,index)
;;                      (let ((,remaining ,index))
;;                        (loop :for ,f :in ,factors :for ,i :from 0
;;                              ,@(if dimension `(:for ,dimension :in (shape-of ,varray)))
;;                              :do (multiple-value-bind (,this-index ,remainder) (floor ,remaining ,f)
;;                                    ,@body (setf ,remaining ,remainder)))))))
;;            (encode-clause (size)
;;              `(the (function ((unsigned-byte ,size)) (unsigned-byte ,size))
;;                    (lambda (,index)
;;                      (let* ((div (rank-of ,varray))
;;                             (byte (/ ,size div)))
;;                        (loop :for ,i :below div
;;                              :do (let ((,this-index (ldb (byte byte (* byte ,i)) ,index)))
;;                                    ,@body)))))))
;;       `(list t (lambda (,index)
;;                  (let ((,remaining ,index))
;;                    (loop :for ,f :in ,factors :for ,i :from 0
;;                          ,@(if dimension `(:for ,dimension :in (shape-of ,varray)))
;;                          :do (multiple-value-bind (,this-index ,remainder) (floor ,remaining ,f)
;;                                ,@body (setf ,remaining ,remainder)))))
;;              ,@(loop :for b :in widths :append (list (intern (format nil "I~a" b) "KEYWORD")
;;                                                      (int-clause b)))
;;              ,@(loop :for b :in widths :append (list (intern (format nil "E~a" b) "KEYWORD")
;;                                                      (encode-clause b)))))))
