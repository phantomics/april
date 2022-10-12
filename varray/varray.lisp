;;;; varray.lisp

(in-package #:varray)

(defclass varray ()
  ((%shape :accessor varray-shape
           :initform nil
           :initarg :shape
           :documentation "The array's shape - typically populated by a (shape-of) method.")
   (%indexer :accessor varray-indexer
             :initform nil
             :initarg :indexer
             :documentation "The array's indexer - typically populated by an (generator-of) method.")
   (%prototype :accessor varray-prototype
               :initform nil
               :initarg :prototype
               :documentation "The array's prototype - typically populated by a (prototype-of) method.")
   (%meta :accessor varray-meta
          :initform nil
          :initarg :meta
          :documentation "Metadata for the array."))
  (:metaclass va-class)
  (:documentation "Virtual array - the ancestor class for all virtual array objects."))

(defun varrayp (item)
  (typep item 'varray))

(defun subrendering-p (item)
  (and (or (typep item 'varray-derived)
           (typep item 'vad-subrendering))
       (vads-subrendering item)))

(defgeneric etype-of (varray)
  (:documentation "Get the element type of an array."))

(defgeneric prototype-of (varray)
  (:documentation "Get the prototype of an array."))

(defgeneric shape-of (varray)
  (:documentation "Get the shape of an array."))

(defgeneric size-of (varray)
  (:documentation "Get the size of an array."))

(defgeneric rank-of (varray)
  (:documentation "Get the rank of an array."))

(defgeneric indexer-of (varray &optional params)
  (:documentation "Get an indexing function for an array."))

(defgeneric base-indexer-of (varray &optional params)
  (:documentation "Get an indexing function for a virtual array's base."))

(defgeneric sub-indexer-of (varray)
  (:documentation "Get a sub-indexing function for an array."))

(defgeneric generator-of (varray &optional indexers params)
  (:documentation "Get a generating function for an array."))

(defgeneric metadata-of (varray)
  (:documentation "Get metadata of a virtual array."))

(defgeneric render (varray &rest params)
  (:documentation "Render an array into memory."))

(defun get-dimensional-factors (dimensions &optional as-vector)
  "Get the set of dimensional factors corresponding to a set of array dimensions."
  (let ((factor) (last-index))
    (if as-vector
        (let* ((rank (length dimensions))
               (output (make-array rank :element-type
                                   ;; (list 'integer 0 (reduce #'* (rest dimensions)))
                                   '(unsigned-byte 32)
                                   )))
          (loop :for d :in (reverse dimensions) :for dx :from 0
                :do (setf factor (setf (aref output (- rank dx 1))
                                       (if (zerop dx) 1 (* factor last-index)))
                          last-index d))
          output)
        (reverse (loop :for d :in (reverse dimensions) :for dx :from 0
                       :collect (setq factor (if (zerop dx) 1 (* factor last-index)))
                       :do (setq last-index d))))))

(defun apl-array-prototype2 (array)
  "Returns the default element for an array based on that array's first element (its prototype in array programming terms); blank spaces in the case of a character prototype and zeroes for others."
  (labels ((derive-element (input)
             (if (characterp input)
                 #\  (if (not (arrayp input))
                         (if (varrayp input)
                             (prototype-of input)
                             (let ((itype (type-of input)))
                               ;; in ECL (and others?), the integer type of a scalar is that number alone,
                               ;; i.e. (integer 2 2) for 2, so make sure the integer range starts with 0
                               (if (eql 'null itype)
                                   'null (coerce 0 (if (eql 'ratio itype) 'integer
                                                       (if (and (listp itype) (eql 'integer (first itype)))
                                                           (list 'integer (min 0 (second itype))
                                                                 (max 0 (or (third itype) 0)))
                                                           (if (typep input 'number) itype 'number)))))))
                         (if (zerop (array-total-size input))
                             (make-array (array-dimensions input))
                             (derive-element (row-major-aref input 0)))))))
    (if (not (arrayp array))
        (if (varrayp array) (prototype-of array)
            (derive-element array))
        (if (zerop (array-total-size array))
            (if (eql 'character (array-element-type array))
                #\  (coerce 0 (array-element-type array)))
            (let ((first-element (row-major-aref array 0)))
              (if (not (arrayp first-element))
                  (derive-element first-element)
                  (funcall (if (< 0 (array-rank first-element))
                               #'identity (lambda (item) (make-array nil :initial-element item)))
                           (let ((first-element (if (< 0 (array-rank first-element))
                                                    first-element (aref first-element))))
                             (if (and (arrayp first-element)
                                      (zerop (array-total-size first-element)))
                                 first-element
                                 (make-array (array-dimensions first-element)
                                             :element-type (array-element-type first-element)
                                             :initial-element (derive-element first-element)))))))))))

(defmethod prototype-of ((item t))
  "The prototype representation of an item is returned by the (apl-array-prototype) function."
  (if (listp item) ;; lists, used to implement things like namespaces, have a nil prototype
      nil (if (and (arrayp item)
                 (array-displacement item)
                 (vectorp (array-displacement item))
                 (listp (aref (array-displacement item) 0))
                 (member :empty-array-prototype (aref (array-displacement item) 0)))
            ;; if an empty array prototype has been stored, retrieve it
            (getf (aref (array-displacement item) 0) :empty-array-prototype)
            (if (and (arrayp item) (zerop (array-rank item)))
                (aplesque:make-empty-array (disclose item))
                (apl-array-prototype2 item)))))

(defmethod prototype-of ((varray varray))
  "The default prototype for a virtual array is 0."
  0)

(defmethod etype-of ((item t))
  "A literal array's element type is returned by the (array-element-type) function."
  (assign-element-type item))

(defmethod etype-of ((array array))
  "A literal array's element type is returned by the (array-element-type) function."
  (array-element-type array))

(defmethod etype-of ((varray varray))
  "The default element type for a virtual array is T."
  't)

(defmethod shape-of ((_ t))
  "Non-arrays have a nil shape."
  (declare (ignore _))
  nil)

(defmethod shape-of ((array array))
  "Literal array shapes are given by (array-dimensions)."
  (array-dimensions array))

(defmethod shape-of ((varray varray))
  "Virtual array shapes are referenced using the (varray-shape) method."
  (varray-shape varray))
  
(defmethod size-of ((item t))
  "Virtual array shapes are referenced using the (varray-shape) method."
  (if (not (arrayp item))
      1 (array-total-size item)))

(defmethod size-of ((varray varray))
  "Virtual array shapes are referenced using the (varray-shape) method."
  (reduce #'* (shape-of varray)))

(defmethod rank-of ((item t))
  "Non-arrays have a rank of 0."
  (declare (ignore item))
  0)

(defmethod rank-of ((array array))
  "Literal array ranks are given by (array-rank)."
  (array-rank array))

(defmethod rank-of ((varray varray))
  "A virtual array's rank is the length of its shape."
  (length (shape-of varray)))

(defmethod indexer-of ((item t) &optional params)
  "The indexer for a non-array is its identity."
  item)

(defmethod indexer-of ((array array) &optional params)
  (if (= 0 (array-rank array))
      ;; array
      ;; TODO: this causes tree printing test to fail
      (lambda (index)
        (when (< index (array-total-size array))
          (row-major-aref array index)))
      (if (= 0 (array-total-size array))
          (prototype-of array)
          ;; TODO: why does wrapping this in a (lambda) cause problems? like array-lib's (0↑⊂,⊂⍬) from 99
          (lambda (index)
            (when (< index (array-total-size array))
              (row-major-aref array index))))))

(defun join-indexers (indexers type)
  (if (not indexers)
      #'identity
      (let ((ctype) ;; the type in common
            (reversed-indexers)
            (defaulting))
        ;; (loop :for i :in indexers :while (not (eq t ctype))
        ;;       :do (if (listp i)
        ;;               (if type (when (not (getf i ctype)) (setf ctype t))
        ;;                   (if (getf i type) (setf ctype type)
        ;;                       (setf ctype t)))
        ;;               (setf ctype t)))
        (loop :for i :in indexers :while (not defaulting)
              :do (if (or (not (listp i)) (not (first i)))
                      (setf defaulting t)
                      (push (first i) reversed-indexers)))
        (when defaulting
          (setf reversed-indexers nil)
          (loop :for i :in indexers :do (push (if (not (listp i)) i (second i))
                                              reversed-indexers)))
        ;; (print (list :re indexers reversed-indexers defaulting type))
        (values (lambda (index)
                  (let ((index-out index))
                    (loop :for i :in reversed-indexers :do (setf index-out (funcall i index-out)))
                    index-out))
                (when (not defaulting) type)))))

(defmethod generator-of ((item t) &optional indexers params)
  (declare (ignore indexers params))
  ;; (lambda (index) (declare (ignore index)) item)
  item)

(defmethod generator-of ((array array) &optional indexers params)
  (multiple-value-bind (composite-indexer is-not-defaulting)
      (join-indexers indexers (getf params :indexer-key))
    ;; (print (list :pa composite-indexer :isd is-defaulting params))
    (values
     (let ((array-size (array-total-size array)))
       (if (and is-not-defaulting (getf params :indexer-key)
                (member (getf params :indexer-key) '(:e8 :e16 :e32 :e64)))
           (let* ((factors (get-dimensional-factors (shape-of array) t))
                  (encoded-type (intern (format nil "I~a" (getf params :encoding)) "KEYWORD"))
                  (converter ;; (fetch-index-generator (getf params :indexer-key) (rank-of array))
                    ;; (encode-rmi)
                    ;; (decode-rmi encoded-type factors (getf params :index-width))
                    (decode-rmi (getf params :encoding) (getf params :index-width)
                                (array-rank array) factors)
                    ))
             ;; (print (list :con converter factors (format nil "I~a" (getf params :encoding))))
             (lambda (index)
               ;; (print (list :ee index))
               (let ((index-out (funcall converter (funcall composite-indexer index))))
                 (when (< index-out array-size) (row-major-aref array index-out)))))
           (lambda (index)
             (let ((index-out (funcall composite-indexer index)))
               (when (< index-out array-size) (row-major-aref array index-out))))))
     is-not-defaulting)))

(defmethod metadata-of ((item t))
  (declare (ignore item))
  nil)

(defmethod metadata-of ((varray varray))
  (varray-meta varray))

(defmethod render ((item t) &rest params)
  "Rendering a non-virtual array object simply returns the object."
  item)

(defun subrendering-base (item)
  (when (typep item 'varray-derived)
    (or (subrendering-p (vader-base item))
        (subrendering-base (vader-base item)))))

(defun sub-byte-element-size (array)
  "Return the element size in bits if the argument is an array whose elements are integers smaller than 7 bits."
  (and (arrayp array)
       (let ((type (array-element-type array)))
         (or (and (eql 'bit type) 1)
             #+clasp (case type (ext:byte2 2)
                           (ext:integer2 2) (ext:byte4 4) (ext:integer4 4))
             #+(not clasp) (and (listp type)
                                (eql 'unsigned-byte (first type))
                                (> 7 (second type))
                                (second type))))))

(defmethod generator-of ((varray varray) &optional indexers params)
  (let ((composite-indexer (join-indexers indexers t))
        (this-indexer (indexer-of varray)))
    ;; (print (list :pg params))
    (if (not (functionp this-indexer))
        this-indexer (lambda (index)
                       (funcall this-indexer (funcall composite-indexer index))))))

(defmethod generator-of :around ((varray varray) &optional indexers params)
  (if (typep varray 'vad-reindexing) (call-next-method)
      (let ((this-generator (call-next-method)))
        ;; (print (list :par params varray))
        (if (not (functionp this-generator))
            this-generator
            (multiple-value-bind (composite-indexer is-not-defaulting)
                (join-indexers indexers t)
              ;; (print (list :ind is-not-defaulting))
              (values (lambda (index)
                        (let ((index-out index))
                          (funcall this-generator (funcall composite-indexer index))))
                      is-not-defaulting))))))

;; (macroexpand `(varray::intraverser (:varray varray :factors factors :dimension dimension :this-index this-index) (do-stuff)))

;; (intraverser (f i)
;;   (when (= i )))

;; (defmethod render ((varray varray) &rest params)
;;   ;; (declare (optimize (speed 3)))
;;   (let ((output-shape (shape-of varray))
;;         (prototype (let ((proto (prototype-of varray)))
;;                      ;; a null-type prototype is nil
;;                      (when (not (eql 'null proto)) proto)))
;;         ;; (indexer (i2ndexer-of varray))
;;         (indexer (generator-of varray))
;;         (to-subrender (or (subrendering-p varray)
;;                           (subrendering-base varray))))
;;     ;; (when (typep varray 'vader-identity) (print :ef))
;;     ;; (print (list :vv varray prototype
;;     ;;              (etype-of varray)
;;     ;;              (if (typep varray 'varray-derived)
;;     ;;                  (subrendering-p (vader-base varray)))))
;;     (if output-shape
;;         (if (zerop (the (unsigned-byte 62) (reduce #'* output-shape)))
;;             (let ((out-meta (when (arrayp prototype)
;;                                (make-array 1 :initial-contents
;;                                            (list (list :empty-array-prototype
;;                                                        (prototype-of varray)))))))
;;               ;; a nil element type results in a t-type array;
;;               ;; nil types may occur from things like +/⍬
;;               (if out-meta (make-array (shape-of varray) :displaced-to out-meta)
;;                   (make-array (shape-of varray) :element-type (or (etype-of varray) t))))
;;             (let* ((output (make-array (shape-of varray) :element-type (etype-of varray)))
;;                    (render-index
;;                      (if to-subrender
;;                          (lambda (i)
;;                            (declare (type (unsigned-byte 62) i))
;;                            ;; (print (list :ix indexer i varray))
;;                            (let ((indexed (if (not (functionp indexer))
;;                                               indexer (funcall indexer i))))
;;                              (setf (row-major-aref output i)
;;                                    (if indexed (render indexed) prototype))))
;;                          (lambda (i)
;;                            (declare (type (unsigned-byte 62) i))
;;                            ;; (print (list :ioi i varray))
;;                            (if (functionp indexer)
;;                                (setf (row-major-aref output i)
;;                                      (or (funcall indexer i) prototype))
;;                                (setf (row-major-aref output i)
;;                                      (or indexer prototype))))))
;;                    (sbsize (sub-byte-element-size output))
;;                    (sbesize (if sbsize (/ 64 sbsize) 1))
;;                    (wcadj *workers-count*)
;;                    (divisions (min wcadj (ceiling (/ (size-of varray) sbesize))))
;;                    (total-size (size-of varray))
;;                    (interval (/ total-size sbesize *workers-count*))
;;                    (lpchannel (lparallel::make-channel))
;;                    (process (lambda (index)
;;                               (lambda ()
;;                                 (let* ((start-intervals (ceiling (* interval index)))
;;                                        (start-at (* sbesize start-intervals))
;;                                        (count (if (< index (1- divisions))
;;                                                   (* sbesize (- (ceiling (* interval (1+ index)))
;;                                                                 start-intervals))
;;                                                   (- total-size start-at))))
;;                                   (loop :for i :from start-at :to (1- (+ start-at count))
;;                                         :do (funcall render-index i))))))
;;                    ;; (total-size (size-of varray))
;;                    ;; (divisions (min total-size (max wcadj (ceiling (/ total-size sbesize)))))
;;                    ;; (interval sbesize) ; (/ total-size sbesize *workers-count*))
;;                    ;; (lpchannel (lparallel::make-channel))
;;                    ;; (process (lambda (index)
;;                    ;;            (lambda ()
;;                    ;;              (let* ((start-intervals (ceiling (* interval index)))
;;                    ;;                     (start-at (* sbesize index))
;;                    ;;                     (count (if (< index (1- divisions))
;;                    ;;                                sbesize (- total-size start-at))))
;;                    ;;                ;; (print (list :sa start-at count total-size))
;;                    ;;                ;; (error "true")
;;                    ;;                (loop :for i :from start-at :to (1- (+ start-at count))
;;                    ;;                      :do (funcall render-index i))))))
;;                    (threaded-count 0))
;;               ;; (print (list :sb sbsize (type-of varray) (etype-of varray) (type-of output)
;;               ;;              (vader-base varray)
;;               ;;              (when (typep varray 'vader-composing) (vacmp-omega varray))))
;;               ;; (print (list :ss interval sbsize sbesize :div divisions wcadj (size-of varray)))
;;               ;; (setf active-workers 0)
;;               ;; (print (list :out (type-of output) (type-of varray)
;;               ;;              divisions division-size sbesize sbsize
;;               ;;              (typep varray 'vader-composing)
;;               ;;              (when (typep varray 'vader-composing)
;;               ;;                (vacmp-threadable varray))))
;;               ;; (print (list :ts to-subrender (setf april::ggt varray)))
;;               (loop :for d :below divisions
;;                     :do (if ;; (or (and (typep varray 'vader-composing)
;;                             ;;           (not (vacmp-threadable varray)))
;;                             ;;      ;; don't thread when rendering the output of operators composed
;;                             ;;      ;; with side-affecting functions as for {⎕RL←5 1 ⋄ 10?⍵}¨10⍴1000
;;                             ;;      (loop :for worker :across (lparallel.kernel::workers lparallel::*kernel*)
;;                             ;;            :never (null (lparallel.kernel::running-category worker))))
;;                          t
;;                          ;; (typep varray 'vacomp-each)
;;                          ;; (lparallel:kernel-worker-index)
;;                             (funcall (funcall process d))
;;                             (progn (incf threaded-count)
;;                                    (lparallel::submit-task lpchannel (funcall process d)))))
;;               (loop :repeat threaded-count
;;                 :do (lparallel::receive-result lpchannel))
;;               output))
;;         (funcall (if (subrendering-p varray)
;;                      (lambda (item)
;;                        (let ((rendered (render item)))
;;                          ;; (print (list :rr rendered item varray
;;                          ;;              (subrendering-p varray)))
;;                          ;; (if (typep varray 'vacomp-reduce)
;;                          ;;     (push varray april::ggg))
;;                          (if (and (zerop (rank-of rendered))
;;                                   (or (not (arrayp rendered))
;;                                       ;; (print (subrendering-p varray))
;;                                       (and (typep varray 'vacomp-reduce)
;;                                            (subrendering-p varray))))
;;                              ;; handle the case of {,/⍵}/3⍴⊂⍳3
;;                              rendered (enclose rendered))))
;;                      (lambda (item)
;;                        (let ((rendered (apply #'render item params)))
;;                          ;; (print (list :ren rendered))
;;                          (if (or (not (shape-of rendered))
;;                                  (typep varray 'vader-mix) ;; put these in a superclass
;;                                  (typep varray 'vader-pick))
;;                              rendered (enclose rendered)))))
;;                  (if (not (functionp indexer))
;;                      indexer (funcall indexer 0))))))

;; (defmacro in-forms (symbols &body body)
;;   (let ((widths '(8 16 32 64))
;;         (i (getf symbols :sub-index))
;;         (interval (getf symbols :interval)))
;;     (let ((index (gensym)) (start-intervals (gensym))
;;           (start-at (gensym)) (count (gensym)))
;;       (flet ((int-clause (size)
;;                `(the (function (ava-worker-count))
;;                      (lambda (,index)
;;                        (declare (optimize (speed 3) (safety 0)))
;;                        (lambda ()
;;                          (let* ((,start-intervals (the (unsigned-byte ,size)
;;                                                        (ceiling (* ,interval ,index))))
;;                                 (,start-at (the (unsigned-byte ,size)
;;                                                 (* sbesize ,start-intervals)))
;;                                 (,count (the (unsigned-byte ,size)
;;                                              (if (< ,index (1- divisions))
;;                                                  (* sbesize (- (ceiling (* ,interval (1+ ,index)))
;;                                                                ,start-intervals))
;;                                                  (- total-size ,start-at)))))
;;                            (loop :for ,i :of-type (unsigned-byte ,size)
;;                                  :from ,start-at :to (1- (+ ,start-at ,count))
;;                                  :do ,@body)))))))
;;         `(list t (the (function (ava-worker-count))
;;                       (lambda (,index)
;;                         (lambda ()
;;                           (let* ((,start-intervals (ceiling (* ,interval ,index)))
;;                                  (,start-at (* sbesize ,start-intervals))
;;                                  (,count (if (< ,index (1- divisions))
;;                                              (* sbesize (- (ceiling (* ,interval (1+ ,index)))
;;                                                            ,start-intervals))
;;                                              (- total-size ,start-at))))
;;                             (loop :for ,i :from ,start-at :to (1- (+ ,start-at ,count))
;;                                   :do ,@body)))))
;;                ,@(loop :for b :in widths :append (list (intern (format nil "I~a" b) "KEYWORD")
;;                                                        (int-clause b)))
;;                ;; ,@(loop :for b :in widths :append (list (intern (format nil "E~a" b) "KEYWORD")
;;                ;;                                         (encode-clause b)))
;;                )))))

(defun encode-rmi (typekey factors byte-size)
  (intraverser (:typekey typekey :linear t)
    (:integer (the (function ((unsigned-byte +index-width+))
                             (unsigned-byte +index-width+))
                   (let ((this-rank (length factors)))
                     (lambda (index)
                       (let ((remaining index)
                             (output (the +index-type+ 0)))
                         (loop :for f :across factors :for ix :from (1- this-rank) :downto 0
                               :do (multiple-value-bind (factor remainder)
                                       (floor remaining f)
                                     (setf output (dpb factor (byte byte-size (* byte-size ix))
                                                       output)
                                           remaining remainder)))
                         ;; (print (list :ind index output))
                         output)))))))

;; (format t "#x~4,'0X~%" (funcall (encode-rmi :i32 #(12 4 1) 8) 14))

;; (defun decode-rmi (typekey factors byte-size)
;;   (let ((this-rank (length factors)))
;;     ;; (print (list :ff (type-of factors) typekey))
;;     (intraverser (:typekey typekey :linear t)
;;       (:integer (the (function ((unsigned-byte +index-width+))
;;                                (unsigned-byte +index-width+))
;;                      (lambda (index) +optimize-for-type+
;;                        (declare (type +index-type+ index))
;;                        (let ((output (the +index-type+ 0)))
;;                          (loop :for fx :of-type +index-type+ :from this-rank :downto 1
;;                                :for ix :of-type +index-type+ :from 0
;;                                :do (incf output (* (the +index-type+ (aref factors ix))
;;                                                    (ldb (byte byte-size (* byte-size (1- fx))) index))))
;;                          ;; (print (list :dec index output factors))
;;                          output)))))))

(let ((function-table
        (intraverser-ex
         (((:encoded) (:eindex-width +eindex-width+ :cindex-width +cindex-width+
                       :rank-width +rank-width+ :sub-base-width +sub-base-width+
                       :rank-plus +rank-plus+))
          (the (function ((simple-array (unsigned-byte 32) (+rank-plus+)))
                         function)
               (lambda (factors)
                 (declare (optimize (speed 3) (safety 0))
                          (type (simple-array (unsigned-byte 32) (+rank-plus+)) factors))
                 (the (function ((unsigned-byte +eindex-width+))
                                (unsigned-byte +eindex-width+))
                      (lambda (index)
                        (declare (type (unsigned-byte +eindex-width+) index))
                        (let ((output (the (unsigned-byte +eindex-width+) 0)))
                          (loop :for fx :of-type (unsigned-byte +rank-width+)
                                  := (1- +rank-plus+) :then (1- fx)
                                :for ix :of-type (unsigned-byte 32) :across factors
                                :do (incf (the (unsigned-byte +eindex-width+) output)
                                          (* (the (unsigned-byte +eindex-width+) ix)
                                             (the (unsigned-byte +cindex-width+)
                                                  (ldb (byte +cindex-width+
                                                             (* +cindex-width+ fx))
                                                       index)))))
                          (the (unsigned-byte +eindex-width+) output))))))))))
  (defun decode-rmi (width element-width rank factors)
    (let ((match (gethash (list '(:encoded) (list width element-width rank))
                          function-table)))
      (values (when match (funcall match factors))
              (lambda (index)
                (let ((output 0))
                  (loop :for fx :from rank :downto 0
                        :for ix :across factors
                        :do (incf output (* ix (ldb (byte element-width
                                                          (* element-width fx))
                                                    index))))
                  output))))))

;; (the (function ((simple-array (unsigned-byte 32) (2))) function)
;;                (lambda (factors)
;;                  (declare (optimize (speed 3) (safety 0))
;;                           (type (simple-array (unsigned-byte 32) (2)) factors))
;;                  (the (function ((unsigned-byte 64)) (unsigned-byte 64))
;;                       (lambda (index)
;;                         (declare (type (unsigned-byte 64) index))
;;                         (let ((output (the (unsigned-byte 64) 0)))
;;                           (loop :for fx :of-type (unsigned-byte 4) := (1-
;;                                                                        2) :then (1-
;;                                                                                  fx)
;;                                 :for ix :of-type (unsigned-byte
;;                                                   32) :across factors
;;                                 :do (incf (the (unsigned-byte 64) output)
;;                                           (* (the (unsigned-byte 64) ix)
;;                                              (the (unsigned-byte 32)
;;                                                   (ldb (byte 32 (* 32 fx))
;;                                                        index)))))
;;                           (the (unsigned-byte 64) output))))))

;; (the (function ((unsigned-byte 63)) (unsigned-byte 64)) (lambda (index) (declare (optimize (speed 3) (safety 0)) (type (unsigned-byte 63) index)) (let ((output (1+ index)))  (the (unsigned-byte 64) output))))

;; (print (funcall (decode-rmi :i32 #(12 4 1) 8) #x20001))

;; (defun increment-encoded (typekey dimensions byte-size)
;;   (let* ((this-rank (length dimensions))
;;          (increments (make-array this-rank :element-type 'fixnum :initial-element 0)))
;;     (loop :for i :below this-rank :do (setf (aref increments i)
;;                                             (expt (expt 2 byte-size) i)))
;;     (intraverser (:typekey typekey :linear t)
;;       (:integer (the (function ((unsigned-byte +index-width+))
;;                                (unsigned-byte +index-width+))
;;                      (lambda (index)
;;                        (let ((output index) (complete))
;;                          (loop :for ix ;; :from (1- this-rank) :downto 0
;;                                :from 0 :below this-rank
;;                                :while (not complete)
;;                                :do (if (< (ldb (byte byte-size (* byte-size ix))
;;                                                index)
;;                                           (1- (aref dimensions ix)))
;;                                        (setf complete (incf output (aref increments ix)))
;;                                        (setf output (dpb 0 (byte byte-size (* byte-size ix))
;;                                                          output))))
                         
;;                          (print (list :in (format nil "#x~4,'0X" index)
;;                                       (format nil "#x~4,'0X" output)))
;;                          output)))))))

(let* (( 8-bit-factors (make-array 8 :element-type '(unsigned-byte 64)))
       (16-bit-factors (make-array 4 :element-type '(unsigned-byte 64)))
       (32-bit-factors (make-array 2 :element-type '(unsigned-byte 64)))
       (function-table
         (intraverser-ex
          (((:encoded) (:eindex-width +eindex-width+ :cindex-width +cindex-width+
                        :rank-width +rank-width+ :sub-base-width +sub-base-width+
                        :rank-plus +rank+))
           (the (function ((simple-array (unsigned-byte +cindex-width+) (+rank+)))
                          function)
                (lambda (dimensions)
                  (declare (optimize (speed 3) (safety 0))
                           (type (simple-array (unsigned-byte +cindex-width+) (+rank+)) dimensions))
                  (let ((factors (case +cindex-width+
                                   (8  (the (simple-array (unsigned-byte 64) (8))  8-bit-factors))
                                   (16 (the (simple-array (unsigned-byte 64) (4)) 16-bit-factors))
                                   (32 (the (simple-array (unsigned-byte 64) (2)) 32-bit-factors)))))
                    (the (function ((unsigned-byte +eindex-width+))
                                   (unsigned-byte +eindex-width+))
                         (lambda (index)
                           (declare (type (unsigned-byte +eindex-width+) index))
                           (let ((output index) (complete (the (unsigned-byte +rank-width+) 0)))
                             (declare (type (unsigned-byte +eindex-width+) output))
                             (loop :for ix ;; :from (1- this-rank) :downto 0
                                     :of-type (unsigned-byte +rank-width+)
                                   :from 0 :below +rank+
                                   :for dim :of-type (unsigned-byte +cindex-width+) :across dimensions
                                   :for fac :of-type (unsigned-byte 64) :across factors
                                   :while (zerop complete)
                                   :do (if (< (the (unsigned-byte +cindex-width+)
                                                   (ldb (byte +cindex-width+ (* +cindex-width+ ix))
                                                        index))
                                              (1- dim))
                                           (incf (the (unsigned-byte +rank-width+) complete)
                                                 (the bit (signum (incf output fac))))
                                           (setf output (dpb 0 (byte +cindex-width+ (* +cindex-width+ ix))
                                                             output))))
                             ;; (print (list :in (format nil "#x~4,'0X" index)
                             ;;              (format nil "#x~4,'0X" output)))
                             (the (unsigned-byte +eindex-width+) output)))))))))))
  (loop :for i :below 8 :do (setf (aref  8-bit-factors i) (expt 256 i)))
  (loop :for i :below 4 :do (setf (aref 16-bit-factors i) (expt 65536 i)))
  (loop :for i :below 2 :do (setf (aref 32-bit-factors i) (expt 4294967296 i)))
  
  (defun increment-encoded (width element-width dimensions)
    (let* ((rank (length dimensions))
           (match (gethash (list '(:encoded) (list width element-width rank))
                           function-table)))
      (values (when match (funcall match dimensions))
              (let ((factors (case element-width (8 8-bit-factors)
                               (16 16-bit-factors) (32 32-bit-factors))))
                (lambda (index)
                  (let ((output index) (complete))
                    (loop :for ix ;; :from (1- this-rank) :downto 0
                          :from 0 :below rank
                          :while (not complete)
                          :do (if (< (ldb (byte element-width (* element-width ix))
                                          index)
                                     (1- (aref dimensions ix)))
                                  (setf complete (incf output (aref factors ix)))
                                  (setf output (dpb 0 (byte element-width (* element-width ix))
                                                    output))))
                    output)))))))

;; (format t "#x~4,'0X" (funcall (increment-encoded :i32 #(2 3 4) 8) #x20001))
;; (format t "#x~4,'0X" (funcall (increment-encoded 32 8 (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(2 3 4))) #x20001))

(defun get-indexing-function (typekey factors shape-vector sbesize interval divisions
                              total-size index-type encoding-type to-call)
  (let* ((ekey (intern (format nil "I~a" encoding-type) "KEYWORD"))
         (encoder (when encoding-type (encode-rmi ekey factors index-type)))
         ;; (incrementer (when encoding-type (increment-encoded ekey shape-vector index-type)))
         (incrementer (when encoding-type (increment-encoded encoding-type index-type
                                                             shape-vector)))
         )
    (intraverser (:typekey typekey)
      (:integer
       (the +root-function-type+
            (lambda (index)
              ;; +optimize-for-type+
              (the (function nil)
                   (lambda ()
                     (let* ((start-intervals (the +index-type+ (ceiling (* interval index))))
                            (start-at (the +index-type+ (* sbesize start-intervals)))
                            (count (the +index-type+
                                        (if (< index (1- divisions))
                                            (* sbesize (- (ceiling (* interval (1+ index)))
                                                          start-intervals))
                                            (- total-size start-at)))))
                       (loop :for i ; :of-type +index-type+
                             :from start-at :to (1- (+ start-at count))
                             :do (funcall to-call i))))))))
      (:encoded
       (the +root-function-type+
            (lambda (index)
              (the (function nil)
                   (lambda ()
                     (let* ((start-intervals (the +index-type+ (ceiling (* interval index))))
                            (start-at (the +index-type+ (* sbesize start-intervals)))
                            (count (the +index-type+
                                        (if (< index (1- divisions))
                                            (* sbesize (- (ceiling (* interval (1+ index)))
                                                          start-intervals))
                                            (- total-size start-at))))
                            (coords (funcall encoder start-at)))
                       (loop :for i :below count
                             :do (funcall to-call coords)
                                 (when (< i (1- count))
                                   (setf coords (funcall incrementer coords)))))))))))))

(defmethod render ((varray varray) &rest params)
  ;; (declare (optimize (speed 3)))
  (let* ((output-shape (shape-of varray))
         (output-rank (length output-shape))
         (metadata (metadata-of varray))
         (indexer) ; (generator-of varray nil (list :meta metadata)))
         ;; (encodable-index t)
         (index-type (or (when (getf metadata :max-size)
                           (loop :for w :in '(8 16 32 64) :when (< (getf metadata :max-size)
                                                                   (expt 2 w))
                                 :return w))
                         t))
         (d-index-type (when (and (> output-rank 0)
                                  (not (eq t index-type)))
                         (let ((fraction (floor index-type output-rank)))
                           (loop :for w :in '(8 16 32 64)
                                 :when (< (getf metadata :max-dim)
                                          (expt 2 w))
                                   :return w))))
         (encoding (when d-index-type
                     ;; encoded integer size that can hold the encoded dimensions,
                     ;; ranging from 8 to 64 bits; for example, a 32-bit integer could hold
                     ;; 4x8 or 2x16-bit dimension indices and a 64-bit integer could hold 8x8,
                     ;; 4x16 or 2x32 dimension indices
                     (let ((fraction (floor index-type output-rank)))
                       (loop :for w :in '(8 16 32 64)
                             :when (>= w (* output-rank d-index-type))
                               :return w))))
         (type-key (intern (format nil "~a~a" (cond (encoding "E")
                                                    (t "I"))
                                   (or encoding index-type))
                           "KEYWORD"))
         (default-generator) (to-subrender))

    (when (getf (varray-meta varray) :gen-meta)
      (setf (getf (rest (getf (varray-meta varray) :gen-meta)) :index-type) d-index-type
            (getf (rest (getf (varray-meta varray) :gen-meta)) :indexer-key) type-key
            (getf (rest (getf (varray-meta varray) :gen-meta)) :index-width) encoding))

    (setf (getf metadata :index-width) index-type
          (getf metadata :indexer-key) type-key)

    (multiple-value-bind (this-generator is-not-defaulting)
        (generator-of varray nil ;; (list :meta metadata)
                      (list :indexer-key type-key
                            :index-width d-index-type
                            :encoding encoding))
      ;; (print (list :dg is-not-defaulting this-generator varray))
      (setf indexer this-generator
            default-generator (not is-not-defaulting)))

    ;; (print (list :dde d-index-type encoding output-rank index-type metadata type-key
    ;;              default-generator varray))
    
    (when (and (typep varray 'vader-select)
               (< 0 (size-of varray)) (functionp indexer))
      (funcall indexer 0))
    ;; IPV-TODO: HACK to handle select subrendering, which is only set when the
    ;; first element is generated - figure out a better way to do this
    (setf to-subrender (or (subrendering-p varray)
                           (subrendering-base varray)))

    ;; (print (list :me (varray-meta varray) index-type type-key))
    ;; (print (list :sr varray (subrendering-p varray)
    ;;              (subrendering-base varray)))
    ;; (print (list :vv varray prototype
    ;;              (etype-of varray)
    ;;              (if (typep varray 'varray-derived)
    ;;                  (subrendering-p (vader-base varray)))))
    ;; (print (list :va varray))
    (if output-shape
        (if (zerop (the (unsigned-byte 62) (reduce #'* output-shape)))
            (let* ((prototype (prototype-of varray))
                   (out-meta (when (arrayp prototype)
                               (make-array 1 :initial-contents
                                           (list (list :empty-array-prototype prototype))))))
              ;; a nil element type results in a t-type array;
              ;; nil types may occur from things like +/⍬
              (if out-meta (make-array output-shape :displaced-to out-meta)
                  (make-array output-shape :element-type (or (etype-of varray) t))))
            ;; (if (not (functionp indexer))
            ;;     (make-array output-shape :element-type (etype-of varray)
            ;;                                   :initial-element indexer))
            (let* ((output (make-array output-shape :element-type (etype-of varray)))
                   (dfactors (when encoding (get-dimensional-factors output-shape t)))
                   ;; the decoder function converts non-row-major index formats like
                   ;; sub-byte-encoded coordinate vectors back to row-major indices
                   ;; to reference elements in the output array
                   (decoder (if (or default-generator (not encoding)) ;; TOGGLE
                                #'identity ;; (decode-rmi (intern (format nil "I~a" encoding)
                                           ;;                     "KEYWORD")
                                           ;;             dfactors d-index-type)
                                (decode-rmi encoding d-index-type output-rank dfactors)
                                ))
                   ;; (de (print (list :dc d-index-type dfactors decoder)))
                   (render-index
                     (if to-subrender
                         (lambda (i)
                           (let ((indexed (if (not (functionp indexer))
                                              indexer (funcall indexer i))))
                             ;; (print (list :ii i))
                             (setf (row-major-aref output (funcall decoder i))
                                   (render indexed))))
                         (lambda (i)
                           ;; (print (list :i2 i))
                           ;; (print (list :in (format nil "#x~6,'0X" i)))
                           (if (functionp indexer)
                               (setf (row-major-aref output (funcall decoder i))
                                     (funcall indexer i))
                               (setf (row-major-aref output (funcall decoder i))
                                     indexer)))))
                   (sbsize (sub-byte-element-size output))
                   (sbesize (if sbsize (/ 64 sbsize) 1))
                   (wcadj *workers-count*)
                   (divisions (min wcadj (ceiling (/ (size-of varray) sbesize))))
                   (total-size (size-of varray))
                   (interval (/ total-size sbesize *workers-count*))
                   (lpchannel (lparallel::make-channel))
                   ;; (processes (in-forms (:sub-index i :interval interval)
                   ;;              (funcall render-index i)))
                   ;; (process (or (getf processes type-key)
                   ;;              (getf processes t)))
                   (shape-vector (when (and d-index-type output-rank)
                                   (make-array output-rank :initial-contents (reverse output-shape)
                                                           :element-type (list 'unsigned-byte
                                                                               d-index-type))))
                   (process-pair (get-indexing-function
                                  type-key dfactors shape-vector ; (coerce output-shape 'vector)
                                  sbesize interval divisions total-size d-index-type
                                  encoding render-index))
                   ;; (eep (print (list :prop process-pair)))
                   (process (or (and (not default-generator) ;; TOGGLE
                                     (first process-pair))
                                (second process-pair)))
                   ;; (process (lambda (index)
                   ;;            (lambda ()
                   ;;              (let* ((start-intervals (ceiling (* interval index)))
                   ;;                     (start-at (* sbesize start-intervals))
                   ;;                     (count (if (< index (1- divisions))
                   ;;                                (* sbesize (- (ceiling (* interval (1+ index)))
                   ;;                                              start-intervals))
                   ;;                                (- total-size start-at))))
                   ;;                (loop :for i :from start-at :to (1- (+ start-at count))
                   ;;                      :do (funcall render-index i))))))
                   (threaded-count 0))
              ;; (print (list :pro process-pair))
              ;; (print (list :sb sbsize (type-of varray) (etype-of varray) (type-of output)
              ;;              (vader-base varray)
              ;;              (when (typep varray 'vader-composing) (vacmp-omega varray))))
              ;; (print (list :ss interval sbsize sbesize :div divisions wcadj (size-of varray)))
              ;; (setf active-workers 0)
              ;; (print (list :out (type-of output) (type-of varray)
              ;;              divisions division-size sbesize sbsize
              ;;              (typep varray 'vader-composing)
              ;;              (when (typep varray 'vader-composing)
              ;;                (vacmp-threadable varray))))
              ;; (print (list :ts to-subrender (setf april::ggt varray)))
              (loop :for d :below divisions
                    :do (if ;; (or (and (typep varray 'vader-composing)
                            ;;           (not (vacmp-threadable varray)))
                            ;;      ;; don't thread when rendering the output of operators composed
                            ;;      ;; with side-affecting functions as for {⎕RL←5 1 ⋄ 10?⍵}¨10⍴1000
                            ;;      (loop :for worker :across (lparallel.kernel::workers lparallel::*kernel*)
                            ;;            :never (null (lparallel.kernel::running-category worker))))
                            t
                            (funcall (funcall process d))
                            (progn (incf threaded-count)
                                   (lparallel::submit-task
                                    lpchannel (funcall process d)))))
              (loop :repeat threaded-count
                :do (lparallel::receive-result lpchannel))
              output))
        (funcall (if (subrendering-p varray)
                     (lambda (item)
                       (let ((rendered (render item)))
                         ;; (print (list :rr rendered item varray
                         ;;              (subrendering-p varray)))
                         ;; (if (typep varray 'vacomp-reduce)
                         ;;     (push varray april::ggg))
                         (if (and (zerop (rank-of rendered))
                                  (or (not (arrayp rendered))
                                      ;; (print (subrendering-p varray))
                                      (and (typep varray 'vacomp-reduce)
                                           (subrendering-p varray))))
                             ;; handle the case of {,/⍵}/3⍴⊂⍳3
                             rendered (enclose rendered))))
                     (lambda (item)
                       (let ((rendered (apply #'render item params)))
                         (if (or (not (shape-of rendered))
                                 (typep varray 'vader-mix) ;; put these in a superclass
                                 (typep varray 'vader-pick))
                             rendered (enclose rendered)))))
                 (if (not (functionp indexer))
                     indexer (funcall indexer 0))))))

(defun segment-length (size section-count)
  "Create a vector of lengths and start points for segments of a vector to be processed in parallel."
  (let* ((section-count (min section-count size))
         (division-size (/ size section-count))
         (start-points (make-array section-count))
         (section-lengths (make-array section-count)))
    (dotimes (i section-count) (setf (aref start-points i) (floor (* i division-size))))
    (dotimes (i section-count) (setf (aref section-lengths i)
                                     (- (if (= i (1- section-count))
                                            size (aref start-points (1+ i)))
                                        (aref start-points i))))
    (values start-points section-lengths)))

;; (time (let ((vec (make-array '(300 300 300) :element-type '(unsigned-byte 8))))
;;         (declare (optimize (speed 3) (safety 0)))
;;         (xdotimes vec (i 27000000) (setf (row-major-aref vec i) (mod i 100))) :done))

(defmacro get-promised (object form)
  `(if ,object (force ,object)
       (progn (setf ,object (promise))
              (fulfill ,object ,form)
              (force ,object))))

(defmacro get-or-assign-shape (object form)
  `(or (varray-shape ,object) (setf (varray-shape ,object) ,form)))

(defclass vad-subrendering ()
  ((%subrendering :accessor vads-subrendering
                  :initform t
                  :initarg :subrendering
                  :documentation "Whether the array contains nested elements to be subrendered."))
  (:metaclass va-class)
  (:documentation "Superclass of derived arrays containing sub-arrays to be rendered."))

(defclass vad-with-io ()
  ((%index-origin :accessor vads-io
                  :initform 0
                  :initarg :index-origin
                  :documentation "Parameter specifying the index origin for an array operation."))
  (:metaclass va-class)
  (:documentation "Superclass of array transformations taking index origin as an implicit argument."))

(defclass vad-with-dfactors ()
  ((%dfactors :accessor vads-dfactors
              :initform nil
              :initarg :dfactors
              :documentation "Array's dimensional factors."))
  (:metaclass va-class)
  (:documentation "Superclass of derived arrays with cached dimensional factors."))

(defclass varray-primal (varray)
  nil (:metaclass va-class)
  (:documentation "A primal array: a virtual array defined wholly by its parameters, not derived from another array."))

(defclass varray-derived (varray)
  ((%base :accessor vader-base
          :initform nil
          :initarg :base
          :documentation "The array from which the array is derived.")
   (%layer :accessor vader-layer
           :initform 0
           :initarg :layer
           :documentation "The number of derived virtual arrays downstream of this array.")
   ;; (%parallel :accessor vader-parallel
   ;;            :initform t
   ;;            :initarg :parallel
   ;;            :documentation "Whether this array and derivative arrays can be indexed in parallel.")
   (%subrendering :accessor vads-subrendering
                  :initform nil
                  :initarg :subrendering
                  :documentation "Whether the array contains nested elements to be subrendered.")
   (%content :accessor vader-content
             :initform nil
             :initarg :inverse
             :documentation "Cached rendered content of the array."))
  (:metaclass va-class)
  (:documentation "A derived array: a virtual array derived from another array."))

(defmethod initialize-instance :around ((varray varray-derived) &key)
  "If the instance's base slot is already bound, it has been populated through one of he above type combinatorics and so should be returned with no changes."
  (when (not (slot-boundp varray '%base))
    (call-next-method))
  
  (when (typep (vader-base varray) 'varray-derived)
    (setf (vader-layer varray) ;; count layers from a non-derived array
          (1+ (vader-layer (vader-base varray))))))

(defmethod shape-of :around ((varray varray-derived))
  (let ((this-shape (call-next-method)))
    (if (varray-meta varray)
        this-shape
        (let* ((this-rank (length this-shape))
               (this-size (reduce #'* this-shape))
               (base-meta (when (typep (vader-base varray) 'varray-derived)
                            (varray-meta (vader-base varray))))
               (base-shape (or (getf base-meta :max-shape)
                               (shape-of (vader-base varray))))
               (max-dim (or (getf base-meta :max-dim)
                            (reduce #'max (or base-shape '(0)))))
               (base-rank (length base-shape))
               (base-size (or (getf base-meta :max-size)
                              (reduce #'* base-shape)))
               (base-lower-rank (< base-rank this-rank))
               (max-shape (if base-lower-rank this-shape base-shape))
               (generator-meta (or (getf base-meta :gen-meta) (list :items))))
          
          (setf max-shape (if base-lower-rank
                              (loop :for s :in this-shape :for sx :from 0
                                    :collect (let ((item (if (>= sx base-rank)
                                                             s (max s (nth sx base-shape)))))
                                               (setf max-dim (max item max-dim))
                                               item))
                              (loop :for s :in base-shape :for sx :from 0
                                    :collect (let ((item (if (>= sx this-rank)
                                                             s (max s (nth sx this-shape)))))
                                               (setf max-dim (max item max-dim))
                                               item))))
          
          (setf (getf (varray-meta varray) :max-size) (max this-size base-size)
                (getf (varray-meta varray) :max-shape) max-shape
                (getf (varray-meta varray) :max-dim) max-dim
                (getf (varray-meta varray) :gen-meta) generator-meta)
          this-shape))))

(defmethod etype-of ((varray varray-derived))
  "The default shape of a derived array is the same as its base array."
  (if (varrayp (vader-base varray))
      (etype-of (vader-base varray))
      (if (arrayp (vader-base varray))
          (array-element-type (vader-base varray))
          (assign-element-type (vader-base varray)))))

(defmethod prototype-of ((varray varray-derived))
  (let ((shape (shape-of varray)))
    ;; (print (list :vd varray (vader-base varray)
    ;;              (print (prototype-of (vader-base varray)))
    ;;              (subrendering-p varray)
    ;;              (subrendering-p (vader-base varray))))
    ;; (print (list :ba varray (vader-base varray) shape
    ;;              (render (vader-base varray))))
    (if (or (not shape) (loop :for dim :in shape :never (zerop dim)))
        (if (and (not (or (typep varray 'vader-expand)
                          (typep varray 'vader-catenate)))
                 ;; TODO: functions that combine an array of arguments shouldn't have base subrendering
                 ;; checked. Is there a better way to establish this rule?
                 (subrendering-p (vader-base varray)))
            (aplesque::make-empty-array (disclose (render (vader-base varray))))
            (if (subrendering-p varray)
                (aplesque::make-empty-array (disclose (render (vader-base varray))))
                ;; (prototype-of (disclose (render (vader-base varray))))
                (let* ((indexer (generator-of varray))
                       (indexed (if (not (functionp indexer))
                                    indexer (funcall indexer 0))))
                  ;; (print (list :aaa indexed varray))
                  ;; (print (list :in indexed (typep indexed 'varray) (type-of indexed)
                  ;;              (if (varrayp indexed) (vader-base indexed))))
                  ;; TODO: remove-disclose when [⍴ shape] is virtually implemented
                  (if indexed (if (typep indexed 'varray)
                                  (prototype-of indexed)
                                  (aplesque::make-empty-array (disclose indexed)))
                      (prototype-of (vader-base varray))))))
        (prototype-of (vader-base varray)))))

(defmethod shape-of ((varray varray-derived))
  "The default shape of a derived array is the same as the original array."
  (get-promised (varray-shape varray) (shape-of (vader-base varray))))

(defmethod base-indexer-of ((varray varray-derived) &optional params)
  "The default shape of a derived array is the same as the original array."
  (let ((this-shape (shape-of varray)))
    ;; (print (list :ti this-shape varray (vader-base varray)))
    ;; (setf april::ccc varray)
    (generator-of (vader-base varray) nil
                  (append (list :shape-deriving (if (typep varray 'vader-reshape)
                                                  (shape-of varray)
                                                  (if (and (typep varray 'vader-section)
                                                           (= 1 (size-of (vads-argument varray))))
                                                      (funcall (lambda (item)
                                                                 (if (typep item 'sequence)
                                                                     (coerce item 'list)
                                                                     (list item)))
                                                               (render (vads-argument varray))))))
                        params))))

(defclass vapri-integer-progression (varray-primal)
  ((%number :accessor vapip-number
            :initform 1
            :initarg :number
            :documentation "The number of values.")
   (%origin :accessor vapip-origin
            :initform 0
            :initarg :origin
            :documentation "The origin point - by default, the index origin.")
   (%factor :accessor vapip-factor
            :initform 1
            :initarg :factor
            :documentation "Factor of values.")
   (%repeat :accessor vapip-repeat
            :initform 1
            :initarg :repeat
            :documentation "Instances of each value."))
  (:metaclass va-class)
  (:documentation "Integer progression vector - a series of numeric values generated by [⍳ index]."))

(defmethod etype-of ((vvector vapri-integer-progression))
  (if (floatp (vapip-factor vvector))
      'double-float (list 'integer (min 0 (vapip-origin vvector))
                          (+ (vapip-origin vvector)
                             (first (shape-of vvector))))))

(defmethod prototype-of ((vvector vapri-integer-progression))
  (declare (ignore vvector))
  0)

;; the shape of an IP vector is its number times its repetition
(defmethod shape-of ((vvector vapri-integer-progression))
  (get-promised (varray-shape vvector) (list (* (vapip-number vvector)
                                                (vapip-repeat vvector)))))

;; the IP vector's parameters are used to index its contents
(defmethod generator-of ((vvector vapri-integer-progression) &optional indexers params)
  (declare (ignore params) (optimize (speed 3) (safety 0)))
  (get-promised (varray-indexer vvector)
                (let ((origin (the (unsigned-byte 62) (vapip-origin vvector)))
                      (factor (the real (vapip-factor vvector)))
                      (repeat (the (unsigned-byte 62) (vapip-repeat vvector))))
                  (funcall (if (or (and (integerp factor) (= 1 factor))
                                   (and (typep factor 'single-float) (= 1.0 factor))
                                   (and (typep factor 'double-float) (= 1.0d0 factor)))
                               #'identity
                               (if (integerp factor)
                                   (lambda (fn) (lambda (item)
                                                  (declare (type (unsigned-byte 62) item))
                                                  (the (unsigned-byte 64)
                                                       (* (the (unsigned-byte 62) factor)
                                                          (the (unsigned-byte 62)
                                                               (funcall (the function fn) item))))))
                                   (lambda (fn) (lambda (item)
                                                  (declare (type (unsigned-byte 62) item)
                                                           (type function fn))
                                                  (* (the float factor)
                                                     (funcall fn item))))))
                           (if (= 1 repeat)
                               (if (zerop origin)
                                   #'identity (if (= 1 origin)
                                                  (lambda (index)
                                                    (declare (type (unsigned-byte 62) index))
                                                    (the (unsigned-byte 62) (1+ index)))
                                                  (lambda (index)
                                                    (declare (type (unsigned-byte 62) index))
                                                    (the (unsigned-byte 64) (+ origin index)))))
                               (lambda (index)
                                 (declare (type (unsigned-byte 62) index))
                                 (the (unsigned-byte 64)
                                      (+ origin (the (unsigned-byte 62) (floor index repeat))))))))))
      
(defclass vapri-coordinate-vector (varray-primal)
  ((%reference :accessor vacov-reference
               :initform nil
               :initarg :reference
               :documentation "The array to which this coordinate vector belongs.")
   (%index :accessor vacov-index
           :initform 0
           :initarg :index
           :documentation "The row-major index of the referenced array this coordinate vector represents."))
  (:metaclass va-class)
  (:documentation "Coordinate vector - a vector of the integer coordinates corresponding to a given row-major index in an array."))

(defmethod etype-of ((vvector vapri-coordinate-vector))
  "The type of the coordinate vector."
  ;; if this refers to a [⍸ where] invocation, it is based on the shape of the argument to [⍸ where];
  ;; it cannot directly reference the argument because the [⍸ where] invocation" because the dimensional
  ;; factors are stored along with the [⍸ where] object
  (list 'integer 0 (reduce #'max (if (typep (vacov-reference vvector) 'vader-where)
                                     (shape-of (vader-base (vacov-reference vvector)))
                                     (shape-of (vacov-reference vvector))))))

(defmethod prototype-of ((vvector vapri-coordinate-vector))
  (declare (ignore vvector))
  0)

(defmethod shape-of ((vvector vapri-coordinate-vector))
  (get-promised (varray-shape vvector)
                (list (length (vads-dfactors (vacov-reference vvector))))))

(defmethod generator-of ((vvector vapri-coordinate-vector) &optional indexers params)
  (let* ((dfactors (vads-dfactors (vacov-reference vvector)))
         (output (make-array (length dfactors) :element-type (etype-of vvector)))
         (remaining (vacov-index vvector)))
    (loop :for f :across dfactors :for ix :from 0
          :do (multiple-value-bind (item remainder) (floor remaining f)
                (setf (aref output ix) (+ item (vads-io (vacov-reference vvector)))
                      remaining remainder)))
    (lambda (index) (aref output index))))

(defclass vapri-coordinate-identity (vad-subrendering varray-primal vad-with-io vad-with-dfactors)
  ((%shape :accessor vapci-shape
           :initform 1
           :initarg :number
           :documentation "The shape of the array."))
  (:metaclass va-class)
  (:documentation "Coordinate identity array - an array of coordinate vectors generated by [⍳ index]."))

(defmethod etype-of ((varray vapri-coordinate-identity))
  "Being a nested array, the type is always t."
  (declare (ignore varray))
  t)

(defmethod prototype-of ((varray vapri-coordinate-identity))
  "Prototype is an array of zeroes with length equal to the array's rank."
  (make-array (length (vapci-shape varray)) :element-type 'bit :initial-element 0))

(defmethod shape-of ((varray vapri-coordinate-identity))
  "Shape is explicit; dimensional factors are generated by this function if not set."
  (when (not (vads-dfactors varray))
    (setf (vads-dfactors varray)
          (get-dimensional-factors (vapci-shape varray) t)))
  (get-promised (varray-shape varray)
                (vapci-shape varray)))

;; (defmethod iorigin-of ((varray vapri-coordinate-identity))
;;   (vapci-origin varray))

(defmethod generator-of ((varray vapri-coordinate-identity) &optional indexers params)
  "Each index returns a coordinate vector."
  (lambda (index) (make-instance 'vapri-coordinate-vector
                                 :reference varray :index index)))

(defclass vapri-axis-vector (vad-subrendering varray-primal vad-with-io vad-with-dfactors)
  ((%reference :accessor vaxv-reference
               :initform nil
               :initarg :reference
               :documentation "The array to which this axis vector belongs.")
   (%axis :accessor vaxv-axis
          :initform nil
          :initarg :axis
          :documentation "The axis along which the axis vector leads.")
   (%window :accessor vaxv-window
            :initform nil
            :initarg :window
            :documentation "The window of division along the axis.")
   (%index :accessor vaxv-index
           :initform nil
           :initarg :index
           :documentation "This axis vector's index within the reference array reduced along the axis."))
  (:metaclass va-class)
  (:documentation "A sub-vector along an axis of an array."))

(defmethod etype-of ((varray vapri-axis-vector))
  (etype-of (vaxv-reference varray)))

(defmethod shape-of ((varray vapri-axis-vector))
  (get-promised (varray-shape varray)
                (list (or (vaxv-window varray)
                          (nth (vaxv-axis varray) (shape-of (vaxv-reference varray)))))))

(defmethod generator-of ((varray vapri-axis-vector) &optional indexers params)
  (get-promised (varray-indexer varray)
                (let* ((axis (vaxv-axis varray))
                       (window (vaxv-window varray))
                       (wsegment)
                       (ref-index (vaxv-index varray))
                       (ref-indexer (generator-of (vaxv-reference varray)))
                       (irank (rank-of (vaxv-reference varray)))
                       (idims (shape-of (vaxv-reference varray)))
                       (rlen (nth axis idims))
                       (increment (reduce #'* (nthcdr (1+ axis) idims))))
                  (loop :for dim :in idims :for dx :from 0
                        :when (and window (= dx axis))
                          :do (setq wsegment (- dim (1- window))))
                  (let ((delta (+ (if window (* rlen (floor ref-index wsegment))
                                      (if (= 1 increment)
                                          0 (* (floor ref-index increment)
                                               (- (* increment rlen) increment))))
                                  (if (/= 1 increment) ref-index
                                      (if window (if (>= 1 irank) ref-index
                                                     (mod ref-index wsegment))
                                          (* ref-index rlen))))))
                    (lambda (index) (funcall ref-indexer (+ delta (* index increment))))))))

;; superclasses encompassing array derivations taking different types of parameters

(defclass vad-reindexing ()
  nil (:metaclass va-class)
  (:documentation "Superclass of array transformations that add an index transformation to those accumulated."))

(defclass vad-on-axis ()
  ((%axis :accessor vads-axis
          :initform nil
          :initarg :axis
          :documentation "The axis along which to transform."))
  (:metaclass va-class)
  (:documentation "Superclass of array transformations occuring along an axis."))

(defclass vad-with-default-axis ()
  ((%default-axis :accessor vads-default-axis
                  :initform nil
                  :initarg :default-axis
                  :documentation "The default axis along which to transform."))
  (:metaclass va-class)
  (:documentation "Superclass of array transformations with a default axis."))

(defclass vad-with-argument ()
  ((%argument :accessor vads-argument
              :initform nil
              :initarg :argument
              :documentation "Parameters passed to an array transformation as an argument."))
  (:metaclass va-class)
  (:documentation "Superclass of array transformations occuring along an axis."))

(defclass vad-with-ct ()
  ((%comp-tolerance :accessor vads-ct
                    :initform 0
                    :initarg :comparison-tolerance
                    :documentation "Parameter specifying the comparison tolerance for an array operation."))
  (:metaclass va-class)
  (:documentation "Superclass of array transformations taking comparison tolerance as an implicit argument."))

(defclass vad-with-rng ()
  ((%rng :accessor vads-rng
         :initform nil
         :initarg :rng
         :documentation "Random number generator specification for array transformations that use it."))
  (:metaclass va-class)
  (:documentation "Superclass of array transformations occuring along an axis."))

(defclass vad-maybe-shapeless ()
  ((%determined :accessor vads-shapeset
                :initform nil
                :initarg :determined
                :documentation "Whether array's shape is determined."))
  (:metaclass va-class)
  (:documentation "Superclass of array transformations taking index origin as an implicit argument."))

(defclass vad-invertable ()
  ((%inverse :accessor vads-inverse
             :initform nil
             :initarg :inverse
             :documentation "Parameters passed to an array transformation as an argument."))
  (:metaclass va-class)
  (:documentation "Superclass of array transformations that have an inverse variant as [↓ drop] is to [↑ take]."))

(defclass vad-limitable ()
  nil (:metaclass va-class)
  (:documentation "Superclass of indefinite array transformations whose output can be dimensionally limited to avoid needless computation."))

;; (defclass vad-meta-compounding ()
;;   nil (:metaclass va-class)
;;   (:documentation "Class of array transformations"))

;; this is subrendering for the case of ≡↓↓2 3⍴⍳6
(defclass vader-subarray (vad-subrendering varray-derived)
  ((%index :accessor vasv-index
           :initform nil
           :initarg :index
           :documentation "Index variable for subarray; often used to index the subarray as part of the larger array it derives from."))
  (:metaclass va-class)
  (:documentation "Subarray of a larger array; its relationship to the larger array is defined by its index parameter, indexer and prototype functions."))

(defmethod prototype-of ((varray vader-subarray))
  (varray-prototype varray))

(defmethod generator-of ((varray vader-subarray) &optional indexers params)
  (varray-indexer varray))

;; this is subrendering for the case of ≡↓↓2 3⍴⍳6
;; (defclass vader-subvector (vad-on-axis vad-invertable)
;;   ((%index :accessor vasv-index
;;            :initform nil
;;            :initarg :index
;;            :documentation "Index value for subvector; its position in main array.")
;;    (%window :accessor vasv-window
;;             :initform nil
;;             :initarg :window
;;             :documentation "Window value for subvector."))
;;   (:metaclass va-class)
;;   (:documentation "Subvector."))

;; (defmethod prototype-of ((varray vader-subvector))
;;   (prototype-of (vader-base varray))
;;   ;; TODO: this is wrong, take prototype from first element
;;   )

;; (defmethod shape-of ((varray vader-subvector))
;;   (get-promised (varray-shape varray)
;;                 (or (vasv-window varray)
;;                     (nth (vads-axis varray)
;;                          (shape-of (vader-base varray))))))

;; (defmethod generator-of ((varray vader-subvector) &optional indexers params)
;;   (get-promised (varray-indexer varray)
;;                 (let* ((valix 0)
;;                        (axis (vads-axis varray))
;;                        (window (vasv-window varray))
;;                        (odims (shape-of (vacmp-omega varray)))
;;                        (irank (rank-of (vader-base varray)))
;;                        (rlen (nth (vads-axis varray)
;;                                   (shape-of (vader-base varray))))
;;                        (base-indexer (generator-of (vader-base varray)))
;;                        (increment (reduce #'* (nthcdr (1+ axis) odims)))
;;                        (wsegment (and window (loop :for dim :in odims :for dx :from 0
;;                                                    :when (and window (= dx axis))
;;                                                      :return (- dim (1- window)))))
;;                        ;; (value (make-array ax-interval))
;;                        (increment-diff (- (* increment rlen) increment))
;;                        (this-index (vasv-index varray))
;;                        (delta (+ (if window (* rlen (floor this-index wsegment))
;;                                      (if (= 1 increment)
;;                                          0 (* increment-diff (floor this-index increment))))
;;                                  (if (/= 1 increment) this-index
;;                                      (if window (if (>= 1 irank)
;;                                                     this-index (mod this-index wsegment))
;;                                          (* this-index rlen))))))
;;                   ;; (print (list 13 axis out-dims (vacmp-omega varray) value valix))
                  
;;                   ;; (print (list :vv value))
;;                   ;; (print (list :win window window-reversed))
;;                   ;; (if window-reversed
;;                   ;;     (lambda (index)
;;                   ;;       (loop :for ix :below window
;;                   ;;             :do (setf (aref value (- ax-interval (incf valix)))
;;                   ;;                       (process-item i ix delta))))
;;                   (if (not (functionp base-indexer))
;;                       (lambda (index) (declare (ignore index)) base-indexer)
;;                       (lambda (index)
;;                         (funcall base-indexer (+ delta (* index increment))))))))

(defclass vader-operate (vad-subrendering varray-derived vad-on-axis vad-with-io)
  ((%params :accessor vaop-params
            :initform nil
            :initarg :params
            :documentation "Parameters for scalar operation to be performed.")
   (%function :accessor vaop-function
              :initform nil
              :initarg :function
              :documentation "Function to be applied to derived array element(s).")
   (%sub-shape :accessor vaop-sub-shape
               :initform nil
               :initarg :sub-shape
               :documentation "Shape of a lower-rank array to be combined with a higher-rank array along given axes."))
  (:metaclass va-class))

(defmethod etype-of ((varray vader-operate))
  (if (and (getf (vaop-params varray) :binary-output)
           (if (listp (vader-base varray))
               (loop :for item :in (vader-base varray) :always (not (shape-of item)))
               (loop :for item :across (vader-base varray) :always (not (shape-of item)))))
      'bit t))

(defmethod prototype-of ((varray vader-operate))
  (if (or (and (not (listp (vader-base varray)))
               (= 1 (size-of (vader-base varray))))
          (and (listp (vader-base varray))
               (= 1 (length (vader-base varray)))))
      (let* ((first-item (if (varrayp (vader-base varray))
                             (let ((gen (generator-of (vader-base varray))))
                               (if (not (functionp gen))
                                   gen (funcall (generator-of (vader-base varray)) 0)))
                             (elt (vader-base varray) 0)))
             (first-shape (shape-of first-item))
             (first-indexer (generator-of first-item)))
        (if (zerop (reduce #'* first-shape))
            (prototype-of (render first-item))
            (if first-shape
                (prototype-of (apply-scalar (vaop-function varray)
                                            (render (if (not (functionp first-indexer))
                                                        first-indexer (funcall first-indexer 0)))))
                (prototype-of (render (if (not (functionp first-indexer))
                                          first-indexer (funcall first-indexer 0)))))))
      (let ((pre-proto)
            (base-list (when (listp (vader-base varray))
                         (vader-base varray)))
            (base-size (if (listp (vader-base varray))
                           (length (vader-base varray))
                           (size-of (vader-base varray))))
            (base-indexer (generator-of (vader-base varray))))
        ;; TODO: more optimization is possible here
        (loop :for i :below base-size ; :for item :across (vader-base varray)
              :do (let ((item (if base-list (first base-list)
                                  (funcall base-indexer i))))
                    ;; (print (list :it item))
                    (when (< 0 (size-of item))
                      (let ((this-indexer (generator-of item)))
                        ;; (print (list :ti this-indexer item))
                        (if (not pre-proto)
                            (setf pre-proto (render (if (not (functionp this-indexer))
                                                        this-indexer (funcall this-indexer 0))))
                            (setf pre-proto
                                  (apply-scalar (vaop-function varray)
                                                pre-proto
                                                (render (if (not (functionp this-indexer))
                                                            this-indexer (funcall this-indexer 0))))))))
                    (setf base-list (rest base-list))))
        (prototype-of pre-proto))))

(defmethod shape-of ((varray vader-operate))
  (get-promised
   (varray-shape varray)
   (let ((shape) (sub-shape)
         (base-size (if (listp (vader-base varray))
                        (length (vader-base varray))
                        (size-of (vader-base varray))))
         (base-indexer (when (not (listp (vader-base varray)))
                         (generator-of (vader-base varray))))
         (axis (setf (vads-axis varray)
                     (when (vads-axis varray)
                       (funcall (lambda (ax)
                                  (if (numberp ax)
                                      (- ax (vads-io varray))
                                      (if (zerop (size-of ax))
                                          ax (if (= 1 (length ax)) ;; disclose 1-item axis vectors
                                                 (- (aref ax 0) (vads-io varray))
                                                 (loop :for a :across ax
                                                       :collect (- a (vads-io varray)))))))
                                (disclose (render (vads-axis varray)))))))
         (base-list (when (listp (vader-base varray)) (vader-base varray))))
     (flet ((shape-matches (a)
              (loop :for s1 :in shape :for s2 :in (shape-of a) :always (= s1 s2))))
       (typecase (vader-base varray)
         (vapri-integer-progression nil)
         ((or varray sequence)
          (loop :for i :below base-size
                :do (let ((a (if ;; (vectorp (vader-base varray))
                                 base-indexer
                                 (funcall base-indexer i)
                                 ;; (aref (vader-base varray) i)
                                 (when base-list (first base-list)))))
                      (when (shape-of a) ;; 1-element arrays are treated as scalars
                        (if (or (not shape)
                                (= 1 (reduce #'* shape)))
                            (setf shape (shape-of a))
                            (let ((rank (length (shape-of a))))
                              (when (or (not (= rank (length shape)))
                                        (and (not (shape-matches a))
                                             (not (= 1 (size-of a)))))
                                (if axis (if (= (length shape)
                                                (if (numberp axis) 1 (length axis)))
                                             (if (> rank (length shape))
                                                 (let ((ax-copy (when (listp axis) (copy-list axis)))
                                                       (shape-copy (copy-list shape))
                                                       (matching t))
                                                   (when (not (vaop-sub-shape varray))
                                                     (setf (vaop-sub-shape varray) shape))
                                                   (loop :for d :in (shape-of a) :for ix :from 0
                                                         :when (and (if ax-copy (= ix (first ax-copy))
                                                                        (= ix axis)))
                                                           :do (when (/= d (first shape-copy))
                                                                 (setf matching nil))
                                                               (setf ax-copy (rest ax-copy)
                                                                     shape-copy (rest shape-copy)))
                                                   (if matching (setf shape (shape-of a))
                                                       (error "Mismatched array dimensions.")))
                                                 (if (= rank (length shape))
                                                     (when (not (shape-matches a))
                                                       (error "Mismatched array dimensions."))
                                                     (error "Mismatched array dimensions.")))
                                             (if (= rank (if (numberp axis) 1 (length axis)))
                                                 (if (not (shape-matches a))
                                                     (or (and (numberp axis)
                                                              (= (first (shape-of a))
                                                                 (nth axis shape))
                                                              (setf (vaop-sub-shape varray)
                                                                    (shape-of a)))
                                                         (and (listp axis)
                                                              (loop :for ax :in axis :for sh :in shape
                                                                    :always (= sh (nth ax shape)))
                                                              (setf (vaop-sub-shape varray) shape))
                                                         (error "Mismatched array dimensions."))
                                                     (setf (vaop-sub-shape varray) (shape-of a)))
                                                 (error "Mismatched array dimensions.")))
                                    (error "Mismatched array dimensions."))))))
                      (setf base-list (rest base-list))))))
       shape))))

(defmethod generator-of ((varray vader-operate) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let* ((out-shape (shape-of varray))
          (sub-shape (vaop-sub-shape varray))
          (out-rank (rank-of varray))
          (axis (vads-axis varray))
          (shape-factors (when axis (get-dimensional-factors out-shape t)))
          (sub-factors (when axis (get-dimensional-factors sub-shape t)))
          (base-size (if (listp (vader-base varray))
                         (length (vader-base varray))
                         (size-of (vader-base varray)))))
     ;; (print (list :vb (vader-base varray) base-size))
     (cond
       ;; ((typep (vader-base varray) 'vapri-integer-progression)
       ;;  (setf result (loop :for i :below ())))
       ((= 1 base-size)
        (let ((base-indexer (generator-of (if (varrayp (vader-base varray))
                                              (funcall (generator-of (vader-base varray)) 0)
                                              (elt (vader-base varray) 0)))))
          (lambda (index)
            
            (if (not (functionp base-indexer))
                (funcall (vaop-function varray) base-indexer)
                (let ((indexed (funcall base-indexer index)))
                  (if (or (arrayp indexed) (varrayp indexed))
                      (make-instance 'vader-operate
                                     :base (vector indexed) :function (vaop-function varray)
                                     :index-origin (vads-io varray) :params (vaop-params varray))
                      (funcall (vaop-function varray)
                               (funcall base-indexer index))))))))
       ((or (vectorp (vader-base varray))
            (varrayp (vader-base varray)))
        (let ((indexer (generator-of (vader-base varray))))
          ;; (print (list :in indexer varray (funcall indexer 0)
          ;;              (render (vader-base varray))))
          (lambda (index)
            (let ((result) (subarrays) (sub-flag))
              (loop :for ax :below (size-of (vader-base varray))
                    :do (let* ((a (funcall indexer ax))
                               (ai (generator-of a))
                               (size (size-of a))
                               (item (if (and (shape-of a) (< 1 size))
                                         (or (if (not (functionp ai))
                                                 ai (funcall ai index))
                                             (prototype-of a))
                                         (if (not (varrayp a))
                                             (if (not (and (arrayp a) (= 1 size)))
                                                 a (if (not (functionp ai))
                                                       ai (funcall ai 0)))
                                             (if (not (functionp ai))
                                                 ai (funcall ai 0))))))
                          ;; (print (list :aa a item (shape-of varray)))
                          (push item subarrays)
                          ;; TODO: this list appending is wasteful for simple ops like 1+2
                          (if (or (arrayp item) (varrayp item))
                              (setf sub-flag t)
                              (setf result (if (not result)
                                               item (funcall (vaop-function varray)
                                                             result item))))))
              ;; (print (list :eee sub-flag))
              (if (not sub-flag)
                  result (make-instance 'vader-operate :base (coerce (reverse subarrays) 'vector)
                                                       :function (vaop-function varray)
                                                       :index-origin (vads-io varray)
                                                       :params (vaop-params varray)))))))
       (t (lambda (index)
            (let ((result) (subarrays) (sub-flag))
              (loop :for a :in (vader-base varray)
                    :do (let* ((ai (generator-of a))
                               (size (size-of a))
                               (item (if (and (shape-of a) (< 1 size))
                                         (if (not (functionp ai))
                                             ai
                                             (if (and axis (not (= out-rank (rank-of a))))
                                                 (funcall
                                                  ai (if (numberp axis)
                                                         (mod (floor index (aref shape-factors axis))
                                                              size)
                                                         (let ((remaining index) (sub-index 0))
                                                           (loop :for f :across shape-factors :for fx :from 0
                                                                 :do (multiple-value-bind (div remainder)
                                                                         (floor remaining f)
                                                                       (setf remaining remainder)
                                                                       (loop :for ax :in axis
                                                                             :for ix :from 0 :when (= ax fx)
                                                                             :do (incf sub-index
                                                                                       (* (aref sub-factors
                                                                                                ix)
                                                                                          div)))))
                                                           sub-index)))
                                                 (or (funcall ai index)
                                                     (prototype-of a))))
                                         (if (not (varrayp a))
                                             (if (not (and (arrayp a) (= 1 size)))
                                                 a (if (not (functionp ai))
                                                       ai (funcall ai 0)))
                                             (if (not (functionp ai))
                                                 ai (funcall ai 0))))))
                          (push item subarrays)
                          ;; TODO: this list appending is wasteful for simple ops like 1+2
                          (if (or (arrayp item) (varrayp item))
                              (setf sub-flag t)
                              (setf result (if (not result)
                                               item (funcall (vaop-function varray)
                                                             result item))))))
              ;; (print (list :eee))
              (if (not sub-flag)
                  result (make-instance 'vader-operate :base (coerce (reverse subarrays) 'vector)
                                                       :function (vaop-function varray)
                                                       :index-origin (vads-io varray)
                                                       :params (vaop-params varray))))))))))

(defclass vader-select (varray-derived vad-on-axis vad-with-io vad-with-argument)
  ((%assign :accessor vasel-assign
            :initform nil
            :initarg :assign
            :documentation "Item(s) to be assigned to selected indices in array.")
   (%assign-if :accessor vasel-assign-if
               :initform nil
               :initarg :assign-if
               :documentation "Function to select items to be assigned as for ⌽@(<∘5)⊢⍳9.")
   (%assign-shape :accessor vasel-assign-shape
                  :initform nil
                  :initarg :assign-shape
                  :documentation "Shape of area to be assigned, eliding 1-sized dimensions.")
   (%calling :accessor vasel-calling
             :initform nil
             :initarg :calling
             :documentation "Function to be called on original and assigned index values.")
   (%selector :accessor vasel-selector
              :initform nil
              :initarg :selector
              :documentation "Object implementing selection function for array."))
  (:metaclass va-class))

(defmethod etype-of ((varray vader-select))
  (if (vasel-calling varray)
      t (if (vasel-assign varray)
            (type-in-common (etype-of (vader-base varray))
                            (etype-of (vasel-assign varray)))
            (call-next-method))))

(defmethod shape-of ((varray vader-select))
  (get-promised
   (varray-shape varray)
   (let* ((idims (shape-of (vader-base varray)))
          (set (vasel-assign varray))
          (indices (when (not (typep (vads-argument varray) 'varray::varray))
                     (vads-argument varray)))
          (naxes (when indices (< 1 (length indices))))
          (assign-shape (when (and set indices)
                          (setf (vasel-assign-shape varray)
                                (loop :for i :in indices :for id :in idims
                                      :when (not i) :collect id
                                        :when (and (shape-of i)
                                                   (< 1 (size-of i)))
                                          :collect (size-of i)))))
          (s 0) (sdims (when set (shape-of set))))
     ;; (print (list :i indices sdims assign-shape set))
     (if (not indices)
         idims (if set (if (and sdims (not (loop :for i :in assign-shape :for sd :in sdims
                                                 :always (= sd i))))
                           (error "Dimensions of assigned area don't ~a"
                                  "match array to be assigned.")
                           idims)
                   (if naxes (loop :for i :in indices :for d :in idims
                                   :append (let ((len (or (and (null i) (list d))
                                                          (and (integerp i) nil)
                                                          (shape-of i))))
                                             (when (and (not len) (not (integerp i)))
                                               (error "Invalid index."))
                                             ;; collect output dimensions according to indices;
                                             ;; this is necessary even when setting values
                                             ;; compatible with the input array in order
                                             ;; to catch invalid indices
                                             (when len (incf s))
                                             len))
                       (shape-of (or (first indices)
                                     (vader-base varray)))))))))

(defmethod generator-of ((varray vader-select) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let* ((indices)
          (iarray-factors)
          (base-indexer (generator-of (vader-base varray)))
          (base-rank (rank-of (vader-base varray)))
          (set (vasel-assign varray))
          (set-indexer (generator-of set))
          (idims (shape-of varray))
          (selector-eindices (when (listp (vasel-selector varray))
                               (vasel-selector varray)))
          (eindexer (when selector-eindices (generator-of (getf selector-eindices :ebase))))
          (index-selector (when (not selector-eindices) (vasel-selector varray)))
          (sub-shape (shape-of index-selector))
          (is-picking)
          (sub-selector (multiple-value-bind (sselector is-pick)
                            (generator-of index-selector nil
                            ;; (indexer-of index-selector
                                          (list :for-selective-assign
                                                (or (shape-of (vasel-assign varray)) t)
                                                :assigning set
                                                :toggle-toplevel-subrendering
                                                (lambda (&optional abc)
                                                  (setf (vads-subrendering varray) t))))
                          (when is-pick (setf is-picking t))
                          sselector))
          (ofactors (when (not set) (get-dimensional-factors idims t)))
          (ifactors (get-dimensional-factors (shape-of (vader-base varray)) t))
          (adims (shape-of (vasel-assign varray)))
          (afactors (when adims (get-dimensional-factors (vasel-assign-shape varray) t))))
     ;; (if index-selector (setf april::ggg index-selector))
     ;; (print (list :in indices ofactors ifactors (vads-argument varray)))
     (setf indices (loop :for item :in (vads-argument varray)
                         :collect (let ((ishape (shape-of item)))
                                    (when (< 1 (length ishape))
                                      (push (get-dimensional-factors ishape)
                                            iarray-factors))
                                    (render item)))
           iarray-factors (reverse iarray-factors))
     ;; (print (list :ia ifactors))
     ;; (print (list :arg (render (vader-base varray)) (vads-argument varray)))
     (flet ((verify-vindex (ind vector-index)
              ;; (PRINT (LIST :III ind VECTOR-INDEX))
              (let ((vector-indexer (generator-of vector-index)))
                ;; (print (list :vv vector-indexer))
                (loop :for v :below (size-of vector-index) :for ix :from 0
                      :when (let* ((this-index (funcall vector-indexer v))
                                   (sub-index (funcall (generator-of this-index) 0)))
                              ;; (print (list :ti this-index sub-index))
                              (cond ((numberp sub-index)
                                     (= ind (- sub-index (vads-io varray))))
                                    ((shape-of sub-index)
                                     (= ind (reduce #'* (loop :for n :across (render sub-index)
                                                              :collect (- n (vads-io varray))))))
                                    (t nil)))
                        :return (funcall vector-indexer v))))
            (compare-path (pindex path)
              (let ((remaining pindex) (valid t))
                (loop :for if :across ifactors :for p :across path :while valid
                      :do (multiple-value-bind (dindex remainder) (floor remaining if)
                            (setf remaining remainder)
                            (when (/= dindex (- p (vads-io varray)))
                              (setf valid nil))))
                valid)))

       ;; (print (list :cc (vads-argument varray) indices ifactors ofactors afactors iarray-factors
       ;;              (vasel-calling varray)
       ;;              (vader-base varray) set))

       (if (not (or set (vasel-calling varray)))
           (lambda (index)
             (let* ((remaining index) (oindex 0) (ofix 0) (valid t) (iafactors iarray-factors))
               (loop :for in :in indices :for ifactor :across ifactors
                     :for ix :from 0 :while valid
                     :do (if (numberp in)
                             (incf oindex (* ifactor (- in (vads-io varray))))
                             (if in (let ((matched-index) (sub-index 0) (aindex index))
                                      (if (or (and (vectorp in) (< 0 (length in)))
                                              (and (or (arrayp in) (varrayp in))
                                                   (not (shape-of in))))
                                          (multiple-value-bind (index remainder)
                                              (floor remaining (if (zerop (length ofactors))
                                                                   1 (aref ofactors ofix)))
                                            (incf ofix)
                                            (setf sub-index index
                                                  remaining remainder))
                                          (progn (loop :for iafactor :in (first iafactors)
                                                       :do (multiple-value-bind (index remainder)
                                                               (floor remaining (aref ofactors ofix))
                                                             (incf sub-index (* iafactor index))
                                                             (incf ofix)
                                                             (setf remaining remainder)))
                                                 (when (not (vectorp in))
                                                   (setf iafactors (rest iafactors)))))
                                      (when (not matched-index)
                                        ;; adjust indices if the index was not an array as for x[⍳3]←5
                                        (let* ((iindexer (generator-of in))
                                               (indexed (if (not (functionp iindexer))
                                                            iindexer (funcall iindexer sub-index))))
                                          ;; (print (list :ind indexed))
                                          (if (numberp indexed)
                                              (incf oindex (* ifactor (- indexed (vads-io varray))))
                                              (setf oindex indexed)))))
                                 (multiple-value-bind (index remainder)
                                     (floor remaining (aref ofactors ofix))
                                   (let ((indexed (when in (funcall (generator-of in) index))))
                                     ;; if choose indexing is in use, set this object to subrender
                                     (when (or (not in) (numberp indexed))
                                       (incf oindex (* ifactor index))))
                                   (incf ofix)
                                   (setf remaining remainder))))
                         (setf adims (rest adims)))
               
               (if (numberp oindex)
                   (let ((indexed (if (not (functionp base-indexer))
                                      base-indexer (funcall base-indexer oindex))))
                     (when (not (shape-of varray))
                       (setf (vads-subrendering varray) t))
                     indexed)
                   (let ((index-shape (first (shape-of oindex))))
                     (setf (vads-subrendering varray) t)
                     (if (and (numberp (funcall base-indexer 0))
                              (= index-shape (length (shape-of (vader-base varray)))))
                         ;; if the length of the index vector is equal to the rank of the indexed array
                         ;; and the first element is a number, as for (5 5⍴⍳25)[(1 2)(3 4)]
                         (make-instance 'vader-select :base (vader-base varray)
                                                      :index-origin (vads-io varray)
                                                      :argument (coerce (render oindex) 'list))
                         ;; if the length of the index vector is not equal to the array's rank
                         ;; (as for (('JAN' 1)('FEB' 2)('MAR' 3))[(2 1)(1 2)])
                         ;; or the first element of the index vector is not a number
                         ;; (as for (2 3⍴('JAN' 1)('FEB' 2)('MAR' 3)('APR' 4)('MAY' 5)('JUN' 6))[((2 3)1)((1 1)2)])
                         (let* ((meta-indexer (generator-of oindex))
                                (meta-index (funcall meta-indexer 0))
                                (sub-base (make-instance 'vader-select
                                                         :base (vader-base varray)
                                                         :index-origin (vads-io varray)
                                                         :argument (if (numberp meta-index)
                                                                       (list meta-index)
                                                                       (coerce (render meta-index) 'list)))))
                           (make-instance 'vader-select :base (disclose (render sub-base))
                                          ;; TODO: wrap this in disclose obj
                                          :index-origin (vads-io varray)
                                          :argument (rest (coerce (render oindex) 'list)))))))))
           (if (vasel-assign-if varray)
               (let* ((assign-indexer (generator-of (vasel-assign varray)))
                      (mask (funcall (vasel-assign-if varray) (vader-base varray)))
                      (mask-indexer (generator-of mask)))
                 (if (vasel-calling varray)
                     ;; TODO: optimize types for mask indices
                     ;; the case of an array assigned depending on logical function result,
                     ;; for example 10 11 12 13@(<∘5)⍳9
                     (let ((mindex 0)
                           (mindices (make-array (shape-of (vader-base varray))
                                                 :element-type 'fixnum :initial-element 0)))
                       ;; create an index mask for the assignments where each nonzero number corresponds
                       ;; to the index of the assigned value - 1
                       (dotimes (i (size-of mask))
                         (when (not (zerop (funcall mask-indexer i)))
                           (setf (row-major-aref mindices i) (incf mindex))))
                       ;; TODO: add case for scalar functions as with ÷@(≤∘5)⊢3 3⍴⍳9 so that
                       ;; the function can work element by element instead of needing displacement
                       (let ((displaced (make-array mindex :element-type (etype-of (vader-base varray)))))
                         (setf mindex 0)
                         (dotimes (i (size-of mindices))
                           (let ((mi (row-major-aref mindices i)))
                             (when (not (zerop mi))
                               (setf (aref displaced (1- (incf mindex)))
                                     (funcall base-indexer i)))))
                         (let ((processed (render (funcall (vasel-calling varray) displaced
                                                           (vasel-assign varray)))))
                           (lambda (index) ;; the case of 10 11 12@(2∘|)⍳5
                             (let ((this-mindex (row-major-aref mindices index)))
                               (if (zerop this-mindex) (funcall base-indexer index)
                                   (aref processed (1- this-mindex))))))))
                     (if (functionp assign-indexer)
                         (lambda (index) ;; the case of (⊃⍳3)@(<∘5)⍳9 ; scalar (virtual) array assigned value
                           (if (zerop (funcall mask-indexer index))
                               (funcall assign-indexer 0) (funcall base-indexer index)))
                         (lambda (index) ;; the case of 9@(<∘5)⍳9 ; scalar assigned value
                           (if (not (zerop (funcall mask-indexer index)))
                               assign-indexer (funcall base-indexer index))))))
               
               (lambda (index)
                 ;;(print (list :ii index))
                 (let* ((remaining index) (oindex 0) (ofix 0) (valid t) (tafix 0)
                        (assign-sub-index) (iafactors iarray-factors))
                   (loop :for in :in indices :for ifactor :across ifactors
                         :for ix :from 0 :while valid
                         :do (if (numberp in) ;; handle numeric indices as for x[1;2]
                                 (multiple-value-bind (index remainder) (floor remaining ifactor)
                                   (when (/= index (- in (vads-io varray)))
                                     (setf valid nil))
                                   (incf ofix)
                                   (setf remaining remainder))
                                 ;; handle arrays as indices as for x[⍳3]
                                 (if in (let ((matched-index) (sub-index 0) (aindex index))
                                          ;; (print (list :ii in ofactors))
                                          (if (or (and (vectorp in) (< 0 (length in)))
                                                  (and (or (arrayp in) (varrayp in))
                                                       (not (shape-of in))))
                                              (multiple-value-bind (index remainder)
                                                  (floor remaining ifactor)
                                                ;; (print (list :fl index remainder in))
                                                (let* ((sub-indexer (generator-of in))
                                                       (sub-indexed (funcall sub-indexer 0))
                                                       (ssindexer (generator-of sub-indexed))
                                                       (ssindexed (when (functionp ssindexer)
                                                                    (funcall ssindexer 0))))
                                                  (if (numberp sub-indexed)
                                                      (let ((toindex)
                                                            (tafactor (if (or (not afactors)
                                                                              (zerop (length afactors)))
                                                                          1 (aref afactors tafix))))
                                                        (loop :for i :below (size-of in)
                                                              :do (when (= index (- (funcall sub-indexer i)
                                                                                    (vads-io varray)))
                                                                    (setf matched-index i
                                                                          toindex (+ oindex
                                                                                     (* i tafactor)))))
                                                        (incf tafix)
                                                        (when toindex (setf oindex toindex)))
                                                      (if (and ssindexed (numberp ssindexed)
                                                               (= base-rank (size-of sub-indexed)))
                                                          ;; the case of g←6 6⍴0 ⋄ g[(3 3)(4 4)]←5
                                                          ;; vectors the length of the base rank
                                                          ;; select an element in the base
                                                          (loop :for b :below (size-of in)
                                                                :do (when (compare-path
                                                                           aindex (funcall
                                                                                   sub-indexer b))
                                                                      (setf oindex aindex
                                                                            matched-index t
                                                                            assign-sub-index index)))
                                                          ;; the case of toasn←(('JAN' 1)('FEB' 2)
                                                          ;; toasn[(2 1)(1 2)]←45 67
                                                          ;; reach indexing into subarrays of the base
                                                          (let ((match (verify-vindex index in)))
                                                            (when match (setf oindex match
                                                                              matched-index t
                                                                              assign-sub-index index)))))
                                                  (setf valid matched-index))
                                                (incf ofix)
                                                (setf remaining remainder))
                                              
                                              (progn (loop :for iafactor :in (first iafactors)
                                                           :do (multiple-value-bind (index remainder)
                                                                   (floor remaining (aref ofactors ofix))
                                                                 (incf sub-index (* iafactor index))
                                                                 (incf ofix)
                                                                 (setf remaining remainder)))
                                                     (when (not (vectorp in))
                                                       (setf iafactors (rest iafactors)))))
                                          (if (zerop (size-of in)) ;; the case of ⍬@⍬⊢1 2 3
                                              (setf valid nil)
                                              (when (not matched-index)
                                                ;; adjust indices if the index was not an array as for x[⍳3]←5
                                                (let* ((iindexer (generator-of in))
                                                       (indexed (if (not (functionp iindexer))
                                                                    iindexer (funcall iindexer sub-index))))
                                                  ;; (print (list :ind indexed))
                                                  (if (numberp indexed)
                                                      (incf oindex (* ifactor (- indexed (vads-io varray))))
                                                      (setf oindex indexed))))))
                                     ;; handle elided indices
                                     (multiple-value-bind (index remainder)
                                         (floor remaining ifactor)
                                       (if (or (not adims)
                                               (< index (first adims)))
                                           (let ((tafactor (if (or (not afactors)
                                                                   (zerop (length afactors)))
                                                               1 (aref afactors tafix))))
                                             
                                             (if tafactor (incf oindex (* tafactor index))
                                                 (incf oindex index)))
                                           (setf valid nil))
                                       (incf ofix)
                                       (incf tafix)
                                       (setf remaining remainder))))
                             (setf adims (rest adims)))
                   ;; (print (list :aa index oindex index-selector set-indexer set valid))
                   ;; (print (list :vl index valid index-selector (funcall sub-selector index) (render index-selector)))
                   ;; index-selector is used in the case of assignment by selection,
                   ;; for example {A←'RANDOM' 'CHANCE' ⋄ (2↑¨A)←⍵ ⋄ A} '*'
                   (when index-selector
                     (setf valid (when (and (or valid (not (vads-argument varray)))
                                            (or (typep index-selector 'vader-pick)
                                                (setf oindex (if (numberp valid)
                                                                 (funcall sub-selector valid)
                                                                 (funcall sub-selector index)))))
                                   valid)))
                   ;; (print (list :val index valid oindex (shape-of oindex) selector-eindices))
                   ;; (if valid (print (list :oin oindex index afactors valid set (vasel-calling varray)
                   ;;                        set-indexer)))
                   ;; (print (list :se set-indexer oindex))
                   (if (numberp oindex)
                       (if valid (if (vasel-calling varray)
                                     (let ((original (if (not (functionp base-indexer))
                                                         base-indexer (funcall base-indexer index))))
                                       ;; (setf (vads-subrendering varray) t)
                                       (funcall (vasel-calling varray)
                                                original (if (functionp set-indexer)
                                                             (funcall set-indexer oindex)
                                                             (vasel-assign varray))))
                                     (if (not (functionp set-indexer))
                                         (if (and index-selector (typep index-selector 'vader-pick))
                                             ;; build a pick array instance to derive from an indexed value,
                                             ;; as for the case of {na←3⍴⊂⍳4 ⋄ (1↑⊃na)←⍵ ⋄ na} 99
                                             (let ((indexed (if (not (functionp base-indexer))
                                                                base-indexer (funcall base-indexer oindex))))
                                               (setf (vads-subrendering varray) t
                                                     (vader-base index-selector) indexed
                                                     (vapick-assign index-selector) (vasel-assign varray)
                                                     (vapick-reference index-selector) indexed)
                                               index-selector)
                                             (if selector-eindices
                                                 (let* ((bindex (if (not (functionp base-indexer))
                                                                    base-indexer (funcall base-indexer index)))
                                                        (assign-indexer (generator-of (vasel-assign varray)))
                                                        (eelement (when (not (arrayp bindex))
                                                                    (funcall eindexer index))))
                                                   (if eelement
                                                       (if (loop :for e :across (getf selector-eindices
                                                                                      :eindices)
                                                                 :never (= e eelement))
                                                           bindex (vasel-assign varray))
                                                       (progn
                                                         (setf (vads-subrendering varray) t)
                                                         (make-instance
                                                          'vader-select
                                                          :base bindex :index-origin (vads-io varray)
                                                          :assign (if (not (functionp assign-indexer))
                                                                      assign-indexer
                                                                      (funcall assign-indexer
                                                                               assign-sub-index))
                                                          :assign-shape (vasel-assign-shape varray)
                                                          :calling (vasel-calling varray)
                                                          :selector (list :eindices
                                                                          (getf selector-eindices
                                                                                :eindices)
                                                                          :ebase (funcall eindexer
                                                                                          index))))))
                                                 set-indexer))
                                         (funcall set-indexer oindex)))
                           (if (not (functionp base-indexer))
                               base-indexer (funcall base-indexer index)))
                       
                       (let ((index-shape (first (shape-of oindex))))
                         (setf (vads-subrendering varray) t)
                         ;; (print (list :in index-shape valid))
                         ;; (error "stop")
                         (if valid
                             (if (typep oindex 'varray::varray)
                                 ;; in the case of an [¨ each]-composed assignment by selection
                                 ;; like {A←'RANDOM' 'CHANCE' ⋄ (2↑¨A)←⍵ ⋄ A} '*'
                                 (let ((sub-indexer (generator-of (vader-base varray)))
                                       (assign-indexer (generator-of (vasel-assign varray)))
                                       (each-target (when (typep index-selector 'vacomp-each)
                                                      (vacmp-omega index-selector))))
                                   (make-instance 'vader-select
                                                  :base (funcall sub-indexer index)
                                                  :index-origin (vads-io varray)
                                                  :selector oindex
                                                  :assign (if (not (functionp assign-indexer))
                                                              assign-indexer (funcall assign-indexer
                                                                                      assign-sub-index))
                                                  :assign-shape (vasel-assign-shape varray)
                                                  :calling (vasel-calling varray)))
                                 (let* ((meta-indexer (generator-of oindex))
                                        (meta-index (funcall meta-indexer 0))
                                        (assign-indexer (generator-of (vasel-assign varray)))
                                        (sub-base (make-instance 'vader-select
                                                                 :base (vader-base varray)
                                                                 :index-origin (vads-io varray)
                                                                 :argument (if (not (vectorp meta-index))
                                                                               (list meta-index)
                                                                               (coerce (render meta-index)
                                                                                       'list)))))
                                   ;; (setf (vads-subrendering varray) t)
                                   ;; (setf (vads-subrendering (vader-base varray)) t)
                                   ;; (print (list :vaa (vasel-assign varray)
                                   ;;              (vader-base varray)))
                                   (make-instance 'vader-select
                                                  :base (disclose (render sub-base))
                                                  ;; TODO: wrap this in disclose obj
                                                  :index-origin (vads-io varray)
                                                  :argument (rest (coerce (render oindex) 'list))
                                                  :assign (if (not (functionp assign-indexer))
                                                              assign-indexer (funcall assign-indexer
                                                                                      assign-sub-index))
                                                  :subrendering t ; needed for cases like 3⌈@(⊂1 3)⊢3⍴⊂5⍴1
                                                  :assign-shape (vasel-assign-shape varray)
                                                  :calling (vasel-calling varray))))
                             (if (not (functionp base-indexer))
                                 base-indexer (funcall base-indexer index)))))))))))))

(defclass vader-random (varray-derived vad-with-rng vad-with-io)
  ((%cached :accessor varand-cached
            :initform nil
            :initarg :cached
            :documentation "Cached randomized array data."))
  (:metaclass va-class)
  (:documentation "An array of randomized elements as from the [? random] function."))

(defmethod etype-of ((varray vader-random))
  (declare (ignore varray)) ;; TODO: define narrower types in certain cases
  t)

(defun apl-random-process (item index-origin generator)
  "Core of (apl-random), randomizing an individual integer or float."
  (if (integerp item)
      (if (zerop item) (if (eq :system generator)
                           (+ double-float-epsilon (random (- 1.0d0 (* 2 double-float-epsilon))))
                           (random-state:random-float generator double-float-epsilon
                                                      (- 1.0d0 double-float-epsilon)))
          (if (eq :system generator) (+ index-origin (random item))
              (random-state:random-int generator index-origin (1- (+ item index-origin)))))
      (if (floatp item)
          (if (eq :system generator)
              (random item)
              (random-state:random-float
               generator double-float-epsilon (- (coerce item 'double-float)
                                                 double-float-epsilon)))
          (error "The right argument to ? can only contain non-negative integers or floats."))))

(defmethod generator-of ((varray vader-random) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let* ((rngs (vads-rng varray))
          (base-indexer (base-indexer-of varray))
          (scalar-base (not (or (varrayp (vader-base varray))
                                (arrayp (vader-base varray)))))
          (gen-name (getf (rest rngs) :rng))
          (generator (or (getf (rest rngs) gen-name)
                         (setf (getf (rest rngs) gen-name)
                               (if (eq :system gen-name)
                                   :system (random-state:make-generator gen-name)))))
          (seed (getf (rest rngs) :seed)))

     ;; randomized array content is generated synchronously
     ;; and cached in case a random seed is in use
     (when (and t ; seed
                (not (varand-cached varray)))
       (setf (varand-cached varray)
             (make-array (size-of (vader-base varray))
                         :element-type (etype-of varray)))
       (loop :for i :below (size-of (vader-base varray))
             :do (setf (row-major-aref (varand-cached varray) i)
                       (apl-random-process (funcall base-indexer i)
                                           (vads-io varray) generator))))

     ;; (if t ; seed
     ;;     (print (list seed (varand-cached varray))))

     ;; (print (list :vr rngs (varand-cached varray) (vads-io varray)))
     
     (lambda (index)
       (if scalar-base (apl-random-process (funcall base-indexer index) (vads-io varray)
                                           generator)
           (if t ; seed
               (row-major-aref (varand-cached varray) index)
               (apl-random-process (funcall base-indexer index) (vads-io varray)
                                   generator)))))))

(defclass vader-deal (varray-derived vad-with-argument vad-with-rng vad-with-io)
  ((%cached :accessor vadeal-cached
            :initform nil
            :initarg :cached
            :documentation "Cached shuffled vector elements."))
  (:metaclass va-class)
  (:documentation "A shuffled index vector as from the [? deal] function."))

(defmethod etype-of ((varray vader-deal))
  (if (/= 1 (reduce #'* (shape-of (vads-argument varray))))
      (error "Both arguments to ? must be non-negative integers.")
      (let* ((base-indexer (generator-of (vader-base varray)))
             (max (if (not (functionp base-indexer))
                      base-indexer (funcall base-indexer 0))))
        (list 'integer 0 (+ max (vads-io varray))))))

(defmethod prototype-of ((varray vader-deal))
  (declare (ignore varray))
  0)

(defmethod shape-of ((varray vader-deal))
  (if (/= 1 (reduce #'* (shape-of (vads-argument varray))))
      (error "Both arguments to ? must be non-negative integers.")
      (let* ((arg-indexer (generator-of (vads-argument varray)))
             (length (if (not (functionp arg-indexer))
                         arg-indexer (funcall arg-indexer 0)))
             ;; (base-indexer (generator-of (vader-base varray)))
             ;; (count (if (not (functionp base-indexer))
             ;;            base-indexer (funcall base-indexer 0)))
             ;; (vector (make-array count :element-type (etype-of varray)))
             )
        ;; (setf (vadeal-cached varray) vector)
        (if (integerp length) (list length)
            (error "Both arguments to ? must be non-negative integers.")))))

(defmethod generator-of ((varray vader-deal) &optional indexer params)
  (get-promised
   (varray-indexer varray)
   (let* ((rngs (vads-rng varray))
          (base-indexer (generator-of (vader-base varray)))
          (count (if (not (functionp base-indexer))
                     base-indexer (funcall base-indexer 0)))
          (gen-name (getf (rest rngs) :rng))
          (generator (or (getf (rest rngs) gen-name)
                         (setf (getf (rest rngs) gen-name)
                               (if (eq :system gen-name)
                                   :system (random-state:make-generator gen-name)))))
          (arg-indexer (generator-of (vads-argument varray)))
          (length (if (not (functionp arg-indexer))
                      arg-indexer (funcall arg-indexer 0)))
          (vector (make-array count :element-type (etype-of varray))))
     
     (setf (vadeal-cached varray) vector)
     ;; (xdotimes vector (x (length vector))
     (dotimes (x (length vector))
       (setf (aref vector x) (+ x (vads-io varray))))

     ;; (print (list :gen count generator))
     
     (loop :for i :from count :downto 2
           :do (rotatef (aref vector (if (eq :system generator)
                                         (random i)
                                         (random-state:random-int generator 0 (1- i))))
                        (aref vector (1- i))))
     
     (lambda (index)
       ;; (print (list :ii))
       (aref (vadeal-cached varray) index)))))

;; (let ((sys (random-state:make-generator :mersenne-twister-64 50)))
;;   (lparallel::pdotimes (x 5)
;;     (let ((vector (make-array 1000 :element-type '(integer 0 1000))))
;;       (loop :for i :from 200 :downto 2
;;             :do (rotatef (aref vector (random-state:random-int sys 0 (1- i)))
;;                          (aref vector 3)))
;;       (print (length vector)))))

(defclass vader-without (varray-derived vad-with-argument vad-limitable)
  nil (:metaclass va-class)
  (:documentation "An array without given elements as from the [~ without] function."))

(defmethod etype-of ((varray vader-without))
  (type-in-common (etype-of (vader-base varray))
                  (etype-of (vads-argument varray))))

(defmethod shape-of ((varray vader-without))
  (get-promised (varray-shape varray)
                (let ((this-indexer (generator-of varray)))
                  (declare (ignore this-indexer))
                  (shape-of (vader-content varray)))))

(defmethod generator-of ((varray vader-without) &optional indexer params)
  (get-promised
   (varray-indexer varray)
   (flet ((compare (o a)
            (funcall (if (and (characterp a) (characterp o))
                         #'char= (if (and (numberp a) (numberp o))
                                     #'= (lambda (a o) (declare (ignore a o)))))
                     o a)))
     (let ((derivative-count (when (getf params :shape-deriving)
                               (reduce #'* (getf params :shape-deriving))))
           (contents (render (vader-base varray)))
           (argument (render (vads-argument varray))))
       ;; (print (list :arg argument))
       (if (and (arrayp argument)
                (< 1 (rank-of argument)))
           (error "The left argument to [~~ without] must be a vector.")
           (let* ((included) (count 0)
                  (cont-vector (if (or (vectorp contents) (not (arrayp contents)))
                                   contents
                                   (make-array (size-of contents)
                                               :displaced-to contents
                                               :element-type (etype-of contents)))))
             ;; (print (list :cont contents cont-vector argument))
             (if (vectorp argument)
                 (loop :for element :across argument :while (or (not derivative-count)
                                                                (< count derivative-count))
                       :do (let ((include t))
                             (if (vectorp cont-vector)
                                 (loop :for ex :across cont-vector
                                       :do (when (compare ex element) (setq include nil)))
                                 (when (compare cont-vector element) (setq include nil)))
                             (when include (push element included)
                                   (incf count))))
                 (let ((include t))
                   (if (vectorp cont-vector)
                       (loop :for ex :across cont-vector
                             :do (when (compare ex argument) (setq include nil)))
                       (when (compare cont-vector argument) (setq include nil)))
                   (when include (push argument included)
                         (incf count))))
             ;; (print (list :inc included derivative-count))
             (setf (vader-content varray)
                   (make-array (length included) :element-type (etype-of argument)
                                                 :initial-contents (reverse included)))))
       (lambda (index) (aref (vader-content varray) index))))))

(defclass vader-umask (varray-derived vad-limitable)
  nil (:metaclass va-class)
  (:documentation "The unique mask of an array as from the [≠ unique mask] function."))

(defmethod etype-of ((varray vader-umask))
  (declare (ignore varray))
  'bit)

(defmethod prototype-of ((varray vader-umask))
  (declare (ignore varray))
  0)

(defmethod shape-of ((varray vader-umask))
  (get-promised (varray-shape varray)
                (let ((this-indexer (generator-of varray)))
                  (declare (ignore this-indexer))
                  (shape-of (vader-content varray)))))

(defmethod generator-of ((varray vader-umask) &optional indexer params)
  (get-promised
   (varray-indexer varray)
   (let* ((derivative-count (if (getf params :shape-deriving)
                                   (reduce #'* (getf params :shape-deriving))))
             (base-shape (shape-of (vader-base varray)))
             (contents (render (vader-base varray)))
             (output-length (if (not derivative-count)
                                (first base-shape)
                                (min derivative-count (first base-shape))))
             (increment (reduce #'* (rest base-shape)))
             (displaced (if (< 1 (array-rank contents))
                            (make-array (rest base-shape) :displaced-to contents
                                                          :element-type (etype-of contents))))
             (uniques)
             (output (make-array output-length :element-type 'bit :initial-element 1)))
     (dotimes (x output-length)
       (if (and displaced (< 0 x))
           (setq displaced (make-array (rest base-shape)
                                       :displaced-to contents 
                                       :element-type (etype-of contents) 
                                       :displaced-index-offset (* x increment))))
       (if (member (or displaced (aref contents x)) uniques :test #'array-compare)
           (setf (aref output x) 0)
           (push (if displaced (make-array (rest base-shape)
                                           :displaced-to contents
                                           :element-type (etype-of contents)
                                           :displaced-index-offset (* x increment))
                     (aref contents x))
                 uniques)))
     (setf (vader-content varray) output)
     (lambda (index) (aref (vader-content varray) index)))))

(defclass vader-inverse-where (varray-derived vad-with-argument vad-with-io)
  nil (:metaclass va-class)
  (:documentation "An inverted product of the [⍸ where] function."))

(defmethod prototype-of ((varray vader-inverse-where))
  (declare (ignore varray))
  0)

(defmethod etype-of ((varray vader-inverse-where))
  (declare (ignore varray))
  'bit)

(defmethod shape-of ((varray vader-inverse-where))
  (get-promised
   (varray-shape varray)
   (let ((base (vader-base varray)))
     (typecase base
       (vapri-integer-progression
        (if (and (= 1 (vapip-repeat base))
                 (= 1 (vapip-factor base))
                 (= (vads-io varray) (vapip-origin base)))
            (list (+ (vads-io varray) (vapip-number varray)))
            (error "Attempted to invoke inverse [⍸ where] on an altered integer progression vector.")))
       (vapri-coordinate-identity (vapci-shape base))
       (vader-where (shape-of (vader-base base)))
       (array (let* ((rank (if (not (vectorp (aref base 0)))
                               1 (length (aref base 0))))
                     (dims (make-array rank :initial-element 0)))
                (if (= 1 rank)
                    (list (aref base (1- (length base))))
                    (progn (dotimes (i (array-total-size base))
                             (if (= rank (length (row-major-aref base i)))
                                 (loop :for b :across (row-major-aref base i) :for i :from 0
                                       :when (not (and (integerp b) (or (zerop b) (plusp b))))
                                         :do (error "t")
                                       :when (> b (aref dims i)) :do (setf (aref dims i) b))
                                 (error "This array contains inconsistent coordinate vectors and cannot be an argument to the inverse [⍸ where] function.")))
                           (coerce dims 'list)))))
       (t (error "The inverse [⍸ where] function cannot be applied to this object."))))))

(defmethod generator-of ((varray vader-inverse-where) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let ((base (vader-base varray)))
     (typecase base
       (vapri-integer-progression (lambda (index) (declare (ignore index)) 1))
       (vapri-coordinate-identity (lambda (index) (declare (ignore index)) 1))
       (vader-where
        (setf (vader-content varray)
              (make-array (shape-of varray) :element-type 'bit :initial-element 0))
        (loop :for i :across (vader-content base)
              :do (setf (row-major-aref (vader-content varray) i) 1))
        (lambda (index) (row-major-aref (vader-content base) index)))
       (array
        (setf (vader-content varray)
              (make-array (shape-of varray) :element-type 'bit :initial-element 0))
        (dotimes (i (array-total-size base))
          (setf (varef (vader-content varray)
                       (row-major-aref base i)
                       (vads-io varray))
                1))
        (lambda (index) (row-major-aref (vader-content varray) index)))))))

(defclass vader-index (varray-derived vad-with-argument vad-with-io)
  ((%base-cache :accessor vaix-base-cache
                :initform nil
                :initarg :base-cache
                :documentation "Cached items to search in array.")
   (%set :accessor vaix-set
         :initform nil
         :initarg :set
         :documentation "Cached items to search in array."))
  (:metaclass va-class)
  (:documentation "An indexed array as from the [⍳ index of] function."))

(defmethod prototype-of ((varray vader-index))
  (declare (ignore varray))
  0)

(defmethod etype-of ((varray vader-index))
  (list 'integer 0 (max 255 (1+ (first (last (shape-of (vads-argument varray))))))))

(defmethod shape-of ((varray vader-index))
  (get-promised (varray-shape varray)
                (butlast (shape-of (vader-base varray))
                         (1- (length (shape-of (vads-argument varray)))))))

(defmethod generator-of ((varray vader-index) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let* ((base-indexer (base-indexer-of varray params))
          (argument (or (vaix-set varray)
                        (setf (vaix-set varray) (render (vads-argument varray)))))
          (arg-cell-size (reduce #'* (rest (shape-of argument))))
          (major-cells-count (if (/= 1 arg-cell-size) (first (shape-of argument))))
          (base (if major-cells-count (or (vaix-base-cache varray)
                                          (setf (vaix-base-cache varray)
                                                (render (vader-base varray)))))))
     (if major-cells-count ;; if comparing sub-arrays
         (lambda (index)
           (let ((base-segment (make-array (rest (shape-of argument))
                                           :element-type (etype-of base) :displaced-to base
                                           :displaced-index-offset (* index arg-cell-size))))
             ;; (print (list :ba base-segment))
             (loop :for a :below major-cells-count
                   :while (not (varray-compare base-segment
                                               (make-array (rest (shape-of argument))
                                                           :displaced-to argument
                                                           :element-type (etype-of argument)
                                                           :displaced-index-offset (* arg-cell-size a))))
                   :counting a :into asum :finally (return (+ asum (vads-io varray))))))
         ;; if comparing individual vector elements
         (lambda (index)
           (let ((base-index (if (not (functionp base-indexer))
                                 base-indexer (funcall base-indexer index))))
             ;; (print (list :ind (vads-io varray) ; base-index (render base-index)
             ;;              argument))
             ;; (setf april::it base-index)
             ;; (sleep 0.001)
             ;; (princ base-index) (princ " ")
             (loop :for a :across argument :while (not (varray-compare a base-index))
                   ;; :do (print (list :lll a))
                   :counting a :into asum :finally (return (+ asum (vads-io varray))))))))))

(defclass vader-shape (varray-derived)
  nil (:metaclass va-class)
  (:documentation "The shape of an array as from the [⍴ shape] function."))

(defmethod prototype-of ((varray vader-shape))
  (declare (ignore varray))
  0)

(defmethod etype-of ((varray vader-shape))
  (or (apply #'type-in-common (loop :for dimension :in (shape-of (vader-base varray))
                                    :collect (assign-element-type dimension)))
      t))

(defmethod shape-of ((varray vader-shape))
  "The shape of a reshaped array is simply its argument."
  (get-promised (varray-shape varray) (list (length (shape-of (vader-base varray))))))

(defmethod generator-of ((varray vader-shape) &optional indexers params)
  "Index a reshaped array."
  (let ((shape (coerce (shape-of (vader-base varray)) 'vector)))
    (lambda (index) (aref shape index))))

(defclass vader-reshape (varray-derived vad-with-argument vad-reindexing)
  nil (:metaclass va-class)
  (:documentation "A reshaped array as from the [⍴ reshape] function."))

(defmethod prototype-of ((varray vader-reshape))
  (prototype-of (vader-base varray)))

(defmethod shape-of ((varray vader-reshape))
  "The shape of a reshaped array is simply its argument."
  (get-promised (varray-shape varray)
                (let ((arg (setf (vads-argument varray)
                                 (render (vads-argument varray)))))
                  (if (typep arg 'sequence)
                      (coerce arg 'list)
                      (list arg)))))

(defmethod indexer-of ((varray vader-reshape) &optional params)
   "Index a reshaped array."
  ;; (declare (ignore params) (optimize (speed 3) (safety 0)))
  (get-promised
   (varray-indexer varray)
   (let* ((base-indexer (base-indexer-of varray params))
          (input-size (the (unsigned-byte 62)
                           (max (the bit 1)
                                (the fixnum (size-of (vader-base varray))))))
          (output-shape (shape-of varray))
          (output-size (the (unsigned-byte 62) (reduce #'* output-shape))))
     ;; (print (list :pa params))
     (if (not (functionp base-indexer))
         (lambda (index) (declare (ignore index)) base-indexer)
         (if output-shape (if (<= output-size input-size)
                              #'identity ;; (lambda (index) (mod index input-size))
                              (intraverser (:typekey t)
                                (:integer
                                 (the +function-type+ (lambda (index) (mod index input-size))))
                                ;; (:encoded
                                ;;  (the +function-type+ (lambda (i) ... )))
                                ))
             (lambda (index) (declare (ignore index)) 0))))))

(defmethod generator-of ((varray vader-reshape) &optional indexers params)
  (let ((output-size (size-of varray)))
    (if (zerop output-size)
        (let ((prototype (prototype-of varray)))
          (lambda (index) (declare (ignore index)) prototype))
        (if (zerop (size-of (vader-base varray)))
            (prototype-of (vader-base varray))
            (let ((indexer (indexer-of varray params)))
              (generator-of (vader-base varray) (cons indexer indexers)))))))

(defclass vader-depth (varray-derived)
  nil (:metaclass va-class)
  (:documentation "The first dimension of an array as from the [≢ first dimension] function."))

(defmethod prototype-of ((varray vader-depth))
  (declare (ignore varray))
  0)

(defmethod etype-of ((varray vader-depth))
  'fixnum)

(defmethod shape-of ((varray vader-depth))
  "The shape of a reshaped array is simply its argument."
  (declare (ignore varray))
  nil)

(defun varray-depth (input &optional layer (uniform t) possible-depth)
  "Find the maximum depth of nested arrays within an array."
  (let ((shape (shape-of input))
        (indexer (generator-of input)))
    (flet ((vap (item) (or (arrayp item) (varrayp item))))
      (if (not (functionp indexer))
          1 (let* ((first-level (not layer))
                   (layer (if layer layer 1))
                   (new-layer layer))
              (dotimes (i (size-of input))
                (let ((item (funcall indexer i)))
                  (if (vap item)
                      (multiple-value-bind (next-layer new-uniform new-possible-depth)
                          (varray-depth item (1+ layer) uniform possible-depth)
                        (setq new-layer (max new-layer next-layer)
                              uniform new-uniform
                              possible-depth new-possible-depth))
                      (if (not possible-depth) (setq possible-depth new-layer)
                          (if (/= layer possible-depth) (setq uniform nil))))))
              (values (funcall (if (and first-level (not uniform)) #'- #'identity)
                               new-layer)
                      uniform possible-depth))))))

(defmethod indexer-of ((varray vader-depth) &optional params)
  "Index an array depth value."
  (get-promised
   (varray-indexer varray)
   (let ((shape (or (shape-of (vader-base varray))
                    (varrayp (vader-base varray))
                    (arrayp (vader-base varray)))))
     (if shape (lambda (index) (declare (ignore index))
                 (varray-depth (vader-base varray)))
         (lambda (index) (declare (ignore index)) 0)))))

(defclass vader-first-dim (varray-derived)
  nil (:metaclass va-class)
  (:documentation "The first dimension of an array as from the [≢ first dimension] function."))

(defmethod prototype-of ((varray vader-first-dim))
  (declare (ignore varray))
  0)

(defmethod etype-of ((varray vader-first-dim))
  (list 'integer 0 (first (shape-of (vader-base varray)))))

(defmethod shape-of ((varray vader-first-dim))
  "The shape of a reshaped array is simply its argument."
  (declare (ignore varray))
  nil)

(defmethod generator-of ((varray vader-first-dim) &optional indexers params)
  "Index a reshaped array."
  (get-promised
   (varray-indexer varray)
   (let ((dimension (first (shape-of (vader-base varray)))))
     (lambda (index) (or dimension 1)))))

(defun varray-compare (item1 item2 &optional comparison-tolerance)
  "Perform a deep comparison of two APL arrays, which may be multidimensional or nested."
  (flet ((vap (item) (or (arrayp item) (varrayp item))))
    (let ((shape1 (shape-of item1))
          (shape2 (shape-of item2))
          (rank1 (rank-of item1))
          (rank2 (rank-of item2))
          (indexer1 (generator-of item1))
          (indexer2 (generator-of item2)))
      ;; (print (list :sh shape1 shape2 indexer1 indexer2))
      (if shape1 (and shape2 (= rank1 rank2)
                      (loop :for d1 :in shape1 :for d2 :in shape2 :always (= d1 d2))
                      ;; compared arrays must be either character or non-character to match
                      (or (< 0 (size-of item1)) ;; 0-size arrays must be of same type, as for ⍬≡''
                          (if (not (member (etype-of item1) '(character base-char)))
                              (not (member (etype-of item2) '(character base-char)))
                              (member (etype-of item2) '(character base-char))))
                      (loop :for i :below (size-of item1)
                            :always (varray-compare (funcall indexer1 i)
                                                    (funcall indexer2 i))))
          (when (not shape2)
            ;; (let ((indexer1 (if (functionp indexer1) (funcall indexer1 0) indexer1))
            ;;       (indexer2 (if (functionp indexer2) (funcall indexer2 0) indexer2)))
            (or (and comparison-tolerance (floatp indexer1) (floatp indexer2)
                     (> comparison-tolerance (abs (- indexer1 indexer2))))
                (and (numberp indexer1)
                     (numberp indexer2)
                     (= item1 indexer2))
                (and (characterp indexer1)
                     (characterp indexer2)
                     (char= item1 indexer2))))))))

(defclass vader-compare (varray-derived vad-with-ct vad-invertable)
  nil (:metaclass va-class)
  (:documentation "The first dimension of an array as from the [≢ first dimension] function."))

(defmethod prototype-of ((varray vader-compare))
  (declare (ignore varray))
  0)

(defmethod etype-of ((varray vader-compare))
  (declare (ignore varray))
  'bit)

(defmethod shape-of ((varray vader-compare))
  "The shape of a comparison result is always nil."
  (declare (ignore varray))
  nil)

(defmethod generator-of ((varray vader-compare) &optional indexers params)
  "Index a reshaped array."
  (let ((base-indexer (base-indexer-of varray params)))
    (lambda (index) (if (funcall (if (vads-inverse varray) #'not #'identity)
                                 (varray-compare (funcall base-indexer 0)
                                                 (funcall base-indexer 1)
                                                 (vads-ct varray)))
                        1 0))))

(defclass vader-enlist (varray-derived vad-limitable)
  nil (:metaclass va-class)
  (:documentation "An array decomposed into a vector as from the [∊ enlist] function."))

(defmethod shape-of ((varray vader-enlist))
  (get-promised (varray-shape varray)
                (let ((this-indexer (generator-of varray)))
                  (declare (ignore this-indexer))
                  (if (eq :raveled (vader-content varray))
                      (list (size-of (vader-base varray)))
                      (shape-of (vader-content varray))))))

(defmethod generator-of ((varray vader-enlist) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let ((base-indexer (generator-of (vader-base varray))))
     (if (not (vader-content varray))
         (let ((first-item (if (not (functionp base-indexer))
                               base-indexer (funcall base-indexer 0))))
           (if (and (not (shape-of (vader-base varray)))
                    (not (or (arrayp first-item)
                             (varrayp first-item))))
               (setf (vader-content varray)
                     (make-array 1 :initial-element first-item :element-type (etype-of first-item)))
               (if (not (eq t (etype-of (vader-base varray))))
                   (setf (vader-content varray) :raveled)
                   (let ((index 0) (length 0)
                         (input (render (vader-base varray)))
                         (derivative-count (if (getf params :shape-deriving)
                                               (reduce #'* (getf params :shape-deriving)))))
                     (labels ((measure-array (in)
                                (loop :for i :below (array-total-size in)
                                      :while (or (not derivative-count)
                                                 (< length derivative-count))
                                      :do (if (arrayp (row-major-aref in i))
                                              (measure-array (row-major-aref in i))
                                              (incf length))))
                              (copy-contents (in destination)
                                (loop :for i :below (array-total-size in)
                                      :while (or (not derivative-count)
                                                 (< index derivative-count))
                                      :do (if (arrayp (row-major-aref in i))
                                              (copy-contents (row-major-aref in i) destination)
                                              (progn (setf (row-major-aref destination index)
                                                           (row-major-aref in i))
                                                     (incf index))))))
                       (measure-array input)
                       (let ((output (make-array length)))
                         (copy-contents input output)
                         (setf (vader-content varray) output))))))))
     (lambda (index)
       (if (eq :raveled (vader-content varray))
           (funcall base-indexer index)
           (aref (vader-content varray) index))))))

(defclass vader-membership (varray-derived vad-with-argument)
  ((%to-search :accessor vamem-to-search
               :initform nil
               :initarg :to-search
               :documentation "Set of elements to be checked for membership in array."))
  (:metaclass va-class)
  (:documentation "A membership array as from the [∊ membership] function."))

(defmethod etype-of ((varray vader-membership))
  (declare (ignore varray))
  'bit)

(defmethod generator-of ((varray vader-membership) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let ((base-indexer (base-indexer-of varray)))
     (labels ((compare (item1 item2)
                (if (and (characterp item1) (characterp item2))
                    (char= item1 item2)
                    (if (and (numberp item1) (numberp item2))
                        (= item1 item2)
                        (if (and (arrayp item1) (arrayp item2))
                            (array-compare item1 item2))))))
       
       (if (not (vamem-to-search varray))
           (let ((argument (render (vads-argument varray))))
             ;; TODO: possible to optimize this?
             (if (functionp base-indexer)
                 (setf (vamem-to-search varray) argument)
                 (if (not (arrayp argument))
                     (setf (vamem-to-search varray)
                           (if (compare argument base-indexer) 1 0))
                     (setf (vamem-to-search varray)
                           (if (not (loop :for i :below (array-total-size argument)
                                          :never (compare base-indexer (row-major-aref argument i))))
                               1 0))))))
       (lambda (index)
         (if (arrayp (vamem-to-search varray))
             (let ((found))
               (loop :for ix :below (size-of (vamem-to-search varray)) :while (not found)
                     :do (setq found (compare (funcall base-indexer index)
                                              (row-major-aref (vamem-to-search varray) ix))))
               (if found 1 0))
             (if (functionp base-indexer)
                 (if (compare (vamem-to-search varray)
                              (funcall base-indexer index))
                     1 0)
                 (vamem-to-search varray))))))))

(defclass vader-find (varray-derived vad-with-argument)
  ((%pattern :accessor vafind-pattern
             :initform nil
             :initarg :pattern
             :documentation "Pattern to find.")
   (%cached :accessor vafind-cached
            :initform nil
            :initarg :cached
            :documentation "Cached data to search."))
  (:metaclass va-class)
  (:documentation "An array of search matches as from the [⍷ find] function."))

(defmethod etype-of ((varray vader-find))
  (declare (ignore varray))
  'bit)

(defmethod prototype-of ((varray vader-find))
  (declare (ignore varray))
  0)

(defmethod generator-of ((varray vader-find) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let* ((target (or (vafind-pattern varray)
                      (setf (vafind-pattern varray)
                            (render (vads-argument varray)))))
          (is-target-array (arrayp target))
          (target-shape (shape-of target))
          (target-rank (length target-shape))
          (target-size (size-of target))
          (target-factors (get-dimensional-factors target-shape t))
          (base (if is-target-array
                    (or (vafind-cached varray)
                        (setf (vafind-cached varray)
                              (render (vader-base varray))))))
          (base-indexer (if (not is-target-array)
                            (generator-of (vader-base varray))))
          (base-shape (shape-of base))
          (base-factors (get-dimensional-factors base-shape t))
          (rank-delta (- (length base-factors) target-rank))
          (target-head (if (not is-target-array)
                           target (row-major-aref target 0))))
     
     (lambda (index)
       (if is-target-array
           (if (not (array-compare target-head (row-major-aref base index)))
               0 (let ((remaining index) (valid t) (shape-ref target-shape))
                   ;; determine whether the target array fits within the dimensions
                   ;; of the searched array from the row-major starting point
                   (loop :for f :across base-factors :for b :in base-shape :for ix :from 0
                         :do (multiple-value-bind (item remainder) (floor remaining f)
                               (if (>= ix rank-delta)
                                   (if (< b (+ item (first shape-ref)))
                                       (setf valid nil)
                                       (setf shape-ref (rest shape-ref))))
                               (setf remaining remainder)))
                   (if (not valid)
                       0 (progn
                           (loop :for it :below target-size :while valid
                                 :do (let ((tremaining it) (bremaining index)
                                           (base-index 0) (tfactors target-factors))
                                       (loop :for b :across base-factors :for ix :from 0
                                             :do (multiple-value-bind (titem tremainder)
                                                     (if (>= ix rank-delta)
                                                         (floor tremaining
                                                                (aref tfactors (- ix rank-delta))))
                                                   (multiple-value-bind (bitem bremainder)
                                                       (floor bremaining b)
                                                     (incf base-index (* b (+ bitem (or titem 0))))
                                                     (setf tremaining (or tremainder it)
                                                           bremaining bremainder))))
                                       (setf valid (array-compare (row-major-aref target it)
                                                                  (row-major-aref base base-index)))))
                           (if valid 1 0)))))
           (if (array-compare target-head (if (not (functionp base-indexer))
                                              base-indexer (funcall base-indexer index)))
               1 0))))))

(defclass vader-where (varray-derived vad-with-io vad-with-dfactors vad-limitable)
  nil (:metaclass va-class)
  (:documentation "The coordinates of array elements equal to 1 as from the [⍸ where] function."))

(defmethod etype-of ((varray vader-where))
  (declare (ignore varray))
  t)

(defmethod prototype-of ((varray vader-where))
  "Prototype is an array of zeroes with length equal to the base array's rank."
  (make-array (rank-of (vader-base varray)) :element-type 'bit :initial-element 0))

(defmethod shape-of ((varray vader-where))
  (get-promised (varray-shape varray)
                (let ((this-indexer (generator-of varray))
                      (base-indexer (generator-of (vader-base varray))))
                  (declare (ignore this-indexer))
                  (let ((shape (or (shape-of (vader-content varray))
                                   (list (if (and (not (functionp base-indexer))
                                                  (integerp base-indexer)
                                                  (= 1 base-indexer))
                                             1 0)))))
                    (when (not (vads-dfactors varray))
                      (setf (vads-dfactors varray)
                            (get-dimensional-factors (shape-of (vader-base varray)) t)))
                    (when (second (shape-of (vader-base varray)))
                      ;; set array to subrender if it will return coordinate vector objects
                      (setf (vads-subrendering varray) t))
                    shape))))

(defmethod generator-of ((varray vader-where) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let* ((base-shape (shape-of (vader-base varray)))
          (base-rank (length base-shape))
          (maximum (when base-shape (reduce #'max base-shape))))
     (if (and maximum (not (vader-content varray)))
         (let* ((derivative-count (if (getf params :shape-deriving)
                                      (reduce #'* (getf params :shape-deriving))))
                (base-indexer (generator-of (vader-base varray)))
                (output-length (if derivative-count (min derivative-count (first base-shape))
                                   (first base-shape)))
                (indices) (match-count 0))
           (loop :for i :below (size-of (vader-base varray))
                 :while (or (not derivative-count) (< match-count derivative-count))
                 :do (let ((this-item (funcall base-indexer i)))
                       (if (and (integerp this-item) (= 1 this-item))
                           (progn (push i indices)
                                  (incf match-count)))))
           (let ((output (make-array match-count
                                     :element-type (list 'integer 0 (size-of (vader-base varray))))))
             (loop :for in :in indices :for ix :from (1- match-count) :downto 0
                   :do (setf (aref output ix) in))
             (setf (vader-content varray) output))))
     (lambda (index)
       (if (zerop base-rank) (make-array 0)
           (if (= 1 base-rank)
               (+ (vads-io varray) (aref (vader-content varray) index))
               (make-instance 'vapri-coordinate-vector
                              :reference varray :index (aref (vader-content varray) index))))))))

(defclass vader-interval-index (varray-derived vad-with-io vad-with-argument)
  nil (:metaclass va-class)
  (:documentation "Interval indices of value(s) as from the [⍸ interval index] function."))

(defmethod etype-of ((varray vader-interval-index))
  (list 'integer 0 (if (not (shape-of (vader-base varray)))
                       1 (+ (vads-io varray)
                            (reduce #'max (shape-of (vader-base varray)))))))

(defmethod prototype-of ((varray vader-interval-index))
  (declare (ignore varray))
  0)

(defmethod shape-of ((varray vader-interval-index))
  (let ((arg-rank (length (shape-of (vads-argument varray))))
        (base-shape (shape-of (vader-base varray))))
    (if (not base-shape)
        nil (butlast base-shape (1- arg-rank)))))

(defmethod generator-of ((varray vader-interval-index) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let* ((base-indexer (generator-of (vader-base varray)))
          (argument (render (vads-argument varray)))
          (arg-shape (shape-of argument))
          (base-rendered (if (second arg-shape)
                             (render (vader-base varray))))
          (arg-indexer (generator-of argument))
          (base-increment (if (second arg-shape)
                              (/ (reduce #'* (shape-of base-rendered))
                                 (reduce #'* (shape-of varray)))))
          (arg-span (if (second arg-shape)
                        (/ (size-of argument) base-increment))))
     (lambda (index)
       (if (not (second arg-shape))
           (let ((count 0)
                 (value (if (not (functionp base-indexer))
                            base-indexer (funcall base-indexer index))))
             (loop :for item :across argument :while (funcall (alpha-compare #'>) value item)
                   :do (incf count))
             count)
           (let ((count 0)
                 (sub-base (make-array base-increment
                                       :element-type (etype-of base-rendered)
                                       :displaced-to base-rendered
                                       :displaced-index-offset (* index base-increment))))
             (loop :for ix :below arg-span
                   :while (vector-grade
                           (alpha-compare #'<)
                           (make-array base-increment
                                       :displaced-to argument :element-type (etype-of argument)
                                       :displaced-index-offset (* ix base-increment))
                           sub-base)
                   :do (incf count))
             count))))))

;; TODO: is subrendering needed here? Check render function
(defclass vader-pare (vader-reshape vad-on-axis vad-with-io)
  nil (:metaclass va-class)
  (:documentation "An array with a reduced shape as from the [, catenate] or [⍪ table] functions."))

(defmethod shape-of ((varray vader-pare))
  "The shape of a reshaped array is simply its argument."
  (get-promised
   (varray-shape varray)
   (let ((base-shape (shape-of (vader-base varray)))
         (axis (setf (vads-axis varray)
                     (if (and (vads-axis varray)
                              (not (keywordp (vads-axis varray))))
                         (funcall (lambda (ax)
                                    (if (numberp ax)
                                        (- ax (vads-io varray))
                                        (if (zerop (size-of ax))
                                            ax (loop :for a :across ax
                                                     :collect (- a (vads-io varray))))))
                                  (disclose (render (vads-axis varray))))
                         (vads-axis varray)))))
     (if (not axis) (list (reduce #'* base-shape))
         (if (eq :tabulate axis)
             (list (or (first base-shape) 1)
                   (reduce #'* (rest base-shape)))
             (if (numberp axis)
                 (if (integerp axis)
                     base-shape (let ((output) (added)
                                      (real-axis (ceiling axis)))
                                  (loop :for s :in base-shape :for ix :from 0
                                        :do (if (= ix real-axis) (setf added (push 1 output)))
                                            (push s output))
                                  (if (not added) (push 1 output))
                                  (reverse output)))
                 (if (zerop (size-of axis))
                     (append base-shape (list 1))
                     (let ((output) (reducing) (prev-axis)
                           (axis (copy-list axis))
                           (shape base-shape))
                       (loop :for s :in base-shape :for ix :from 0
                             :do (if reducing
                                     (if axis (if (/= (first axis) (1+ prev-axis))
                                                  (error "Invalid axis for [, ravel].")
                                                  (if (= ix (first axis))
                                                      (setf (first output)
                                                            (* s (first output))
                                                            prev-axis (first axis)
                                                            axis (rest axis))))
                                         (progn (push s output)
                                                (setf reducing nil)))
                                     (progn (push s output)
                                            (if (and axis (= ix (first axis)))
                                                (setf reducing t
                                                      prev-axis (first axis)
                                                      axis (rest axis))))))
                       (reverse output)))))))))

(defclass vader-catenate (varray-derived vad-on-axis vad-with-argument vad-with-io)
  ((%laminating :accessor vacat-laminating
                :initform nil
                :initarg :laminating
                :documentation "Whether this catenation laminates arrays."))
  (:metaclass va-class)
  (:documentation "A catenated array as from the [, catenate] function."))

(defmethod etype-of ((varray vader-catenate))
  (let ((base-indexer (generator-of (vader-base varray)))
        (base-size (size-of (vader-base varray))))
    (apply #'type-in-common (loop :for i :below base-size
                                  :when (< 0 (size-of (funcall base-indexer i)))
                                    :collect (etype-of (funcall base-indexer i))))))

;; (defmethod prototype-of ((varray vader-catenate))
;;   ;; (prototype-of (aref (vader-base varray) 0))
;;   ;; (print (list :va1 (aref (vader-base varray) 0)))
;;   (print (call-next-method))
;;   )

(defmethod prototype-of ((varray vader-catenate))
  (let ((base-indexer (generator-of (vader-base varray))))
    (prototype-of (funcall base-indexer 0))))

(defmethod shape-of ((varray vader-catenate))
  (get-promised
   (varray-shape varray)
   (let* ((ref-shape) (uneven)
          (base-indexer (generator-of (vader-base varray)))
          (base-size (size-of (vader-base varray)))
          (each-shape (loop ;; :for a :across (vader-base varray)
                            :for i :below base-size
                            :collect (let* ((a (funcall base-indexer i))
                                            (shape (shape-of a)))
                                       ;; (shape-of) must be called before checking whether an
                                       ;; array subrenders since (shape-of) determines whether a
                                       ;; catenated array subrenders
                                       (if (or (subrendering-p a)
                                               (subrendering-base a))
                                           (setf (vads-subrendering varray) t))
                                       (if (or (varrayp a) (arrayp a))
                                           shape))))
          (max-rank (reduce #'max (mapcar #'length each-shape)))
          (axis (disclose-unitary (render (vads-axis varray))))
          (axis (if (eq :last axis)
                        (max 0 (1- max-rank))
                        (- axis (vads-io varray))))
          (to-laminate (setf (vacat-laminating varray)
                             (or (typep axis 'ratio)
                                 (and (floatp axis)
                                      (< double-float-epsilon (nth-value 1 (floor axis)))))))
          (axis (setf (vads-axis varray) (ceiling axis))))
     (if to-laminate
         (loop :for shape :in each-shape
               :do (if shape (if ref-shape
                                 (if (not (and (= (length shape)
                                                  (length ref-shape))
                                               (loop :for r :in ref-shape :for a :in shape
                                                     :always (= a r))))
                                     (error "Mismatched array dimensions for laminate operation."))
                                 (setf ref-shape shape))))
         (loop :for shape :in each-shape
               :do (if (and shape (< (length ref-shape) (length shape)))
                       (if (or (not ref-shape)
                               (and (not uneven)
                                    (setf uneven (= 1 (- (length shape)
                                                         (length ref-shape))))))
                           (if (destructuring-bind (longer-dims shorter-dims)
                                   (funcall (if (>= (length ref-shape)
                                                    (length shape))
                                                #'identity #'reverse)
                                            (list ref-shape shape))
                                 (or (not shorter-dims)
                                     (loop :for ld :in longer-dims :for ax :from 0
                                           :always (if (< ax axis)
                                                       (= ld (nth ax shorter-dims))
                                                       (if (= ax axis)
                                                           t (= ld (nth (1- ax) shorter-dims)))))))
                               (setf ref-shape shape)
                               (error "Mismatched array dimensions."))
                           (error "Catenated arrays must be at most one rank apart."))
                       (if (and shape (not (loop :for d :in shape
                                                 :for s :in ref-shape :for dx :from 0
                                                 :always (or (= d s) (= dx axis)))))
                           (error "Mismatched array dimensions.")))))

     ;; if all elements to be catenated are scalar as for 1,2,
     ;; the output length is the same as the input length
     (if (not ref-shape) (setf ref-shape (list ;; (length (vader-base varray))
                                               base-size)))
     
     (if to-laminate
         (if (zerop max-rank) ;; the ref-shape can be left as is if all elements are scalar
             ref-shape (loop :for dx :below (1+ (length ref-shape))
                             :collect (if (< dx axis) (nth dx ref-shape)
                                          (if (= dx axis) base-size ; (length (vader-base varray))
                                              (nth (1- dx) ref-shape)))))
         (loop :for d :in ref-shape :for dx :from 0
               :collect (if (/= dx axis)
                            d (loop :for shape :in each-shape
                                    :summing (if (or (not shape)
                                                     (/= (length shape)
                                                         (length ref-shape)))
                                                 1 (nth axis shape)))))))))

(defmethod generator-of ((varray vader-catenate) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let* ((out-shape (shape-of varray))
          (to-laminate (vacat-laminating varray))
          (axis (disclose-unitary (vads-axis varray)))
          (ofactors (get-dimensional-factors out-shape t))
          (base-indexer (base-indexer-of varray params))
          (base-size (size-of (vader-base varray)))
          (each-shape (loop :for i :below base-size ; :for a :across (vader-base varray)
                            :collect (let ((a (funcall base-indexer i)))
                                       (if (or (varrayp a) (arrayp a))
                                           (shape-of a)))))
          (increments (loop :for shape :in each-shape
                            :summing (if (or (not shape)
                                             (< (length shape) (length out-shape)))
                                         1 (nth (max 0 axis) shape))
                              :into total :collect total))
          ;; (max 0 axis) is needed because otherwise the below can have bugs. TODO: figure out why
          ;; (april (with (:space tree-lib-space))
          ;;       "↓disp 4 3⍴(tree 12↑⎕a){fmt ⍶ rem foldl ⍵}¨,\' HIJDEFGKBCLA'")
          (indexers (make-array base-size ; (length (vader-base varray))
                                :initial-contents (loop ; :for a :across (vader-base varray)
                                                        :for i :below base-size 
                                                        :collect (generator-of (funcall base-indexer i))))) ;a
          (ifactors (make-array base-size ; (length (vader-base varray))
                                :initial-contents (loop ;; :for a :across (vader-base varray)
                                                        :for i :below base-size 
                                                        :collect (let ((a (funcall base-indexer i)))
                                                                   (if (or (arrayp a) (varrayp a))
                                                                       (reverse (get-dimensional-factors
                                                                                 (shape-of a)))))))))
     
     (lambda (orig-index)
       (let ((remaining orig-index) (sum 0) (sub-indices) (array-index 0)
             (axis-offset (abs (- axis (1- (length (shape-of varray))))))
             (row-major-index) (source-array))
         (loop :for ofactor :across ofactors :for fx :from 0
               :do (multiple-value-bind (index remainder) (floor remaining ofactor)
                     (setf remaining remainder)
                     ;; (print (list :in index remainder orig-index))
                     (push (if (/= fx axis)
                               index (loop :for i :in increments :for ix :from 0
                                           :while (>= index i)
                                           :do (progn (setf sum i) (incf array-index))
                                           :finally (return (- index sum))))
                           sub-indices)))
         ;; (print (list :ai array-index ifactors sub-indices ofactors increments))
         (setf source-array (funcall base-indexer array-index) ;(aref (vader-base varray) array-index)
               row-major-index
               (if (or (arrayp source-array)
                       (varrayp source-array))
                   (if to-laminate
                       (loop :for si :in sub-indices :for ix :from 0
                             :summing (* si (or (nth (max 0 (if (< ix axis-offset)
                                                                ix (1- ix)))
                                                     (aref ifactors array-index))
                                                1))
                               :into rmi :finally (return rmi))
                       (loop :for si :in (loop :for si :in sub-indices :for sx :from 0
                                               :when (or (= (length out-shape)
                                                            (length (shape-of source-array)))
                                                         (/= sx axis-offset))
                                                 :collect si)
                             :for df :in (aref ifactors array-index)
                             :summing (* si df) :into rmi :finally (return rmi)))))
         ;; (print (list :rr row-major-index))
         (if (not (functionp (aref indexers array-index)))
             (disclose (aref indexers array-index))
             (let ((indexed (funcall (aref indexers array-index) row-major-index)))
               (if (not (subrendering-p indexed))
                   indexed (render indexed)))))))))

(defclass vader-mix (varray-derived vad-on-axis vad-with-io)
  ((%shape-indices :accessor vamix-shape-indices
                   :initform nil
                   :initarg :shape-indices
                   :documentation "Indices of shape dimensions.")
   (%cached-elements :accessor vamix-cached-elements
                     :initform nil
                     :initarg :cached-elements
                     :documentation "Cached mixed array elements."))
  (:metaclass va-class)
  (:documentation "A mixed array as from the [↑ mix] function."))

(defmethod etype-of ((varray vader-mix))
  (let ((base-indexer (base-indexer-of varray)))
    ;; (or (apply #'type-in-common (loop :for aix :below (size-of (vader-base varray))
    ;;                                   :when (and (functionp base-indexer)
    ;;                                              (< 0 (size-of (funcall base-indexer aix))))
    ;;                                     :collect (etype-of (funcall base-indexer aix))
    ;;                                   :when (not (functionp base-indexer))
    ;;                                     :collect (assign-element-type base-indexer)))
    ;;     t)
    ;; above is very slow
    t
    ))

(defmethod prototype-of ((varray vader-mix))
  (let ((base-indexer (base-indexer-of varray)))
    (prototype-of (or (vamix-cached-elements varray)
                      (if (not (functionp base-indexer))
                          base-indexer (funcall base-indexer 0))))))

(defmethod shape-of ((varray vader-mix))
  (get-promised
   (varray-shape varray)
   (let* ((axis (vads-axis varray))
          (base (vader-base varray))
          (base-shape (shape-of base))
          (base-indexer (generator-of base))
          (base-indexer (if (and (not (varrayp base-indexer))
                                 (not (or (not (arrayp base-indexer))
                                          (not (shape-of base-indexer)))))
                            base-indexer (generator-of base-indexer)))
          (max-rank 0) (each-shape))
     ;; (print (list :ba base-shape (render base) (shape-of base)))
     (cond
       ((and (not (functionp base-indexer))
             (not (arrayp base-indexer))
             (not (varrayp base-indexer)))
        ;; (print (list :bb base-indexer))
        base-shape) ;; handle the ↑⍬ case
       ;; ((not base-shape)
       ;;  ;; (print (list :bb base base-indexer))
       ;;  (let ((indexed (funcall base-indexer 0)))
       ;;    (loop :while (and (varrayp indexed) (or (not (shape-of indexed))
       ;;                                            (and (= 1 (length (shape-of indexed)))
       ;;                                                 (= 1 (first (shape-of indexed))))
       ;;                                            ))
       ;;          :do (setf indexed (funcall (generator-of indexed) 0)))
       ;;    (setf (vamix-cached-elements varray) indexed))
       ;;  ;; (print (list :vv (vamix-cached-elements varray) (shape-of (vamix-cached-elements varray))
       ;;  ;;              (render (vamix-cached-elements varray))))
       ;;  ;; (setf (vamix-cached-elements varray)
       ;;  ;;       (funcall base-indexer 0))
       ;;  ;; (print (list :va (vamix-cached-elements varray)))
       ;;  (shape-of (vamix-cached-elements varray)))
       ((not base-shape)
        ;; (print (list :bb base base-indexer))
        (setf (vamix-cached-elements varray) (funcall base-indexer 0))
        (shape-of (vamix-cached-elements varray)))
       (t (loop :for ix :below (reduce #'* base-shape)
                :do (let ((member (funcall base-indexer ix)))
                      (setf max-rank (max max-rank (length (shape-of member))))
                      (push (shape-of member) each-shape)))
          (let ((out-shape) (shape-indices)
                (max-shape (make-array max-rank :element-type 'fixnum :initial-element 0)))
            (loop :for shape :in each-shape
                  :do (loop :for d :in shape :for dx :from 0
                            :do (setf (aref max-shape dx)
                                      (max d (aref max-shape dx)))))

            (setf axis (setf (vads-axis varray)
                             (if (eq :last axis) (length base-shape)
                                 (ceiling (- axis (vads-io varray))))))
            ;; push the outer shape elements to the complete shape
            (loop :for odim :in base-shape :for ix :from 0
                  :do (when (= ix axis)
                        (loop :for ms :across max-shape :for mx :from 0
                              :do (push ms out-shape)
                                  (push (+ mx (length base-shape)) shape-indices)))
                      (push odim out-shape)
                      (push ix shape-indices))
            
            (when (= axis (length base-shape))
              ;; push the inner shape elements if for the last axis
              (loop :for ms :across max-shape :for mx :from 0
                    :do (push ms out-shape)
                        (push (+ mx (length base-shape)) shape-indices)))

            (setf (vamix-shape-indices varray) (reverse shape-indices))
            
            (reverse out-shape)))))))

(defmethod generator-of ((varray vader-mix) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let* ((oshape (shape-of varray))
          (ofactors (get-dimensional-factors oshape t))
          (oindexer (base-indexer-of varray params))
          (dim-indices (vamix-shape-indices varray))
          (orank (length (shape-of (vader-base varray))))
          (outer-shape (loop :for i :in dim-indices :for s :in oshape
                             :when (> orank i) :collect s))
          (inner-shape (loop :for i :in dim-indices :for s :in oshape
                             :when (<= orank i) :collect s))
          (inner-rank (length inner-shape))
          (iofactors (get-dimensional-factors outer-shape t)))
     ;; TODO: add logic to simply return the argument if it's an array containing no nested arrays
     (if (not oshape) ;; if the argument is a scalar
         (if (not (functionp oindexer)) ;; a scalar value like 5
             (lambda (i) (declare (ignore i)) (disclose oindexer))
             ;; TODO: change indexer-of for rank 0 arrays to obviate this
             (let* ((indexed (funcall oindexer 0))
                    (iindexer (generator-of indexed))
                    (sub-index (if (not (functionp iindexer))
                                   iindexer (funcall (generator-of indexed) 0))))
               (if (and (typep (vader-base varray) 'varray)
                        (not (shape-of (vader-base varray))))
                   (generator-of (vader-base varray))
                   (lambda (i) (declare (ignore i)) sub-index))))
         (if (not (shape-of (vader-base varray)))
             ;; pass through the indexer of enclosed arrays as for ↑⊂2 4
             (generator-of (vamix-cached-elements varray))
             (if (vamix-cached-elements varray)
                 (lambda (index) (row-major-aref (vamix-cached-elements varray) index))
                 (let* ((iarray (when (not (shape-of varray))
                                  (render (vader-base varray))))
                        (ishape (when iarray (copy-list (shape-of iarray))))
                        (iifactors (when iarray (get-dimensional-factors ishape)))
                        (prototype (prototype-of varray)))
                   (lambda (index)
                     (let ((remaining index) (row-major-index) (outer-indices) (inner-indices))
                       (loop :for ofactor :across ofactors :for di :in dim-indices :for fx :from 0
                             :do (multiple-value-bind (this-index remainder) (floor remaining ofactor)
                                   (setf remaining remainder)
                                   (if (> orank di) (push this-index outer-indices)
                                       (push this-index inner-indices))))

                       (let* ((inner-indices (reverse inner-indices))
                              (oindex (when (not iarray)
                                        (loop :for i :in (reverse outer-indices)
                                              :for f :across iofactors :summing (* i f))))
                              (iarray (or iarray (funcall oindexer oindex)))
                              (ishape (or ishape (copy-list (shape-of iarray))))
                              (iifactors (or iifactors (get-dimensional-factors ishape)))
                              (iindexer (generator-of iarray))
                              (irank (length ishape))
                              (doffset (- inner-rank irank))
                              (iindex 0))
                         (if (not (shape-of varray))
                             (when (zerop (reduce #'+ inner-indices)) iindexer)
                             (progn (loop :for i :in inner-indices :for ix :from 0 :while iindex
                                          :do (if (< ix doffset) (if (not (zerop i))
                                                                     (setf iindex nil))
                                                  (if (< i (first ishape))
                                                      (progn (incf iindex (* i (first iifactors)))
                                                             (setf ishape (rest ishape)
                                                                   iifactors (rest iifactors)))
                                                      (setf iindex nil))))
                                    (if (not iindex) prototype
                                        (if (not (functionp iindexer))
                                            iindexer (funcall iindexer iindex)))))))))))))))

(defclass vader-split (vad-subrendering varray-derived vad-on-axis vad-with-io vad-maybe-shapeless)
  nil (:metaclass va-class)
  (:documentation "A split array as from the [↓ split] function."))

(defmethod etype-of ((varray vader-split))
  "The [↓ split] function returns a nested array unless its argument is scalar."
  (let ((base (vader-base varray)))
    (if (or (arrayp base) (varrayp base))
        t (assign-element-type base))))

(defmethod shape-of ((varray vader-split))
  (get-promised
   (varray-shape varray)
   (if (vads-shapeset varray)
       (varray-shape varray)
       (let* ((base-shape (shape-of (vader-base varray)))
              (axis (setf (vads-axis varray)
                          (if (eq :last (vads-axis varray))
                              (max 0 (1- (length base-shape)))
                              (- (disclose (render (vads-axis varray)))
                                 (vads-io varray))))))
         (setf (vads-shapeset varray) t)
         (loop :for d :in base-shape :for ix :from 0 :when (not (= axis ix)) :collect d)))))

(defclass vader-subarray-split (vader-subarray)
  ((%core-indexer :accessor vasbs-core-indexer
                  :initform nil
                  :initarg :core-indexer
                  :documentation "Core indexer for split subarray."))
  (:metaclass va-class)
  (:documentation "An element of a split array as from the [↓ split] function."))

(defmethod prototype-of ((varray vader-subarray-split))
  (declare (ignore params))
  (get-promised (varray-prototype varray)
                (let ((first-item (funcall (generator-of varray) 0)))
                  (if (varrayp first-item) (prototype-of first-item)
                      (apl-array-prototype first-item)))))

(defmethod generator-of ((varray vader-subarray-split) &optional indexers params)
  (declare (ignore params))
  (let ((base-indexer (generator-of (vader-base varray))))
    (lambda (index)
      (funcall base-indexer (funcall (vasbs-core-indexer varray) (vasv-index varray) index)))))

(defmethod generator-of ((varray vader-split) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let* ((output-shape (shape-of varray))
          (axis (vads-axis varray))
          (base-shape (shape-of (vader-base varray)))
          (sv-length (nth axis base-shape))
          (base-indexer (base-indexer-of varray params))
          (base-factors (get-dimensional-factors base-shape t))
          (core-indexer (indexer-split axis (length output-shape)
                                       base-factors (get-dimensional-factors output-shape t))))
     
       (if (functionp base-indexer)
           (lambda (index)
             (make-instance 'vader-subarray-split
                            :base (vader-base varray) :shape (when sv-length (list sv-length))
                            :index index :core-indexer core-indexer
                            :prototype (when (not output-shape)
                                         (prototype-of (vader-base varray)))))
           (lambda (index) (declare (ignore index)) base-indexer)))))

(defclass vader-section (varray-derived vad-on-axis vad-with-argument vad-with-io vad-invertable vad-reindexing)
  ((%overtaking :accessor vasec-overtaking
                :initform nil
                :initarg :overtaking
                :documentation "Whether this section performs an overtake of its base, creating empty space to be filled with the array's prototype."))
  (:metaclass va-class)
  (:documentation "A sectioned array as from the [↑ take] or [↓ drop] functions."))

(defmethod prototype-of ((varray vader-section))
  (get-promised (varray-prototype varray)
                (progn (generator-of varray)
                       (varray-prototype varray))
                ;; (let ((indexer (generator-of varray))
                ;;       (size (size-of varray))
                ;;       (base-size (size-of (vader-base varray))))
                ;;   (if (or (zerop size) (zerop base-size))
                ;;       (prototype-of (vader-base varray))
                ;;       (aplesque::make-empty-array (render (if (not (functionp indexer))
                ;;                                               indexer (funcall indexer 0))))))
                ))

(defmethod shape-of ((varray vader-section))
  "The shape of a sectioned array is the parameters (if not inverse, as for [↑ take]) or the difference between the parameters and the shape of the original array (if inverse, as for [↓ drop])."
  (get-promised
   (varray-shape varray)
   (let* ((base (vader-base varray))
          (arg-shape (shape-of (vads-argument varray)))
          (arg-indexer (generator-of (setf (vads-argument varray)
                                         (render (vads-argument varray)))))
          (is-inverse (vads-inverse varray))
          (iorigin (vads-io varray))
          (axis (vads-axis varray)))

     (if (and (not is-inverse)
              (eq :last axis)
              (typep (vader-base varray) 'vad-limitable)
              (= 1 (size-of (vads-argument varray))))
         (if (functionp arg-indexer)
             (loop :for a :below (first arg-shape)
                   :collect (abs (funcall arg-indexer a)))
             (list (abs arg-indexer)))
         (let* ((base-shape (copy-list (shape-of base)))
                ;; the shape of the base is only needed for [↓ drop]
                (pre-shape (coerce (loop :for b :below (max (length base-shape)
                                                            (if (not arg-shape) 1 (first arg-shape)))
                                         :collect (or (nth b base-shape) 1))
                                   'vector)))
           (if (vectorp axis)
               (loop :for x :across axis :for ix :from 0
                     :do (setf (aref pre-shape (- x iorigin))
                               (if is-inverse (- (aref pre-shape (- x iorigin))
                                                 (abs (funcall arg-indexer ix)))
                                   (let ((element (abs (funcall arg-indexer ix))))
                                     (when (> element (aref pre-shape (- x iorigin)))
                                       (setf (vasec-overtaking varray) t))
                                     element))))
               (if (eq :last axis)
                   (if (functionp arg-indexer)
                       (loop :for a :below (first arg-shape) :for ix :from 0
                             :do (setf (aref pre-shape ix)
                                       (if is-inverse (max 0 (- (aref pre-shape ix)
                                                                (abs (funcall arg-indexer a))))
                                           (let ((element (abs (funcall arg-indexer a))))
                                             (when (> element (aref pre-shape ix))
                                               (setf (vasec-overtaking varray) t))
                                             element))))
                       (setf (aref pre-shape 0)
                             (if is-inverse (max 0 (- (aref pre-shape 0) (abs arg-indexer)))
                                 (let ((element (abs arg-indexer)))
                                   (when (> element (aref pre-shape 0))
                                     (setf (vasec-overtaking varray) t))
                                   element))))
                   (setf (aref pre-shape (- axis iorigin))
                         (if is-inverse (max 0 (- (aref pre-shape (- axis iorigin))
                                                  (abs arg-indexer)))
                             (let ((element (abs arg-indexer)))
                               (when (> element (aref pre-shape (- axis iorigin)))
                                 (setf (vasec-overtaking varray) t))
                               element)))))
           (coerce pre-shape 'list))))))

(defmethod indexer-of ((varray vader-section) &optional params)
  "Indexer for a sectioned array."
  (get-promised
   (varray-indexer varray) ;; 6↑○⍳3 8↑'a',1 2 3 3↑⊂3 3⍴5 4↑(3 4⍴⍳12) 8 9
   (let* ((assigning (getf params :for-selective-assign))
          (this-base (when assigning (vader-base varray)))
          (base-indexer)
          (layers-below)
          (size (size-of varray))
          (iorigin (vads-io varray))
          (axis (vads-axis varray))
          (is-inverse (vads-inverse varray))
          (base-size (size-of (vader-base varray)))
          (out-dims (if is-inverse (make-array (length (shape-of varray))
                                               :initial-element 0)
                        (coerce (shape-of varray) 'vector)))
          (arg-shape (shape-of (vads-argument varray)))
          (arg-indexer (generator-of (vads-argument varray))))

     (setf layers-below (when assigning (typecase (vader-base varray) (vader-section t)
                                                  (vader-pick
                                                   (setf (vapick-assign (vader-base varray))
                                                         (getf params :assigning))
                                                   (funcall (getf params :toggle-toplevel-subrendering))
                                                   :pick)))
           base-indexer (base-indexer-of varray ;; (list :for-selective-assign
                                                ;;       (getf params :for-selective-assign)
                                                ;;       :assigning (getf params :assigning)
                                                ;;       :toggle-toplevel-subrendering
                                                ;;       (getf params :toggle-toplevel-subrendering))
                                         params
                                         ))

     (if (vectorp axis)
         (loop :for x :across axis :for ix :from 0
               :do (setf (aref out-dims (- x iorigin))
                         (funcall arg-indexer ix)))
         (if (eq :last axis)
             (if (functionp arg-indexer)
                 (loop :for a :below (first arg-shape) :for ix :from 0
                       :do (setf (aref out-dims ix)
                                 (funcall arg-indexer a)))
                 (setf (aref out-dims 0) arg-indexer))
             (setf (aref out-dims (- axis iorigin))
                   arg-indexer)))

     (let* ((indexer (indexer-section (vads-inverse varray)
                                      (or (shape-of (vader-base varray)) '(1))
                                      out-dims assigning))
            ;; (ee (print (funcall indexer 0)))
            (prototype (when (not assigning)
                         (setf (varray-prototype varray)
                               (if (or (zerop size) (zerop base-size))
                                   (prototype-of (vader-base varray))
                                   (let ((indexed (funcall indexer 0)))
                                     (if indexed
                                         (if (not (functionp base-indexer))
                                             (prototype-of base-indexer)
                                             (aplesque:make-empty-array
                                              (render (funcall base-indexer indexed))))
                                         (prototype-of (vader-base varray)))))))))
       
       (if assigning (lambda (index)
                       (declare (type integer index))
                       (let ((indexed (funcall indexer index)))
                         (if (not layers-below) indexed
                             ;; handle the next layer, as for x←⍳9 ⋄ (2↑4↓x)←99 ⋄ x
                             ;; (funcall base-indexer indexed)
                             (let ((inext (funcall base-indexer index)))
                               (when inext (if (eq :pick layers-below)
                                               inext (funcall indexer inext)))))))
           (if (not (functionp base-indexer)) ;; TODO: why is the disclose needed?
               (lambda (index)
                 (declare (ignore index))
                 (disclose base-indexer))
               (lambda (index)
                 (declare (type integer index))
                 ;; (let ((indexed (funcall indexer index)))
                 ;;   (if (not indexed)
                 ;;       prototype (funcall base-indexer indexed)))
                 ;; (print (list :ii index))
                 (funcall indexer index))))))))

(defmethod generator-of ((varray vader-section) &optional indexers params)
  (let ((indexer (indexer-of varray params)))
    (if (getf params :for-selective-assign)
        indexer
        (if (vasec-overtaking varray)
            (let* ((composite-indexer (join-indexers indexers t))
                   (arg (vads-argument varray))
                   (size (size-of varray))
                   (is-negative (when (or (numberp arg) (and (vectorp arg) (= 1 (length arg))))
                                  (minusp (disclose-unitary (vads-argument varray)))))
                   (scalar-index (when (not (shape-of (vader-base varray))) 0))
                   (prototype (prototype-of varray)))
              (when scalar-index
                (if (vectorp arg)
                    (loop :for a :across arg :for d :in (shape-of varray)
                          :for f :in (get-dimensional-factors (shape-of varray))
                          :when (minusp a) :do (incf scalar-index (* f(1- d))))
                    (when (minusp arg) (incf scalar-index (1- (first (shape-of varray)))))))
              
              ;; IPV-TODO: figure out how to allow n-rank mixed positive-negative takes for scalars
              (lambda (index)
                (let ((indexed (funcall indexer (funcall composite-indexer index))))
                  ;; (print (list :iin index indexed :base (vader-base varray)))
                  ;; (print (list :pro (prototype-of varray)))
                  (if indexed (let ((generator (generator-of (vader-base varray))))
                                ;; (print (list :gen index generator indexed))
                                ;; (print (list :cc indexed generator is-negative))
                                (if (not (numberp indexed)) ;; IPV-TODO: remove after refactor ?
                                    (if (zerop index) indexed prototype)
                                    (if (not (functionp generator))
                                        (if (= index scalar-index) generator prototype)
                                        (funcall generator indexed))))
                      prototype))))
            (if (zerop (size-of varray))
                (let ((prototype (prototype-of varray)))
                  (lambda (index) (declare (ignore index)) prototype))
                (generator-of (vader-base varray)
                              (if (not indexer) indexers (cons indexer indexers))))))))

(defclass vader-enclose (vad-subrendering varray-derived vad-on-axis vad-with-io
                         vad-with-argument vad-maybe-shapeless)
  ((%inner-shape :accessor vaenc-inner-shape
                 :initform nil
                 :initarg :inner-shape
                 :documentation "Inner shape value for re-enclosed array."))
  (:metaclass va-class)
  (:documentation "An enclosed or re-enclosed array as from the [⊂ enclose] function."))

(defmethod etype-of ((varray vader-enclose))
  (let ((base (vader-base varray)))
    (if (or (arrayp base) (varrayp base)
            (vads-argument varray))
        t (assign-element-type base))))

(defmethod prototype-of ((varray vader-enclose))
  (if (not (zerop (size-of varray)))
      (call-next-method)
      (make-instance 'vader-expand :argument 0 :axis :last :base (vader-base varray)
                                   :index-origin (vads-io varray))))

(defmethod shape-of ((varray vader-enclose))
  (get-promised (varray-shape varray)
                (if (vads-shapeset varray)
                    (varray-shape varray)
                    (let* ((axis (setf (vads-axis varray)
                                       (when (vads-axis varray)
                                         (apply-scalar #'- (disclose (render (vads-axis varray)))
                                                       (vads-io varray)))))
                           (positions (when (vads-argument varray)
                                        (enclose-atom (render (vads-argument varray)))))
                           (base-shape (shape-of (vader-base varray)))
                           (axis-size (if (and axis positions)
                                          (nth axis base-shape)
                                          (or (first (last base-shape)) 1)))
                           (input-offset 0) (intervals (list 0)))
                      (when positions
                        (dotimes (i (if (is-unitary positions) (or (first (last base-shape)) 1)
                                        (length positions)))
                          (let ((p (if (is-unitary positions) (disclose-unitary positions)
                                       (aref positions i))))
                            (if (zerop p) (progn (incf (first intervals))
                                                 (incf input-offset))
                                (progn (setq intervals
                                             (append (loop :for i :below p :collect 0) intervals))
                                       (when (and axis-size (> axis-size input-offset))
                                         (incf input-offset)
                                         (when (first intervals) (incf (first intervals)))))))))
                      
                      (when (and axis-size (second intervals))
                        (if (>= axis-size input-offset)
                            (incf (first intervals) (- axis-size input-offset))
                            (error "Size of partitions exceeds size of input array on axis ~w." axis)))
                      
                      (setf (vads-shapeset varray) (or (when positions
                                                         (coerce (reverse intervals) 'vector))
                                                       t))

                      (if positions (list (1- (length intervals)))
                          (when axis (let ((outer-shape) (inner-shape)
                                           (base-shape (shape-of (vader-base varray))))
                                       (loop :for d :in base-shape :for dx :from 0
                                             :do (if (if (integerp axis) (not (= dx axis))
                                                         (loop :for a :across axis :never (= a dx)))
                                                     (push d outer-shape)
                                                     (push d inner-shape)))
                                       (setf (vaenc-inner-shape varray) (reverse inner-shape))
                                       (reverse outer-shape))))))))

(defmethod generator-of ((varray vader-enclose) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let* ((base-shape (shape-of (vader-base varray)))
          (output-shape (shape-of varray))
          (output-size (reduce #'* output-shape))
          (inner-shape (vaenc-inner-shape varray))
          (axis (or (vads-axis varray)
                    (max 0 (1- (length base-shape)))))
          (ofactors (get-dimensional-factors base-shape))
          (base-indexer (base-indexer-of varray))
          (outer-factors (get-dimensional-factors output-shape t))
          (inner-factors (get-dimensional-factors inner-shape t))
          (intervals (vads-shapeset varray))
          (offset-indexer
            (lambda (o)
              (lambda (i)
                (let ((orest o) (irest i) (index 0) (inner-dx 0) (outer-dx 0))
                  (loop :for f :in ofactors :for fx :from 0
                        :do (let ((in-outer (if (numberp axis) (/= fx axis)
                                                (loop :for a :across axis :never (= fx a)))))
                              (multiple-value-bind (factor remaining)
                                  (if in-outer (floor orest (aref outer-factors outer-dx))
                                      (floor irest (aref inner-factors inner-dx)))
                                (if in-outer (progn (incf outer-dx) (setf orest remaining))
                                    (progn (incf inner-dx) (setf irest remaining)))
                                (incf index (* f factor)))))
                  (funcall base-indexer index)))))
          (each-offset (when (vectorp intervals)
                         (coerce (loop :for i :across intervals :summing i :into s :collect s)
                                 'vector)))
          (section-size (when (vectorp intervals)
                          (reduce #'* (loop :for d :in base-shape :for dx :from 0
                                            :when (> dx axis) :collect d))))
          (iseg (when section-size (if (< 1 section-size)
                                       section-size (or (nth axis base-shape) 1))))
          (last-indim (first (last base-shape)))
          (partition-indexer
            (lambda (o shape)
              (let ((input-offset (aref each-offset o))
                    (last-idim (or (first (last shape)) 1))
                    (this-size (reduce #'* shape)))
                (lambda (i)
                  (when (not (zerop this-size))
                    (let ((oseg (floor i iseg)) (ivix (mod i iseg)))
                      (if (not (functionp base-indexer))
                          base-indexer (funcall base-indexer
                                                (if (= 1 section-size)
                                                    (+ input-offset (mod i last-idim)
                                                       (* last-indim (floor i last-idim)))
                                                    (+ ivix (* iseg (+ input-offset oseg)))))))))))))
     
     (lambda (index)
       (if (vectorp intervals)
           (if (< 0 output-size)
               (let* ((this-shape (loop :for dim :in base-shape :for dx :from 0
                                        :collect (if (/= dx axis)
                                                     dim (aref intervals (1+ index)))))
                      (sub-indexer (funcall partition-indexer index this-shape))
                      (first-item (funcall sub-indexer 0))
                      (prototype (if (varrayp first-item)
                                     (prototype-of first-item)
                                     (apl-array-prototype first-item))))
                 ;; (print (list :tt index this-shape))
                 (make-instance 'vader-subarray :prototype prototype :indexer sub-indexer
                                                :shape (or this-shape '(1)) :base (vader-base varray)))
               (make-instance 'vader-subarray :prototype (prototype-of (vader-base varray))
                                              :shape output-shape :base (vader-base varray)))
           (if (not inner-shape)
               (if (vads-axis varray)
                   (if (not (functionp base-indexer))
                       base-indexer (funcall base-indexer index))
                   (vader-base varray))
               (if (not (functionp base-indexer))
                   base-indexer (let* ((sub-indexer (funcall offset-indexer index))
                                       (first-item (funcall sub-indexer 0))
                                       (prototype (if (not output-shape)
                                                      (aplesque::make-empty-array
                                                       (vader-base varray))
                                                      (if (varrayp first-item)
                                                          (prototype-of first-item)
                                                          (apl-array-prototype first-item)))))
                                  (make-instance 'vader-subarray
                                                 :prototype prototype
                                                 :base (vader-base varray)
                                                 :shape inner-shape :indexer sub-indexer)))))))))

(defclass vader-partition (vad-subrendering varray-derived vad-on-axis vad-with-io
                           vad-with-argument vad-maybe-shapeless)
  ((%params :accessor vapart-params
            :initform nil
            :initarg :params
            :documentation "Parameters for partitioning."))
  (:metaclass va-class)
  (:documentation "A partitioned array as from the [⊆ partition] function."))

(defmethod etype-of ((varray vader-partition))
  (let ((base (vader-base varray)))
    (if (or (arrayp base) (varrayp base)
            (vads-argument varray))
        t (assign-element-type base))))

(defmethod shape-of ((varray vader-partition))
  (get-promised
   (varray-shape varray)
   (let* ((idims (shape-of (vader-base varray)))
          (arank (length idims))
          (positions (when (vads-argument varray)
                       (setf (vads-argument varray)
                             (funcall (lambda (i)
                                        (if (not (arrayp i))
                                            i (if (< 0 (array-rank i))
                                                  i (vector (aref i)))))
                                      (disclose-unitary (render (vads-argument varray)))))))
          (axis (setf (vads-axis varray)
                      (when (vads-axis varray)
                        (max 0 (if (eq :last (vads-axis varray))
                                   (1- arank)
                                   (- (vads-axis varray)
                                      (vads-io varray))))))))
     (if (not positions)
         (let ((base-type (etype-of (vader-base varray))))
           (when (eq t base-type) ;; array is simple if not t-type
             (let ((is-complex)
                   (base-indexer (generator-of (vader-base varray))))
               (when base-indexer
                 (loop :for i :below (size-of (vader-base varray))
                       :do (if (shape-of (funcall base-indexer i))
                               (setf is-complex t)))
                 (when is-complex (shape-of (vader-base varray)))))))
         (if (not (arrayp positions))
             (when (< 1 (size-of (vader-base varray)))
               (setf (vapart-params varray) (list :partitions 1))
               (list 1))
             (let ((r-indices) (r-intervals) (indices) (intervals)
                   (interval-size 0) (current-interval -1) (partitions 0))
               (declare (dynamic-extent r-indices r-intervals indices intervals
                                        interval-size current-interval partitions))
               ;; find the index where each partition begins in the
               ;; input array and the length of each partition
               (loop :for pos :across positions :for p :from 0
                     :do (when (not (zerop current-interval))
                           (incf interval-size))
                         ;; if a position lower than the current interval index is encountered,
                         ;; decrement the current index to it, as for 1 1 1 2 1 1 2 1 1⊆⍳9
                         (when (and current-interval (< 0 pos current-interval))
                           (setq current-interval pos))
                     :when (or (< current-interval pos)
                               (and (zerop pos) (not (zerop current-interval))))
                       :do (setq r-indices (cons p r-indices)
                                 r-intervals (if (rest r-indices) (cons interval-size r-intervals)))
                           (incf partitions (if (zerop pos) 0 1))
                           (setq current-interval pos interval-size 0))
               ;; add the last entry to the intervals provided the
               ;; positions list didn't have a 0 value at the end
               (when (not (zerop (aref positions (1- (length positions)))))
                 (push (- (length positions) (first r-indices))
                       r-intervals))
               
               (when (/= (length r-indices) (length r-intervals))
                 (setq r-indices (rest r-indices)))
               ;; collect the indices and intervals into lists the right way
               ;; around, dropping indices with 0-length intervals
               ;; corresponding to zeroes in the positions list
               (loop :for rint :in r-intervals :for rind :in r-indices
                     :when (not (zerop rint))
                       :do (push rint intervals)
                           (push rind indices))
               
               (setf (vapart-params varray)
                     (list :partitions (or partitions 1)
                           :intervals (coerce intervals 'vector)
                           :indices (coerce indices 'vector)))

               (loop :for dim :in idims :for dx :below arank
                     :collect (if (= dx axis) partitions dim))))))))

(defmethod generator-of ((varray vader-partition) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (if (not (or (vads-argument varray)
                (vapart-params varray)))
       (if (shape-of varray)
           (base-indexer-of varray)
           (lambda (index) (vader-base varray)))
       (let* ((idims (shape-of (vader-base varray)))
              (partitions (getf (vapart-params varray) :partitions))
              (intervals (getf (vapart-params varray) :intervals))
              (indices (getf (vapart-params varray) :indices))
              (base-indexer (base-indexer-of varray))
              (axis (vads-axis varray))
              (section-size (reduce #'* (loop :for d :in idims :for dx :from 0
                                              :when (> dx axis) :collect d)))
              (output-shape (shape-of varray))
              (ofactors (get-dimensional-factors output-shape t))
              (ifactors (get-dimensional-factors idims t))
              (partition-indexer
                (lambda (i focus)
                  (lambda (ix)
                    (let ((rest i) (input-index 0))
                      (loop :for of :across ofactors :for if :across ifactors :for fx :from 0
                            :do (multiple-value-bind (factor remaining) (floor rest of)
                                  (setq rest remaining)
                                  (incf input-index (* if (if (/= fx axis) factor
                                                              (+ ix (if (not indices)
                                                                        0 (aref indices focus))))))))
                      (funcall base-indexer input-index))))))
         
         (lambda (index)
           (if (not (functionp base-indexer))
               base-indexer (let* ((focus (mod (floor index section-size) partitions))
                                   (sub-indexer (funcall partition-indexer index focus))
                                   (first-item (funcall sub-indexer 0))
                                   (prototype (if (not output-shape)
                                                  (aplesque::make-empty-array
                                                   (vader-base varray))
                                                  (if (varrayp first-item)
                                                      (prototype-of first-item)
                                                      (apl-array-prototype first-item)))))
                              (make-instance 'vader-subarray
                                             :prototype prototype :base (vader-base varray)
                                             :shape (if (not intervals)
                                                        idims (list (aref intervals focus)))
                                             :indexer sub-indexer))))))))

(defclass vader-expand (varray-derived vad-on-axis vad-with-io vad-with-argument vad-invertable vad-reindexing)
  ((%separating :accessor vadex-separating
                :initform nil
                :initarg :separating
                :documentation "Whether this section separates sections of the input with empty space in between to be filled with the array's prototype."))
  (:metaclass va-class)
  (:documentation "An expanded (as from [\ expand]) or compressed (as from [/ compress]) array."))

(defmethod shape-of ((varray vader-expand))
  "The shape of an expanded or compressed array."
  (get-promised
   (varray-shape varray)
   (let* ((degrees-count (first (shape-of (vads-argument varray))))
          (degrees (setf (vads-argument varray)
                         (funcall (lambda (i)
                                    (if (not (arrayp i))
                                        i (if (< 0 (array-rank i))
                                              i (vector (aref i)))))
                                  (disclose-unitary (render (vads-argument varray))))))
          (base-shape (copy-list (shape-of (vader-base varray))))
          (base-rank (length base-shape))
          (is-inverse (vads-inverse varray))
          (axis (setf (vads-axis varray)
                      (max 0 (if (eq :last (vads-axis varray))
                                 (1- base-rank)
                                 (- (vads-axis varray)
                                    (vads-io varray)))))))
     
     ;; designate the array as separating if appropriate;
     ;; this will determine the generator logic
     (if (not (vectorp degrees)) (when (not (plusp degrees))
                                   (setf (vadex-separating varray) t))
         (loop :for d :across degrees :when (not (plusp d))
               :do (setf (vadex-separating varray) t)))
     ;; REDUCE SHAPE ERROR CAUSED HERE

     (cond ((and base-shape (zerop (reduce #'* base-shape)))
            (if is-inverse
                (if (> axis (1- base-rank))
                    (error "This array does not have an axis ~a." axis)
                    (if (or (not (arrayp degrees))
                            (or (not degrees-count)
                                (= 1 degrees-count)))
                        (loop :for d :in base-shape :for dx :from 0
                              :collect (if (/= dx axis) d (* d (disclose-unitary degrees))))
                        (if (not degrees-count)
                            (loop :for d :below base-rank :for dx :from 0
                                  :collect (if (= dx axis) 0 d))
                            (if (and degrees-count (/= degrees-count (nth axis base-shape)))
                                (error "Compression degrees must equal size of array in dimension to compress.")
                                (let ((output-size (loop :for d :below degrees-count
                                                         :summing (if (not (arrayp degrees))
                                                                      degrees (aref degrees d)))))
                                  (loop :for d :in base-shape :for dx :from 0
                                        :collect (if (= dx axis) output-size d)))))))
                (if (or (not degrees-count)
                        (= 1 degrees-count)
                        (not (arrayp degrees)))
                    (list (if (and (= 1 base-rank)
                                   (zerop (if (not (arrayp degrees))
                                              degrees (aref degrees 0))))
                              1 (abs (if (not (arrayp degrees))
                                         degrees (aref degrees 0)))))
                    (if (and (loop :for d :across degrees :always (zerop d))
                             (zerop (nth axis base-shape)))
                        (if (= 1 base-rank) (list degrees-count)
                            (loop :for d :in base-shape :for dx :from 0
                                  :collect (if (= dx axis) degrees-count d)))
                        (error "An empty array can only be expanded to a single negative degree ~a"
                               "or to any number of empty dimensions.")))))
           ((and (not is-inverse)
                 (or (and (not (arrayp degrees))
                          (zerop degrees))
                     (and degrees-count (= 1 degrees-count)
                          (if (not (arrayp degrees))
                              (arrayp degrees)
                              (zerop (aref degrees 0))))))
            (append (butlast base-shape) (list 0)))
           ((and (not base-shape)
                 (not (arrayp degrees)))
            (setf (vads-argument varray) (list (abs degrees))))
           ((and is-inverse base-shape degrees-count (< 1 degrees-count)
                 (nth axis base-shape)
                 (/= degrees-count (nth axis base-shape)))
            (error "Attempting to replicate elements across array but ~a"
                   "degrees are not equal to length of selected input axis."))
           ((and (not is-inverse)
                 base-shape (< 1 (reduce #'* base-shape))
                 (nth axis base-shape)
                 (/= (or (and (arrayp degrees)
                              (loop :for degree :across degrees :when (plusp degree)
                                    :counting degree :into dcount :finally (return dcount)))
                         degrees)
                     (nth axis base-shape)))
            (error "Attempting to expand elements across array but ~a"
                   "positive degrees are not equal to length of selected input axis."))
           (t (let ((ex-dim))
                (if (not (arrayp degrees))
                    (setf ex-dim (* (abs degrees) (or (nth axis base-shape) 1)))
                    (loop :for degree :across degrees :for dx :from 0
                          :summing (max (abs degree) (if is-inverse 0 1)) :into this-dim
                          :finally (setf ex-dim this-dim)))
                (loop :for dim :in (or base-shape '(1)) :for index :from 0
                      :collect (if (/= index axis) dim (* 1 ex-dim)))))))))

(defmethod indexer-of ((varray vader-expand) &optional params)
  (get-promised (varray-indexer varray)
                (let* ((arg-rendered (render (vads-argument varray)))
                       ;; TODO: why can't (vector item) be used below?
                       (arg-vector (if (not (typep arg-rendered 'sequence))
                                       arg-rendered (coerce arg-rendered 'vector)))
                       (base-indexer (base-indexer-of varray)))
                  (when (or (integerp arg-rendered)
                            (< 0 (size-of arg-vector)))
                    (indexer-expand arg-vector (shape-of (vader-base varray))
                                    (vads-axis varray)
                                    (vads-inverse varray)
                                    (getf params :for-selective-assign))))))

(defmethod generator-of ((varray vader-expand) &optional indexers params)
  (let ((indexer (indexer-of varray params)))
    (if (getf params :for-selective-assign)
        indexer
        (if (vadex-separating varray)
            (let ((generator (generator-of (vader-base varray)))
                  (prototype (prototype-of (vader-base varray)))
                  (composite-indexer (join-indexers indexers t)))
              (lambda (index)
                (let ((indexed (funcall indexer (funcall composite-indexer index))))
                  ;; (print (list :pro (prototype-of varray)))
                  (if (not indexed) prototype
                      ;; (print (list :gen index generator indexed))
                      ;; (print (list :cc indexed generator is-negative))
                      (if (not (functionp generator))
                          generator (funcall generator indexed))))))
            (generator-of (vader-base varray)
                          (cons indexer indexers))))))

(defclass vader-pick (varray-derived vad-with-argument vad-with-io)
  ((%reference :accessor vapick-reference
               :initform nil
               :initarg :reference
               :documentation "Reference to the array designated by this function.")
   (%assign :accessor vapick-assign
            :initform nil
            :initarg :assign
            :documentation "Item to be assigned to selected index in array.")
   (%selector :accessor vapick-selector
              :initform nil
              :initarg :selector
              :documentation "Virtual array selecting items within index to assign.")
   (%function :accessor vapick-function
              :initform nil
              :initarg :assign
              :documentation "Function to be applied to selected index in array.")
   (%assign-path-index :accessor vapick-apath-index
                       :initform 0
                       :initarg :assign-path-index
                       :documentation "Index for assignment path."))
  (:metaclass va-class)
  (:documentation "An element from within an array as from the [⊃ disclose] or [⊃ pick] functions."))

(defun get-path-value (varray index base)
  (if (numberp index)
      (- index (vads-io varray))
      (let ((ix 0)
            (iindexer (if (shape-of index)
                          (generator-of index)
                          (generator-of (funcall (generator-of index) 0))))
            (index-length (if (shape-of index)
                              (size-of index)
                              (size-of (funcall (generator-of index) 0))))
            (factors (get-dimensional-factors (shape-of base))))
        (loop :for f :in factors :for s :below index-length
              :do (incf ix (* f (- (if (not (functionp iindexer))
                                       iindexer (funcall iindexer s))
                                   (vads-io varray)))))
        ix)))

(defgeneric fetch-reference (varray base &optional path path-index)
  (:documentation "Fetch the array targeted by an invocation of [⊃ disclose] or [⊃ pick]."))

(defmethod fetch-reference ((varray vader-pick) base &optional path path-index)
  (or (vapick-reference varray)
      (let* ((path (or path (vads-argument varray)))
             (path-indexer (generator-of path))
             (path-length (size-of path))
             (base-indexer (generator-of base)))
        (if path
            (let ((path-value (get-path-value varray (if (not (functionp path-indexer))
                                                         path-indexer (funcall path-indexer
                                                                               (or path-index 0)))
                                              base)))
              (if path-index (if (/= path-index (1- path-length))
                                 (fetch-reference varray (funcall base-indexer path-value)
                                                  path (1+ (or path-index 0)))
                                 (let ((indexer (if (not (functionp base-indexer))
                                                    ;; TODO: special mix case, generalize
                                                    base-indexer (if (typep base 'vader-mix)
                                                                     (let ((bix (funcall base-indexer 0)))
                                                                       (if (not (arrayp bix))
                                                                           bix (row-major-aref
                                                                                bix path-value)))
                                                                     (funcall base-indexer path-value)))))
                                   (when (and (not (shape-of indexer))
                                              (or (arrayp indexer) (varrayp indexer)))
                                     (setf (vads-subrendering varray) t))
                                   indexer))
                  (setf (vapick-reference varray)
                        (if (= 1 path-length)
                            (let ((indexer (if (not (functionp base-indexer))
                                               base-indexer (if (typep base 'vader-mix)
                                                                (let ((bix (funcall base-indexer 0)))
                                                                  (if (not (arrayp bix))
                                                                      bix (row-major-aref bix path-value)))
                                                                (funcall base-indexer path-value)))))
                              (if (and (not (shape-of indexer))
                                       (or (arrayp indexer) (varrayp indexer)))
                                  (setf (vads-subrendering varray) t))
                              indexer)
                            (fetch-reference varray (funcall base-indexer path-value)
                                             path (1+ (or path-index 0)))))))
            (setf (vapick-reference varray)
                  ;; the 'vader-pick clause handles nested pick references like 2⊃⊃⊃{,/⍵}/3⍴⊂⍳3
                  (let ((indexer (if (not (functionp base-indexer))
                                     base-indexer (if (zerop (size-of base))
                                                      (prototype-of base)
                                                      (let ((bix (funcall base-indexer 0)))
                                                        ;; (print (list :ba base bix))
                                                        (if (or (not (arrayp bix))
                                                                ;; TODO: special mix case, generalize
                                                                (not (typep base 'vader-mix))
                                                                )
                                                            bix (row-major-aref bix 0)))))))
                    (when (and (shape-of base) (not (shape-of indexer))
                               (or (arrayp indexer)
                                   (varrayp indexer)))
                      (setf (vads-subrendering varray) t))
                    indexer))))))

(defgeneric assign-reference (varray base &optional path path-indexer path-index))

(defmethod assign-reference ((varray vader-pick) base &optional path path-indexer path-index)
  (let ((base-indexer (generator-of base))
        (assigned-index (if (not path)
                            0 (get-path-value
                               varray (if (not (functionp path-indexer))
                                          path-indexer (funcall path-indexer (or path-index 0)))
                               base))))
    (setf (vads-subrendering varray) t)
    (lambda (index)
      (if (= index assigned-index)
          (if (vapick-selector varray)
              (make-instance
               'vader-select :base (funcall base-indexer index)
                             :index-origin (vads-io varray)
                             :assign (vapick-assign varray)
                             :selector (vapick-selector varray))
              (if (vapick-function varray)
                  (funcall (vapick-function varray)
                           (funcall base-indexer index) (vapick-assign varray))
                  (vapick-assign varray)))
          (funcall base-indexer index)))))

(defmethod etype-of ((varray vader-pick))
  (if (vapick-assign varray) ;; TODO: specify
      t (etype-of (fetch-reference varray (vader-base varray)))))

(defmethod prototype-of ((varray vader-pick))
  (if (vapick-assign varray)
      (prototype-of (vader-base varray))
      (prototype-of (fetch-reference varray (vader-base varray)))))

(defmethod shape-of ((varray vader-pick))
  (if (vapick-assign varray)
      (shape-of (vader-base varray))
      (if (zerop (size-of (vader-base varray)))
          (shape-of (prototype-of (vader-base varray)))
          
          (let* ((ref (fetch-reference varray (vader-base varray)))
                 (this-indexer (generator-of ref)))
            (if (not (functionp this-indexer)) ;; handle cases like (scc≡⍳∘≢) (⍳10),⊂⍬
                (if (arrayp this-indexer)
                    (shape-of (row-major-aref this-indexer 0))
                    (when (arrayp ref)
                      (shape-of ref)))
                (shape-of ref))))))

(defmethod generator-of ((varray vader-pick) &optional indexers params)
  ;; (print (list :ii (vapick-selector varray) (vapick-assign varray)))
  ;; PROMISE SUPPORT
  (if (vapick-assign varray)
      (if (= (vapick-apath-index varray)
             (1- (size-of (vads-argument varray))))
          (assign-reference varray (vader-base varray) (vads-argument varray)
                            (generator-of (vads-argument varray))
                            0)
          (let* ((base-indexer (generator-of (vader-base varray)))
                 (path-indexer (generator-of (vads-argument varray)))
                 (this-index (get-path-value
                              varray (if (not (functionp path-indexer))
                                         path-indexer (funcall path-indexer
                                                               (vapick-apath-index varray)))
                              (vader-base varray))))
            (setf (vads-subrendering varray) t)
            ;; (print (list :ti this-index (vapick-apath-index varray) (vapick-apath-index varray)))
            (lambda (index)
              (if (= index this-index)
                  (if (vapick-selector varray)
                      (make-instance
                       'vader-select :base (funcall base-indexer this-index)
                                     :index-origin (vads-io varray)
                                     :assign (vapick-assign varray)
                                     :selector (vapick-selector varray))
                      (make-instance
                       'vader-pick :base (funcall base-indexer this-index)
                                   :argument (vads-argument varray) :index-origin (vads-io varray)
                                   :assign (vapick-assign varray)
                                   :assign-path-index (1+ (vapick-apath-index varray))))
                  (funcall base-indexer index)))))
      
      (let* ((this-reference (fetch-reference varray (vader-base varray)))
             (this-indexer (generator-of this-reference))) ;; IPV-TODO: generator bug!
        ;; (print (list :as (shape-of varray) (shape-of (vader-base varray)))) ; (scc≡⍳∘≢) (⍳10),⊂⍬ ⋄ md1 wpath 1 4 ⋄ (↑g w) wspan 1
        (if (varrayp this-indexer)
            (generator-of this-indexer)
            (if (not (arrayp this-indexer))
                this-indexer (generator-of (row-major-aref this-indexer
                                                           (or (vads-argument varray) 0))))))))

(defclass vader-intersection (varray-derived vad-limitable)
  nil (:metaclass va-class)
  (:documentation "An array intersection as from the [∩ intersection] function."))

(defmethod etype-of ((varray vader-intersection))
  (let ((this-indexer (generator-of varray)))
    (declare (ignore this-indexer))
    (etype-of (vader-content varray))
    ;; (apply #'type-in-common (loop :for array :across (vader-base varray) :collect (etype-of array)))
    ))

(defmethod prototype-of ((varray vader-intersection))
  (let ((this-indexer (generator-of varray)))
    (declare (ignore this-indexer))
    (prototype-of (vader-content varray))))

(defmethod shape-of ((varray vader-intersection))
  (get-promised (varray-shape varray) (let ((this-indexer (generator-of varray)))
                                        (declare (ignore this-indexer))
                                        (shape-of (vader-content varray)))))

(defmethod generator-of ((varray vader-intersection) &optional indexers params)
  (when (not (vader-content varray))
    (let ((derivative-count (if (getf params :shape-deriving)
                                (reduce #'* (getf params :shape-deriving))))
          (contents (loop :for a :across (render (vader-base varray)) :collect (render a))))
      (if (not (loop :for a :in contents :always (not (second (shape-of a)))))
          (error "Arguments to [∩ intersection] must be vectors.")
          (let* ((match-count 0)
                 (matches (if (arrayp (first contents))
                              (loop :for item :across (first contents)
                                    :while (or (not derivative-count) (< match-count derivative-count))
                                    :when (loop :for c :in (rest contents)
                                                :always (if (arrayp c)
                                                            (find item c :test #'array-compare)
                                                            (array-compare item c)))
                                      :collect item :and :do (incf match-count))
                              (when (loop :for c :in (rest contents)
                                          :always (if (arrayp c)
                                                      (find (first contents)
                                                            c :test #'array-compare)
                                                      (array-compare (first contents) c)))
                                (incf match-count)
                                (list (first contents))))))
            (setf (vader-content varray)
                  (make-array match-count :initial-contents matches
                                          :element-type (apply #'type-in-common
                                                               (mapcar #'etype-of contents))))))))
  (lambda (index) (aref (vader-content varray) index)))

(defclass vader-unique (varray-derived vad-limitable)
  ((%indices :accessor vauni-indices
             :initform nil
             :initarg :indices
             :documentation "Parameters for partitioning."))
  (:metaclass va-class)
  (:documentation "An array intersection as from the [∩ intersection] function."))

(defmethod etype-of ((varray vader-unique))
  (declare (ignore varray))
  t)

(defmethod shape-of ((varray vader-unique))
  (get-promised (varray-shape varray)
                (let ((this-indexer (generator-of varray)))
                  (declare (ignore this-indexer))
                  (if (vader-content varray)
                      (cons (length (vauni-indices varray))
                            (rest (shape-of (vader-content varray))))
                      (list (length (vauni-indices varray)))))))

(defmethod generator-of ((varray vader-unique) &optional indexer params)
  (let* ((base-shape (shape-of (vader-base varray)))
         (cell-size (reduce #'* (rest base-shape)))
         (base-size (size-of (vader-base varray)))
         (base-rank (length base-shape))
         (base-indexer (generator-of (vader-base varray))))
    (if (not (vauni-indices varray))
        (let ((derivative-count (when (getf params :shape-deriving)
                                  (reduce #'* (getf params :shape-deriving)))))
          (if (not (functionp base-indexer))
              (setf (vauni-indices varray)
                    (if (not base-shape) #*0 #()))
              (if (= 1 base-rank)
                  (let ((uniques) (indices) (unique-count 0))
                    (loop :for ix :below base-size
                          :while (or (not derivative-count) (< unique-count derivative-count))
                          :do (let ((item (render (funcall base-indexer ix))))
                                (when (not (find item uniques :test #'array-compare))
                                  (push item uniques)
                                  (push ix indices)
                                  (incf unique-count))))
                    (setf (vauni-indices varray)
                          (make-array unique-count :element-type (list 'integer 0 base-size)
                                                   :initial-contents (reverse indices))))
                  (let ((base (render (vader-base varray)))
                        (major-cells (make-array (first base-shape)))
                        (indices) (uniques) (unique-count 0))
                    (loop :for i :below (first base-shape)
                          :do (setf (aref major-cells i)
                                    (make-array (rest base-shape)
                                                :displaced-to base :element-type (etype-of base)
                                                :displaced-index-offset (* i cell-size))))
                    (loop :for item :across major-cells :for ix :from 0
                          :when (not (find item uniques :test #'array-compare))
                            :do (push ix indices)
                                (push item uniques)
                                (incf unique-count))
                    (setf (vader-content varray) base
                          (vauni-indices varray)
                          (make-array unique-count :element-type (list 'integer 0 (first base-shape))
                                                   :initial-contents (reverse indices))))))))
    (lambda (index)
      ;; (print (list :inde index (etype-of varray) (shape-of varray)))
      (if (not (functionp base-indexer))
          base-indexer
          (if (= 1 base-rank)
              (funcall base-indexer (aref (vauni-indices varray) index))
              (multiple-value-bind (count remainder) (floor index cell-size)
                (row-major-aref (vader-content varray)
                                (+ remainder (* cell-size (aref (vauni-indices varray)
                                                                count))))))))))

(defclass vader-union (varray-derived vad-limitable)
  nil (:metaclass va-class)
  (:documentation "An array intersection as from the [∩ union] function."))

(defmethod etype-of ((varray vader-union))
  (apply #'type-in-common (loop :for array :across (vader-base varray) :collect (etype-of array))))

(defmethod shape-of ((varray vader-union))
  (get-promised (varray-shape varray)
                (let ((this-indexer (generator-of varray)))
                  (declare (ignore this-indexer))
                  (list (+ (or (first (shape-of (aref (vader-content varray) 0))) 1)
                           (or (first (shape-of (aref (vader-content varray) 1))) 1))))))

(defmethod generator-of ((varray vader-union) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let* ((derivative-count (if (getf params :shape-deriving)
                                (reduce #'* (getf params :shape-deriving))))
          (contents (loop :for a :across (vader-base varray) :collect (render a)))
          (shapes (loop :for a :across (vader-base varray) :collect (shape-of a)))
          (indexers (loop :for a :across (vader-base varray) :collect (generator-of a)))
          (first (first contents)))
     (if (not (loop :for shape :in shapes :always (not (second shape))))
         (error "Arguments to [∪ union] must be vectors.")
         (let ((matched) (appended))
           (loop :for this-item :in (rest contents)
                 :do (push nil matched)
                     (if (arrayp first)
                         (if (arrayp this-item)
                             (loop :for ti :across this-item
                                   :when (not (find ti first :test #'array-compare))
                                     :do (push ti (first matched)))
                             (when (not (find this-item first :test #'array-compare))
                               (push this-item (first matched))))
                         (if (arrayp this-item)
                             (loop :for ti :across this-item
                                   :when (not (array-compare ti first))
                                     :do (push ti (first matched)))
                             (when (not (array-compare this-item first))
                               (push this-item (first matched)))))
                     (loop :for m :in (rest matched)
                           :do (if (arrayp this-item)
                                   (loop :for ti :across this-item
                                         :when (not (find ti m :test #'array-compare))
                                           :do (push ti (first matched)))
                                   (if (not (find this-item m :test #'array-compare))
                                       (push this-item (first matched))))))
           (loop :for m :in matched
                 :do (loop :for subm :in m :do (push subm appended)))
           (setf (vader-content varray)
                 (vector first (make-array (length appended)
                                           :initial-contents appended)))))
     (lambda (index)
       (if (not (arrayp (aref (vader-content varray) 0)))
           (if (zerop index) (aref (vader-content varray) 0)
               (aref (aref (vader-content varray) 1)
                     (1- index)))
           (if (< index (length (aref (vader-content varray) 0)))
               (aref (aref (vader-content varray) 0) index)
               (aref (aref (vader-content varray) 1)
                     (- index (length (aref (vader-content varray) 0))))))))))

(defclass vader-turn (varray-derived vad-on-axis vad-with-io vad-with-argument vad-invertable vad-reindexing)
  nil (:metaclass va-class)
  (:documentation "A rotated array as from the [⌽ rotate] function."))

(defun arg-process (varray)
  (let ((argument (vads-argument varray)))
    (funcall (if (vads-inverse varray) #'- #'identity)
             (if (or (listp argument) (numberp argument))
                 argument (if (varrayp argument)
                              (render argument) ;; TODO: eliminate forced render here
                              (when (arrayp argument) argument))))))

(defmethod indexer-of ((varray vader-turn) &optional params)
  ;; (declare (ignore params) (optimize (speed 3) (safety 0)))
  ;; (print (list :mm (varray-meta varray) (vader-base varray) (render (vader-base varray))
  ;;              (vads-argument varray)))
  ;; (setf april::iii (vader-base varray))
  (when (shape-of varray)
    (indexer-turn (if (eq :last (vads-axis varray))
                      (1- (rank-of varray))
                      (- (vads-axis varray)
                         (vads-io varray)))
                  (shape-of varray)
                  (or (when (getf (varray-meta varray) :gen-meta) ;; TOGGLE
                        (getf (rest (getf (varray-meta varray) :gen-meta)) :indexer-key))
                      t)
                  (getf (rest (getf (varray-meta varray) :gen-meta)) :index-width) ;; tg-new
                  (getf (rest (getf (varray-meta varray) :gen-meta)) :index-type) ;; tg-new
                  (arg-process varray))))

(defmethod generator-of ((varray vader-turn) &optional indexers params)
  (let ((indexer (indexer-of varray params)))
    (generator-of (vader-base varray)
                  (if (not indexer) indexers (cons indexer indexers))
                  params)))

(defmethod initialize-instance :after ((varray vader-turn) &key)
  "Sum cumulative rotations into a single rotation; currently only works with a scalar left argument."
  (let ((base (vader-base varray))
        (axis (vads-axis varray))
        (argument (vads-argument varray)))
    (when (typep base 'vader-turn)
      (let ((base-axis (vads-axis base))
            (base-arg (vads-argument base))
            (sub-base (vader-base base)))
        (when (or (and (eq :last axis)
                       (eq :last base-axis))
                  (and (numberp axis)
                       (numberp base-axis)
                       (= axis base-axis)))
          (if (and argument base-arg)
              (setf (vads-argument varray) (+ argument base-arg)
                    (vader-base varray) sub-base)))))))

(defclass vader-permute (varray-derived vad-with-io vad-with-argument vad-reindexing)
  ((%is-diagonal :accessor vaperm-is-diagonal
                 :initform nil
                 :initarg :is-diagonal
                 :documentation "Whether this permutation is diagonal."))
  (:metaclass va-class)
  (:documentation "A permuted array as from the [⍉ permute] function."))

(defmethod shape-of ((varray vader-permute))
  "The shape of a permuted array."
  (get-promised
   (varray-shape varray)
   (let* ((base-shape (shape-of (vader-base varray)))
          (argument (setf (vads-argument varray)
                          (render (vads-argument varray))))
          (arg (when argument
                 (if (vectorp argument)
                     (coerce (loop :for a :across argument :collect (max 0 (- a (vads-io varray))))
                             'vector)
                     (- argument (vads-io varray)))))
          (base-rank (length base-shape))
          (odims) (positions) (diagonals))
     (if (not argument)
         (reverse base-shape)
         (progn (setf odims (loop :for i :below base-rank :collect nil))
                (if (vectorp arg)
                    (loop :for i :across arg :for id :in base-shape :for ix :from 0
                          :do (setf (nth i odims) (if (nth i odims)
                                                      (min (nth i odims)
                                                           (nth ix base-shape))
                                                      (nth ix base-shape)))
                              ;; if a duplicate position is found, a diagonal section
                              ;; is being performed
                              (when (not (member i positions))
                                (push i positions))
                              ;; collect possible diagonal indices into diagonal list
                              (if (assoc i diagonals)
                                  (push ix (rest (assoc i diagonals)))
                                  (push (list i ix) diagonals)))
                    (setf odims base-shape
                          positions (cons arg positions)))
                (setf (vaperm-is-diagonal varray)
                      (= base-rank (length positions)))
                (remove nil odims))))))

(defmethod indexer-of ((varray vader-permute) &optional params)
  (if (or (shape-of varray) (getf params :for-selective-assign))
      (let* ((assigning (getf params :for-selective-assign))
             (argument (render (vads-argument varray))))
        (indexer-permute (shape-of (vader-base varray))
                         (shape-of varray)
                         (when argument
                           (if (vectorp argument)
                               (coerce (loop :for a :across argument
                                             :collect (max 0 (- a (vads-io varray))))
                                       'vector)
                               (- argument (vads-io varray))))
                         (not (or (not (vads-argument varray))
                                  (vaperm-is-diagonal varray)))
                         ;; t ;; type of index
                         ;; (or (getf (getf params :meta) :indexer-key) t)
                         ;; (getf (rest (getf (varray-meta varray) :gen-meta)) :index-width) ;; tg-new
                         ;; (getf (rest (getf (varray-meta varray) :gen-meta)) :index-type) ;; tg-new
                         assigning))))

(defmethod generator-of ((varray vader-permute) &optional indexers params)
  (let ((indexer (indexer-of varray params)))
    ;; (print (list :ni indexer indexers params (vader-base varray)))
    (if (getf params :for-selective-assign)
        indexer
        (generator-of (vader-base varray)
                      (if (not indexer) indexers (cons indexer indexers))
                      params))))

(defclass vader-grade (varray-derived vad-with-argument vad-with-io vad-invertable)
  nil (:metaclass va-class)
  (:documentation "A graded array as from the [⍋⍒ grade up/down] functions."))

(defmethod etype-of ((varray vader-grade))
  (let ((this-shape (first (shape-of varray))))
    (list 'integer (vads-io varray)
          (+ this-shape (vads-io varray)))))

(defmethod prototype-of ((varray vader-grade))
  (declare (ignore varray))
  0)

(defmethod shape-of ((varray vader-grade))
  (get-promised (varray-shape varray)
                (let ((base-shape (shape-of (vader-base varray))))
                  (if base-shape (list (first base-shape))
                      (error "The [⍋ grade] function cannot take a scalar argument.")))))

(defmethod generator-of ((varray vader-grade) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let* ((arg-rendered (render (vads-argument varray)))
          (iorigin (vads-io varray))
          (base-rendered (render (vader-base varray))))
     (setf (vader-content varray)
           (if arg-rendered (grade (if (vectorp arg-rendered)
                                       (index-of base-rendered arg-rendered iorigin)
                                       (array-grade arg-rendered base-rendered))
                                   iorigin (alpha-compare (if (vads-inverse varray)
                                                              #'> #'<)))
               (grade base-rendered iorigin (alpha-compare (if (vads-inverse varray)
                                                               #'>= #'<=)))))
     (lambda (index) (aref (vader-content varray) index)))))

(defclass vader-matrix-inverse (varray-derived)
  ((%cached :accessor vaminv-cached
            :initform nil
            :initarg :cached
            :documentation "Cached inverse matrix elements."))
  (:metaclass va-class)
  (:documentation "A matrix-inverse array as from the [⌹ matrix inverse] function."))

(defmethod etype-of ((varray vader-matrix-inverse))
  (declare (ignore varray))
  t)

(defmethod shape-of ((varray vader-matrix-inverse))
  (generator-of varray)
  (shape-of (vaminv-cached varray)))

(defmethod generator-of ((varray vader-matrix-inverse) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let* ((content (when (shape-of (vader-base varray))
                     (or (vaminv-cached varray)
                         (setf (vaminv-cached varray)
                               (funcall (if (and (= 2 (rank-of (vader-base varray)))
                                                 (reduce #'= (shape-of (vader-base varray))))
                                            #'invert-matrix #'left-invert-matrix)
                                        (render (vader-base varray)))))))
          (base-indexer (when (not content) (generator-of (vader-base varray))))
          (content-indexer (when content (generator-of content))))
     (lambda (index)
       (if content (funcall content-indexer index)
           (/ (if (not (functionp base-indexer))
                  base-indexer (funcall base-indexer index))))))))

(defclass vader-matrix-divide (varray-derived vad-with-argument)
  ((%cached :accessor vamdiv-cached
            :initform nil
            :initarg :cached
            :documentation "Cached divided matrix elements."))
  (:metaclass va-class)
  (:documentation "A matrix-divided array as from the [⌹ matrix divide] function."))

(defmethod etype-of ((varray vader-matrix-divide))
  (declare (ignore varray))
  t)

(defmethod shape-of ((varray vader-matrix-divide))
  (generator-of varray)
  (shape-of (vamdiv-cached varray)))

(defmethod generator-of ((varray vader-matrix-divide) &optional indexers params)
  (get-promised (varray-indexer varray)
                (let* ((content (when (shape-of (vader-base varray))
                                  (or (vamdiv-cached varray)
                                      (setf (vamdiv-cached varray)
                                            (array-inner-product
                                             (invert-matrix (render (vader-base varray)))
                                             (render (vads-argument varray))
                                             (lambda (arg1 arg2) (apply-scalar #'* arg1 arg2))
                                             #'+ t)))))
                       (content-indexer (generator-of content)))
                  (lambda (index) (funcall content-indexer index)))))

(defclass vader-encode (varray-derived vad-with-argument vad-invertable)
  nil (:metaclass va-class)
  (:documentation "An encoded array as from the [⊤ encode] function."))

(defmethod shape-of ((varray vader-encode))
  (get-promised (varray-shape varray)
                (append (if (not (and (vads-inverse varray)
                                      (not (shape-of (vads-argument varray)))))
                            (shape-of (vads-argument varray))
                            (let* ((base (render (vader-base varray)))
                                   (max-base (when (not (shape-of base)) base))
                                   (arg (render (vads-argument varray))))
                              (when (not max-base)
                                (setf max-base 0)
                                (dotimes (i (size-of base))
                                  (when (< max-base (row-major-aref base i))
                                    (setf max-base (row-major-aref base i)))))
                              (list (if (zerop max-base)
                                        0 (1+ (floor (log max-base)
                                                     (log (render (vads-argument varray)))))))))
                        (shape-of (vader-base varray)))))

(defmethod generator-of ((varray vader-encode) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let* ((out-dims (shape-of varray))
          (scalar-arg (not (shape-of (vads-argument varray))))
          (adims (if (vads-inverse varray)
                     (or (shape-of (vads-argument varray))
                         (butlast out-dims (rank-of (vader-base varray))))
                     (shape-of (vads-argument varray))))
          (base-shape (shape-of (vader-base varray)))
          (base-indexer (base-indexer-of varray))
          (arg-indexer (generator-of (vads-argument varray)))
          (aifactors (get-dimensional-factors adims))
          (oifactors (get-dimensional-factors base-shape t))
          (ofactors (get-dimensional-factors out-dims t)))
     (lambda (index)
       (let ((remaining index) (base 1) (oindex 0) (afactor 0) (oix 0) (value))
         (loop :for af :in aifactors :for of :across ofactors :for ix :from 0
               :do (multiple-value-bind (this-index remainder) (floor remaining of)
                     (incf oix)
                     (when (not (zerop ix))
                       (setf afactor (+ afactor (* af this-index))))
                     (setf remaining remainder)))
         (loop :for of :across oifactors
               :do (multiple-value-bind (this-index remainder) (floor remaining (aref ofactors oix))
                     (incf oix)
                     (setf oindex (+ oindex (* of this-index))
                           remaining remainder)))
         (setf remaining index
               value (if (not (functionp base-indexer))
                         base-indexer (funcall base-indexer oindex)))

         ;; (print (list :aa arg-indexer value adims (vads-argument varray)))
         (if (and (not (vads-inverse varray)) (not (shape-of (vads-argument varray))))
             (setf value (nth-value 1 (floor value arg-indexer)))
             (let ((last-base) (element) (aindex) (component 1)
                   (this-index (floor index (aref ofactors 0))))
               (loop :for b :from (1- (first adims)) :downto this-index
                     :do (setq last-base base
                               aindex (+ afactor (* b (first aifactors)))
                               base (* base (if (and scalar-arg (vads-inverse varray))
                                                arg-indexer (if (not (functionp arg-indexer))
                                                                arg-indexer (funcall arg-indexer aindex))))
                               component (if (zerop base) value
                                             (nth-value 1 (floor value base)))
                               value (- value component)
                               element (if (zerop last-base) 0
                                           (floor component last-base))))
               (setf value element)))
         value)))))

(defclass vader-decode (varray-derived vad-with-argument)
  nil (:metaclass va-class)
  (:documentation "A decoded array as from the [⊥ decode] function."))

(defmethod etype-of ((varray vader-decode))
  (declare (ignore varray))
  t)

(defmethod shape-of ((varray vader-decode))
  (get-promised (varray-shape varray) (append (butlast (shape-of (vads-argument varray)))
                                              (rest (shape-of (vader-base varray))))))

(defmethod generator-of ((varray vader-decode) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let* ((odims (shape-of (vader-base varray)))
          (adims (shape-of (vads-argument varray)))
          (osize (size-of (vader-base varray)))
          (asize (size-of (vads-argument varray)))
          (base-indexer (base-indexer-of varray))
          (arg-indexer (generator-of (vads-argument varray)))
          (ovector (or (first odims) 1))
          (afactors (make-array (if (and (< 1 osize) (and adims (< 1 (first (last adims)))))
                                    adims (append (butlast adims 1) (list ovector)))
                                :initial-element 1))
          (asegments (reduce #'* (butlast adims)))
          (av2 (or (if (and (< 1 osize)
                            (or (not adims)
                                (= 1 (first (last adims)))))
                       (first odims)
                       (first (last adims)))
                   1))
          (out-section (reduce #'* (rest odims))))

     (when (shape-of varray)
       (dotimes (a asegments)
         (loop :for i :from (- (* av2 (1+ a)) 2) :downto (* av2 a)
               :do (setf (row-major-aref afactors i)
                         (* (if (not (functionp arg-indexer))
                                arg-indexer
                                (funcall arg-indexer (if (not (and (< 1 asize)
                                                                   (< 1 osize)
                                                                   (= 1 (first (last adims)))))
                                                         (1+ i)
                                                         (floor i (or (first odims) 1)))))
                            (row-major-aref afactors (1+ i)))))))
     
     (if (shape-of varray)
         (lambda (i)
           (let ((result 0))
             (loop :for index :below av2
                   :do (incf result (* (if (not (functionp base-indexer))
                                           base-indexer
                                           (funcall base-indexer (mod (+ (mod i out-section)
                                                                         (* out-section index))
                                                                      osize)))
                                       (row-major-aref afactors
                                                       (+ index (* av2 (floor i out-section)))))))
             result))
         (lambda (i)
           (let ((result 0) (factor 1))
             (loop :for i :from (1- (if (< 1 av2) av2 ovector)) :downto 0
                   :do (incf result (* factor (if (not (functionp base-indexer))
                                                  base-indexer (funcall base-indexer
                                                                        (min i (1- ovector))))))
                       (setq factor (* factor (if (not (functionp arg-indexer))
                                                  arg-indexer (funcall arg-indexer
                                                                       (min i (1- av2)))))))
             result))))))

(defclass vader-identity (vad-subrendering varray-derived vad-maybe-shapeless vad-reindexing)
  nil (:metaclass va-class)
  (:documentation "The identity of an array as from the [⊢ identity] function."))

(defmethod prototype-of ((varray vader-identity))
  (prototype-of (vader-base varray)))

(defmethod etype-of ((varray vader-identity))
  (etype-of (vader-base varray)))

(defmethod shape-of ((varray vader-identity))
  (if (vads-shapeset varray)
      (varray-shape varray)
      (let ((shape (shape-of (vader-base varray))))
        (setf (vads-shapeset varray) t
              (varray-shape varray) shape))))

(defmethod generator-of ((varray vader-identity) &optional indexers params)
  (generator-of (vader-base varray) indexers params))

(defgeneric inverse-count-to (array index-origin)
  (:documentation "Invert an [⍳ index] function, returning the right argument passed to ⍳."))

(defmethod inverse-count-to ((item t) index-origin)
  (if (and (vectorp item) (loop :for v :across item :for i :from index-origin
                                :always (and (numberp v) (= v i))))
      (length item)
      (error "Attempted to invoke inverse [⍳ index] on something other than an integer progression vector.")))

(defmethod inverse-count-to ((varray varray-derived) index-origin)
  (inverse-count-to (render varray) index-origin))

(defmethod inverse-count-to ((varray vader-identity) index-origin)
  (inverse-count-to (vader-base varray) index-origin))

(defmethod inverse-count-to ((varray vapri-integer-progression) index-origin)
  ;; TODO: this does not get invoked by for instance ⍳⍣¯1⊢⍳9 because of the identity varray
  (if (and (= 1 (vapip-repeat varray))
           (= 1 (vapip-factor varray))
           (= index-origin (vapip-origin varray)))
      (+ index-origin (vapip-number varray))
      (error "Attempted to invoke inverse [⍳ index] on an altered integer progression vector.")))

(defun side-effect-free (function)
  "Use a function's metadata to check whether it has side effects. Needed for multithreaded operators - the functions composed with operators must be free of side effects for multithreading."
  (let ((fn-meta (handler-case (funcall function :get-metadata)
                   (error () nil))))
    
    (and fn-meta (listp fn-meta)
         (or (member :side-effects fn-meta)
             (member :lexical-reference fn-meta))
         (not (getf fn-meta :side-effects))
         (not (getf fn-meta :side-refs))
         (or (not (getf fn-meta :symfns-called))
             (loop :for fn :in (getf fn-meta :symfns-called)
                   :always (or (not (fboundp fn))
                               (side-effect-free (symbol-function fn))))))))

(defclass vader-composing (varray-derived)
  ((%left  :accessor vacmp-left
           :initform nil
           :initarg :left
           :documentation "Left function composed with operator.")
   (%right :accessor vacmp-right
           :initform nil
           :initarg :right
           :documentation "Right function composed with operator.")
   (%omega :accessor vacmp-omega
           :initform nil
           :initarg :omega
           :documentation "Right argument to composed function.")
   (%alpha :accessor vacmp-alpha
           :initform nil
           :initarg :alpha
           :documentation "Left argument to composed function.")
   (%threadable :accessor vacmp-threadable
                :initform t
                :initarg :threadable
                :documentation "Whether this operation is threadable; i.e. operand functions lack side effects."))
  (:metaclass va-class)
  (:documentation "An array produced by an operator-composed function."))

(defmethod etype-of ((varray vader-composing))
  (declare (ignore varray))
  t)

(defun op-compose (type &rest args)
  (when (getf args :axis)
    (setf (getf args :axis) (first (getf args :axis))))
  (labels ((this (omega &optional alpha)
             (if (eq :reassign-axes omega)
                 (let ((last-key))
                   (setf args (loop :for a :in args :collect (if (not (eq :axis last-key))
                                                                 a (first alpha))
                                    :do (setf last-key a)))
                   #'this)
                 (if (not (eq :get-metadata omega))
                     (apply #'make-instance type :omega omega :alpha alpha args)
                     (if (getf args :right)
                         (append (list :operator-reference type)
                                 (list :left-meta (funcall (getf args :left) :get-metadata))
                                 (list :right-meta (funcall (getf args :right) :get-metadata)))
                         (append (list :operator-reference type)
                                 (funcall (getf args :left) :get-metadata)))))))
    #'this))

(defclass vacomp-reduce (vad-subrendering vader-composing vad-on-axis vad-with-io vad-with-default-axis)
  ((%unitary :accessor vacred-unitary
             :initform nil
             :initarg :unitary
             :documentation "Whether the array measures only one unit along the axis to be reduced."))
  (:metaclass va-class)
  (:documentation "A reduce-composed array as with the [/ reduce] operator."))

(defmethod prototype-of ((varray vacomp-reduce))
  0)

(defmethod shape-of ((varray vacomp-reduce))
  (get-promised (varray-shape varray)
                (progn
                  (when (and (vads-default-axis varray)
                             (not (vads-axis varray)))
                    (setf (vads-axis varray) (vads-default-axis varray)))
                  (let* ((base-shape (shape-of (vacmp-omega varray)))
                         ;; (base-size (size-of (vacmp-omega varray)))
                         (window (when (vacmp-alpha varray)
                                   (setf (vacmp-alpha varray)
                                         (disclose-unitary (render (vacmp-alpha varray))))))
                         (window (when window (abs window)))
                         (axis (setf (vads-axis varray)
                                     ;; TODO: what's sending an ⎕IO of 0 here for ⌊10000×+∘÷/40/1 ?
                                     (max 0 (if (vads-axis varray)
                                                (- (vads-axis varray) (vads-io varray))
                                                (1- (length base-shape))))))
                         (output))
                    (loop :for b :in base-shape :for ix :from 0 :when (/= ix axis) :collect b
                          :when (and (= ix axis) (= 1 b)) :do (setf (vacred-unitary varray) t)
                            :when (and window (= ix axis)) :collect (- b (1- window)))))))

(defclass vader-subarray-reduce (vader-subarray)
  ((%window :accessor vasbr-window
            :initform nil
            :initarg :window
            :documentation "Window length for subarray to reduce.")
   (%delta :accessor vasbr-delta
           :initform nil
           :initarg :delta
           :documentation "Delta for subarray to reduce.")
   (%reverse :accessor vasbr-reverse
             :initform nil
             :initarg :reverse
             :documentation "Is subarray to be traversed in reverse order?."))
  (:metaclass va-class)
  (:documentation "An element of a split array as from the [↓ split] function."))

(defmethod prototype-of ((varray vader-subarray-reduce))
  (declare (ignore params))
  (get-promised (varray-prototype varray)
                (let* ((generator (generator-of varray))
                       (first-item (funcall generator 0)))
                  (if (varrayp first-item) (prototype-of first-item)
                      (apl-array-prototype first-item)))))

(defmethod generator-of ((varray vader-subarray-reduce) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let ((base-indexer (generator-of (vader-base varray) indexers params)))
     (if (vasbr-reverse varray)
         (let ((this-length (1- (first (shape-of varray)))))
           (lambda (index)
             (funcall base-indexer (+ (* (- this-length index) (vasv-index varray))
                                      (vasbr-delta varray)))))
         (lambda (index)
           (funcall base-indexer (+ (* index (vasv-index varray))
                                    (vasbr-delta varray))))))))

(defmethod generator-of ((varray vacomp-reduce) &optional indexers params)
  "Reduce an array along by a given function along a given dimension, optionally with a window interval."
  (get-promised
   (varray-indexer varray)
   (progn
     ;; (if (not (vads-default-axis varray))
     ;;     (print (list :ty (type-of (vacmp-omega varray)))))
     ;; (setf (vacmp-omega varray) (render (vacmp-omega varray)))
     ;; (print (list :om (vacmp-omega varray) varray))
     (let ((irank (rank-of (vacmp-omega varray)))
           (osize (size-of (vacmp-omega varray))))
       (if (or (> 2 osize) (vacred-unitary varray))
           (if (zerop irank) (vacmp-omega varray)
               ;; return just the omega indexer in cases like +/2 2 1⍴'a'
               (if (zerop osize)
                   (let ((fn-meta (funcall (vacmp-left varray) :get-metadata nil)))
                     (or (getf fn-meta :id)
                         (error "Attempted to [/ reduce] with a function that has no identity value.")))
                   (generator-of (vacmp-omega varray))))
           (let* ((odims (shape-of (vacmp-omega varray)))
                  (omega-indexer (generator-of (vacmp-omega varray)))
                  (out-dims (shape-of varray))
                  (axis (or (vads-axis varray) (1- (length odims))))
                  (rlen (nth axis odims))
                  (increment (reduce #'* (nthcdr (1+ axis) odims)))
                  (window (vacmp-alpha varray))
                  (window-reversed (and window (> 0 window)))
                  (window (when window (abs window)))
                  (wsegment)
                  (non-nested (not (eq t (etype-of (vacmp-omega varray)))))
                  (fn-meta (funcall (vacmp-left varray) :get-metadata nil))
                  (scalar-fn (and (getf fn-meta :scalar)
                                  (not (getf fn-meta :operator-reference))))
                  (catenate-fn (and (getf fn-meta :lexical-reference)
                                    (not (getf fn-meta :operator-reference))
                                    (member (getf fn-meta :lexical-reference)
                                            '(#\, #\⍪ #\∩) :test #'char=)))
                  (ax-interval (or window rlen))
                  (increment-diff (- (* increment rlen) increment))
                  ;; TODO: create a better way to determine the catenate function
                  )
             ;; (print (list :fm fn-meta osize (vacmp-omega varray)))
             ;; (print (list :sc scalar-fn (vacmp-left varray)
             ;;              (vads-default-axis varray)
             ;;              (list :ax (vads-axis varray))
             ;;              out-dims axis))
             (loop :for dim :in odims :for dx :from 0
                   :when (and window (= dx axis))
                     :do (setq wsegment (- dim (1- window))))
             ;; (print (list :ws rlen (funcall (vacmp-left varray) :get-metadata nil)))
             (cond
               ((and (or scalar-fn (and catenate-fn (not window)))
                     (not out-dims) (arrayp (vacmp-omega varray)))
                ;; reverse the argument vector in the case of a scalar function;
                ;; this also applies in the case of the next two clauses
                (lambda (i)
                  (declare (optimize (safety 1)))
                  (let* ((output (funcall (vacmp-left varray)
                                          :arg-vector (funcall (if scalar-fn #'reverse #'identity)
                                                               (vacmp-omega varray))))
                         (out-indexer (generator-of output)))
                    ;; pass the indexer through for a shapeless output as from +/⍳5;
                    ;; pass the output object through for an output with a shape as from +/(1 2 3)(4 5 6)
                    (if (shape-of output)
                        output (if (not (functionp out-indexer))
                                   out-indexer (funcall out-indexer i))))))
               ((and (or scalar-fn (and catenate-fn (not window)))
                     (= (- axis (vads-io varray))
                        (length out-dims))
                     (arrayp (vacmp-omega varray)))
                ;; (print (list 11 axis out-dims (vacmp-omega varray)))
                (lambda (i)
                  (declare (optimize (safety 1)))
                  (funcall (vacmp-left varray)
                           :arg-vector (funcall (if scalar-fn #'reverse #'identity)
                                                (make-array
                                                 rlen :element-type (etype-of (vacmp-omega varray))
                                                      :displaced-to (vacmp-omega varray)
                                                      :displaced-index-offset (* i rlen))))))
               (t (flet ((process-item (i ix delta)
                           (if (not (functionp omega-indexer))
                               omega-indexer (funcall omega-indexer (+ delta (* ix increment))))))
                    ;; (print :gg)
                    ;; (print (list :tt window (type-of (vacmp-omega varray))
                    ;;              (vacmp-omega varray)
                    ;;              (or scalar-fn catenate-fn)))
                    (if (or scalar-fn catenate-fn)
                        (lambda (i)
                          ;; in the case of a scalar function that can take entire vectors as an argument
                          (declare (optimize (safety 1)))
                          (let ((valix 0)
                                (value (make-array ax-interval))
                                (delta (+ (if window (* rlen (floor i wsegment))
                                              (if (= 1 increment)
                                                  0 (* increment-diff (floor i increment))))
                                          (if (/= 1 increment) i
                                              (if window (if (>= 1 irank) i (mod i wsegment))
                                                  (* i rlen))))))
                            ;; (print (list 13 axis out-dims (vacmp-omega varray) value valix))
                            
                            (if window-reversed
                                (loop :for ix :below window
                                      :do (setf (aref value (- ax-interval (incf valix)))
                                                (process-item i ix delta)))
                                (loop :for ix :from (1- ax-interval) :downto 0
                                      :do (setf (aref value (- ax-interval (incf valix)))
                                                (process-item i ix delta))))
                            ;; (print (list :ll value))
                            (setf value (funcall (vacmp-left varray)
                                                 :arg-vector (funcall (if scalar-fn #'reverse #'identity)
                                                                      value)))
                            value
                            
                            ;; (print (list :ren delta ax-interval
                            ;;              window scalar-fn
                            ;;              (render (make-instance
                            ;;                       'vader-subarray-reduce
                            ;;                       :delta delta :index increment
                            ;;                       :base-indexer omega-indexer :reverse scalar-fn
                            ;;                       :window window :shape (list ax-interval)))))
                            
                            ;; (funcall (vacmp-left varray)
                            ;;          :arg-vector 
                            ;;          (make-instance
                            ;;           'vader-subarray-reduce
                            ;;           :delta delta :index increment :base omega
                            ;;           :window window :shape (list ax-interval) :reverse scalar-fn))
                            ))
                        (lambda (i) ;; in the case of other functions
                          (declare (optimize (safety 1)))
                          (let ((value)
                                (delta (+ (if window (* rlen (floor i wsegment))
                                              (if (= 1 increment)
                                                  0 (* (floor i increment)
                                                       (- (* increment rlen) increment))))
                                          (if (/= 1 increment) i
                                              (if window (if (>= 1 irank) i (mod i wsegment))
                                                  (* i rlen))))))
                            ;; (print (list 13 axis out-dims (vacmp-omega varray) value valix))
                            ;; (print (list :vv value))
                            ;; (print (list :win window window-reversed))
                            (if window-reversed (loop :for ix :below window
                                                      :do (let ((item (process-item i ix delta)))
                                                            (setq value (if (not value) item
                                                                            (funcall (vacmp-left varray)
                                                                                     value item)))))
                                (loop :for ix :from (1- ax-interval) :downto 0
                                      :do (let ((item (process-item i ix delta)))
                                            (setq value (if (not value) item
                                                            (funcall (vacmp-left varray)
                                                                     value item))))))
                            (when (typep value 'vacomp-reduce)
                              (setf (vads-subrendering varray) nil))
                            value))))))))))))

(defclass vacomp-each (vad-subrendering vader-composing)
  nil (:metaclass va-class)
  (:documentation "An each-composed array as with the [¨ each] operator."))

(defmethod prototype-of ((varray vacomp-each))
  0)

(defmethod shape-of ((varray vacomp-each))
  (get-promised (varray-shape varray)
                (let* ((oshape (shape-of (vacmp-omega varray)))
                       (ashape (shape-of (vacmp-alpha varray)))
                       (osize (reduce #'* oshape))
                       (asize (reduce #'* ashape)))
                  (if oshape (if (not ashape)
                                 oshape (if (= 1 osize)
                                            (if (zerop asize) oshape ashape)
                                            ;; if one size is 0, the shape is 0
                                            (if (zerop osize) (list 0)
                                                (if (or (= 1 asize)
                                                        (and (= (length oshape) (length ashape))
                                                             (loop :for o :in oshape :for a :in ashape
                                                                   :always (= o a))))
                                                    oshape (error "Shapes must match.")))))
                          ashape))))

(defmethod generator-of ((varray vacomp-each) &optional indexers params)
  (get-promised (varray-indexer varray)
                (let ((this-shape (shape-of varray))
                      (oshape (shape-of (vacmp-omega varray)))
                      (ashape (shape-of (vacmp-alpha varray)))
                      (oindexer (generator-of (vacmp-omega varray)))
                      (aindexer (generator-of (vacmp-alpha varray)))
                      (threaded (side-effect-free (vacmp-left varray))))
                  (when (not threaded) (setf (vacmp-threadable varray) nil))
                  (lambda (index)
                    ;; (print (list :in index oindexer aindexer (vacmp-omega varray)
                    ;;              (vacmp-alpha varray) oindexer aindexer (funcall oindexer index)
                    ;;              ))
                    ;; (print (list :rr (shape-of varray)
                    ;;              (vacmp-omega varray)
                    ;;              (render (funcall oindexer (if oshape index 0)))))
                    (if (vacmp-alpha varray)
                        (funcall (vacmp-left varray)
                                 (if (not (functionp oindexer))
                                     oindexer (funcall oindexer (if oshape index 0)))
                                 (if (not (functionp aindexer))
                                     aindexer (funcall aindexer (if ashape index 0))))
                        (funcall (vacmp-left varray)
                                 (if (not (functionp oindexer))
                                     oindexer (funcall oindexer (if oshape index 0)))))))))

(defclass vacomp-produce (vad-subrendering vader-composing vad-with-io vad-with-default-axis)
  nil (:metaclass va-class)
  (:documentation "A reduce-composed array as with the [/ reduce] operator."))

(defmethod prototype-of ((varray vacomp-produce))
  (let ((this-indexer (generator-of varray)))
    (if (zerop (size-of varray))
        ;; TODO: should prototype reflect both arguments?
        (prototype-of (vacmp-alpha varray))
        (prototype-of (funcall this-indexer 0)))))

(defmethod shape-of ((varray vacomp-produce))
  (get-promised (varray-shape varray)
                (let ((left (vacmp-left varray))
                      (adims (shape-of (vacmp-alpha varray)))
                      (odims (shape-of (vacmp-omega varray))))
                  (if (eq :outer left) (append adims odims)
                      (append (butlast adims) (rest odims))))))

(defclass vader-subarray-displaced (vader-subarray)
  nil (:metaclass va-class)
  (:documentation "An element of a split array as from the [↓ split] function."))

(defmethod prototype-of ((varray vader-subarray-displaced))
  (declare (ignore params))
  (get-promised (varray-prototype varray)
                (let* ((gen (generator-of varray))
                       (first-item (if (not (functionp gen))
                                       gen (funcall (generator-of varray) 0))))
                  ;; (print (list :fi first-item))
                  (if (varrayp first-item) (prototype-of first-item)
                      (apl-array-prototype first-item)))))

(defmethod generator-of ((varray vader-subarray-displaced) &optional indexers params)
  (declare (ignore params))
  (get-promised (varray-indexer varray)
                (let ((interval (reduce #'* (shape-of varray)))
                      (base-indexer (generator-of (vader-base varray) indexers params)))
                  (if (not (functionp base-indexer))
                      base-indexer (lambda (index)
                                     (funcall base-indexer
                                              (+ index (* interval (vasv-index varray)))))))))

(defmethod generator-of ((varray vacomp-produce) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let* ((omega (vacmp-omega varray))
          (alpha (vacmp-alpha varray))
          (osize (size-of omega)) (asize (size-of alpha))
          (is-outer (eq :outer (vacmp-left varray)))
          (oindexer (generator-of omega))
          (aindexer (generator-of alpha))
          (orank (rank-of omega)) (arank (rank-of alpha))
          (oscalar (when (not (shape-of omega))
                     (if (not (functionp oindexer))
                         oindexer (funcall oindexer 0))))
          (ascalar (when (not (shape-of alpha))
                     (if (not (functionp aindexer))
                         aindexer (funcall aindexer 0)))))
     (if is-outer
         (lambda (index)
           ;; (print (list :ra (render (or oscalar (funcall oindexer (mod index osize))))
           ;;              (render (or ascalar (funcall aindexer (floor index osize))))))
           (funcall (vacmp-right varray) (or oscalar (funcall oindexer (mod index osize)))
                    (or ascalar (funcall aindexer (floor index osize))))
           ;; (make-instance 'vacomp-each :left (vacmp-right varray)
           ;;                             :omega (or oscalar (funcall oindexer (mod index osize)))
           ;;                             :alpha (or ascalar (funcall aindexer (floor index osize))))
           )
         (let* ((odims (shape-of omega))
                (asegment (first (last (shape-of alpha))))
                (osegment (if (not (and asegment (first odims) (/= asegment (first odims))))
                              (first odims)
                              (error "To find an inner product the last dimension of the left array ~w"
                                     "must equal the first dimension of the right array.")))
                (ashape (when asegment (list asegment)))
                (oshape (when osegment (list osegment)))
                (ovectors (reduce #'* (rest odims))))
           ;; (print (list :om omega alpha (render omega) oshape ashape (shape-of varray)))
           (if (or (zerop osize) (zerop asize))
               (if (or (< 1 orank) (< 1 arank))
                   (lambda (i) (declare (ignore i)) (vector))
                   ;; inner product with an empty array of rank > 1 gives an empty vector
                   (or (let ((identity (getf (funcall (vacmp-left varray) :get-metadata nil)
                                             :id)))
                         (lambda (i)
                           (declare (ignore i))
                           (if (functionp identity) (funcall identity) identity)))
                       (error "Left operand given to [. inner product] has no identity.")))
               (lambda (index)
                 (let* ((avix (floor index ovectors))
                        (ovix (mod index ovectors)))
                   ;; (oholder (if odims (if (and (= osegment osize) (= 1 (rank omega)))
                   ;;                        omega (make-array osegment :element-type
                   ;;                                          (element-type omega)))))
                   ;; (adisp (make-instance 'vader-subarray-displaced
                   ;;                       :base omega :index avix :shape (list asegment)))
                   ;; (print (list :as ashape asegment))
                   (if (= 0 orank arank)
                       (funcall (vacmp-right varray) omega alpha)
                       (make-instance
                        'vader-pick
                        :base (make-instance 'vacomp-reduce
                                             :left (vacmp-left varray) :index-origin 1
                                             :omega
                                             (make-instance
                                              'vacomp-each
                                              :left (vacmp-right varray)
                                              :omega (if (not (functionp oindexer))
                                                         oindexer
                                                         (make-instance
                                                          'vader-subarray-reduce
                                                          :delta ovix :index ovectors
                                                          :base omega :shape oshape))
                                              :alpha (if (not (functionp aindexer))
                                                         aindexer
                                                         (make-instance 'vader-subarray-displaced
                                                                        :base alpha :index avix
                                                                        :shape ashape)))
                                             ;; (funcall (vacmp-right varray)
                                             ;;          (if (not (functionp oindexer))
                                             ;;              oindexer
                                             ;;              (make-instance
                                             ;;               'vader-subarray-reduce
                                             ;;               :delta ovix :index ovectors
                                             ;;               :base omega :shape oshape))
                                             ;;          (if (not (functionp aindexer))
                                             ;;              aindexer
                                             ;;              (make-instance 'vader-subarray-displaced
                                             ;;                             :base alpha :index avix :shape ashape)))

                                             )))))))))))

(defclass vacomp-stencil (vad-subrendering vader-composing vad-with-io)
  ((%base-dims :accessor vacst-base-dims
               :initform nil
               :initarg :base-dims
               :documentation "Vector of base array's dimensions.")
   (%win-dims :accessor vacst-win-dims
              :initform nil
              :initarg :win-dims
              :documentation "Dimensions for windows within stencil.")
   (%win-offsets :accessor vacst-win-offsets
                 :initform nil
                 :initarg :win-offsets
                 :documentation "Space offsets for windows within stencil.")
   (%win-factors :accessor vacst-win-factors
                 :initform nil
                 :initarg :win-factors
                 :documentation "Dimensional factors for stencil window.")
   (%movement :accessor vacst-movement
              :initform nil
              :initarg :movement
              :documentation "Stencil's movement parameters.")
   (%in-factors :accessor vacst-in-factors
                :initform nil
                :initarg :in-factors
                :documentation "Dimensional factors for stencil input.")
   (%out-factors :accessor vacst-out-factors
                 :initform nil
                 :initarg :out-factors
                 :documentation "Dimensional factors for stencil output."))
  (:metaclass va-class)
  (:documentation "A stencil-composed array as with the [⌺ stencil] operator."))

(defmethod prototype-of ((varray vacomp-stencil))
  (let ((this-indexer (generator-of varray)))
    (if t ; (zerop (size-of varray))
        ;; TODO: should prototype reflect both arguments?
        (prototype-of (vacmp-omega varray))
        ;; (prototype-of (funcall this-indexer 0))
        )))

(defmethod shape-of ((varray vacomp-stencil))
  (get-promised
   (varray-shape varray)
   (let* ((right-value (render (vacmp-right varray)))
          (left-function (vacmp-left varray))
          (omega-shape (shape-of (vacmp-omega varray)))
          (omega-rank (rank-of (vacmp-omega varray))))
     (flet ((get-row (matrix index)
              (let ((row-length (first (last (shape-of matrix)))))
                (coerce (loop :for x :below row-length
                              :collect (row-major-aref matrix (+ x (* index row-length))))
                        'vector))))
       (if (not (or (and (< 2 (rank-of right-value))
                         (error "The right operand of [⌺ stencil] may not have more than 2 dimensions."))
                    (and (not left-function)
                         (error "The left operand of [⌺ stencil] must be a function."))))
           (let ((idims (shape-of (vacmp-omega varray)))
                 (window-dims (if (not (arrayp right-value))
                                  (vector right-value)
                                  (if (= 1 (rank-of right-value))
                                      right-value (get-row right-value 0))))
                 (movement (if (not (arrayp right-value))
                               (vector 1)
                               (if (= 2 (rank-of right-value)) (get-row right-value 1)
                                   (make-array (length right-value) :element-type 'fixnum
                                                                    :initial-element 1)))))
             (setf (vacst-movement varray)
                   (if (= omega-rank (length movement))
                       movement (coerce (loop :for s :in omega-shape :for i :from 0
                                              :collect (if (>= i (length window-dims))
                                                           0 (aref movement i)))
                                        'vector))
                   (vacst-win-dims varray)
                   (if (= omega-rank (length window-dims))
                       window-dims (coerce (loop :for s :in omega-shape :for i :from 0
                                                 :collect (if (>= i (length window-dims))
                                                              s (aref window-dims i)))
                                           'vector))
                   (vacst-base-dims varray) (coerce omega-shape 'vector))
             (loop :for dim :below (length window-dims) :for dx :from 0
                   :collect (ceiling (- (/ (nth dim idims) (aref movement dim))
                                        (if (and (evenp (aref window-dims dim))
                                                 (or (= 1 (aref movement dim))
                                                     (oddp (nth dim idims))))
                                            1 0))))))))))

(defclass vader-stencil-window (varray-derived)
  ((%index :accessor vastw-index
           :initform nil
           :initarg :index
           :documentation "Index of stencil window."))
  (:metaclass va-class)
  (:documentation "A stencil window, part of a stencil array produced by [⌺ stencil]."))

(defmethod prototype-of ((varray vader-stencil-window))
  (prototype-of (vacmp-omega (vader-base varray)))) ;; TODO: fix this to proto from individual frame

(defmethod generator-of ((varray vader-stencil-window) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let* ((base-indexer (generator-of (vacmp-omega (vader-base varray))))
          (idims (apply #'vector (shape-of (vacmp-omega (vader-base varray)))))
          (this-rank (rank-of varray))
          (prototype (prototype-of varray))
          (movement (vacst-movement (vader-base varray)))
          (in-factors (vacst-in-factors (vader-base varray)))
          (win-factors (vacst-win-factors (vader-base varray)))
          (win-offsets (vacst-win-offsets (vader-base varray)))
          (oindices (make-array this-rank :element-type 'fixnum :initial-element 0)))

     (let ((remaining (vastw-index varray)))
       (loop :for of :across (vacst-out-factors (vader-base varray)) :for i :from 0
             :do (multiple-value-bind (index remainder) (floor remaining of)
                   (setf (aref oindices i) index
                         remaining remainder))))
     
     (lambda (index)
       (let ((remaining index) (rmi 0) (valid t) (ox 0))
         (loop :while valid :for mv :across movement :for idim :across idims
               :for if :across in-factors :for wf :across win-factors :for wo :across win-offsets
               :do (multiple-value-bind (this-index remainder) (floor remaining wf)
                     (let* ((static (zerop mv))
                            (a-index (+ this-index (if static 0 (- (* mv (aref oindices ox)) wo)))))
                       ;; 0 if no borders needed
                       (incf rmi (* if a-index))
                       (setf remaining remainder)
                       (when (not static) (incf ox)
                             (when (not (< -1 a-index idim))
                               (setf valid nil))))))
         (if (not valid) prototype (funcall base-indexer rmi)))))))

(defclass vader-stencil-margin (varray-derived vad-with-argument)
  ((%index :accessor vaste-index
           :initform nil
           :initarg :index
           :documentation "Index of stencil margin vector."))
  (:metaclass va-class)
  (:documentation "A stencil margin vector, enumerating the margin padding in a stencil window array produced by [⌺ stencil]."))

(defmethod prototype-of ((varray vader-stencil-margin))
  (declare (ignore varray))
  0)

(defmethod etype-of ((varray vader-stencil-margin))
  (declare (ignore varray))
  ;; '(signed-byte 8)
  'fixnum
  ) ;; 8-bit elements for efficiency - TODO: is a different type better?

(defmethod generator-of ((varray vader-stencil-margin) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (let* ((base-dims (vacst-base-dims (vader-base varray)))
          (win-index (vaste-index varray))
          (win-dims (vacst-win-dims (vader-base varray)))
          (win-factors (vacst-win-factors (vader-base varray)))
          (win-offsets (vacst-win-offsets (vader-base varray))))
     (lambda (index)
       (let ((from-start (- (* (if (zerop index) (floor win-index (aref win-factors index))
                                   (mod win-index (aref win-dims index)))
                               (aref (vacst-movement (vader-base varray)) index))
                            (aref win-offsets index))))
         (- (if (not (zerop (min 0 from-start)))
                from-start (max 0 (- (+ from-start (aref win-dims index))
                                     (aref base-dims index))))))))))

(defmethod generator-of ((varray vacomp-stencil) &optional indexers params)
  (get-promised
   (varray-indexer varray)
   (progn
     (setf (vacmp-omega varray) (render (vacmp-omega varray)))
     (let* ((irank (rank-of (vacmp-omega varray)))
            (idims (apply #'vector (shape-of (vacmp-omega varray))))
            (this-shape (shape-of varray)) ;; must derive shape before fetching window-dims
            (this-rank (rank-of varray))
            (window-dims (vacst-win-dims varray)) (wrank (length window-dims))
            (win-offsets (or (vacst-win-offsets varray)
                             (setf (vacst-win-offsets varray)
                                   (make-array wrank :element-type 'fixnum :initial-element 0))))
            (in-factors (or (vacst-in-factors varray)
                            (setf (vacst-in-factors varray)
                                  (make-array irank :element-type 'fixnum :initial-element 0))))
            (out-factors (or (vacst-out-factors varray)
                             (setf (vacst-out-factors varray)
                                   (make-array this-rank :element-type 'fixnum :initial-element 0))))
            (win-factors (or (vacst-win-factors varray)
                             (setf (vacst-win-factors varray)
                                   (make-array wrank :element-type 'fixnum :initial-element 0))))
            (edge-shape (list (rank-of varray)))
            (wd-list) (last-dim))
       
       ;; generate factor vector for window dimensions
       (loop :for dx :below (length window-dims)
             :do (let ((d (aref window-dims (- wrank 1 dx))))
                   (setf (aref win-factors (- wrank 1 dx))
                         (if (zerop dx) 1 (* last-dim (aref win-factors (- wrank dx))))
                         wd-list (cons d wd-list)
                         last-dim d)))

       ;; generate dimensional factors vector for input
       (loop :for dx :below irank
             :do (let ((d (aref idims (- irank 1 dx))))
                   (setf (aref in-factors (- irank 1 dx))
                         (if (zerop dx) 1 (* last-dim (aref in-factors (- irank dx))))
                         last-dim d)))
       
       ;; generate dimensional factors vector for output
       (loop :for d :in (reverse this-shape) :for dx :from 0
             :do (setf (aref out-factors (- this-rank 1 dx))
                       (if (zerop dx) 1 (* last-dim (aref out-factors (- this-rank dx))))
                       last-dim d))

       ;; generate offsets determining 0-spacing at edges
       (loop :for wd :across window-dims :for i :from 0
             :do (setf (aref win-offsets i) (floor (- wd (if (evenp wd) 1 0)) 2)))

       (lambda (index)
         (funcall (vacmp-left varray) (make-instance 'vader-stencil-window
                                                     :shape wd-list :base varray :index index)
                  (make-instance 'vader-stencil-margin :shape edge-shape :base varray :index index)))))))

;; (print (render (make-instance 'vacomp-each
;;                               :left (vacmp-right varray)
;;                               :omega (if (not (functionp oindexer))
;;                                 oindexer
;;                                 (make-instance
;;                                  'vader-subarray-reduce
;;                                  :delta ovix :index ovectors
;;                                  :base omega :shape oshape))
;;                               :alpha (if (not (functionp aindexer))
;;                                 aindexer
;;                                 (make-instance 'vader-subarray-displaced
;;                                                :base alpha :index avix :shape ashape)))))

;; (1 2 3) (2 3 4)∘.⌽[1]⊂3 3⍴⍳9 NOT IN DYALOG?
