;;;; setup.lisp

(in-package #:varray)

;; specialized types for April virtual arrays
(deftype ava-worker-count () `(integer 0 ,(max 1 (1- (serapeum:count-cpus :default 2)))))
(deftype ava-rank () `(integer 0 ,(1- array-rank-limit)))
(deftype ava-dimension () `(integer 0 ,(1- array-dimension-limit)))
(deftype ava-size () `(integer 0 ,(1- array-total-size-limit)))

(defparameter *workers-count* (max 1 (1- (serapeum:count-cpus :default 2))))

(defmacro intraverser-ex (params function)
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
                        ;; width to base width, as needed for functions like ⌽, where movement along an axis
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

(let ((indexer-table-encoded
        (intraverser-ex
         (:eindex-width +eindex-width+ :cindex-width +cindex-width+ :rank-width +rank-width+
          :sub-base-width +sub-base-width+ :address-fraction +address-fraction+)
         (the (function ((unsigned-byte +sub-base-width+) (unsigned-byte +sub-base-width+))
                        function)
              (lambda (degrees rlen)
                (declare (optimize (speed 3) (safety 0))
                         (type (unsigned-byte +sub-base-width+) degrees rlen))
                (let ((byte-offset (* +cindex-width+ dindex)))
                (print (list :fn degrees rlen +cindex-width+ +eindex-width+ +address-fraction+))
                (the (function ((unsigned-byte +eindex-width+)) (unsigned-byte +eindex-width+))
                     (lambda (i)
                       (declare (type (unsigned-byte +eindex-width+) i))
                       (let ((iindex (the (unsigned-byte +cindex-width+)
                                          (ldb (byte +cindex-width+ +address-fraction+)
                                               i))))
                         (the (unsigned-byte +eindex-width+)
                              (dpb (the (unsigned-byte +cindex-width+)
                                        (incf iindex degrees))
                                   (byte +cindex-width+ +address-fraction+)
                                   i))))))))))
  
(defun indexer-section (dims span pad is-inverse output-shorter)
  "Return indices of an array sectioned as with the [↑ take] or [↓ drop] functions."
  ;; (print (list :is inverse dims dimensions output-shorter span padding))
  (let* ((scalar (not dims))
         (dims (or dims '(1)))
         (isize (reduce #'* dims)) (irank (length dims))
         (idims (make-array irank :element-type (if (zerop isize) t (list 'integer 0 isize))
                                  :initial-contents dims))
         (odims (loop :for ix :below irank :for sp :across span
                      :collect (+ (- (aref span (+ ix irank)) sp)
                                  (aref pad ix)
                                  (aref pad (+ ix irank)))))
         (osize (reduce #'* odims))
         (last-dim)
         (id-factors (make-array irank :element-type 'fixnum))
         (od-factors (make-array irank :element-type 'fixnum)))
    
    ;; generate dimensional factors vectors for input and output
    (loop :for dx :below irank
          :do (let ((d (aref idims (- irank 1 dx))))
                (setf (aref id-factors (- irank 1 dx))
                      (if (zerop dx) 1 (* last-dim (aref id-factors (- irank dx))))
                      last-dim d)))

    (loop :for d :in (reverse odims) :for dx :from 0
          :do (setf (aref od-factors (- irank 1 dx))
                    (if (zerop dx) 1 (* last-dim (aref od-factors (- irank dx))))
                    last-dim d))
    ;; (print (list :pad odims irank dims span pad idims odims id-factors od-factors))
    (if output-shorter
        ;; choose shorter path depending on whether input or output are larger, and
        ;; always iterate over output in the case of sub-7-bit arrays as this is necessary
        ;; to respect the segmentation of the elements
        (lambda (i)
          (let ((oindex 0) (remaining i) (valid t))
            ;; calculate row-major offset for outer array dimensions
            (loop :for i :below irank :while valid :for od :in odims
                  :for ifactor :across id-factors :for ofactor :across od-factors
                  :do (multiple-value-bind (index remainder) (floor remaining ifactor)
                        (let ((adj-index (- index (aref span i))))
                          (setf valid (when (< -1 adj-index od)
                                        (incf oindex (* ofactor adj-index))
                                        (setq remaining remainder))))))
            (when valid oindex)))
        (lambda (i)
          (let ((iindex 0) (remaining i) (valid t))
            ;; calculate row-major offset for outer array dimensions
            (loop :for i :below irank :while valid :for id :across idims
                  :for ifactor :across id-factors :for ofactor :across od-factors
                  :do (multiple-value-bind (index remainder) (floor remaining ofactor)
                        (let ((adj-index (+ (aref span i)
                                            (- index (aref pad i)))))
                          (setf valid (when (< -1 adj-index (aref span (+ i irank)))
                                        (incf iindex (* ifactor adj-index))
                                        (setq remaining remainder))))))
            (when valid iindex))))))

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
      (function-table
        (intraverser-ex
         (:eindex-width +eindex-width+ :cindex-width +cindex-width+ :rank-width +rank-width+
          :sub-base-width +sub-base-width+ :address-fraction +address-fraction+)
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
                                   i))))))))))
  
  (defun indexer-turn (axis idims iwidth itype &optional degrees)
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
              (list (let ((match (gethash (list iwidth itype (- irank 1 axis))
                                          function-table)))
                      (when match (funcall match degrees rlen)))
                    (funcall default-function increment vset-size degrees rlen)))
          (lambda (i)
            (+ (mod i increment)
               (* vset-size (floor i vset-size))
               (* increment (abs (- (mod (floor i increment) rlen)
                                    (1- rlen))))))))))

(let ((regular-handler-table
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
         (:eindex-width +eindex-width+ :cindex-width +cindex-width+
          :rank-plus +rank-plus+ :rank-width +rank-width+)
         (the (function ((simple-array (unsigned-byte 8) (+rank-plus+)))
                         function)
               (lambda (indices)
                 (declare (optimize (speed 3) (safety 0))
                          (type (simple-array (unsigned-byte 8) (+rank-plus+))
                                indices)) ;; TODO: fix hardcoded type
                 ;; (print (list :in indices))
                 (the (function ((unsigned-byte +eindex-width+)) (unsigned-byte +eindex-width+))
                      (lambda (i)
                        (declare (type (unsigned-byte +eindex-width+) i))
                        (let ((iindex (the (unsigned-byte +eindex-width+) 0)))
                          (loop :for a :of-type (unsigned-byte +cindex-width+) :across indices
                                :for n :of-type (unsigned-byte 8)
                                := (1- +rank-plus+) :then (1- n)
                                :do (setf (the (unsigned-byte +eindex-width+) iindex)
                                          (dpb (ldb (byte +cindex-width+
                                                          (* +cindex-width+ (- +rank-plus+ a 1)))
                                                    i)
                                               (byte +cindex-width+ (* n +cindex-width+))
                                               iindex)))
                          ;; (print (format nil "#x~8,'0X" i))
                          ;; (print (format nil "#x~8,'0X~%" iindex))
                          iindex)))))
         ))
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
         (:eindex-width +eindex-width+ :cindex-width +cindex-width+
          :rank +rank+ :rank-width +rank-width+)
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
                           (the (unsigned-byte +eindex-width+) iindex))))))))))
  
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
              (let ((match (gethash (list iwidth itype irank)
                                    regular-handler-table)))
                ;; (print (list :ma idims match iwidth itype irank indices-vector))
                (list (when match (funcall match indices-vector))
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
              (let ((match (gethash (list iwidth itype (1- irank))
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
