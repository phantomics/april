;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Varray -*-
;;;; x86.lisp

(in-package #:varray)

"Effectors manifesting x86 assembly language clauses implementing array transformations. This assembler system currently rely on SBCL's vop faculties."

(defun mask-signed (x size)
  (logior x (- (mask-field (byte 1 (1- size)) x))))

(defun build-iterator-x86asm (insym osym dimensions encoding ctype)
  ;; (print (list :in insym dimensions encoding ctype))
  (let ((tags (if (= 8 ctype) #(:byte :word :dword :dword :qword)
                  (if (= 16 ctype) #(:word :dword :qword)
                      #(:dword :qword)))))
    ;; (print (list :ta tags ctype))
    (loop :for d :in (reverse dimensions) :for dx :from 0
          :append (let ((tag (aref tags (min (1- (length tags)) dx))))
                    `(,@(if (zerop dx)
                            `((inst inc ,tag ,insym))
                            (if (> 32 (* ctype dx))
                                `((inst add :qword ,insym ,(ash 1 (* ctype dx))))
                                `((inst mov :qword ,osym ,(ash 1 (* ctype dx)))
                                  (inst add :qword ,insym ,osym))))
                      ,@(when (< 1 (length dimensions))
                          (if (< dx (1- (length dimensions)))
                              `((inst cmp ,tag ,insym ,(* d (ash 1 (* ctype dx))))
                                (inst jmp :b ITERATED)
                                ,(if (position dx #(0 1 3 7))
                                     `(inst xor ,tag ,insym ,insym)
                                     `(inst and ,tag ,insym ,(ash 255 (* ctype (1+ dx))))))
                              `(ITERATED))))))))

;; (varray::build-iterator-x86asm 'bla '(3 4 5 6) 32 8)

(defun build-encoder-x86asm (starting-value syms d-factors encoding coordinate-type)
  ;; (print (list :in insym dimensions encoding coordinate-type))
  (let ((enc (case encoding (16 :word) (32 :dword) (64 :qword))))
    (destructuring-bind (ra rc rd rb r6 r7 r8 r9 r10 r11) syms
      (declare (ignorable ra rc rd rb r6 r7 r8 r9 r10 r11))
      (loop :for d :in d-factors :for dx :from 0
            :append (if (< dx (1- (length d-factors)))
                        `(,@(when (zerop dx) `((inst mov ,enc ,ra ,starting-value)
                                               (inst xor ,enc ,rd ,rd)
                                               (inst xor ,enc ,rb ,rb)))
                          (inst mov ,enc ,rc ,d)
                          (inst div ,enc ,rc)
                          (inst add ,enc ,rb ,ra)
                          (inst shl ,enc ,rb ,coordinate-type)
                          ,@(when (< dx (- (length d-factors) 2))
                              `((inst mov ,enc ,ra ,rd)
                                (inst xor ,enc ,rd ,rd))))
                        `((inst add ,enc ,rb ,rd)))))))

;; (build-encoder-x86asm 138 '(a c d b) '(20 5 1) 32 8)

(defun build-decoder-x86asm (syms d-factors encoding coordinate-type)
  ;; (print (list :in insym dimensions encoding coordinate-type))
  (let ((cty (case coordinate-type (8 :byte) (16 :word) (32 :dword)))
        (enc (case encoding (16 :word) (32 :dword) (64 :qword))))
    (destructuring-bind (ra rc rd rb r6 r7 r8 r9 r10 r11) syms
      (loop :for d :in (reverse d-factors) :for dx :from 0
            :append  (if (< 0 dx)
                         `((inst shr ,enc ,r8 ,coordinate-type)
                           (inst xor ,enc ,ra ,ra)
                           (inst mov ,cty ,ra ,r8)
                           (inst xor ,enc ,rd ,rd)
                           (inst mov ,enc ,rc ,d)
                           (inst mul ,enc ,rc)
                           (inst add ,enc ,rb ,ra)
                           ;; (inst add ,enc ,rb ,rd)
                           )
                         ;; must zero all of RBX as the full register
                         ;; is used to calculate the write address
                         `((inst xor :qword ,rb ,rb)
                           (inst add ,cty ,rb ,r8))
                         ;; `((inst mov :qword ,b-sym ,rd)
                         ;;   (inst and :qword ,b-sym ,(expt 2 coordinate-type)))
                         )))))

(defmethod effect :around ((varray varray) (output-array array) &key (format :lisp))
  (case format
    (:x86-asm
     (let* ((oshape (shape-of varray))
            (vaspec (specify varray))
            (word-size 64)
            (collect-sub-quadwords)
            (metadata (getf (metadata-of varray) :shape))
            (encoding (getf (rest (getf metadata :gen-meta)) :index-width))
            (coordinate-type (getf (rest (getf metadata :gen-meta)) :index-type))
            (varray-point varray)
            (effectors)
            (start-points (make-array *workers-count* :element-type 'fixnum))
            (counts (make-array *workers-count* :element-type 'fixnum)))

       (setf (getf metadata :format) format)
       (loop :while (and varray-point (typep varray-point 'varray-derived))
             :do (let ((this-effector (effector-of varray-point metadata)))
                   (if this-effector (progn (push this-effector effectors)
                                            (setf varray-point (vader-base varray-point)))
                       (setf varray-point nil))))
       ;; (print (list :abcd))
       (when (and varray-point encoding coordinate-type
                  (not (member (array-element-type varray-point)
                               '(t base-char character) :test #'eql)))
         ;; (print :eee)
         (let ((enc (case encoding (16 :word) (32 :dword) (64 :qword)))
               (write-transport-width 0 ) ; 256)
               (byte-shift)
               (el-width) (transfer-type)
               (ishape (shape-of varray-point))
               (temp-syms '(ra rc rd rb r6 r7 aec r9 r10 r11)))
           ;; can also loop across sb-vm::*room-info*
           (loop :for i :across sb-vm::*specialized-array-element-type-properties*
                 :while (not el-width)
                 :when (and (sb-vm::specialized-array-element-type-properties-p i)
                            (equalp (array-element-type varray-point)
                                    (sb-vm::saetp-specifier i)))
                   :do (setf el-width (sb-vm::saetp-n-bits i)
                             transfer-type (case el-width (64 :qword) (32 :dword)
                                                 (16 :word) (t :byte))
                             byte-shift (+ -3 (floor (log el-width 2)))))

           (let* ((segment (max 1 (/ write-transport-width el-width)))
                  (total 0) (seg 0) (count (size-of varray))
                  (inc (/ count segment *workers-count*)))
             (loop :for i :below *workers-count* :for ix :from 0
                   :do (let ((to-add 0))
                         (incf seg inc)
                         (if (< 1 seg)
                             (let ((dividend (floor seg)))
                               (decf seg dividend)
                               (setf (aref start-points ix) total)
                               (if (= i (1- *workers-count*))
                                   (setf (aref counts ix) (max 0 (- count total)))
                                   (setf (aref counts ix) (* dividend segment)))
                               (incf total (* dividend segment)))
                             (if (= i (1- *workers-count*))
                                 (setf (aref counts ix) (max 0 (- count total))
                                       (aref start-points ix) total)
                                 (setf (aref start-points ix) 0))))))

           ;; (print (list :segs el-width start-points counts (type-of varray-point)))
           
           (values
            `(progn
               (sb-c:defknown varray::vop-ph
                   ((unsigned-byte ,word-size) (unsigned-byte ,word-size)
                    (unsigned-byte ,word-size) (unsigned-byte ,word-size))
                   (unsigned-byte ,word-size) (sb-c:foldable sb-c:flushable sb-c:movable)
                 :overwrite-fndb-silently t)
               (unless (fboundp 'vop-ph)
                 (proclaim '(special vop-ph))
                 (setf (symbol-function 'vop-ph) (lambda (a b c d) a)))
               (define-vop (varray::vop-ph)
                 (:policy :fast-safe)
                 (:translate varray::vop-ph)
                 (:args (st-arg :scs (unsigned-reg)) (ct-arg :scs (unsigned-reg))
                        (ia-arg :scs (unsigned-reg)) (oa-arg :scs (unsigned-reg)))
                 (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
                 ;; (:results (rout :scs (unsigned-reg)))
                 ;; (:result-types) ; unsigned-num)
                 ;; A and D registers are hardwired due to their relations to MUL and DIV
                 (:temporary (:sc sb-vm::unsigned-reg :from :eval :offset sb-vm::rax-offset) ra)
                 (:temporary (:sc sb-vm::unsigned-reg :from :eval) rc)
                 (:temporary (:sc sb-vm::unsigned-reg :from :eval :offset sb-vm::rdx-offset) rd)
                 (:temporary (:sc sb-vm::unsigned-reg :from :eval) rb)
                 (:temporary (:sc sb-vm::unsigned-reg :from :eval) r5)
                 (:temporary (:sc sb-vm::unsigned-reg :from :eval) ia)
                 (:temporary (:sc sb-vm::unsigned-reg :from :eval) aec) ;; encoded coords
                 (:temporary (:sc sb-vm::unsigned-reg :from :eval) pt)  ;; output point
                 (:temporary (:sc sb-vm::unsigned-reg :from :eval) ct)  ;; count
                 (:temporary (:sc sb-vm::unsigned-reg :from :eval) sgs)
                 (:temporary (:sc sb-vm::unsigned-reg :from :eval) sgi)
                 (:temporary (:sc sb-vm::single-avx2-reg :from :eval) sec) ;; stored encoded coords
                 ,@(unless (zerop write-transport-width)
                     `((:temporary (:sc sb-vm::double-avx2-reg :from :eval) to-copy)
                       (:temporary (:sc sb-vm::double-avx2-reg :from :eval) to-write)))
                 (:generator
                  1 ;; first, load input variables into named temporary registers
                  (inst mov :qword pt st-arg) ;; starting point
                  (inst mov :qword ct ct-arg)
                  (inst mov :qword ia ia-arg) ;; input address
                  ,@(unless (zerop write-transport-width)
                      '((inst vpxor to-copy to-copy to-copy))) ;; zero the copy vector register
                  ;; load clear mask into vector register
                  (inst mov :qword sgi oa-arg)
                  ;; segment index register temporarily holds output address
                  ;; output address
                  
                  ;; encode initial coordinates based on starting point
                  ,@(build-encoder-x86asm 'pt temp-syms (get-dimensional-factors oshape)
                                          encoding coordinate-type)
                  ;; after encoding the initial coordinate set, multiply the point and count-to
                  ;; values if necessary (i.e. the elements are wider than 8 bits) and add the
                  ;; starting point to the end point to get the correct end point
                  ,@(unless (zerop byte-shift) `((inst shl :qword pt ,byte-shift)
                                                 (inst shl :qword ct ,byte-shift)))
                  (inst add :qword ct pt) ;; ending point
                  (inst movq sec rb)      ;; stored encoded coordinates
                  (inst mov ,enc aec rb)  ;; active encoded coordinates
                  ;; starting output address added to point and count
                  (inst add :qword ct sgi)  ;; input address
                  (inst add :qword pt sgi)  ;; output address
                  (inst xor :qword sgi sgi) ;; segment index
                  
                  (inst jmp START-LOOP)
                  START-LOOP-INCREMENTING
                  ;; iterate coordinates
                  ,@(build-iterator-x86asm 'aec 'rc oshape encoding coordinate-type)
                  (inst movq sec aec)
                  
                  START-LOOP
                  ;; do operations here on encoded coordinates
                  ,@(loop :for ef :in (reverse effectors) :append (funcall ef temp-syms))
                  ;; decode the modified coordinates
                  ,@(build-decoder-x86asm temp-syms (get-dimensional-factors ishape)
                                          encoding coordinate-type)

                  ;; adjust B register to point to input data location
                  ,@(unless (zerop byte-shift) `((inst shl :qword rb ,byte-shift)))
                  (inst add :qword rb ia)
                  ;; (inst jmp INDIVIDUAL-TRANSFER)
                  ,@(unless (or (= 64 el-width) (not collect-sub-quadwords))
                      `((inst cmp :qword sgi 0)
                        ;; if the segment index isn't zero, serial transfer must be ongoing
                        (inst jmp :ne SERIAL-TRANSFER)
                        (inst mov :qword rc ct)
                        (inst sub :qword rc pt)
                        (inst cmp :qword rc ,(ash word-size -3))
                        ;; transfer one by one if the remaining elements won't fit in a larger register
                        (inst jmp :b INDIVIDUAL-TRANSFER)
                        SERIAL-TRANSFER ;; this clause moves elements in sets fitting in 64-bit registers
                        ;; move each element into segment storage
                        ,@(if (= 32 el-width)
                              ;; if array elements are 32 bits, the MOV instruction from memory will zero
                              ;; the register's upper doubleword so it must be placed in a temporary register
                              ;; and then united with the segment register, then the segment register
                              ;; must have its doublewords flipped before it's written to memory
                              ;; or to the vector register that will store data before writing
                              `((inst mov :dword rc (ea rb))
                                (inst shl :qword sgs 32)
                                (inst or :qword sgs rc))
                              `((inst mov ,transfer-type sgs (ea rb))
                                (inst ror :qword sgs ,el-width)))
                        (inst inc :qword sgi)
                        (inst cmp :byte sgi ,(/ word-size el-width))
                        ;; the transfer cycle is complete if the segment isn't full yet
                        (inst jmp :b TRANSFER-CYCLE-COMPLETE)))
                  ,@(unless (zerop write-transport-width)
                      ;; implement pre-write storage of data in vector registers
                      ;; (inst jmp TRANSFER-ONE-REGISTER)
                      `((inst cmp :word sgi #x00FF)
                        (inst jmp :a VECTOR-TRANSFER-ONGOING)
                        ;; a vector transfer is ongoing if the upper byte of the segment index is > 0
                        (inst mov :qword rc ct)
                        (inst sub :qword rc pt)
                        ;; if a vector transfer is not confirmed to be ongoing, check whether the
                        ;; limit - point delta is greater than the number of elements that will fit in
                        ;; a vector register; if it is, then a vector register may be used.
                        (inst cmp :qword rc ,(ash write-transport-width -3))
                        (inst jmp :b TRANSFER-ONE-REGISTER)
                        VECTOR-TRANSFER-ONGOING
                        ;; doublewords must be flipped before writing because of the way their
                        ;; assignment from memory works
                        ,@(when (= 32 el-width) '((inst ror :qword sgs 32)))
                        ;; copy the full register into the lower portion of the copy vector register,
                        ;; clear space in the write vector register for the new quadword, and use a logical
                        ;; or to insert the new quadword in the write vector register, accomplishing the
                        ;; same type of rotation done with quadword registers above at a larger scale
                        (inst movq to-copy sgs)
                        (inst vpblendd to-write to-write to-copy #b00000011)
                        (inst vpermq to-write to-write #b00111001)
                        (inst add :qword sgi #x0100)
                        (inst and :qword sgi #xFF00) ;; zero lower byte
                        ;; increment the upper byte of the segment index and zero the lower byte;
                        ;; if the vector register is not full yet, the transfer cycle is done,
                        ;; if it's full then write to memory
                        (inst cmp :qword sgi ,(* 256 (/ write-transport-width word-size)))
                        (inst jmp :b TRANSFER-CYCLE-COMPLETE)
                        (inst vmovdqu (ea pt) to-write)
                        (inst xor :qword sgi sgi)
                        (inst add :qword pt ,(ash write-transport-width -3))
                        (inst jmp TRANSFER-CYCLE-COMPLETE)
                        TRANSFER-ONE-REGISTER))
                  ,@(unless (or (= 64 el-width) (not collect-sub-quadwords))
                      `(,@(when (= 32 el-width) '((inst ror :qword sgs 32)))
                        ;; as above, flip doublewords before writing
                        (inst mov :qword (ea pt) sgs)
                        (inst and :qword sgi 65280) ;; zero lower byte
                        (inst add :qword pt ,(ash word-size -3))
                        (inst jmp TRANSFER-CYCLE-COMPLETE)
                        INDIVIDUAL-TRANSFER ;; this clause moves elements individually
                        ))
                  (inst mov ,transfer-type ra (ea rb))
                  (inst mov ,transfer-type (ea pt) ra)
                  ,@(let ((increment (ash el-width -3)))
                      (if (= 1 increment)
                          `((inst inc :qword pt))
                          `((inst add :qword pt ,(ash el-width -3)))))
                  TRANSFER-CYCLE-COMPLETE ;; data transfer cycle is finished
                  (inst movq aec sec)
                  (inst cmp :qword pt ct)               ;; reached ending point yet?
                  (inst jmp :b START-LOOP-INCREMENTING) ;; if not, loop again
                  ))
               (setf (symbol-function 'varray::vop-ph)
                     (compile nil '(lambda (a b c d)
                                    (declare (optimize (speed 3) (safety 0)))
                                    (varray::vop-ph a b c d)))))
            ;; (print :ggg)
            ;; (error "A")
            varray-point :x86-asm start-points counts)))))
    (t (call-next-method))))

(defmethod effector-of :around ((varray vader-section) &optional params)
  (let* ((format (getf params :format))
         (metadata (getf (metadata-of varray) :shape))
         (encoding (getf (rest (getf metadata :gen-meta)) :index-width))
         (coordinate-type (getf (rest (getf metadata :gen-meta)) :index-type))
         (etag (case encoding (8 :byte) (16 :word) (32 :dword) (64 :qword))))
    (case format
      (:x86-asm
       ;; current disabled when padding is present
       (when (loop :for p :across (vasec-pad varray) :always (zerop p))
         (lambda (symbols)
           (let ((sum 0))
             (loop :for dx :below (rank-of varray)
                   :do (incf sum (ash (+ (aref (vasec-span varray) dx)
                                         (aref (vasec-pad varray) dx))
                                      (* coordinate-type
                                         (- (rank-of varray) (1+ dx))))))
             (destructuring-bind (_ rc _ _ _ _ eac &rest _) symbols
               (declare (ignore _))
               (unless (zerop sum)
                 (if (< sum (expt 2 32))
                     `((inst add ,etag ,eac ,sum))
                     `((inst mov :qword ,rc ,sum)
                       (inst add :qword ,eac ,rc)))))))))
      (t (call-next-method)))))

(defmethod effector-of :around ((varray vader-turn) &optional params)
  (let* ((format (getf params :format))
         (ewidth (getf (rest (getf params :gen-meta)) :index-width))
         (cwidth (getf (rest (getf params :gen-meta)) :index-type))
         (etag (case ewidth (8 :byte) (16 :word) (32 :dword) (64 :qword)))
         (axis (max 0 (if (eq :last (vads-axis varray))
                          (1- (rank-of varray))
                          (- (vads-axis varray)
                             (vads-io varray)))))
         (arg (setf (vads-argument varray) (arg-process varray)))
         (dimension (nth axis (shape-of varray))))
    ;; (print (list :ct coordinate-type))
    (unless (vaturn-degrees varray)
      (setf (vaturn-degrees varray)
            (if (integerp arg) (when dimension (mod arg dimension))
                (when (arrayp arg)
                  (let ((out (make-array (array-dimensions arg)
                                         :element-type (array-element-type arg))))
                    (dotimes (i (array-total-size arg))
                      (setf (row-major-aref out i)
                            (mod (row-major-aref arg i) dimension)))
                    out)))))
    (case format
      (:x86-asm
       (lambda (symbols)
         (let* ((cindex (- (rank-of varray) (1+ axis)))
                (adj-degrees (ash (vaturn-degrees varray) (* cwidth cindex)))
                (adj-dim (ash dimension (* cwidth cindex)))
                ;; (ltag (when ))
                (mask (ash (1- (expt 2 cwidth)) (* cwidth cindex)))
                (ROTATED (gensym)))
           (destructuring-bind (ra rc rd rb r6 r7 r8 r9 r10 r11) symbols
             (declare (ignorable ra rc rd rb r6 r7 r8 r9 r10 r11))
             `((inst add ,etag ,r8 ,adj-degrees)
               ,@(if (zerop axis)
                     `((inst cmp ,etag ,r8 ,adj-dim))
                     `((inst mov ,etag ,ra ,r8)
                       (inst and ,etag ,ra ,mask)
                       (inst cmp ,etag ,ra ,adj-dim)))
               (inst jmp :b ,ROTATED)
               (inst sub ,etag ,r8 ,adj-dim)
               ,ROTATED)))))
      (t (call-next-method)))))

(defmethod plex-of :around ((varray vapri-apro-vector) &optional params)
  (declare (ignore params))
  (with-accessors ((origin vapip-origin)
                   (offset vapip-offset)
                   (number vapip-number)
                   (factor vapip-factor)
                   (repeat vapip-repeat)) varray
    (let* ((start (* (+ origin offset) factor))
           (range (petalisp:range start (+ start (* factor number)) factor)))
      (petalisp:lazy-reshape
       (petalisp:lazy-index-components (~* range))
       (~* range ~ repeat)
       (petalisp:flattening-reshaper)))))

(defmethod plex-of :around ((varray vapri-onehot-vector) &optional params)
  (declare (ignore params))
  (destructuring-bind (length) (varray-shape varray)
    (let ((index (vaohv-index varray)))
      (petalisp:lazy-overwrite
       (petalisp:lazy-reshape 0 (~ length))
       (petalisp:lazy-reshape 1 (~ index (1+ index)))))))

(defmethod plex-of :around ((varray vader-turn) &optional params)
  (declare (ignore params))
  (let* ((array (petalisp:lazy-array (render varray)))
         (amount (setf (vads-argument varray) (arg-process varray)))
         (axis (max 0 (if (eq :last (vads-axis varray))
                          (1- (rank-of varray))
                          (- (vads-axis varray)
                             (vads-io varray)))))
         (shape (petalisp:lazy-array-shape array))
         (rank (petalisp:shape-rank shape))
         (range (petalisp:shape-range shape axis))
         (start (petalisp:range-start range))
         (size (petalisp:range-size range))
         (step (petalisp:range-step range))
         (shift (if (minusp amount)
                    (* (+ size amount) step)
                    (* amount step)))
         (position (+ start shift))
         (offsets (let ((v (make-array rank :initial-element 0)))
                    (setf (aref v axis) (- shift)) v))
         (transformation (petalisp:make-transformation :offsets offsets)))
    (multiple-value-bind (lo hi)
        (petalisp:split-shape shape axis position)
      (petalisp:lazy-stack
       axis
       (petalisp:lazy-reshape array hi transformation)
       (petalisp:lazy-reshape array lo)))))

;; (defmethod effector-of :around ((varray vader-turn) &optional params)
;;   (let* ((format (getf params :format))
;;          (ewidth (getf (rest (getf params :gen-meta)) :index-width))
;;          (cwidth (getf (rest (getf params :gen-meta)) :index-type))
;;          (ctag (case cwidth (8 :byte) (16 :word) (32 :dword) (64 :qword)))
;;          (etag (case ewidth (8 :byte) (16 :word) (32 :dword) (64 :qword)))
;;          (axis (max 0 (if (eq :last (vads-axis varray))
;;                           (1- (rank-of varray))
;;                           (- (vads-axis varray)
;;                              (vads-io varray)))))
;;          (arg (setf (vads-argument varray) (arg-process varray)))
;;          (dimension (nth axis (shape-of varray))))
;;     ;; (print (list :ct coordinate-type))
;;     (unless (vaturn-degrees varray)
;;       (setf (vaturn-degrees varray)
;;             (if (integerp arg) (when dimension (mod arg dimension))
;;                 (when (arrayp arg)
;;                   (let ((out (make-array (array-dimensions arg)
;;                                          :element-type (array-element-type arg))))
;;                     (dotimes (i (array-total-size arg))
;;                       (setf (row-major-aref out i)
;;                             (mod (row-major-aref arg i) dimension)))
;;                     out)))))
;;     (case format
;;       (:x86-asm
;;        (lambda (symbols)
;;          (let* ((cindex (- (rank-of varray) (1+ axis)))
;;                 (adj-degrees (ash (vaturn-degrees varray)
;;                                   (* cwidth (- (rank-of varray) (1+ axis)))))
;;                 (adj-dim (ash dimension (* cwidth (- (rank-of varray) (1+ axis)))))
;;                 (mask (when (and (not (zerop axis))
;;                                  (/= axis (1- (rank-of varray))))
;;                         (ash (1- (expt 2 cwidth))
;;                              (* cwidth (- (rank-of varray) (1+ axis)))))))
;;            (print (list :mm mask adj-degrees adj-dim cwidth (- (rank-of varray) (1+ axis))
;;                         (* cwidth (- (rank-of varray) (1+ axis)))))
;;            (destructuring-bind (ra rc rd rb r6 r7 r8 r9 r10 r11) symbols
;;              (declare (ignorable ra rc rd rb r6 r7 r8 r9 r10 r11))
;;              `((inst add ,etag ,r8 ,adj-degrees)
;;                ,@(if mask
;;                      `((inst mov ,etag ,ra ,r8)
;;                        (inst and ,etag ,ra ,mask)
;;                        (inst cmp ,etag ,ra ,adj-dim))
;;                      `((inst cmp ,etag ,r8 ,adj-dim)))
;;                (inst jmp :b ROTATED)
;;                (inst sub ,etag ,r8 ,adj-dim)
;;                ROTATED)))))
;;       (t (call-next-method)))))

(defmethod effector-of :around ((varray vader-permute) &optional params)
  (let ((format (getf params :format)))
    (case format
      (:x86-asm
       (let* ((metadata (getf (metadata-of varray) :shape))
              (encoding (getf (rest (getf metadata :gen-meta)) :index-width))
              (cwidth (getf (rest (getf metadata :gen-meta)) :index-type))
              (etag (case encoding (8 :byte) (16 :word) (32 :dword) (64 :qword))))
         (if (and cwidth (= 8 cwidth) (not (vads-argument varray)))
             (lambda (symbols)
               (destructuring-bind (ra rc rd rb r6 r7 r8 r9 r10 r11) symbols
                 (declare (ignorable ra rc rd rb r6 r7 r8 r9 r10 r11))
                 `((inst bswap ,etag ,r8)
                   ,@(when (/= encoding (* cwidth (rank-of varray)))
                       `((inst shr ,etag ,r8 ,(- encoding (* cwidth (rank-of varray)))))))))
             (when (= 2 (rank-of varray))
               (lambda (symbols)
                 (destructuring-bind (ra rc rd rb r6 r7 r8 r9 r10 r11) symbols
                   (declare (ignorable ra rc rd rb r6 r7 r8 r9 r10 r11))
                   `((inst ror ,etag ,r8 ,(/ encoding 2)))))))))
      (t (call-next-method)))))

(defmethod effector-of :around ((varray vader-expand) &optional params)
  (let ((format (getf params :format)))
    (case format
      (:x86-asm
       (let* ((axis (vads-axis varray))
              (metadata (getf (metadata-of varray) :shape))
              (ewidth (getf (rest (getf metadata :gen-meta)) :index-width))
              (cwidth (getf (rest (getf params :gen-meta)) :index-type)))
         (unless (arrayp (vads-argument varray))
           (lambda (symbols)
             (let ((mask (mask-signed (ash (1- (expt 2 cwidth))
                                           (* cwidth (- (rank-of varray) (1+ axis))))
                                      ewidth)))
               (multiple-value-bind (bits fraction) (floor (log (vads-argument varray) 2))
                 (destructuring-bind (ra rc rd rb r6 r7 r8 r9 r10 r11) symbols
                   (declare (ignorable ra rc rd rb r6 r7 r8 r9 r10 r11)) ; r12 r13 r14 r15))
                   (if (zerop fraction)
                       ;; perform a bit-shift if dividing by a power of 2
                       `((inst mov :qword ,ra ,r8)
                         (inst and :qword ,r8 ,mask)
                         (inst shr :qword ,r8 ,bits)
                         ,@(when (/= axis (1- (rank-of varray)))
                             `((inst and :qword ,r8 ,mask)))
                         ;; must mask again in case a 1 got shifted to the next byte, unless
                         ;; expanding on the last axis so the bit(s) shift into nothingness
                         (inst and :qword ,ra ,(- (1+ mask)))
                         (inst add :qword ,r8 ,ra))
                       `((inst mov :qword ,ra ,r8)
                         (inst mov :qword ,rb ,r8)
                         (inst and :qword ,ra ,mask)
                         (inst mov :qword ,rc ,(vads-argument varray))
                         (inst xor :qword ,r8 ,r8)
                         (inst xor :qword ,rd ,rd)
                         (inst div :qword ,rc)
                         (inst mov :qword ,r8 ,rb)
                         ,@(when (/= axis (1- (rank-of varray)))
                             `((inst and :qword ,ra ,mask)))
                         (inst and :qword ,r8 ,(- (1+ mask)))
                         (inst add :qword ,r8 ,ra))))))))))
      (t (call-next-method)))))
