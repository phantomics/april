;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Varray -*-
;;;; effectors-x86-asm.lisp

(in-package #:varray)

"Effectors manifesting x86 assembly language clauses implementing array transformations. This assembler system currently rely on SBCL's vop faculties."

(defun build-iterator-x86asm (insym dimensions encoding coordinate-type)
  ;; (print (list :in insym dimensions encoding coordinate-type))
  (let ((tags (if (= 8 coordinate-type)
                  #(:byte :word :dword :dword :qword)
                  (if (= 16 coordinate-type)
                      #(:word :dword :qword) #(:dword :qword)))))
    (loop :for d :in (reverse dimensions) :for dx :from 0
          :append (let ((tag (aref tags (min (1- (length tags))
                                             dx))))
                    `(,(if (zerop dx)
                           `(inst inc ,tag ,insym)
                           `(inst add ,tag ,insym ,(ash 1 (* 8 dx))))
                      ,@(when (< 1 (length dimensions))
                          (if (< dx (1- (length dimensions)))
                              `((inst cmp ,tag ,insym ,(* d (ash 1 (* 8 dx))))
                                (inst jmp :b ITERATED)
                                ,(if (position dx #(0 1 3 7))
                                     `(inst xor ,tag ,insym ,insym)
                                     `(inst and ,tag ,insym ,(ash 255 (* 8 (1+ dx))))))
                              `(ITERATED))))))))

;; (varray::build-iterator-x86asm 'bla '(3 4 5 6) 32 8)

(defun build-encoder-x86asm (starting-value syms d-factors encoding coordinate-type)
  ;; (print (list :in insym dimensions encoding coordinate-type))
  (let (;; (cty (case coordinate-type (8 :byte) (16 :word) (32 :dword)))
        ;; (hlf (case encoding (16 :byte) (32 :word)  (64 :dword)))
        (enc (case encoding (16 :word) (32 :dword) (64 :qword))))
    (destructuring-bind (ra rc rd rb r6 r7 r8 r9 r10 r11 r12 r13 r14 r15) syms
      (declare (ignorable ra rc rd rb r6 r7 r8 r9 r10 r11 r12 r13 r14 r15))
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
    (destructuring-bind (ra rc rd rb r6 r7 r8 r9 r10 r11 r12 r13 r14 r15) syms
      (loop :for d :in (reverse d-factors) :for dx :from 0
            :append  (if (< 0 dx)
                         `((inst shr ,enc ,r8 ,coordinate-type)
                           (inst xor ,enc ,ra ,ra)
                           (inst mov ,cty ,ra ,r8)
                           (inst xor ,enc ,rd ,rd)
                           (inst mov ,enc ,rc ,d)
                           (inst mul ,enc ,rc)
                           (inst add ,enc ,rb ,ra))
                         ;; must zero all of RBX as the full register
                         ;; is used to calculate the write address
                         `((inst xor :qword ,rb ,rb)
                           (inst add ,cty ,rb ,r8))
                         ;; `((inst mov :qword ,b-sym ,rd)
                         ;;   (inst and :qword ,b-sym ,(expt 2 coordinate-type)))
                         )))))

;; (build-decoder-x86asm '(a c d b) '(20 5 1) 32 8)

(let ((xmm-rotate-mask-qword (make-array 4 :element-type '(unsigned-byte 32)
                                           :initial-contents '(2 3 0 1)))
      (ymm-rotate-mask-qword (make-array 8 :element-type '(unsigned-byte 32)
                                           :initial-contents '(2 3 4 5 6 7 0 1))))
  (defmethod effect :around ((varray varray) (output-array array) &key (format :lisp))
    (case format
      (:x86-asm
       (let* ((oshape (shape-of varray))
              (vaspec (specify varray))
              (metadata (getf (metadata-of varray) :shape))
              (encoding (getf (rest (getf metadata :gen-meta)) :index-width))
              (coordinate-type (getf (rest (getf metadata :gen-meta)) :index-type))
              (varray-point varray)
              (ishape) (effectors))

         (setf (getf metadata :format) format)
         (loop :while (and varray-point (typep varray-point 'varray-derived))
               :do (let ((this-effector (effector-of varray-point metadata)))
                     ;; (print (list :ti this-effector))
                     (if this-effector (progn (push this-effector effectors)
                                              (setf varray-point (vader-base varray-point)))
                         (setf varray-point nil))))
         
         (when (and varray-point encoding coordinate-type)
           ;; (print (list :enc encoding coordinate-type))
           (when varray-point (setf ishape (shape-of varray-point)))
           (let ((enc (case encoding (16 :word) (32 :dword) (64 :qword)))
                 ;; (ctag (case coordinate-type (8 :byte) (16 :word) (32 :dword) (64 :qword)))
                 (el-width) (transfer-type)
                 (temp-syms '(ra rc rd rb r6 r7 r8 r9 r10 r11 r12 r13 r14 r15)))
             
             ;; can also loop across sb-vm::*room-info*
             (loop :for i :across sb-vm::*specialized-array-element-type-properties*
                   :while (not el-width)
                   :when (and (sb-vm::specialized-array-element-type-properties-p i)
                              (equalp (array-element-type varray-point)
                                      (sb-vm::saetp-specifier i)))
                     :do (setf el-width (sb-vm::saetp-n-bits i)
                               transfer-type (case el-width (64 :qword) (32 :dword)
                                                   (16 :word) (t :byte))))
             ;; (print (list :sh ishape oshape))
             
             (values
              `(progn
                 (sb-c:defknown varray::vop-ph
                     ((unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64))
                     (unsigned-byte 64) (sb-c:foldable sb-c:flushable sb-c:movable)
                   :overwrite-fndb-silently t)
                 (unless (fboundp 'vop-ph)
                   (proclaim '(special vop-ph))
                   (setf (symbol-function 'vop-ph) (lambda (a b c d) a)))
                 (define-vop (varray::vop-ph)
                   (:policy :fast-safe)
                   (:translate varray::vop-ph)
                   (:args (st :scs (unsigned-reg)) (ct :scs (unsigned-reg))
                          (ia :scs (unsigned-reg)) (oa :scs (unsigned-reg)))
                   (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
                   ;; (:results (rout :scs (unsigned-reg)))
                   (:result-types) ; unsigned-num)
                   ,@(loop :for temp :in temp-syms
                           :collect `(:temporary (:sc sb-vm::unsigned-reg
                                                  :offset ,(case temp
                                                             (ra  'sb-vm::rax-offset)
                                                             (rc  'sb-vm::rcx-offset)
                                                             (rd  'sb-vm::rdx-offset)
                                                             (rb  'sb-vm::rbx-offset)
                                                             (r4  'sb-vm::rsp-offset)
                                                             (r5  'sb-vm::rbp-offset)
                                                             (r6  'sb-vm::rsi-offset)
                                                             (r7  'sb-vm::rdi-offset)
                                                             (r8  'sb-vm::r8-offset )
                                                             (r9  'sb-vm::r9-offset )
                                                             (r10 'sb-vm::r10-offset)
                                                             (r11 'sb-vm::r11-offset)
                                                             (r12 'sb-vm::r12-offset)
                                                             (r13 'sb-vm::r13-offset)
                                                             (r14 'sb-vm::r14-offset)
                                                             (r15 'sb-vm::r15-offset))
                                                  :from :eval)
                                                 ,temp))
                   ;; (:temporary (:sc sb-vm::single-avx2-reg :offset 2 :from :eval) vcr)
                   (:generator
                    1
                    (inst mov :qword r10 st) ;; starting point
                    (inst mov :qword r11 ct)
                    (inst add :qword r11 st) ;; ending point
                    (inst mov :qword r6  ia) ;; input address
                    (inst mov :qword r7  oa) ;; output address
                    (inst push r12)
                    (inst mov :qword r12 0)  ;; segment index
                    ;; ,@(when (< 8 el-width)
                    ;;     `((inst shl :qword r9 ,(ash el-width -3))))
                    ,@(build-encoder-x86asm 'r10 temp-syms (get-dimensional-factors oshape)
                                            encoding coordinate-type)
                    (inst mov ,enc r9 rb) ;; stored encoded coordinates
                    (inst mov ,enc r8 rb) ;; active encoded coordinates
                    (inst jmp START-LOOP)
                    START-LOOP-INCREMENTING
                    ,@(build-iterator-x86asm 'r8 oshape encoding coordinate-type)
                    (inst mov ,enc r9 r8)
                    START-LOOP
                    ;; do operations here on r8
                    ,@(loop :for ef :in (reverse effectors) :append (funcall ef temp-syms))
                    ,@(build-decoder-x86asm temp-syms (get-dimensional-factors ishape)
                                            encoding coordinate-type)

                    ;; move data with b-sym
                    ,@(if nil ; (= 8 el-width)
                          `((inst mov ,transfer-type r14 (ea ia rb ,(ash el-width -3)))
                            (inst shl :qword r14 ,el-width)
                            (inst inc :qword r12)
                            (inst cmp :byte r12 8)
                            (inst jmp :b NOT-YET-SENT)
                            (inst mov :qword (ea r7 r10 ,(ash el-width -3)) r14)
                            (inst xor :byte r12 r12)
                            (inst add :qword r10 8)
                            NOT-YET-SENT)
                          `((inst mov ,transfer-type ra (ea r6 rb ,(ash el-width -3)))
                            (inst mov ,transfer-type (ea r7 r10 ,(ash el-width -3)) ra)
                            (inst inc :qword r10)))
                    
                    (inst mov ,enc r8 r9)
                    (inst cmp :qword r10 r11)             ;; reached ending point yet?
                    (inst jmp :b START-LOOP-INCREMENTING) ;; if not, loop again
                    (inst pop r12)
                    ;; (inst mov :qword rout r9)
                    )) ;; return final coordinates
                 (setf (symbol-function 'varray::vop-ph)
                       (lambda (a b c d)
                         (declare (optimize (speed 3) (safety 0)))
                         (varray::vop-ph a b c d))))
              varray-point :x86-asm)))))
      (t (call-next-method)))))

(defmethod effector-of :around ((varray vader-section) &optional params)
  (declare (ignore params))
  (let* ((format (getf params :format))
         (metadata (getf (metadata-of varray) :shape))
         (encoding (getf (rest (getf metadata :gen-meta)) :index-width))
         (coordinate-type (getf (rest (getf metadata :gen-meta)) :index-type))
         (etag (case encoding (8 :byte) (16 :word) (32 :dword) (64 :qword))))
    (case format
      (:x86-asm
       (lambda (symbols)
         (let ((sum 0))
           (loop :for dx :below (rank-of varray)
                 :do (incf sum (ash (+ (aref (vasec-span varray) dx)
                                       (aref (vasec-pad varray) dx))
                                    (* coordinate-type
                                       (- (rank-of varray) (1+ dx))))))
           (destructuring-bind (_ _ _ _ _ _ r8 &rest _) symbols
             (declare (ignore _))
             `((inst add ,etag ,r8 ,sum))))))
      (t (call-next-method)))))

(defmethod effector-of :around ((varray vader-turn) &optional params)
  (declare (ignore params))
  (let* ((format (getf params :format))
         (coordinate-type (getf (rest (getf params :gen-meta)) :index-type))
         (ctag (case coordinate-type (8 :byte) (16 :word) (32 :dword) (64 :qword)))
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
         (let ((cindex (- (rank-of varray) (1+ axis))))
           (destructuring-bind (ra rc rd rb r6 r7 r8 r9 r10 r11 r12 r13 r14 r15) symbols
             (declare (ignorable ra rc rd rb r6 r7 r8 r9 r10 r11 r12 r13 r14 r15))
             `((inst add ,ctag ,r8 ,(ash (vaturn-degrees varray) (* cindex coordinate-type)))
               (inst cmp ,ctag ,r8 ,(ash dimension (* cindex coordinate-type)))
               (inst jmp :b ROTATED)
               (inst sub ,ctag ,r8 ,(ash dimension (* cindex coordinate-type)))
               ROTATED)))))
      (t (call-next-method)))))

(defmethod effector-of :around ((varray vader-permute) &optional params)
  (declare (ignore params))
  (let ((format (getf params :format)))
    (case format
      (:x86-asm
       (let* ((metadata (getf (metadata-of varray) :shape))
              (encoding (getf (rest (getf metadata :gen-meta)) :index-width))
              (cwidth (getf (rest (getf metadata :gen-meta)) :index-type))
              (etag (case encoding (8 :byte) (16 :word) (32 :dword) (64 :qword))))
         (when (and cwidth (= 8 cwidth) (not (vads-argument varray)))
           (lambda (symbols)
             (destructuring-bind (ra rc rd rb r6 r7 r8 r9 r10 r11 r12 r13 r14 r15) symbols
             (declare (ignorable ra rc rd rb r6 r7 r8 r9 r10 r11 r12 r13 r14 r15))
               `((inst bswap ,etag ,r8)
                 ,@(when (/= encoding (* cwidth (rank-of varray)))
                     `((inst shr ,etag ,r8 ,(- encoding (* cwidth (rank-of varray))))))))))))
      (t (call-next-method)))))

(defmethod effector-of :around ((varray vader-expand) &optional params)
  (let ((format (getf params :format)))
    (case format
      (:x86-asm
       (let* ((axis (vads-axis varray))
              (metadata (getf (metadata-of varray) :shape))
              (ewidth (getf (rest (getf metadata :gen-meta)) :index-width))
              (cwidth (getf (rest (getf params :gen-meta)) :index-type))
              (etag (case ewidth (8 :byte) (16 :word) (32 :dword) (64 :qword))))
         (unless (arrayp (vads-argument varray))
           (lambda (symbols)
             (let* ((mask (ash (1- (expt 2 cwidth))
                               (* cwidth (- (rank-of varray) (1+ axis))))))
               (multiple-value-bind (bits fraction) (floor (log (vads-argument varray) 2))
                 (destructuring-bind (ra rc rd rb r6 r7 r8 r9 r10 r11 r12 r13 r14 r15) symbols
                   (declare (ignorable ra rc rd rb r6 r7 r8 r9 r10 r11 r12 r13 r14 r15))
                   (if (zerop fraction)
                       ;; perform a bit-shift if dividing by a power of 2
                       `((inst mov :qword ,ra ,r8)
                         (inst and :qword ,r8 ,mask)
                         (inst shr :qword ,r8 ,bits)
                         ,@(when (/= axis (1- (rank-of varray)))
                             `((inst and :qword ,r8 ,mask)))
                         ;; must mask again in case a 1 got shifted to the next byte, unless
                         ;; expanding on the last axis so the bit would shift into nothingness
                         (inst and :qword ,ra ,(- mask))
                         (inst add :qword ,r8 ,ra))
                       `((inst mov :qword ,ra ,r8)
                         (inst mov :qword ,rb ,r8)
                         (inst and :qword ,ra ,mask)
                         (inst mov :qword ,rc ,(vads-argument varray))
                         (inst xor :qword ,r8 ,r8)
                         (inst div :qword ,rc)
                         (inst mov :qword ,r8 ,rb)
                         (inst and :qword ,r8 ,(- mask))
                         (inst add :qword ,r8 ,ra))))))))))
      (t (call-next-method)))))
