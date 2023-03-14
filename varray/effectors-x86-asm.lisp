;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8; Package:Varray -*-
;;;; effectors-x86-asm.lisp

(in-package #:varray)

"Effectors manifesting x86 assembly language clauses implementing array transformations. The ASM implementations currently rely on SBCL."

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
  (let ((cty (case coordinate-type (8 :byte) (16 :word) (32 :dword)))
        (hlf (case encoding (16 :byte) (32 :word)  (64 :dword)))
        (enc (case encoding (16 :word) (32 :dword) (64 :qword))))
    (destructuring-bind (a-sym c-sym d-sym b-sym) syms
      (loop :for d :in d-factors :for dx :from 0
            :append (if (< dx (1- (length d-factors)))
                        `(,@(when (zerop dx) `((inst mov ,enc ,a-sym ,starting-value)
                                               (inst xor ,enc ,d-sym ,d-sym)
                                               (inst xor ,enc ,b-sym ,b-sym)))
                          (inst mov ,enc ,c-sym ,d)
                          (inst div ,enc ,c-sym)
                          (inst add ,enc ,b-sym ,a-sym)
                          (inst shl ,enc ,b-sym ,coordinate-type)
                          ,@(when (< dx (- (length d-factors) 2))
                              `((inst mov ,enc ,a-sym ,d-sym)
                                (inst xor ,enc ,d-sym ,d-sym))))
                        `((inst add ,enc ,b-sym ,d-sym)))))))

;; (build-encoder-x86asm 138 '(a c d b) '(20 5 1) 32 8)

(defun build-decoder-x86asm (syms d-factors encoding coordinate-type)
  ;; (print (list :in insym dimensions encoding coordinate-type))
  (let ((cty (case coordinate-type (8 :byte) (16 :word) (32 :dword)))
        (enc (case encoding (16 :word) (32 :dword) (64 :qword))))
    (destructuring-bind (a-sym c-sym d-sym b-sym) syms
      (loop :for d :in (reverse d-factors) :for dx :from 0
            :append  (if (< 0 dx)
                         `((inst shr ,enc ,d-sym ,coordinate-type)
                           (inst xor ,enc ,a-sym ,a-sym)
                           (inst mov ,cty ,a-sym ,d-sym)
                           (inst xor ,enc ,d-sym ,d-sym)
                           (inst mov ,enc ,c-sym ,d)
                           (inst mul ,enc ,c-sym)
                           (inst add ,enc ,b-sym ,a-sym))
                         ;; must zero all of RBX as the full register
                         ;; is used to calculate the write address
                         `((inst xor :qword ,b-sym ,b-sym)
                           (inst add ,cty ,b-sym ,d-sym)))))))

;; (build-decoder-x86asm '(a c d b) '(20 5 1) 32 8)

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
               (ctag (case coordinate-type (8 :byte) (16 :word) (32 :dword) (64 :qword)))
               ;; (ilocation (sb-c::with-array-data ((raveled varray-point) (start 0) (end))
               ;;              (sb-vm::sap-int (sb-sys::vector-sap raveled))))
               ;; (olocation (sb-c::with-array-data ((raveled output-array) (start 0) (end))
               ;;              (sb-vm::sap-int (sb-sys::vector-sap raveled))))
               (el-width) (transfer-type)
               (temp-syms '(ra rc rd rb r8 r9 r10 r11 r12 r13 r14 r15)))
           
           ;; can also loop across sb-vm::*room-info*
           (loop :for i :across sb-vm::*specialized-array-element-type-properties*
                 :while (not el-width)
                 :when (and (sb-vm::specialized-array-element-type-properties-p i)
                            (equalp (array-element-type varray-point)
                                    (sb-vm::saetp-specifier i)))
                   :do (setf el-width (sb-vm::saetp-n-bits i)
                             transfer-type (case el-width (64 :qword) (32 :dword)
                                                 (16 :word) (t :byte))))
           
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
                 (:results (rout :scs (unsigned-reg)))
                 (:result-types unsigned-num)
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
                 (:generator
                  1 (inst xor :qword r10 r10)
                  (inst mov :qword r10 st) ;; starting point
                  ;; (inst mov :qword r12 ct)
                  ;; (inst add :qword r12 st) ;; ending point
                  (inst add :dword ct st) ;; ending point
                  ;; (inst mov :qword r9 st)
                  ;; ,@(when (< 8 el-width)
                  ;;     `((inst shl :qword r9 ,(ash el-width -3))))
                  (inst mov :qword r8 ia)
                  ;; (inst add :qword r8 r9)
                  (inst mov :qword r9 oa)
                  ,@(build-encoder-x86asm 'st '(ra rc rd rb)
                                          (get-dimensional-factors ishape)
                                          encoding coordinate-type)
                  (inst mov ,enc r11 rb)
                  (inst mov ,enc rd rb)
                  START-LOOP
                  ;; do operations here on d-sym
                  ;; ,@(loop :for ef :in effectors
                  ;;         :append (funcall ef '(ra rc rd rb)))
                  ,@(build-decoder-x86asm '(ra rc rd rb)
                                          (get-dimensional-factors oshape)
                                          encoding coordinate-type)

                  ;; move data with b-sym
                  (inst mov ,transfer-type ra (ea r8 rb ,(ash el-width -3)))
                  (inst mov ,transfer-type (ea r9 r10 ,(ash el-width -3)) ra)
                  
                  (inst mov ,enc rd r11)
                  ,@(build-iterator-x86asm 'rd (shape-of varray)
                                           encoding coordinate-type)
                  (inst mov ,enc r11 rd)
                  (inst inc :qword r10)
                  (inst cmp :qword r10 ct)  ;; reached ending point yet?
                  (inst jmp :b START-LOOP)  ;; if not, loop again
                  (inst mov :dword rout r11)))
               (setf (symbol-function 'varray::vop-ph)
                     (lambda (a b c d)
                       (declare (optimize (speed 3) (safety 0)))
                       (varray::vop-ph a b c d))))
            varray-point :x86-asm)))))
    (t (call-next-method))))

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
           (destructuring-bind (ra rc rd rb) symbols
             (declare (ignorable ra rc rd rb))
             `((inst add ,etag ,rd ,sum))))))
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
           (destructuring-bind (ra rc rd rb) symbols
             (declare (ignorable ra rc rd rb))
             `((inst add ,ctag ,rd ,(ash (vaturn-degrees varray) (* cindex coordinate-type)))
               (inst cmp ,ctag ,rd ,(ash dimension (* cindex coordinate-type)))
               (inst jmp :b ROTATED)
               (inst sub ,ctag ,rd ,(ash dimension (* cindex coordinate-type)))
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
             (destructuring-bind (ra rc rd rb) symbols
               (declare (ignorable ra rc rd rb))
               `((inst bswap ,etag ,rd)
                 ,@(when (/= encoding (* cwidth (rank-of varray)))
                     `((inst shr ,etag ,rd ,(- encoding (* cwidth (rank-of varray))))))))))))
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
                 (destructuring-bind (ra rc rd rb) symbols
                   (declare (ignorable ra rc rd rb))
                   (if (zerop fraction)
                       ;; perform a bit-shift if dividing by a power of 2
                       `((inst mov :qword ,ra ,rd)
                         (inst and :qword ,rd ,mask)
                         (inst shr :qword ,rd ,bits)
                         ,@(when (/= axis (1- (rank-of varray)))
                             `((inst and :qword ,rd ,mask)))
                         ;; must mask again in case a 1 got shifted to the next byte, unless
                         ;; expanding on the last axis so the bit would shift into nothingness
                         (inst and :qword ,ra ,(- mask))
                         (inst add :qword ,rd ,ra))
                       `((inst mov :qword ,ra ,rd)
                         (inst mov :qword ,rb ,rd)
                         (inst and :qword ,ra ,mask)
                         (inst mov :qword ,rc ,(vads-argument varray))
                         (inst xor :qword ,rd ,rd)
                         (inst div :qword ,rc)
                         (inst mov :qword ,rd ,rb)
                         (inst and :qword ,rd ,(- mask))
                         (inst add :qword ,rd ,ra))))))))))
      (t (call-next-method)))))
