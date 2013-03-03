;;; jvm-class-file.lisp
;;;
;;; Copyright (C) 2010 Erik Huelsmann
;;; $Id: jvm-class-file.lisp 14096 2012-08-15 22:55:27Z ehuelsmann $
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
;;;
;;; As a special exception, the copyright holders of this library give you
;;; permission to link this library with independent modules to produce an
;;; executable, regardless of the license terms of these independent
;;; modules, and to copy and distribute the resulting executable under
;;; terms of your choice, provided that you also meet, for each linked
;;; independent module, the terms and conditions of the license of that
;;; module.  An independent module is a module which is not derived from
;;; or based on this library.  If you modify this library, you may extend
;;; this exception to your version of the library, but you are not
;;; obligated to do so.  If you do not wish to do so, delete this
;;; exception statement from your version.

(in-package "JVM")

(require '#:jvm-class-file)
(require '#:jvm-instructions)

(defvar *stack-effects*
  (make-hash-table :test 'eq))

(defun %define-stack-effect (names lambda)
  (dolist (name (if (consp names) names (list names)))
    (setf (gethash name *stack-effects*) lambda)))

(defmacro define-stack-effect (opcode args &body body)
  `(%define-stack-effect ',opcode (lambda ,args ,@body)))

(define-stack-effect (nop ineg lneg fneg dneg)
    (instruction stack locals pool)
  (declare (ignore instruction locals pool))
  stack)

(define-stack-effect aconst_null (instruction stack locals pool)
  (declare (ignore instruction locals pool))
  (cons :null stack))

(define-stack-effect (iconst_m1 iconst_0 iconst_1
                      iconst_2 iconst_3
                      iconst_4 iconst_5
                      bipush sipush
                      iload_0 iload_1
                      iload_2 iload_3) (instruction stack locals pool)
  (declare (ignore instruction locals))
  (cons :int stack))

(define-stack-effect iload (instruction stack locals pool)
  (declare (ignore instruction locals))
  (cons :int stack))

(define-stack-effect (aload_0 aload_1 aload_2 aload_3)
    (instruction stack locals pool)
  (declare (ignore instruction locals))
  (let* ((opcode (instruction-opcode instruction)))
    (cons (car (nth (ecase opcode
                      ;; todo? use the instruction opcode register
                      (aload_0 0)
                      (aload_1 1)
                      (aload_2 2)
                      (aload_3 3))
                    locals))
          stack)))

(define-stack-effect (istore fstore astore istore_0 istore_1
                      istore_2 istore_3 fstore_0 fstore_1 fstore_2
                      fstore_3 astore_1 astore_2 astore_3 pop)
    (instruction stack locals pool)
  (declare (ignore instruction locals))
  (cdr stack))

(defun apply-stack-effect (context instruction)
  (let ((handler (gethash (instruction-opcode instruction)
                          *stack-effects*)))
    (if handler
        (funcall handler instruction (method-context-stack context)
                 (method-context-locals context)
                 (class-pool (method-context-class context)))
        ;; (method-context-stack context)
        (assert (and "no opcode defined" nil)))))


(defstruct (method-context (:constructor %make-method-context))
  method ;; jvm method
  code   ;; list of lists with the first value the instruction,
         ;; the second the stack after instruction execution and
         ;; the third the state of the function locals during execution
  class ;; jvm class
  locals ;; a list of conses: each local occupies a cons of which
         ;;   the CAR is the type last declared (or NIL if none)
         ;;   and the CDR indicates availability (NIL or :AVAILABLE)
  stack  ;; a list of types pushed onto the stack
         ;;   either a symbol, indicating a primitive type, or
         ;;   a JVM-CLASS-NAME structure indicating a real class
  )

(defun make-method-context (class name return args &key (flags '(:public)))
  (let ((frame (make-stack-frame-state))
        (method (make-jvm-method name return args :flags flags)))
    (dolist (arg args)
      (allocate-local frame arg))
    (%make-method-context :method method
                          :code (method-ensure-code method)
                          :class class
                          :frame-state frame)))

(defun add-instruction (context instruction)
  "Adds the instruction to the method, updating the context's stack."
  (let ((stack (apply-stack-effect instruction (method-context-stack context))))
    (push (list instruction stack (method-context-locals context)) code)
    (setf (method-context-stack context) stack)))


(defun allocate-local (context type)
  (let ((allocated (find-if :available (method-context-locals context)
                            :key #'cdr))
        (new-value (cons type)))
    (setf (method-context-locals context)
          (if allocated
              (substitute (cons type) allocated
                          (method-context-locals context))
              (append (method-context-locals context)
                      (list new-value))))
    new-value))

(defun declare-local-type (context local-number type)
  (let ((local (nth local-number (method-context-locals frame))))
    (assert local)
    (setf (car local) type)))

(defun free-local (context local-number)
  (let ((local (nth local-number (method-context-locals frame))))
    (assert local)
    (setf (cdr local) :available)))








(declaim (ftype (function (t t t) t) analyze-stack-path))
(defun analyze-stack-path (code start-index depth)
  (declare (optimize speed))
  (declare (type fixnum start-index depth))
  (do* ((i start-index (1+ i))
        (limit (length code)))
       ((>= i limit))
    (declare (type fixnum i limit))
    (let* ((instruction (aref code i))
           (instruction-depth (instruction-depth instruction))
           (instruction-stack (instruction-stack instruction)))
      (declare (type fixnum instruction-stack))
      (when instruction-depth
        (unless (= (the fixnum instruction-depth)
                   (the fixnum (+ depth instruction-stack)))
          (internal-compiler-error "Stack inconsistency detected ~
                                    in ~A at index ~D: ~
                                    found ~S, expected ~S."
                                   (if *current-compiland*
                                       (compiland-name *current-compiland*)
                                       "<unknown>")
                                   i instruction-depth
                                   (+ depth instruction-stack)))
        (return-from analyze-stack-path))
      (let ((opcode (instruction-opcode instruction)))
        (setf depth (+ depth instruction-stack))
        (setf (instruction-depth instruction) depth)
        (unless (<= 0 depth)
          (internal-compiler-error "Stack inconsistency detected ~
                                    in ~A at index ~D: ~
                                    negative depth ~S."
                                   (if *current-compiland*
                                       (compiland-name *current-compiland*)
                                       "<unknown>")
                                   i depth))
        (when (branch-p opcode)
          (let ((label (car (instruction-args instruction))))
            (declare (type symbol label))
            (analyze-stack-path code (symbol-value label) depth)))
        (when (unconditional-control-transfer-p opcode)
          ;; Current path ends.
          (return-from analyze-stack-path))))))

(declaim (ftype (function (t) t) analyze-stack))
(defun analyze-stack (code exception-entry-points)
  (declare (optimize speed))
  (let* ((code-length (length code)))
    (declare (type vector code))
    (dotimes (i code-length)
      (let* ((instruction (aref code i))
             (opcode (instruction-opcode instruction)))
        (when (eql opcode 202) ; LABEL
          (let ((label (car (instruction-args instruction))))
            (set label i)))
        (unless (instruction-stack instruction)
          (setf (instruction-stack instruction)
                (opcode-stack-effect opcode))
          (unless (instruction-stack instruction)
            (sys::%format t "no stack information for instruction ~D~%"
                          (instruction-opcode instruction))
            (aver nil)))))
    (analyze-stack-path code 0 0)
    (dolist (entry-point exception-entry-points)
      ;; Stack depth is always 1 when handler is called.
      (analyze-stack-path code (symbol-value entry-point) 1))
    (let ((max-stack 0))
      (declare (type fixnum max-stack))
      (dotimes (i code-length)
        (let* ((instruction (aref code i))
               (instruction-depth (instruction-depth instruction)))
          (when instruction-depth
            (setf max-stack (max max-stack (the fixnum instruction-depth))))))
      max-stack)))

;; (defun analyze-locals (code)
;;   (let ((code-length (length code))
;;         (max-local 0))
;;     (dotimes (i code-length max-local)
;;       (let* ((instruction (aref code i))
;;              (opcode (instruction-opcode instruction)))
;;         (setf max-local
;;               (max max-local
;;                    (or (let ((opcode-register
;;                                 (jvm-opcode-register-used opcode)))
;;                          (if (eq t opcode-register)
;;                              (car (instruction-args instruction))
;;                              opcode-register))
;;                        0)))))))




(declaim (ftype (function (t) label-target-instructions) hash-labels))
(defun label-target-instructions (code)
  (let ((ht (make-hash-table :test 'eq))
        (code (coerce code 'vector))
        (pending-labels '()))
    (dotimes (i (length code))
      (let ((instruction (aref code i)))
        (cond ((label-p instruction)
               (push (instruction-label instruction) pending-labels))
              (t
               ;; Not a label.
               (when pending-labels
                 (dolist (label pending-labels)
                   (setf (gethash label ht) instruction))
                 (setf pending-labels nil))))))
    ht))



(defun delete-unused-labels (code handler-labels)
  (declare (optimize speed))
  (let ((code (coerce code 'vector))
        (changed nil)
        (marker (gensym)))
    ;; Mark the labels that are actually branched to.
    (dotimes (i (length code))
      (let ((instruction (aref code i)))
        (when (branch-p (instruction-opcode instruction))
          (let ((label (car (instruction-args instruction))))
            (set label marker)))))
    ;; Add labels used for exception handlers.
    (dolist (label handler-labels)
      (set label marker))
    ;; Remove labels that are not used as branch targets.
    (dotimes (i (length code))
      (let ((instruction (aref code i)))
        (when (= (instruction-opcode instruction) 202) ; LABEL
          (let ((label (car (instruction-args instruction))))
            (declare (type symbol label))
            (unless (eq (symbol-value label) marker)
              (setf (aref code i) nil)
              (setf changed t))))))
    (values (if changed (delete nil code) code)
            changed)))


(defun optimize-instruction-sequences (code)
  (let* ((code (coerce code 'vector))
         (changed nil))
    (dotimes (i (1- (length code)))
      (let* ((this-instruction (aref code i))
             (this-opcode (and this-instruction
                               (instruction-opcode this-instruction)))
             (labels-skipped-p nil)
             (next-instruction (do ((j (1+ i) (1+ j)))
                                   ((or (>= j (length code))
                                        (/= 202 ; LABEL
                                            (instruction-opcode (aref code j))))
                                    (when (< j (length code))
                                      (aref code j)))
                                 (setf labels-skipped-p t)))
             (next-opcode (and next-instruction
                               (instruction-opcode next-instruction))))
        (case this-opcode
          (205 ; CLEAR-VALUES
           (when (eql next-opcode 205)       ; CLEAR-VALUES
             (setf (aref code i) nil)
             (setf changed t)))
          (178 ; GETSTATIC
           (when (and (eql next-opcode 87)   ; POP
                      (not labels-skipped-p))
             (setf (aref code i) nil)
             (setf (aref code (1+ i)) nil)
             (setf changed t)))
          (176 ; ARETURN
           (when (eql next-opcode 176)       ; ARETURN
             (setf (aref code i) nil)
             (setf changed t)))
          ((200 167)                         ; GOTO GOTO_W
           (when (and (or (eql next-opcode 202)  ; LABEL
                          (eql next-opcode 200)  ; GOTO_W
                          (eql next-opcode 167)) ; GOTO
                      (eq (car (instruction-args this-instruction))
                          (car (instruction-args next-instruction))))
             (setf (aref code i) nil)
             (setf changed t))))))
    (values (if changed (delete nil code) code)
            changed)))

(defun optimize-jumps (code)
  (declare (optimize speed))
  (let* ((code (coerce code 'vector))
         (ht (label-target-instructions code))
         (changed nil))
    (dotimes (i (length code))
      (let* ((instruction (aref code i))
             (opcode (and instruction (instruction-opcode instruction))))
        (when (and opcode (branch-p opcode))
          (let* ((target-label (car (instruction-args instruction)))
                 (next-instruction (gethash1 target-label ht)))
            (when next-instruction
              (case (instruction-opcode next-instruction)
                ((167 200)                  ;; GOTO
                 (setf (instruction-args instruction)
                       (instruction-args next-instruction)
                       changed t))
                (176 ; ARETURN
                 (when (unconditional-control-transfer-p opcode)
                   (setf (instruction-opcode instruction) 176
                         (instruction-args instruction) nil
                         changed t)))))))))
    (values code changed)))

(defun delete-unreachable-code (code)
  ;; Look for unreachable code after GOTO.
  (declare (optimize speed))
  (let* ((code (coerce code 'vector))
         (changed nil)
         (after-goto/areturn nil))
    (dotimes (i (length code))
      (declare (type (unsigned-byte 16) i))
      (let* ((instruction (aref code i))
             (opcode (instruction-opcode instruction)))
        (cond (after-goto/areturn
               (if (= opcode 202) ; LABEL
                   (setf after-goto/areturn nil)
                   ;; Unreachable.
                   (progn
                     (setf (aref code i) nil)
                     (setf changed t))))
              ((unconditional-control-transfer-p opcode)
               (setf after-goto/areturn t)))))
    (values (if changed (delete nil code) code)
            changed)))

(defvar *enable-optimization* t)

(defknown optimize-code (t t) t)
(defun optimize-code (code handler-labels pool)
  (unless *enable-optimization*
    (format t "optimizations are disabled~%"))
  (when *enable-optimization*
    (when *compiler-debug*
      (format t "----- before optimization -----~%")
      (print-code code pool))
    (loop
       (let ((changed-p nil))
         (multiple-value-setq
             (code changed-p)
           (delete-unused-labels code handler-labels))
         (if changed-p
             (setf code (optimize-instruction-sequences code))
             (multiple-value-setq
                 (code changed-p)
               (optimize-instruction-sequences code)))
         (if changed-p
             (setf code (optimize-jumps code))
             (multiple-value-setq
                 (code changed-p)
               (optimize-jumps code)))
         (if changed-p
             (setf code (delete-unreachable-code code))
             (multiple-value-setq
                 (code changed-p)
               (delete-unreachable-code code)))
         (unless changed-p
           (return))))
    (unless (vectorp code)
      (setf code (coerce code 'vector)))
    (when *compiler-debug*
      (sys::%format t "----- after optimization -----~%")
      (print-code code pool)))
  code)

(defun code-bytes (code)
  (let ((length 0)
        labels ;; alist
        )
    (declare (type (unsigned-byte 16) length))
    ;; Pass 1: calculate label offsets and overall length.
    (dotimes (i (length code))
      (declare (type (unsigned-byte 16) i))
      (let* ((instruction (aref code i))
             (opcode (instruction-opcode instruction)))
        (if (= opcode 202) ; LABEL
            (let ((label (car (instruction-args instruction))))
              (set label length)
              (setf labels
                    (acons label length labels)))
            (incf length (opcode-size opcode)))))
    ;; Pass 2: replace labels with calculated offsets.
    (let ((index 0))
      (declare (type (unsigned-byte 16) index))
      (dotimes (i (length code))
        (declare (type (unsigned-byte 16) i))
        (let ((instruction (aref code i)))
          (when (branch-p (instruction-opcode instruction))
            (let* ((label (car (instruction-args instruction)))
                   (offset (- (the (unsigned-byte 16)
                                (symbol-value (the symbol label)))
                              index)))
              (assert (<= -32768 offset 32767))
              (setf (instruction-args instruction) (s2 offset))))
          (unless (= (instruction-opcode instruction) 202) ; LABEL
            (incf index (opcode-size (instruction-opcode instruction)))))))
    ;; Expand instructions into bytes, skipping LABEL pseudo-instructions.
    (let ((bytes (make-array length))
          (index 0))
      (declare (type (unsigned-byte 16) index))
      (dotimes (i (length code))
        (declare (type (unsigned-byte 16) i))
        (let ((instruction (aref code i)))
          (unless (= (instruction-opcode instruction) 202) ; LABEL
            (setf (svref bytes index) (instruction-opcode instruction))
            (incf index)
            (dolist (byte (instruction-args instruction))
              (setf (svref bytes index) byte)
              (incf index)))))
      (values bytes labels))))

(defun finalize-code (code handler-labels optimize pool)
  (setf code (coerce (nreverse code) 'vector))
  (when optimize
    (setf code (optimize-code code handler-labels pool)))
  (resolve-instructions (expand-virtual-instructions code)))


(provide '#:jvm-method)