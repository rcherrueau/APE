#lang typed/racket/base

(require "utils.rkt"
         racket/list
         racket/match
         racket/string)

(require/typed "utils.rkt"
  [unlines ((U String (Listof String))* -> String)]
  [snoc (All (A) ((Listof A) A -> (Listof A)))])

(provide (all-defined-out))


;; ASM Datatype
;;
;; Datatype for 32-bit x86 assembly. See,
;; https://www.cs.virginia.edu/~evans/cs216/guides/x86.html

;; ASM Registers -- 32-bit (4-byte). See,
;; https://www.cs.virginia.edu/~evans/cs216/guides/x86.html#registers
(define-datatype Register
  EAX  ;; Accumulator. The register that contains results. This
       ;; register is the one that compiled C programs expect to find
       ;; return values in.
  )

;; Arguments for ASM `Instruction`s. Seek notation in
;; https://www.cs.virginia.edu/~evans/cs216/guides/x86.html#instructions
;;
;; Not implemented:
;; <reg32>  Any 32-bit register (EAX, EBX, ECX, EDX, ESI, EDI, ESP, or EBP)
;; <reg16>  Any 16-bit register (AX, BX, CX, or DX)
;; <reg8>   Any 8-bit register (AH, BH, CH, DH, AL, BL, CL, or DL)
;; <mem>    A memory address (e.g., [eax], [var + 4], or dword ptr [eax+ebx])
;; <con32>  Any 32-bit constant
;; <con16>  Any 16-bit constant
;; <con8>   Any 8-bit constant
(define-datatype Arg
  [Const Number]   ;; Any 8-, 16-, or 32-bit numeric constant
  [Reg Register]   ;; Any named register
  )

;; Machine Instructions. See
;; https://www.cs.virginia.edu/~evans/cs216/guides/x86.html#instructions
(define-datatype Instruction
  [Move Arg Arg]   ;; Copies the data item referred to by its second
                   ;; operand (i.e. register contents, memory
                   ;; contents, or a constant value) into the location
                   ;; referred to by its first operand (i.e. a
                   ;; register or memory).
  [Add Arg Arg]    ;; Adds together its two operands, storing the
                   ;; result in its first operand. Note, whereas both
                   ;; operands may be registers, at most one operand
                   ;; may be a memory location.
  )

;; An `ASM` program is a list of `Instruction`s.
(define-type ASM (Listof Instruction))


;; Textual form

;; Converts a `Register` into a textual form.
(: register->string (Register -> String))
(define (register->string reg)
  (match reg
    [(EAX) "eax"]
    [else (error "Unsupported register " reg)]))

;; Converts an instruction `Arg` into a textual form.
(: arg->string (Arg -> String))
(define (arg->string arg)
  (match arg
    [(Const n) (number->string n)]
    [(Reg r)   (register->string r)]
    [else (error "Unsupported instruction argument " arg)]))

;; Converts an `Instruction` into a textual form.
(: instruction->string (Instruction -> String))
(define (instruction->string instruction)
  (match instruction
    [(Move a1 a2)
     (format "mov ~a, ~a" (arg->string a1) (arg->string a2))]
    [(Add a1 a2)
     (format "add ~a, ~a" (arg->string a1) (arg->string a2))]
    [else (error "Unsupported instruction " instruction)]))

;; Converts an `ASM` list of instructions into a textual form.
(: asm->string (ASM -> String))
(define (asm->string asm)
  ;; The textual form of all Instructions.
  (define the-asm-strings (map instruction->string asm))

  ;; Surround the program with the necessary scaffolding.
  (define $scaffolding
    (unlines "    .intel_syntax noprefix"
             "    .global _the_asm_code"
             "    .text"
             ""
             "_the_asm_code:"
             (make-list (length the-asm-strings) "    ~a")
             "    ret"))

  ;; The textual ASM program
  (apply format $scaffolding the-asm-strings))
