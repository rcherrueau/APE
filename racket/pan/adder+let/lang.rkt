#lang racket

(require (for-syntax racket/base
                     syntax/parse
                     (only-in "utils.rkt" unique-ids? primitive-op-id?))
         (prefix-in racket/base/ racket/base)
         racket/match
         syntax/parse/define
         typed/racket
         "utils.rkt")

;; Adder+let -- A starter language that add number + Let-bindings and
;; simple stack allocations.
;;
;; See:
;; - https://course.ccs.neu.edu/cs4410/lec_let-and-stack_notes.html
;; - https://course.ccs.neu.edu/cs4410/hw_02_assignment.html

;; n  ∈ Nat
;; id ∈ Identifier
;;
;; exp ::=                            (Exp)
;;         n                          (Num)
;;       | (add1 exp)                 (Prim1 Add1)
;;       | (sub1 exp)                 (Prim1 Sub1)
;;       | id                         (Id)  -- New in adder+let
;;       | (let ([id exp] ...) exp)   (Let) -- New in adder+let


;; Parser -- This language assumes an s-exp reader
(require "ast.rkt")

;; A module is an `EXP`, not the traditional list of `EXP` (ie,
;; EXP...). In other word, it ensures that only one program could be
;; given instead of a list of programs.
;;
;; The module compiles the underlining expressions with `compile-exp`.
;; Function `compile-exp` takes an `EXP` and produces an `ASM` (ie,
;; List of ASM instructions). It then gives the compiled expression to
;; `asm->string` that converts the `ASM` into a textual form.
(define-syntax (@%module-begin stx)
  (syntax-parse stx
    [(_ EXP:expr)
     #'(racket/base/#%module-begin
        (provide ast asm)              ;; Expose `ast` and `asm` out
                                       ;; for unit tests.

        (define ast EXP)               ;; The AST of the Program
        (define asm (compile-exp ast)) ;; The ASM of the Program
        (printf (asm->string asm))     ;; Print textual form of ASM
        )]))

;; Id
(define-syntax-parser @%id
  [(_ . ID:id)
   #'(Id 'ID)])

;; Let
(define-syntax (@let stx)
  (syntax-parse stx
    [(_ ([ID:id EXP] ...) BODY)
     #:fail-when (not (unique-ids? #'(ID ...))) "duplicate identifier found"
     #'(Let (list (cons 'ID EXP) ...) BODY)]))

;; Num -- Only Nat are valid program
(define-syntax-parser @%datum
  [(_ . N:nat) #'(Num (racket/base/#%datum . N))])

;; Prim1 Add1
(define-syntax-parser @add1
  [(_ EXP:expr) #'(Prim1 (Add1) EXP)])

;; Prim1 Sub1
(define-syntax-parser @sub1
  [(_ EXP:expr) #'(Prim1 (Sub1) EXP)])


;; A Note on The Stack -- How memory is used in programs. See,
;; https://course.ccs.neu.edu/cs4410/lec_let-and-stack_notes.html#(part._.The_stack)
;;
;; 32-bit architecture provides 32 bits to encode memory addresses.
;; That works out to 2^32 unique combinations of addresses. Thus,
;; memory is - in theory - an array addressed from 0 to 2^32. Note
;; that one address points to 1 byte of data. Therefore, memory is
;; made of 2^32 bytes of data (ie, 4294967296 bytes or 4 Gigabytes).
;;
;; There are restrictions and conventions on how to use memory
;; addresses appropriately. The memory addresses are organized as
;; follow:
;;
;; Low:     0 → +-------+
;;              |Code   |
;;              +-------+
;;              |Global |
;;              +-------+
;;              |       |
;;              |Heap   |
;;              |       |
;;              +-------+
;;              |       |
;;              |Stack  |
;;              |       |
;; High: 2^32 → +-------+
;;
;; The /Code/ includes code for the program. The /Global/ includes
;; global data. The /Heap/ includes dynamically allocated memory. The
;; /Stack/ is used as program calls functions and returns from them.
;;
;; Heap and Stack are adjacent and they must not overlap, or else the
;; same region of memory would not have a unique interpretation, and
;; the program will crash. To avoid this, the convention has been that
;; the heap grows upwards from "Lower" addresses, while the stack
;; grows downward from "Higher" addresses.
;;
;; The stack itself must conform to a particular structure, so that
;; functions can call each other reliably. From a bird's eye view, the
;; stack is divided into /Stack Frames/, one per function-in-progress.
;; A stack frame holds statically allocated memory (ie, the static
;; values of local variables). In the following, values we push on the
;; stack are numbers and all *4 bytes* long (size of an int in C).
;; Because one address of the stack points to 1 byte of data, and we
;; push data on the Stack frame downward from `ESP`. The address of
;; the first local value is `ESP - 4`, then the value of the second
;; local value is `ESP - 8` and so on. When the function returns, its
;; stack frame is feed for use by future calls.
;;
;; When a function is called, it needs to be told where its stack
;; frame begins. This address is stored in the `ESP` register (also
;; called, /Stack Pointer/):
;;
;;           +--------------+
;;           |              |
;;           | Not yet used |
;;           |              |
;; ESP - 8 → +--------------+
;;           |   Local #2   |
;; ESP - 4 → +--------------+
;;           |   Local #1   |
;;     ESP → +--------------+
;;           |    Earlier   |
;;           | stack frames |
;;    High → +--------------+


;; Compiler
(require "asm.rkt")

;; Take an `exp` and compiles into a list of assembly Instructions.
;; `env` is a map between and identifier and its address on the stack
;; frame (ie, its offset). `stack-offset` is the current offset value
;; for the stack frame (ie, number of 4 bytes values actually pushed
;; on the stack frame)
(: compile-exp (Exp (Immutable-HashTable Symbol Integer) Integer -> ASM))
(define (compile-exp exp [env (make-immutable-hash)] [stack-offset 1])
  (exp⇒asm exp

    [(Id id)
     ;; Finds offset of `id`.
     (define id-offset (hash-ref env id))
     => ;; Loads address of `id` in EAX.
        (Move (Reg (EAX)) (Mem (ESP) id-offset))]

    [(Let `((,id . ,val-exp)) body)
     => ;; Compiles binder expression `val-exp`; Result is in EAX.
        (compile-exp val-exp env stack-offset)
        ;; Moves results of EAX on the stack at current offset.
        (Move (Mem (ESP) stack-offset) (Reg (EAX)))
        ;; Puts id stack address in the env; Computes new offset; And
        ;; compiles Let body.
        (let* ([new-env (hash-set env id stack-offset)]
               [new-offset (add1 stack-offset)])
          (compile-exp body new-env new-offset))]

    [(Let `((,id . ,val-exp) ,bindings ...) body)
     ;; Rewrite Let* so that reminding bindings are part of a new Let
     ;; in the body (ie, recurs on Let binding until reducing to the
     ;; case with only one binder in the Let).
     (define b1 `((,id . ,val-exp))) ;; First binding
     (define bs bindings)            ;; Reminding bindings
     => (compile-exp (Let b1 (Let bs body)) env stack-offset)]

    [(Prim1 (Add1) exp)
     => ;; Compiles expression `exp`; Result is in EAX.
     (compile-exp exp env stack-offset)
     ;; Add 1 to EAX
     (Add (Reg (EAX)) (Const 1))]

    [(Prim1 (Sub1) exp)
     => ;; Compiles expression `exp`; Result is in EAX.
     (compile-exp exp env stack-offset)
     ;; Add 1 to EAX
     (Sub (Reg (EAX)) (Const 1))]

    ;; -- Definitions from "neonate"
    [(Num n) => (Move (Reg (EAX)) (Const n))]
    [else (error "Compilation Error: Unsupported Exp" exp)]))


;; Interface
(provide (rename-out [@%module-begin #%module-begin]
                     [@%datum #%datum]
                     [@add1 add1] [@sub1 sub1]
                     [@let let]
                     [@%id #%top])
         quote
         #%app #%top-interaction compile-exp)
