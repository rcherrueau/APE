#lang racket

(require (for-syntax racket/base
                     syntax/parse)
         (prefix-in racket/base/ racket/base)
         syntax/parse/define
         racket/match
         racket/undefined
         "asm.rkt"
         "utils.rkt")

;; Adder -- A starter language that add number + Let-bindings and
;; simple stack allocations.
;;
;; See:
;; - https://course.ccs.neu.edu/cs4410/lec_let-and-stack_notes.html
;; - https://course.ccs.neu.edu/cs4410/hw_02_assignment.html

;; n  ∈ Nat
;; id ∈ Identifier
;;
;; exp ::= n                          (Num)
;;       | (add1 exp)                 (Prim1 Add1)
;;       | (sub1 exp)                 (Prim1 Sub1)
;;       | id                         (Id)
;;       | (let ([id exp] ...) exp)   (Let)

(require (only-in "ast.rkt" Exp Num Prim1 Add1 Sub1 Let Id))


;; Parser
(extends-lang "adder.rkt")

(define-syntax-parser id
  [(_ . ID:id) #'(Id 'ID)])

(define-syntax (let stx)
  (define (unique-ids? ids)
    (not (check-duplicate-identifier (syntax->list ids))))

  (syntax-parse stx
    [(_ ([ID:id EXP] ...) BODY)
    (cond
      ;; Ensure all identifiers are unique
      [(unique-ids? #'(ID ...))
       #'(Let (list (cons 'ID EXP) ...) BODY)]
      [else
       (raise-syntax-error #f "duplicate identifier found" stx)])]))

(provide compile-exp (rename-out [id #%top]) let quote)


;; The Stack -- How memory is used in programs. See,
;; https://course.ccs.neu.edu/cs4410/lec_let-and-stack_notes.html#(part._.The_stack)
;;
;; 32-bit architecture provides 32 bits to encode memory addresses.
;; That works out to 2^32 unique combinations of addresses. Thus,
;; memory is - in theory - an array addressed from 0 to 2^32. Note
;; that one address points to 1 byte of data. Therefore, memory is
;; made of 2^32 bytes of data (ie, 4294967296 bytes of 4 Gigabytes).
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
;; global data. The /Heap/ includes memory dynamically allocated. The
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
;; When the function returns, its stack frame is feed for use by
;; future calls.
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
;;
;; The factor of 4 comes because addresses are measured in bytes.
;; TODO: Add an explanation on the handle of `let`.


;; Compiler
;;
;; - `env` is a map between and identifier and its address (ie, its
;;   offset).
;; - `stack-offset` is the current offset value for the stack frame.
;;
;; (: compile-exp (Exp (Immutable-HashTable Symbol Integer) Integer -> ASM))
(define (compile-exp exp [env (make-immutable-hash)] [stack-offset 1])
  (exp⇒asm exp

    [(Id x)
     ;; Finds offset of `x`.
     (define x-offset (hash-ref env x))
     => ;; Loads address of `x` in EAX.
        (Move (Reg (EAX)) (Mem (ESP) x-offset))]

    [(Let (list (cons x e)) e-body)
     => ;; Compiles binder expression `e`; Result is in EAX.
        (compile-exp e env stack-offset)
        ;; Moves results of EAX on the stack at current offset.
        (Move (Mem (ESP) stack-offset) (Reg (EAX)))
        ;; Puts x stack address in the env; Computes new offset; And
        ;; compiles Let body.
        (let* ([new-env (hash-set env x stack-offset)]
               [new-offset (racket/base/add1 stack-offset)])
          (compile-exp e-body new-env new-offset))]

    [(Let bindings body)
     ;; Rewrite Let so that remanding bindings are part of a new Let
     ;; in the body (ie, recur on Let binding until reducing to the
     ;; previous case).
     (define b1 (list (car bindings))) ;; First binding
     (define bs (cdr bindings))        ;; Reminding bindings
     => (compile-exp (Let b1 (Let bs body)) env stack-offset)]

    ;; -- Definitions from "adder.rkt"
    [(Prim1 (Add1) e)
     => (compile-exp e env stack-offset) (Add (Reg (EAX)) (Const 1))]
    [(Prim1 (Sub1) e)
     => (compile-exp e env stack-offset) (Sub (Reg (EAX)) (Const 1))]
    [(Num n)
     => (Move (Reg (EAX)) (Const n))]
    [else (error "Compilation Error: Unsupported Exp" exp)]))
