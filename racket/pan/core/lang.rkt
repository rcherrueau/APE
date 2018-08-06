#lang racket

(require (for-syntax racket/base
                     syntax/parse)
         (prefix-in racket/base/ racket/base)
         "asm.rkt")

;; Base lang that defines `module-begin` with the core workflow
;; followed by all extension of the language. The workflow consists
;; in:
;; 1. Call macro expansion on the program to build the AST and make it
;;    available under `ast`.
;; 2. Call `compile-exp` of calling site on that AST to build ASM and
;;    make it available under `asm`.
;; 3. Compute and print the textual form of ASM.


;; Parser -- This language assumes an s-exp reader

;; A module is an `EXP`, not the traditional list of `EXP` (ie,
;; EXP...). In other word, it ensures that only one program could be
;; given instead of a list of programs.
;;
;; The module compiles the underlining expressions with `compile-exp`.
;; Function `compile-exp` takes an `EXP` and produces an `ASM` (ie,
;; List of ASM instructions). It then gives the compiled expression to
;; `asm->string` that converts the `ASM` into a textual form.
(define-syntax (#%module-begin stx)
  (syntax-parse stx
    [(_ EXP:expr)
     ;; By default, racket macro are hygienic. That is, racket
     ;; determines the binding of `compile-exp` identifier where the
     ;; macro is defined (aka, definition site). But I wanna define
     ;; new languages by reusing part of this language and extends
     ;; `compile-exp` at the place of the new language. Thus I want to
     ;; determine the binding of `compile-exp` at the place where the
     ;; macro is invoked (aka, calling site).
     ;;
     ;; To determine the binding of a identifier, racket looks inside
     ;; the /lexical context/. Thus, we should tell racket to create
     ;; the `compile-exp` identifier with the lexical context of the
     ;; new language definition.
     ;;
     ;; The `datum->syntax` function converts its second argument to a
     ;; syntax object by using the lexical context of its first
     ;; argument. Here, I create the `compile-exp` identifier with the
     ;; lexical context of `stx`. Considering that `stx` refers to a
     ;; `module` of the new language, the following does the trick.
     ;;
     ;; See: https://beautifulracket.com/explainer/hygiene.html
     (with-syntax ([COMPILE-EXP (datum->syntax stx 'compile-exp)])
       #'(racket/base/#%module-begin
          (provide ast asm)

          (define ast EXP)               ;; The AST of the Program
          (define asm (COMPILE-EXP ast)) ;; The ASM of the Program
          (printf (asm->string asm))     ;; Print textual form of ASM
          ))]))

(provide #%module-begin #%app #%top-interaction compile-exp)


;; Compiler

;; Do nothing
;; (: compile-exp (Exp -> ASM))
(define (compile-exp exp)
  (exp⇒asm exp
    [else (error "Compilation Error: Unsupported Exp" exp)]))
