#lang racket/base

(require racket/port
         (for-syntax racket/base))

;; From http://beautifulracket.com


;; Reader
;;
;; Implementation of stacker reader. The reader is in charge of
;; parsing your program and producing an AST in return. To do so, the
;; reader implements a specific methods call `read-syntax' that takes
;; your program as an `input-port' and produces a s-exp `syntax'
;; object that represents your AST wrapped into a `module'. The module
;; then tells racket where to find the semantic of your program (the
;; expander).
;;
;; The reader of the `stacker' language produces an AST derived from the
;; following grammar:
;; Root ::= (handle x) Root | (handle x)   with x an identifier

(provide read-syntax)

(define (read-syntax src-path in)
  (define src-lines (port->lines in))
  (define src-datums (map string->datum src-lines))
  (define ast (map (λ (x) `(handle ,x)) src-datums))
  (datum->syntax #f `(module stacker-module "stacker.rkt"
                       ,@ast)))

(define (string->datum str)
  (unless (equal? "" str)
    (let ([result (read (open-input-string (format "(~a)" str)))])
      (if (= (length result) 1)
          (car result)
          result))))


;; Expander
;;
;; Implementation of stacker expander. The expander is in charge of
;; defining the semantic of your program with real Racket expressions.
;; These expressions are then evaluated to produce a result.
;; Concretely, an expander for a specific language is provided by
;; implementing a function `#%module-begin' that returns a `syntax'
;; object. This syntax object can be transformed thanks to macro and
;; are expanded until the most basic forms (fix point) that should
;; call `#%module-begin' of an underlining language (e.g., Racket).
;; The underlining language handles the final evaluation.
;;
;; - A name used in the code is called an /identifier/.
;; - A connection to an actual value or function is called a /binding/.
;; - The expander prepares the code for evaluation by ensuring that
;;   /every identifier has a binding/.
;; - Once an identifier has a binding, it becomes a /variable/.
;;
;; Note: A syntax object defined by `#'' not only creates the datum,
;; but also captures the current /lexical context/, and attaches that
;; to the new syntax object. Lexical context is the list of available
;; variables which means that the syntax object made with `#'' will be
;; able to access all the variables defined at that point of the code.
;;
;; As a general methodology:
;; 1. Handle any language-specific processing of the code in the
;;    `#%module-begin' macro.
;; 2. Pass the result to an existing `#%module-begin' for the rest of
;;    the heavy lifting (note that all `#%module-begin' macro have the
;;    same name. Some care is needed to keep them straight).
;;
;; The expander of the `stacker' language calls the `#%module-begin'
;; of Racket/base (cf. `#lang racket/base'). Then evaluates `handle'
;; expression that pushes and pops number on the stack. And finally
;; prints the top of the stack.

(provide (rename-out [stacker-module-begin #%module-begin])
         #%top #%app #%datum #%top-interaction
         handle + *)

(define-syntax (stacker-module-begin stx)
  (syntax-case stx ()
      [(_ HANDLE-EXP ...)
       #'(#%module-begin
          ;; To debug, make it a symbol 'HANDLE-EXP ...
          HANDLE-EXP ...
          (displayln (car stack)))]))

;; The heavy lifting: Racket code that implements the semantics
(define stack '())

(define (pop-stack!)
  (define hd (car stack))
  (set! stack (cdr stack))
  hd)

(define (push-stack! arg)
  (set! stack (cons arg stack)))

(define (handle [arg #f])
  (cond
    [(number? arg)
     (push-stack! arg)]
    [(or (equal? * arg) (equal? + arg))
     (define op-result (arg (pop-stack!) (pop-stack!)))
     (push-stack! op-result)]))
