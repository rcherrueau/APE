#lang racket

; Fear of Macros
; http://www.greghendershott.com/fear-of-macros/


; Most useful syntax transformers work by taking some input syntax,
; and rearranging the pieces into something else. As we saw, it's more
; convenient and less error-prone to use `match' to do pattern
; matching. It tunrs out that pattern-matching was one of the first
; improvements to be added to the Racket macro system. It's called
; `syntax-case' and has a shorthand for simple situations called
; `define-syntax-rule'.
; The way we specify the new syntax is similar. We d'ont need to do
; quasi-quoting and unquoting. We don't need to use `datum->syntax'.
; Instead, we supply a "template", which uses vaiable from the
; pattern.
(define-syntax (my-if-using-syntax-case stx)
  (syntax-case stx ()
    [(_ condition? right-expr left-expr)
     #'(cond [condition? right-expr]
             [else left-expr])]))
(my-if-using-syntax-case #t
       ((lambda () (displayln "true") "true"))
       ((lambda () (displayln "false") "false")))

; Pattern variable vs. template -- fight! ----------------------------

; Let's say we want to define a function with a hyphenated name, a-b,
; but we supply the a and b separately:

;; (define-syntax (hyphen-define stx)
;;   (syntax-case stx ()
;;     [(_ a b (args ...) body ...)
;;      (let ([name (string->symbol (format "~a-~a" a b))])
;;        #'(define (name args ...) body ...))]))

; patter-matching.rkt:35:49: a: pattern variable cannot be used
; outside of a template in: a. We have no idea what this error message
; means. Well, let's try to work it out. The "template the error
; message refers to is the #'(define (name args ...) body ...) part.
; The `let' isn't part of that template. It soundslike we can't use
; `a' (or a `b') in the `let' part. But you can have as many template
; as you want. Thus, you can use `syntax' on a pattern variable. This
; makes another template, albeit a small, template.

(define-syntax (hyphen-define/wrong stx)
  (syntax-case stx ()
    [(_ a b (args ...) body ...)
     (let ([name (string->symbol (format "~a-~a" #'a #'b))])
       #'(define (name args ...) body ...))]))
(hyphen-define/wrong foo bar () #t)
;; (foo-bar)
;; foo-bar: undefined;
;;  cannot reference an identifier before its definition

; Be careful, our macro is defining a function with some name other
; than `foo-bar'. Using the Macro Stepper in DrRacket, it appears that
; the use of our macro `(hyphen-define/wrong1.1 foo bar () #t)'
; exanpded to `(define (name) #t)'. Instead, we wanted to expand to
; `(define (foo-bar) #t)'. Our template is using the symbol `name' but
; we wanted its value, such as `foo-bar' in this use of macro.

; Our pattern doesn't include `name' because we don't expect it in the
; original syntax -- indeed the whole point of this macro is to create
; it. So name can't be in the main pattern. Fine -- let's make an
; additional pattern. We can do that using an additional nested
; `syntax-case'.

(define-syntax (hyphen-define/ok1 stx)
  ; Normally our transformer function is given syntax by Racket, and
  ; we pass that syntax to `syntax-case'. But we can also create some
  ; syntax of our own, on the fly, and pass that to `syntax-case'.
  (syntax-case stx ()
    [(_ a b (args ...) body ...)
     ; The whole `(datum->syntax ...)' expression is syntax that we're
     ; creating on the fly. Then, we can give that to `syntax-case',
     ; and match it using a pattern variable named `name'. Voilà, we
     ; have a new pattern variable. We can use it in a template, and
     ; its value will go in the template.
     (syntax-case (datum->syntax stx
                                 (string->symbol (format "~a-~a"
                                                         (syntax->datum #'a)
                                                         (syntax->datum #'b))))
                  ()
       [name
        #'(define (name args ...) body ...)])]))

; with-syntax --------------------------------------------------------

; Instead of an additional, nested `syntax-case', we could use
; `with-syntax'. This rearranges the `syntax-case' to look more like a
; `let' statement. Also, it's more convenient if we need to define
; more than one patter variable.
(define-syntax (hyphen-define/ok2 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body ...)
     ; Use the `with-syntax' as a let
     (with-syntax
         ([name (datum->syntax stx
                               (string->symbol (format "~a-~a"
                                                       (syntax->datum #'a)
                                                       (syntax->datum #'b))))])
       #'(define (name args ...) body ...))]))
(hyphen-define/ok2 foo bar () #t)
(foo-bar)

; Whether we use an additional `syntax-case' or use `with-syntax',
; either way we are simply defining additional pattern variable. Don't
; let the terminology and structure make it seem mysterious.

; We know that `let' doesn't let us use a binding in a subsequent one.
; Instead we can use nested lets. Or use a shortand for nesting,
; `let*'. Similary, instead of writing nested `with-syntax', we can
; use `with-syntax*'. One gotcha is that `with-syntax*' isn't provided
; by racket/base. We must require racket/syntax. Otherwise we may get
; a rather bewildering error message: ...: ellipses not allowed as an
; expression in: ...
(require (for-syntax racket/syntax))
(define-syntax (foo stx)
  (syntax-case stx ()
    [(_ a)
     (with-syntax* ([b #'a]
                    [c #'b])
       #'c)]))
(foo (displayln "foo"))

; format-id ----------------------------------------------------------

; There is a utility function in racket/syntax called `format-id' that
; lets us format identifier names more succintly than what we did
; above:
(require (for-syntax racket/syntax))
(define-syntax (hyphen-define/ok3 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body ...)
     (with-syntax
         ([name (format-id stx "~a-~a" #'a #'b)])
       #'(define (name args ...)
           body ...))]))
(hyphen-define/ok3 bar baz () #t)
(bar-baz)

; Another example ----------------------------------------------------

; A variation that accepts an arbitrary number of name parts to be
; joined with hyphens:
(require (for-syntax racket/string))
(define-syntax (hyphen-defines* stx)
  ; Takes a list of string and return a new syntax object where each
  ; string are hyphened
  (define (hyphenyze names)
    (datum->syntax stx (string->symbol (string-join names "-"))))
  (syntax-case stx ()
    [(_ (name ...) (arg ...) body ...)
     (with-syntax
         ([hyphened-name (hyphenyze
                          (map symbol->string
                               (map syntax->datum
                                    (syntax->list #'(name ...)))))])
       #'(define (hyphened-name arg ...)
           body ...))]))

(hyphen-defines* (foo bar baz) (v) (* 2 v))
(displayln (string-append "result is "
                          (number->string (foo-bar-baz 2))))

; Making our own struct ----------------------------------------------

; Let's apply what we just learned to a more-realistic example. We'll
; pretend that Racket doesn't already have a `struct' capability.
; Fortunately, we can write a macro to provide our own system for
; defining and using structure. To keep things simple, our structure
; will be immutable (read-only) and it won't support inheritance.
;
; (my-struct name (filed1 field2 ...))
;
; We need to define some procedures:
; * A constructor procedure whose name is the struct name. We'll
;   represent structures as a `vector'. The structure name will be
;   element zero. The fields will be elements one onward.
; * A predicate, whose name is the struct name with "?" appended.
; * For each field, an accessor procedure to get its value? These wil
;   be named "struc-field".
(require (for-syntax racket/syntax))
(define-syntax (my-struct stx)
  (syntax-case stx ()
    [(_ name (field ...))
     (with-syntax ([name? (format-id stx "~a?" #'name)])
     #`(begin
         ; Constructor
         (define (name field ...)
           (apply vector (cons 'name (list field ...))))
         ; Predicate
         (define (name? struct)
           (and (vector? struct)
                (eq? (vector-ref struct 0) 'name)))
         ; accessor for each field
         #,@(for/list ([x (syntax->list #'(field ...))]
                       [n (in-naturals 1)])
              (with-syntax ([name-field
                             (format-id stx "~a-~a" #'name x)]
                            [idx n])
                #`(define (name-field struct)
                    (unless (name? struct)
                      (error 'name-field
                             "~a is not a ~a struct" struct 'name))
                    (vector-ref struct idx))))))]))
; Test it out
(require rackunit)
(my-struct foo (a b))
(define s (foo 1 2))
(check-true (foo? s))
(check-false (foo? 1))
(check-equal? (foo-a s) 1)
(check-equal? (foo-b s) 2)
(check-exn exn:fail?
           (lambda () (foo-a "furble")))
; Next, what if someone tries to declare:
;; (my-struct "blah" ("bli" "blo"))
;; format-id: contract violation
;;  expected: (or/c string? symbol? identifier? keyword? char? number?)
;;  given: #<syntax:: "blah">

; The error message is not very helpful. It's coming from `format-id',
; which is a private implementation detail of our macro. A
; `syntax-case' clause can take an optional "guard" or "fender"
; expression. Insread of [pattern template] it cloud be
; [pattern fender template]:
(require (for-syntax racket/syntax))
(define-syntax (my-struct/fendered stx)
  (syntax-case stx ()
    [
     ; pattern
     (_ name (field ...))
     ; fender
     (for-each (lambda (x)
                 (unless (identifier? x)
                   (raise-syntax-error #f "not an identifier" stx x)))
               (cons #'name (syntax->list #'(field ...))))
     ; template
     (with-syntax ([name? (format-id stx "~a?" #'name)])
     #`(begin
         (define (name field ...)
           (apply vector (cons 'name (list field ...))))
         (define (name? struct)
           (and (vector? struct)
                (eq? (vector-ref struct 0) 'name)))
         #,@(for/list ([x (syntax->list #'(field ...))]
                       [n (in-naturals 1)])
              (with-syntax ([name-field
                             (format-id stx "~a-~a" #'name x)]
                            [idx n])
                #`(define (name-field struct)
                    (unless (name? struct)
                      (error 'name-field
                             "~a is not a ~a struct" struct 'name))
                    (vector-ref struct idx))))))]))
; Test it out
(require rackunit)
(my-struct/fendered foo/fendered (a b))
(define s/fendered (foo/fendered 1 2))
(check-true (foo/fendered? s/fendered))
(check-false (foo/fendered? 1))
(check-equal? (foo/fendered-a s/fendered) 1)
(check-equal? (foo/fendered-b s/fendered) 2)
(check-exn exn:fail?
           (lambda () (foo/fendered-a "furble")))
; Test a macro that raises a syntax error, raises it at compile time.
; But the `with-handlers' doesn't (or rather, wouldn't) get set up to
; catche the exception until run-time. The methode to catch the syntax
; error at run-time is to wrap the macro call in something that
; catches the compile-time exception and produces code that raises a
; similar run-time exception.
; http://lists.racket-lang.org/users/archive/2012-December/055343.html
(define-syntax (convert-syntax-error stx)
  (syntax-case stx ()
    [(_ expr)
     (with-handlers ([exn:fail:syntax?
                      (lambda (e)
                        #`(error '#,(exn-message e)))])
       (parameterize ((error-print-source-location #f))
         (local-expand #'expr 'expression null)))]))
(check-exn exn:fail?
           (lambda ()
             (convert-syntax-error
              (my-struct/fendered "blah" ("bli" "blo")))))

; Using dot notation -------------------------------------------------

; The previsous two examples used a macro to define functions whose
; names were made by joining identifiers provided to the macro. This
; example does the opposite: The identifier given to the macro is
; split into pieces.

; If you write programs for web services you deal with JSON, which is
; represented in Racket by a `jsexpr?'. JSON often has dictionaries
; that contain other dictionaires. In a `jsexpr?' these are
; represented by nested `hashed' tables:

; Nested `hasheq' typical of a jsexpr:
(define js (hasheq 'a (hasheq 'b (hasheq 'c "value"))))

; In Javascript you can use dot notation: `foo = js.a.b.c'. In Racket
; it's not so convenient:
; `(hash-ref (hash-ref (hash-ref js 'a) 'b) 'c)'
; We can write a helper function to make this a bit cleaner:
(define/contract (hash-refs h ks [def #f])
  ((hash? (listof any/c)) (any/c) . ->* . any)
  (with-handlers ([exn:fail? (const (cond [(procedure? def) (def)]
                                          [else def]))])
    (for/fold ([h h])
        ([k (in-list ks)])
      (hash-ref h k))))
(hash-refs js '(a b c))

; That's better. Can we go even further and use a dot notation
; somewhat like Javascript?
(require (for-syntax racket/syntax))
(define-syntax (hash.refs stx)
  (syntax-case stx ()
    ; Assume default is #f
    [(_ chain)
     #'(hash.refs chain #f)]
    [(_ chain default)
     (let ([xs (map (lambda (x)
                      (datum->syntax stx (string->symbol x)))
                    (regexp-split
                     #rx"\\."
                     (symbol->string (syntax->datum #'chain))))])
       (with-syntax ([h (car xs)]
                     [ks (cdr xs)])
         #'(hash-refs h 'ks default)))]))
(hash.refs js.a.b.c)
(hash.refs js.a.blah 'did-not-exist)

; We've started to appreciate that our macros should give helpful
; messages when used in error. Let's trys to do that here:
(require (for-syntax racket/syntax))
(define-syntax (hash.refs/fendered stx)
  (syntax-case stx ()
    ; No args
    [(_)
     ; Fence: raises syntax error
     (raise-syntax-error #f
                         "Expected (hash.key0[.key1 ...] [default])"
                         stx #'chain)]
    ; No default: Assume default is #f
    [(_ chain)
     #'(hash.refs/fendered chain #f)]
    [(_ chain default)
     ; Fence: Test chain is not a string or number
     (unless (symbol? (syntax-e #'chain))
       (raise-syntax-error #f
                           "Expected (hash.key0[.key1 ...] [default])"
                           stx #'chain))
       (let ([xs (map (lambda (x)
                        (datum->syntax stx (string->symbol x)))
                      (regexp-split
                       #rx"\\."
                       (symbol->string (syntax->datum #'chain))))])
         ; Fence: Test that we have at least hash.key
         (unless (and (>= (length xs) 2)
                      (not (eq? (syntax-e (cadr xs)) '||)))
           (raise-syntax-error #f
                               "Expected (hash.key0[.key1 ...] [default])"
                               stx #'chain))
         (with-syntax ([h (car xs)]
                       [ks (cdr xs)])
           #'(hash-refs h 'ks default)))]))
(hash.refs/fendered js.a.b.c)
(hash.refs/fendered js.a.blah 'did-not-exist)
(with-handlers ([exn:fail? (lambda (e) 'syntax-error-caught)])
  (convert-syntax-error (hash.refs/fendered)))
(with-handlers ([exn:fail? (lambda (e) 'syntax-error-caught)])
  (convert-syntax-error (hash.refs/fendered js."lala")))
(with-handlers ([exn:fail? (lambda (e) 'syntax-error-caught)])
  (convert-syntax-error (hash.refs/fendered js)))
