#lang racket/base

(require racket/match
         racket/function
         racket/hash
         racket/list
         racket/pretty
         )

(provide (all-defined-out))

(define (zip l1 l2) (map list l1 l2))

(struct ownership-scheme (type owner cparams) #:prefab)

;; mk-ownership-type: context → ownership-scheme → ownership-type
(define (mk-ownership-type Σ o-scheme)
(void)
  )

;; statically-visible? : expr -> o-scheme -> Bool
(define (statically-visible? expr t)

  ;; contexts : ownership-scheme → list
  (define (contexts o-scheme)
    (cons (ownership-scheme-owner o-scheme)
          (ownership-scheme-cparams o-scheme)))

  (cond
    ;; If expr is this, then OK
    [(eq? expr 'this) #t]
    ;; Else, rep should not be in contexts
    [else (not (member 'rep (contexts t)))]))

(define (write-ownership-scheme expr)
  (match expr
    [(ownership-scheme type owner cparams)
     (printf "type: ~s, owner: ~s, cparams: ~s~n" type owner cparams)]
    [`(class ,O-SCHEME _ ,BODY ...)
     (write-ownership-scheme O-SCHEME)
     (for-each write-ownership-scheme BODY)]
    [`(field ,O-SCHEME _)
     (write-ownership-scheme O-SCHEME)]
    [`(def ,R-O-SCHEME _ ((,A-O-SCHEMEs _) ...) ,BODY)
     (write-ownership-scheme R-O-SCHEME)
     (for-each write-ownership-scheme A-O-SCHEMEs)
     (write-ownership-scheme BODY)]
    [`(new ,O-SCHEME)
     (write-ownership-scheme O-SCHEME)]
    ;; [`(get-field ,CLASS ,FIELD) (void)]
    [`(set-field! ,CLASS ,FIELD ,E)
     (write-ownership-scheme E)]
    [`(send ,CLASS-E ,DEF ,ARGS ...)
     (write-ownership-scheme CLASS-E)
     (for-each write-ownership-scheme ARGS)]
    ;; Prog
    [`(prog ,CLASSES ... ,E)
     (for-each write-ownership-scheme CLASSES)
     (write-ownership-scheme E)]
    ;; Else
    [_ (void)]))

;; A map from fields name to their types
;; field-types: expr -> symbol -> #hash([FIELD . TYPE])
;; FIXME: All field name have to be unique.
(define (type-fields expr)
  (match expr
    ;; Class
    [`(class ,O-SCHEME ,NAME ,BODY ...)
     (define tf-fields (map type-fields BODY))
     (foldr hash-union (car tf-fields) (cdr tf-fields))]
    ;; Field
    [`(field ,O-SCHEME ,NAME)
     (hash NAME O-SCHEME)]
    ;; Prog
    [`(prog ,CLASSES ... ,E)
     (define tfs-classes (map type-fields CLASSES))
     (foldr hash-union (car tfs-classes) (cdr tfs-classes))]
    [_ #hash()]))

(define (type-check prog)
  ;; (pretty-write prog)
  ;; (write-ownership-scheme prog)

  (define 𝒜 (type-fields prog))
  (writeln 𝒜)

  (define (tc Σ Γ expr)
    (writeln expr) (write Σ) (writeln Γ)

    (match expr
      ;; Class
      [`(class ,O-SCHEME ,NAME ,BODY ...)
       (define new-Σ (cons 'Θ (ownership-scheme-cparams O-SCHEME)))
       (define new-Γ (hash 'this O-SCHEME))
       (for-each (curry tc new-Σ new-Γ) BODY)
       ]

      ;; Field
      [`(field ,O-SCHEME ,NAME)
       (tc Σ Γ O-SCHEME)
       ]

      ;; Def
      [`(def ,RET-O-SCHEME ,NAME (([,ARG-O-SCHEMEs ...] . ,ARG-NAMEs) ...) ,BODY)
       (tc Σ Γ RET-O-SCHEME)
       (for-each (curry tc Σ Γ) ARG-O-SCHEMEs)
       (let* ([ARGs  (zip ARG-O-SCHEMEs ARG-NAMEs)]
              [new-Γ (apply hash-set* Γ (apply append ARGs))])
         (tc Σ new-Γ BODY))
       ]

      ;; Let
      [`(let [,ARG-O-SCHEME ,ARG-NAME ,ARG-EXP] ,BODY)
       (tc Σ Γ ARG-O-SCHEME)
       (tc Σ Γ ARG-EXP)

       (let ([new-Γ (hash-set Γ ARG-NAME ARG-O-SCHEME)])
         (tc Σ new-Γ BODY))
       ]

      ;; New
      [`(new ,O-SCHEME)
       (tc  Σ Γ O-SCHEME)
       ]

      ;; Get Field
      [`(get-field ,CLASS ,FIELD)
       (define o-scheme (hash-ref 𝒜 FIELD))
       (unless (statically-visible? CLASS o-scheme)
         (error (format "Not (SV ~s ~s) -- (2)" CLASS o-scheme)))
       ]

      ;; Set Field
      [`(set-field! ,CLASS ,FIELD ,E)
       (tc Σ Γ E)

       (define o-scheme (hash-ref 𝒜 FIELD))
       (unless (statically-visible? CLASS o-scheme)
         (error (format "Not (SV ~s ~s) -- (3)" CLASS o-scheme)))
       ]

      ;; Send

      ;; Type
      [(ownership-scheme TYPE OWNER CPARAMS)
       (let* ([Σ (append Σ '(rep norep))]
              [types (cons OWNER CPARAMS)])

         (for ([t types])
           (unless (member t Σ)
             (error (format "Type error: ~s not in ~s -- (1)" t Σ)))))]

      ;; Prog
      [`(prog ,CLASSES ... ,E)
       (for-each (curry tc '() #hash()) CLASSES)
       (tc '() #hash() E)]

      [_ (writeln 'nop)]

   ))

  (tc '() #hash() prog))

;; (define (type-check expr)
;;   (match expr
;;     [`(class ,CLASS ,CPARAMS ,BODY ...) (...)]
;;     [`(field ,(O-SCHEME OWNER CPARAMS) ,FIELD) (...)]
;;     ;; [`(def ,R-O-SCHEME ,DEF ,A-O-SCHEME ,BODY) (...)]
;;     [`(def ,(R-O-SCHEME R-OWNER R-CPARAMS) ,DEF ,A-O-SCHEME ,BODY) (...)]
;;     [`(new ,CLASS ,OWNER ,CPARAMS) (...)]
;;     [`(get-field ,CLASS ,FIELD) (...)]
;;     [`(set-field! ,CLASS ,FIELD ,E) (...)]
;;     [`(send ,CLASS-E ,DEF ,ARG ...) (...)]
;;     ;; Prog
;;     [(list class ... e) (...)]))


;; (define (test-statically-visible expr)
;;   (define (print-test expr scheme)
;;     (printf "expr: ~.S, o-scheme: ~s~n" expr scheme)
;;     (printf "↪ statically-visible? ~s~n" (statically-visible? expr scheme))
;;     )
;;   (match expr
;;       [`(class ,O-SCHEME ,NAME ,BODY ...)
;;        (print-test expr O-SCHEME)
;;        (for-each test-statically-visible BODY)]
;;       [`(field ,O-SCHEME ,NAME)
;;        (print-test expr O-SCHEME)]
;;       ;; [`(def ,R-O-SCHEME ,NAME ((,A-O-SCHEMEs ,A-NAMEs) ...) ,BODY)
;;       ;;  (write-ownership-scheme R-O-SCHEME)
;;       ;;  (for-each write-ownership-scheme A-O-SCHEMEs)
;;       ;;  (write-ownership-scheme BODY)]
;;       ;; [`(new ,O-SCHEME)
;;       ;;  (write-ownership-scheme O-SCHEME)]
;;       ;; ;; [`(get-field ,CLASS ,FIELD) (void)]
;;       ;; [`(set-field! ,CLASS ,FIELD ,E)
;;       ;;  (write-ownership-scheme E)]
;;       ;; [`(send ,CLASS-E ,DEF ,ARGS ...)
;;       ;;  (write-ownership-scheme CLASS-E)
;;       ;;  (for-each write-ownership-scheme ARGS)]
;;       ;; ;; Prog
;;       [`(,CLASSES ... ,E)
;;        (for-each test-statically-visible CLASSES)
;;        (test-statically-visible E)]
;;       ;; Else
;;       [_ (void)]))
