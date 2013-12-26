;; Syntax: Meta-programming Helpers
;; Phases and Reusable Syntax Classes

;; Creating reusable syntax classes requires some awareness of the Racket
;; phase level separation. A syntax class defined immediately within a
;; module cannot be used by macros in the same module. It is defined at
;; the wrong phase.
;;
;; > (module phase-mismatch-mod racket
;;     (require (for-syntax syntax/parse)
;;              syntax/parse)
;;
;;     (define-syntax-class foo
;;       (pattern (a b c)))
;;
;;     (define-syntax (macro x)
;;       (syntax-parse x
;;         [(_ f:foo) #'(+ f.a f.b f.c)])))
;;
;; > syntax-parse: not defined as syntax class at: foo
;;
;; In the module above, the syntax class `foo' is defined at phase level
;; 0. The reference to `foo' within the `macro', however, is at phase
;; level 1, being the implementation of a macro transformer (Needing to
;; require `syntax/parse' twice, once normally and once `for-syntax' is a
;; common warning sign of phase level incompatibility.)
;;
;; A solution is to define the syntax class in a separate module and
;; require that module `for-syntax'. Be careful, those two module have
;; to be in separated files.

(module stxclass-mod racket
  (provide foo)
  (require syntax/parse)
  (define-syntax-class foo
    (pattern (a b c))))

(module macro-mod racket
  (provide macro)
  (require (for-syntax syntax/parse
                       'stxclass-mod))
  (define-syntax (macro x)
    (syntax-parse x
      [(_ f:foo) #'(+ f.a f.b f.c)])))

(require 'macro-mod)
(macro (1 2 3))
