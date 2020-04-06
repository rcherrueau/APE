#lang reader "../lang.rkt"

;; Type def
(class X) (class Y) (class Unit) (class Engine)

;; Prog
(class Driver #;... )

(class Pair {m n}
  (field [fst : m/X])
  (field [snd : n/Y])
  )

(class Intermediate
  (field [p1 : rep/Pair{rep world}])
  (field [p2 : Pair{rep world}])
  (field [p3 : Unit])
  (field [p4 : Engine])

  (def (a → rep/Pair{rep world})
    (get-field this p1))

  (def (b → world/Pair{rep world})
    p2)

  ;; ;; Check duplicated field
  ;; (field [p1 : rep/Pair{rep world}])

  ;; ;; Check duplicated def
  ;; (def (b → world/Pair{rep world}) ???)

  ;; ;; Check unknow field
  ;; (def (c → rep/Pair{rep world})
  ;;   foo)

  (def (x → rep/X)
    (get-field p1 fst))

  (def (y → Y)
    (get-field (get-field this p2) snd))

  ;; ;; Check fstt not a field of Pair
  ;; (def (updateX-bad → rep/X)
  ;;   (set-field! (get-field this p1) fstt (new rep/X)))

  (def (updateX → rep/X)
    (set-field! (get-field this p1) fst (new rep/X)))

  (def (with-args [e : Engine] → Unit)
    p3)

  ;; ;; Check bad call: unknown definition
  ;; (def (call-with-args-unknown-def → Unit)
  ;;   (send this with-args-udef p3))

  ;; ;; Check bad call: arity error
  ;; (def (call-with-args-arity-error0 → Unit)
  ;;   (send this with-args))

  ;; ;; Check bad call: arity error
  ;; (def (call-with-args-arity-error2 → Unit)
  ;;   (send this with-args p3 p3))

  ;; ;; Check bad call: type mismatch
  ;; (def (call-with-args-arity-error2 → Unit)
  ;;   (send this with-args p1))

  ;; ;; FIXME
  ;; ;; Note: seems fixed
  ;; (def (call-with-args → Unit)
  ;;   (send this with-args p4))

  (def (get-engine [e : rep/Engine]
                   [b : rep/X{a c}]
                   → rep/Pair{rep world})
    (get-field this p1))

  (def (get-engine2 [e : rep/Engine]
                    [b : rep/X{a c}]
                    → rep/Engine)
    e)

  ;; ;; Check: unknown Foo type
  ;; (def (unknown-type → Foo) ???)

  ;; ;; Check: type mismatch get-field
  ;; (def (get-engine-bad [e : rep/Engine]
  ;;                      [b : rep/X{a c}]
  ;;                      → rep/Engine)
  ;;   p1)

  (def (get-engine3 [e : rep/Engine]
                    [b : rep/X{a c}]
                    → rep/Engine)
    ???)
  )

(class Main []
  (field [safe : Intermediate])

  (def (main → rep/Y)
    (let ([a : rep/Pair{rep world}  (send safe a)]
          [b : Pair{rep world}      (send safe b)]
          [x : rep/X                (send safe x)])
      (send safe y))))

;; (send (new Main) main)
(let ([main : Main (new Main)])
  (send main main)
  )

;; Local Variables:
;; eval: (progn (defun racket/run-lang () (interactive) (save-buffer) (with-current-buffer "ctx-params-example.rkt" (racket-run))) (evil-define-key (quote normal) (quote racket-mode-map) (kbd "E") (quote racket/run-lang)))
;; End:
