#lang reader "lang.rkt"

;; Type def
(class X) (class Y) (class Unit) (class Engine)

;; Prog
(class Driver #;... )

(class Pair [m n]
  (field [fst : m/X])
  (field [snd : n/Y])
  )


(class Intermediate
  ;; Check wrong owner/type stx
  ;; (field [p1 : rep/(Pair rep world)])
  (field [p1 : (rep/Pair rep world)])
  (field [p2 : (Pair rep world)])

  (def (a → [rep/Pair rep world])
    (get-field this p1))

  (def (b → [world/Pair rep world])
    p2)

  ;; Check duplicated field
  (field [p1 : (rep/Pair rep world)])

  ;; ;; Check duplicated def
  ;; (def (b → [world / (Pair rep world)]) void)

  ;; ;; Check unknow field
  ;; (def (c → [rep   / (Pair rep world)])
  ;;   foo)

  (def (x → rep/X)
    (get-field p1 fst))

  (def (y → Y)
    (get-field (get-field this p2) snd))

  (def (updateX → rep/X)
    ;; Check:
    ;; (set-field! (get-field this p1) fstt (new X)))
    (set-field! (get-field this p1) fst (new rep/X)))


  (def (get-engine [e : rep/Engine]
                   [b : (rep/X a c)]
                   → [rep/Pair rep world])
    (get-field this p1))

  (def (get-engine2 [e : rep/Engine]
                    [b : (rep/X a c)]
                    → rep/Engine)
    e)

  ;; Check: type mismatch get-field
  ;; (def (get-engine-bad [e : (rep / Engine)]
  ;;                      [b : (rep / (X a c))]
  ;;                      → [rep / Engine])
  ;;   (get-field this p1))

  (def (get-engine3 [e : rep/Engine]
                    [b : (rep/X a c)]
                    → rep/Engine)
    ???)
  )

(class Main []
  (field [safe : Intermediate])

  (def (main → rep/Y)
    (let ([a : (rep/Pair rep world)  (send safe a)]
          [b : (Pair rep world)      (send safe b)]
          [x : rep/X                 (send safe x)])
      (send safe y a b x))))

;; (send (new Main) main)
(let ([main : Main (new Main)])
  (send main main)
  )

;; Local Variables:
;; eval: (progn (defun racket/run-lang () (interactive) (save-buffer) (with-current-buffer "ctx-params-example.rkt" (racket-run))) (evil-define-key (quote normal) (quote racket-mode-map) (kbd "E") (quote racket/run-lang)))
;; End:
