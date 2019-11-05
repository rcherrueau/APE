#lang reader "lang.rkt"

(class Driver
  ;; ...
  )

(class Pair [m n]
  (field [fst : (m / X)])
  (field [snd : (n / Y)])
  )

;; ;; Check duplicated class
;; (class Pair []
;;   ;; ...
;;   )

(class Pair_ []
  ;; ...
  )

(class Intermediate
  (field [p1 : (rep   / (Pair rep world))])
  (field [p2 : (world / (Pair rep world))])

  ;; ;; Check duplicated field
  ;; (field [p2 : (world / (Pair rep world))])

  (def (a → [rep   / (Pair rep world)])
    (get-field this p1))

  (def (b → [world / (Pair rep world)])
    p2)

  ;; ;; Check duplicated def
  ;; (def (b → [world / (Pair rep world)]) void)

  (def (x → [rep / X])
    (get-field p1 fst))

  (def (y → [world / Y])
    (get-field p2 snd))

  (def (updateX → Unit)
    (set-field! (get-field this p1) fst (new X)))

  (def (get-engine [e : (rep / Engine)]
                   [b : (rep / (X a c))]
                   → [rep / (Pair rep world)])
    (get-field this p1))

  (def (get-engine2 [e : (rep / Engine)]
                    [b : (rep / (X a c))]
                    → [rep / (Pair rep world)])
    e)

  )

(class Main []
  (field [safe : (world / Intermediate)])

  (def (main → (rep / Y))
    (let ([a : (rep   / (Pair rep world))  (get-field safe a)]
          [b : (world / (Pair rep world))  (get-field safe b)]
          [x : (rep   / X)                 (get-field safe x)])
      (get-field safe y))))

;; (send (new Main) main)
(let ([main : (world / Main) (new Main)])
  (send main main)
  )

;; Local Variables:
;; eval: (progn (defun racket/run-lang () (interactive) (save-buffer) (with-current-buffer "ctx-params-example.rkt" (racket-run))) (evil-define-key (quote normal) (quote racket-mode-map) (kbd "E") (quote racket/run-lang)))
;; End:
