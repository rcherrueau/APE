#lang s-exp "surface-lang.rkt"

(class Driver
  ;; ...
  )

(class Pair [m n]
  (field [fst : (m / X)])
  (field [snd : (n / Y)]))

;; Check duplicated class
;; (class Driver []
;;   ;; ...
;;   )

(class Driver_ []
  ;; ...
  )

(class Intermediate
  (field [p1 : (rep   / (Pair rep norep))])
  (field [p2 : (norep / (Pair rep norep))])

  ;; Check duplicated field
  ;; (field [p2 : (norep / (Pair rep norep))])

  (def (a → [rep   / (Pair rep norep)])
    (get-field this p1))

  (def (b → [norep / (Pair rep norep)])
    p2)

  ;; Check duplicated def
  ;; (def (b) void)

  (def (x → [rep / X])
    (get-field p1 fst))

  (def (y → [norep / Y])
    (get-field p2 snd))

  (def (updateX → Unit)
    (set-field! (get-field this p1) fst (new X)))

  (def (get-engine [e : (rep / Engine)]
                   [b : (rep / (X a c))]
                   → [rep / (Pair rep norep)])
    (get-field this p1))
  )

(class Main []
  (field [safe : (norep / Intermediate)])

  (def (main → (rep Y))
    (let ([a : (rep   / (Pair rep norep))  (get-field safe a)]
          [b : (norep / (Pair rep norep))  (get-field safe b)]
          [x : (rep   / X)                 (get-field safe x)])
      (get-field safe y))))

(let ([main : Main (new Main)])
  (send main main))

;; Local Variables:
;; eval: (progn (defun racket/run-lang () (interactive) (save-buffer) (with-current-buffer "ctx-params-example.rkt" (racket-run))) (evil-define-key (quote normal) (quote racket-mode-map) (kbd "E") (quote racket/run-lang)))
;; End:
