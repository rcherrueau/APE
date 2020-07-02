#lang reader "../lang.rkt"

;; Prelude
(class X) (class Y) (class Unit)

;; Figure 2: Context Parameters Example
(class Pair{m n}
  (field [fst : m/X])
  (field [snd : n/Y]))

(class Intermediate
  (field [pair1 : rep/Pair{rep world}])
  (field [pair2 : world/Pair{rep world}])

  (def (a -> rep/Pair{rep world})
    pair1)

  (def (b -> world/Pair{rep world})
    pair2)

  (def (x -> rep/X)
    (get-field pair1 fst))

  (def (y -> world/Y)
    (get-field pair1 snd))

  (def (update-x -> Unit)
    (set-field! pair1 fst (new rep/X))
    (new Unit)))

(class Main
  (field [safe : Intermediate])

  (def (main -> Unit)

    ;; ;; fail -- pair1 belongs to Intermediate instance not the root
    ;; ;; system
    ;; (send safe a)

    ;; ;; fail -- may give a latter access to pair2.fst with `(get-field
    ;; ;; (send safe b) fst)`.
    ;; (send safe b)

    ;; ;; fail -- pair1.fst belongs to Intermediate instance not
    ;; ;; the root system.
    ;; (send safe x)

    ;; OK -- pair1.snd belongs to the root system.
    (send safe y)

    ;; OK -- unit operation valid even though it involves a `rep`
    ;; because the `rep` is accessed through Intermediate instance.
    ;; Encapsulation protects `rep` to go out from the `safe`
    ;; instance.
    (send safe update-x)))

(send (new Main) main)
