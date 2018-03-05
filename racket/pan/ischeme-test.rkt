#lang s-exp "ischeme.rkt"

(ρ ([a 10])
   a)

(ρ ([a 10])
    (set! a 20))

(ρ ([a 10]
    [_ (set! a 20)])
   a)

((λ a a) 10)
