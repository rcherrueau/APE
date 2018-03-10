#lang s-exp "ischeme.rkt"

(ρ ([a 10])
   a)

(ρ ([a (ref 10)])
    a)

(ρ ([a (ref 10)])
   (deref a))

(ρ ([a (ref 10)])
   (set! a 10))

(ρ ([a (ref 10)]
    [_ (set! a 20)])
   ((λ a a)(deref a)))

((λ a a) 10)
