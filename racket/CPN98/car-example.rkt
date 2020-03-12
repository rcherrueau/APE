#lang s-exp "surface-lang.rkt"

(class Engine
  (def (start → Unit) void)
  (def (stop  → Unit) void))

(class Driver
  ;; ...
  )

(class Driver []
  ;; ...
  )

(class Car [n m]
  (field [engine : (rep   / Engine)])
  (field [driver : (world / Driver)])

  (def (init → Unit)
    (set-field! this engine (new Engine)))

  (def (get-engine → [rep / Engine])
    (get-field this engine))

  (def (set-engine! [e : (rep / Engine)] → Unit)
    (set-field! this engine e))

  (def (go → Unit)
    (send engine start)))

(let ([bob : (world / Driver) (new (world / Driver))]
      [car : (world / Car) (send (new (world / Car)) init)]
      [_   : Unit (set-field! car driver bob)]
      [_   : Unit (send car go)]
      [e   : Engine (get-field car engine)]
      [_   : Unit (send e1  stop)]
      [e   : Engine (send car get-engine)]
      [_   : Unit (send e1 stop)]
      [e   : (rep / Engine) (new Engine)])
  (send car set-engine! e))