-- idris -p effects rng.idr
module rng

import Effect.State
import Effects

Seed : Type
Seed = Integer

RAND : EFFECT
RAND = STATE Integer

srand : Seed -> Eff () [RAND]
srand seed = update (\x => seed)

nextInt : Eff Integer [RAND]
nextInt = do seed <- get
             let nseed = newSeed seed
             put nseed
             pure $ (transpose 5 nseed) * 2
  where
  transpose : Integer -> Integer -> Integer
  transpose dig x =
              let str  = show x in
              let str' = (foldr (\c => \(str,i) =>
                                    if i <= dig then (strCons  c str, i - 1)
                                    else (str, i - 1))
                                 ("", toIntegerNat $ length str)
                                 (unpack str)) in
              cast str {to=Integer}

  newSeed : Seed -> Seed
  newSeed seed = let stp1 = (transpose 3 seed) * 3 in
                 (transpose 4 stp1) * 6


prog1 : Integer
prog1 = runPure $ do srand 42      -- runPure use default env, ie
                     i <- nextInt  -- `Env id [State Int]`, which
                     j <- nextInt  -- the value is 0
                     pure j

-- runPureInit will use Handler instance of State to construct a STATE
-- Integer effect
prog2 : Integer
prog2 = runPureInit [42] $ do i <- nextInt
                              j <- nextInt
                              pure j

prog3 : Integer
prog3 = runPureInit ((::) 42 Nil {eff=State} {m=id}) $
                      do i <- nextInt
                         j <- nextInt
                         pure j
