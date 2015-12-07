-- http://docs.idris-lang.org/en/latest/effects/depeff.html

import Effects
import Effect.State
import Effect.StdIO

import Data.Vect

-- Operation that changes the available effects.
namespace DependentStates
  -- function that reads input form the console, converts it to an
  -- integer and adds it to a list which is stored in a `STATE`
  readInt1 : Eff () [STATE (List Int), STDIO]
  readInt1 = do let x = trim !getStr
                put (cast x :: !get)

  -- What if, instead of a list of integers, we store the value into a
  -- `Vect` :
  --
  -- > readInt2 : Eff () [STATE (Vect n Int), STDIO]
  -- > readInt2 = do let x = trim !getStr
  -- >               put (cast x :: !get)
  --
  -- Doesn't type check since, `Vect (S n) Int` which is the type of
  -- `put (cast x :: !get)` mismatch with `Vect n Int` which is the
  -- type of the state.
  --
  -- Instead, we need to specify in the type that the operation begins
  -- with an availbale effect `Vect n Int`, and ends with effect `Vect
  -- (S n) Int`. `Effects.DepEff.Eff` is what we are looking for
  --
  --  λΠ> :doc Effects.DepEff.Eff
  --  Effects.DepEff.Eff : (x : Type) ->
  --  (es : List EFFECT) -> (ce : x -> List EFFECT) -> Type
  --
  -- With `es` the list of input effects and `ce` the list of output
  -- effects.
  readInt2 : Eff () [STATE (Vect n Int), STDIO]
                    [STATE (Vect (S n) Int), STDIO]
  -- Now, we need to update the STATE type at the same time as the
  -- value. Let's do a reasearch inspired by the `put`:
  --
  -- λΠ> :s y -> Eff _ [STATE _] [STATE y]
  -- > Effect.State.putM : y -> Eff () [STATE x] [STATE y]
  --
  -- Bingo!
  readInt2 = do let x = trim !getStr
                putM (cast x :: !get)


-- Effectful operation that changes the effect.
namespace ResultDependentEffects
  -- We want to update the vector only if the input is a valid
  -- interger. At first attempt, we put a test and update the STATE
  -- only if the input is a valid integer.
  --
  -- > readInt3 : Eff Bool [STATE (Vect n Int), STDIO]
  -- >                     [STATE (Vect (S n) Int), STDIO]
  -- > readInt3 = do let x = trim !getStr
  -- >               case all isDigit (unpack x) of
  -- >                 False => pure False -- `pure` and `return` are aliases
  -- >                 True  => do putM (cast x :: !get)
  -- >                             pure True
  --
  -- Problem, the type mismatch between `Vect n Int` which is the type
  -- of the vector in the `False` case, and the expected type `Vect (S
  -- n) Int`.
  --
  -- The size of the vector in the result type depends on whether of
  -- not the read value is valid. Let's exepress this in the type:
  readInt3 : Eff Bool [STATE (Vect n Int), STDIO]
                      (\ok => if ok then [STATE (Vect (S n) Int), STDIO]
                                    else [STATE (Vect n Int), STDIO])
  readInt3 = do let x = trim !getStr
                case all isDigit (unpack x) of
                  False => pureM False  -- pureM allows the output
                                        -- effects to be calculated
                                        -- from the given value.
                  True  => do putM (cast x :: !get)
                              pureM True

  -- Now, when we use `readInt3`, we have to check its return value to
  -- know what the new set of effects is. The next function reads a
  -- set number of values into a vector:
  readN : (n : Nat) -> Eff () [STATE (Vect m Int), STDIO]
                              [STATE (Vect (n + m) Int), STDIO]
  readN Z         = pure ()
  readN (S k) {m} = case !readInt3 of
                      False => readN (S k)
                      -- λΠ> :s k + (S m) = S (k + m)
                      -- = Prelude.Nat.plusSuccRightSucc : (left : Nat) ->
                      -- (right : Nat) ->
                      -- S (left + right) = left + S right
                      True  => rewrite plusSuccRightSucc k m in readN k
  -- /!\ Only `case` will work here. We cannot use `if/then/else`
  -- because the `then` and `else` branches must have the same type.

-- Local Variables:
-- idris-load-packages: ("effects")
-- End:
