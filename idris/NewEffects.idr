-- Creating new effects
-- http://docs.idris-lang.org/en/latest/effects/impleff.html

import Effects

-- Effects are described by ADT. A constructor describes the operation
-- when the effect is available.
--
-- λΠ> :doc Effect
-- Effects.Effect : Type
-- The Effect type describes effectful computations.
--
-- This type is parameterised by:
-- + The return type of the computation.
-- + The input resource (the resource type on input)
-- + The computation to run on the resource given the return value (a
--   function which computes the resource type on output)
--
-- λΠ> :printdef Effect
-- Effect : Type
-- Effect = (x : Type) -> Type -> (x -> Type) -> Type
--
-- Speeking monad, the resource is what it is hide/manage/absract by
-- the monad, for instance, for the state monade, the resource is the
-- state. See the second and third parameter as type of the resource
-- before and after the computation.

namespace state
  data State : Effect where
    -- `Get` gets the current state so it returns something of the
    -- type of the current state `a`, because the input of the
    -- computation is something of type `a` (the type of the current
    -- state). And getting the current state doesn't alter the state
    -- so the output type of the state is also a `a`.
    Get : State a a (\x => a)
    -- `Put` replaces the state inside the effect, so it takes the
    -- new state as argument. The result of updating the effect
    -- returns an element of `Unit`. The state was initially of type
    -- `a`, but because we update it, the type of the state on output
    -- of the computation is `b`.
    Put : b -> State () a (\x => b)

  -- To make our effect (of type Effect) usable in an effect list (of
  -- type [EFFECT]), we have to convert it using `MKEff`.
  --
  --  λΠ> :doc EFFECT
  --  Data type Effects.EFFECT : Type
  --  The EFFECT Data type describes how to promote the Effect description into a
  --  concrete effect.
  --  Constructors:
  --  MkEff : Type -> ((x : Type) -> Type -> (x -> Type) -> Type) -> EFFECT
  STATE : Type -> EFFECT
  STATE t = MkEff t State

  -- Finally, we must explain how the effect is executed in a
  -- particular computation context (my rudimentary vision of this is
  -- see it as something similar as giving the implementation of the
  -- combinators for your monad, even if effect is not a monad ~~). So
  -- for handle function:
  -- + The first argument is the resource of the input.
  -- + The second argument is the effectful operation (ie, Get, Put
  --   for State).
  -- + The third argument is a continuation that escapes the pure
  --   program in order to achieve the effectful computation. The
  --   continuation waits for two arguments. The first one is the
  --   result value of the opération (get, put) and the second one is
  --   the updated resource.
  instance Handler State m where
    handle st  Get       k = k st st
    handle st1 (Put st2) k = k () st2

  -- Finally, because it's tidy to directly use `Get`, `Put` with
  -- `Eff`, we define top level functions
  get : Eff a [STATE a]
  get = call Get

  -- We can also add another combinator here. For instance, we do, at
  -- this level, the distinction between simple and dependent effects.
  put : a -> Eff () [STATE a]
  put a = call (Put a)

  putM : b -> Eff () [STATE a] [STATE b]
  putM b = call (Put b)

namespace exception
  data Exception : Type -> Effect where
    -- e is the type of the exception
    Raise : e -> (Exception e) b () (\x => ())

  EXCEPTION : Type -> EFFECT
  EXCEPTION t = MkEff () (Exception t)

  -- How to handle exception when run into Maybe
  instance Handler (Exception e) Maybe where
    handle _ (Raise x) k = Nothing

  -- writing:
  -- > instance Handler (Exception e) (Either e) where
  -- gives me a weird error type
  instance Handler (Exception ex) (Either ex) where
    handle _ (Raise x) k = (Left x)

  instance Handler (Exception e) List where
    handle _ (Raise x) k = []

-- Local Variables:
-- idris-load-packages: ("effects")
-- End:
