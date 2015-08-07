-- http://www.davidchristiansen.dk/2014/05/21/idris-dec-techniques/
module LT

%default total

-- We sometime need a proof as an argument to a function. In many
-- cases, these proof can be constructed *automatically* by a
-- *decision procedure*. Idris offers decision procedure for some
-- property `p` which is a function that returns something in the type
-- `Dec p`.

-- An instance of `Dec p` is either a proof of `p` or a proof that `p`
-- is an absurdity. The datatype is available in `Prelude.Basics` and
-- is defined as ```
-- data Dec : Type -> Type where
--   Yes : p -> Dec p
--   No  : (p -> Void) -> Dec p
-- ```

-- As an example, take the LTE predicate form the Idris library that
-- represent a proof that one natural number is less than or equal to
-- another:
data MyLTE : Nat -> Nat -> Type where
  lteZero : MyLTE Z m
  lteSucc : MyLTE n m -> MyLTE (S n) (S m)

-- It happens to be the case that, for any two natural numbers, it is
-- decidable whether the first is less than or equal to the second. To
-- prove this, we start with two simple lemmas. The first states that
-- it is not the case that the successor any number is less than or
-- equal to zero:
notMyLTESZ : MyLTE (S k) Z -> Void
notMyLTESZ lteZero impossible

-- Additionally, we show that, if k !≤ j, then k + 1 !≤ j + 1
notMyLTESS : (MyLTE k j -> Void) -> MyLTE (S k) (S j) -> Void
notMyLTESS nope (lteSucc x) = nope x

-- We are now in position to write the decision procedure, by
-- examining each case.
decMyLTE : (n: Nat) -> (m: Nat) -> Dec (n `MyLTE` m)
decMyLTE Z     m                     = Yes lteZero
decMyLTE (S k) Z                     = No notMyLTESZ
decMyLTE (S k) (S j) with (decMyLTE k j)
  -- Here, pattern matching on the left of `|` is not mandatory since
  -- making a decision -- at the right of `=` -- does not require
  -- accessing `k` of `j`.
  decMyLTE (S k) (S j) | (Yes p)     = Yes (lteSucc p)
  decMyLTE (S k) (S j) | (No contra) = No (notMyLTESS contra)

-- Now, we can see various techniques for getting Idris to fill out
-- these proof for us.

-- First, with interactive editing feature to get idris to construct
-- the proof term directly.
ok : 3 `MyLTE` 5 -- C-c C-s on OK
-- ok = ?ok_rhs -- C-c C-a on metavariable
ok = lteSucc (lteSucc (lteSucc lteZero))
-- And here, the decision procedure simply is not necessary!
-- Alternatively, tactic scripts are allowed as the right-hand side of
-- the definitions. So we can simply invoke the search tactiv
-- explicitly, rather than using it through the compiler's editor
-- interface.
okTactic : 3 `MyLTE` 5
okTactic = proof search
-- Once again, the decision procedure is not necessary! But Idris's
-- built-in proof search has two major limitations:
-- 1. It cannot solve more "interesting" goals, in which a simple
--    recursive application of constructors is insufficnet.
-- 2. It fails to find large goals, as the recursion depth is limited.
--    For example, it cannot solve 103 `MyLTE` 105
-- What we really want to do is use `decMyLTE` to find the term.
okImplicit : 103 `MyLTE` 105
okImplicit = getProof
  where getProof : {p: 103 `MyLTE` 105} -> {auto yep : decMyLTE 103 105 = Yes p} -> 103 `MyLTE` 105
        getProof {p} = p

-- testImplicit : (n: Nat) -> (m: Nat) -> n `MyLTE` m
-- testImplicit n m = getProof
--   where getProof : {p: n `MyLTE` m} -> {auto yep : decMyLTE n m = Yes p} -> n `MyLTE` m
--         getProof {p} = p

-- okTestImplicit : 103 `MyLTE` 105
-- okTestImplicit = testImplicit 103 105

getYes : (res : Dec p) -> case res of { Yes _ => p ; No _ => () }
getYes (Yes prf) = prf
getYes (No contra) = ()

okYes :  3 `MyLTE` 5
okYes = getYes (decMyLTE 3 5)

paramYes : (n: Nat) -> (m: Nat) -> n `MyLTE` m
paramYes n m = getYes (decMyLTE n m)
