module sql.schema

import Data.List
import Data.So

%default total

-- Attribute
record Attr where
  constructor MkAttr
  name : String
  type : Type

instance Eq Attr where
  attr == attr' = (name attr) == (name attr')

instance DecEq Attr where
    decEq x y = if x == y then Yes primitiveEq else No primitiveNotEq
       where primitiveEq : x = y
             primitiveEq = believe_me (Refl {x})
             postulate primitiveNotEq : x = y -> Void

attrDate : Attr
attrDate = MkAttr "Date" String

attrName : Attr
attrName = MkAttr "Name" String

attrAddr : Attr
attrAddr = MkAttr "Addr" Integer

attrId : Attr
attrId = (MkAttr "Id" Nat)

-- Schema
Schema : Type
Schema = List Attr

scAgenda : Schema
scAgenda = [attrDate, attrName, attrAddr]

-- List utils

InIntersection : a -> List a -> List a -> Type
InIntersection z xs ys = (Elem z xs, Elem z ys)

inIntersection : DecEq a => (z : a) -> (xs : List a) -> (ys : List a) -> Dec (InIntersection z xs ys)
inIntersection z xs ys with (isElem z xs)
  inIntersection z xs ys | (Yes zinxs) with (isElem z ys)
    inIntersection z xs ys | (Yes zinxs) | (Yes zinys) = Yes (zinxs, zinys)
    inIntersection z xs ys | (Yes zinxs) | (No zninys) = No (\(_,zinys) => zninys zinys)
  inIntersection z xs ys | (No zninxs) = No (\(zinxs,_) => zninxs zinxs)

-- List Inclusion: assert that the elements of the first list are
-- included in the second list.
Include : List a -> List a -> Type
Include xs ys = (z : _) -> Elem z xs -> Elem z ys

lem11 : (Include xs ys -> Void) -> Include (x :: xs) ys -> Void
lem11 nop incxxsys = nop $ \z => \zinxs => incxxsys z (There zinxs)


lem16 : Elem z [x] -> z = x
lem16 (There zinnil) = absurd zinnil
lem16 Here           = Refl

lem15 : (xs : List a) -> (z = x -> Void) -> Elem z (x :: xs) -> Elem z xs
lem15 [] znotx zinxxs = lem17 znotx zinxxs
  where
  lem17 : (z = x -> Void) -> Elem z [x] -> Elem z []
  lem17 _ (There zinnil) = absurd zinnil
  lem17 znotx Here = void (znotx Refl)
lem15 (z :: xs) znotx Here = There (lem15 xs znotx Here)
lem15 (x :: xs) znotx (There p) = p

lem12 : DecEq a => {x : a} -> Elem x ys -> Include xs ys -> Include (x :: xs) ys
lem12 xinys incxsys {x} {xs} = \z => \zinxs => case (decEq z x) of
                                               No znotx => let zinxs = lem15 xs znotx zinxs in
                                                           incxsys z zinxs
                                               Yes zisx => rewrite zisx in xinys

lem13 : (Elem x ys -> Void) -> Include xs ys -> Include (x :: xs) ys -> Void
lem13 xninys incxsys incxxsys {x} = xninys $ incxxsys x Here

include : DecEq a => (xs, ys : List a) -> Dec (Include xs ys)
include [] ys = Yes $ \z => \zinxs => case (isElem z ys) of
                                           (Yes prf) => prf
                                           (No contra) => absurd zinxs
include (x :: xs) ys with (include xs ys)
  include (x :: xs) ys | (Yes incxsys) with (isElem x ys)
    include (x :: xs) ys | (Yes incxsys) | (Yes xinys) = let xinxxs = Here {x=x} {xs=xs} in
                                                         Yes $ lem12 xinys incxsys
    include (x :: xs) ys | (Yes incxsys) | (No contra) = No (\incxxsys => lem13 contra incxsys incxxsys)
  include (x :: xs) ys | (No contra) = No (\incxxsys => lem11 contra incxxsys)


-- Construct the proof that `(xs ∩ ys) ⊆ ys`
xsInterYsIncYs : (Eq a, DecEq a) => (intersect, ys : List a) -> Include intersect ys
xsInterYsIncYs zs ys = xsInterYsIncYs'
  where
    postulate lem1 : (Elem z ys -> Void) -> (Elem z zs) -> Void

    xsInterYsIncYs' : Include zs ys
    xsInterYsIncYs' z zinzs with (isElem z ys)
      xsInterYsIncYs' z zinzs | (Yes prf)   = prf
      xsInterYsIncYs' z zinzs | (No contra) = void (lem1 contra zinzs)
-- interProof : (Eq a, DecEq a) => (xs,ys : List a) -> Include (intersect xs ys) ys
-- interProof xs ys = let zs = (intersect xs ys) in interProof'
--   where
--   postulate lem1 : (Elem z ys -> Void) -> (Elem z zs) -> Void

--   interProof' : Include zs ys
--   interProof' z zinzs with (isElem z ys)
--     interProof' z zinzs | (Yes prf) = prf
--     interProof' z zinzs | (No contra) = void (lem1 contra zinzs)
