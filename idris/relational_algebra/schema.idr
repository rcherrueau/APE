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

-- List Inclusion: assert that the elements of the first list are
-- included in the second list.
Include : List a -> List a -> Type
Include xs ys = (z : _) -> Elem z xs -> Elem z ys

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
