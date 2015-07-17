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

-- Utils
inter: Schema -> Schema -> Schema
inter Nil       ys = Nil
inter (x :: xs) ys with (x `isElem` ys)
  | (Yes _)  = x :: inter xs ys
  | (No _)   = inter xs ys

inter_lemmaNilXS_Nil : (l: Schema) -> ([] `inter` l) = []
inter_lemmaNilXS_Nil []        = Refl
inter_lemmaNilXS_Nil (x :: xs) = inter_lemmaNilXS_Nil xs

inter_lemmaXSNil_Nil : (l: Schema) -> (l `inter` []) = []
inter_lemmaXSNil_Nil []        = Refl
inter_lemmaXSNil_Nil (x :: xs) = inter_lemmaXSNil_Nil xs

-- inter_lemmaXSYS_YSXS : (xs,ys: Schema) -> (xs `inter` ys) = (ys `inter` xs)
-- inter_lemmaXSYS_YSXS [] []        = Refl

-- inter_lem1 : (t:Attr) -> (ts : Schema) -> (p : t `Elem` ts) -> (inter [t] ts) = [t]
-- inter_lem1 t [] p = absurd p
-- inter_lem1 t (x :: xs) p with (t == x)
--   inter_lem1 t (x :: xs) (There p) | False = inter_lem1 t xs p
--   inter_lem1 t (t :: xs) Here | False = ?absurd1
--   inter_lem1 t (t :: xs) Here | True = ?inter_lem1_rhs_2
--   inter_lem1 t (x :: xs) (There p) | True = ?absurd2

-- inter_lemNotIn : (t: Attr) -> (ts: Schema) -> (p: Elem t ts) -> (intersect [t] ts) = [t]
-- inter_lemNotIn t [] p = absurd p
-- inter_lemNotIn t (x :: xs) p with (p)
--   inter_lemNotIn t (t :: xs) _ | Here ?= (t :: (replace inter_lemIn (intersect [t] xs) t ts void) = [t] --?inter_lemNotIn_rhs_1
--   inter_lemNotIn t (x :: xs) _ | (There y) = ?inter_lemNotIn_rhs_2

-- inter_lemIn : (t: Attr) -> (ts: Schema) -> (p: (Elem t ts) -> Void) -> (intersect [t] ts) = []
