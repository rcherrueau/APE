module sql.row

import Data.List

%default total

infixr 7 |:

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

Schema : Type
Schema = List Attr

data Sub : Schema -> Schema -> Type where
  Stop : Sub [] s'
  Pop  : Sub ts ts' -> {auto p: t `Elem` ts'} -> Sub (t :: ts) ts'

data Row : Schema -> Type where
  RNil : Row []
  (|:) : {n: String} -> (v: t) -> Row ts -> Row $ (MkAttr n t) :: ts

scAgenda : Schema
scAgenda = attrDate :: attrName :: attrAddr :: Nil

row1 : Row scAgenda
row1 = "2015-07-08" |: "Alice" |: 0 |: RNil

row2 : Row scAgenda
row2 = "2015-07-08" |: "Bob"   |: 0 |: RNil

row3 : Row scAgenda
row3 = "2015-07-10" |: "Alice" |: 1 |: RNil

-- Operations
head : Row $ t :: ts -> (type t)
head (r |: rs) = r

tail : Row $ t :: ts -> Row ts
tail (r |: rs) = rs

get : (t: Attr) -> Row s -> {auto p: t `Elem` s} -> (type t)
get _ (r |: rs) {p=Here}     = r
get t (r |: rs) {p=There p'} = get t rs {p=p'}

delete : (t: Attr) -> Row s -> Row (t `delete` s)
delete t RNil = RNil
delete t (r |: rs) {s=(MkAttr n' t') :: _} with (t == (MkAttr n' t'))
                                           | True = rs
                                           | False = r |: (delete t rs)

diff : (s': Schema) -> Row s -> Row (s \\ s')
diff []          rs = rs
diff (t' :: ts') rs = diff ts' (delete t' rs)

π : (s' : Schema) -> Row s -> {auto p : s' `Sub` s} -> Row s'
π []                       rs = RNil
π (t'@(MkAttr _ _) :: ts') rs {p=Pop x {p=elem}} =
                                    let r = get t' rs {p=elem} in
                                    r |: π ts' rs {p=x}

frag : (s' : Schema) -> Row s -> {auto p : s' `Sub` s} -> (Row s', Row (s \\ s'))
frag s rs {p=p'} = let left = π s rs {p=p'} in
                   let right = diff s rs in
                   (left, right)

-- TODO: implements π with intersect insted of proof in argument.
inter: List Attr -> List Attr -> List Attr
inter Nil       ys = Nil
inter (x :: xs) ys with (x `isElem` ys)
  | (Yes _)  = x :: inter xs ys
  | (No _)   = inter xs ys

inter_lemmaNilXS_Nil : (l: List Attr) -> ([] `inter` l) = []
inter_lemmaNilXS_Nil []        = Refl
inter_lemmaNilXS_Nil (x :: xs) = inter_lemmaNilXS_Nil xs

inter_lemmaXSNil_Nil : (l: List Attr) -> (l `inter` []) = []
inter_lemmaXSNil_Nil []        = Refl
inter_lemmaXSNil_Nil (x :: xs) = inter_lemmaXSNil_Nil xs

-- Note: proof `exact absurd`
pi : (s : Schema) -> Row s' -> Row (s `inter` s')
pi [] rs = RNil
pi (x :: xs) rs {s'=ts'} with (isElem x ts')
  pi (t@(MkAttr n _) :: xs) rs {s'=ts'} | (Yes prf) = let r = get t rs {p=prf} in
                                                      r |: (pi xs rs)
  pi (x :: xs) rs {s'=ts'} | (No contra) = (pi xs rs)

-- pi ((MkAttr n t) :: ts) rs {s'=ts'} with (isElem (MkAttr n t) ts')
--   | (Yes p) = ?pi_yes
--   | (No  _) = ?pi_no

-- with (t `isElem` ts')
--   pi (t :: ts) rs  | (Yes tints') = ?tyui
--   pi (t :: ts) rs  | (No tnints') = ?tyun

-- pi [] rs {s'=ts'} = RNil
-- pi ((MkAttr n t) :: ts) rs {s'=ts'} with ((MkAttr n t) `isElem` ts')
--   | (Yes p) ?= let r = get (MkAttr n t) rs {p=p} in (|:) r (pi ts rs) {n=n}

-- π' : (s' : Schema) -> Row s -> Row (s `inter` s')
-- π' [] rs ?= RNil
-- π' s  RNil = RNil
-- π' (MkAttr n' t' :: ts') rs {s=ts} with (MkAttr n' t' `isElem` ts)
--   | (Yes p) ?= let r = (get (MkAttr n' t') rs {p=p}) in
--               (|:) r (π' ts' rs) {n=n'}
--   | (No contra) ?= π' ts' rs

-- π' : (s': Schema) -> Row s -> Row (s `intersect` s')
-- π' [] rs ?= RNil
-- π' s  RNil = RNil
-- π' (t' :: ts') rs {s=ts} with (t' `isElem` ts)
--   π' (t' :: ts') rs {s=ts} | (Yes p) = ?lkjfs
  -- | (Yes p) ?= let r = (get t' rs {p=p}) in
  --             (|:) r  (π' ts' rs)
  -- | (No _) ?= π' ts' rs


-- utils for π'_lemma_3
-- mljk : (ts: List Attr) -> (ts': List Attr) ->
--     (p: (ts `inter` (t' :: ts')) = (ts `inter` ts')) -> Elem t' ts -> Void
-- mljk [] [] p = ?giveMeContra

-- π_lemmaEqNotIn : (p: Elem t' ts -> Void) -> (ts `inter` (t' :: ts')) = (ts `inter` ts')
-- π_lemmaEqNotIn {ts=[]} {ts'=[]} p = Refl
-- π_lemmaEqNotIn {ts=[]} {ts'=ts'} p = Refl
-- π_lemmaEqNotIn {ts=ts} {ts'=ts'} p = let inductHyp = π_lemmaEqNotIn {ts:ts}




-- Si un proc fait appel à get, on peut calculer la valeur de p grace
-- au decElem dans un let. l'astuce est de faire comme pour okImplicit
-- de decproc. Ici le okImplicit serait l'affectation de notre let.
-- TODO: Tester cette astuce dans la projection d'un get.


-- elemProof : (t: Attr) -> (ts: List Attr) -> t `Elem` ts
-- elemProof t ts = getProof
--   where getProof : {p : t `Elem` ts} -> {auto yep : isElem t ts = Yes p} -> t `Elem` ts
--         getProof {p} = p

-- Here is a way to use decision procedure. But, here I have to
-- specify the case where the proof is a contradiction (No case). The
-- second solution is ensuring in the prototype that every element of
-- s are parts of s'.
-- π : (s: Schema ts) -> (Row s') -> Row s
-- π (Sch [])        rs = RNil
-- π (Sch $ (MkAttr n t) :: ts) rs {s'=(Sch ts')} with (isElem (MkAttr n t) ts')
--   | (Yes p) = let r = (get (MkAttr n t) rs {p=p}) in
--               r |: π (Sch ts) rs
--   | (No contra) = ?rear
-- π : (s : Schema ts) -> Row s' -> {auto p : s `Sub` s'} -> Row s
-- π (Sch [])                      rs  = RNil
-- π (Sch $ t@(MkAttr n tt) :: ts) rs {p=Pop x {p=elem}} =
--                                     let r = get t rs {p=elem} in
--                                     r |: π (Sch ts) rs {p=x}

-- -- s' \\ s
-- diff : (s: Schema ts) -> Row (Sch ts') -> Row (Sch $ ts' \\ ts)
-- diff (Sch [])  rs    = rs
-- diff s         RNil ?= RNil
-- diff (Sch ts) rs {ts'=t'@(MkAttr n' _) :: ts'} = let hypo = (t' `elem` ts) in ?mlkjf
--   -- | True ?= diff (Sch ts) (tail rs)
--   -- | False ?= (|:) (head rs) (diff (Sch ts) (tail rs)) {n=n'}


-- diff' : (s: List Attr) -> Row' s' -> Row' (s' \\ s)
-- diff' [] rs    = rs
-- diff' s  RNil' ?= RNil'
-- diff' s (r |+ rs) {s'= (MkAttr n t) :: ys} with ((MkAttr n t) `elem` s)
--   | True ?= diff' s rs
--   | False = r |+ diff' s rs

-- diff (Sch ts) (r |: rs) {s'=(Sch $ t'@(MkAttr n' tt') :: ts')} =
--                let hypo = (t' `elem` ts) in
--                ?diff_lemma_3
  -- | True  ?= diff (Sch ts) rs
  -- | False ?= (|:) r (diff (Sch ts) rs) {n=n'}

-- frag : (s : Schema ts) -> Row s' -> {auto p : s `Sub` s'} -> (Row s, Row (s' `diff` s))
-- frag (Sch [])  rs ?= (RNil, rs)
-- frag s rs {p=p'} = let left = π s rs {p=p'} in
--                    let right = diff s rs in
--                    (left, right)

-- Tests
foo : (1 = 2) -> Void
foo Refl impossible


---------- Proofs ----------

-- sql.row.π'_lemma_1 = proof
--   intro
--   intro
--   exact absurd
--   intros
--   rewrite sym (inter_lemmaXSNil_Nil s)
--   exact value
