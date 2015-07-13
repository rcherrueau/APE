module sql.row

import schema
import Data.List

%default total

infixr 7 |:
infixr 7 |+

Schema : Type
Schema = List Attr

data Row : Schema ts -> Type where
  RNil : Row (Sch [])
  (|:) : {n:String} -> (v: t) -> Row (Sch ts) -> Row (Sch $ (MkAttr n t) :: ts)

row1 : Row scAgenda
row1 = "2015-07-08" |: "Alice" |: 0 |: RNil

row2 : Row scAgenda
row2 = "2015-07-08" |: "Bob"   |: 0 |: RNil

row3 : Row scAgenda
row3 = "2015-07-10" |: "Alice" |: 1 |: RNil

data Row' : List Attr -> Type where
  RNil' : Row' []
  (|+) : {n: String} -> (v: t) -> Row' ts -> Row' $ (MkAttr n t) :: ts

scAgenda' : List Attr
scAgenda' = attrDate :: attrName :: attrAddr :: Nil

row1' : Row' scAgenda'
row1' = "2015-07-08" |+ "Alice" |+ 0 |+ RNil'

row2' : Row' scAgenda'
row2' = "2015-07-08" |+ "Bob"   |+ 0 |+ RNil'

-- Operations
head : Row (Sch $ t :: ts) -> (type t)
head (r |: rs) = r

tail : Row (Sch $ t :: ts) -> Row (Sch ts)
tail (r |: rs) = rs

get : (t: Attr) -> Row (Sch ts) -> {auto p: t `Elem` ts} -> (type t)
get _ (t' |: ts) {p=Here}     = t'
get t (t' |: ts) {p=There p'} = get t ts {p=p'}

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
π : (s : Schema ts) -> Row s' -> {auto p : s `Sub` s'} -> Row s
π (Sch [])                      rs  = RNil
π (Sch $ t@(MkAttr n tt) :: ts) rs {p=Pop x {p=elem}} =
                                    let r = get t rs {p=elem} in
                                    r |: π (Sch ts) rs {p=x}

-- s' \\ s
diff : (s: Schema ts) -> Row (Sch ts') -> Row (Sch $ ts' \\ ts)
diff (Sch [])  rs    = rs
diff s         RNil ?= RNil
diff (Sch ts) rs {ts'=t'@(MkAttr n' _) :: ts'} = let hypo = (t' `elem` ts) in ?mlkjf
  -- | True ?= diff (Sch ts) (tail rs)
  -- | False ?= (|:) (head rs) (diff (Sch ts) (tail rs)) {n=n'}

diff'_lemmaSRNil_RNil : (l: List Attr) -> ([] \\ l) = []
diff'_lemmaSRNil_RNil []        = Refl
diff'_lemmaSRNil_RNil (x :: xs) = diff'_lemmaSRNil_RNil xs

diff' : (s: List Attr) -> Row' s' -> Row' (s' \\ s)
diff' [] rs    = rs
diff' s  RNil' ?= RNil'
diff' s (r |+ rs) {s'= (MkAttr n t) :: ys} with ((MkAttr n t) `elem` s)
  | True ?= diff' s rs
  | False = r |+ diff' s rs

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

---------- Proofs ----------

sql.row.diff'_lemma_1 = proof
  intros
  rewrite sym (diff'_lemmaSRNil_RNil s)
  rewrite sym (diff'_lemmaSRNil_RNil s)
  exact value


sql.row.diff_lemma_1 = proof
  intros
  induction ts
  compute
  exact value
  intros
  compute
  exact ihl__0
