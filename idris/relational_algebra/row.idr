module sql.row

import schema
import Data.List

%default total

infixr 7 |:

data Row : Schema -> Type where
  RNil : Row []
  (|:) : {n: String} -> (v: t) -> Row ts -> Row $ (MkAttr n t) :: ts

-- Operations
head : Row $ t :: ts -> (type t)
head (t |: ts) = t

tail : Row $ t :: ts -> Row ts
tail (t |: ts) = ts

get : (v: Attr) -> Row s -> {auto p: v `Elem` s} -> (type v)
get _ (t |: ts) {p=Here}     = t
get v (t |: ts) {p=There p'} = get v ts {p=p'}

union : Row s -> Row s' -> Row (s ++ s')
union RNil      us = us
union (t |: ts) us = t |: union ts us

delete : (v: Attr) -> Row s -> Row (v `delete` s)
delete v RNil = RNil
delete v (t |: ts) {s=(MkAttr n' v') :: _} with (v == (MkAttr n' v'))
  delete v (t |: ts) {s=(MkAttr n' v') :: _} | False = t |: delete v ts
  delete v (t |: ts) {s=(MkAttr n' v') :: _} | True  = ts

diff : (s': Schema) -> Row s -> Row (s \\ s')
diff []          rs = rs
diff (t' :: ts') rs = diff ts' (delete t' rs)

π : (s : Schema) -> Row s' -> Row (intersect s s')
π s rs {s'} = let zs = intersect s s' in π'
  where
  π' : Row zs
  π' {zs = []}                   = RNil
  π' {zs = ((MkAttr n t) :: xs)} =
                      let rec = π' {zs=xs} in
                      let include = xsInterYsIncYs ((MkAttr n t) :: xs) s' in
                      let prf = include (MkAttr n t) Here in
                      let r = get (MkAttr n t) rs {p=prf} in
                      r |: rec

frag : (s : Schema) -> Row s' -> (Row (s `intersect` s'),
                                  Row (s' \\ s))
frag s rs = let left = π s rs in
            let right = diff s rs in
            (left, right)


-- Note: The following requires a proof that s' is a subset of s.
-- Henceforth, the proof should be give as argument, which is not as
-- practiable as defining `π` as the intersection of s' and s.
data Sub : Schema -> Schema -> Type where
  Stop : Sub [] s'
  Pop  : Sub ts ts' -> {auto p: t `Elem` ts'} -> Sub (t :: ts) ts'

π' : (s' : Schema) -> Row s -> {auto p : s' `Sub` s} -> Row s'
π' []                       rs                    = RNil
π' (t'@(MkAttr _ _) :: ts') rs {p=Pop x {p=elem}} =
                                    let r = get t' rs {p=elem} in
                                    r |: π' ts' rs {p=x}

frag' : (s' : Schema) -> Row s -> {auto p : s' `Sub` s} -> (Row s',
                                                            Row (s \\ s'))
frag' s rs {p=p'} = let left = π' s rs {p=p'} in
                    let right = diff s rs in
                    (left, right)


π'' : (s : Schema) -> Row s' -> Row (intersect s s')
π'' s rs {s'} = let zs = intersect s s' in π'
  where
  lem : (Include zs s' -> Void) -> Elem z zs -> Elem z s' -> Void
  -- lem zsnins' zinzs zins' {z} = let zsins' = ?zsinz' in --z zinzs zins'
  --                               zsnins'

  π' : Row zs
  π' {zs = []}                   = RNil
  π' {zs = ((MkAttr n t) :: xs)} with (include ((MkAttr n t) :: xs) s')
    π' {zs = ((MkAttr n t) :: xs)} | (Yes zsins') =
                                           let prf = zsins' (MkAttr n t) Here in
                                           let r = get (MkAttr n t) rs {p=prf} in
                                           let rec = π' {zs=xs} in
                                           r |: rec
    π' {zs = ((MkAttr n t) :: xs)} | (No zsnins') = let inductHypo = π' {zs=xs} in
                                                    let foo = zsnins' ?foocontra in
                                                    ?π_rhs_2

-- Tests
row1 : Row scAgenda
row1 = "2015-07-08" |: "Alice" |: 0 |: RNil

row2 : Row scAgenda
row2 = "2015-07-08" |: "Bob"   |: 0 |: RNil

row3 : Row scAgenda
row3 = "2015-07-10" |: "Alice" |: 1 |: RNil

hd1  : String
hd1  = head row1

hd2  : type (head scAgenda)
hd2  = head row1

tl1  : Row [attrName, attrAddr]
tl1  = tail row1

tl2  : Row (tail scAgenda)
tl2  = tail row1

get1 : (get attrDate row1) = "2015-07-08"
get1 = Refl

get2 : String
get2 = get attrName row1

get3 : Integer
get3 = get attrAddr row1 {p=There (There Here)}

del1 : (delete attrName row1) = "2015-07-08" |: 0 |: RNil
del1 = Refl

del2 : Row [attrDate, attrAddr]
del2 = delete attrName row1

dif1 : Row scAgenda
dif1 = diff [] row1

dif2 : Row []
dif2 = diff scAgenda row1

dif3 : Row [attrDate, attrAddr]
dif3 = diff [attrName] row1

π1   : Row ([attrDate] `intersect` scAgenda)
π1   = π [attrDate] row1

π2   : Row scAgenda
π2   = π scAgenda row1

π3   : Row scAgenda
π3   = π scAgenda row1

π4   : Row [attrDate]
π4   = π [attrDate] row1

frg1 : (Row ([attrDate] `intersect` scAgenda), Row [attrName,attrAddr])
frg1 = frag [attrDate] row1

frg2 : (Row [attrDate], Row [attrName,attrAddr])
frg2 = frag [attrDate] row1

π'1  : Row [attrDate]
π'1  = π' [attrDate] row1

fg'1 : (Row [attrDate], Row [attrName, attrAddr])
fg'1 = frag' [attrDate] row1
