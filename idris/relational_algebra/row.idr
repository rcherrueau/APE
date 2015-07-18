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

π : (s : Schema) -> Row s' -> Row (s `inter` s')
π []        rs                                       = RNil
π (x :: xs) rs {s'=ts'} with (isElem x ts')
  π (t@(MkAttr n _) :: xs) rs {s'=ts'} | (Yes prf)   = let r = get t rs {p=prf} in
                                                       r |: (π xs rs)
  π (x :: xs) rs {s'=ts'}              | (No contra) = (π xs rs)

frag : (s : Schema) -> Row s' -> (Row (s `inter` s'), Row (s' \\ s))
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
π' []                       rs = RNil
π' (t'@(MkAttr _ _) :: ts') rs {p=Pop x {p=elem}} =
                                    let r = get t' rs {p=elem} in
                                    r |: π' ts' rs {p=x}

frag' : (s' : Schema) -> Row s -> {auto p : s' `Sub` s} -> (Row s', Row (s \\ s'))
frag' s rs {p=p'} = let left = π' s rs {p=p'} in
                    let right = diff s rs in
                    (left, right)

-- Je veux du void. Le seul moyen d'avoir du void c'est d'utiliser f.
-- Maintenant il me faut un preuve que `Elem x b`. Cette preuve, je
-- l'ai en Popant sur sub.
lemma1 : (Elem x b -> Void) -> Sub (x :: xs) b -> Void
lemma1 f (Pop x {p=xinb}) = f xinb

lemma2 : (Sub a b) -> (Sub (x :: a) b -> Void) -> (Elem x b -> Void)
lemma2 x f x1 = f (Pop x)

lemma3 : (Sub a b) -> (Sub (x :: a) b) -> (Elem x b)
lemma3 x (Pop {p} y) = p

lemma_AintB_isinB : (a : Schema) -> (b : Schema) -> Sub (intersect a b) b
lemma_AintB_isinB a b = lemma' (intersect a b)
  where
  lemma' : (s: Schema) ->  Sub s b
  lemma' []        = Stop
  lemma' (x :: xs) with (isElem x b)
    lemma' (x :: xs) | (Yes prf)   = Pop {p=prf} (lemma' xs)
    lemma' (x :: xs) | (No contra) with (lemma' xs)
      lemma' (x :: []) | (No contra) | Stop = ?foo
      lemma' (x :: (t :: ts)) | (No contra) | (Pop y) = ?truc_rhs_2


  -- void (lemma1 contra (Pop {p=} subrec))

 -- with (lemma' xs)
 --      lemma' (x :: [])        | (No contra)  | Stop             = ?lemma'_rhs_1
 --      lemma' (x :: (t :: ts)) | (No contra)  | (Pop y {p=yinb}) = ?lemm -- void (lemma1 contra (Pop {p=yinb} $ y)) -- ?lemma'_rhs_3

    -- lemma' (x :: [])        | Stop with (isElem x b)
    --   lemma' (x :: [])        | Stop | (Yes prf) = Pop Stop
    --   lemma' (x :: [])        | Stop | (No contra) = let rec = (lemma' []
    -- lemma' (x :: (t :: ts)) | (Pop y) = ?xs_rhs_2


pi : (s : Schema) -> Row s' -> Row (intersect s s')
pi s rs {s'} = let sub = lemma_AintB_isinB s s' in  -- assert_total $ makeSub (s `intersect` s') s' in
                π' (s `intersect` s') rs {p=sub}

inter2 : Schema -> Schema -> Schema
inter2 [] ys = []
inter2 (x :: xs) ys with (x `elem` ys)
  inter2 (x :: xs) ys | False = inter2 xs ys
  inter2 (x :: xs) ys | True = x :: (inter2 xs ys)

inter_lemmaNil : (l: Schema) -> (inter2 l []) = []
inter_lemmaNil [] = Refl
inter_lemmaNil (x :: xs) = inter_lemmaNil xs

lem1 : Sub (inter2 a b) b -> Elem x b -> Sub (inter2 (x :: a) b) b
lem1 p Here = ?lem1_rhs_1
lem1 p (There z) = ?with_pat

lem6 : (Elem z (_ :: b) -> Void) -> (Elem z b -> Void)
lem6 f Here = f (There Here)
lem6 f (There x) = f (There (There x))

lem7 : (z = x) -> (Elem z [x])
lem7 prf = rewrite prf in Here

lem9 : {x,z: Attr} -> (z = x -> Void) -> (z == x) = False
lem9 f {z} {x} with (z == x)
  lem9 f | False = Refl
  lem9 f | True = ?lem9_rhs_2

lem8 : (Elem z [x] -> Void) -> (z = x -> Void) -> inter2 [z] [x] = []
lem8 f g {z} {x} with (decEq z x) -- Je fais l'hypothèse absurd que z = x
  lem8 f g {z} {x} | (Yes prf) = void (g prf)
  lem8 f g {z} {x} | (No contra) = ?x_rhs_2

lem5 : (b : Schema) -> (Elem z b -> Void) -> inter2 [z] b = []
lem5 [] nop = Refl
lem5 (x :: []) nop {z} with (z `decEq` x)
  lem5 (x :: []) nop | (Yes prf) = void (nop (lem7 prf)) -- absurd: z = x and z ∉ [x]
  lem5 (x :: []) nop | (No contra) = ?lem5_rhs_2  -- z ‡ x and z ∉ [x]
lem5 (x :: xs) nop ?= lem5 xs (lem6 nop) -- (Elem z (x :: b) -> Void) -> (Elme z b -> Void)

lem4 : (b : Schema) -> (Elem z b) -> inter2 [z] b = [z]
lem4 []        zinb = absurd zinb
lem4 (z :: xs) Here = ?lem4_rhs_1 -- I have to reduce xs to []
lem4 (x :: xs) (There zinxs) = ?lem4_rhs_2

lem2 : (a,b : Schema) -> (Elem z b) -> inter (z :: a) b = z :: (inter2 a b)
lem2 [] [] zinb = absurd zinb
lem2 [] b zinb = ?lem2_rhs_5
lem2 (x :: xs) b zinb = ?lem2_rhs_2

lem3 : (xs, ys : Schema) -> (z : Attr) ->
  Elem z xs -> Elem z ys -> Elem z (inter2 xs ys)

makePrf : (a : Schema) -> (b : Schema) -> Sub (inter2 a b) b
makePrf [] b = Stop
makePrf (x :: xs) [] ?= Stop {s'=[]}
makePrf (x :: xs) b with (isElem x b)
  makePrf (x :: xs) b | (Yes prf) = let inter = makePrf xs b in
                                    ?rhs_1

  makePrf (x :: xs) b | (No contra) = ?rhs_2

pi2 : (s : Schema) -> Row s' -> Row (inter2 s s')
pi2 [] rs {s'} = RNil
pi2 (x :: xs) rs {s'} with (isElem x s')
  pi2 (x :: xs) rs {s'} | (Yes prf) = ?pi2_rhs_1
  pi2 (x :: xs) rs {s'} | (No contra) = ?pi2_rhs_2

  -- where
  -- -- TODO, make a Dec instead
  -- makeSub : (s : Schema) -> (s' : Schema) -> (s `Sub` s')
  -- makeSub []        s' = Stop
  -- makeSub (x :: xs) s' with (x `isElem` s')
  --   makeSub (x :: xs) s' | (Yes prf)  = Pop {p=prf} (makeSub xs s')
  --   -- makeSub (x :: xs) s' | (No contra) = absurd x



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

π1   : Row ([attrDate] `inter` scAgenda)
π1   = π [attrDate] row1

π2   : Row scAgenda
π2   = pi scAgenda row1

π3   : Row scAgenda
π3   = pi2 scAgenda row1

π4   : Row [attrDate]
π4   = pi2 [attrDate] row1

frg1 : (Row ([attrDate] `inter` scAgenda), Row [attrName,attrAddr])
frg1 = frag [attrDate] row1

-- frg2 : (Row [attrDate], Row [attrName,attrAddr])
-- frg2 ?= frag [attrDate] row1

π'1  : Row [attrDate]
π'1  = π' [attrDate] row1

fg'1 : (Row [attrDate], Row [attrName, attrAddr])
fg'1 = frag' [attrDate] row1

---------- Proofs ----------

sql.row.makePrf_lemma_1 = proof
  intros
  rewrite sym (inter_lemmaNil xs)
  exact value
