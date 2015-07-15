module sql.schema

import Data.List

%default total

record Attr where
  constructor MkAttr
  name : String
  type : Type

instance Eq Attr where
  attr == attr' = (name attr) == (name attr') -- && (So (type attr) = (type attr'))

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

data Schema : List Attr -> Type where
  Sch : (ts: List Attr) -> Schema ts

scAgenda : Schema (attrDate :: attrName :: attrAddr :: Nil)
scAgenda = Sch (attrDate :: attrName :: attrAddr :: Nil)

scNameDate : Schema (attrName :: attrDate :: Nil)
scNameDate = Sch (attrName :: attrDate :: Nil)

-- Predicate
-- total
-- elem : Attr -> Schema ts -> Bool
-- elem _ SNil     = False
-- elem t (Sch ts) = t `elem` ts

-- data Elem : Attr -> Schema ts -> Type where
--   Stop : Elem t (Sch $ t :: ts)
--   Pop  : Elem t (Sch ts) -> Elem t (Sch $ t' :: ts)

-- instance Uninhabited (t `Elem` SNil) where
--   uninhabited Stop impossible

-- -- lemma cf hole of (No ?contra)
-- lemmaNotElemT : (Elem t (Sch ts) -> Void) -> Elem t (Sch $ t' :: ts) -> Void
-- lemmaNotElemT nop (Pop x) = nop x

-- decElem : (t: Attr) -> (s: Schema ts) -> Dec (t `Elem` s)
-- decElem t SNil = No uninhabited
-- decElem t (Sch $ t' :: ts) with (decEq t t')
--   | (Yes p) ?= (Yes (Stop {t=t} {ts=ts}))
--   | (No  _) with (decElem t (Sch ts))
--     | (Yes p) = (Yes $ Pop p)
--     | (No contra) = (No $ lemmaNotElemT contra)

-- s C= s'
data Sub : Schema ts -> Schema ts' -> Type where
  Stop : Sub (Sch []) s'
  Pop  : Sub (Sch ts) (Sch ts') -> {auto p: t `Elem` ts'} -> Sub (Sch $ t :: ts) (Sch ts')
-- sub : Schema _ -> Schema _ -> Bool
-- sub (Sch [])        s' = True
-- sub (Sch $ t :: ts) s' = (t `elem` s') && ((Sch ts) `sub` s')

-- -- instance Uninhabited (((Sch $ t :: ts) `sub` (Sch []) = True) -> Void) where
-- --   uninhabited Refl impossible

-- Operations
head : Schema (t :: ts) -> Attr
head (Sch $ t :: ts) = t

tail : Schema (t :: ts) -> Schema ts
tail (Sch $ t :: ts) = (Sch ts)

delete : (t: Attr) -> Schema ts -> Schema (delete t ts)
delete t (Sch ts) = Sch (delete t ts)

-- total
-- lemma_unionTSNil_TS : (ts: List Attr) -> (ts ++ []) = ts
-- lemma_unionTSNil_TS [] = Refl
-- lemma_unionTSNil_TS ts = assert_total $ lemma_unionTSNil_TS ts

union : Schema ts -> Schema ts' -> Schema (ts ++ ts')
union (Sch ts) (Sch ts') = Sch (ts ++ ts')

-- total
-- lemma_diffNilTS_Nil : (ts: List Attr) -> ([] \\ ts) = []
-- lemma_diffNilTS_Nil [] = Refl
-- lemma_diffNilTS_Nil (t :: ts) = lemma_diffNilTS_Nil ts

diff : Schema ts -> Schema ts' -> Schema (ts \\ ts')
diff (Sch ts) (Sch ts') = Sch (ts \\ ts')

lemma_diffSNil_S : (s: Schema ts) -> (s `diff` (Sch [])) = s
lemma_diffSNil_S (Sch []) = Refl
lemma_diffSNil_S s        = assert_total $ lemma_diffSNil_S s

lemma_diffNilS_Nil : (s: Schema ts) -> ((Sch []) `diff` s) = (Sch [])
lemma_diffNilS_Nil (Sch [])        = Refl
lemma_diffNilS_Nil (Sch $ t :: ts) = lemma_diffNilS_Nil (Sch ts)

-- -- s ⊆ s'
-- -- data Sub : Schema ts -> Schema ts' -> Type where
-- --   Stop  : Sub (Sch []) s
-- --   Pop   : Sub (Sch ts) (Sch us) -> {auto p: (t `elem` us) = True} -> Sub (Sch $ t :: ts) (Sch us)

-- -- pSub : scNameDate `Sub` scAgenda
-- -- pSub = Pop (Pop Stop)

-- -- -- lemma that s !⊆ []
-- -- instance Uninhabited (Sub (Sch (t :: ts)) (Sch []))  where
-- --   uninhabited Stop impossible
-- --   uninhabited (Pop p) = uninhabited p

-- -- notSubSNil : {s: Schema (t :: ts)} -> s `Sub` (Sch []) -> Void
-- -- notSubSNil Stop impossible
-- -- notSubSNil (Pop p) = notSubSNil p

-- -- decSub : (s: Schema _) -> (s': Schema _) -> Dec (s `Sub` s')
-- -- decSub (Sch [])        s                = Yes Stop
-- -- decSub (Sch $ t :: ts) (Sch [])         = No notSubSNil
-- -- decSub (Sch $ t :: ts) (Sch us) with (decSub (Sch ts) (Sch us))
-- --   | (Yes p) = ?makep
-- --   | (No contra) = ?makenp

--   -- decSub (Sch []) s | (Yes Stop) = Yes (Pop Stop)
--   -- decSub s s'       | (Yes p {p=p'}) = Yes $ Pop p {p=p'}

-- -- data Schema : List Attr -> Type where
-- --   SNil : Schema []
-- --   (::) : (t: Attr) -> Schema ts -> Schema (t :: ts)

-- -- scAgenda : Schema (attrDate :: attrName :: attrAddr :: Nil)
-- -- scAgenda = attrDate :: attrName :: attrAddr :: SNil

-- -- total
-- -- head : Schema ts -> {auto ok: isCons ts = True} -> Attr
-- -- head SNil      {ok=Refl} impossible
-- -- head (s :: ss) {ok=p}    = s

-- -- total
-- -- tail : Schema (t :: ts) -> Schema ts
-- -- tail (s :: ss) = ss

-- -- total
-- -- delete : Attr -> Schema ts -> (ts' ** Schema ts')
-- -- delete t SNil      = (_ ** SNil)
-- -- delete t (s :: ss) with (delete t ss)
-- --                    | (_ ** ss') = if (t == s) then (_ ** ss')
-- --                                   else (_ ** s :: ss')


-- -- scDateAddr : Schema (attrDate :: attrAddr :: Nil)
-- -- scDateAddr = getProof $ delete attrName scAgenda

-- -- total
-- -- delete' : (t: Attr) -> Schema ts -> Schema (delete t ts)
-- -- delete' t SNil       = SNil
-- -- delete' t (s :: ss)  = let hypo = (t == s) in
-- --                        if hypo then ?deleteEq
-- --                        else ?deleteNotEq


-- -- -- data Elem : Attr -> Schema _ -> Type where
-- -- --   Here  : Elem t (t :: ss)
-- -- --   There : Elem t ss -> Elem t (t' :: ss)

-- -- -- instance Uninhabited (Elem t SNil) where
-- -- --      uninhabited Here impossible
-- -- --      uninhabited (There p) impossible

-- -- -- isElem : (t: Attr) -> (ss: Schema ts) -> Dec (Elem t ss)
-- -- -- isElem t SNil      = No absurd
-- -- -- isElem t (s :: ss) with (decEq t s)
-- -- --   isElem t (_ :: ss) | (Yes Refl)  = Yes Here
-- -- --   -- isElem t (s :: ss) | (No contra) with (isElem t ss)


-- -- total
-- -- myDelete : (DecEq a) => a -> List a -> List a
-- -- myDelete _ [] = []
-- -- myDelete x (y :: xs) with (decEq x y)
-- --        myDelete x (x :: xs) | Yes Refl = xs
-- --        myDelete x (y :: xs) | No _     = y :: (myDelete x xs)

-- -- total
-- -- elem : Attr -> Schema _ -> Bool
-- -- elem t SNil                           = False
-- -- elem t (x :: ts) with (t == x) | True = True
-- --                                | _    = elem t ts
-- -- -- Set operation
-- -- -- Interserction (S,T) Returns the intersection of S and T
-- -- -- Difference (S,T) Returns the difference of S and T
-- -- -- Subset (S,T) A predicate that test whether the set S is a subset of T

-- -- -- Returns the union of ss and ss'
-- -- total
-- -- union : Schema ts -> Schema ts' -> Schema (ts ++ ts')
-- -- union SNil      ss' = ss'
-- -- union (s :: ss) ss' = s :: (ss `union` ss')

-- -- total
-- -- filter : (p: Attr -> Bool) -> Schema ts -> (ts' ** Schema ts')
-- -- filter p SNil                         = (_ ** SNil)
-- -- filter p (t :: ts) with (filter p ts)
-- --                    | (_ ** tail)      = if p t then (_ ** t :: tail)
-- --                                         else (_ ** tail)

-- -- scDateName : Schema (attrDate :: attrName :: Nil)
-- -- scDateName = getProof $ filter (\t => t `elem` (attrDate :: attrName :: SNil)) scAgenda

-- -- -- Difference s' C s
-- -- data Diff : List Attr -> List Attr -> List Attr -> Type where
-- --   DiffNil : Diff Nil l Nil
-- --   DiffIn  : {auto p: (x `elem` l') = True}  -> Diff l l' res -> Diff (x :: l) l' res
-- --   DiffOut : {auto p: (x `elem` l') = False} -> Diff l l' res -> Diff (x :: l) l' (x :: res)

-- -- -- Diff pridicate test
-- -- -- tDiff : Diff (attrDate :: attrName :: attrAddr :: Nil) (attrAddr :: Nil) (attrDate :: attrName :: Nil)
-- -- -- tDiff = DiffOut $ DiffOut $ DiffIn $ DiffNil

-- -- total
-- -- diff : Schema ts -> Schema ts' -> {auto p: Diff ts ts' res} -> Schema res
-- -- diff SNil      _   {p=DiffNil}    = SNil
-- -- diff (s :: ss) _   {p=DiffNil}    impossible
-- -- diff (s :: ss) ss' {p=DiffIn p'}  = diff ss ss' {p=p'}
-- -- diff (s :: ss) ss' {p=DiffOut p'} = s :: (diff ss ss' {p=p'})

-- -- -- scDateAddr : Schema (attrDate :: attrAddr :: Nil)
-- -- -- scDateAddr = diff scAgenda (attrName :: SNil)


-- -- diffOfNil : (l: List Attr) -> ([] \\ l = [])
-- -- diffOfNil []         = Refl
-- -- diffOfNil (x :: xs)  = ?diffOfNil_XS
-- -- -- diffOfNil : (l: List Attr) -> ([] = [] \\ l)
-- -- -- diffOfNil []         = Refl
-- -- -- diffOfNil (x :: xs)  = diffOfNil xs

-- -- diff1 : (ts: List Attr) -> (ts': List Attr) -> List Attr
-- -- diff1 Nil       ts' = Nil
-- -- diff1 x         Nil = x
-- -- diff1 (t :: ts) ts' with (t `elem` ts') | True = diff1 ts ts'
-- --                                         | _    = t :: (diff1 ts ts')

-- -- diffOfList : (l: List Attr) -> (l': List Attr) -> (l \\ l') = diff1 l l'
-- -- diffOfList ts ts' = ?diffOfList

-- -- diff' : Schema ts -> Schema ts' -> Schema (ts \\ ts')
-- -- diff' SNil      ss'                            ?= SNil
-- -- diff' (s :: ss) ss' with (s `elem` ss') | True ?= diff' ss ss'
-- --                                         | _    ?= s :: diff' ss ss'

-- -- total
-- -- myDiff : (DecEq a) => List a -> List a -> List a
-- -- myDiff Nil       ys = Nil
-- -- myDiff (x :: xs) ys with (x `isElem` ys)
-- --                  | (Yes p) = myDiff xs ys
-- --                  | (No p)  = x :: (myDiff xs ys)

-- -- lemmDiffWithNil : (DecEq a) => (l: List a) -> (l `myDiff` []) = l
-- -- lemmDiffWithNil Nil       = Refl
-- -- lemmDiffWithNil (x :: xs) = lemmDiffWithNil (x :: xs)

-- -- -- diff'' : Schema ts -> Schema ts' -> Schema (ts \\ ts')
-- -- -- diff'' SNil      ss'                            = ?diffNil
-- -- -- diff'' (s :: ss) ss' with (s `elem` ss') | True = ?diffIn  -- diff'' ss ss'
-- -- --                                          | _    = ?diffOut -- s :: diff'' ss ss'
-- -- -- diff'' : Schema ts -> Schema ts' -> Schema (ts `myDiff` ts')
-- -- -- diff'' SNil      ss' = ?diffNil
-- -- -- diff'' (s :: ss) ss' = ?diffNotNil

-- -- -- plusReducesZ : (n: Nat) -> n = plus n Z
-- -- -- plusReducesZ Z     = ?plusredZ_Z
-- -- -- plusReducesZ (S k) = let ih = plusReducesZ k in
-- -- --                      ?plusredZ_S



-- -- -- Returns the intersection of s and s'
-- -- -- Seems impossible to implements, since, this requiers to give a proof that
-- -- -- inter : (s: Schema ts) -> Schema ts' -> filter (\t => t `elem` ts') s
-- -- -- inter : Schema ts -> Schema ts' -> Schema (filter (\t => t `elem` ts') ts)
-- -- -- inter s s'



-- -- -- take : (t: Attribute) -> Schema ts -> {auto p: (t `aelem` ts) = True} -> Attribute
-- -- -- Two schemas are equals if there list of attributes (at type level)
-- -- -- is equal.
-- -- -- take : (t: Attribute) -> (s: Schema ts) -> {p: (elemBy (\t1,t2 => (fst t1) == (fst t2)) t ts) = True} -> Attribute

-- -- -- take t SNil {p=refl}                                     impossible
-- -- -- take t (x :: ts) {p=refl} with (fst t == fst x) | True = t


-- -- -- instance Eq (Schema []) where
-- -- --   SNil == SNil = True

-- -- -- instance (Eq n, Eq (Schema ts)) => Eq (Schema ((n,_) :: ts)) where
-- -- --   _ == _ = False
-- -- --   -- ((n,_) :: ts) == ((n,_) :: ts') = ts == ts'
-- -- --   -- ((n,_) :: _)  == ((x,_) :: _)   = False


-- -- -- instance Eq (Schema []) where
-- -- --   SNil == SNil = True

-- -- -- instance Eq (Schema ts) => Eq (Schema ((n,a) :: ts)) where
-- -- --   ((n,a) :: ts) == ((n,a) :: ts') = ts == ts'
-- -- --   ((n,a) :: ts) == _              = False

-- -- truc : (l: List a) -> {p: isCons l = True} -> a
-- -- truc l {p=p'} = head l {ok=p'}

-- -- ---------- Proofs ----------

-- -- sql.schema.diffNil = proof
-- --   intros
-- --   induction ts'
-- --   compute
-- --   exact SNil
-- --   intros
-- --   compute
-- --   exact ihl__0


-- -- -- sql.schema.diffNil = proof
-- -- --   intros
-- -- --   rewrite sym (diffOfNil ts')
-- -- --   rewrite sym (diffOfNil ts')
-- -- --   exact SNil


-- -- -- sql.schema.diffNil = proof
-- -- --   intros
-- -- --   rewrite sym (diffOfNil ts')
-- -- --   rewrite sym (diffOfNil ts')
-- -- --   exact SNil


-- -- -- sql.schema.diff'_lemma_1 = proof
-- -- --   intros
-- -- --   compute
-- -- --   induction ts'
-- -- --   compute
-- -- --   exact SNil
-- -- --   intros
-- -- --   compute
-- -- --   exact ihl__0


-- -- -- sql.schema.diff'_lemma_1 = proof
-- -- --   intros
-- -- --   induction ts'
-- -- --   exact SNil
-- -- --   intro
-- -- --   intro
-- -- --   intro
-- -- --   refine ihl__0

-- -- sql.schema.diffOfNil_XS = proof
-- --   intros
-- --   induction xs
-- --   refine Refl
-- --   intros
-- --   refine ihl__0
