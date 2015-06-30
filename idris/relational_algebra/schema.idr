module sql.schema

record Attr where
  constructor MkAttr
  name : String
  type : Type

instance Eq Attr where
  attr == attr' = (name attr) == (name attr')

attrDate : Attr
attrDate = MkAttr "Date" String

attrName : Attr
attrName = MkAttr "Name" String

attrAddr : Attr
attrAddr = MkAttr "Addr" Integer

data Schema : List Attr -> Type where
  SNil : Schema []
  (::) : (t: Attr) -> Schema ts -> Schema (t :: ts)

scAgenda : Schema (attrDate :: attrName :: attrAddr :: Nil)
scAgenda = attrDate :: attrName :: attrAddr :: SNil

total
head : Schema ts -> {auto ok: isCons ts = True} -> Attr
head SNil      {ok=Refl} impossible
head (s :: ss) {ok=p}    = s

total
tail : Schema (t :: ts) -> Schema ts
tail (s :: ss) = ss

total
elem : Attr -> Schema _ -> Bool
elem t SNil                           = False
elem t (x :: ts) with (t == x) | True = True
                               | _    = elem t ts
-- Set operation
-- Interserction (S,T) Returns the intersection of S and T
-- Difference (S,T) Returns the difference of S and T
-- Subset (S,T) A predicate that test whether the set S is a subset of T

-- Returns the union of ss and ss'
total
union : Schema ts -> Schema ts' -> Schema (ts ++ ts')
union SNil      ss' = ss'
union (s :: ss) ss' = s :: (ss `union` ss')

total
filter : (p: Attr -> Bool) -> Schema ts -> (ts' ** Schema ts')
filter p SNil                         = (_ ** SNil)
filter p (t :: ts) with (filter p ts)
                   | (_ ** tail)      = if p t then (_ ** t :: tail)
                                        else (_ ** tail)

scDateName : Schema (attrDate :: attrName :: Nil)
scDateName = getProof $ filter (\t => t `elem` (attrDate :: attrName :: SNil)) scAgenda

-- Difference s' C s
data Diff : List Attr -> List Attr -> List Attr -> Type where
  DiffNil : Diff Nil l Nil
  DiffIn  : {auto p: (x `elem` l') = True}  -> Diff l l' res -> Diff (x :: l) l' res
  DiffOut : {auto p: (x `elem` l') = False} -> Diff l l' res -> Diff (x :: l) l' (x :: res)

-- Diff pridicate test
-- tDiff : Diff (attrDate :: attrName :: attrAddr :: Nil) (attrAddr :: Nil) (attrDate :: attrName :: Nil)
-- tDiff = DiffOut $ DiffOut $ DiffIn $ DiffNil

total
diff : Schema ts -> Schema ts' -> {auto p: Diff ts ts' res} -> Schema res
diff SNil      _   {p=DiffNil}    = SNil
diff (s :: ss) _   {p=DiffNil}    impossible
diff (s :: ss) ss' {p=DiffIn p'}  = diff ss ss' {p=p'}
diff (s :: ss) ss' {p=DiffOut p'} = s :: (diff ss ss' {p=p'})

scDateAddr : Schema (attrDate :: attrAddr :: Nil)
scDateAddr = diff scAgenda (attrName :: SNil)


diffOfNil : (l: List Attr) -> [] \\ l = []
diffOfNil []         = Refl
diffOfNil (x :: xs)  = ?diffOfNil_XS

diff' : Schema ts -> Schema ts' -> Schema (ts \\ ts')
diff' SNil      ss'                            ?= SNil
diff' (s :: ss) ss' with (s `elem` ss') | True ?= diff' ss ss'
                                        | _    ?= s :: diff' ss ss'

diff'' : Schema ts -> Schema ts' -> Schema (ts \\ ts')
diff'' SNil      ss'                            = ?diffNil
diff'' (s :: ss) ss' with (s `elem` ss') | True = ?diffIn  -- diff'' ss ss'
                                         | _    = ?diffOut -- s :: diff'' ss ss'

-- plusReducesZ : (n: Nat) -> n = plus n Z
-- plusReducesZ Z     = ?plusredZ_Z
-- plusReducesZ (S k) = let ih = plusReducesZ k in
--                      ?plusredZ_S



-- Returns the intersection of s and s'
-- Seems impossible to implements, since, this requiers to give a proof that
-- inter : (s: Schema ts) -> Schema ts' -> filter (\t => t `elem` ts') s
-- inter : Schema ts -> Schema ts' -> Schema (filter (\t => t `elem` ts') ts)
-- inter s s'



-- take : (t: Attribute) -> Schema ts -> {auto p: (t `aelem` ts) = True} -> Attribute
-- Two schemas are equals if there list of attributes (at type level)
-- is equal.
-- take : (t: Attribute) -> (s: Schema ts) -> {p: (elemBy (\t1,t2 => (fst t1) == (fst t2)) t ts) = True} -> Attribute
-- take t SNil {p=refl}                                     impossible
-- take t (x :: ts) {p=refl} with (fst t == fst x) | True = t


-- instance Eq (Schema []) where
--   SNil == SNil = True

-- instance (Eq n, Eq (Schema ts)) => Eq (Schema ((n,_) :: ts)) where
--   _ == _ = False
--   -- ((n,_) :: ts) == ((n,_) :: ts') = ts == ts'
--   -- ((n,_) :: _)  == ((x,_) :: _)   = False


-- instance Eq (Schema []) where
--   SNil == SNil = True

-- instance Eq (Schema ts) => Eq (Schema ((n,a) :: ts)) where
--   ((n,a) :: ts) == ((n,a) :: ts') = ts == ts'
--   ((n,a) :: ts) == _              = False

---------- Proofs ----------

sql.schema.diff'_lemma_1 = proof
  intros
  induction ts'
  exact SNil
  intro
  intro
  intro
  refine ihl__0


sql.schema.diffNil = proof
  intros
  induction ts'
  exact SNil
  intros
  refine ihl__0


sql.schema.diffOfNil_XS = proof
  intros
  induction xs
  refine Refl
  intros
  refine ihl__0
