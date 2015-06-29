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

total
head : Schema ts -> {auto ok: isCons ts = True} -> Attr
head SNil      {ok=Refl} impossible
head (t :: ts) {ok=p}    = t

total
tail : Schema (t :: ts) -> Schema ts
tail (t :: ts) = ts

-- Utils, auxiliary functions on list
-- data IsIn : Attr -> List Attr -> Type where
--   InHead : {t: Attr} -> IsIn t (t :: ts)
--   InTail : IsIn t ts -> IsIn t (t' :: ts)

-- isIn : (t: Attr) -> (ts: List Attr) -> {auto p: t `IsIn` ts} -> Bool
-- isIn t Nil {p=InHead} impossible
-- isIn _ _   {p=p'}     = True

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
filter' : (p: Attr -> Bool) -> Schema ts -> (ts' ** Schema ts')
filter' p SNil                         = (_ ** SNil)
filter' p (t :: ts) with (filter' p ts)
                    | (_ ** tail)      = if p t then (_ ** t :: tail)
                                         else (_ ** tail)

total
elem : Attr -> Schema _ -> Bool
elem t SNil                           = False
elem t (x :: ts) with (t == x) | True = True
                               | _    = elem t ts

-- Difference s' C s
-- Todo
diff : Schema ts -> Schema ts' -> Schema (ts \\ ts')
diff SNil ss' = SNil
diff (s :: ss) ss' with (s `elem` ss') | True = diff ss ss'
                                       | _    = diff s :: ss ss'




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
