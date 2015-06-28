module sql

infixr 7 |:
infixr 7 |+

-- Attribute of a Database
-- =======================
total
Attribute : Type
Attribute = (String, Type)

-- Examples
attrDate : Attribute
attrDate = ("Date", String)

attrName : Attribute
attrName = ("Name", String)

attrAddr : Attribute
attrAddr = ("Addr", Integer)

-- Schema for a Database
-- =====================
data Schema : List Attribute -> Type where
  SNil : Schema Nil
  (|:) : (a :Attribute) -> Schema as -> Schema (a :: as)

scAgenda : Schema (("Date", String)  ::
                   ("Name", String)  ::
                   ("Addr", Integer) :: Nil)
scAgenda = attrDate |: attrName |: attrAddr |: SNil

scDate : Schema (("Date", String) :: Nil)
scDate = attrDate |: SNil

scDateAddr : Schema (("Date", String) :: ("Addr", Integer) :: Nil)
scDateAddr = attrDate |: attrAddr |: SNil

-- -- Predicate: Ensures, based on attribute's name, that an attribute is
-- -- an element of the schema
-- data Elem : Schema _ -> String -> Type -> Type where
--   Here  : {n: String} -> {s: Schema ((n,a) :: _)} -> Elem s n a
--   There : Elem s n a -> Elem (_ |: s) n a

-- Predicate: Ensures that an attribute is an element of the schema
data Elem : Attribute -> Schema _ -> Type where
  Here  : {t: Attribute} -> {s: Schema (t :: _)} -> Elem t s
  There : Elem t s -> Elem t (_ |: s)

hasDate : ("Date", String) `Elem` scAgenda
hasDate = Here

hasName : ("Name", String) `Elem` scAgenda
hasName = There Here

hasAddr : ("Addr", Integer) `Elem` scAgenda
hasAddr = There $ There Here

-- Predicate: Ensures that all attributes of a schema `s` are elements
-- of a schema `s'`. In other words, `s` is a subset of `s'`.
data Sub : Schema _ -> Schema _ -> Type where
  IsSub  : Sub SNil s'
  SubRec : {auto p: Elem t s'} -> Sub s s' -> Sub (t |: s) s'

nilSub : SNil `Sub` scAgenda
nilSub = IsSub

dateSub : scDate `Sub` scAgenda
dateSub = SubRec IsSub

dateAddrSub : scDateAddr `Sub` scAgenda
dateAddrSub = SubRec $ SubRec IsSub

-- -- Row of a Database
-- -- =================
data Row : Schema _ -> Type where
  RNil : Row SNil
  (|+) : {n: String} -> (t: a) -> Row as -> Row ((n,a) |: as)

row1 : Row scAgenda
row1 = "2015-07-25" |+ "Alice" |+ 0 |+ RNil

row2 : Row scAgenda
row2 = "2015-07-26" |+ "Alice" |+ 0 |+ RNil

row3 : Row scAgenda
row3 = "2015-07-25" |+ "Bob"   |+ 1 |+ RNil

-- Returns the value of an attribute.
total
get : (t: Attribute) -> Row s -> {auto p: t `Elem` s} -> snd (t)
get _ (t |+ ts) {p=Here}     = t
get _ (t |+ ts) {p=There p'} = get _ ts {p=p'}

dateR1 : String
dateR1 = get ("Date", String) row1

nameR2 : String
nameR2 = get ("Name", String) row2

addrR3 : Integer
addrR3 = get ("Addr", Integer) row3

-- Projects, by a schema, on a row. Attributes in schema passed in
-- parameters have to be a subset of attributes in the row's schema.
total
πRow : (s: Schema _) -> Row s' -> {auto p: s `Sub` s'} -> Row s
πRow SNil          r {p=IsSub}                = RNil
πRow ((n,a) |: ts) r {p=SubRec {p=isElem} p'} = let t = get (n,a) r {p=isElem} in
                                                t |+ πRow ts r {p=p'}

dateR : Row scDate
dateR = πRow scDate row1

dateAddrR : Row scDateAddr
dateAddrR = πRow scDateAddr row1

-- Table of a Database
-- ===================
data Table : Schema _ -> Type where
  TNil : Table s
  (::) : (r: Row s) -> Table s -> Table s


tAgenda : Table scAgenda
tAgenda = row1 ::
          row2 ::
          row3 :: TNil

-- Selection
total
σ : (Row s -> Bool) -> Table s -> Table s
σ p TNil                        = TNil
σ p (r :: rs) with (p r) | True = r :: σ p rs
                         | _    = σ p rs
todayAgendaV1 : Table scAgenda
todayAgendaV1 = σ (\r => get ("Date", String) r == "2015-07-25")  tAgenda

-- Generic selection predicate today
today : {auto p: ("Date", String) `Elem` s} -> Row s -> Bool
today r = let date = get ("Date", String) r in
          date == "2015-07-25"

todayAgenda : Table scAgenda
todayAgenda = σ today tAgenda

-- Projection
total
π : (s: Schema _) -> Table s' -> {auto p: s `Sub` s'} -> Table s
π s TNil       = TNil
π s (r :: rs) {p=p'} = πRow s r {p=p'} :: π s rs {p=p'}

tDate : Table scDate
tDate = π scDate tAgenda

tDateAddr : Table scDateAddr
tDateAddr = π scDateAddr tAgenda
