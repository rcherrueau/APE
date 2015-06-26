module sql

infixr 7 |:

data Attribute : String -> Type -> Type where
  MkAttribute : (s: String) -> (a: Type) -> Attribute s a

attrDate : Attribute "Date" String
attrDate = MkAttribute "Date" String

attrName : Attribute "Name" String
attrName = MkAttribute "Name" String

attrAddr : Attribute "Addr" Integer
attrAddr = MkAttribute "Addr" Integer

data Schema : List Type -> Type where
  SNil  : Schema Nil
  SCons : Attribute s a -> Schema tl -> Schema (Attribute s a :: tl)

schemaAgenda : Schema (Attribute "Date" String
                       :: Attribute "Name" String
                       :: Attribute "Addr" Integer
                       :: Nil)
schemaAgenda = SCons attrDate (SCons attrName (SCons attrAddr SNil))

data Row : Schema s -> Type where
  RNil : Row SNil
  (|:) : {s: String} -> (c: a) -> Row cs -> Row (SCons (MkAttribute s a) cs)

row1 : Row schemaAgenda
row1 = "2015-07-25" |: "Alice" |: 0 |: RNil

row2 : Row schemaAgenda
row2 = "2015-07-26" |: "Alice" |: 0 |: RNil

row3 : Row schemaAgenda
row3 = "2015-07-25" |: "Bob" |: 1 |: RNil

-- Predicate that a schema `s` has an attribute `n`
using (n: String)
  data HasAttribute : Schema _ -> String -> Type -> Type where
    Here  : {s: Schema (Attribute n a :: as)} -> HasAttribute s n a
    There : {t: Attribute _ _} -> {s: Schema as} ->
                HasAttribute s n a -> HasAttribute (t `SCons` s) n a

hasAttributeDate : HasAttribute schemaAgenda "Date" String
hasAttributeDate = Here

hasAttributeName : HasAttribute schemaAgenda "Name" String
hasAttributeName = There Here

hasAttributeAddr : HasAttribute schemaAgenda "Addr" Integer
hasAttributeAddr = There $ There Here

-- Predicate that a schema `s'` is a subset of schema `s`. In this
-- implementation of `Sub`, order of attributes in Schemas matters.
data Sub : Schema _' -> Schema _ -> Type where
  SubNilS2 : Sub SNil s
  SubS1S2  : Sub cs'  cs -> Sub (c `SCons` cs') (c `SCons` cs)
  SubS1S2D : Sub cs'  cs -> Sub cs'             (c `SCons` cs)

snilSubAgenda : SNil `Sub` schemaAgenda
snilSubAgenda = SubNilS2

dateSubAgenda : (attrDate `SCons` SNil) `Sub` schemaAgenda
dateSubAgenda = SubS1S2 SubNilS2

nameSubAgenda : (attrName `SCons` SNil) `Sub` schemaAgenda
nameSubAgenda = SubS1S2D $ SubS1S2 SubNilS2

-- Get the value of a given attribute
get : (n: String) -> Row s -> {auto pf: HasAttribute s n a} -> a
get n (c |: cs) {pf = Here} = c
get n (c |: cs) {pf = There pf'} = get {pf = pf'} n cs

dateR1 : String
dateR1 = get "Date" row1

nameR2 : String
nameR2 = get  "Name" row2

addrR3 : Integer
addrR3 = get "Addr" row3

data Table : Schema s -> Type where
  TNil : Table s
  (::) : (r: Row s) -> Table s -> Table s

tAgenda : Table schemaAgenda
tAgenda = row1
          :: row2
          :: row3
          :: ("2015-07-24" |: "Bob" |: 2 |: RNil)
          :: ("2015-07-24" |: "Bob" |: 3 |: RNil)
          :: TNil

-- Seclection
total
σ : Table s -> (Row s -> Bool) -> Table s
σ (r :: t) p with (p(r)) | True = r :: (σ t p)
                         |_     = σ t p
σ TNil     _                    = TNil

todayAgendaV1 : Table schemaAgenda
todayAgendaV1 =  σ tAgenda (\(c |: cs) => c == "2015-07-25")

-- Generic predicate today
today : {auto pf: HasAttribute s "Date" String} -> Row s -> Bool
today r = get "Date" r == "2015-07-25"

todayAgendaV2 : Table schemaAgenda
todayAgendaV2 = σ tAgenda today

-- Projection
-- π : Table s -> (s': Schema _) -> {auto pf : s' `Sub` s} -> Table s'
