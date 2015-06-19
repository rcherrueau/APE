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

-- Predicate that a schema `s` has an attribute `n`
using (n: String)
  data HasAttribute : Schema as -> String -> Type -> Type where
    Here  : {s: Schema (Attribute n a :: as)} -> HasAttribute s n a
    There : {t: Attribute _ _} -> {s: Schema as} ->
                HasAttribute s n a -> HasAttribute (t `SCons` s) n a

-- using (n String)
--   data GetAttribute : (s: Schema _) -> Row s -> {auto pf: HasAttribute s n a} -> a -> Type where
--     GHere  : GetAttribute (Schema (Attribute n _ :: s)) (c |: cs) c
--   -- There :

hasAttributeDate : HasAttribute schemaAgenda "Date" String
hasAttributeDate = Here

hasAttributeName : HasAttribute schemaAgenda "Name" String
hasAttributeName = There Here

hasAttributeAddr : HasAttribute schemaAgenda "Addr" Integer
hasAttributeAddr = There $ There Here

get : {s: Schema as} -> (n: String) -> Row s -> {pf: HasAttribute s n a} -> a
get {pf=pf'} {s=(MkAttribute n a) `SCons` _} n (c |: cs) = c

data Table : Schema s -> Type where
  TNil : Table s
  (::) : (r: Row s) -> Table s -> Table s

tAgenda : Table schemaAgenda
tAgenda = row1
          :: ("2015-07-24" |: "Bob" |: 2 |: RNil)
          :: ("2015-07-24" |: "Bob" |: 3 |: RNil)
          :: TNil

-- Seclection
total
σ : Table s -> (Row s -> Bool) -> Table s
σ (r :: t) p with (p(r)) | True = r :: (σ t p)
                         |_     = σ t p
σ TNil     _                    = TNil

todayAgenda : Table schemaAgenda
todayAgenda =  σ tAgenda (\(c |: cs) => c == "2015-07-25")