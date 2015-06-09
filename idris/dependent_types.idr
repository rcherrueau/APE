--http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html
module DependentTypes

-- First Class Types
--http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html#dependent-types

-- Types are first class language construct. First class citizen
-- meaning that they can be pass as parameter, returned by a function
-- and assigned to a variable. Here we write a function which computes
-- a type. It calculates the type from a `Bool` which flags whether
-- the type should be a singleton or not.
isSingleton : Bool -> Type
isSingleton True = Nat
isSingleton False = List Nat

-- We can use this function to calculate a type anywhere that a type
-- can be found. For example, to calculate a return type.
mkSingle : (x : Bool) -> isSingleton x
mkSingle True = 0
mkSingle False = []

-- Function `isSingleton` can also be used to have varying input
-- types. In the following, sum takes two parameter. Function
-- `isSingleton` calculates the type of the second one. It could be
-- either a Nat or a List of Nat.
sum : (single : Bool) -> isSingleton single -> Nat
sum True  x = x
sum False [] = 0
sum False (x :: xs) = x + sum False xs

-- Vectors (list with length)
-- http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html#vectors
-- `Vect` declares a family of types (return type is `Type`, the type
-- of types) and is parameterized by a natural and a type. We said
-- that "Vect declares a family of types" meaning that `Vect` gets
-- `(Nat x Type)` possible types (if we don't consider constructor).
-- As in Haskell, data constructor (i.e: `Nil`, `(::)`) constructs a
-- value of type `Vect`. But, unlike Haskell, we have to declare the
-- type of the constructor. Obviously, the checker cannot infer that
-- `b :: a :: Nil` should increment the value of `Nat` twice in the
-- type.
-- Note: Here Nat stands for a value of Nat.
data Vect : Nat -> Type -> Type where
  Nil  : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

v1 : Vect Z Int
v1 = Nil

v2 : Vect (S (S Z)) Char
v2 = 'b' :: 'a' :: Nil


-- The Finite Sets
-- http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html#the-finite-sets
-- Note: Here Nat stands for a value of Nat.
data Fin : Nat -> Type where
  FZ : Fin (S k)
  FS : Fin k -> Fin (S k)


-- We declare F4, the type of the finite set that contains four
-- elements.
F4 : Type
F4 = Fin 4

-- `first` value is one inhabitant of `F4`. It represents the first
-- element of the set that contains four elements.
first : F4
first = FZ

-- `fourth` value, as `first`, is one inhabitant of `F4`, but `fourth`
-- represents the fourth element of the set that contains four
-- elements.
fourth : F4
fourth = FS (FS (FS FZ))

-- Returns the element at index `Fin n`.
index : Fin n -> Vect n a -> a
index FZ     (x :: xs) = x
index (FS k) (x :: xs) = index k xs

vFirst : Char
vFirst = index first ('a' :: 'b' :: 'c' :: 'd' :: Nil) -- 'a'

vFourth : Char
vFourth = index fourth  ('a' :: 'b' :: 'c' :: 'd' :: Nil) -- 'd'


-- fifth : Fin 5
-- fifth = FS fourth
-- -- Doesn't type check, `fifth` is type `Fin 5`, thus `n` in `Fin n` of
-- -- `Fin n -> Vect n a -> a` is  5, whereas `n` is 4 in `Vect n a`.
-- vFifth : Char
-- vFifth = index fifth ('a' :: 'b' :: 'c' :: 'd' :: Nil)

-- Implicit
-- http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html#implicit-arguments
-- If we look closer to the definition of `index`, it takes two
-- arguments, an element of the finit set of n `Fin n`, and a vector
-- with n elements `Vect n a`. But there is also two names, `n` and
-- `a`, which are not declared explicitly. These are *implicit*
-- arguments to `index`. We could also write the type of `index` as
index2 : {b: Type} -> {m: Nat} -> Fin m -> Vect m b -> b
-- index2 fin vect = index {a=b} {n=m} fin vect -- Doesn't type checks,
--                                              -- see "using notation"
index2 fin vect = index fin vect

-- Value of a/b and n/m could be inferred from the types of the`Fin n` and
-- `Vect n a`. But they can explicitly be given in applications:
v2First : Char
v2First = index2 {b=Char} {m=4} FZ ('a' :: 'b' :: 'c' :: 'd' :: Nil) -- 'a'

-- An instance of Elem x xs states that x is an element of xs
-- Note: Here `a` and `Vect n a` stand for a value of type `a` and a
-- value of type `Vect n a`. Remember that data declares a family of
-- types where type a parameterized by values. A type for Elem should
-- be something like `Elem 5 (1 :: 2 :: Nil)`. `1 :: 1 :: Nil` is a
-- value of `Vect n a` thus `a` is fix to `Int`. Henceforth, the type
-- definition of Elem requires a `Int` value for the first argument
-- `a`. In our example we choose `5`.
data Elem : a -> Vect n a -> Type where
  -- Here construct an instance of Elem, *enforcing that the predicate
  -- Elem is true*. The obvious case where the predicate is true (and
  -- where wa can construct an Elem) is when the given value is the
  -- head of Vect:
  Here  : {hd:a} -> {tl:Vect n a} -> Elem hd (hd :: tl)
  -- If the given value in the type, is not the head of the vect, then
  -- we have to try to construct Elem in the tail of the vector `Elem
  -- x tl`. If we can construct a `Elem x tl`, thus we can construct
  -- the predicate Elem for our currect vect `Elem x (hd :: tl)`.
  There : {x,hd:a} -> {tl:Vect n a} -> Elem x tl -> Elem x (hd :: tl)

isIn : Elem 1 (1 :: 2 :: Nil)
isIn = Here

-- Doesn't type check, cannot unify 5 and 1
-- isNotIn : Elem 5 (1 :: 2 :: Nil)
-- isNotIn = Here

isInTail : Elem 2 (1 :: 2 :: Nil)
isInTail = There Here

-- In Elem definition, using implicit makes definition difficult to
-- read. To avoid this problem, a `using` block exists.
using (x: a, y: a, xs: Vect n a)
  data Elem2 : a -> Vect n a -> Type where
    Here2  : Elem2 x (x :: xs)
    There2 : Elem2 x xs -> Elem2 x (y :: xs)

elemA: Elem2 'a' v2
elemA = There2 Here2 -- We construct the proof that 'a' is an element
                     -- of `v2`

mutual
  even : Nat -> Bool
  even Z     = True
  even (S k) = odd k

  odd : Nat -> Bool
  odd Z     = False
  odd (S k) = even k

-- Pair
t3Sugared : (Integer, Integer, Integer)
t3Sugared = (1, 1, 1)

-- Pair in libs/prelude/Prelude and MkPaire in libs/prelude/Builtins
t3Desugared : Pair Integer (Pair Integer Integer)
t3Desugared = MkPair 1 (MkPair 1 1)

data MyPair : Type -> Type -> Type where
  MkMyPair : a -> b -> MyPair a b


data Nested : Pair a (Pair b c) -> Type where
  -- Nest : x -> (y: Pair _ _) -> Nested (MkPair x y)
  Nest : Nested x
