--http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html
module DependentTypes

-- First Class Types
  --http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html#dependent-types

-- Types are first class language construct. First class citizen
-- meaning that they can be pass as parameter and returned by a
-- function. Here we write a function which computes a type. It
-- calculates the type from a `Bool` which flags whether the type
-- should be a singleton or not.
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
data Vect : Nat -> Type -> Type where
     Nil  : Vect Z a
     (::) : a -> Vect k a -> Vect (S k) a
