module MyList

data MyList : (a : Type) -> Type where
  Nil  : MyList a
  (::) : a -> MyList a -> MyList a

l1 : MyList Nat
l1 = Nil

l2 : MyList Nat
l2 = Z :: Z :: Nil

-- test
append : (a : Type) -> MyList a -> MyList a -> MyList a
append a Nil       right = right
append a (x :: xs) right = x :: (append a xs right)

l3 : MyList Nat
l3 = append Nat l1 l2


namespace MyVect
  data Vect : Nat -> (a : Type) -> Type where
    Nil  : Vect Z a
    (::) : {n : Nat} -> a -> Vect n a -> Vect (S n) a

  l3: Vect 1 Nat
  l3 = 1 :: Nil

  append : {a : Type} -> {m, n : Nat} ->
           Vect m a -> Vect n a -> Vect (plus m n) a
  append Nil       ys = ys
  append (x :: xs) ys = x :: (append xs ys)
