data MyNat : Type where
  Z : MyNat
  S : MyNat -> MyNat

total
isZero : MyNat -> Bool
isZero Z     = True
isZero (S k) = False

total
isSucc : MyNat -> Bool
isSucc x = not (isZero x)

total
plus : MyNat -> MyNat -> MyNat
plus Z     k' = k'
plus (S k) k' = S (plus k k')

total
mult : MyNat -> MyNat -> MyNat
mult Z k'     = k'
mult (S k) k' = plus k' (mult k k')

fromIntToMyNat : Integer -> MyNat
fromIntToMyNat 0 = Z
fromIntToMyNat i =
  if (i > 0) then
    S (fromIntToMyNat (i - 1))
  else
    Z

fromMyNatToInt : Nat -> Integer
fromMyNatToInt Z = 0
fromMyNatToInt (S k) = 1 + (fromMyNatToInt k)

total
minus : MyNat -> MyNat -> MyNat
minus Z     _      = Z
minus k     Z      = k
minus (S k) (S k') = minus k k'

total
power : MyNat -> MyNat -> MyNat
power Z _      = Z
power _ Z      = (S Z)
power k (S k') = mult k (power k k')

-- Predicate: As in scala, predicate are types.
data MyLTE: MyNat -> MyNat -> Type where
  MyLTEZero : MyLTE Z k
  MyLTESucc : MyLTE k k'  -> MyLTE (S k) (S k')

-- vt : MyLTE Z (S (S Z))
-- vt = MyLTEZero

total
MyGTE : MyNat -> MyNat -> Type
MyGTE k k' = MyLTE k' k

total
MyLT : MyNat -> MyNat -> Type
MyLT k k' = MyLTE (S k) k'

total
MyGT : MyNat -> MyNat -> Type
MyGT k k' = MyLT k' k

-- Use that predicate:
succNotLTEzero : Not $ MyLTE (S m) Z
succNotLTEzero MyLTEZero impossible

-- If two numbers are ordered, their predecessors are too
fromLteSucc : (S k `MyLTE` S k') -> (k `MyLTE` k')
fromLteSucc (MyLTESucc x) = x

-- isLTE : (k: MyNat) -> (k': MyNat) -> Dec (k `MyLTE` k')
-- isLTE Z     k' = Yes MyLTEZero
-- isLTE (S k) Z  = No succNotLTEzero
-- isLTE (S k) (S k') with (isLTE k k')

data Vect : MyNat -> Type -> Type where
  Nil  : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a


index : (idx: MyNat) -> Vect n a -> {p: idx `MyGTE` n} -> a
index         Z     (hd :: tl) = hd
index {p=p'} (S k) (hd :: tl)  = index k tl {p=fromLteSucc p'}

value : Int
value = index (S (S Z)) (1 :: 1 :: Nil) {p=MyLTESucc (MyLTESucc MyLTEZero)}
