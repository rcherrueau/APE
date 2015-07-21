module MyNat

data MyNat : Type where
  Z : MyNat
  S : MyNat -> MyNat

total
isZero : MyNat -> Bool
isZero Z     = True
isZero (S _) = False

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

-- lemma1 : ((i > 0) = True) -> (i - 1 >= 0) = True
-- lemma1 prf =

-- fromIntToMyNat : (i : Integer) -> (p : (i >= 0) = True) -> MyNat
-- fromIntToMyNat i _ with (isGT i 0)
--   fromIntToMyNat i _ | (No contra) = Z
--   fromIntToMyNat i _ | (Yes prf) = S (fromIntToMyNat (i - 1) (lemma1 prf))

-- fromIntToMyNat 0 prf = Z
-- fromIntToMyNat i prf = S (fromIntToMyNat (i - 1) ?prf)

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

vt : MyLTE Z (S (S Z))
vt = MyLTEZero

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

-------------------------------------------------------------------------------
-- A use case with Vect: In idris library, index on Vect takes a `Fin
-- Nat` as index value. Here we take a `Nat` and we satisfy the `LTE`
-- predicate.
data Vect : MyNat -> Type -> Type where
  Nil  : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

-- Use auto, to automatically proof `pf`.
-- index : (idx: MyNat) -> Vect n a -> {pf: idx `MyLTE` n} -> a
index : (idx: MyNat) -> Vect n a -> {auto pf: idx `MyLTE` n} -> a
index          Z     (hd :: tl)  = hd
index {pf=pf'} (S k) (hd :: tl)  = index k tl {pf=fromLteSucc pf'}

value : Int
-- value = index (S (S Z)) (1 :: 1 :: Nil) {pf=MyLTESucc (MyLTESucc MyLTEZero)}
value = index (S (S Z)) (1 :: 1 :: Nil)

-- Some Fun with Rémi!
total
lemma3 : LTE left right -> LTE left (S right)
lemma3 LTEZero     = LTEZero
lemma3 (LTESucc x) = LTESucc (lemma3 x)

total
lemma2 : (n: Nat) -> (m: Nat) -> plus n (S m) = S (plus n m)
lemma2 Z     m = Refl
lemma2 (S k) m = cong (lemma2 k m) {f=S}

total
lemma1 : (x : Nat) -> LTE x (x + x)
lemma1 Z     = LTEZero
lemma1 (S k) = let inducHypo = (lemma1 k) in
               ?lemma1_rhs

total
toto : (x : Nat) -> { default (lemma1 x) p : LTE x (x + x) } -> Nat
toto x = x

total
titi : Nat
titi = toto (S (S Z))

---------- Proofs ----------

MyNat.lemma1_rhs = proof
  intros
  refine LTESucc
  rewrite sym (lemma2 k k)
  exact (lemma3 inducHypo)
