-- Untyped, Call by Value, Lambda Calculus, with de Bruijn notation
-- idris -p contrib ulambda.idr
module ubλ

import Data.SortedSet

infixr 10 |->

-- Identifier: In de Bruijn notation, named variables are replaced by
-- natural numbers, where the number `k` stands for "the variable
-- bound by the `k`-th enclosing λ". λx.x = λ.0 and λx.λy.x (y x) =
-- λ.λ.1 (0 1)
Id : Type
Id = Integer

-- λ-term
-- ======
data Term =                   -- terms:
            Var Id            -- variables
            | Abs Term        -- abstraction
            | App Term Term   -- application

instance Eq Term where
  (Var x)     == (Var y)       = x == y
  (Abs t)     == (Abs t')      = (t == t')
  (App t1 t2) == (App t1' t2') = (t1 == t1') && (t2 == t2')
  (Var _)     == _             = False
  (Abs _)     == _             = False
  (App _ _)   == _             = False

instance Show Term where
  show (Var id)    = show id
  show (Abs t)     = "\\l." ++ show t
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"


-- Evaluation Context
data Ctx = (|->) Id Term

instance Show Ctx where
  show (x |-> s) = "[" ++ (show x) ++ " |-> " ++ (show s) ++ "]"


-- λ-term Examples
λZ : Term  -- Zero
λZ = Abs (Abs (Var 0))

λ1 : Term  -- One
λ1 = Abs (Abs (App (Var 1) (Var 0)))

λ2 : Term  -- One
λ2 = Abs (Abs (App (Var 1) (App (Var 1) (Var 0))))

λ3 : Term
λ3 = Abs (Abs (App (Var 1) (App (Var 1) (App (Var 1) (Var 0)))))

λS : Term  -- Succ
λS = Abs (Abs (Abs (App (Var 1) (App (App (Var 2) (Var 1)) (Var 0)))))

λP: Term   -- Plus
λP = Abs (Abs (Abs (Abs (App (App (Var 3) (Var 1))
                        (App (App (Var 2) (Var 1)) (Var 0))))))

-- Rules
-- =====
-- Shifting: renumbers the indices of the free variables in a term. In
-- [1 |-> s](λ.2) (i.e: [x |-> s](λy.x)) 1 becomes 2, so that, the
-- context in which the substitution is taking place becomes one
-- variable longer. Seeing that, it is clear that we need to increment
-- the indices of the free variables in `s`. And free variables are
-- ones that there indice is upper than the number of binders through.
-- The following function shifts a term t from current position to
-- `d`-place
shift : Integer -> Term -> Term
shift d t = walk 0 t where
  walk : Integer -> Term -> Term
  walk c (Var k) with (k < c) | True = Var k
                              | _    = Var (k + d)
  walk c (Abs t)                     = Abs $ walk (c + 1) t
  walk c (App t1 t2)                 = App (walk c t1) (walk c t2)

-- Substitution: replaces all free occurences of a variable `j` in a
-- term with expression `s`.
substitute : Ctx -> Term -> Term
substitute (j |-> s) (Var k)
  with (k == j) | True           = s
                | _              = (Var k)
substitute (j |-> s) (Abs t)     = Abs $ substitute ((j + 1) |-> (shift 1 s)) t
substitute c         (App t1 t2) = App (substitute c t1) (substitute c t2)
substitute _         t           = t

subsTest : Bool
subsTest =
  substitute (1 |-> (Abs (App (Var 0) (Var 2))))
             (Abs (Var 1)) ==
    Abs (Abs (App (Var 0) (Var 2))) -- &&
  -- -- Avoid substitution on rebinding
  -- substitute ("x" |-> (Var "y")) (Abs "x" (Var "x")) ==
  --   Abs "x" (Var "x") &&
  -- -- α-converts on var capturing
  -- substitute ("x" |-> (Var "z")) (Abs "z" (Var "x")) ==
  --   Abs "zz" (Var "z") &&
  -- substitute ("x" |-> (App (Var "y") (Var "z")))
  --            (Abs "y" (App (Var "x") (Var "y"))) ==
  --   Abs "yy" (App (App (Var "y") (Var "z")) (Var "yy"))

-- Substitution: replaces all free occurences of a variable `v` in a
-- term with expression `s`. The substitution sometimes empl
-- subs : Ctx -> Term -> Term
-- subs (j |-> s) t = walk 0 t where
--   walk c (Var k) with (k == (j+c)) | True = shift c s
--                                    | _    = Var k
--   walk c (Abs t)                          = Abs $ walk (c+1) t1
--   walk c (App t1 t2)                      = App (walk c t1) (walk c t2)
-- subs ctx       t = t

-- substitute : Term -> Term -> Term
-- substitute s t = shift (-1) $ subs (0 |-> shift 1 s) t

-- -- Evaluation
-- -- ==========
-- -- Single Step evaluation function with call by value reduction strategy.
-- -- In call by value, only the outermost redexes are reduced: a redex
-- -- is reduces only when its right hand side has reduces to a value.
-- eval1 : Term -> Maybe Term
-- -- E-AppAbs (computation)
-- eval1 (App (Abs x t) s) with (isVal s)
--                           | True    = Just $ substitute (x |-> s) t
--                           | _       = do s' <- eval1 s
--                                          return $ App (Abs x t) s'
-- -- E-App (congruence)
-- eval1 (App t1        t2) with (isVal t2)
--                           | True    = do t1' <- eval1 t1
--                                          return $ App t1' t2
--                           | _       = do t2' <- eval1 t2
--                                          return $ App t1 t2'
-- eval1 (Abs x t)                     = do t' <- eval1 t
--                                          return $ Abs x t'
-- eval1 _                             = Nothing

-- -- Multi-step evaluation function
-- eval : Term -> Term
-- eval t = case eval1 t of
--            Just t' => eval t'
--            _       => t

-- -- Tests
-- -- =====
-- subsTest: Bool
-- subsTest =
--   substitute ("x" |-> (Abs "z" (App (Var "z") (Var "w"))))
--              (Abs "y" (Var "x")) ==
--     Abs "y" (Abs "z" (App (Var "z") (Var "w"))) &&
--   -- Avoid substitution on rebinding
--   substitute ("x" |-> (Var "y")) (Abs "x" (Var "x")) ==
--     Abs "x" (Var "x") &&
--   -- α-converts on var capturing
--   substitute ("x" |-> (Var "z")) (Abs "z" (Var "x")) ==
--     Abs "zz" (Var "z") &&
--   substitute ("x" |-> (App (Var "y") (Var "z")))
--              (Abs "y" (App (Var "x") (Var "y"))) ==
--     Abs "yy" (App (App (Var "y") (Var "z")) (Var "yy"))

-- evalTest : Bool
-- evalTest =
--   (eval $ App λS λZ) ==  λ1
--   && (eval $ App λS $ App λS λZ) == λ2
--   && (eval $ App λS $ App λS $ App λS λZ) ==  λ3
--   && (eval $ App (App λP $ App λS λZ) (App λS $ App λS λZ)) == λ3
