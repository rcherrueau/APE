-- Untyped Lambda Calculus
module uλ

import Data.SortedSet
import Debug.Trace

infixr 10 |->

-- Identifier
Id : Type
Id = String

-- λ-term
-- ======
data Term =                   -- terms:
            Var Id            -- variables
            | Abs Id Term     -- abstraction
            | App Term Term   -- application

instance Eq Term where
  (Var x)     == (Var y)       = x == y
  (Abs x t)   == (Abs y t')    = (x == y) && (t == t')
  (App t1 t2) == (App t1' t2') = (t1 == t1') && (t2 == t2')
  (Var _)     == _             = False
  (Abs _ _)   == _             = False
  (App _ _)   == _             = False

instance Show Term where
  show (Var id)    = id
  show (Abs id t)  = "\\" ++ id ++ "." ++ show t
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"


-- Evaluation Context
data Ctx = (|->) Id Term

instance Show Ctx where
  show (x |-> s) = "[" ++ (show x) ++ " |-> " ++ (show s) ++ "]"


-- λ-term Examples
λZ : Term  -- Zero
λZ = Abs "f" (Abs "x" (Var "x"))

λ1 : Term  -- One
λ1 = Abs "f" (Abs "x" (App (Var "f") (Var "x")))

λS : Term  -- Succ
λS = Abs "n" (Abs "f" (Abs "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x")))))

λP: Term   -- Plus
λP = Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (Var "m") (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x")))))))

-- Rules
-- =====
-- Rename: Terms that differ only in the names of bound variables are
-- interchangeable in all contexts. This function does the alpha
-- renaming on *Abstraction terms*. Replaces old identifier (`o`) by
-- fresh identifer (`f`) in the current term.
αRename : Term -> Term
αRename (Abs old t) = let fresh = old ++ old in
                      Abs fresh (αRename' old fresh t)
                      where
  αRename' : Id -> Id -> Term -> Term
  αRename' o f (Var id)    = if (id == o) then Var f else Var id
  αRename' o f (App t1 t2) = let t1' = αRename' o f t1 in
                             let t2' = αRename' o f t2 in
                             App t1' t2'
  αRename' o f (Abs id t)  = if (id == o) || (id == f) then
                              -- Current abstraction rebinds the old
                              -- or fresh identifier. Thus, we have,
                              -- first, to αRename' the current
                              -- abstraction by giving her a fresh
                              -- name. And then αRename's the old
                              -- identifier into the fresh new
                              -- abstraction.
                              let f' = f ++ f in
                              let t' = αRename' id f' t in
                              let fabs = (Abs f' t') in
                              αRename' o f fabs
                            else
                              -- The old identifier to αRename' is
                              -- different to the one bind by the
                              -- abstraction: Do the renaming in the
                              -- body of abstraction Without changing
                              -- the identifier of the abstraction
                              let t' = αRename' o f t in
                              Abs id t'
αRename t           = t

αTest : Bool
αTest =
  -- Var tests
  (αRename $ Abs "y" (Var "y")) == Abs "yy" (Var "yy")
  && (αRename $ Abs "y" (Var "x")) == Abs "yy" (Var "x")
  -- Abs tests
  && (αRename $ Abs "y" (Abs "x" (Var "x"))) == Abs "yy" (Abs "x" (Var "x"))
  && (αRename $ Abs "y" (Abs "x" (Var "y"))) == Abs "yy" (Abs "x" (Var "yy"))
  && (αRename $ Abs "y" (Abs "y" (Var "y"))) ==
       Abs "yy" (Abs "yyyy" (Var "yyyy"))
  && (αRename $ Abs "y" (Abs "yy" (Var "y"))) ==
       Abs "yy" (Abs "yyyy" (Var "yy"))
  && (αRename $ Abs "y" (Abs "yy" (Var "yy"))) ==
       Abs "yy" (Abs "yyyy" (Var "yyyy"))
  -- App Test
  && (αRename $ Abs "y" (App (Var "y") (Var "z"))) ==
       Abs "yy" (App (Var "yy") (Var "z"))
  && (αRename $ Abs "y" (App (Var "z") (Var "y"))) ==
       Abs "yy" (App (Var "z") (Var "yy"))
  && (αRename $ Abs "y" (App (Var "y") (Var "y"))) ==
       Abs "yy" (App (Var "yy") (Var "yy"))
  && (αRename $ Abs "y" (App (Var "y") (App (Var "z") (Var "y")))) ==
       Abs "yy" (App (Var "yy") (App (Var "z") (Var "yy")))
  && (αRename $ Abs "y" (App (Var "y") (App (Var "y") (Var "y")))) ==
       Abs "yy" (App (Var "yy") (App (Var "yy") (Var "yy")))

-- Returns all free identifier of an expression
FV : Term -> SortedSet Id
FV (Var x)     = insert x empty
FV (Abs x t)   = delete x $ FV t
FV (App t1 t2) = let fv1 = FV t1 in
                 let fv2 = FV t2 in
                 fromList $ (SortedSet.toList fv1) ++
                            (SortedSet.toList fv2)

-- Substitution: replaces all free occurences of a variable `v` in a
-- term with expression `s`. The substitution sometimes employes
-- α-conversion to avoid variable capture.
substitute : Ctx -> Term -> Term
-- Idris (as haskell) avoids non-linearity on pattern definition, we
-- have to use the `with` rule
substitute (x |-> s) (Var y) with (x == y)
                     | True                  = s
                     | _                     = (Var y)
substitute (x |-> s) (Abs y t) with (x == y)
-- There is two case to get an good substitution:
-- 1) avoid subsition on identifier rebinds by the
--    identifier of the current abstraction
                     | True                  = (Abs y t)
-- 2) Avoid variable capture: Variable capture phenomenon appears when
--    a free variable in the context (here `s`) becoming bound when
--    the term `s` is substituted into `t`. One solution is α-rename
--    the binder identifier to avoid variable capture.
                     | _                     = if isVarCapture y s then
                                                 let abs = αRename (Abs y t) in
                                                 case abs of
                                                   (Abs y' t') => Abs y' (substitute (x |-> s) t')
                                               else
                                                 Abs y (substitute (x |-> s) t)
                                               where
                                                 isVarCapture : Id -> Term -> Bool
                                                 isVarCapture x s = contains x (FV s)
substitute ctx (App t1 t2)                   = App (substitute ctx t1)
                                                   (substitute ctx t2)
substitute ctx t                             = t

subsTest : Bool
subsTest =
  substitute ("x" |-> (Abs "z" (App (Var "z") (Var "w")))) (Abs "y" (Var "x")) ==
    Abs "y" (Abs "z" (App (Var "z") (Var "w"))) &&
  -- Avoid substitution on rebinding
  substitute ("x" |-> (Var "y")) (Abs "x" (Var "x")) ==
    Abs "x" (Var "x") &&
  -- α-converts on var capturing
  substitute ("x" |-> (Var "z")) (Abs "z" (Var "x")) ==
    Abs "zz" (Var "z") &&
  substitute ("x" |-> (App (Var "y") (Var "z"))) (Abs "y" (App (Var "x") (Var "y"))) ==
    Abs "yy" (App (App (Var "y") (Var "z")) (Var "yy"))


-- Evaluation
-- ==========
-- Single Step evaluation function
eval1 : Term -> Maybe Term
-- E-AppAbs
eval1 (App (Abs x t) s@(Abs _ _)) = Just $ substitute (x |-> s) t -- 1
eval1 (App (Abs x t) s@(Var _))   = Just $ substitute (x |-> s) t -- 2
-- E-App1: reduce right after left, see E-App2
eval1 (App λ@(Abs _ _) t2)        = do t2' <- eval1 t2            -- 3
                                       return $ App λ t2'
eval1 (App (Var x)     t2)        = do t2' <- eval1 t2            -- 4
                                       return $ App (Var x) t2'
-- E-App2: Left most application (reduce left first)
eval1 (App t1          t2)        = do t1' <- eval1 t1            -- 5
                                       return $ App t1' t2
-- Reduction to minimal form
eval1 (Abs x t)                   = do t' <- eval1 t              -- 6
                                       return $ Abs x t'
eval1 _                           = Nothing                       -- 7

-- Multi-step evaluation function
eval : Term -> Term
eval t = case eval1 t of
           Just t' => eval t'
           _       => t

evalTest : Bool
evalTest =
  (eval $ App λS λZ) ==                                -- 1
    Abs "f" (Abs "x" (App (Var "f") (Var "x"))) &&
  (eval $ App λS $ App λS λZ) ==                       -- 2
    Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (Var "x")))) &&
  (eval $ App λS $ App λS $ App λS λZ) ==              -- 3
    Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (App (Var "f") (Var "x"))))) -- &&
  -- (eval $ App λP (App (App λS λZ) (App λS λZ))) ==
  --   (eval $ App λS $ App λS λZ)

-- eval1 $ App λP λ1
