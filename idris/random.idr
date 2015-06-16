import Effect.Random
import Effects

  -- Identifier
Id : Type
Id = String

-- λ term
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

-- Rename: Terms that differ only in the names of bound variables are
-- interchangeable in all contexts. Do the alpha renaming. replae `old` by
-- `fresh` in the current term.
-- old -> fresh -> term -> Eff Term [RND]
αRename : Id -> Id -> Term -> { [RND] } Eff Term
αRename o f (Var id)    = if (id == o) then
                            return (Var f)
                          else
                            return (Var id)
αRename o f (App t1 t2) = do t1' <- αRename o f t1
                             t2' <- αRename o f t2
                             return $ App t1' t2'
αRename o f (Abs id t)  = if (id == o) then
                            -- The old identifier to αRename is the
                            -- same as the one binded by the
                            -- abstraction. We have, first, to αRename
                            -- the current abstraction by giving her a
                            -- fresh name. And then αRenames the old
                            -- identifier into the fresh new
                            -- abstraction.
                            do i <- rndInt 0 100
                               let f' = id ++ (show i)
                               t' <- αRename id f' t
                               let fabs = (Abs f' t')
                               αRename o f fabs
                          else
                            -- The old identifier to αRename is
                            -- different to the one bind by the
                            -- abstraction: Do the renaming in the
                            -- body of abstraction Without changing
                            -- the identifier of the abstraction
                            do t' <- αRename o f t
                               return (Abs id t')
test : Bool
test =
  -- Var tests
  (runPure $ αRename "y" "w" (Var "y")) ==
    (Var "w") &&
  -- App tests
  (runPure $ αRename "y" "w" (App (Var "y") (Var "z"))) ==
    (App (Var "w") (Var "z")) &&
  (runPure $ αRename "y" "w" (App (Var "z") (Var "y"))) ==
    (App (Var "z") (Var "w")) &&
  (runPure $ αRename "y" "w" (App (Var "y") (Var "y"))) ==
    (App (Var "w") (Var "w")) &&
  (runPure $ αRename "y" "w" (App (Var "y") (App (Var "z") (Var "y")))) ==
    (App (Var "w") (App (Var "z") (Var "w"))) &&
  (runPure $ αRename "y" "w" (App (Var "y") (App (Var "y") (Var "y")))) ==
    (App (Var "w") (App (Var "w") (Var "w"))) &&
  -- Abs tests
  (runPure $ αRename "y" "w" (Abs "x" (Var "x"))) ==
    (Abs "x" (Var "x")) &&
  (runPure $ αRename "y" "w" (Abs "y" (Var "y"))) ==
    (Abs "y23" (Var "y23")) &&
  (runPure $ αRename "y" "w" (Abs "z" (Var "y"))) ==
    (Abs "z" (Var "w"))

-- Random monad tests
test1 : { [RND] } Eff ()
test1 = do srand 12345

test2 : Eff () [RND]
test2 = do srand 12345

test3 : { [RND] } Eff Integer
test3 = do srand 12345
           rndInt 0 100

test4 : { [RND] } Eff Integer
test4 = do srand 42
           i <- rndInt 0 100
           return i

test5 : { [RND] } Eff Integer
test5 = do rndInt 0 100

test5' : Integer
test5' = runPure test4
