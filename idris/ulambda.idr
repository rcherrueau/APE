import Data.SortedMap
import Data.SortedSet

-- Identifier
Id : Type
Id = String

-- λ-term
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


-- λ-term Examples
λZ : Term  -- Zero
λZ = Abs "f" (Abs "x" (Var "x"))

λS : Term  -- Succ
λS = Abs "n" (Abs "f" (Abs "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x")))))

λP: Term   -- Plus
λP = Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (Var "m") (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x")))))))

-- Utils
lookupOrElse : k -> v -> SortedMap k v -> v
lookupOrElse k v sm = case lookup k sm of
                        Just v' => v'
                        _       => v

-- Rename: Terms that differ only in the names of bound variables are
-- interchangeable in all contexts. Do the alpha renaming. replae `old` by
-- `fresh` in the current term.
-- old -> fresh -> term -> Eff Term [RND]
αRename : Id -> Id -> Term -> Term
αRename o f (Var id)    = if (id == o) then Var f else Var id
αRename o f (App t1 t2) = let t1' = αRename o f t1 in
                          let t2' = αRename o f t2 in
                          App t1' t2'
αRename o f (Abs id t)  = if (id == o) then
                            -- The old identifier to αRename is the
                            -- same as the one binded by the
                            -- abstraction. We have, first, to αRename
                            -- the current abstraction by giving her a
                            -- fresh name. And then αRenames the old
                            -- identifier into the fresh new
                            -- abstraction.
                            let f' = id ++ id in
                            let t' = αRename id f' t in
                            let fabs = (Abs f' t') in
                            αRename o f fabs
                          else
                            -- The old identifier to αRename is
                            -- different to the one bind by the
                            -- abstraction: Do the renaming in the
                            -- body of abstraction Without changing
                            -- the identifier of the abstraction
                            let t' = αRename o f t in
                            Abs id t'
-- αTest : Bool
-- αTest =
--   -- Var tests
--   (αRename "y" "w" (Var "y")) ==
--     (Var "w") &&
--   -- App tests
--   (αRename "y" "w" (App (Var "y") (Var "z"))) ==
--     (App (Var "w") (Var "z")) &&
--   (αRename "y" "w" (App (Var "z") (Var "y"))) ==
--     (App (Var "z") (Var "w")) &&
--   (αRename "y" "w" (App (Var "y") (Var "y"))) ==
--     (App (Var "w") (Var "w")) &&
--   (αRename "y" "w" (App (Var "y") (App (Var "z") (Var "y")))) ==
--     (App (Var "w") (App (Var "z") (Var "w"))) &&
--   (αRename "y" "w" (App (Var "y") (App (Var "y") (Var "y")))) ==
--     (App (Var "w") (App (Var "w") (Var "w"))) &&
--   -- Abs tests
--   (αRename "y" "w" (Abs "x" (Var "x"))) ==
--     (Abs "x" (Var "x")) &&
--   (αRename "y" "w" (Abs "y" (Var "y"))) ==
--     (Abs "yy" (Var "yy")) &&
--   (αRename "y" "w" (Abs "z" (Var "y"))) ==
--     (Abs "z" (Var "w"))

-- Returns all free identifier of an expression
getFV : Term -> SortedSet Id
getFV t = let (bd, fv) = getFV' t empty empty in fv
  where
  getFV' : Term -> SortedSet Id -> SortedSet Id ->
                (SortedSet Id, SortedSet Id)
  getFV' (Var id) bds fvs = if contains id bds then
                              (bds, fvs)
                            else
                              (bds, insert id fvs)
  getFV' (Abs id t) bds fvs = getFV' t (insert id bds) fvs
  getFV' (App t1 t2) bds fvs =
    let (bds1, fvs1) = getFV' t1 bds fvs in
    let (bds2, fvs2) = getFV' t2 bds fvs in
      (
        (fromList $ (SortedSet.toList bds1) ++ (SortedSet.toList bds2)),
        (fromList $ (SortedSet.toList fvs1) ++ (SortedSet.toList fvs2)))

-- Evaluation Context
Ctx : Type
Ctx = SortedMap Id Term

-- Substitution: replaces all free occurences of a variable `v` in a
-- term with expression `s`. The substitution sometimes employes
-- α-conversion to avoid variable capture.
substitute : Ctx -> Term -> Term
substitute ctx v@(Var id)  = lookupOrElse id v ctx
substitute ctx (Abs id t) =
  -- There is two case to get an good substitution:
  -- 1) avoid subsition on identifier rebinds by the
  --    identifier of the current abstraction
  let ctx' = delete id ctx in
  -- 2) Avoid variable capture: Variable capture phenomenon appears
  -- when a free variable in term part of `ctx` (call it `s`) becoming
  -- bound when the term `s` is substituted into `t`. One solution
  -- is α-rename the binder identifier to avoid variable capture.
  let fvt = getFV t in -- we first gets all the free variables of t
  -- All fvt will may be substitute by a an `s`. This give us all ours
  -- `s` ss: List Term
  let ss = mapMaybe (\id => lookup id ctx) (SortedSet.toList fvt) in
  -- Then we have to find all free variables for all of our `s`
  let fvss = mapMaybe (\s => let fvs = getFV s in
                             if SortedSet.toList fvs == Nil then Nothing
                             else Just fvs) ss in
  -- Next, we concat all those fvss to get free variable of ptoential s
  let fvs = fromList $ foldl (\l,v => l ++ SortedSet.toList v) [] fvss in
  -- Finally, if the current abstraction will capture a free variable,
  -- we α-rename the abstraction.
  if contains id fvs then
    let f = (id ++ id) in
    let t' = αRename id f t in
    Abs f (substitute ctx' t')
  else
    Abs id (substitute ctx' t)
substitute ctx (App t1 t2) = (App (substitute ctx t1) (substitute ctx t2))

-- subsTest : Bool
-- subsTest =
--   (substitute (insert "x" (Abs "z" (App (Var "z") (Var "w"))) empty) (Abs "y" (Var "x"))) ==
--     (Abs "y" (Abs "z" (App (Var "z") (Var "w")))) &&
--   -- Avoid substitution on rebinding
--   (substitute (insert "x" (Var "y") empty) (Abs "x" (Var "x"))) ==
--     (Abs "x" (Var "x")) &&
--   -- α-converts on var capturing
--   (substitute (insert "x" (Var "z") empty) (Abs "z" (Var "x"))) ==
--     (Abs "zz" (Var "z"))
--   (substitute (insert "x" (App (Var "y") (Var "z")) empty) (Abs "y" (App (Var "x") (Var "y")))) ==
--     (Abs "yy" (App (App (Var "y") (Var "z")) (Var "yy")))


-- Evaluation
-- Single Step evaluation function
eval1 : Ctx -> Term -> Maybe Term
-- E-AppAbs
eval1 ctx (App (Abs id t)    var@(Var _)  ) =
                           Just $ substitute (insert id var ctx) t
eval1 ctx (App (Abs id t)    abs@(Abs _ _)) =
                           Just $ substitute (insert id abs ctx) t
-- E-App1
eval1 ctx (App abs@(Abs _ _) t2           ) =
                           do t2' <- eval1 ctx t2
                              return $ App abs t2'
eval1 ctx (App (Var id)      t2           ) =
                           do t2' <- eval1 ctx t2
                              return $ App (Var id) t2'
-- E-App2
eval1 ctx (App t1            t2           ) =
                           do t1' <- eval1 ctx t1
                              return $ App t1' t2
-- Reduction to minimal form
eval1 ctx (Abs id t)                       =
                           do t' <- eval1 ctx t
                              return (Abs id t')
eval1 _   _                                 =
                           Nothing

-- Multi-step evaluation function
eval : Term -> Term
eval t = case eval1 empty t of
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
  --   eval $ App λS $ App λS λZ
