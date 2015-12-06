-- Evaluator for a simple expression language.
-- http://docs.idris-lang.org/en/latest/effects/simpleeff.html#example-an-expression-calculator

import Effects
import Effect.Exception
import Effect.State
import Effect.Random
import Effect.StdIO

-- First, only considers addition on integers values. In this
-- language, an evaluator always returns an `Integer`, and there are
-- no situations in which it can fail!
namespace v1
  data Expr : Type where
    Val : Integer -> Expr
    Add : Expr -> Expr -> Expr

  eval : Expr -> Integer
  eval (Val   x) = x
  eval (Add l r) = (eval l) + (eval r)

-- Next, add varaibles. Now, evaluator will need to be able to access
-- the values stored in variables, and varaibles may be undefined.
namespace v2
  data Expr : Type where
    Var : String -> v2.Expr
    Val : Integer -> v2.Expr
    Add : v2.Expr -> v2.Expr -> v2.Expr

  -- Changes the type of eval so that it is effectful and suport
  -- exceptions for throwing error, and a state containing a mapping
  -- form variables names to their values
  Env : Type
  Env = List (String, Integer)

  eval : v2.Expr -> Eff Integer [EXCEPTION String, STATE Env]
  eval (Val   x) = return x
  eval (Var   x) = case lookup x !get of
                     Nothing    => raise x
                     (Just val) => return val
  eval (Add l r) = do l' <- eval l
                      r' <- eval r
                      return (l' + r')

  runEval : Env -> v2.Expr -> Maybe Integer
  runEval env expr = run (eval' expr)
    where
    -- Programs which support the EXCEPTION effect can be run in any
    -- context which has some way of throwing errors. This program is
    -- automatically lifted into Maybe Integer:
    -- > the (Maybe Integer) $ run (eval' expr)
    -- > the (Either String Integer) $ run (eval' expr)
    eval' : v2.Expr -> Eff Integer [EXCEPTION String, STATE Env]
    eval' e = do put env
                 eval e

-- Now, we extends our language with Random Number.
namespace v3
  data Expr : Type where
    Rand : Integer -> v3.Expr  -- The interger is the upper bound for
                               -- random number generation
    Var  : String -> v3.Expr
    Val  : Integer -> v3.Expr
    Add  : v3.Expr -> v3.Expr -> v3.Expr

  Env : Type
  Env = List (String, Integer)

  -- To deal with the new expr, we add the `RND` effect that generates
  -- random number (It doesn't matter where `RND` appears in the
  -- list). For test purposes, we might also want to print the random
  -- number which gas been generated:
  eval : v3.Expr -> Eff Integer [STDIO, EXCEPTION String, RND, STATE v3.Env]
  eval (Rand upper) = do val <- rndInt 0 upper
                         putStrLn (show val)
                         return val
  eval (Val      x) = return x
  eval (Var      x) = case lookup x !get of
                         Nothing    => raise x
                         (Just val) => return val
  eval (Add    l r) = do l' <- eval l
                         r' <- eval r
                         return (l' + r')

  -- We have to Put the result into IO since `v3.eval` performs I/O
  -- actions. The EXCEPTION effect is automatically lifted into IO
  runEval : v3.Env -> v3.Expr -> IO Integer
  runEval env expr = run (eval' expr)
    where
    eval' : v3.Expr -> Eff Integer [STDIO, EXCEPTION String, RND, STATE v3.Env]
    eval' e = do put env
                 v3.eval e

-- Once effect lists get longer, it can be a good idea instead to
-- encapsulate sets of effects in a type synonym. This is achieved as
--  simply by defining a function which computes a type.
namespace v4
  Env : Type
  Env = List (String, Integer)

  EvalEff : Type -> Type
  EvalEff t = Eff t [STDIO, EXCEPTION String, RND, STATE v4.Env]

  data Expr : Type where
    Rand : Integer -> v4.Expr
    Var  : String -> v4.Expr
    Val  : Integer -> v4.Expr
    Add  : v4.Expr -> v4.Expr -> v4.Expr

  eval : v4.Expr -> EvalEff Integer
  eval (Rand upper) = do val <- rndInt 0 upper
                         putStrLn (show val)
                         return val
  eval (Val      x) = return x
  eval (Var      x) = case lookup x !get of
                         Nothing    => raise x
                         (Just val) => return val
  eval (Add    l r) = do l' <- eval l
                         r' <- eval r
                         return (l' + r')

  runEval : v4.Env -> v4.Expr -> IO Integer
  runEval env expr = run (eval' expr)
    where
    eval' : v4.Expr -> EvalEff Integer
    eval' e = do put env
                 v4.eval e

main : IO ()
main = do putStr "Number: "
          x <- getLine
          y <- v4.runEval [("foo", cast x)] $
               Add (Add (Var "foo") (Val 42)) (Rand 100)
          putStrLn $ "Answer: " ++ (show y)

-- Local Variables:
-- idris-load-packages: ("effects")
-- End:
