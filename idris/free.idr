-- http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html

import Control.Monad.Free

%default total

data Toy : (b : Type) -> next -> Type where
  Output : b -> next -> Toy b next -- prints a "b" to the console
  Bell : next -> Toy b next        -- rings the computer's bell
  Done : Toy b next                -- end of execution

-- I could write a sample program that I might want to pass to an
-- interpreter:
prg1 : Toy Char (Toy a next)
prg1 = Output 'A' Done

-- ... but unfortunately this doesn't work because every time I want
-- to add a command, it change the type:
prg2 : Toy a (Toy Char (Toy b next))
prg2 = Bell (Output 'A' Done)

-- Fortunately, we can cheat and use the following data type to wrap
-- as many Toys as we want into the dame data type. This is the fixed
-- point of the functor.
data Fix : (f : Type -> Type) -> Type where
  FixPt : f (Fix f) -> Fix f

-- Now our exemples (prg3/4) have the same type
prg3 : Fix (Toy Char)
prg3 = FixPt (Output 'A' (FixPt Done))

prg4 : Fix (Toy Char)
prg4 = FixPt (Bell (FixPt (Output 'A' (FixPt Done))))

-- There's still a problem. This approach only works if you can use
-- the Done constructor to terminate every chain of functors.
-- Unfortunately, programmers don't often have the luxury of writing
-- the entire program from start to finish. We often just want to
-- write subroutines that can be called from within other programs and
-- our Fix trick doesn't let us write a subroutine without terminating
-- the entire program.
--
-- Ok, so let's hack together a quick and dirty fix to work around
-- this problem. Our subroutine finished but we are not ready to call
-- Done, so instead we throw an exception and let whoever calls our
-- subroutine catch it and resume from where we left off:
data FixE : (f : Type -> Type) -> (e : Type) -> Type where
  Throw : e -> FixE f e
  MkFix : f (FixE f e) -> FixE f e -- Same as FixPt

catch : (Functor f) => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (MkFix  k) g = assert_total $ MkFix (map (flip catch g) k)
catch (Throw e1) g = g e1

instance Functor (Toy b) where
  map m (Output x next) = Output x (m next)
  map m (Bell     next) = Bell (m next)
  map m Done            = Done

data IncompleteException = MkIncomplete

-- output 'A'
-- throw IncompleteException
subroutine : FixE (Toy Char) IncompleteException
subroutine = MkFix (Output 'A' (Throw MkIncomplete))

-- try {
--   subroutine
-- } catch (IncompleteException) {
--   bell
--   done
-- }
program : FixE (Toy Char) IncompleteException
program = subroutine `catch` (\_ => MkFix (Bell $ MkFix Done))

-- Free Monads
output : a -> Free (Toy a) ()
output x = Bind (Output x $ Pure ())

bell : Free (Toy a) ()
bell = Bind (Bell $ Pure ())

done : Free (Toy a) r
done = Bind Done

test : Free (Toy a) (List Nat)
test = Bind (Bell $ Pure [1,2,3])

subroutine' : Free (Toy Char) ()
subroutine' = output 'A'

program' : Free (Toy Char) r
program' = do subroutine'
              bell
              done

prg5 : Free (Toy Char) r
prg5 = output 'A' >>= (\a =>
       test       >>= (\_ =>
       ?val))

Schema : Type
Schema = String

-- Keep interface as it
data Term : (s1 : Type) -> (s2 : Type) -> a -> Type where
  -- Frag   : (s : Schema) -> a -> Term s' (s,s') a -- prg6
  Frag   : s -> a -> Term s' (s,s') a
  -- UnFrag : Term (s,s') s a
  -- Query  : (s -> r) -> a -> Term s s a
  -- QueryL : (s -> r) -> a -> Term (s,s') (s,s') a
  -- QueryR : (s' -> r) -> a -> Term (s,s') (s,s') a
  MDone  : Term s1 s2 a

-- prg6 : Term 'a' ("a",'a') a
-- prg6 = Frag "a"

prg6' : Term Char (String, Char) (Term Char (String, Char) a)
prg6' = Frag "a" MDone
-- prg6' = Frag "a" ?test

-- data Term : a -> Type where
--   Frag   : a -> Term a
--   Query  : a -> Term a
--   QueryR : a -> Term a
--   QueryL : a -> Term a

-- instance Functor (Term) where
--   map m (Frag   x) = Frag (m x)
--   map m (Query  x) = Query (m x)
--   map m (QueryR x) = QueryR (m x)
--   map m (QueryL x) = QueryL (m x)


-- data Term : a -> Type where
--   Lit :    Int -> Term Int
--   Succ :   Term Int -> Term Int
--   IsZero : Term Int -> Term Bool
--   If :     Term Bool -> Term a -> Term a -> Term a

-- eval : Term a -> a
-- eval (Lit x) = x
-- eval (Succ x) = (eval x) + 1
-- eval (IsZero x) = (eval x) == 0
-- eval (If x y z) = if (eval x) then (eval y) else (eval z)

-- prg5 : Term Int
-- prg5 = If (IsZero $ Lit 0) (Lit 1) (Lit 2)

-- do crypt
--    frag
--    ql <- lfrag ...
--    qr <- rfrag ...
-- put  join ql qr

-- Local Variables:
-- idris-load-packages: ("idris_free")
-- End:
