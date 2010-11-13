{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies #-}

module Intel.Cnc.EasyEmit where

import Control.Monad 

import Intel.Cnc.Spec.Util hiding (app)
import Text.PrettyPrint.HughesPJ
import StringTable.Atom

import Data.Data
import Data.List
import GHC.Exts -- For IsString

import qualified  Control.Monad.State.Strict as S 
import Debug.Trace

import qualified Prelude as P
import Prelude hiding ((&&), (||), (==), (/=), not, Eq
		       , Ord, (<), (<=), (>), (>=), max, min -- , compare
		      )

data Type =
   TInt
 | TFloat
 -- An abstract type not intpreted by CnC:
 | TSym Atom
 | TPtr Type
 | TDense Type -- Density annotations.
 | TTuple [Type]
 deriving (P.Eq, P.Ord, Show,Data,Typeable)

dType ty = case ty of 
  TInt   -> t "int"
  TFloat -> t "float"
  TSym s -> textAtom s
  TPtr ty -> dType ty <> t "*"

  -- This doesn't affect the C-type, any influence has already taken place.
  TDense ty -> dType ty 

  -- Here is the convention for representing tuples in C++.
  --TTuple [a,b]   -> t "Pair"   <> angles (hcat$ punctuate commspc (map dType [a,b]))
  --TTuple [a,b,c] -> t "Triple" <> angles (hcat$ punctuate commspc (map dType [a,b,c]))
  TTuple ls -> t "cnctup::tuple" <> angles (hcat$ punctuate commspc$ map dType ls)
  --TTuple ls -> error$ "CppOld codegen: Tuple types of length "++ show (length ls) ++" not standardized yet!"



----------------------------------------------------------------------------------------------------
-- Overloaded booleans and predicates from Lennart Augustsson:
----------------------------------------------------------------------------------------------------

-- | Generalization of the 'Bool' type.  Used by the generalized 'Eq' and 'Ord'.
class Boolean bool where
    (&&)  :: bool -> bool -> bool   -- ^Logical conjunction.
    (||)  :: bool -> bool -> bool   -- ^Logical disjunction.
    not   :: bool -> bool           -- ^Locical negation.
    false :: bool                   -- ^Truth.
    true  :: bool                   -- ^Falsity.
    fromBool :: Bool -> bool        -- ^Convert a 'Bool' to the generalized Boolean type.
    fromBool b = if b then true else false

-- Why not just make this part of the above class?
-- | Generalization of the @if@ construct.
--class (Boolean bool) => Conditional bool a where -- orig
--class (Boolean bool) => Conditional bool where
--    conditional :: bool -> a -> a -> a -- ^Pick the first argument if the 'Boolean' value is true, otherwise the second argument.


--class (Boolean bool) => Eq a bool {- x | a -> bool -} where
class (Boolean bool) => Eq a bool | a -> bool where
    (==) :: a -> a -> bool
    (/=) :: a -> a -> bool

    x /= y  =  not (x == y)
    x == y  =  not (x /= y)


--class (Conditional bool Ordering, Eq a bool) => Ord a bool | a -> bool where
--class (Conditional bool, Eq a bool) => Ord a bool | a -> bool where
class (Boolean bool, Eq a bool) => Ord a bool | a -> bool where
    (<), (<=), (>), (>=) :: a -> a -> bool
    max, min             :: a -> a -> a

-- Need to define precedence because these new operations really have nothing to do with the originals:
infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||


----------------------------------------------------------------------------------------------------
-- Monad plus HOAS for C++
----------------------------------------------------------------------------------------------------

-- A monad for generating syntax:
-- The state consists of an accumulator for lines of syntax, plus a counter for temporary variables.
type EasyEmit a = S.State ([Doc], Int) a

runEasyEmit :: EasyEmit () -> Doc
runEasyEmit m = vcat (reverse ls)
 where 
  (a,(ls,_)) = S.runState m ([], 0)

-- This runs a subcomputation only for value -- discards its emission side effects.
forValueOnly :: EasyEmit a -> EasyEmit a
forValueOnly m =
 do (ls,c) <- S.get 
    let (val,(_,c2)) = S.runState m ([], c)
    S.put (ls,c2)
    return val

instance P.Eq Doc where
  -- Would be nice to memoize this!
  a == b  =  render a P.== render b

data Syntax = Syn Doc
  deriving (Show, P.Eq)

deSyn (Syn s) = s

-- Also, overloading the string constants themselves is nice:
instance IsString Syntax where
    fromString s = Syn (text s)

-- instance IsString Doc where
--     fromString s = (text s)

instance Num Syntax where 
  (Syn a) + (Syn b) = Syn (parens $ a <> " + " <> b )
  (Syn a) * (Syn b) = Syn (parens $ a <> " * " <> b )
  (Syn a) - (Syn b) = Syn (parens $ a <> " - " <> b )
  abs    (Syn a) = Syn ("abs" <> parens a )
  negate (Syn a) = Syn (parens ("-" <> parens a))
  fromInteger n = Syn (text $ show n)
  -- Could implement this I suppose...
  signum _ = "__ERROR__"

instance Fractional Syntax where 
  (Syn a) / (Syn b) = Syn (parens $ a <> " / " <> b )
  recip (Syn a) = Syn (parens $ "1 / " <> a )
  fromRational rat = Syn ( text$ show rat )

instance Boolean Syntax where
    (Syn a) && (Syn b) = Syn (parens $ a <> " && " <> b )
    (Syn a) || (Syn b) = Syn (parens $ a <> " || " <> b )
    not (Syn a) = Syn ("!" <> parens a )
    false = Syn "false"
    true  = Syn "true"

instance Ord Syntax Syntax where
    (Syn a) < (Syn b) = Syn (parens $ a <> " < " <> b )
    (Syn a) > (Syn b) = Syn (parens $ a <> " > " <> b )
    (Syn a) <= (Syn b) = Syn (parens $ a <> " <= " <> b )
    (Syn a) >= (Syn b) = Syn (parens $ a <> " >= " <> b )
    max (Syn a) (Syn b) = Syn ("max" <> commasep [a,b] )
    min (Syn a) (Syn b) = Syn ("min" <> commasep [a,b] )

--    conditional (Syn a) b c = Syn (parens $ a <> " ? " <> b <> ":" <> c )

instance Eq Syntax Syntax where
    (Syn a) == (Syn b) = Syn (parens $ a <> " == " <> b )   

-- Adds implicit newline at the end:
addChunk (Syn doc) = 
  do (ls,c) <- S.get
     S.put (doc : ls, c)

-- Adds the semi-colon at the end also:
addLine (Syn doc) = addChunk (Syn$ doc <> semi)

--------------------------------------------------------------------------------

(Syn a) +++ (Syn b) = Syn (a <> b)

-- Comma separate either horizontally or vertically
commasep ls = parens$ fcat$ intersperse (text", ") ls


-- Declaring variables
----------------------

-- With names:
var :: Type -> Syntax -> EasyEmit Syntax
var ty (Syn name) = do addLine (Syn (dType ty <+> name ))
		       return (Syn name)
-- Without names:
tmpvar :: Type -> EasyEmit Syntax
tmpvar ty = do Syn name <- gensym "tmp"
	       addLine (Syn (dType ty <+> name ))
	       return (Syn name)

gensym root = 
   do (ls,cnt) <- S.get
      S.put (ls,cnt+1)
      return$ Syn$ root <> int cnt
	   

-- With initialization expression:
varinit :: Type -> Syntax -> Syntax -> EasyEmit Syntax
varinit ty (Syn name) (Syn rhs) = 
   do addLine (Syn (dType ty <+> name <+> "=" <+> rhs))
      return (Syn name)


------------------------------------------------------------
-- C++ Statements 
------------------------------------------------------------

-- Variable Assignment:
set :: Syntax -> Syntax -> EasyEmit ()
set (Syn x) (Syn v) = addLine$ Syn$ x <+> "=" <+> v 

-- Function application (command context):
-- --app :: String -> String -> IO String
--app :: Syntax -> [Syntax] -> IO Syntax
--app (Syn f) ls = return (f <> parens (commasep$ map deSyn ls))

app :: ([Syntax] -> Syntax) -> [Syntax] -> EasyEmit ()
app fn ls = addLine$ fn ls


-- A shorthand that looks C-ish:
(Syn x) += (Syn n) = addLine$ Syn$ x <+> "+=" <+> n
(Syn x) -= (Syn n) = addLine$ Syn$ x <+> "-=" <+> n

-- Comments:
comm :: Doc -> EasyEmit ()
comm x  = addChunk$ Syn$ txt
 where 
   txt = text$ init$ unlines lns -- init strips the last newline
   lns = map fn $ lines (render x)
   fn "" = ""
   fn other = "// " ++ other

if_ (Syn a) m1 m2 = 
  do let bod1 = runEasyEmit m1
	 bod2 = runEasyEmit m2
     addChunk$ Syn$ hangbraces ("if " <> parens a) indent bod1
     addChunk$ Syn$ "else"
     addChunk$ Syn$ hangbraces (empty) indent bod2

ret (Syn x) = addLine$ Syn$ "return " <> x

------------------------------------------------------------
-- C++ Definitions & Declarations
------------------------------------------------------------

-- The funDef function creates a function definition as well as
-- returning a Haskell function that can be used to construct
-- applications of that function.
class FunDefable args where
  funDef :: Type -> Syntax -> [Type] -> (args -> EasyEmit ()) -> EasyEmit ([Syntax] -> Syntax)

instance FunDefable Syntax                        where funDef r n ts fn = funDefShared r n ts fn (\ [a] -> a)
instance FunDefable (Syntax,Syntax)               where funDef r n ts fn = funDefShared r n ts fn (\ [a,b] -> (a,b))
instance FunDefable (Syntax,Syntax,Syntax)        where funDef r n ts fn = funDefShared r n ts fn (\ [a,b,c] -> (a,b,c))
instance FunDefable (Syntax,Syntax,Syntax,Syntax) where funDef r n ts fn = funDefShared r n ts fn (\ [a,b,c,d] -> (a,b,c,d))


funDefShared retty (Syn name) tyls fn formTup = 
    do (ls,c) <- S.get 
       --args <- mapM (forValueOnly . tmpvar) tyls -- Generate temp names only (emit nothing).
       args <- sequence$ take (length tyls) (repeat$ gensym "arg") -- Generate temp names only (emit nothing).
       let body = runEasyEmit (fn$ formTup args)
	   formals = map (\ (t,a) -> dType t <+> deSyn a) (zip tyls args)
       addChunk$ Syn$ hangbraces (dType retty <+> name <> commasep formals) indent body
       addChunk$ "\n"
       return (\ args -> Syn$ name <> (commasep (map deSyn args)))


-- Common case: for loop over a range with integer index:
------------------------------------------------------------
forLoopSimple (start,end) fn = 
  do --var <- forValueOnly (tmpvar TInt)
     Syn var <- gensym "i"
     --init <- runEasyEmit varinit TInt "i" (int start)
     let body = runEasyEmit$ fn (Syn var)
     addChunk$ Syn$ hangbraces ("for " <> parens ( dType TInt <+> var <+> "=" <+> int start <> semi <+>
						   var <+> "<" <+> int end <> semi <+>
						   var <> "++"
						 )) indent $
	            body

cppClass name m = m


------------------------------------------------------------
-- C++ Expressions
------------------------------------------------------------

-- Hack, these must always be used together to get balanced parens:
(Syn a) ?  (Syn b) = Syn ("(" <> a <> " ? " <> b )
(Syn a) .: (Syn b) = Syn ( a <> " : " <> b  <> ")")


----------------------------------------------------------------------------------------------------

example :: EasyEmit ()
example = 
  do 
     comm "\nFirst some global vars:\n "
     y <- tmpvar TInt
     x <- var TInt "x"

     bar <- funDef (TSym "void") "bar" [TInt, TInt] $ \ (z,q) -> do
	 set y (z + q)

     comm "\nA function declaration:"
     foo <- funDef TInt "foo" [TInt] $ \y -> do
	 x += (x + y * 3)
	 x -= 4
	 ret x

     forLoopSimple (0,10) $ \i -> do
	 if_ (i == 10 || i > 100)
	     (app foo [foo [i / 10]])
	     (app foo [i ? 3 .: 4])

   --   cppClass "blah" $ do 
   --       if_ (app f x) x x 
   -- --      funDef "method" [TInt, TFloat] $ \(y,z) -> y + z

