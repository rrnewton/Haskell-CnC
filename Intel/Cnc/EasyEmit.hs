{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances #-}

{-|
   This module provides a simple way to use HOAS (higher order abstract syntax) to emit C++ code.

   This is an AST-free method!  It doesn't define a complete model for
   C++ code, just an easier way of emitting concretes syntax than
   using strings.
   
 -}

module Intel.Cnc.EasyEmit where


import Intel.Cnc.Spec.Util hiding (app, commasep)
import Intel.Cnc.Spec.AST  

import Control.Monad 
import qualified  Control.Monad.State.Strict as S 

import Data.Data
import Data.List
import GHC.Exts -- For IsString

import StringTable.Atom
import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJClass 
import Test.HUnit

import qualified Prelude as P
import Prelude hiding ((&&), (||), (==), (/=), not, Eq
		       , Ord, (<), (<=), (>), (>=), max, min -- , compare
		      )


----------------------------------------------------------------------------------------------------
-- Monad plus HOAS for C++
----------------------------------------------------------------------------------------------------

-- | A monad for generating syntax:
--   The state consists of an accumulator for lines of syntax, plus a counter for temporary variables.
newtype EasyEmit a = EE (S.State EEState a)
type EEState = ([Doc],Int)

-- | Run a syntax-emitting computation and render it to a document.


runEasyEmit :: EasyEmit a -> (a,Doc)
runEasyEmit m = (a,b)
 where (a,_,b) = rawRunEasyEmit 0 m

-- This full version includes the counter:
rawRunEasyEmit :: Int -> EasyEmit a -> (a, Int, Doc)
rawRunEasyEmit start (EE m) = (a, cnt, vcat (reverse ls))
 where 
  (a,(ls,cnt)) = S.runState m ([], start)

execEasyEmit :: EasyEmit a -> Doc
execEasyEmit = snd . runEasyEmit

evalEasyEmit :: EasyEmit a -> a
evalEasyEmit = fst . runEasyEmit 


-- This runs a subcomputation only for value -- discards its emission side effects.
forValueOnly :: EasyEmit a -> EasyEmit a
forValueOnly (EE m) =
 do (ls,c) <- S.get 
    let (val,(_,c2)) = S.runState m ([], c)
    S.put (ls,c2)
    return val

-- BOILERPLATE, because of newtype rather than type synonym for EasyEmit:
instance Monad EasyEmit where
  -- Whew, need to verify that this has no perfermance overhead:
  (EE ma) >>= fn = EE (ma >>= (\x -> case fn x of EE m -> m))
  return x       = EE (return x)
instance S.MonadState EEState EasyEmit where
  get   = EE S.get
  put x = EE (S.put x)

instance StringBuilder EasyEmit where 
--instance StringBuilder (S.State ([Doc], Int)) where 
  putS s = S.modify (\ (ls,cnt) -> (text s:ls, cnt) )
  runSB (EE m) = 
      let (res, (ls,cnt)) = S.runState m ([],0) 
      in (render$ vcat$ reverse ls, res)

instance ToAtom Syntax where
   toAtom d = toAtom$ deSyn d


--------------------------------------------------------------------------------
-- First, simple helpers / utilities:
--------------------------------------------------------------------------------

instance P.Eq Doc where
  -- Would be nice to memoize this!
  a == b  =  render a P.== render b

-- | The EasyEmit monad uses this Syntax type internally:
data Syntax = Syn Doc
  deriving (Show, P.Eq)

deSyn (Syn s) = s
synToStr = render . deSyn 

fromInt = Syn . int 

atomToSyn = Syn . text . fromAtom
strToSyn  = Syn . text 

(Syn a) +++ (Syn b) = Syn (a <> b)

(Syn a) `dot`   (Syn b) = Syn (a <> text "." <> b)
(Syn a) `arrow` (Syn b) = Syn (a <> text "->" <> b)

dereference (Syn a) = Syn$ parens (text "*" <> a)
addressOf   (Syn a) = Syn$ parens (text "&" <> a)

-- Array subscript operator:
arrsub (Syn a) (Syn b) = Syn$ (a <> text "[" <> b <> text "]")

-- Adds implicit newline at the end:
addChunk :: Syntax -> EasyEmit ()
addChunk (Syn doc) = 
  do (ls,c) <- S.get
     S.put (doc : ls, c)

-- A bit ugly, this adds the chunk to the end of the previous line.
addChunkPrevLine (Syn doc) = 
  do (ls,c) <- S.get
     S.put $ case ls of 
              []      -> (doc : ls, c)
	      (hd:tl) -> (hd <> doc : tl, c)

-- Adds the semi-colon at the end also:
addLine (Syn doc) = addChunk (Syn$ doc <> semi)

-- Also, overloading the string constants themselves is nice:
instance IsString Syntax where
    fromString s = Syn (text s)

-- instance IsString Doc where
--     fromString s = (text s)


-- Comma separate Docs either horizontally or vertically:
-- TEMP: MAKING THIS HCAT FOR NOW UNTIL I CAN FIGURE OUT SOME OF THE INDENTATION PROBELMS:
commasep ls = hcat$ intersperse (text", ") ls

-- (This version includes parens)
--pcommasep = parens . commasep
pcommasep ls = parens$ fcat$ intersperse (text", ") ls 


--------------------------------------------------------------------------------
-- C++ Expressions
--------------------------------------------------------------------------------

(Syn a) ?  (Syn b) = Syn ("(" <> a <> " ? " <> b )
-- Hack, these must always be used together to get balanced parens:
(Syn a) .: (Syn b) = Syn ( a <> " : " <> b  <> ")")

-- The most important class of expressions are generated by overloaded
-- standard operators, as follows:

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
    max (Syn a) (Syn b) = Syn ("max" <> pcommasep [a,b] )
    min (Syn a) (Syn b) = Syn ("min" <> pcommasep [a,b] )

--   conditional (Syn a) b c = Syn (parens $ a <> " ? " <> b <> ":" <> c )

instance Eq Syntax Syntax where
    (Syn a) == (Syn b) = Syn (parens $ a <> " == " <> b )   

-- Use a C++ constant 
constant :: String -> Syntax
constant = fromString

stringconst :: String -> Syntax
stringconst str = Syn$ dubquotes$ escapeString str

-- new :: Syntax -> [Syntax] -> Syntax
new :: Type -> [Syntax] -> Syntax
new ty args = Syn$ "new" <+> (deSyn$ function (Syn$ pPrint ty) args)


--------------------------------------------------------------------------------
-- Declaring variables
--------------------------------------------------------------------------------

-- With names:
var :: Type -> Syntax -> EasyEmit Syntax
var ty (Syn name) = do addLine (Syn (cppType ty <+> name ))
		       return (Syn name)

-- With initialization expression:
varinit :: Type -> Syntax -> Syntax -> EasyEmit Syntax
varinit ty (Syn name) (Syn rhs) = 
   do addLine (Syn (cppType ty <+> name <+> "=" <+> rhs))
      return (Syn name)

-- A var declaration with a constructor invocation.
classvar :: Type -> Syntax -> [Syntax] -> EasyEmit Syntax
classvar ty (Syn name) args =
   do addLine (Syn (cppType ty <+> name <+> pcommasep (map deSyn args)))
      return (Syn name)

-- Without names:
----------------------------------------
gensym root = 
   do (ls,cnt) <- S.get
      S.put (ls,cnt+1)
      return$ Syn$ root <> int cnt

tmpvar :: Type -> EasyEmit Syntax
tmpvar ty = do name <- gensym "tmp"
	       var ty name

tmpvarinit :: Type -> Syntax -> EasyEmit Syntax
tmpvarinit ty rhs = 
   do name <- gensym "tmp"
      varinit ty name rhs
	   
-- A var declaration with a constructor invocation.
tmpclassvar :: Type -> [Syntax] -> EasyEmit Syntax
tmpclassvar ty args =
   do name <- gensym "obj"
      classvar ty name args


------------------------------------------------------------
-- C++ Statements 
------------------------------------------------------------

-- This is a function in the meta language that represents a function
-- in the object language.
type ObjFun = ([Syntax] -> Syntax)

-- Variable Assignment:
set :: Syntax -> Syntax -> EasyEmit ()
set (Syn x) (Syn v) = addLine$ Syn$ x <+> "=" <+> v 

-- Assignment for array locations:
arrset :: Syntax -> Syntax -> Syntax -> EasyEmit ()
arrset arr i rhs = set (arr `arrsub` i) rhs

-- Assignment for a field R.x.
fieldset :: Syntax -> Syntax -> Syntax -> EasyEmit ()
fieldset arr x rhs = set (arr `dot` x) rhs

-- Function application (command context):
app :: ObjFun -> [Syntax] -> EasyEmit ()
app fn ls = addLine$ fn ls


-- A shorthand that looks C-ish:
(Syn x) += (Syn n) = addLine$ Syn$ x <+> "+=" <+> n
(Syn x) -= (Syn n) = addLine$ Syn$ x <+> "-=" <+> n

-- Comments:
--comm :: Doc -> EasyEmit ()
comm :: String -> EasyEmit ()
comm x  = addChunk$ Syn$ txt
 where 
   txt = text$ maybeInit$ unlines lns -- init strips the last newline
   maybeInit [] = []
   maybeInit ls = init ls
   lns = map fn $ lines x --(render x)
   fn "" = ""
   fn other = "// " ++ other

if_ (Syn a) m1 m2 = 
  do let bod1 = execEasyEmit m1
	 bod2 = execEasyEmit m2
     addChunk$ Syn$ hangbraces ("if " <> parens a) indent bod1
     addChunk$ Syn$ "else"
     addChunk$ Syn$ hangbraces (empty) indent bod2

ret (Syn x) = addLine$ Syn$ "return " <> x

assert (Syn exp) = 
 do addLine$ Syn ("assert" <> pcommasep [exp])


------------------------------------------------------------
-- C++ Definitions & Declarations
------------------------------------------------------------

-- The funDef function creates a function definition as well as
-- returning a Haskell function that can be used to construct
-- applications of that function.
class FunDefable args where
  -- This is VERY painful, but need to expose separate keywords for pre- and post- the function name/args.
  funDefAttr :: String -> String -> Type -> Syntax -> [Type] -> (args -> EasyEmit ()) -> EasyEmit ObjFun

instance FunDefable ()                            where funDefAttr pre post r n ts fn = funDefShared pre post r n ts fn (\ [] -> ())
instance FunDefable Syntax                        where funDefAttr pre post r n ts fn = funDefShared pre post r n ts fn (\ [a] -> a)        
instance FunDefable (Syntax,Syntax)               where funDefAttr pre post r n ts fn = funDefShared pre post r n ts fn (\ [a,b] -> (a,b))         
instance FunDefable (Syntax,Syntax,Syntax)        where funDefAttr pre post r n ts fn = funDefShared pre post r n ts fn (\ [a,b,c] -> (a,b,c))     
instance FunDefable (Syntax,Syntax,Syntax,Syntax) where funDefAttr pre post r n ts fn = funDefShared pre post r n ts fn (\ [a,b,c,d] -> (a,b,c,d)) 
instance FunDefable [Syntax]                      where funDefAttr pre post r n ts fn = funDefShared pre post r n ts fn id


-- Terrible ugliness just to deal with those darn const qualifiers:
funDef       :: FunDefable args => Type -> Syntax -> [Type] -> (args -> EasyEmit ()) -> EasyEmit ObjFun
constFunDef  :: FunDefable args => Type -> Syntax -> [Type] -> (args -> EasyEmit ()) -> EasyEmit ObjFun
inlineFunDef :: FunDefable args => Type -> Syntax -> [Type] -> (args -> EasyEmit ()) -> EasyEmit ObjFun

funDef       = funDefAttr "" ""
constFunDef  = funDefAttr "" "const"
inlineFunDef = funDefAttr "inline" ""


-- Internal helper function used above:
funDefShared pre postqualifiers retty (Syn name) tyls fn formTup  = 
    do (ls,c) <- S.get 
       --args <- mapM (forValueOnly . tmpvar) tyls -- Generate temp names only (emit nothing).
       args <- sequence$ take (length tyls) (repeat$ gensym "arg") -- Generate temp names only (emit nothing).
       let body = execEasyEmit (fn$ formTup args)
	   formals = map (\ (t,a) -> cppType t <+> deSyn a) (zip tyls args)
       addChunk$ Syn$ 
          hangbraces ((if pre P.== "" then empty else text (pre++" ")) <> 
		      cppType retty <+> name <> pcommasep formals <+> (text postqualifiers)) 
	             indent body
--       addChunk$ "\n"
       return (\ args -> Syn$ name <> (pcommasep (map deSyn args)))


-- This applies a C++ function referred to by name (defined elsewhere):
function :: Syntax -> ObjFun
function (Syn name) = 
  \ args -> Syn$ name <> (pcommasep$ map deSyn args)

-- Shortcut for applying a method given an object or object reference.
methcall :: Syntax -> Syntax -> ObjFun
methcall obj meth args = obj `dot` (function meth args)


-- Common case: for loop over a range with integer index:
------------------------------------------------------------
-- Input is [Inclusive,Exclusive) range.
forLoopSimple :: (Syntax,Syntax) -> (Syntax -> EasyEmit ()) -> EasyEmit ()
forLoopSimple (start,end) fn = 
  do 
     Syn var <- gensym "i"
     (ls,oldcnt) <- S.get

     let (_,newcnt,body) = rawRunEasyEmit oldcnt $ fn (Syn var)

     -- To maintain uniqueness we take the counter mods from the body.
     -- TODO: A cleaner way to do this would be to include the indent level in the state monad and NOT LEAVE IT.
     S.put (ls, newcnt)

     addChunk$ Syn$ hangbraces ("for " <> parens ( cppType TInt <+> var <+> "=" <+> deSyn start <> semi <+>
						   var <+> "<" <+> deSyn end <> semi <+>
						   var <> "++"
						 )) indent $
	            body

-- Helper used below:
a <++> b | a P.== "" = b
a <++> b | b P.== "" = a
a <++> b             = a <+> b

-- This creates a class-decl-like pattern with a ':'
-- It is parameterized by a little prefix which could be "class" or "struct" or anything else.
-- Similarly, there's a postfix which is usually ";".
classLike :: String -> String -> Syntax -> Syntax -> EasyEmit () -> EasyEmit ()
classLike prefix postfix (Syn name) (Syn inherits) m = 
  do 
     putD$ t prefix <++> 
	   hsep (if inherits P.== empty
		 then [name]
		 else [name <+> colon, inherits])
     block m
     addChunkPrevLine$ strToSyn postfix 

-- | Takes: name, inheritance expression, and a body.
cppClass :: Syntax -> Syntax -> EasyEmit () -> EasyEmit ()
cppClass = classLike "class" ";"

-- | Takes: name, inheritance expression, and a body.
cppStruct :: Syntax -> Syntax -> EasyEmit () -> EasyEmit ()
cppStruct = classLike "struct" ";"

-- Arg list is (type,argname) and init list is (name,expression/value)
cppConstructor :: Syntax -> [(Type,Syntax)] -> [(Syntax,Syntax)] -> EasyEmit () -> EasyEmit ()
cppConstructor (Syn name) args inits body = 
    let fn (t,x) = pPrint t <+> deSyn x in
    classLike "" "" (Syn$ name <> parens (commasep$ map fn args)) 
		    (Syn$ commasep$ map (\ (a,b) -> deSyn a <> parens (deSyn b)) inits)
	      body

-- Constructor method prototypes just look like applications
constructorPrototype :: Syntax -> [Syntax] -> EasyEmit ()
constructorPrototype name args = 
  app (function name) args

-- Curly-brace delimited, indented block:
-- POSSIBLY INEFFICIENT!
-- TODO: Implement this by tracking the indent level in the monad:
block :: EasyEmit () -> EasyEmit ()
block m = 
  do let body = execEasyEmit m
     addChunk "{"
     addChunk$ Syn$ nest indent body
     addChunk "}"



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
-- Internal helpers:
----------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------
-- Testing:
----------------------------------------------------------------------------------------------------

t1 = cppClass "foo" "bar" $ comm "body"

t2 = execEasyEmit$ funDef TInt "foo" [TInt] $ \(Syn y) -> 
       do comm "Body"
	  funDef TInt "nested" [] $ \ () -> comm "Inner body"
	  return ()


ee_example :: EasyEmit ()
ee_example = 
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

     let baz  = function "baz"
	 quux = fromString "quux"

     app baz [quux, quux]

     forLoopSimple (0,10) $ \i -> do
	 if_ (i == 10 || i > 100)
	     (app foo [foo [i / 10]])
	     (app foo [i ? 3 .: 4])

   --   cppClass "blah" $ do 
   --       if_ (app f x) x x 
   -- --      funDef "method" [TInt, TFloat] $ \(y,z) -> y + z

ee_test1 = testCase "" "Code emission example"$ 
	   391 ~=? (length$ render$ execEasyEmit ee_example)

tests_easyemit = testSet "EasyEmit" [ee_test1]
