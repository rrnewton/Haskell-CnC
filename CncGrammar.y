{
{-# LANGUAGE DeriveDataTypeable #-}
module Main where
--module CncGrammar where
import Char
import CncLexer hiding (main)
import StringTable.Atom
import Data.Data
import Text.PrettyPrint.HughesPJClass
import Data.Generics.Serialization.SExp
import Data.Generics.Serialization.Streams
}

-- (Based on example from Simon Marlow.)

-- First thing to declare is the name of your parser,
-- and the type of the tokens the parser reads.

%name parse_cnc
%tokentype { Lexeme }

-- The parser will be of type [Token] -> ?, where ? is determined by the
-- production rules.  Now we declare all the possible tokens:

%token 
-->	let		{ TokenLet }
-- >	in		{ TokenIn }
-- >	int		{ TokenInt $$ }
-- >	'='		{ TokenEq }
-- >	'+'		{ TokenPlus }
-- >	'-'		{ TokenMinus }
-- >	'*'		{ TokenTimes }
-- >	'/'		{ TokenDiv }
-- >	'('		{ TokenOB }
-- >	')'		{ TokenCB }

	var		{ L _ LVarId  $$ }
	qvar		{ L _ LQVarId $$ }
	int		{ L _ LInteger $$ }

	'+'		{ L _ LVarOp "+" }
	'-'		{ L _ LVarOp "-" }
	'*'		{ L _ LVarOp "*" }
	'/'		{ L _ LVarOp "/" }
	op		{ L _ LVarOp $$ }

-- The left hand side are the names of the terminals or tokens,
-- and the right hand side is how to pattern match them.

%nonassoc '<' '>' '<=' '>=' '=='
%left '+' '-'
%left '*' '/'

-- Like yacc, we include %% here, for no real reason.
%%

-- Now the production rules.
----------------------------------------------------------------------------------------------------



-- > Exp :: { Exp }
--> Exp : let var '=' Exp in Exp	{ Let $2 $4 $6 }
-- >     | Exp1			{ Exp1 $1 }
-- > 
-- > Exp1 : Exp1 '+' Term		{ Plus $1 $3 }
-- >      | Exp1 '-' Term		{ Minus $1 $3 }
-- > 
--> Exp : Exp op Exp	        { App $2 [$1, $3] } 

Exp : Exp op Exp	        { App (Var $2) [$1, $3] } 
--    | int			{ Lit (LitInt $ read $1) }

Exp : var	             	{ Var $1 }
Exp : qvar	             	{ Var $1 }
Exp : int	             	{ Lit (LitInt $ read $1) }

-- Including explicit productions for arithmetic just to handle precedence/associativity:

Exp : Exp '+' Exp	        { App (Var "+") [$1, $3] }
Exp : Exp '-' Exp	        { App (Var "-") [$1, $3] }
Exp : Exp '*' Exp	        { App (Var "*") [$1, $3] }
Exp : Exp '/' Exp	        { App (Var "/") [$1, $3] }

-- We are simply returning the parsed data structure!  Now we need
-- some extra code, to support this parser, and make in complete:

----------------------------------------------------------------------------------------------------
{

-- All parsers must declair this function, which is called when an error
-- is detected.  Note that currently we do no error recovery.

happyError :: [Lexeme] -> a
happyError _ = error ("Parse error\n")

-- Now we declare the datastructure that we are parsing.

data Lit = LitInt Int | LitFloat Float
 deriving (Eq, Ord, Show, Data, Typeable)

data Exp = 
   Lit Lit
 | Var String
 | App Exp [Exp]

 deriving (Eq, Ord, Show, Data, Typeable)


instance Pretty Lit where 
 pPrint (LitInt i)   = pPrint i
 pPrint (LitFloat f) = pPrint f

instance Pretty Exp where 
 pPrint (Lit l) = pPrint l
 pPrint (Var s) = text s
 pPrint (App rator rands) = 
     parens $  
        pPrint rator <+> sep (map pPrint rands)
--      sep (pPrint rator : map pPrint rands)


runCnc :: String -> Exp
runCnc = parse_cnc . scan_to_list

-- data SrcLoc = SrcLoc {
-- 		srcFilename :: String,
-- 		srcLine     :: Int,
-- 		srcColumn   :: Int
-- 		}
--  deriving (Eq,Ord,Show,Typeable,Data)


 -- I don't actually see why we need interned strings for filenames.  How
 -- many unique files are they?  They should be shared properly even as
 -- normal strings.  And how often do they need to be compared?

data SrcLoc
--  = SrcLoc	Atom	-- A precise location (file name)
  = SrcLoc	String	-- A precise location (file name)
		{-# UNPACK #-} !Int		-- line number, begins at 1
		{-# UNPACK #-} !Int		-- column number, begins at 1
 deriving (Eq,Ord,Show)


quit = print "runCnc failed\n"


foo = buildList .  sexpSerialize

-- Here we test our parser.
main = do 
 s <- getContents
 putStrLn "Lexed: "
 sequence_ $ map print $ scan_to_list s

 let parsed = runCnc s
 putStrLn "\nParsed:"
 print parsed
 putStrLn "\nPretty:"
 putStrLn$ renderStyle style $ pPrint parsed

 putStrLn "\n Ok how bout sexp:"
 let str = foo parsed

 putStrLn str

 putStrLn "\n Done.."    
    
}
