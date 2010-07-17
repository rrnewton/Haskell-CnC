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
--import Intel.Cnc.Translator.AST
import AST
}

-- (Based on example from Simon Marlow.)

-- First thing to declare is the name of your parser,
-- and the type of the tokens the parser reads.

%name parse_cnc
%tokentype { Lexeme }

-- The parser will be of type [Token] -> ?, where ? is determined by the
-- production rules.  Now we declare all the possible tokens:


-- These are similar macros to those used by the GHC parser:
-- define L0   L noSrcSpan
-- define L1   sL (getLoc $1)
-- define LL   sL (comb2 $1 $>)

%token 

	var		{ L _ LVarId  _ }
	qvar		{ L _ LQVarId _ }
	int		{ L _ LInteger _ }

	"->"		{ L _ LReservedOp "->" }
	"<-"		{ L _ LReservedOp "<-" }
	"::"		{ L _ LReservedOp "::" }

	'('		{ L _ LSpecial "(" }
	')'		{ L _ LSpecial ")" }
	';'		{ L _ LSpecial ";" }

	'['		{ L _ LSpecial "[" }
	']'		{ L _ LSpecial "]" }
	','		{ L _ LSpecial "," }

	'+'		{ L _ LVarOp "+" }
	'-'		{ L _ LVarOp "-" }
	'*'		{ L _ LVarOp "*" }
	'/'		{ L _ LVarOp "/" }
	op		{ L _ LVarOp _ }

	tags		{ L _ LReservedId "tags" }
	items		{ L _ LReservedId "items" }
	steps		{ L _ LReservedId "steps" }

	comment		{ L _ LComment _ }


-- The left hand side are the names of the terminals or tokens,
-- and the right hand side is how to pattern match them.

%nonassoc '<' '>' '<=' '>=' '=='
%left '+' '-'
%left '*' '/'

-- Like yacc, we include %% here, for no real reason.
%%

-- Now the production rules.
----------------------------------------------------------------------------------------------------


File :: { [PStatement SrcLoc] } 
File : Statements                          { $1 }

Statements : Statement Statements          { $1 : $2 }
           | Statement                     { [$1] }
Statement  : Terminated_Relation           { $1 }
           | Terminated_Decl               { $1 }

Terminated_Relation : Relation ';'         { $1 }
Terminated_Decl     : Decl     ';'         { $1 }

Decl :: { PStatement SrcLoc } 
Decl     
  : tags var                               { DeclareTags (lexLoc $1) (unLoc $2) Nothing }

Relation :: { PStatement SrcLoc }
Relation 
  :  Instances "->" Instances              { Produce (lexLoc $2) $1 $3 }

Instances :: { [Instance] }
Instances
  : Instance                               { [$1] }
  | Instance ',' Instances                 { $1 : $3 }

Instance 
  : var                                    { IName (unLoc $1) }
--  | var '[' ']'                            { $1 }

Exp :: { Exp SrcLoc } -- The haskell type of the result of parsing this syntax class.

Exp : var	             	{ Var (lexLoc $1) (unLoc $1) }
    | qvar	             	{ Var (lexLoc $1) (unLoc $1) }
    | int	             	{ Lit (lexLoc $1) (LitInt $ read (unLoc $1)) }
    | '(' Exp ')'               { $2 }

-- Including explicit productions for arithmetic just to handle precedence/associativity:
    | Exp '+' Exp	        { App (getLoc $1) (Var (lexLoc $2) "+") [$1, $3] }
    | Exp '-' Exp	        { App (getLoc $1) (Var (lexLoc $2) "-") [$1, $3] }
    | Exp '*' Exp	        { App (getLoc $1) (Var (lexLoc $2) "*") [$1, $3] }
    | Exp '/' Exp	        { App (getLoc $1) (Var (lexLoc $2) "/") [$1, $3] }
    | Exp op Exp	        { App (getLoc $1) (Var (lexLoc $2) (unLoc $2)) [$1, $3] } 


-- We are simply returning the parsed data structure!  Now we need
-- some extra code, to support this parser, and make in complete:

----------------------------------------------------------------------------------------------------
{

-- All parsers must declair this function, which is called when an error
-- is detected.  Note that currently we do no error recovery.

happyError :: [Lexeme] -> a
happyError _ = error ("Parse error\n")

-- Now we declare the datastructure that we are parsing.

runCnc :: String -> [PStatement SrcLoc]
runCnc = parse_cnc . scan_to_list


unLoc :: Lexeme -> String
unLoc (L _ _ str) = str

lexLoc :: Lexeme -> SrcLoc
lexLoc (L (AlexPn n l c) _ _) = SrcLoc "unknownfile" l c

quit = print "runCnc failed\n"



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
 let str = buildList $ sexpSerialize parsed

 putStrLn str

 putStrLn "\n Done.."    
    
}
