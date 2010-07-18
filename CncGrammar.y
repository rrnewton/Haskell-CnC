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
import SrcLoc
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

--	comment		{ L _ LComment _ }


-- The left hand side are the names of the terminals or tokens,
-- and the right hand side is how to pattern match them.

%nonassoc '<' '>' '<=' '>=' '=='
%left '+' '-'
%left '*' '/'

-- Like yacc, we include %% here, for no real reason.
%%

-- Now the production rules.
----------------------------------------------------------------------------------------------------


File :: { [PStatement SrcSpan] } 
File : Statements                          { $1 }

Statements : Statement Statements          { $1 : $2 }
           | Statement                     { [$1] }
Statement  : Terminated_Relation           { $1 }
           | Terminated_Decl               { $1 }

Terminated_Relation : Relation ';'         { $1 }
Terminated_Decl     : Decl     ';'         { $1 }

Decl :: { PStatement SrcSpan } 
Decl     
  : tags var                               { DeclareTags (lexSpan $1) (lexStr $2) Nothing }

Relation :: { PStatement SrcSpan }
Relation :  Instances Chain                { Chain $1 $2 }

Chain :: { [RelLink SrcSpan] }
Chain : Link                               { [$1] }
      | Link Chain                         { $1 : $2 }
Link  :  "->" Instances                    { ProduceLink (lexSpan $1) $2 }

Instances :: { [CollectionInstance SrcSpan] }
Instances
  : Instance                               { [$1] }
  | Instance ',' Instances                 { $1 : $3 }

Instance 
  : var                                    { InstName        (lexStr $1) }
  | var '[' TagExps ']'                    { InstDataTags    (lexStr $1) $3 }
  | var '(' TagExps ')'                    { InstControlTags (lexStr $1) $3 }

TagExps :: { [Exp SrcSpan] }
TagExps : Exp                              { [$1] }
	| Exp ',' TagExps                  { $1 : $3 }

--Tag : var                                  { TEVar (lexStr $1) }

Exp :: { Exp SrcSpan } -- The haskell type of the result of parsing this syntax class.

Exp : var	             	{ Var (lexSpan $1) (lexStr $1) }
    | qvar	             	{ Var (lexSpan $1) (lexStr $1) }
    | int	             	{ Lit (lexSpan $1) (LitInt $ read (lexStr $1)) }
    | '(' Exp ')'               { $2 }

-- Including explicit productions for arithmetic just to handle precedence/associativity:
    | Exp '+' Exp	        { App (combExpSpans $1 $3) (Var (lexSpan $2) "+") [$1, $3] }
    | Exp '-' Exp	        { App (combExpSpans $1 $3) (Var (lexSpan $2) "-") [$1, $3] }
    | Exp '*' Exp	        { App (combExpSpans $1 $3) (Var (lexSpan $2) "*") [$1, $3] }
    | Exp '/' Exp	        { App (combExpSpans $1 $3) (Var (lexSpan $2) "/") [$1, $3] }
    | Exp op Exp	        { App (combExpSpans $1 $3) (Var (lexSpan $2) (lexStr $2)) [$1, $3] } 


-- We are simply returning the parsed data structure!  Now we need
-- some extra code, to support this parser, and make in complete:

----------------------------------------------------------------------------------------------------
{

-- All parsers must declair this function, which is called when an error
-- is detected.  Note that currently we do no error recovery.

happyError :: [Lexeme] -> a
happyError ls =
 error ("Parse error before token at location : \n   " ++
        show (pPrint (lexLoc $ head ls)))

-- Now we declare the datastructure that we are parsing.

runCnc :: String -> [PStatement SrcSpan]
-- For now filter out comments before parsing:
runCnc = parse_cnc . filter (not . is_comment) . scan_to_list

is_comment ( L _ LComment _ ) = True 
is_comment _ = False 

lexStr :: Lexeme -> String
lexStr (L _ _ str) = str

lexLoc :: Lexeme -> SrcLoc
lexLoc (L (AlexPn n l c) _ _) = (SrcLoc "unknownfile" l c)

lexSpan :: Lexeme -> SrcSpan
lexSpan (L (AlexPn n l c) _ _) = srcLocSpan (SrcLoc "unknownfile" l c)

-- Combine the spans in two expressions.
combExpSpans e1 e2 = combineSrcSpans (getExpDecoration e1) (getExpDecoration e2)

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
 putStrLn$ renderStyle style $ hcat $ map pPrint parsed

 putStrLn "\n Ok how bout sexp:"
 let str = buildList $ sexpSerialize parsed

 putStrLn str

 putStrLn "\n Done.."    
    
}
