
{
--module Main  where
--module Main (main) where
--module CncLexer (Token(..), alexScanTokens) where
module Intel.Cnc.Spec.CncLexer where
}

%wrapper "monad"

-- Based on example from Happy distribution.

-- First some useful macros:
----------------------------------------------------------------------------------------------------
$whitechar = [ \t\n\r\f\v]
-- RRN [2010.07.19] Moving asterisk to special category:
-- This may be a bad idea wrt extensibility.
$special   = [\(\)\,\;\[\]\`\{\}\*]

$digit     = 0-9
$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]

$ascsymbol = [\!\#\$\%\&\+\.\/\<\=\>\?\@\\\^\|\-\~]
$symbol    = [$ascsymbol] # [$special \_\:\"\']

-- Almost everything:
$graphic   = [$small $large $symbol $digit $special \:\"\']

$idchar    = [$alpha $digit \']
$symchar   = [$symbol \:]

@reservedid = 
	module|step|fun|tags|items|steps|dense|bounded

@reservedop =
        "::" | "|" | "<-" | "->" | "{" | "}"  | "<" | ">" 
-- | "[" | "]"  
 -- | "*" | "+"

@varid  = $idchar+
@capid  = $large $idchar*

-- So called "ops" are usually infix and start with punctuation:
@varop = $symbol $symchar*
--@consym = \: $symchar*

@decimal     = $digit+
@exponent    = [eE] [\-\+] @decimal

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
	 | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
	 | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
	 | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal )
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap



-- Here are the productions themselves:
----------------------------------------------------------------------------------------------------
haskell :-

<0> $white+			{ skip }

-- <0> "//"\-*[^$symbol].*		{ mkL LComment }
<0> "//".*		{ mkL LComment }
"/*"				{ nested_comment }

<0> $special			{ mkL LSpecial }

<0> @reservedid			{ mkL LReservedId }
<0> ( @capid \. )+ @varid	{ mkL LQVarId }
<0> @varid			{ mkL LVarId }
<0> @capid			{ mkL LCapid }

<0> @reservedop			{ mkL LReservedOp }
<0> ( @capid \. )+ @varop	{ mkL LVarOp }
--<0> @capid \. @consym		{ mkL LConSym }
<0> @varop			{ mkL LVarOp }
--<0> @consym			{ mkL LConSym }

<0> @decimal       		{ mkL LInteger }

<0> @decimal \. @decimal @exponent?
  | @decimal @exponent		{ mkL LFloat }

<0> \' ($graphic # [\'\\] | " " | @escape) \'
				{ mkL LChar }
<0> \" @string* \"		{ mkL LString }



----------------------------------------------------------------------------------------------------
{ -- Begin Haskell code block to include in output:

-- The type of tokens:
data Lexeme = L AlexPosn LexemeClass String
  deriving Show

data LexemeClass
  = LInteger
  | LFloat
  | LChar
  | LString
    
  | LComment

  | LSpecial
  | LReservedId
  | LReservedOp
  | LVarId
  | LQVarId
  | LCapid
  | LQCapid
  | LVarOp
  | LQVarOp
--  | LConSym
--  | LQConSym
  | LEOF
 deriving (Eq, Show)
  
-- Handle a common case: create one token 
mkL :: LexemeClass -> AlexInput -> Int -> Alex Lexeme
mkL c (p,_,str) len = return (L p c (take len str))


-- This handles arbitrarily nested comments:
nested_comment :: AlexInput -> Int -> Alex Lexeme
nested_comment (apos, chr, str) int = do
  input <- alexGetInput
  go 1 input "*/"
        -- When finished, set the position to after the comment ('input')
--  where go 0 input acc = do alexSetInput input; alexMonadScan
  where go 0 input acc = do alexSetInput input; (mkL LComment (apos,chr, reverse acc) (length acc))
	go n input acc = do
	  -- The 'n' parameter here keepstrack of the nesting.
	  case alexGetChar input of
	    Nothing  -> err input
	    Just (c,input) -> do
	      case c of

                -- We've got a comment ENDING:
	    	'*' -> do
		  case alexGetChar input of
		    Nothing  -> err input
		    Just ('/',input) -> go (n-1) input ('/':'*':acc)
		    Just (c,input)   -> go n input (c:'*':acc)
					
                -- Here we've got another comment BEGINNING:
	     	'/' -> do
		  case alexGetChar input of
		    Nothing  -> err input
		    Just ('*',input) -> go (n+1) input ('*':'/':acc)
		    Just (c,input)   -> go n input (c:'/':acc)
	    	c -> go n input (c:acc)

        err input = do alexSetInput input; lexError "error in nested comment"  


lexError :: String -> Alex b 
lexError s = do
  (p,c,input) <- alexGetInput
  alexError (showPosn p ++ ": " ++ s ++ 
		   (if (not (null input))
		     then " before " ++ show (head input)
		     else " at end of file"))

-- Returns either a list of tokens or an error:
scan_to_list :: String -> [Lexeme]
scan_to_list str = 
   case result of 
     Left err -> error$ "Error in lexing stage:\n" ++ err
     Right ls -> ls
 where 
   result = runAlex str $ do
{-
-- TODO: get line number for lex error:
     let loop i = do tok@(L _ cl _) <- alexMonadScan; 
-}		     
-- Maybe we can hack the monad here by replacing bind with our own version.
     let loop i = do tok@(L _ cl _) <- alexMonadScan; 
		     if cl == LEOF
		        then return []
			else do ls <- loop $! (i+1)
				return (tok:ls)
     loop 0


alexEOF = return (L undefined LEOF "")

showPosn (AlexPn _ line col) = "line " ++ show line ++ ", col " ++ show col

main = do
  putStrLn "HEllo!\n"
  s <- getContents
  --s <- getLine
  --print (scanner s)
  sequence_ (map print $ scan_to_list s)
  -- case scan_to_list s of 
  --   Left err -> print err
  --   Right ls -> sequence_ (map print ls)

} -- End final code block:



