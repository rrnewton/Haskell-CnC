


----------------------------------------------------------------------------------------------------
-- Read .harch profiled/partitioned graph files.
--
-- Original Author: Ryan Newton
----------------------------------------------------------------------------------------------------

module Intel.Cnc.Spec.ReadHarch where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Data.List
import System.IO
import Control.Monad

simple :: Parser Char
simple  = letter

nesting :: Parser Int
nesting = do{ char '('
            ; n <- nesting
            ; char ')'
            ; m <- nesting
            ; return (max (n+1) m)
            }
        <|> return 0        

-- word    :: Parser String
-- word    = do{  c  <- letter
--             ;  do{  cs <- word
--                  ;  return (c:cs)
--                  }
--                <|> return [c]
--             }
--  word    

word = many1 letter

wordls :: Parser [String]
wordls = do w <- word; 
	    (do space; spaces; ws <- wordls; return$ w:ws) <|> return [w]
	  <|> return []

--	     ((do ws <- wordls; return$ w:ws)
--	      <|> return [w])

t0 = run nesting "(())()"

run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x

-- Require at least one:
--props = prop `sepBy1` (many1 space)
--props = prop `sepBy` (many1 space)
--props = prop `sepBy` spaces
--props = many1 (do spaces; prop;)


-- numbers = do spaces 
-- 	     strs <- sepBy1 (many1 digit) (many1 space)
-- 	     return$ map read strs

----------------------------------------------------------------------------------------------------

data HarchNode = HarchNode {
    name       :: String,             -- A mandatory field.
    properties :: [(String, String)], -- 
    out_edges  :: [Int]
  } 
 deriving Show

spc = oneOf " \t"
whitespc = many spc

prop :: Parser (String,String)
prop = do w <- many1 letter
	  char '=';
	  p <- many1 (noneOf ";")
 	  char ';';
	  return (w,p)

-- Having problems with sepBy:
props :: Parser [(String,String)]
props  = do p <- prop; 
	    (do many1 spc; ps <- props; return$ p:ps) <|> return [p]
	  <|> return []

numbers :: Parser [Int]
numbers = do n <- many1 digit; 
	     (do many1 spc; ns <- numbers; return$ (read n):ns) <|> return [read n]
	   <|> return []

--commentline :: Parser String
harchnode :: Parser HarchNode
-- harchnode = 
--   do whitespc; char '%'; whitespc; string "HARCHNODE"; whitespc
--      ps <- props
--      whitespc; newline; whitespc
--      nums <- numbers
--      whitespc; newline
--      let ([("name",nm)], rest) = partition ((== "name") . fst) ps 
--      return HarchNode { name= nm, properties= ps, out_edges= nums }

harchnode = 
  do ps   <- nodeHeader
     nums <- edgeline
     let ([("name",nm)], rest) = partition ((== "name") . fst) ps 
     return HarchNode { name= nm, properties= ps, out_edges= nums }

nodeHeader = 
  do whitespc; char '%'; whitespc; string "HARCHNODE"; whitespc
     ps <- props; 
     --newline
     char '\n'
     --whitespc; newline
     return ps 

edgeline = 
  do whitespc
     nums <- numbers
     whitespc; newline
     return nums



harchfile :: Parser [HarchNode]
harchfile = 
  do numbers; newline -- Skip the first line
     many harchnode

debug = 
  do whitespc; char '%'; whitespc; string "HARCHNODE"; whitespc
     props
     -- prop
     -- spcs
     -- prop
--     ps <- props
--     spcs; newline
--     return "yay"

----------------------------------------------------------------------------------------------------
-- Testing 

foo = do whitespc; newline
bar = do char '\n'

t1 = run prop$ "name=foo;"

t2 = run props$ "name=foo; direction=01;" -- No spcs at start end
t3 = run props$ "name=foo;" 

t4 = run debug$ "% HARCHNODE name=blah; \n"

l1 = "% HARCHNODE name=blah; direction=01;\n"
--t5 = run debug$ l1
t5 = run nodeHeader l1

l2 = "0 1 2 \n"
--t6 = run numbers$ l2
t6 = run edgeline  l2 

t7 = run foo "   \n"
t8 = run foo "\n"
t9 = run bar "\n"


--t11 = run harchnode$ "% HARCHNODE name=blah; direction=01;\nw% HARCHNODE name=blah; direction=01; 0 1 2"
t11 = run harchnode$ l1 ++ "\n" ++ l2

tests = sequence_ [t1,t2,t3,t4,t5,t6, t11]

test = do file <- openFile "/Users/newton/cnc/experimental/graphPartitioner/test.harch" ReadMode 
	  txt <- hGetContents file
	  run harchfile txt


