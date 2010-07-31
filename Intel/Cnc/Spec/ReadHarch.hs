

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

----------------------------------------------------------------------------------------------------

-- A simple datatype for parsed Harch Nodes:
data HarchNode = HarchNode {
    name       :: String,             -- A mandatory field.
    properties :: [(String, String)], -- 
    num        :: Int,                -- Using numeric idenifiers for now
    out_edges  :: [Int]
  }
 deriving Show

harchfile :: Parser [HarchNode]
harchfile = 
  do numbers; newline -- Skip the first line
     nodes <- many harchnode
     return$ map (\ (n,rec) -> rec { num= n })
	         (zip [1..] nodes)

harchnode :: Parser HarchNode
harchnode = 
  do whitespc; char '%'; whitespc; string "HARCHNODE"; whitespc
     ps   <- props;    whitespc; newline; 
     nums <- numbers;  whitespc; newline;
     let ([("name",nm)], rest) = partition ((== "name") . fst) ps 
     return HarchNode { name= nm, properties= rest, out_edges= nums, num=0 }

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
props = prop `sepBy` (many1 spc)

numbers :: Parser [Int]
numbers = do whitespc
	     strs <- (many1 digit) `sepEndBy` (many1 spc)
	     return$ map read strs

----------------------------------------------------------------------------------------------------
-- Generic harness for running a parser:
run :: Show a => Parser a -> String -> a
run p input
        = case (parse p "" input) of
            Left err -> error ("parse error at "++ show err)
            Right x  -> x

testread = 
 do file <- openFile "/Users/newton/cnc/experimental/graphPartitioner/test.harch" ReadMode 
    txt <- hGetContents file
    let ls = run harchfile txt
    sequence_$ map print ls

----------------------------------------------------------------------------------------------------
-- Testing 

runPr prs str = print (run prs str)

foo = do whitespc; newline
bar = do char '\n'

t1 = runPr prop$ "name=foo;"

t2 = runPr props$ "name=foo; direction=01;" -- No spcs at start end
t3 = runPr props$ "name=foo;" 

l1 = "% HARCHNODE name=blah; direction=01;\n"
--t5 = runPr debug$ l1
--t5 = runPr nodeHeader l1

l2 = " 0 1 2 "
t6 = runPr numbers$ l2
--t6 = runPr edgeline  l2 

t7 = runPr foo "   \n"
t8 = runPr foo "\n"
t9 = runPr bar "\n"


--t11 = runPr harchnode$ "% HARCHNODE name=blah; direction=01;\nw% HARCHNODE name=blah; direction=01; 0 1 2"
t11 = runPr harchnode$ l1 ++ l2 ++ "\n"

tests = sequence_ [t1,t2,t3, t7,t8,t9, t11]

----------------------------------------------------------------------------------------------------
