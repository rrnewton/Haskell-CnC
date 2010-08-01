{-# LANGUAGE RecordWildCards #-}

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
import System.IO
import Control.Monad
import Data.List
import Data.Set as S hiding (map, filter, partition)
import Data.List.Split (splitOn)
import Data.Graph

----------------------------------------------------------------------------------------------------

-- A simple datatype for parsed Harch Nodes:
data HarchNode = HarchNode {
    name       :: String,             -- A mandatory field.
    properties :: [(String, String)], -- 
    num        :: Int,                -- Using numeric idenifiers for now
    in_edges   :: [Int],
    out_edges  :: [Int]
  }
 deriving Show


-- A datatype for Harch trees:
-- Parameterized by the type of the partitions themselves.
data HarchTree a = HT a [HarchTree a]

-- A simple representation to start with is a set of Ints for each partition:
type HarchTreeUnordered = HarchTree (Set Int)

-- After topological sorting the partitions are ordered.
type HarchTreeOrdered = HarchTree [Int]

----------------------------------------------------------------------------------------------------
-- Parsec parser for .harch syntax:

harchfile :: Parser [HarchNode]
harchfile = 
  do numbers; newline -- Skip the first line
     nodes <- many harchnode
     return$ map (\ (n,rec) -> rec { num= n })
	         (zip [1..] nodes)

-- Remove one property from a property list.
popProp name pls = 
    case partition ((== name) . fst) pls of
      ([(_,nm)], rest) -> (nm, rest)
      (ls,_) -> error$ "Currently, exactly one '"++ show name ++"' property is required for each graph node, not "
		        ++ show (length ls)++ ": "++ show ls

harchnode :: Parser HarchNode
harchnode = 
  do whitespc; char '%'; whitespc; string "HARCHNODE"; whitespc
     ps   <- props;    newline; 
     (wght:edges) <- numbers;  whitespc; newline;

     -- Parse out all the special fields:
     let (nm, rest) = popProp "name" ps 

     -- Are the edges mentioned inbound or outbound?
     let (dirs,rest2) = popProp "directions" rest
     let (ins,outs) = 
	  if not (length dirs == length edges) 
	  then error$ "Number of 'directions' incorrect, "++ show (length dirs) ++" "++
		      show dirs ++ " expected "++ show (length edges) ++ " for edges " ++ show edges
	  else (map snd $ filter ((== '0') . fst) $ zip dirs edges,
		map snd $ filter ((== '1') . fst) $ zip dirs edges)

     return HarchNode { name= nm, properties= rest2, 
			in_edges= ins, out_edges= outs, 
			num=0 }

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
props = prop `sepEndBy` (many1 spc)

numbers :: Parser [Int]
numbers = do whitespc
	     strs <- (many1 digit) `sepEndBy` (many1 spc)
	     return$ map read strs

------------------------------------------------------------
-- Generic harness for running a parser:
run :: Show a => Parser a -> String -> a
run p input
        = case (parse p "" input) of
            Left err -> error ("parse error at "++ show err)
            Right x  -> x

main = testread
testread = 
-- do file <- openFile "/Users/newton/cnc/experimental/graphPartitioner/test.harch" ReadMode 
-- do file <- openFile "/Users/newton/cnc/experimental/graphPartitioner/test2.harch" ReadMode 
 do file <- openFile "/Users/newton/cnc/experimental/graphPartitioner/outputs/pipes.harch.partitioned" ReadMode 
    txt <- hGetContents file
    let ls = run harchfile txt
    sequence_$ map print ls


----------------------------------------------------------------------------------------------------
-- Conversion to partitioned format:

extractPartitions :: [HarchNode] -> HarchTreeUnordered
extractPartitions nodes = undefined
 where
  foo = map (\ HarchNode{..} -> properties ) nodes


----------------------------------------------------------------------------------------------------
-- Testing 

runPr prs str = print (run prs str)

foo = do whitespc; newline
bar = do char '\n'

t1 = runPr prop$ "name=foo;"

t2 = runPr props$ "name=foo; direction=01;" -- No spcs at start end
t3 = runPr props$ "name=foo;" 

t4 = runPr props$ "name=foo; blah=baz; " 


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
