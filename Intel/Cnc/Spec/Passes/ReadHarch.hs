{-# LANGUAGE RecordWildCards, TupleSections, ScopedTypeVariables #-}

----------------------------------------------------------------------------------------------------
-- Read .harch profiled/partitioned graph files.
--
-- Original Author: Ryan Newton
----------------------------------------------------------------------------------------------------

module Intel.Cnc.Spec.Passes.ReadHarch 
    -- (
    --   readHarchFile,
    --   HarchNode (..),
    --   test_readharch
    -- ) 
  where

import Intel.Cnc.Spec.CncGraph
import Intel.Cnc.Spec.Util

import Test.HUnit 
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import System.IO
import Control.Monad
import Data.List
--import Data.Set as S hiding (map, filter, partition)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split (splitOn) -- from 'split' package 
import Data.Graph
import Debug.Trace
import Data.Function
import qualified Control.Exception as CE

import qualified  Data.Graph.Inductive as G
import Data.Graph.Inductive.Query.Monad (mapFst, mapSnd)

----------------------------------------------------------------------------------------------------

-- A graph from a Harch file is currently not quite the same datatype as a CncGraph.
-- The HarchGraph has only step collections currently.  It has no edge labels.
type HarchGraph = G.Gr HarchNode ()

-- A simple datatype for parsed Harch Nodes:
data HarchNode = HarchNode {
    name       :: String,             -- A mandatory field.
    properties :: [(String, String)], -- 
    num        :: Int                 -- Using numeric idenifiers for now
  }
 deriving Show

data HarchNodeParse = HNP {
    hnode      :: HarchNode,
    in_edges   :: [Int],
    out_edges  :: [Int]
  }
 deriving Show

-- A datatype for Harch trees:
-- Parameterized by the type of the partitions themselves.
data HarchTree a = HT a [HarchTree a]
 deriving (Show, Eq, Ord)

-- A simple representation to start with is a set of Ints for each partition:
type HarchTreeUnordered = HarchTree (Set Int)

-- After topological sorting the partitions are ordered.
type HarchTreeOrdered = HarchTree [Int]

----------------------------------------------------------------------------------------------------
-- Parsec parser for .harch syntax:

harchfile :: Parser [HarchNodeParse]
harchfile = 
  do numbers; newline -- Skip the first line
     nodes <- many harchnode
     return$ map (\ (n, rec) -> 
		   let h = hnode rec in  rec { hnode= h{ num= n } })
	         (zip [1..] nodes)

-- Remove one property from a property list.
popProp name pls = 
    case partition ((== name) . fst) pls of
      ([(_,nm)], rest) -> (nm, rest)
      (ls,_) -> error$ "Currently, exactly one '"++ show name ++"' property is required for each graph node, not "
		        ++ show (length ls)++ ": "++ show ls

-- Remove a property that may occur multiple times:
popMultiProp name pls = 
  error "TODO, popMultiProp: implement me"

harchnode :: Parser HarchNodeParse
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
	  then error$ "\nNumber of 'directions' incorrect, "++ show (length dirs) ++" "++
		      show dirs ++ " expected "++ show (length edges) ++ " for edges " ++ show edges
	  else (map snd $ filter ((== '0') . fst) $ zip dirs edges,
		map snd $ filter ((== '1') . fst) $ zip dirs edges)

     return HNP {
	      hnode = HarchNode { name= nm, properties= rest2, num=0 },
	      in_edges= ins, out_edges= outs 
	    }

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

--main = testread

readHarchFile :: String -> IO HarchGraph
readHarchFile path = 
 do file <- openFile path ReadMode 
    txt  <- hGetContents file
    let ls = run harchfile txt
    let part = extractPartitions ls
    let gr = convertHarchGraph ls
    return gr



----------------------------------------------------------------------------------------------------
-- Conversion to partitioned format:

extractPartitions :: [HarchNodeParse] -> HarchTreeUnordered
extractPartitions parsednodes = 
  --trace ("Allnums: "++ show allnums)$ 
  build (allnums, sorted)
 where
  nodes = map hnode parsednodes
  allnums :: Set Int
  allnums = Set.fromList$ map num nodes

  sorted :: [([Int], HarchNode)]
  sorted = sortBy (\ a b -> fst a `compare` fst b) $
	          map extract nodes
  
  extract :: HarchNode -> ([Int], HarchNode) 
  extract (nd@HarchNode{..}) = 
     let (partstr, rest) = popProp "partitions" properties in
     (map read $ splitOn ":" partstr :: [Int],
      nd { properties= rest})

  -- Build a tree recursively:
  build :: (Set Int, [([Int], HarchNode)]) -> HarchTreeUnordered
--  build (set, paths) | Set.null set = 
  build (set, paths) = 
     -- Each group is a child partition
     let grouped = map (map (mapFst tail)) $ 
		   groupBy ((==) `on` (head . fst)) $ 
		   filter (not . null . fst) paths 
	 -- For each sub-partition cut down the node set.
         restricted = map (Set.intersection set) $ 
		       map (Set.fromList . map (num . snd)) grouped

     in 
     --trace ("grouped "++ show (map (map fst) grouped) ++ " \t\tset "++ show set) $
     HT set (map build $ zip restricted grouped)
     

convertHarchGraph  :: [HarchNodeParse] -> HarchGraph
convertHarchGraph parsednodes = 
   -- Quick sanity check, catch things before the HORRIBLE fgl errors do.
   if Set.null diff 
   then G.mkGraph vertices edges
   --trace (" Vertices "++ show (map fst vertices) ++"\n Edges "++ show edges) $
   else error$ "Edges to connect to nonexistent nodes! " ++ show (Set.toList diff)
   
 where
  nodes = map hnode parsednodes
  vertices = zip nums nodes
  edges = concat (map (\ nd -> map (num (hnode nd), , ()) $ out_edges nd)
		      parsednodes)
  nums      = map num nodes
  all_edges = Set.fromList$ concat$ map (\ (a,b,_) -> [a,b]) edges
  diff = Set.difference all_edges (Set.fromList nums)



----------------------------------------------------------------------------------------------------
-- Testing: Or at least some miscellaneous unit tests.
----------------------------------------------------------------------------------------------------

runPr prs str = print (run prs str)

t1 = run prop$ "name=foo;"
t2 = run props$ "name=foo; direction=01;" -- No spcs at start end
t3 = run props$ "name=foo;" 
t4 = run props$ "name=foo; blah=baz; " 

l1 = "% HARCHNODE name=blah; direction=01;\n"
--t5 = runPr debug$ l1
--t5 = runPr nodeHeader l1
l2 = " 0 1 2 "
t6 = run numbers$ l2
foo = do whitespc; newline
bar = do char '\n'
t7 = run foo "   \n"
t8 = run foo "\n"
t9 = run bar "\n"
t11 = runPr harchnode$ l1 ++ l2 ++ "\n"

test_readharch = 
  testSet "ReadHarch" $
  [
    testCase "" "simple parse test 1"$ ("name","foo")                      ~=? t1
  , testCase "" "simple parse test 2"$ [("name","foo"),("direction","01")] ~=? t2
  , testCase "" "simple parse test 3"$ [("name","foo")]                    ~=? t3 
  , testCase "" "simple parse test 4"$ [("name","foo"),("blah","baz")]     ~=? t4 
  , testCase "" "parse numbers"$ [0,1,2]     ~=? t6
  , testCase "" "whitespace newline 1"$ '\n' ~=? t7
  , testCase "" "whitespace newline 2"$ '\n' ~=? t8
  , testCase "" "whitespace newline 3"$ '\n' ~=? t9
  , testCase "" "expected parse error"$ TestCase $ do
      CE.catch (do t11; assertFailure "Parse of incorrect syntax must return an error")
               (\ (e :: CE.SomeException) -> return ())

  ]


----------------------------------------------------------------------------------------------------
