{-# LANGUAGE RecordWildCards, TupleSections, ScopedTypeVariables, DeriveFunctor #-}

----------------------------------------------------------------------------------------------------
-- Read .harch profiled/partitioned graph files.
--
-- Original Author: Ryan Newton
----------------------------------------------------------------------------------------------------

module Intel.Cnc.Spec.Passes.ReadHarch 
    -- (
    --   readHarchFile, parseHarchFile,
    --   HarchNode (..), showTreePath, 
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
import Data.Set (Set, member)
import qualified Data.Set as Set
import Data.List.Split (splitOn) -- from 'split' package 
import Data.Graph
import Debug.Trace
import Data.Function
import qualified Control.Exception as CE

import qualified  Data.Graph.Inductive as G
import qualified  Data.Graph.Inductive.Query.DFS as DFS
import Data.Graph.Inductive.Query.Monad (mapFst, mapSnd)

----------------------------------------------------------------------------------------------------

-- A graph from a Harch file is currently not quite the same datatype as a CncGraph.
-- The HarchGraph has only step collections currently.  It has no edge labels.
data HarchSpec = HarchSpec {
    hgraph :: HarchGraph, 
    htree  :: HarchTreeOrdered
}

type HarchGraph = G.Gr HarchNode ()

-- An individual node within the graph:
data HarchNode = HarchNode {
    name       :: String,             -- A mandatory field.
    properties :: [(String, String)], -- 
    num        :: Int,                -- Using numeric idenifiers for now
    nodecomments :: [String]          -- User comments or documentation.
  }
 deriving Show

-- | A datatype for Harch trees, e.g. for hierarchical decompositions.
--   Parameterized by the type of the partitions themselves.
-- 
-- This is sometimes a redundant representation because not all of the
-- intermediate nodes need to be labeled with the full contents.  In
-- fact, for HarchTreeUnordered only leaves need be labeled.  But
-- HarchTreeOrdered encodes extra information at all the intermediate
-- nodes (namely, potentially unique orderings).
data HarchTree a = HT a [HarchTree a] 
  deriving (Show, Eq, Ord, Functor)

-- A simple representation to start with is a set of Ints for each partition:
type HarchTreeUnordered = HarchTree (Set Int)

-- After topological sorting the partitions are ordered.
type HarchTreeOrdered = HarchTree [Int]

-- | Read a Harch file from disk.
readHarchFile :: String -> IO HarchSpec
readHarchFile path = 
 do file <- openFile path ReadMode 
    txt  <- hGetContents file
    return (parseHarchFile txt)

-- | Parse the contents of a Harch file stored in a string.
--parseHarchFile :: String -> (HarchGraph, HarchTreeUnordered)
parseHarchFile :: String -> HarchSpec
parseHarchFile txt = HarchSpec gr ordered
  where 
   ls = run harchfile txt
   gr = convertHarchGraph ls
   sorted = DFS.topsort gr
   part = extractPartitions ls
   ordered = fmap (\set -> filter (flip member set) sorted) part



----------------------------------------------------------------------------------------------------
-- Temporary datatypes used in parsing:
----------------------------------------------------------------------------------------------------

-- A simple datatype for parsed Harch Nodes:
data HarchNodeParse = HNP {
    hnode      :: HarchNode,
    in_edges   :: [Int],
    out_edges  :: [Int]
  }
 deriving Show

----------------------------------------------------------------------------------------------------
-- Parsec parser for .harch syntax:

-- We use METIS comment lines for two purposes.  Normal comments, and the reserved HARCHNODE lines.
-- Maybe we should have used a different delimiter, like %% for harch nodes to avoid this backtracking.
commentline :: Parser String
commentline = try $
  do whitespc; char '%'; whitespc
     notFollowedBy (string "HARCHNODE")
     str <- many (noneOf "\n")
     newline
     return str

harchfile :: Parser [HarchNodeParse]
harchfile = 
  do global_comments <- many commentline
     numbers; newline -- Skip the first line
     nodes <- many harchnode
     return$ map (\ (n, rec) -> 
		   let h = hnode rec in  rec { hnode= h{ num= n } })
	         (zip [1..] nodes)

-- Remove one property from a property list and return the remainder.
popProp :: String -> [(String, String)] -> (String, [(String, String)])
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
  do
     node_comments1 <- many commentline
     whitespc; char '%'; whitespc; string "HARCHNODE"; whitespc
     node_comments2 <- many commentline
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
	      hnode = HarchNode { name= nm, properties= rest2, num=0, 
				  nodecomments = node_comments1 ++ node_comments2 },
	      in_edges= ins, out_edges= outs
	    }

spc = oneOf " \t"
whitespc = many spc

-- Parse one property in the property list:
prop :: Parser (String,String)
prop = do w <- many1 letter
	  char '=';
	  -- The property may be the empty string:
	  p <- many (noneOf ";")
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


----------------------------------------------------------------------------------------------------
-- Conversion to partitioned format:

showTreePath :: [Int] -> String
showTreePath intls = concat$ intersperse ":" $ map show intls

-- Parse the partition information packed into each vertices' metadata
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

  -- This pulls out the tree index (e.g. "0:1:3:0") as an int list:
  extract :: HarchNode -> ([Int], HarchNode) 
  extract (nd@HarchNode{..}) = 
     let (partstr, rest) = popProp "partitions" properties in
     (map read $ filter (not . null) $ splitOn ":" partstr :: [Int],
      nd { properties= rest})

  -- Build a tree recursively:
  build :: (Set Int, [([Int], HarchNode)]) -> HarchTreeUnordered
--  build (set, paths) | Set.null set = 
  build (set, paths) = 
     -- Each group is a child partition
     let 
         still_here = filter (not . null . fst) paths -- remove those that ran out
         sorted = sortBy (compare `on` (head . fst)) still_here
	 
	 -- Group by the head-index and then chop it off.
         grouped = groupBy ((==) `on` (head . fst)) sorted
         clipped = map (map (mapFst tail)) grouped
                   		   
	 -- For each sub-partition cut down the node set.
         restricted = map (Set.intersection set) $ 
		      map (Set.fromList . map (num . snd)) clipped

         tree_children = map build $ zip restricted clipped

         -- Also sanity check those head-indexes.
	 heads = map (head . fst . head) grouped

         shownpaths = 
	     "heads " ++ show heads ++ " of paths "++ 
	     concat (intersperse "  "$ map showTreePath$ map fst paths)
         checked = 
	   (if length heads == 1
	    then trace ("WARNING: Degenerate sub-partition is equal to parent partition: "++
		       shownpaths)
	    else id) $ 
	    -- FIXME: expensive way of checking:
           (if null heads || (minimum heads == 0 &&  heads == [0..maximum heads])
	    then tree_children
	    else trace ("WARNING: Tree-indices of sub-partitions are irregular; should be consecutive range [0,N).\n"++
			"         Instead received "++ shownpaths)
		       tree_children)

     in HT set checked
     
-- Convert parsed adjacency list info into a real graph.
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
t5 = run props$ "name=; blah=; "  -- Blank properties


l1 = "% HARCHNODE name=blah; direction=01;\n"

l2 = " 0 1 2 "
t6 = run numbers$ l2
foo = do whitespc; newline
bar = do char '\n'
t7 = run foo "   \n"
t8 = run foo "\n"
t9 = run bar "\n"
t11 = runPr harchnode$ l1 ++ l2 ++ "\n"

testfile = unlines $
   ["4 3 10",
    "% graph with 4 verts 3 edges",
    "% HARCHNODE name=foo; directions=11; partitions=1; index=1;",
    "1 2 3",
    "% HARCHNODE name=bar; directions=; partitions=1:0; index=2;",
    "1",
    "% HARCHNODE name=baz1; directions=1; partitions=1:1; index=3;",
    "1 4",
    "% HARCHNODE name=baz2; directions=; partitions=1:1; index=4;",
    "1"]

t12 = runPr harchfile testfile

test_readharch = 
  testSet "ReadHarch" $
  [
    testCase "" "simple parse test 1"$ ("name","foo")                      ~=? t1
  , testCase "" "simple parse test 2"$ [("name","foo"),("direction","01")] ~=? t2
  , testCase "" "simple parse test 3"$ [("name","foo")]                    ~=? t3 
  , testCase "" "simple parse test 4"$ [("name","foo"),("blah","baz")]     ~=? t4 
  , testCase "" "simple parse test: blank props"$ [("name",""),("blah","")] ~=? t5 
  , testCase "" "parse numbers"$ [0,1,2]     ~=? t6
  , testCase "" "whitespace newline 1"$ '\n' ~=? t7
  , testCase "" "whitespace newline 2"$ '\n' ~=? t8
  , testCase "" "whitespace newline 3"$ '\n' ~=? t9
  , testCase "" "expected parse error"$ TestCase $ do
      CE.catch (do t11; assertFailure "Parse of incorrect syntax must return an error")
               (\ (e :: CE.SomeException) -> return ())

  , testCase "" "commentline 1"$ "blah blah blah" ~=? run commentline " % blah blah blah\n"
  , testCase "" "commentline 2"$ "HARCHNO blah" ~=? run commentline " % HARCHNO blah\n"
  , testCase "" "commentline 3, expect fail"$ TestCase $ do
      CE.catch (do runPr commentline " % HARCHNODE foo \n"
  		   assertFailure "Parse of HARCHNODE line must not satisfy commentline.")
               (\ (e :: CE.SomeException) -> return ())

  , testCase "" "parse complete file"$ TestCase t12
	  
  ]


----------------------------------------------------------------------------------------------------
