{-# LANGUAGE FlexibleInstances
  , BangPatterns
  , MagicHash 
  , ScopedTypeVariables
  , TypeFamilies 
  , UndecidableInstances
  , OverlappingInstances
  , DeriveDataTypeable
  , MultiParamTypeClasses
  , RankNTypes
  #-}
--  Note: PatternSignatures was deprecated after 6.8.
{-# OPTIONS_HADDOCK prune #-}
{-
 - Intel Concurrent Collections for Haskell
 - Copyright (c) 2010, Intel Corporation.
 -
 - This program is free software; you can redistribute it and/or modify it
 - under the terms and conditions of the GNU Lesser General Public License,
 - version 2.1, as published by the Free Software Foundation.
 -
 - This program is distributed in the hope it will be useful, but WITHOUT
 - ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 - FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 - more details.
 -
 - You should have received a copy of the GNU Lesser General Public License along with
 - this program; if not, write to the Free Software Foundation, Inc., 
 - 51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.
 -
 -}

-- #define DEBUG_HASKELL_CNC

-- This must be defined because the runtime may allow a low
-- probability of duplicating stolen work.
-- #define REPEAT_PUT_ALLOWED

-- [2010.06.13] If NOT memoizing... need to turn on REPEAT_PUT_ALLOWED for replay implementations.

{-|
  This module implements the Intel Concurrent Collections (CnC) programming model.
  The variations of this module ("Intel.Cnc3", "Intel.Cnc5", "Intel.Cnc6", and "Intel.Cnc8")
  each implement the same programming model using different runtime schedulers.
  All of them internally use the IO monad but expose a pure interface.
  (The module "Intel.CncPure" is an alternative implementation that
  exposes the same interface as this module but is internally pure.)


  CnC is a data-flow like deterministic parallel programming model.
  To use it, one constructs a /CnC graph/ of computation steps.  
  Steps are arbitrary Haskell functions (which may themselves expose
  parallelism through 'GHC.Conc.par').
  Edges in the graph are control and data relationships, 
  implemented by  /tag/ and /item/ collections respectively.

  A brief introduction to CnC using this module can be found at <http://software.intel.com/en-us/blogs/2010/05/27/announcing-intel-concurrent-collections-for-haskell-01/>.
  General documentation on the CnC model can be found at 
   <http://software.intel.com/en-us/articles/intel-concurrent-collections-for-cc/>.

 -}
#ifndef INCLUDEMETHOD
module MODNAME (
		  Step, TagCol, ItemCol,
                  -- |The @GraphCode@ monad represents
                  -- computations for constructing CnC graphs. 
		  GraphCode,
		  -- |The @StepCode@ monad represents computations 
                  -- running inside individual nodes of CnC graphs (in parallel).		      
		  StepCode(..), 
		  newItemCol, newTagCol, prescribe, prescribeNT,
		  putt, put, get, 
		  initialize, finalize,
                  runGraph, 

                  itemsToList,
		  stepPutStr, cncPutStr, cncVariant,
		  stepUnsafeIO, cncUnsafeIO,

                  -- Undocumented experimental features:
                  Item, newItem, readItem, putItem,
                  cncFor, cncFor2D, graphInStep,

                  tests, 
-- * Example Program
{- |

Below is a simple program that prints \"Hello World 99\".  Item
collections are indexed by string tags (keys).  The CnC graph consists
of one node.

@
myStep items tag =
  do word1 <- 'get' items \"left\"
     word2 <- 'get' items \"right\"
     'put' items \"result\" (word1 ++ word2 ++ show tag)

cncGraph = 
  do tags  <- 'newTagCol'
     items <- 'newItemCol'
     'prescribe' tags (myStep items)
     'initialize' $
        do 'put' items \"left\"  \"hello \"
           'put' items \"right\" \"world \"
           'putt' tags 99
     'finalize' $ 
        do 'get' items \"result\"

main = putStrLn (runGraph cncGraph)
@

 -}
		 )
where
#else
#warning "Loading imperative, IO-based CnC implementation."
#endif

{- 

   This is an implementation of CnC based on the IO monad.  The
   exposed interface is the same as the pure implementation, and CnC
   computations remain pure.

  This version formulates steps as side-effecting functions on tables
  of MVars (item collections).

  If we had concurrent hashtables, that would be one option.  Instead
  we need to use immutable maps stored inside a mutable reference.
  (Course lock to protect hash tables would also be a, probably
  undesirable, option.)  
-}

import qualified Data.Set as Set
import qualified Data.HashTable as HT
import qualified Data.Map as Map
import qualified System.Posix
import qualified Data.Sequence as Seq
import qualified Data.Array as Array

import qualified System.Random as Random

import Data.Int
import Data.IORef
import Data.Word
import Data.Typeable
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Cont as C
import qualified  Control.Monad.State.Strict as S 

import qualified Control.Monad.Reader as R

--import qualified  Control.Monad.State.Lazy as S 
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent
--import Control.Exception
import Control.Exception.Extensible
import System.IO.Unsafe
import GHC.IO
import GHC.Conc
import GHC.Prim
import GHC.Exts 

import Test.HUnit

-- Inline the utility library as well:
#ifndef INCLUDEMETHOD
import Intel.CncUtil as GM hiding (tests)
#else
#include "CncUtil.hs"
#endif

------------------------------------------------------------
-- Configuration Toggles:

#ifdef MEMOIZE
#warning "Memoization enabled"
memoize = True
#else
memoize = False
#endif


#ifdef HASHTABLE_TEST
#define ITEMPREREQS (Eq tag, Ord tag, Hashable tag, Show tag)
#else 
#ifdef USE_GMAP
-- #define ITEMPREREQS (Ord tag, Eq tag, GMapKey tag, Show tag)
#define ITEMPREREQS (GMapKey tag)
#else
#define ITEMPREREQS (Eq tag, Ord tag, Show tag)
#endif
#endif

------------------------------------------------------------
-- Type signatures for the primary API operations:

-- |Attach a computation step to a supply of control tags.  This adds a new node in the computation graph.
prescribe   :: TagCol tag -> Step tag -> GraphCode ()

-- |Convenience: Generate a new tag collection and prescribe several steps in one call.
prescribeNT :: [Step tag] -> GraphCode (TagCol tag)
prescribeNT ls = do nt <- newTagCol; forM_ ls (prescribe nt); return nt

-- |Put-Tag.  Push a control tag out into the computation graph.
#ifdef MEMOIZE
putt :: Ord tag         => TagCol  tag     -> tag         -> StepCode ()
#else
putt ::                    TagCol  tag     -> tag         -> StepCode ()
#endif

-- |Put an item.  Subsequently, any steps waiting on the item may subsequently execute.
put  :: ITEMPREREQS     => ItemCol tag val -> tag -> val  -> StepCode ()
-- |Get an item.  Synchronous read-data operation.
get  :: ITEMPREREQS     => ItemCol tag val -> tag         -> StepCode val

-- |Run an initial step which populates the CnC graph with input tags and items.
--  Presently only a single initialize is allowed within a graph execution.
initialize :: StepCode () -> GraphCode ()
-- |Run a final step which collects outputs of the graph that are of interest to the larger application.
--  Presently only a single finalize is allowed within a graph execution.
finalize   :: StepCode a -> GraphCode a
--finalize   :: StepCode a -> GraphCode ()

-- |Construct a new tag collection.
newTagCol  :: GraphCode (TagCol tag)
-- |Construct a new item collection.
newItemCol :: ITEMPREREQS => GraphCode (ItemCol tag val)

-- |Convert an entire item collection into an association list.  In
-- general, this can only be done from the 'finalize' step and
-- requires selecting a runtime scheduler which supports /quiescence/, that is,
-- a scheduler that waits for all active steps to complete before executing 'finalize'.
-- (Currently, all schedulers but version 3 support quiescence.)
itemsToList :: ITEMPREREQS => ItemCol tag b -> StepCode [(tag,b)]

-- |Steps are functions that take a single 'tag' as input and perform
-- a computation in the "StepCode" monad, which may perform "put"s and "get"s.
type Step     a   = a -> StepCode ()


--------------------------------------------------------------------------------
--                             Implementation                                 --
--------------------------------------------------------------------------------

-- These 'new' functions need an argument if we don't want to run in
-- to the monomorphism restriction (-fno-monomorphism-restriction)
#ifndef SUPPRESS_newItemCol
newItemCol = GRAPHLIFT newMutableMap
#endif
#ifndef SUPPRESS_newTagCol
newTagCol = do ref1 <- GRAPHLIFT newIORef Set.empty
	       ref2 <- GRAPHLIFT newIORef []
	       return (ref1, ref2)
#endif

-- Putting items: If it's not there we add the mvar ourselves.
-- 
-- [2010.02.15] Made this strict in the item.  Otherwise we could
-- unintentionally delay work until the after the (parallel) CnC
-- evaluation and do it in serial!
#ifndef SUPPRESS_put
put col tag (!item) = 
    do mvar <- STEPLIFT assureMvar col tag 
       bool <- STEPLIFT tryPutMVar mvar item
#ifdef REPEAT_PUT_ALLOWED
       return ()
#else
       if not bool then error ("Already an item with tag " ++ show tag) else return ()
#endif
#endif

-- A tag collection stores the list of subscribed step collections.
-- To "prescribe", simply add it to the list:
prescribe (_set,_steps) step = 
    do steps <- GRAPHLIFT readIORef _steps
       GRAPHLIFT writeIORef _steps (step:steps)

-- This encapsulates the book-keeping necessary for a put-tag (putt).
-- It is common to all the scheduler variants below.
-- 
-- FIXME: Consider a trampoline.  Some schedulers may stack leak.
--proto_putt :: Ord a =>  ([Step a] -> a -> StepCode b) -> TagCol a -> a -> StepCode b
proto_putt action tc@(_set,_steps) tag = 
    do set   <- STEPLIFT readIORef _set
       steps <- STEPLIFT readIORef _steps
--       if memoize 
--        then 
--        else 
#ifdef MEMOIZE
       if Set.member tag set
        then return ()
        else STEPLIFT writeIORef _set (Set.insert tag set)
#else
       return ()
#endif
       action steps tag

#ifndef SUPPRESS_itemsToList
itemsToList ht = 
 do if not quiescence_support 
       then error "need to use a scheduler with quiescence support for itemsToList" 
       else return ()
    ls <- STEPLIFT mmToList ht
    foldM (\ acc (key,mvar) -> 
	   do --putStrLn "Try take mvar..."
	      val <- STEPLIFT readMVar mvar
	      --putStrLn "  Took!"
	      return $ (key,val) : acc)
	  [] ls
#endif

-- Embed StepCode in the graph construction program:
#ifndef SUPPRESS_initialize
initialize x = x
#endif

-- |Construct a CnC graph and execute it to completion.  Completion
--   is defined as the 'finalize' action having completed.
runGraph :: GraphCode a -> a
#ifndef SUPPRESS_runGraph
runGraph x = unsafePerformIO x
#endif

-- Currently these are undocumented and unofficial:
stepUnsafeIO :: IO a -> StepCode a 
cncUnsafeIO  :: IO a -> GraphCode a 
stepUnsafeIO io = STEPLIFT  io
cncUnsafeIO  io = GRAPHLIFT io

-- | Print a message within a step (unsafe side effect).
stepPutStr :: String -> StepCode ()
stepPutStr str = stepUnsafeIO (putStr str)
-- | Print a message within the graph construction code (unsafe side effect).
cncPutStr :: String -> GraphCode ()
cncPutStr  str = cncUnsafeIO  (putStr str)

-- |An informal identifier of the CnC version presently in use (for example, identifying a scheduler implementation).
cncVariant :: String
cncVariant ="io-based, scheduler " ++ show (CNC_SCHEDULER :: Int)  
#ifdef USE_GMAP
       ++ ", gmap enabled"
#else 
       ++ ", gmap disabled"
#endif
#ifdef MEMOIZE
       ++ ", memoize enabled"
#else 
       ++ ", memoize disabled"
#endif
#ifdef INCLUDEMETHOD
       ++ ", #include method"
#endif
#ifdef REPEAT_PUT_ALLOWED
       ++ ", repeated identical puts permitted"
#endif
#ifdef DEBUG_HASKELL_CNC
       ++ ", DEBUG enabled"
#endif




--------------------------------------------------------------------------------
--  Testing
--------------------------------------------------------------------------------

-- Here's a tiny program to run:
incrStep d1 (t2,d2) tag = 
 do val <- get d1 tag 
    stepPutStr ("  ("++ show tag ++") Incrementing " ++ show val ++"\n")
    put d2 tag (val + 1)
    putt t2 tag

smalltest = testCase "Small test of Cnc model under Cnc.hs" $ 
            putStrLn ("Final Result: "++ show v)
  where 
   v = runGraph g
   g = do -- First, allocate collections:
        t1 <- newTagCol
        t2 <- newTagCol
        t3 <- newTagCol
        d1 <- newItemCol
        d2 <- newItemCol
        d3 <- newItemCol

         -- Build and execute the graph:
        prescribe t1 (incrStep d1 (t2,d2))
 	prescribe t2 (incrStep d2 (t3,d3))

        -- Start things up:	 
	initialize $ do put d1 'a' 33
 			put d1 'b' 100
			putt t1 'b'
			putt t1 'a'

        let incrStep d1 (t2,d2) tag = 
	     do n <- get d1 tag
	        put d2 tag (n+1)
	        putt t2 tag

        -- Get some of the results:
	finalize $ 
	  do a <- itemsToList d1
	     b <- itemsToList d2
	     c <- itemsToList d3
	     return (a,b,c)

#ifndef INCLUDEMETHOD
tests :: Test
tests = TestList [ smalltest ]
#endif

--------------------------------------------------------------------------------
-- EXPERIMENTAL:
--------------------------------------------------------------------------------
-- This is a proposed addition for manipulating items outside of item collections.

newItem  :: StepCode (Item a)
readItem :: Item a -> StepCode a
putItem  :: Item a -> a -> StepCode ()

-- #if CNC_SCHEDULER != 3 && CNC_SCHEDULER != 5
#ifndef DEFINED_free_items
type Item a = ()
newItem  = error "newItem not implemented under this scheduler"
readItem = error "readItem not implemented under this scheduler"
putItem  = error "putItem not implemented under this scheduler"
#endif


-- Internal function. We may allow extending the graph from within a
-- step.  I'm not sure what the best name for this is.  
graphInStep :: GraphCode a -> StepCode a
#ifndef SUPPRESS_graphInStep
-- Default is to assume the monads are the same:
graphInStep x = x
#endif


#ifndef SUPPRESS_cncFor
-- | \"@cncFor start end body@\" runs @body@ in parallel over the inclusive range @[start..end]@.
-- 
-- Frequently, CnC graphs are serial within steps but parallel at the
-- level of the graph.  In contrast, 'cncFor' exposes parallelism
-- /within a step/.  Whether the body of the parallel for is doing
-- work, or just spawning work via 'putt', 'cncFor' can help
-- distribute the work more efficiently.
cncFor :: Int -> Int -> (Int -> StepCode ()) -> StepCode ()
-- Parallel for and tag-ranges can make things much more efficient for
-- a common case. 
-- 
-- It may be nice under some schedulers to use forkOnIO to explicitly
-- disseminate the ranges to processors.  This alas wouldn't work well
-- with nested cncFor loops.  But if we disencourage those and
-- explicitly provide cncFor2D etc...
-- 
cncFor start end body = 
 do ts <- graphInStep newTagCol
    --stepPutStr$ "Performing cncFor on range " ++ show (start,end) ++ "\n"
    graphInStep$ prescribe ts$ \(x,y) -> 
      do --stepPutStr$ "  Executing range segment: "++ show (x,y) ++ "\n"
         for_ x (y+1) body
    --stepPutStr$ "Desired segments "++ show (4*numCapabilities) ++ " putting first segment...\n"
    let range_segments = splitInclusiveRange (4*numCapabilities) (start,end)
    --stepPutStr$ "PUTTING RANGES "++ show (length range_segments) ++" "++ show range_segments ++"\n"
    forM_ range_segments (putt ts)
#endif

#ifndef SUPPRESS_cncFor2D
-- | A two dimensional loop.
{-# INLINE cncFor2D #-}
cncFor2D :: (Int,Int) -> (Int,Int) -> (Int -> Int -> StepCode ()) -> StepCode ()
-- In this version we don't do anything special for 2D loops:
cncFor2D (s1,s2) (e1,e2) body =
  cncFor s1 e1 $ \ i ->  
   cncFor s2 e2 (body i)

-- Stripe distribution -- only parallelize the outer loop.
-- (Tiles would be better but slightly more complex.)
-- Oddly... this doesn't do better in any scheduler on the current benchmark...
-- cncFor2D (s1,s2) (e1,e2) body =
--   cncFor s1 e1 $ \ i ->  
--    for_ s2 (e2+1) (body i)
#endif


-- instance Show (StepCode ()) where 
--   show ref = "<StepCode () action>"

--------------------------------------------------------------------------------


-- <eof> *** This file will be included into the per-scheduler implementations. *** 
