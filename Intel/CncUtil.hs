{-# LANGUAGE FlexibleInstances
  , BangPatterns
  , MagicHash 
  , ScopedTypeVariables
  , TypeFamilies 
  , UndecidableInstances
  , OverlappingInstances
  , MultiParamTypeClasses
  , FunctionalDependencies
  #-}
{-# OPTIONS_HADDOCK hide #-}
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

-- |An internal utility module that supports the CnC implementations.
#ifndef INCLUDEMETHOD
module Intel.CncUtil (
		      foldRange, for_, splitN, splitInclusiveRange, forkJoin, 
		      doTrials, FitInWord (..), 
		      GMapKey (..), 
		      Hashable (..),
		      (!),
		      testCase,
		      tests,

                      MutableMap, newMutableMap, assureMvar, mmToList,
		      HotVar, newHotVar, readHotVar, writeHotVar, modifyHotVar, modifyHotVar_, 
		      hotVarTransaction, readHotVarRaw, writeHotVarRaw,

                      ChoosePairRepr, 
                      ChooseRepr, 

		      )
where
#else
#warning "Loading CncUtil.hs through include method..."
#endif

import GHC.Conc
import Control.Concurrent
import Data.Time.Clock -- Not in 6.10
import qualified Data.Map as DM
import qualified Data.IntMap as DI
import qualified Data.List as L
import Prelude hiding (lookup)
import Data.Char (ord,chr)
import Data.Word
import Data.Int
import Data.Bits
import Data.IORef
import qualified Data.HashTable as HT
import Debug.Trace

import Test.HUnit
-- import Test.QuickCheck (quickCheck, (==>))

--------------------------------------------------------------------------------
-- Miscellaneous Utilities
--------------------------------------------------------------------------------

-- |A simple loop construct to use if you don't trust rewrite based deforestation.
-- Usage foldRange start end acc, where start is inclusive, end uninclusive.
{-# INLINE foldRange #-}
foldRange start end acc fn = loop start acc
 where
  loop !i !acc
    | i == end = acc
    | otherwise = loop (i+1) (fn acc i)

-- |My own forM, again, less trusting of optimizations.
-- Inclusive start, exclusive end.
{-# INLINE for_ #-}
for_ start end fn | start > end = error "for_: start is greater than end"
for_ start end fn = loop start 
 where 
  loop !i | i == end  = return ()
	  | otherwise = do fn i; loop (i+1) 

-- |Split a list into N pieces (not evenly sized if N does not divide
-- the length of the list).
splitN :: Int -> [a] -> [[a]]
splitN n ls | n <= 0 = error "Cannot split list by a factor of 0"
splitN n ls = loop n ls
  where 
    sz = length ls `quot` n
    loop 1 ls = [ls]
    loop n ls = hd : loop (n-1) tl
       where (hd,tl) = splitAt sz ls

-- Similar to splitN but for a (start,end) range not an actual list.
-- The first segment gets the extras and the rest are evenly sized:
-- splitInclusiveRange pieces (start,end) = 
--   (start, start + portion - 1 + remain) : map fn [1 .. pieces-1]
--  where 	
--    len = end - start + 1 -- inclusive [start,end]
--    (portion, remain) = len `quotRem` pieces
--    fn i = let nextstart = start + i * portion + remain
--           in (nextstart, nextstart + portion - 1) 

-- Instead of having one oversized piece, spread the remainder one per
-- segment:
{-# INLINE splitInclusiveRange #-}
splitInclusiveRange pieces (start,end) = 
  map largepiece [0..remain-1] ++ 
  map smallpiece [remain..pieces-1]
 where 	
   len = end - start + 1 -- inclusive [start,end]
   (portion, remain) = len `quotRem` pieces
   largepiece i = 
       let offset = start + (i * (portion + 1))
       in (offset, offset + portion)
   smallpiece i = 
       let offset = start + (i * portion) + remain
       in (offset, offset + portion - 1)


-- |Run IO threads in parallel and wait till they're done.
forkJoin actions = 
-- I'm amazed this is not built-in.
    do joiner <- newChan 
       mapM (\a -> forkIO (do a; writeChan joiner ())) actions
       mapM_ (\_ -> readChan joiner)  actions
       return ()

t = forkJoin [putStrLn "foo", putStrLn "bar", putStrLn "baz"]


-- |Run a test and time it.
doTrials trials mnd = 
  sequence_ $ take trials $ repeat $ 
    do putStrLn "------------------------------------------------------------"
       strt <- getCurrentTime
       --start <- getCPUTime
       mnd
       --end <- getCPUTime
       end  <- getCurrentTime
       let diff = (diffUTCTime end strt)
       --let diff = fromIntegral (end-start) / (10.0 ^ 12)
       putStrLn$ show diff ++  " real time consumed"


--------------------------------------------------------------------------------
-- Mutable Maps.  
--------------------------------------------------------------------------------
-- Abstract over the shared mutable data structure used
-- for item collections (in the IO-based Cnc.hs)

#ifdef HASHTABLE_TEST

-- TODO -- try it with a global lock to make it safe.
type MutableMap a b = HashTable a (MVar b)
newMutableMap :: (Eq tag, Hashable tag) => IO (MutableMap tag b)
newMutableMap = HT.new (==) hash
assureMvar col tag = 
  do mayb <- HT.lookup col tag
     case mayb of 
         Nothing -> do mvar <- newEmptyMVar
		       HT.insert col tag mvar
		       return mvar
	 Just mvar -> return mvar
mmToList = HT.toList
#warning "Enabling HashTable item collections.  These are not truly thread safe (yet)."

#else 
#ifdef USE_GMAP
#warning "Using experimental indexed type family GMap implementation..."
-- Trying to use GMaps:
type MutableMap a b = IORef (GMap a (MVar b))
newMutableMap :: (GMapKey tag) => IO (MutableMap tag b)
newMutableMap = newIORef empty
assureMvar col tag = 
  do map <- readIORef col
     case lookup tag map of 
         Nothing -> do mvar <- newEmptyMVar
		       atomicModifyIORef col 
			  (\mp -> 
			   let altered = alter 
			                  (\mv -> 
					    case mv of
					     Nothing -> Just mvar
					     Just mv -> Just mv)
			                  tag mp 
			   -- Might be able to optimize this somehow...
			   in (altered, (!) altered tag))
	 Just mvar -> return mvar
mmToList col = 
    do map <- readIORef col 
       return (toList map)
#else
-- A Data.Map based version:
-- Can probably get rid of this once we build a little confidence with GMap:
type MutableMap a b = IORef (DM.Map a (MVar b))
newMutableMap :: (Ord tag) => IO (MutableMap tag b)
newMutableMap = newIORef DM.empty
assureMvar col tag = 
  do map <- readIORef col
     case DM.lookup tag map of 
         Nothing -> do mvar <- newEmptyMVar
		       atomicModifyIORef col 
			  (\mp -> 
			   let altered = DM.alter 
			                  (\mv -> 
					    case mv of
					     Nothing -> Just mvar
					     Just mv -> Just mv)
			                  tag mp 
			   -- Might be able to optimize this somehow...
			   in (altered, (DM.!) altered tag))
	 Just mvar -> return mvar
mmToList col = 
    do map <- readIORef col 
       return (DM.toList map)
#endif
#endif

------------------------------------------------------------
-- Hot Atomic Words operations
------------------------------------------------------------

-- In this library we abuse individual words of memory with many
-- concurrent, atomic operations.  In Haskell, there are three choices
-- for these: IORef, MVars, and STVars.

-- We want to experiment with all three of these. 

#ifndef HOTVAR
#define HOTVAR 3
#endif
newHotVar     :: a -> IO (HotVar a)
modifyHotVar  :: HotVar a -> (a -> (a,b)) -> IO b
modifyHotVar_ :: HotVar a -> (a -> a) -> IO ()

#if HOTVAR == 1
type HotVar a = IORef a
newHotVar     = newIORef
modifyHotVar  = atomicModifyIORef
modifyHotVar_ v fn = atomicModifyIORef v (\a -> (fn a, ()))
readHotVar    = readIORef
writeHotVar   = writeIORef
instance Show (IORef a) where 
  show ref = "<ioref>"

hotVarTransaction = id
readHotVarRaw  = readHotVar
writeHotVarRaw = writeHotVar

#elif HOTVAR == 2 
#warning "Using MVars for hot atomic variables."
-- This uses MVars that are always full with *something*
type HotVar a = MVar a
newHotVar   x = do v <- newMVar; putMVar v x; return v
modifyHotVar  v fn = modifyMVar  v (return . fn)
modifyHotVar_ v fn = modifyMVar_ v (return . fn)
readHotVar    = readMVar
writeHotVar v x = do swapMVar v x; return ()
instance Show (MVar a) where 
  show ref = "<mvar>"

hotVarTransaction = id
readHotVarRaw  = readHotVar
writeHotVarRaw = writeHotVar


#elif HOTVAR == 3
#warning "Using TVars for hot atomic variables."
-- Simon Marlow said he saw better scaling with TVars (surprise to me):
type HotVar a = TVar a
newHotVar = newTVarIO
modifyHotVar  tv fn = atomically (do x <- readTVar tv 
				     let (x2,b) = fn x
				     writeTVar tv x2
				     return b)
modifyHotVar_ tv fn = atomically (do x <- readTVar tv; writeTVar tv (fn x))
readHotVar x = atomically $ readTVar x
writeHotVar v x = atomically $ writeTVar v x
instance Show (TVar a) where 
  show ref = "<tvar>"

hotVarTransaction = atomically
readHotVarRaw  = readTVar
writeHotVarRaw = writeTVar

#endif

instance Show (IO a) where 
  show ref = "<io>"


--------------------------------------------------------------------------------
-- Class of types which are hashable.
--------------------------------------------------------------------------------

-- TODO: Might as well replace this by the Data.Hash module on cabal.

class Hashable a where
    hash :: a -> Int32

instance Hashable Bool where
    hash True  = 1
    hash False = 0

instance Hashable Int where
    hash = HT.hashInt
instance Hashable Char where
    hash = HT.hashInt . fromEnum 
instance Hashable Word16 where
    hash = HT.hashInt . fromIntegral
--instance Hashable String where -- Needs -XTypeSynonymInstances 
instance Hashable [Char] where
    hash = HT.hashString
instance (Hashable a, Hashable b) => Hashable (a,b) where 
    hash (a,b) = hash a + hash b

instance Hashable a => Hashable [a] where
    hash []    = 0 
    hash (h:t) = hash h + hash t

-- Needs -fallow-undecidable-instances:
-- instance Integral t => Hashable t where
--     hash n = hashInt (fromInteger (toInteger n))
-- instance Enum a => Hashable a where
--     hash = hashInt . fromEnum 


--------------------------------------------------------------------------------
-- Class of types that fit in a machine word.
--------------------------------------------------------------------------------

-- |All datatypes that can be packed into a single word, including
-- scalars and some tuple types.
class FitInWord v where 
  toWord   :: v -> Word
  fromWord :: Word -> v

intToWord :: Int -> Word
intToWord = fromIntegral

wordToInt :: Word -> Int
wordToInt = fromIntegral 

instance FitInWord Char where
  toWord   = intToWord . ord
  fromWord = chr . wordToInt

instance FitInWord Int where
  toWord   = fromIntegral
  fromWord = fromIntegral

instance FitInWord Int16 where
  toWord   = fromIntegral
  fromWord = fromIntegral

instance FitInWord Int8 where
  toWord   = fromIntegral
  fromWord = fromIntegral

instance FitInWord Word8 where
  toWord   = fromIntegral
  fromWord = fromIntegral

instance FitInWord Word16 where
  toWord   = fromIntegral
  fromWord = fromIntegral


#ifdef x86_64_HOST_ARCH
instance FitInWord Int64 where
  toWord   = fromIntegral
  fromWord = fromIntegral
instance FitInWord Word64 where
  toWord   = fromIntegral
  fromWord = fromIntegral
#endif

-- Pairs can fit in words too!
-- FIXME TODO: Use some code generation method to generate instances for all
-- combinations of small words/ints that fit in a machine word (a lot).
instance FitInWord (Word16,Word16) where
  toWord (a,b) = shiftL (fromIntegral a) 16 + (fromIntegral b)
  fromWord n = (fromIntegral$ shiftR n 16, 
		fromIntegral$ n .&. 0xFFFF)

instance FitInWord (Int16,Int16) where
  toWord (a,b) = shiftL (fromIntegral a) 16 + (fromIntegral b)
  fromWord n = (fromIntegral$ shiftR n 16, 
		fromIntegral$ n .&. 0xFFFF)


--------------------------------------------------------------------------------
-- A better representation for pair keys
--------------------------------------------------------------------------------

-- Now we wish to define optimized instances of GMapKey for
-- pairs of items that fit within a word.
-- The following answers Ryan Newton's question

-- Define our own product type, to avoid overlapping instances with the
-- general GMapKey for pairs
-- It's a newtype: it has no run-time overhead
newtype OptimalPair a b = OptimalPair (a,b)
            deriving (Eq,Ord,Show)
--  deriving instance MonadState Int Foo

--instance (Ord (a,b)) => Ord (OptimalPair a b) where 
--  compare (OptimalPair t) = compare t

-- Auxiliary class to choose the appropriate pair
class ChoosePairRepr a b pr | a b -> pr where
   choose_pair  :: (a,b) -> pr
   choosen_pair :: pr -> (a,b)


instance ChoosePairRepr Int16 Int16 (OptimalPair Int16 Int16) where
   choose_pair = OptimalPair
   choosen_pair (OptimalPair p) = p

-- Choose a generic pair for all other pairs of values
instance pr ~ (a,b) => ChoosePairRepr a b pr where
   choose_pair   = id
   choosen_pair  = id


--prlookup = GM.lookup . choosen_pair -- monomorphism
prlookup x = lookup (choosen_pair x)


#if 0
-- A specific instance is chosen
test1_choosepair = 
       let m = empty in
       (m, lookup (choose_pair (1::Int16,2::Int16)) m)
-- Nothing

test2_choosepair = 
       let m = empty in
       (m, lookup (choose_pair (1::Int64,2::Int64)) m)

#else 
test1_choosepair = 
       let m = empty in
       (m, lookup (pack_repr (1::Int16,2::Int16)) m)

test2_choosepair = 
       let m = empty in
       (m, lookup (pack_repr (1::Int64,2::Int64)) m)

#endif

--------------------------------------------------------------------------------
-- Testing a more ambitious option:
--------------------------------------------------------------------------------

--newtype OptimalRepr t = OptimalRepr t deriving (Eq,Ord,Show)

-- It could dispatch to one of these:
-- Only packed would FitInWord
--newtype NormalRepr t = NormalRepr t deriving (Eq,Ord,Show)
newtype OrdOnlyRepr t = OrdOnlyRepr t deriving (Eq,Ord,Show)

newtype PackedRepr t = PackedRepr t deriving (Eq,Ord,Show)

-- Auxiliary class to choose the appropriate pair
class ChooseRepr a b | a -> b where
   pack_repr  :: a -> b
   unpack_repr :: b -> a

instance ChooseRepr (Int16,Int16) (PackedRepr (Int16,Int16)) where
   pack_repr = PackedRepr
   unpack_repr (PackedRepr p) = p

--instance (Ord a, b ~ a) => ChooseRepr a (b) where
instance (b ~ a) => ChooseRepr a (b) where
   pack_repr   = id
   unpack_repr  = id


-- CONFLICT IN FUNCTIONAL DEPS HERE:
-- Otherwise fall through to a normal representation:
-- --instance ChooseRepr a (NormalRepr b) where
-- instance (Ord b, Ord a, b ~ a) => ChooseRepr a (OrdOnlyRepr b) where
--    pack_repr = OrdOnlyRepr
--    unpack_repr (OrdOnlyRepr p) = p



--------------------------------------------------------------------------------
-- Types that are simplifiable to other types
--------------------------------------------------------------------------------

-- Same problem with overlaps:
{-
class Simplifyable a b where 
  simplify   :: a -> b
  complicate :: b -> a

instance Simplifyable (a,b,c) (a,(b,c)) where 
  simplify    (a,b,c)  = (a,(b,c))
  complicate (a,(b,c)) = (a,b,c)

instance (Simplifyable a b, GMapKey a) => GMapKey b where
  data GMap b v           = GMapSmpl (GMap b v) deriving Show
  empty                   = GMapSmpl empty
  lookup k    (GMapSmpl m) = lookup (simplify k) m
  insert k v  (GMapSmpl m) = GMapSmpl (insert (simplify k) v m)
  alter  fn k (GMapSmpl m) = GMapSmpl (alter fn (simplify k) m)
  toList      (GMapSmpl m) = map (\ (i,v) -> (complicate i, v)) $ 
			         toList m
-}
  
--------------------------------------------------------------------------------
-- ADT definition for generic Maps:
-- TODO: Factor into a separate module:
--------------------------------------------------------------------------------

-- |Class for generic map key types.  By using indexed type families,
-- each key type may correspond to a different data structure that
-- implements it.
class (Ord k, Eq k, Show k) => GMapKey k where
  data GMap k :: * -> *
  empty       :: GMap k v
  lookup      :: k -> GMap k v -> Maybe v
  insert      :: k -> v -> GMap k v -> GMap k v
  alter       :: (Maybe a -> Maybe a) -> k -> GMap k a -> GMap k a
  toList      :: GMap k a -> [(k,a)]

--------------------------------------------------------------------------------

#if 0
instance (Show a, Show b, Ord a, Ord b, FitInWord (a,b)) 
         => GMapKey (OptimalPair a b) where
 data GMap (OptimalPair a b) v = GMapOP (DI.IntMap v) deriving Show
 empty = GMapOP DI.empty
 lookup (OptimalPair k) (GMapOP m)  = DI.lookup (fromIntegral$ toWord k) m
 insert (OptimalPair k) v  (GMapOP m) = GMapOP (DI.insert (wordToInt$ toWord k) v m)
 alter  fn (OptimalPair k) (GMapOP m) = GMapOP (DI.alter fn (wordToInt$ toWord k) m)
 toList      (GMapOP m) = map (\ (i,v) -> (OptimalPair$ fromWord$ intToWord i, v)) $ 
			   DI.toList m
#else
instance (Show a, Show b, Ord a, Ord b, FitInWord (a,b)) 
         => GMapKey (PackedRepr (a,b)) where
 data GMap (PackedRepr (a,b)) v = GMapPR (DI.IntMap v) deriving Show
 empty = GMapPR DI.empty
 lookup (PackedRepr k) (GMapPR m) = DI.lookup (fromIntegral$ toWord k) m
 insert (PackedRepr k) v  (GMapPR m) = GMapPR (DI.insert (wordToInt$ toWord k) v m)
 alter  fn (PackedRepr k) (GMapPR m) = GMapPR (DI.alter fn (wordToInt$ toWord k) m)
 toList      (GMapPR m) = map (\ (i,v) -> (PackedRepr$ fromWord$ intToWord i, v)) $ 
			   DI.toList m
#endif


-- What problems was I running into here:
-- It's hard to avoid conflicting instances, for example with the tuple instance.
-- I think I may need a NotFitInWord class constraint..
#if 0
instance FitInWord t => GMapKey t where
  data GMap t v           = GMapInt (DI.IntMap v) deriving Show
  --empty                   = trace "\n <<<<< FitInWord Gmap... >>>>\n"$ GMapInt DI.empty
  empty                   = GMapInt DI.empty
  lookup k    (GMapInt m) = DI.lookup (wordToInt$ toWord k) m
  insert k v  (GMapInt m) = GMapInt (DI.insert (wordToInt$ toWord k) v m)
  alter  fn k (GMapInt m) = GMapInt (DI.alter fn (wordToInt$ toWord k) m)
  toList      (GMapInt m) = map (\ (i,v) -> (fromWord$ intToWord i, v)) $ 
			    DI.toList m

#else

-- Unit and Bool can have specialized implementations, but because
-- they also "FitInWord", these result in conflicts.
------------------------------------------------------------
instance GMapKey () where
  data GMap () v           = GMapUnit (Maybe v)
  empty                    = GMapUnit Nothing
  lookup ()   (GMapUnit v) = v
  insert () v (GMapUnit _) = GMapUnit $ Just v
  alter fn () (GMapUnit v) = GMapUnit $ fn v
  toList (GMapUnit Nothing) = []
  toList (GMapUnit (Just v)) = [((),v)]
instance GMapKey Bool where
  data GMap Bool v              = GMapBool (Maybe v) (Maybe v)
  empty                       = GMapBool Nothing Nothing
  lookup True  (GMapBool v _) = v
  lookup False (GMapBool _ v) = v
  insert True v  (GMapBool a b) = GMapBool (Just v) b
  insert False v (GMapBool a b) = GMapBool a (Just v)
  alter fn True  (GMapBool a b) = GMapBool (fn a) b
  alter fn False (GMapBool a b) = GMapBool a (fn b)
  toList (GMapBool Nothing Nothing)   = []
  toList (GMapBool (Just a) Nothing)  = [(True,a)]
  toList (GMapBool Nothing (Just b))  = [(False,b)]
  toList (GMapBool (Just a) (Just b)) = [(True,a),(False,b)]
------------------------------------------------------------


------------------------------------------------------------
-- All keys that can be represented by INTS:
------------------------------------------------------------

-- GMaps on Int keys become Data.IntMaps:
instance GMapKey Int where
  data GMap Int v         = GMapInt (DI.IntMap v) deriving Show
  empty                   = GMapInt DI.empty
  lookup k    (GMapInt m) = DI.lookup k m
  insert k v  (GMapInt m) = GMapInt (DI.insert k v m)
  alter  fn k (GMapInt m) = GMapInt (DI.alter fn k m)
  toList      (GMapInt m) = DI.toList m

-- Then other scalar keys can be converted to Ints:
-- CODE DUPLICATION
instance GMapKey Char where
  data GMap Char v         = GMapChar (GMap Int v) deriving Show
  empty                    = GMapChar empty
  lookup k (GMapChar m)    = lookup (ord k) m
  insert k v (GMapChar m)  = GMapChar (insert (ord k) v m)
  alter  fn k (GMapChar m) = GMapChar (alter fn (ord k) m)
  toList      (GMapChar m) = map (\ (i,v) -> (chr i,v)) (toList m)

instance GMapKey Word8 where
  data GMap Word8 v        = GMapWord8 (GMap Int v) deriving Show
  empty                    = GMapWord8 empty
  lookup k (GMapWord8 m)    = lookup (fromIntegral k) m
  insert k v (GMapWord8 m)  = GMapWord8 (insert (fromIntegral k) v m)
  alter  fn k (GMapWord8 m) = GMapWord8 (alter fn (fromIntegral k) m)
  toList      (GMapWord8 m) = map (\ (i,v) -> (fromIntegral i,v)) (toList m)

instance GMapKey Word16 where
  data GMap Word16 v         = GMapWord16 (GMap Int v) deriving Show
  empty                    = GMapWord16 empty
  lookup k (GMapWord16 m)    = lookup (fromIntegral k) m
  insert k v (GMapWord16 m)  = GMapWord16 (insert (fromIntegral k) v m)
  alter  fn k (GMapWord16 m) = GMapWord16 (alter fn (fromIntegral k) m)
  toList      (GMapWord16 m) = map (\ (i,v) -> (fromIntegral i,v)) (toList m)

instance GMapKey Word where
  data GMap Word v        = GMapWord (GMap Int v) deriving Show
  empty                    = GMapWord empty
  lookup k (GMapWord m)    = lookup (fromIntegral k) m
  insert k v (GMapWord m)  = GMapWord (insert (fromIntegral k) v m)
  alter  fn k (GMapWord m) = GMapWord (alter fn (fromIntegral k) m)
  toList      (GMapWord m) = map (\ (i,v) -> (fromIntegral i,v)) (toList m)

instance GMapKey Int8 where
  data GMap Int8 v        = GMapInt8 (GMap Int v) deriving Show
  empty                    = GMapInt8 empty
  lookup k (GMapInt8 m)    = lookup (fromIntegral k) m
  insert k v (GMapInt8 m)  = GMapInt8 (insert (fromIntegral k) v m)
  alter  fn k (GMapInt8 m) = GMapInt8 (alter fn (fromIntegral k) m)
  toList      (GMapInt8 m) = map (\ (i,v) -> (fromIntegral i,v)) (toList m)

instance GMapKey Int16 where
  data GMap Int16 v         = GMapInt16 (GMap Int v) deriving Show
  empty                    = GMapInt16 empty
  lookup k (GMapInt16 m)    = lookup (fromIntegral k) m
  insert k v (GMapInt16 m)  = GMapInt16 (insert (fromIntegral k) v m)
  alter  fn k (GMapInt16 m) = GMapInt16 (alter fn (fromIntegral k) m)
  toList      (GMapInt16 m) = map (\ (i,v) -> (fromIntegral i,v)) (toList m)

-- TODO: Int64 + Word64

-- Can't get past the "Conflicting family instance declarations"
-- instance GMapKey (Int16, Int16) where
#if 0
  data GMap (Int16,Int16) v         = GMapInt16Pair (GMap Int v) deriving Show
  empty                     = trace "<<< Constructing Double-Int16 GMAP (Intmap) >>>> " $ 
			      GMapInt16Pair empty
  lookup k (GMapInt16Pair m)    = lookup (fromIntegral k) m
  insert k v (GMapInt16Pair m)  = GMapInt16Pair (insert (fromIntegral k) v m)
  alter  fn k (GMapInt16Pair m) = GMapInt16Pair (alter fn (fromIntegral k) m)
  toList      (GMapInt16Pair m) = map (\ (i,v) -> (fromIntegral i,v)) (toList m)
#endif

#endif


--------------------------------------------------------------------------------

-- |GMaps over pairs are implemented by nested GMaps.
instance (GMapKey a, GMapKey b) => GMapKey (a, b) where
  data GMap (a, b) v            = GMapPair (GMap a (GMap b v))
  empty		                = --trace "CONSTRUCTED GMAP USING NESTED MAPS!"$ 
                                  GMapPair empty
  lookup (a, b) (GMapPair gm)   = lookup a gm >>= lookup b 
  insert (a, b) v (GMapPair gm) = GMapPair $ case lookup a gm of
				    Nothing  -> insert a (insert b v empty) gm
				    Just gm2 -> insert a (insert b v gm2  ) gm
  alter fn (a, b) (GMapPair gm) = GMapPair $ alter newfun a gm
     where 
       newfun entry =
	   case entry of 
	    Nothing -> case fn Nothing of 
	                Nothing -> Nothing
	                Just v  -> Just $ insert b v empty
	    Just m -> Just$ alter fn b m

  toList (GMapPair gm) = L.foldl' (\ acc (a,m) -> map (\ (b,v) -> ((a,b),v)) (toList m) ++ acc) [] $ 
			 toList gm

-- |Sum types are represented by separate GMaps for the separate variants.
instance (GMapKey a, GMapKey b) => GMapKey (Either a b) where
  data GMap (Either a b) v                = GMapEither (GMap a v) (GMap b v)
  empty                                   = GMapEither empty empty
  lookup (Left  a) (GMapEither gm1  _gm2) = lookup a gm1
  lookup (Right b) (GMapEither _gm1 gm2 ) = lookup b gm2
  insert (Left  a) v (GMapEither gm1 gm2) = GMapEither (insert a v gm1) gm2
  insert (Right b) v (GMapEither gm1 gm2) = GMapEither gm1 (insert b v gm2)
  alter fn (Left  a) (GMapEither gm1 gm2) = GMapEither (alter fn a gm1) gm2
  alter fn (Right b) (GMapEither gm1 gm2) = GMapEither gm1 (alter fn b gm2)
  toList (GMapEither gm1 gm2) = 
      map (\ (a,v) -> (Left  a, v)) (toList gm1) ++ 
      map (\ (b,v) -> (Right b, v)) (toList gm2)


-- TODO: Use template haskell to generalize this strategy to other tuples:
-- For now here's a hack:
-- Could simplify to nested binary tuples....
instance (GMapKey a, GMapKey b, GMapKey c) => GMapKey (a,b,c) where
  data GMap (a,b,c) v     = GMapTriple (DM.Map (a,b,c) v) deriving Show
  empty                   = GMapTriple DM.empty
  lookup k    (GMapTriple m) = DM.lookup k m
  insert k v  (GMapTriple m) = GMapTriple (DM.insert k v m)
  alter  fn k (GMapTriple m) = GMapTriple (DM.alter fn k m)
  toList      (GMapTriple m) = DM.toList m




-- |GMaps with list indices could be treated like tuples (nested
-- maps).  Instead, we put them in a regular Data.Map.
instance (GMapKey a) => GMapKey [a] where
  data GMap [a] v         = GMapList (DM.Map [a] v) deriving Show
  empty                   = GMapList DM.empty
  lookup k    (GMapList m) = DM.lookup k m
  insert k v  (GMapList m) = GMapList (DM.insert k v m)
  alter  fn k (GMapList m) = GMapList (DM.alter fn k m)
  toList      (GMapList m) = DM.toList m
 

--------------------------------------------------------------------------------

-- We would love to just do this -- fall through for any old Ord representation:
-- instance Ord a => GMapKey a where
--   data GMap a v         = GMapOrd (DM.Map a v) deriving Show
--   empty                   = GMapOrd DM.empty
--   lookup k    (GMapOrd m) = DM.lookup k m
--   insert k v  (GMapOrd m) = GMapOrd (DM.insert k v m)
--   alter  fn k (GMapOrd m) = GMapOrd (DM.alter fn k m)
--   toList      (GMapOrd m) = DM.toList m


instance GMapKey Int64 where
  data GMap Int64 v           = GMapInt64 (DM.Map Int64 v) deriving Show
  empty                   = GMapInt64 DM.empty
  lookup k    (GMapInt64 m) = DM.lookup k m
  insert k v  (GMapInt64 m) = GMapInt64 (DM.insert k v m)
  alter  fn k (GMapInt64 m) = GMapInt64 (DM.alter fn k m)
  toList      (GMapInt64 m) = DM.toList m



--------------------------------------------------------------------------------

(!) :: (GMapKey k) => GMap k v -> k -> v
(!) m k  = 
  case lookup k m of
    Nothing -> error "GMap (!) operator failed, element was not present."
    Just x -> x


myGMap :: GMap (Int, Either Char ()) String
myGMap = insert (5, Left 'c') "(5, Left 'c')"    $
	 insert (4, Right ()) "(4, Right ())"    $
	 insert (5, Right ()) "This is the one!" $
	 insert (5, Right ()) "This is the two!" $
	 insert (6, Right ()) "(6, Right ())"    $
	 insert (5, Left 'a') "(5, Left 'a')"    $
	 empty

intMap :: GMap Int String
intMap = insert 3 "Entry 3"    $
	 insert 4 "(4, Right ())"    $
	 empty
--------------------------------------------------------------------------------
-- Experimental: trying to parameterize by both key and value type and
-- thereby use things like Judy arrays.
-- We also switch to a mutable data structure here:
--------------------------------------------------------------------------------

-- UNFINISHED:

-- A key/value pair that works inside a GMap2.
class GMapKeyVal k v where
  data GMap2 k v :: *
  empty2       :: IO (GMap2 k v)
  lookup2      :: k -> GMap2 k v -> IO (Maybe v)
  insert2      :: k -> v -> GMap2 k v -> IO ()

-- instance GMapKeyVal Int Int where
--   data GMap2 Int Int       = GMapInt2 (DI.IntMap Int) deriving Show
--   empty2                   = GMapInt2 DI.empty
--   lookup2 k   (GMapInt2 m) = DI.lookup k m
--   insert2 k v (GMapInt2 m) = GMapInt2 (DI.insert k v m)

-- instance (FitInWord k, FitInWord v) => GMapKeyVal k v where
--   data GMap2 k v           = GMapInt2 (DI.IntMap v) deriving Show
--   empty2                   = GMapInt2 DI.empty
--   lookup2 k   (GMapInt2 m) = DI.lookup (wordToInt $ toWord k) m
--   insert2 k v (GMapInt2 m) = GMapInt2 (DI.insert (wordToInt $ toWord k) v m)


-- [2010.05.19] TEMPTOGGLE uncommenting to compile on laptop:
{-
instance FitInWord t => J.JE t where
  toWord   = 
  fromWord = 

-- If we know a little more, use the Judy version:
instance (FitInWord k, J.JE v) => GMapKeyVal k v where
  data GMap2 k v           = GMapInt2 (J.JudyL v) 
  empty2  = do x <- J.new 
	       return $ GMapInt2 x
  lookup2 = 
  insert2 = 
  -- empty2                   = do x <- J.new
  -- 				return $ GMapInt2 x
  -- lookup2 k   (GMapInt2 r) = do m <- readIORef r
  -- 				return$ DI.lookup (wordToInt $ toWord k) m
  -- insert2 k v (GMapInt2 r) = modifyIORef r (DI.insert (wordToInt $ toWord k) v)
-}



-- Otherwise this is the Data.IntMap version
-- instance (FitInWord k) => GMapKeyVal k v where
--   data GMap2 k v           = GMapInt2 (IORef (DI.IntMap v)) 
--   empty2                   = do x <- newIORef DI.empty
-- 				return $ GMapInt2 x
--   lookup2 k   (GMapInt2 r) = do m <- readIORef r
-- 				return$ DI.lookup (wordToInt $ toWord k) m
--   insert2 k v (GMapInt2 r) = modifyIORef r (DI.insert (wordToInt $ toWord k) v)




test1gmap = putStrLn $ maybe "Couldn't find key!" id $ lookup (5, Right ()) myGMap
test2gmap = putStrLn $ maybe "Couldn't find key!" id $ lookup 3 intMap

-- There's a problem with quickcheck where it doesn't
-- newline-terminate the "Cases: N" report message.
testCase str io = TestLabel str $ TestCase$ do putStrLn$ "\n *** Running unit test: "++str; io; putStrLn ""

test1 = testCase "Spot check list lengths"$ assertEqual "splitN" [[1,2], [3,4,5]] (splitN 2 [1..5]) 


-- [2010.05.31] I don't have quickcheck working under 6.13.xx
-- test2 = testCase "Quickcheck splitN - varying split size"$ 
-- 	quickCheck$ (\ (n::Int) -> n>0 ==> 
-- 		     (\ (l::[Int]) -> concat (splitN n l) == l)) 

-- tests = TestList [test1, test2]

tests = TestList [test1]
