{-# LANGUAGE FlexibleInstances
  , BangPatterns
  , MagicHash 
  , ScopedTypeVariables
  , TypeFamilies 
  , UndecidableInstances
  , OverlappingInstances
  , MultiParamTypeClasses
  #-}
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
		      foldRange, for_, splitN, forkJoin, 
		      doTrials, FitInWord (..), 
		      GMapKey (..), 
		      (!)
		      )
where
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
import Debug.Trace


-- |A simple loop construct to use if you don't trust rewrite based deforestation.
-- Usage foldRange start end acc, where start is inclusive, end uninclusive.
foldRange start end acc fn = loop start acc
 where
  loop !i !acc
    | i == end = acc
    | otherwise = loop (i+1) (fn acc i)


-- |My own forM, again, less trusting of optimizations.
-- Inclusive start, exclusive end.
for_ start end fn = loop start 
 where 
  loop !i | i == end  = return ()
	  | otherwise = do fn i; loop (i+1) 

-- |Split a list into N pieces (not evenly sized if N does not divide
-- the length of the list).
splitN n ls = loop n ls
  where 
    sz = length ls `quot` n
    loop 1 ls = [ls]
    loop n ls = hd : loop (n-1) tl
       where (hd,tl) = splitAt sz ls

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
-- TODO: Use some code generation method to generate instances for all
-- combinations of small words/ints that fit in a machine word (a lot).
instance FitInWord (Word16,Word16) where
  toWord (a,b) = shiftL (fromIntegral a) 16 + (fromIntegral b)
  fromWord n = (fromIntegral$ shiftR n 16, 
		fromIntegral$ n .&. 0xFFFF)


--------------------------------------------------------------------------------
-- ADT definition for generic Maps:
--------------------------------------------------------------------------------

-- |Class for generic map key types.  By using indexed type families,
-- |each key type may correspond to a different data structure that
-- |implements it.
class (Ord k, Eq k, Show k) => GMapKey k where
  data GMap k :: * -> *
  empty       :: GMap k v
  lookup      :: k -> GMap k v -> Maybe v
  insert      :: k -> v -> GMap k v -> GMap k v
  alter       :: (Maybe a -> Maybe a) -> k -> GMap k a -> GMap k a
  toList      :: GMap k a -> [(k,a)]

--------------------------------------------------------------------------------

#if 0
instance FitInWord t => GMapKey t where
  -- data GMap t v            = GMapWord (GMap t v) deriving Show
  -- empty                    = GMapWord empty
  -- lookup k    (GMapWord m) = lookup (ord k) m
  -- insert k v  (GMapWord m) = GMapWord (insert (ord k) v m)
  -- alter  fn k (GMapWord m) = GMapWord (alter fn (ord k) m)
  -- toList      (GMapWord m) = map (\ (i,v) -> (chr i,v)) (toList m)
  data GMap t v           = GMapInt (DI.IntMap v) deriving Show
  empty                   = trace "\n <<<<< Empty FitInWord Gmap... >>>>\n"$ GMapInt DI.empty
  lookup k    (GMapInt m) = DI.lookup (wordToInt$ toWord k) m
  insert k v  (GMapInt m) = GMapInt (DI.insert (wordToInt$ toWord k) v m)
  alter  fn k (GMapInt m) = GMapInt (DI.alter fn (wordToInt$ toWord k) m)
  toList      (GMapInt m) = map (\ (i,v) -> (fromWord$ intToWord i, v)) $ 
			    DI.toList m


#else

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

instance GMapKey Int where
  data GMap Int v         = GMapInt (DI.IntMap v) deriving Show
  empty                   = GMapInt DI.empty
  lookup k    (GMapInt m) = DI.lookup k m
  insert k v  (GMapInt m) = GMapInt (DI.insert k v m)
  alter  fn k (GMapInt m) = GMapInt (DI.alter fn k m)
  toList      (GMapInt m) = DI.toList m

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

-- TODO IFDEF 64 BIT THEN WE CAN FIT AN INT64 AND WORD64 TOO!!
#endif


--------------------------------------------------------------------------------


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


-- |GMaps over pairs are implemented by nested GMaps.
instance (GMapKey a, GMapKey b) => GMapKey (a, b) where
  data GMap (a, b) v            = GMapPair (GMap a (GMap b v))
  empty		                = GMapPair empty
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
{-
-- Here's a traditional Data.Map implementation:
instance (Ord a, Ord b) => GMapKey (a, b) where
  newtype GMap (a, b) v         = GMapPair (DM.Map (a,b) v)
  empty		                = GMapPair DM.empty
  lookup pr   (GMapPair gm) = DM.lookup pr gm
  insert pr v (GMapPair gm) = GMapPair $ DM.insert pr v gm
-}

-- -- Here's a traditional Data.Map implementation:
-- instance (Ord a, Ord b) => GMapKey (a, b) where
--   empty	= DM.empty
--   lookup = DM.lookup
--   insert = DM.insert

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

-- |GMaps with list indices could be treated like tuples (nested
-- |maps).  Instead, we put them in a regular Data.Map.
instance (GMapKey a) => GMapKey [a] where
  data GMap [a] v         = GMapList (DM.Map [a] v) deriving Show
  empty                   = GMapList DM.empty
  lookup k    (GMapList m) = DM.lookup k m
  insert k v  (GMapList m) = GMapList (DM.insert k v m)
  alter  fn k (GMapList m) = GMapList (DM.alter fn k m)
  toList      (GMapList m) = DM.toList m
 


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
  toWord   = undefined
  fromWord = undefined

-- If we know a little more, use the Judy version:
instance (FitInWord k, J.JE v) => GMapKeyVal k v where
  data GMap2 k v           = GMapInt2 (J.JudyL v) 
  empty2  = do x <- J.new 
	       return $ GMapInt2 x
  lookup2 = undefined
  insert2 = undefined
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


