{-# LANGUAGE RecordWildCards, ScopedTypeVariables, DeriveDataTypeable, OverloadedStrings  #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
----------------------------------------------------------------------------------------------------
-- "Vacuum" mode is for sucking up trace output and doing useful things with it.
--
-- The initial intentions are to use it to formulate a prototype .cnc
-- spec file and to use it for visualization of an execution (and possible debugging).
--
-- Original Author: Ryan Newton
----------------------------------------------------------------------------------------------------

module Intel.Cnc.Spec.TraceVacuum 
    ( 
      NameTag, CncTraceEvent (..),  
      parseCncTrace, 
      packCncTrace, unpackCncTrace, isPackedTrace, 
      isGZipped,
      sample_trace, test_traceVacuum
    )
 where

import Intel.Cnc.Spec.Util
import Intel.Cnc.Spec.Version 

import Debug.Trace
import Data.Maybe
import Data.Data
import Data.Binary
-- import Data.Binary.Generic

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy as BLW
import qualified Data.ByteString.Char8 as B

import Codec.Compression.GZip
import Control.Monad

import Text.Parsec as P
import Text.Parsec.ByteString

import Test.HUnit
import StringTable.Atom

--------------------------------------------------------------------------------

--type NameTag = (String,String)
-- type NameTag = (Atom,String)
type NameTag = (Atom, B.ByteString)

data CncTraceEvent = 
   Prescribe Atom Atom
 | PutI NameTag NameTag
 | GetI NameTag NameTag
 | PutT NameTag NameTag
 | StartStep NameTag 
 | EndStep   NameTag 
 | PARSEFAIL B.ByteString -- For debugging purposes record the failures.
  deriving (Show, Eq, Data, Typeable)

-- We should parse tags that we can make sense of, namely scalars and tuples.
data CncTraceTag = 
   TTUnknown String
 | TTInt Int
 | TTFloat Float

--------------------------------------------------------------------------------
-- Parsing and Encoding traces

-- spc :: ParsecT s u m Char
spc :: Parser Char
spc = oneOf (" \t" :: String)
whitespc = many spc

defaultStepContext :: NameTag
defaultStepContext = (toAtom special_environment_name,"")

-- | This converts the lines of a trace file into a parsed trace.
parseCncTrace :: [B.ByteString] -> [CncTraceEvent]
parseCncTrace lines = loop defaultStepContext lines
 where 
  loop enclosing [] = []
  loop enclosing (line:tl) = 
     let parsed = Just$ doParse (traceline enclosing) line
	 rest = case parsed of 
		   Just (StartStep nametag) -> loop nametag tl
		   _                        -> loop enclosing tl
     in case parsed of 
	   Nothing -> rest
	   Just x  -> (x:rest)


-- Test if a stream of bytes is GZip format.
-- This uses the magic bytes at the beginning of the file: 1f8b
isGZipped :: BL.ByteString -> Bool
-- An alternative would be to just TRY to uncompress it and catch an exception...
-- But that would be awful sloppy...
isGZipped bs = 
    -- Byte 1f = 31, and 8b = 139
   prefix == [31,139]
   -- I'm seeing 8b1f on hexdump... that's a bit weird.
 where 
  prefix = take 2 $ BLW.unpack bs

--------------------------------------------------------------------------------
-- Pack traces into a binary format:
--------------------------------------------------------------------------------

-- Convention: Our file format is simple.  We have one line of plain
-- ASCII for identification, and then a gzipped BLOb containing the
-- marshalled haskell datatype.

preface = "Intel CnC binary trace file, version "
tagline = preface ++ version ++ "\n"

-- [2011.01.26] Switching this to a CUSTOM binary format rather than
-- using a default (Data.Binary.Generic) one.

-- FORMAT CHANGE LOG:
-- ------------------
-- Ver 0.1.3.107 -- using Data.Binary.Generic encoding
-- Ver 0.1.3.108 -- Wrote new Data.Binary instance.
--                  This version uses compressed format inspite of stack/space problems [2011.01.26]

{-
-- For a first cut we use[d] a default binary encoding.  We could
-- standardize this if it was going to be read by any other tools....
instance Binary CncTraceEvent where
  put = putGeneric
  get = getGeneric
-}


do_compress = True

-- NOTE: Could strip the PARSEFAIL entries upon packing.
--       Something to think about...
packCncTrace :: [CncTraceEvent] -> BL.ByteString 
packCncTrace trace =
 BL.append (BL.pack tagline)$
       (if do_compress then compress else id)
       (encode trace)

unpackCncTrace :: BL.ByteString -> [CncTraceEvent]
unpackCncTrace bstr = 
   -- tail chops off the '\n' character:
   decode$ 
   (if do_compress then  decompress else id) $
   BL.tail rest 
 where 
  (fst,rest) = BL.break (=='\n') bstr


-- Actually this is not YET a fully specified binary format because it
-- depends on the Data.Binary representation of LISTS.  We should lock
-- this down if we want it to be read by non-haskell languages.  (Of
-- course, the binary package itself is bound to never change this
-- format for compatibility.)

instance Binary CncTraceEvent where 
    put x =  
      -- This is pretty much a pile of boilerplate:
      case x of 
        Prescribe a b -> do putWord8 0; put a; put b 
        PutI a b      -> do putWord8 1; put a; put b
        GetI a b      -> do putWord8 2; put a; put b
        PutT a b      -> do putWord8 3; put a; put b
        StartStep nm  -> do putWord8 4; put nm
        EndStep   nm  -> do putWord8 5; put nm
        PARSEFAIL str -> do putWord8 6; put str

    get = do tag <- getWord8 
	     case tag of 
	       0 -> liftM2 Prescribe get get 
	       1 -> liftM2 PutI      get get 
	       2 -> liftM2 GetI      get get 
	       3 -> liftM2 PutT      get get 
	       4 -> liftM StartStep get 
	       5 -> liftM EndStep   get 
	       6 -> liftM PARSEFAIL get 
	       _ -> error$ "Unmarshalling CncTraceEvent, got bad tag byte: "++ show tag

-- | Check the first bytes in the stream to tell if its a CnC trace:
isPackedTrace :: BL.ByteString -> Bool
isPackedTrace = BL.isPrefixOf (BL.pack preface)
  

--------------------------------------------------------------------------------
-- Helpers:

-- doParse :: Parser CncTraceEvent -> String -> CncTraceEvent
doParse :: Parser CncTraceEvent -> B.ByteString -> CncTraceEvent
doParse p input
  = case (parse p "" input) of
      Left err -> PARSEFAIL input
      Right x  -> x

cnc_identifier :: Parser Atom
cnc_identifier = 
   do name <- many1 (letter <|> digit <|> oneOf "_")
      return$ toAtom name

traceline :: NameTag -> Parser CncTraceEvent
traceline stepctxt = 
 let nametag :: Char -> Char -> Parser NameTag
     nametag open close = 
       do name <- cnc_identifier
          char ':'; whitespc
          -- Then we grab EVERYTHING up until the ">" that ends things
          --tag <- many1 (noneOf end)
	  tag <- balanced_nest open close
	  return (name, tag)

     ruletemplate (str :: B.ByteString) open close fn = 
--       try (do string (str++" "++[open]); whitespc
       try (do 
	       --string (str `B.append` B.pack [' ',open])
	       string (B.unpack str ++ [' ',open])
	       whitespc
               pr <- nametag open close
               return$ fn pr)
 in
  ruletemplate "Start step" '('')' StartStep <|> 
  ruletemplate "End step"   '('')' EndStep <|> 
  ruletemplate "Put tag"    '<''>' (PutT stepctxt) <|> 
  ruletemplate "Put item"   '['']' (PutI stepctxt) <|> 
  ruletemplate "Get item"   '['']' (GetI stepctxt) <|> 
  ruletemplate "GetX item"  '['']' (GetI stepctxt) <|>
    do string "Prescribe"    ; whitespc 
       tags <- cnc_identifier; whitespc
       step <- cnc_identifier
       return (Prescribe tags step)

-- This is any old text but it must be balanced in the delimeters of interest: e.g. () <> []
balanced_nest :: Char -> Char -> Parser B.ByteString
balanced_nest open close = loop [] 0
 -- This is pretty inefficent because it goes character by character...
 where 
  loop acc n = 
       do c<-noneOf [open,close]; loop (c:acc) n
   <|> do c<-char open;           loop (c:acc) (n+1)
   <|> do c<-char close; 
          if n==0 then return (B.reverse$ B.pack acc)
  	   else loop (c:acc) (n-1)





------------------------------------------------------------------------------------------------------------------------
-- Testing
------------------------------------------------------------------------------------------------------------------------


runPr prs str = print (run prs str)
run :: Show a => Parser a -> B.ByteString -> a
run p input
        = case (P.parse p "" input) of
            Left err -> error ("parse error at "++ show err)
            Right x  -> x


-- tryParse :: Parser a -> String -> Maybe a
tryParse :: Parser a -> B.ByteString -> Maybe a
tryParse p input
  = case (P.parse p "" input) of
      Left err -> Nothing
--      Left err -> Just (PARSEFAIL input)
      Right x  -> Just x

stoA :: String -> Atom
stoA = toAtom 

test_traceVacuum = 
 testSet "TraceVacuum" $ 
 let tP = tryParse (traceline defaultStepContext) 
     sample = map (tryParse (traceline defaultStepContext)) sample_trace
     sample' = catMaybes sample
     isfail (Just (PARSEFAIL _)) = True
     isfail _ = False
     tC = testCase ""
 in
 [ tC "traceline1: parse one line"$ Just (StartStep (stoA "fib_step","0"))           ~=? tP "Start step (fib_step: 0)"
 , tC "traceline2: parse one line"$ Just (PutT (stoA special_environment_name,"") 
					  (stoA "tags","10")) ~=? tP "Put tag <tags: 10>"
 , tC "traceline3: parse one line"$ Nothing                                            ~=? tP "__Put tag <tags: 10>"
 , tC "traceline4: parse one line"$ Just (Prescribe (stoA "control_S1") (stoA "kj_compute"))
                                      ~=? tP  "Prescribe control_S1 kj_compute"

 , tC "sample trace: #fail"   $    0 ~=? length (filter isfail sample)
 , tC "sample trace: #success"$  111 ~=? length (filter (not . isfail) sample)
 , tC "sample trace: #noparse"$   16 ~=? length (filter (==Nothing) sample)

 , tC "balanced nesting"      $  Just"foo (a) (b c) bar" ~=? tryParse (balanced_nest '(' ')') "foo (a) (b c) bar) baz"		      

 , tC "encode . decode = id for NameTag" $ 
      let x = ("Hi"::Atom, BL.pack "There") in
      x ~=? decode (encode x)

 , tC "unpack . pack = id " $ sample' ~=? (unpackCncTrace$ packCncTrace sample')

 ]

sample_trace = 
 ["Prescribe tags fib_step",
  "Prescribe tags fibctrl",
  "Put tag <tags: 10>",
  "Start step (fibctrl: 10)",
  "Put tag <tags: 9>",
  "Put tag <tags: 8>",
  "End step (fibctrl: 10)",
  "Start step (fibctrl: 8)",
  "Put tag <tags: 7>",
  "Put tag <tags: 6>",
  "End step (fibctrl: 8)",
  "Start step (fibctrl: 6)",
  "Put tag <tags: 5>",
  "Put tag <tags: 4>",
  "End step (fibctrl: 6)",
  "Start step (fibctrl: 4)",
  "Put tag <tags: 3>",
  "Put tag <tags: 2>",
  "End step (fibctrl: 4)",
  "Start step (fibctrl: 2)",
  "Put tag <tags: 1>",
  "Put tag <tags: 0>",
  "End step (fibctrl: 2)",
  "Start step (fibctrl: 0)",
  "End step (fibctrl: 0)",
  "Start step (fib_step: 0)",
  "Put item [fibs: 0] -> 0",
  "End step (fib_step: 0)",
  "Start step (fibctrl: 1)",
  "End step (fibctrl: 1)",
  "Start step (fib_step: 1)",
  "Put item [fibs: 1] -> 1",
  "End step (fib_step: 1)",
  "Start step (fib_step: 2)",
  "GetX item [fibs: 1] -> 1",
  "GetX item [fibs: 0] -> 0",
  "Put item [fibs: 2] -> 1 getcount=2",
  "End step (fib_step: 2)",
  "Start step (fib_step: 3)",
  "GetX item [fibs: 2] -> 1",
  "GetX item [fibs: 1] -> 1",
  "Put item [fibs: 3] -> 2 getcount=2",
  "item [fibs: <2>] m_getCount decremented to 1",
  "End step (fib_step: 3)",
  "Start step (fib_step: 4)",
  "GetX item [fibs: 3] -> 2",
  "GetX item [fibs: 2] -> 1",
  "Put item [fibs: 4] -> 3 getcount=2",
  "item [fibs: <3>] m_getCount decremented to 1",
  "item [fibs: <2>] m_getCount decremented to 0",
  "End step (fib_step: 4)",
  "Start step (fib_step: 5)",
  "GetX item [fibs: 4] -> 3",
  "GetX item [fibs: 3] -> 2",
  "Put item [fibs: 5] -> 5 getcount=2",
  "item [fibs: <4>] m_getCount decremented to 1",
  "item [fibs: <3>] m_getCount decremented to 0",
  "End step (fib_step: 5)",
  "Start step (fib_step: 6)",
  "GetX item [fibs: 5] -> 5",
  "GetX item [fibs: 4] -> 3",
  "Put item [fibs: 6] -> 8 getcount=2",
  "item [fibs: <5>] m_getCount decremented to 1",
  "item [fibs: <4>] m_getCount decremented to 0",
  "End step (fib_step: 6)",
  "Start step (fib_step: 7)",
  "GetX item [fibs: 6] -> 8",
  "GetX item [fibs: 5] -> 5",
  "Put item [fibs: 7] -> 13 getcount=2",
  "item [fibs: <6>] m_getCount decremented to 1",
  "item [fibs: <5>] m_getCount decremented to 0",
  "End step (fib_step: 7)",
  "Start step (fib_step: 8)",
  "GetX item [fibs: 7] -> 13",
  "GetX item [fibs: 6] -> 8",
  "Put item [fibs: 8] -> 21 getcount=2",
  "item [fibs: <7>] m_getCount decremented to 1",
  "item [fibs: <6>] m_getCount decremented to 0",
  "End step (fib_step: 8)",
  "Start step (fib_step: 9)",
  "GetX item [fibs: 8] -> 21",
  "GetX item [fibs: 7] -> 13",
  "Put item [fibs: 9] -> 34 getcount=2",
  "item [fibs: <8>] m_getCount decremented to 1",
  "item [fibs: <7>] m_getCount decremented to 0",
  "End step (fib_step: 9)",
  "Start step (fib_step: 10)",
  "GetX item [fibs: 9] -> 34",
  "GetX item [fibs: 8] -> 21",
  "Put item [fibs: 10] -> 55 getcount=2",
  "item [fibs: <9>] m_getCount decremented to 1",
  "item [fibs: <8>] m_getCount decremented to 0",
  "End step (fib_step: 10)",
  "Start step (fibctrl: 3)",
  "Put tag <tags: 2>",
  "Put tag <tags: 1>",
  "End step (fibctrl: 3)",
  "Start step (fibctrl: 5)",
  "Put tag <tags: 4>",
  "Put tag <tags: 3>",
  "End step (fibctrl: 5)",
  "Start step (fibctrl: 7)",
  "Put tag <tags: 6>",
  "Put tag <tags: 5>",
  "End step (fibctrl: 7)",
  "Start step (fibctrl: 9)",
  "Put tag <tags: 8>",
  "Put tag <tags: 7>",
  "End step (fibctrl: 9)",
  "Get item [fibs: 10] -> 55",
  "CnC recursive (10): 55"]


