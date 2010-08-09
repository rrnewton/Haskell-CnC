{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
----------------------------------------------------------------------------------------------------
-- "Vacuum" mode is for sucking up trace output and doing useful things with it.
--
-- The initial intentions are to use it to formulate a prototype .cnc
-- spec file and to use it for visualization of an execution (and possible debugging).
--
-- Original Author: Ryan Newton
----------------------------------------------------------------------------------------------------

module Intel.Cnc.Spec.Vacuum where

import Data.Maybe
import Control.Monad
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import StringTable.Atom

--------------------------------------------------------------------------------

--type NameTag = (String,String)
type NameTag = (Atom,String)

data CncTraceEvent = 
   Prescribe Atom Atom
 | PutI NameTag NameTag
 | GetI NameTag NameTag
 | PutT NameTag NameTag
 | StartStep NameTag 
 | EndStep   NameTag 
 | FAIL String
  deriving Show 

-- We should parse tags that we can make sense of, namely scalars and tuples.
data CncTraceTag = 
   TTUnknown String
 | TTInt Int
 | TTFloat Float

--------------------------------------------------------------------------------
-- Parsing traces

spc = oneOf " \t"
whitespc = many spc
defaultStepContext = (toAtom "env","")


tracefile :: [String] -> [CncTraceEvent]
--tracefile lines = loop (error "No enclosing step!") lines
tracefile lines = loop defaultStepContext lines
 where 
  loop enclosing [] = []
  loop enclosing (line:tl) = 
     let parsed = tryParse (traceline enclosing) line
	 rest = case parsed of 
		   Just (StartStep nametag) -> loop nametag tl
		   _                        -> loop enclosing tl
     in case parsed of 
	   Nothing -> rest
	   Just x  -> (x:rest)


cnc_identifier = 
   do name <- many1 (letter <|> oneOf "_")
      return$ toAtom name

traceline :: NameTag -> Parser CncTraceEvent
traceline stepctxt = 
 let nametag end = 
       do name <- cnc_identifier
          char ':'; whitespc
          -- Then we grab EVERYTHING up until the ">" that ends things
          tag <- many1 (noneOf end)
	  return (name,tag)

     ruletemplate str open close fn = 
       try (do string (str++" "++open); whitespc
               pr <- nametag close
               return$ fn pr)
 in
  ruletemplate "Start step" "("")" StartStep <|> 
  ruletemplate "End step"   "("")" EndStep <|> 
  ruletemplate "Put tag"    "<"">" (PutT stepctxt) <|> 
  ruletemplate "Put item"   "[""]" (PutI stepctxt) <|> 
  ruletemplate "Get item"   "[""]" (GetI stepctxt) <|> 
  ruletemplate "GetX item"  "[""]" (GetI stepctxt) <|>
    do string "Prescribe"; whitespc 
       tags <- cnc_identifier; whitespc
       step <- cnc_identifier
       return (Prescribe tags step)


------------------------------------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------------------------------------


runPr prs str = print (run prs str)
run :: Show a => Parser a -> String -> a
run p input
        = case (parse p "" input) of
            Left err -> error ("parse error at "++ show err)
            Right x  -> x


tryParse :: Parser a -> String -> Maybe a
tryParse p input
  = case (parse p "" input) of
      Left err -> Nothing
--      Left err -> Just (FAIL input)
      Right x  -> Just x


t19 = runPr (traceline defaultStepContext) "Start step (fib_step: 0)"

t20 = runPr (traceline defaultStepContext) "Put tag <tags: 10>"
t21 = tryParse (traceline defaultStepContext) "Put tag <tags: 10>"
t22 = tryParse (traceline defaultStepContext) "__Put tag <tags: 10>"


isfail (Just (FAIL _)) = True
isfail _ = False

t23 = mapM_ print $ filter (not . isfail) $ map (tryParse (traceline defaultStepContext)) sample_trace

t24 = mapM_ print $ filter isfail $ map (tryParse (traceline defaultStepContext)) sample_trace


t25 = mapM_ print  $ map (tryParse (traceline defaultStepContext)) sample_trace

--t26 = mapM_ print  $ catMaybes $ tryParse $ tracefile sample_trace
t26 = mapM_ print $ tracefile sample_trace

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


