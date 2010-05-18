#!/usr/bin/env runhaskell
{-# LANGUAGE NamedFieldPuns
  #-}

import Text.PrettyPrint.HughesPJClass
import Text.Regex
import Data.List
import Data.Function
import Control.Monad
import System

import HSH



-- import Graphics.Gnuplot.Simple
-- import Graphics.Gnuplot.Advanced
-- import Graphics.Gnuplot.Frame
-- import Graphics.Gnuplot.Frame.OptionSet

-- import qualified Graphics.Gnuplot.Terminal.X11
-- import Graphics.Gnuplot.Plot.TwoDimensional



import qualified Graphics.Gnuplot.Simple as Simple

import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Terminal.X11 as X11

import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.Option as Opt
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts

import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import Graphics.Gnuplot.Plot.TwoDimensional (linearScale, )

import Data.Array (listArray, )
import Data.Monoid (mappend, )


--import qualified Graphics.Gnuplot.Private.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.LineSpecification as LineSpec

simple2d :: Plot2D.T
simple2d =
   Plot2D.function (linearScale 100 (-10,10::Double)) sin

circle2d :: Plot2D.T
circle2d =
   fmap
      (Graph2D.typ Graph2D.points)
      (Plot2D.parameterFunction
         (linearScale 24 (-pi,pi::Double))
         (\t -> (cos t, sin t)))

overlay2d :: Frame.T Graph2D.T
overlay2d =
   Frame.cons (Opts.size 1 0.4 $ Opts.remove Opt.key $ Opts.deflt) $
   Plot2D.function (linearScale 100 (-pi,pi::Double)) cos
   `mappend`
   circle2d

-- mypath :: Graph2D.T 
mypath :: Plot2D.T
mypath = 
   fmap (Graph2D.lineSpec $ 
	  LineSpec.title "blaht" $ 
	  LineSpec.lineWidth 3.0 $ 
	  LineSpec.pointSize 3.0 $ 
	  LineSpec.deflt) $ 
   fmap (Graph2D.typ Graph2D.linesPoints) $
   Plot2D.path [(0,0), (1,1), (3,2)]

spec :: LineSpec.T
spec = LineSpec.title "blah" LineSpec.deflt

myoverlay :: Frame.T Graph2D.T
myoverlay =
  --Graph2D.lineSpec (LineSpec.title "blah" LineSpec.deflt) $ 
   Frame.cons (Opts.deflt) $
   mypath --(Graph2D.lineSpec spec mypath)
   `mappend`
   circle2d




--x11 = terminal Terminal.X11.cons
--x11 = terminal cons
--x11 = terminal Graphics.Gnuplot.Terminal.X11.cons
--x11 = terminal X11.cons

--------------------------------------------------------------------------------
-- Let's take a particular interpretation of Enum for pairs:
instance (Enum t1, Enum t2) => Enum (t1,t2) where 
  succ (a,b) = (succ a, succ b)
  pred (a,b) = (pred a, pred b)
  toEnum n = (toEnum n, toEnum n)
  fromEnum (a,b) = case (fromEnum a, fromEnum b) of
                     (x,y) | x == y -> x
		     (x,y) -> error$ "fromEnum of pair: nonmatching numbers: " ++ show x ++" and "++ show y


-- Removes single blanks and separates lines into groups based on double blanks.
sepDoubleBlanks :: [String] -> [[String]]
sepDoubleBlanks ls = loop [] ls 
 where 
  loop acc []        = [reverse acc]
  loop acc ("":"":t) = reverse acc : loop [] (stripLeadingBlanks t)
  loop acc ("":t)    = loop acc t
  loop acc (h :t)    = loop (h:acc) t 
  stripLeadingBlanks []     = [] 
  stripLeadingBlanks ("":t) = stripLeadingBlanks t
  stripLeadingBlanks ls     = ls


remComments :: String -> [String] -> [String]
remComments commentchars ls = filter (pred . stripLeadingWhitespace) ls
 where 
  pred str = not (take (length commentchars) str == commentchars) 
  stripLeadingWhitespace []      = [] 
  stripLeadingWhitespace (' ':t) = stripLeadingWhitespace t
  stripLeadingWhitespace ls      = ls

--------------------------------------------------------------------------------

-- Here's the schema for the data from my timing tests:
data Entry = Entry { 
  name     :: String,
  variant  :: String,
  sched    :: Int, 
  threads  :: Int, 
  hashhack :: Bool, 
  tmin     :: Double,
  tmed     :: Double,
  tmax     :: Double
}
  deriving Show

instance Pretty Entry where
  --pPrint x = pPrint$ show x
  pPrint Entry { name, variant, sched, threads, tmin, tmed, tmax } = 
       pPrint ("ENTRY", name, variant, sched, threads, tmin, tmed, tmax)


parse [a,b,c,d,e,f,g,h] =
  Entry { name     = a, 
	  variant  = b,
	  sched    = read c,
	  threads  = read d,
	  hashhack = not (e == "0"),
	  tmin     = read f,
	  tmed     = read g,
	  tmax     = read h
	} 
parse other = error$ "Cannot parse, wrong number of fields, "++ show (length other) ++" expected 8: "++ show other

groupSort fn = 
   (groupBy ((==) `on` fn)) . 
   (sortBy (compare `on` fn))

-- Add three more levels of list nesting to organize the data:
organize_data :: [Entry] -> [[[[Entry]]]]
organize_data = 
	 (map (map (groupSort sched)))  . 
  	      (map (groupSort variant)) .
                   (groupSort name)


newtype Mystr = Mystr String

instance Show Mystr where
  show (Mystr s) = s


-- mypath :: Graph2D.T 
--Plot2D.T
--plot_benchmark :: [[[Entry]]] -> IO ()
--plot_benchmark :: [[[Entry]]] -> Plot2D.T
plot_benchmark [io, pure] = 
    --Plot.plot (X11.title "foobar" X11.cons) $
    Plot.plot X11.cons $
    Frame.cons (Opts.title ("Benchmark: " ++ benchname ++ " normalized to time " ++ show basetime) $ Opts.deflt) plots
 where 
  benchname = name $ head $ head io 
  plots = foldl1 mappend (map persched io ++ map persched pure)
  basetime = foldl1 min $ map tmed $
	     filter ((== 0) . threads) $
	     (concat io ++ concat pure)
  persched :: [Entry] -> Plot2D.T
  persched dat = 
    let 
	schd = sched$   head dat
	var  = variant$ head dat
        mins = map tmin dat
        meds = map tmed dat
        maxs = map tmax dat
	--zip4 = map$ \ a b c d -> (a,b,c,d)
	zip4 s1 s2 s3 s4 = map (\ ((a,b), (c,d)) -> (a,b,c,d))
	                   (zip (zip s1 s2) (zip s3 s4))
        pairs = zip4 (map (fromIntegral . threads) dat) 
		     (map (basetime / ) meds)
		     (map (basetime / ) mins)
		     (map (basetime / ) maxs)
	quads = map (\ (a,b,c,d) -> Mystr (show a ++" "++ show b ++" "++ show d ++" "++ show c))
		pairs 
    in 
      fmap (Graph2D.lineSpec $ 
	    LineSpec.title (var ++"/"++ show schd) $ 
	    LineSpec.lineWidth 3.0 $ 
	    LineSpec.pointSize 3.0 $ 
	    LineSpec.deflt) $ 
      fmap (Graph2D.typ Graph2D.linesPoints) $
      --Plot2D.path pairs
      --Plot2D.path (map ( \ (a,b,c,d) -> (a,b)) pairs)
      --fmap (Graph2D.typ Graph2D.errorBars) $
      Plot2D.list quads


-- Ok, yuck, giving up on the Cabal gnuplot package and generating the gnuplot output myself.
plot_benchmark2 root [io, pure] = action (io ++ pure)
 where 
  benchname = name $ head $ head io 
  -- What was the best single-threaded execution time across variants/schedulers:
  basetime = foldl1 min $ map tmed $
	     filter ((== 0) . threads) $
	     (concat io ++ concat pure)
  (filebase,_) = break (== '.') $ basename benchname 

  scrub '_' = ' '
  scrub x = x
  -- scrub [] = []
  -- scrub ('_':t) = "\\_"++ scrub t
  -- scrub (h:t)   = h : scrub t

  action lines = 
   do 
      let scriptfile = root ++ filebase ++ ".gp"
      putStrLn$ "Dumping gnuplot script to: "++ scriptfile
      runIO$ echo "set terminal postscript enhanced color\n"         -|- appendTo scriptfile
      runIO$ echo ("set output \""++filebase++".eps\"\n")            -|- appendTo scriptfile
      runIO$ echo ("set title \"Benchmark: "++ map scrub filebase ++
		   ", speedup relative to serial time " ++ show basetime ++" seconds\"\n") -|- appendTo scriptfile
      runIO$ echo ("set xlabel \"Number of Threads\"\n")             -|- appendTo scriptfile
      runIO$ echo ("set ylabel \"Parallel Speedup\"\n")              -|- appendTo scriptfile
      runIO$ echo ("plot \\\n")                                      -|- appendTo scriptfile
      -- In this loop lets do the errorbars:
      forM_ (zip [1..] lines) $ \(i,points) -> do 
          let datfile = root ++ filebase ++ show i ++".dat"
	  runIO$ echo ("   \""++ basename datfile ++"\" using 1:2:3:4 with errorbars title \"\", \\\n") -|- appendTo scriptfile

      -- Now a second loop for the lines themselves and to dump the actual data:
      forM_ (zip [1..] lines) $ \(i,points) -> do 
          let datfile = root ++ filebase ++ show i ++".dat"          
	  let schd = sched$   head points
	  let var  = variant$ head points
	  let nickname = var ++"/"++ show schd
	  runIO$ echo ("# Data for variant "++ nickname ++"\n") -|- appendTo datfile
          forM_ points $ \x -> do 
	      runIO$ echo (show (fromIntegral (threads x)) ++" "++
			   show (basetime / tmed x)        ++" "++
                           show (basetime / tmax x)        ++" "++ 
			   show (basetime / tmin x)        ++" \n") -|- appendTo datfile

	  let comma = if i == length lines then "" else ",\\"
	  runIO$ echo ("   \""++ basename datfile ++
		       "\" using 1:2 with lines linewidth 4.0 lt "++ show i ++" title \""++nickname++"\" "++comma++"\n")
		   -|- appendTo scriptfile

      putStrLn$ "Finally, running gnuplot..."
      -- runIO$ "(cd "++root++"; gnuplot "++basename scriptfile++")"
      -- runIO$ "(cd "++root++"; ps2pdf "++ filebase ++".eps )"


plot_benchmark2 root _ = putStrLn "plot_benchmark2: Unexpected input"		       

isMatch rg str = case matchRegex rg str of { Nothing -> False; _ -> True }

main = do 
 dat <- run$ catFrom ["results.dat"] -|- remComments "#" 

 let parsed = map (parse . splitRegex (mkRegex "[ \t]+")) 
	          (filter (not . isMatch (mkRegex "ERR")) $
		   filter (not . null) dat)
 let organized = organize_data$ filter ((`elem` ["io","pure"]) . variant) parsed


 -- let chunked = sepDoubleBlanks dat		 
 -- let chopped = map (parse . splitRegex (mkRegex "[ \t]+"))
 -- 	           (chunked !! 0)
 -- let bysched = groupBy ((==) `on` sched) $
 -- 	       sortBy (compare `on` sched) 
 -- 		      chopped
 -- putStrLn$ show (pPrint (map length chopped))
 -- putStrLn$ show (pPrint (map parse chopped))

 putStrLn$ renderStyle (style { lineLength=150 }) (pPrint organized)

 --Plot.plot X11.cons myoverlay
 --Simple.plotList [Simple.LineStyle 0 [Simple.LineTitle "foobar"]] [0,5..100]

 let root = "./graph_temp/"
 -- For hygiene, completely anhilate output directory:
 system$ "rm -rf "  ++root
 system$ "mkdir -p "++root
 forM_ organized    $ \ perbenchmark -> do 
  plot_benchmark2 root perbenchmark
  forM_ perbenchmark $ \ pervariant -> 
   forM_ pervariant   $ \ persched -> 
     do let mins = map tmin persched
	let pairs = (zip (map (fromIntegral . threads) persched) mins)
	putStrLn$ show pairs
	--plot Graphics.Gnuplot.Terminal.X11.cons (path pairs)
	--System.exitWith ExitSuccess
	--plot x11 (path pairs)
	
        return ()

 --forM_ organized    $ \ perbenchmark -> 


 --plotLists [x11] [dat, [50..25]]
 --plotLists [x11] [dat, [100,95..0]]

 --plotDots [x11, Size$ Scale 3.0] dat
 --plotDots [x11, LineStyle 0 [PointSize 5.0]] dat
 putStrLn$ "Plotted list"


