{-# LANGUAGE RecordWildCards #-}

module Intel.Cnc.Spec.Curses where

--import System.IO
--import System.Environment
import System.Exit
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper as Help
import UI.HSCurses.Widgets

import Control.Monad
import System.Posix.Unistd
import Data.Sequence as Seq 
import Data.IORef


centeredLine win row str = 
  do let len = Prelude.length str 
     (height,width) <- scrSize
     move row ((width - len) `quot` 2)
     wAddStr win str

-- Hmm...  I absolutely shouldn't have to write this:
box win (y,x) (h,w) = 
  do let horiz = "+"++ (Prelude.take (w-2) $ repeat '-') ++"+"
     mvWAddStr win y x horiz
     mvWAddStr win (y + h - 1) x horiz
     forM_ [1..h-2] $ \r -> mvWAddStr win (y + r) x "|"
     forM_ [1..h-2] $ \r -> mvWAddStr win (y + r) (x + w - 1) "|"
     

-- I shouldn't have to write this either.
-- The hscurses textwidget is written in a profoundly inefficient way.

data ScrollBufRec = SB { 
    --Int
    bottom :: Bool, -- Are we following the bottom or have we "paged up" to the history?
    offscreen :: Seq.Seq String, -- The lines that have scrolled off
    visible :: Seq.Seq String,    -- The lines we can see.
    pos :: Pos,
    size :: Size
}

type ScrollBuf = IORef ScrollBufRec

--mkScrollBuf :: Int -> ScrollBuf
--mkScrollBuf n = SB n Seq.empty
--emptyScrollBuf pos size = SB True Seq.empty Seq.empty pos size

--mkScrollBuf :: Pos -> Size -> IO (IORef ScrollBuf)
mkScrollBuf :: Pos -> Size -> IO ScrollBuf
mkScrollBuf pos size = newIORef $ SB True Seq.empty Seq.empty pos size       

scrollBufAddLine :: ScrollBuf -> String -> IO ()
scrollBufAddLine ref str = 
  do orig <- readIORef ref
     let (y,x) = pos orig
	 (h,w) = size orig
         newvis = visible orig |> str
         extra = Seq.length newvis - h
	 sb = if bottom orig && extra > 0
	      then orig{ offscreen= offscreen orig >< Seq.take extra newvis,
		         visible= Seq.drop extra newvis
		       }
	      else orig{ visible= newvis }
     writeIORef ref sb
     drawScrollBuf sb

-- Adds to the end of the last line:
--scrollBufAddStr 

drawScrollBuf sb = 
  do let lns = viewLtoList $ viewl $ visible sb
         tw = newTextWidget defaultTWOptions $ unlines lns
     drawTextWidget (pos sb) (size sb) DHNormal tw

-- scrollBufToTop ref = 
--   do sb <- readIORef ref
--      let new = sb { bottom = False, 
-- 		    offscreen= Seq.empty,
-- 		    visible= offscreen sb >< visible sb }
--      writeIORef ref new
--      drawScrollBuf new

scrollBufToTop ref = 
  do sb <- readIORef ref
     scrollBufUp ref (Seq.length $ offscreen sb)

scrollBufToBottom ref = 
  do sb <- readIORef ref
     scrollBufDown ref (Seq.length $ visible sb)

scrollBufUp ref n = 
  do sb <- readIORef ref
     let loop 0 sb = sb
	 loop n sb = 
	  loop (n-1) $ 
          case viewr$ offscreen sb of
           EmptyR -> sb 
	   rest :> r -> sb { bottom = False, 
		             offscreen= rest,
		             visible= r <| visible sb }
	 new = loop n sb
     writeIORef ref new
     drawScrollBuf new


scrollBufDown ref n = 
  do sb <- readIORef ref
     if bottom sb 
      then return ()
      else do 
       let (h,w) = size sb 
	   --loop n vis
	   extra = Seq.length (visible sb) - h
	   scroll = min extra n
	   loop 0 sb = sb
	   loop n sb = 
	       loop (n-1) $
	         case viewl$ visible sb of
                   EmptyL -> sb { bottom = True }
	           l :< rest -> sb { --bottom = False, 
				     bottom = (Seq.length rest <= h) || bottom sb,
				     offscreen= offscreen sb |> l, 
				     visible= rest }
	   new = if scroll <= 0 then sb 
		 else loop scroll sb
       writeIORef ref new
       drawScrollBuf new

scrollBufSet ref lines = 
  do sb <- readIORef ref
     let new = sb{ bottom=True, offscreen= Seq.empty, visible= Seq.fromList lines }
     writeIORef ref new
     drawScrollBuf new

viewLtoList EmptyL = []
viewLtoList (a :< rest) = a : viewLtoList (viewl rest)



----------------------------------------------------------------------------------------------------


--drawScrollBuf :: ScrollBuf -> Pos -> Size -> IO ()
--drawScrollBuf orig (y,x) (h,w) = 
  -- do let extra = Seq.length (visible orig) - h
  -- 	 sb = if extra > 0
  -- 	      then SB{ offscreen= (offscreen orig) >< Seq.take extra (visible orig),
  -- 		       visible= Seq.drop extra (visible orig)
  -- 		     }
  -- 	      else orig
  -- 	 lns = viewLtoList $ viewl $ visible sb
  --        tw = newTextWidget defaultTWOptions $ unlines lns
  --    undefined

widget pos size = 
  do putStrLn$ "foo"
     let lns = map ((" "++) . show) [1..100]
	 tw = newTextWidget defaultTWOptions $ unlines lns
     drawTextWidget pos size DHNormal $ 
     --drawTextWidget (6,2) (20,20) DHActive $
     --drawTextWidget pos size DHFocus $ 
	--textWidgetScrollUp (10,10) tw
        textWidgetScrollDown (15,0) tw -- problems
        --textWidgetScrollLeft (1,1) tw
        --textWidgetScrollRight (0,0) tw -- problems
        --tw

     refresh 
     --usleep (900 * 1000) -- 0.1 second sleep.
     --drawTextWidget pos size DHNormal $ textWidgetSetText tw $ unlines (drop 10 lns)
     refresh 

     return ()


main = do putStrLn$ "Hello"

          --initCurses
	  Help.start
	  win <- initScr
	  colors <- hasColors 
	  if not colors then error "COLORS not available on this terminal" else return()	  
	  startColor 

          numPairs <- colorPairs

	  initColor (Color 90) (0,0,0)
	  initColor (Color 91) (500,0,0)
	  initColor (Color 92) (1000,1000,1000)
	  initPair (Pair 17) (Color 91) (Color 90)
	  initPair (Pair 18) (Color 92) (Color 90)

          size@(height,width) <- scrSize

          let sb_start = 9

	  mvWAddStr win sb_start 2 $ "Event Scrollback History:" 
          box win (sb_start+1,1) (17,width-4)
          refresh

          --sb <- mkScrollBuf (sb_start+2,4) (15,width-6)
	  sb <- mkScrollBuf (sb_start+2,3) (15,width-7)

	  scrollBufAddLine sb "hello"
	  scrollBufAddLine sb "yay"
	  mapM_ (scrollBufAddLine sb) $ map show [1..200]
	  refresh

	  --usleep (700 * 1000) -- 0.1 second sleep.

	  --scrollBufToTop sb

{-
	  forM_ [1..7] $ \_ -> do 
	     scrollBufUp sb 2
  	     --usleep (100 * 1000)
	     refresh 

	  forM_ [1..7] $ \_ -> do 
	     scrollBufDown sb 2
  	     --usleep (100 * 1000)
	     refresh 
-}

          move 1 0
	  [redstyle, yellow] <- convertStyles [Style DarkRedF BlackB, Style YellowF BlackB]
	  withStyle redstyle $ drawLine width (repeat '=')

          centeredLine win 0 "CnC Interactive Trace Visualizer"

	  mvWAddStr win 2 0 $ "Trace events:          Time elapsed:" 
	  mvWAddStr win 3 0 $ "Current time:" 

	  mvWAddStr win 5 0 $ "Enter keyboard input:" 

	  let cursor_pos = (6,2)
	  withStyle yellow$ mvWAddStr win (fst cursor_pos) 0 $ "> " 

          gotoTop 
	  --wAddStr win$ "BLAH"

          test_loop win size sb cursor_pos sb_start width

          endWin 
	  Help.end
	  --update

          putStrLn$ "Exited ncurses"
          return ()
  where 
 test_loop win size sb (y,x) sb_start width = 
       do move (20) 40
	  colors <- hasColors 
	  --wAddStr win$ "FOO, size "++ show size++ "  colors " ++ (show colors)
	  --move 21 40
	  --drawLine 30 (repeat '=')
	  refresh

          cursor_field <- mkScrollBuf (y,x) (1,15)

          let loop row 0 = return ()
	      loop row n = 
		do --move row 50
		   --wAddStr win$ "Press any key["++ show n ++"]: "
		   box win (sb_start+1,1) (17,width-4)
                   move y x
		   refresh

		   c <- Help.getKey refresh
		   --c <- getCh
		   --c_ <- getch; let c = decodeKey c_
		   --beep
		   --wAddStr win$ "  You pressed: "
		   --[blue] <- convertStyles [Style BlueF BlackB]
		   --withStyle blue$ 
		   scrollBufSet cursor_field ["  You pressed: "]

		   attrBoldOn
		   attrSet attr0 (Pair 17)
	           wAddStr win$ show c ++ "            "
		   attrBoldOff
		   attrSet attr0 (Pair 18)
		   --useDefaultColors
		   
                   case c of 
		     KeyUp   -> scrollBufUp   sb 1
		     KeyDown -> scrollBufDown sb 1
		     KeyPPage -> scrollBufUp   sb 10
		     KeyNPage -> scrollBufDown sb 10

		     KeyHome  -> scrollBufToTop sb
		     KeyEnd   -> scrollBufToBottom sb

		     KeyDC    -> scrollBufSet sb ["DELETED"]

		     KeyChar c -> scrollBufAddLine sb [c]
		     _ -> return ()

		   refresh
		   if c == KeyChar '\ETX' 
		     then return() 
		     else  loop (row+1) (n-1)

          loop 22 999999999999



