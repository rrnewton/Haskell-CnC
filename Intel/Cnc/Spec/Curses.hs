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
-- It creates a bounding border (box)
box win (y,x) (h,w) = 
  do let horiz = "+"++ (Prelude.take (w-2) $ repeat '-') ++"+"
     mvWAddStr win y x horiz
     mvWAddStr win (y + h - 1) x horiz
     forM_ [1..h-2] $ \r -> mvWAddStr win (y + r) x "|"
     forM_ [1..h-2] $ \r -> mvWAddStr win (y + r) (x + w - 1) "|"
     

------------------------------------------------------------------------------------------------------------------------
-- Scrolling Text Windows (Buffers)
------------------------------------------------------------------------------------------------------------------------
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

scrollBufSet :: ScrollBuf -> [String] -> IO ()
scrollBufSet ref lines = 
  do sb <- readIORef ref
     let new = sb{ bottom=True, offscreen= Seq.empty, visible= Seq.fromList lines }
     writeIORef ref new
     drawScrollBuf new

viewLtoList EmptyL = []
viewLtoList (a :< rest) = a : viewLtoList (viewl rest)

----------------------------------------------------------------------------------------------------
-- Labeled text fields.
-- These are something simpler... just a single-line text field that is updated.
----------------------------------------------------------------------------------------------------

-- These are drawn with wAddStr directly rather than the "textWidget"

data TextFieldRec = TF {
    contents :: String, 
    tfpos :: Pos,
    width :: Int
--    hookOn  :: [IO ()],
--    hookOff :: [IO ()]
}

type TextField = IORef TextFieldRec

--mkTextField :: Window -> String -> Pos -> Int -> [IO ()] -> [IO ()] -> IO TextField
mkTextField :: Window -> String -> Pos -> Int -> IO TextField
mkTextField win label (y,x) width {-onhk offhk-} = 
  do 
     mvWAddStr win y x label
     newIORef $ TF "" (y, x + Prelude.length label) (width - Prelude.length label) -- onhk offhk

setTextField :: Window -> TextField -> String -> IO ()
setTextField win ref str = 
  do tf <- readIORef ref
     let (y,x) = tfpos tf
     -- Crop the right end of the string:
     mvWAddStr win y x (Prelude.take (width tf) str)
     wAddStr win (Prelude.take (width tf - Prelude.length str) $ repeat ' ')
     writeIORef ref tf{ contents = str }

-- Add text to a field rather than overwriting.  Allows building up contents with different styles.
appendTextField win ref str = 
  do tf <- readIORef ref
     let (y,x) = tfpos tf
	 len = Prelude.length $ contents tf
     mvWAddStr win y (x + len) (Prelude.take (width tf - len) str)
     writeIORef ref tf{ contents = contents tf ++ str }

-- If appending bits of text, cap when finished to blank the remainder of the text field.
capTextField win ref = 
  do tf <- readIORef ref
     let (y,x) = tfpos tf
	 len = Prelude.length $ contents tf
     mvWAddStr win y (x + len) (Prelude.take (width tf - len) $ repeat ' ')

-- Multiple text fields across a line:
textFieldsLine :: Window -> Int -> [String] -> IO [TextField]
textFieldsLine win row labels = 
  do size@(height,width) <- scrSize
     let len = Prelude.length labels
	 portion = width `quot` len
     forM (Prelude.zip [0 .. len-1] labels) $ \ (i,lab) -> do
       mkTextField win lab (row, i * portion) portion --[] []
       
     

----------------------------------------------------------------------------------------------------

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
          box win (sb_start+1,1) (height-sb_start-1,width-4)
          refresh

          --sb <- mkScrollBuf (sb_start+2,4) (15,width-6)
	  sb <- mkScrollBuf (sb_start+2,3) (height-sb_start-3,width-7)

          move 1 0
	  [redstyle, yellow, grey, green] <- 
	      convertStyles [Style DarkRedF BlackB, Style YellowF BlackB, 
			     Style GreyF BlackB, Style DarkGreenF BlackB ]
	  withStyle redstyle $ drawLine width (repeat '=')

          centeredLine win 0 "CnC Interactive Trace Visualizer"

	  --mvWAddStr win 2 0 $ "Trace events:          Time elapsed:" 
	  --mvWAddStr win 3 0 $ "Current time:" 
	  let ln1 = ["Total elapsed time: ", " Trace events: " ]
	      ln2 = ["      Current time: ", "Current event: "]
	  [totaltime, numevents]        <- withStyle grey$ textFieldsLine win 2 ln1
	  [current_time, current_event] <- withStyle grey$ textFieldsLine win 3 ln2

	  attrBoldOn
          setTextField win current_event "0"
          setTextField win totaltime "99"
          setTextField win numevents "lots of them"
	  attrBoldOff

	  mvWAddStr win 5 0 $ "Enter keyboard input (? or 'h' for help):" 

	  let cursor_pos = (6,2)
	  withStyle yellow$ mvWAddStr win (fst cursor_pos) 0 $ "> " 

          gotoTop 

	  scrollBufAddLine sb "hello"
	  scrollBufAddLine sb "yay"
	  mapM_ (scrollBufAddLine sb) $ map show [1..200]
	  refresh


          ----------------------------------------
	  move (20) 40
	  colors <- hasColors 
	  refresh
	  size@(height,width) <- scrSize
	  [user_input, cursor_field] <- withStyle green$ textFieldsLine win (fst cursor_pos) ["> ", ""]
	  event_counter <- newIORef 0 

	  --------------------------------------------------------------------------------
	  -- Begin main event loop
	  --------------------------------------------------------------------------------
	  let event_loop row 0 = return ()
	      event_loop row n = do
		   withStyle green$ box win (sb_start+1,1) (height-sb_start-1,width-4)
                   uncurry move cursor_pos
		   refresh

		   c <- Help.getKey refresh
		   setTextField win cursor_field $ "You pressed: "

		   attrBoldOn
		   attrSet attr0 (Pair 17)
		   appendTextField win cursor_field $ show c
		   capTextField win cursor_field 
		   attrBoldOff
		   attrSet attr0 (Pair 18)
		   --useDefaultColors
		   
                   case c of 
		     KeyUp   -> scrollBufUp   sb 1
		     KeyDown -> scrollBufDown sb 1

		     KeyLeft  -> do c <- readIORef event_counter 
				    if c == 0 
				     then setTextField win current_event "0"
				     else do writeIORef event_counter (c-1)
					     setTextField win current_event (show$ c-1)
		     KeyRight -> do c <- readIORef event_counter 
				    if c == 999999 
				     then setTextField win current_event (show c)
				     else do writeIORef event_counter (c+1)
					     setTextField win current_event (show$ c+1)

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
		     else event_loop (row+1) (n-1)
	  --------------------------------------------------------------------------------
	  -- End event_loop definition
	  --------------------------------------------------------------------------------
	  event_loop 22 999999999999

          endWin 
	  Help.end
	  --update

          putStrLn$ "Exited ncurses"
          return ()
          



