{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

--------------------------------------------------------------------------------
-- This is an adaption of GHC's SrcLoc.lhs
--
-- Copyright 2004, The University Court of the University of Glasgow.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:

--     * Redistributions of source code must retain the above
--     * copyright notice, this list of conditions and the following
--     * disclaimer.

--     * Redistributions in binary form must reproduce the above
--     * copyright notice, this list of conditions and the following
--     * disclaimer in the documentation and/or other materials
--     * provided with the distribution.

--     * Neither name of the University nor the names of its
--     * contributors may be used to endorse or promote products
--     * derived from this software without specific prior written
--     * permission.

-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY COURT OF THE UNIVERSITY
-- OF GLASGOW AND THE CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
-- WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
-- OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY COURT OF THE
-- UNIVERSITY OF GLASGOW OR THE CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
-- INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
-- OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------------------------------------

module Intel.Cnc.Spec.SrcLoc where
import Data.Data
import Data.Bits
import Text.PrettyPrint.HughesPJClass

 -- I don't actually see why we would need interned strings for
 -- filenames.  How many unique files are they?  They should be shared
 -- properly even as normal strings.  And how often do they need to be
 -- compared?
--type FileNameString = Atom
type FileNameString = String
mkFileNameString = id

data SrcLoc = SrcLoc {
		srcFilename :: FileNameString,
		srcLine     :: {-# UNPACK #-} !Int,
		srcColumn   :: {-# UNPACK #-} !Int
		}
  | UnhelpfulLoc FileNameString	-- Just a general indication
 deriving (Eq,Ord,Show,Typeable,Data)

-- data SrcLoc
-- --  = SrcLoc	Atom	-- A precise location (file name)
--   = SrcLoc	String	-- A precise location (file name)
-- 		{-# UNPACK #-} !Int		-- line number, begins at 1
-- 		{-# UNPACK #-} !Int		-- column number, begins at 1
--  deriving (Eq,Ord,Show,Data,Typeable)

--data Loc a = Loc SrcLoc a  deriving (Eq,Ord,Show)

--unknownLoc = SrcLoc "<unknown file>" 0 0 

mkSrcLoc :: FileNameString -> Int -> Int -> SrcLoc
mkSrcLoc x line col = SrcLoc x line col

-- | Built-in "bad" 'SrcLoc' values for particular locations
noSrcLoc, generatedSrcLoc :: SrcLoc -- interactiveSrcLoc 
noSrcLoc	  = UnhelpfulLoc (mkFileNameString "<no location info>")
generatedSrcLoc   = UnhelpfulLoc (mkFileNameString "<compiler-generated code>")
--interactiveSrcLoc = UnhelpfulLoc (mkFileNameString "<interactive session>")

-- | Creates a "bad" 'SrcLoc' that has no detailed information about its location
mkGeneralSrcLoc :: FileNameString -> SrcLoc
mkGeneralSrcLoc = UnhelpfulLoc 




-- | Move the 'SrcLoc' down by one line if the character is a newline,
-- to the next 8-char tabstop if it is a tab, and across by one
-- character in any other case
advanceSrcLoc :: SrcLoc -> Char -> SrcLoc
advanceSrcLoc (SrcLoc f l _) '\n' = SrcLoc f  (l + 1) 1
advanceSrcLoc (SrcLoc f l c) '\t' = SrcLoc f  l (((((c - 1) `shiftR` 3) + 1)
                                                  `shiftL` 3) + 1)
advanceSrcLoc (SrcLoc f l c) _    = SrcLoc f  l (c + 1)


{- |
A SrcSpan delimits a portion of a text file.  It could be represented
by a pair of (line,column) coordinates, but in fact we optimise
slightly by using more compact representations for single-line and
zero-length spans, both of which are quite common.

The end position is defined to be the column /after/ the end of the
span.  That is, a span of (1,1)-(1,2) is one character long, and a
span of (1,1)-(1,1) is zero characters long.
-}
data SrcSpan
  = SrcSpanOneLine 		-- a common case: a single line
	{ srcSpanFile     :: !FileNameString,
	  srcSpanLine     :: {-# UNPACK #-} !Int,
	  srcSpanSCol     :: {-# UNPACK #-} !Int,
	  srcSpanECol     :: {-# UNPACK #-} !Int
	}

  | SrcSpanMultiLine
	{ srcSpanFile	  :: !FileNameString,
	  srcSpanSLine    :: {-# UNPACK #-} !Int,
	  srcSpanSCol	  :: {-# UNPACK #-} !Int,
	  srcSpanELine    :: {-# UNPACK #-} !Int,
	  srcSpanECol     :: {-# UNPACK #-} !Int
	}

  | SrcSpanPoint
	{ srcSpanFile	  :: !FileNameString,
	  srcSpanLine	  :: {-# UNPACK #-} !Int,
	  srcSpanCol      :: {-# UNPACK #-} !Int
	}

  | UnhelpfulSpan !FileNameString  -- Just a general indication
				   -- also used to indicate an empty span
 deriving (Eq,Typeable,Data)


-- | Create a 'SrcSpan' corresponding to a single point
srcLocSpan :: SrcLoc -> SrcSpan
srcLocSpan (UnhelpfulLoc str) = UnhelpfulSpan str
srcLocSpan (SrcLoc file line col) = SrcSpanPoint file line col


-- | Create a 'SrcSpan' between two points in a file mkSrcSpan :: SrcLoc -> SrcLoc -> SrcSpan
mkSrcSpan (UnhelpfulLoc str) _ = UnhelpfulSpan str
mkSrcSpan _ (UnhelpfulLoc str) = UnhelpfulSpan str
mkSrcSpan loc1 loc2
  | line1 == line2 = if col1 == col2
			then SrcSpanPoint file line1 col1
			else SrcSpanOneLine file line1 col1 col2
  | otherwise      = SrcSpanMultiLine file line1 col1 line2 col2
  where
	line1 = srcLine loc1
	line2 = srcLine loc2
	col1 = srcColumn loc1
	col2 = srcColumn loc2
	file = srcFilename loc1

-- | Combines two 'SrcSpan' into one that spans at least all the characters
-- within both spans. Assumes the "file" part is the same in both inputs
combineSrcSpans	:: SrcSpan -> SrcSpan -> SrcSpan
combineSrcSpans	(UnhelpfulSpan _) r = r -- this seems more useful
combineSrcSpans	l (UnhelpfulSpan _) = l
combineSrcSpans	start end 
 = case line1 `compare` line2 of
     EQ -> case col1 `compare` col2 of
		EQ -> SrcSpanPoint file line1 col1
		LT -> SrcSpanOneLine file line1 col1 col2
		GT -> SrcSpanOneLine file line1 col2 col1
     LT -> SrcSpanMultiLine file line1 col1 line2 col2
     GT -> SrcSpanMultiLine file line2 col2 line1 col1
  where
	line1 = srcSpanStartLine start
	col1  = srcSpanStartCol start
	line2 = srcSpanEndLine end
	col2  = srcSpanEndCol end
	file  = srcSpanFile start

-- | Test if a 'SrcSpan' is "good", i.e. has precise location information
isGoodSrcSpan :: SrcSpan -> Bool
isGoodSrcSpan SrcSpanOneLine{} = True
isGoodSrcSpan SrcSpanMultiLine{} = True
isGoodSrcSpan SrcSpanPoint{} = True
isGoodSrcSpan _ = False

isOneLineSpan :: SrcSpan -> Bool
-- ^ True if the span is known to straddle only one line.
-- For "bad" 'SrcSpan', it returns False
isOneLineSpan s
  | isGoodSrcSpan s = srcSpanStartLine s == srcSpanEndLine s
  | otherwise	    = False		


-- | Raises an error when used on a "bad" 'SrcSpan'
srcSpanStartLine :: SrcSpan -> Int
-- | Raises an error when used on a "bad" 'SrcSpan'
srcSpanEndLine :: SrcSpan -> Int
-- | Raises an error when used on a "bad" 'SrcSpan'
srcSpanStartCol :: SrcSpan -> Int
-- | Raises an error when used on a "bad" 'SrcSpan'
srcSpanEndCol :: SrcSpan -> Int

panic = error

srcSpanStartLine SrcSpanOneLine{ srcSpanLine=l } = l
srcSpanStartLine SrcSpanMultiLine{ srcSpanSLine=l } = l
srcSpanStartLine SrcSpanPoint{ srcSpanLine=l } = l
srcSpanStartLine _ = panic "SrcLoc.srcSpanStartLine"

srcSpanEndLine SrcSpanOneLine{ srcSpanLine=l } = l
srcSpanEndLine SrcSpanMultiLine{ srcSpanELine=l } = l
srcSpanEndLine SrcSpanPoint{ srcSpanLine=l } = l
srcSpanEndLine _ = panic "SrcLoc.srcSpanEndLine"

srcSpanStartCol SrcSpanOneLine{ srcSpanSCol=l } = l
srcSpanStartCol SrcSpanMultiLine{ srcSpanSCol=l } = l
srcSpanStartCol SrcSpanPoint{ srcSpanCol=l } = l
srcSpanStartCol _ = panic "SrcLoc.srcSpanStartCol"

srcSpanEndCol SrcSpanOneLine{ srcSpanECol=c } = c
srcSpanEndCol SrcSpanMultiLine{ srcSpanECol=c } = c
srcSpanEndCol SrcSpanPoint{ srcSpanCol=c } = c
srcSpanEndCol _ = panic "SrcLoc.srcSpanEndCol"




-- | Returns the location at the start of the 'SrcSpan' or a "bad" 'SrcSpan' if that is unavailable
srcSpanStart :: SrcSpan -> SrcLoc
-- | Returns the location at the end of the 'SrcSpan' or a "bad" 'SrcSpan' if that is unavailable
srcSpanEnd :: SrcSpan -> SrcLoc

srcSpanStart (UnhelpfulSpan str) = UnhelpfulLoc str
srcSpanStart s = mkSrcLoc (srcSpanFile s) 
			  (srcSpanStartLine s)
		 	  (srcSpanStartCol s)

srcSpanEnd (UnhelpfulSpan str) = UnhelpfulLoc str
srcSpanEnd s = 
  mkSrcLoc (srcSpanFile s) 
	   (srcSpanEndLine s)
 	   (srcSpanEndCol s)

-- | Obtains the filename for a 'SrcSpan' if it is "good"
srcSpanFileName_maybe :: SrcSpan -> Maybe FileNameString
srcSpanFileName_maybe (SrcSpanOneLine { srcSpanFile = nm })   = Just nm
srcSpanFileName_maybe (SrcSpanMultiLine { srcSpanFile = nm }) = Just nm
srcSpanFileName_maybe (SrcSpanPoint { srcSpanFile = nm})      = Just nm
srcSpanFileName_maybe _                                       = Nothing

srcSpanSetFileName :: FileNameString -> SrcSpan -> SrcSpan
srcSpanSetFileName file (s@SrcSpanOneLine{..})   = s { srcSpanFile = file }
srcSpanSetFileName file (s@SrcSpanMultiLine{..}) = s { srcSpanFile = file }
srcSpanSetFileName file (s@SrcSpanPoint{..})     = s { srcSpanFile = file }
srcSpanSetFileName file (UnhelpfulSpan _)        = UnhelpfulSpan file


-- We want to order SrcSpans first by the start point, then by the end point.
instance Ord SrcSpan where
  a `compare` b = 
     (srcSpanStart a `compare` srcSpanStart b) `thenCmp` 
     (srcSpanEnd   a `compare` srcSpanEnd   b)

-- | Determines whether a span encloses a given line and column index
spans :: SrcSpan -> (Int, Int) -> Bool
spans span (l,c) = srcSpanStart span <= loc && loc <= srcSpanEnd span
   where loc = mkSrcLoc (srcSpanFile span) l c

-- | Determines whether a span is enclosed by another one
isSubspanOf :: SrcSpan -- ^ The span that may be enclosed by the other
            -> SrcSpan -- ^ The span it may be enclosed by
            -> Bool
isSubspanOf src parent 
    | srcSpanFileName_maybe parent /= srcSpanFileName_maybe src = False
    | otherwise = srcSpanStart parent <= srcSpanStart src &&
                  srcSpanEnd parent   >= srcSpanEnd src

----------------------------------------------------------------------------------------------------

instance Pretty SrcLoc where
  pPrint (UnhelpfulLoc s) = pPrint s
  pPrint (SrcLoc f l c) = pPrint f <+> text "line " <> int l <> text ", column " <> int c

-- Eventually this should print a snippet of the file:
-- NOTE: We convert 0-indexed lines to 1-indexed lines for emacs compatibility:
-- Hmm... I'm not sure about columns.
instance Pretty SrcSpan where
  pPrint span = 
      let startL = (0+)$ srcLine$   srcSpanStart span
	  startC =       srcColumn$ srcSpanStart span
	  endL   = (0+)$ srcLine$   srcSpanEnd span
	  endC   =       srcColumn$ srcSpanEnd span
      in
      sep [text ("file " ++ (srcFilename $ srcSpanStart span)),
	   if (startL,startC) == (endL,endC)
	   then text $ "at line:column "       ++ (show startL) ++ ":" ++ (show startC)
	   else text $ "between line:column " ++ (show startL) ++ ":" ++ (show startC)
   	               ++ " and " ++ (show endL) ++ ":" ++ (show endC)]


--              pPrint (srcSpanStart span) <> text " : " <>
--		pPrint (srcSpanEnd span)

-- Might as well use the pretty version for plain show:
instance Show SrcSpan where
    show = show . pPrint 

-- An error with a location.
locErr span msg = 
  error$ msg ++ "\n Location: "++ (show span)

----------------------------------------------------------------------------------------------------

infixr 9 `thenCmp`

thenCmp :: Ordering -> Ordering -> Ordering
{-# INLINE thenCmp #-}
thenCmp EQ       ordering = ordering
thenCmp ordering _        = ordering

