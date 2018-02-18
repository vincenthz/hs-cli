-- |
-- Module      : Console.Display
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
-- Displaying utilities
--

{-# LANGUAGE NoImplicitPrelude #-}

module Console.Display
    ( TerminalDisplay
    -- * Basic
    , displayInit
    , display
    , displayTextColor
    , displayLn
    -- * Progress Bar
    , ProgressBar
    , progress
    , progressTick
    -- * Summary line
    , Summary
    , summary
    , summarySet
    -- * Attributes
    , ColorComponent
    , OutputElem(..)
    , termText
    , justify
    -- * Table
    , Justify(..)
    , Table
    , Column
    , columnNew
    , tableCreate
    , tableHeaders
    , tableAppend
    ) where

import Basement.Terminal
import Basement.Terminal.ANSI
import Basement.Types.OffsetSize

import Foundation
import Foundation.Numerical
import Foundation.IO
import Foundation.IO.Terminal
import Foundation.String
import Foundation.Collection

import           System.IO (Handle, hSetBuffering, BufferMode(NoBuffering))

import           Control.Applicative
import           Control.Monad (when)
import           Control.Concurrent.MVar

-- | Element to output text and attributes to the display
data OutputElem =
      Bg ColorComponent
    | Fg ColorComponent
    | T  String
    | LeftT (CountOf Char) String      -- ^ Left-aligned text
    | RightT (CountOf Char) String     -- ^ Right-aligned text
    | CenterT (CountOf Char) String    -- ^ Centered text
    | JustifiedT (CountOf Char) String -- ^ Justified text
    | NA
    deriving (Show,Eq)

-- | Terminal display state
data TerminalDisplay = TerminalDisplay (MVar Bool) Handle

hPutStr :: Handle -> String -> IO ()
hPutStr h = hPut h . toBytes UTF8

termText :: String -> String
termText = id

-- | Create a new display
displayInit :: IO TerminalDisplay
displayInit = do
    initialize
    hSetBuffering stdout NoBuffering
    cf <- newMVar False
    pure $ TerminalDisplay cf stdout

-- | Display
display :: TerminalDisplay -> [OutputElem] -> IO ()
display tdisp@(TerminalDisplay clearFirst term) oelems = do
    cf <- modifyMVar clearFirst $ \cf -> return (False, cf)
    when cf $ hPutStr term eraseLineFromCursor
    hPutStr term $ renderOutput tdisp oelems
  where
        clearLineFirst = eraseLineFromCursor

renderOutput :: TerminalDisplay -> [OutputElem] -> String
renderOutput (TerminalDisplay _ term) to = mconcat $ fmap toString to
  where
        wF = flip sgrForeground False
        wB = flip sgrBackground False
        rD = sgrReset

        toString (Fg c) = wF c
        toString (Bg c) = wB c
        toString (T t)  = t
        toString (LeftT size t)      = justify JustifyLeft      size t
        toString (RightT size t)     = justify JustifyRight     size t
        toString (CenterT size t)    = justify JustifyCenter    size t
        toString (JustifiedT size t) = justify JustifyJustified size t
        toString NA     = rD

-- | A simple utility that display a @msg@ in @color@
displayTextColor :: TerminalDisplay -> ColorComponent -> String -> IO ()
displayTextColor term color msg =
    display term [Fg color, T msg]

-- | A simple utility that display a @msg@ in @color@ and newline at the end.
displayLn :: TerminalDisplay -> ColorComponent -> String -> IO ()
displayLn disp color msg = displayTextColor disp color (msg <> "\n")

white :: ColorComponent
white = 7

-- | A progress bar widget created and used within the `progress` function.
data ProgressBar = ProgressBar TerminalDisplay ProgressBackend (MVar ProgressState)

type ProgressBackend = String -> IO ()

-- | Summary
data Summary = Summary SummaryBackend
type SummaryBackend = [OutputElem] -> IO ()

data ProgressState = ProgressState
    { pgLhs     :: String
    , pgRhs     :: String
    , pgMax     :: Int
    , pgCurrent :: Int
    }

initProgressState :: Int -> ProgressState
initProgressState maxItems = ProgressState
    { pgLhs     = ""
    , pgRhs     = ""
    , pgMax     = maxItems
    , pgCurrent = 0
    }

-- | Create a new progress bar context from a terminal display, a number of
-- items, and a progress update function.
--
-- The progress bar update function should perform the desired actions on each
-- of the items, and call `progressTick` whenever an item is fully processed.
--
-- Each time the given update function calls progressTick the progress bar
-- fills by one item until the given number of items is matched. Once the bar
-- is filled it will not fill any further even if progressTick is called
-- again.
--
-- The progress bar will disappear when the given update function completes
-- running, even if the progress bar is not yet entirely filled.
--
-- For example, the following function will create a progress bar of 100
-- items, and complete one of the items every tenth of a second. Once all of
-- the items are completed the progress bar is removed and a completion String
-- is returned.
--
-- @
-- runProgressBar :: IO String
-- runProgressBar = do
--     terminalDisplay <- displayInit
--     progress terminalDisplay 100 (progressFunction 100)
--   where
--     progressFunction :: Int -> ProgressBar -> IO String
--     progressFunction 0 _   = return "Completed!"
--     progressFunction n bar = do
--       threadDelay 100000
--       progressTick bar
--       progressFunction (n - 1) bar
-- @
progress :: TerminalDisplay       -- ^ The terminal display to display the progress bar on.
         -> Int                   -- ^ The number of items the progress bar represents.
         -> (ProgressBar -> IO a) -- ^ The progression bar update function.
         -> IO a                  -- ^ The results of the progress bar update function.
progress tdisp@(TerminalDisplay cf term) numberItems f = do
    let b = backend (Just cursorDown)
                    (Just "\r")
                    (Just eraseLineFromCursor)
    pbar <- ProgressBar tdisp b <$> newMVar (initProgressState numberItems)

    progressStart pbar
    a <- f pbar
    displayLn tdisp white ""
    return a
  where
    backend :: Maybe (Word64 -> String)
            -> Maybe String
            -> Maybe String
            -> ProgressBackend
    backend _ (Just goHome) (Just clearEol) = \msg -> do
        hPutStr term $ mconcat [clearEol, msg, goHome]
        modifyMVar_ cf $ return . const True
    backend _ _ _ = displayLn tdisp white

showBar :: ProgressBar -> IO ()
showBar (ProgressBar _ backend pgsVar) = do
    pgs <- readMVar pgsVar
    let bar = getBar pgs
    backend bar
  where
    getBar (ProgressState lhs rhs maxItems current) =
            lhs `sep` bar `sep`
            (show current <> "/" <> show maxItems) `sep`
            rhs
      where
        sep s1 s2
            | null s1   = s2
            | null s2   = s1
            | otherwise = s1 <> " " <> s2

        bar
            | maxItems == current = "[" <> replicate (CountOf szMax) fillingChar <> "]"
            | otherwise           = "[" <> replicate (CountOf filled) fillingChar <> ">" <> replicate unfilled' ' ' <> "]"

        fillingChar = '='

        unfilled   = CountOf $ szMax - filled
        unfilled'  = CountOf $ szMax - filled - 1
        filled     = roundDown numberChar

        numberChar = fromIntegral szMax / currentProgress
        szMax      = 40 :: Int

        currentProgress :: Double
        currentProgress = fromIntegral maxItems / fromIntegral current

-- | Start displaying the progress bar
progressStart :: ProgressBar -> IO ()
progressStart pbar = do
    showBar pbar
    return ()

-- | Ticks an element on the given progress bar. Should be used within a
-- progress bar update function that is passed into `progress`.
--
-- @
-- progressFunction :: ProgressBar -> IO String
-- progressFunction bar = do
--   threadDelay 100000
--   progressTick bar
--   threadDelay 200000
--   progressTick bar
--   return "Completed!"
-- @
progressTick :: ProgressBar -> IO ()
progressTick pbar@(ProgressBar _ _ st) = do
    modifyMVar_ st $ \pgs -> return $ pgs { pgCurrent = min (pgMax pgs) (pgCurrent pgs + 1) }
    showBar pbar
    return ()

-- | Create a summary
summary :: TerminalDisplay -> IO Summary
summary tdisp@(TerminalDisplay cf term) = do
    let b = backend (Just cursorDown)
                    (Just "\r")
                    (Just eraseLineFromCursor)
    return $ Summary b
  where
    backend :: Maybe (Word64 -> String)
            -> Maybe String
            -> Maybe String
            -> SummaryBackend
    backend _ (Just goHome) (Just clearEol) = \msg -> do
        hPutStr term $ mconcat [clearEol, renderOutput tdisp msg, goHome]
        modifyMVar_ cf $ return . const True
    backend _ _ _ = \msg ->
        hPutStr term $ mconcat [renderOutput tdisp msg]

-- | Set the summary
summarySet :: Summary -> [OutputElem] -> IO ()
summarySet (Summary backend) = backend

-- | A justification of text.
data Justify = JustifyLeft       -- ^ Left align text
             | JustifyRight      -- ^ Right align text
             | JustifyCenter     -- ^ Center text
             | JustifyJustified  -- ^ Text fills the whole line width

-- | Boxes a string to a given size using the given justification.
--
-- If the size of the given string is greater than or equal to the given boxing
-- size, then the original string is returned.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> justify JustifyRight 35 "Lorem ipsum dolor sit amet"
-- "Lorem ipsum dolor sit amet         "
--
-- >>> justify JustifyLeft 35 "Lorem ipsum dolor sit amet"
-- "         Lorem ipsum dolor sit amet"
--
-- >>> justify JustifyCenter 35 "Lorem ipsum dolor sit amet"
-- "    Lorem ipsum dolor sit amet     "
--
-- >>> justify JustifyJustified 35 "Lorem ipsum dolor sit amet"
-- "Lorem    ipsum   dolor   sit   amet"
--
-- Apply a justified justification to a one word string, resulting in a string
-- of the given length with the word at the left followed by the remaining
-- space.
--
-- >>> justify JustifyJustified 10 "Hello."
-- "Hello.    "
--
-- Attempt to box a string that is larger than the given box, yielding the
-- original string.
--
-- >>> justify JustifyRight 5 "Hello, World!"
-- "Hello, World!"
justify :: Justify -> CountOf Char -> String -> String
justify justification size string
    | size <= stringSize = string
    | otherwise = case justification of
        JustifyLeft      -> padding <> string
        JustifyRight     -> string <> padding
        JustifyCenter    -> if even sizeDifference
          then halfPadding <> string <>       halfPadding
          else halfPadding <> string <> ' ' `cons` halfPadding
        JustifyJustified -> justifyJustified size string
  where
    padding        = replicate sizeDifference ' '
    halfPadding    = replicate (toCount $ fromCount sizeDifference `div` 2) ' '
    sizeDifference = fromMaybe 0 $ size - stringSize
    stringSize     = length string
    even (CountOf n) = (n `mod` 2) == 0

-- | Justifies the given string so that it takes up the space of the entire
-- given box size.
justifyJustified :: CountOf Char -> String -> String
justifyJustified size string
    | numberOfWords == 1 = string <> replicate (toCount lengthDifference) ' '
    | otherwise          = justifiedString
  where
    justifiedString  = intercalate spaces $ insertExcessSpaces (toCount excessChars) stringWords
    spaces           = replicate (toCount spacing) ' '
    excessChars      = lengthDifference `mod` (numberOfWords - 1)
    spacing          = lengthDifference `div` (numberOfWords - 1)
    lengthDifference = fromCount size - wordsLength
    wordsLength      = foldl' (+) 0 $ fmap (fromCount . length) stringWords
    numberOfWords    = fromCount (length stringWords)
    stringWords      = words string

-- | Inserts excess spaces between words for the process of justifying a
-- string.
insertExcessSpaces :: CountOf Char -> [String] -> [String]
insertExcessSpaces _ []     = []
insertExcessSpaces 0 w      = w
insertExcessSpaces n (x:xs) = (x <> " ") : insertExcessSpaces (fromMaybe 0 $ n - 1) xs

{-
data Attr = Attr
    { attrHasSize    :: Maybe Int
    , attrHasJustify :: Maybe Justify
    , attrHasColor   :: Maybe Color
    , attrHasBgColor :: Maybe Color
    , attrText       :: String
    }

instance Monoid Attr where
    mempty = Attr Nothing Nothing Nothing Nothing ""
    mappend a1 a2 =
        Attr { attrHasSize    = attrHasSize a2
             , attrHasJustify = attrHasJustify a2
             , attrHasColor   = attrHasColor a2
             , attrHasBgColor = attrHasBgColor a2
             , attrText       = attrText a1 ++ attrText a2
             }

attrColor :: Color -> Attr -> Attr
attrColor c a = a { attrHasColor = Just c }

attrSize :: Int -> Attr -> Attr
attrSize 0 a = a { attrHasSize = Nothing }
attrSize n a = a { attrHasSize = Just n }

attrJustify :: Justify -> Attr -> Attr
attrJustify j a = a { attrHasJustify = Just j }

text :: String -> Attr
text s = mempty { attrText = s }

toElem :: Attr -> [OutputElem]
toElem attr =
    let j = case attrHasSize attr of
            Just n ->
                case attrHasJustify attr of
                    Nothing           -> [LeftT n " "]
                    Just JustifyLeft  -> [LeftT n " "]
                    Just JustifyRight -> [RightT n " "]
            Nothing ->
                []
        fwc = case attrHasColor attr of
                    Nothing -> []
                    Just c  -> [Fg c]
        bgc = case attrHasBgColor attr of
                    Nothing -> []
                    Just c  -> [Bg c]
     in mconcat [j,fwc,bgc, [T $ attrText attr,NA]]
-}

-- | Column for a table
data Column = Column
    { columnSize    :: CountOf Char     -- ^ Size of the column
    , columnName    :: String  -- ^ Name of the column. used in 'tableHeaders'
    , columnJustify :: Justify -- ^ The justification to use for the cell of this column
    , columnWrap    :: Bool    -- ^ if needed to wrap, whether text disappear or use multi lines row (unimplemented)
    }

-- | Create a new column setting the right default parameters
columnNew :: CountOf Char -> String -> Column
columnNew n name = Column
    { columnSize    = n
    , columnName    = name
    , columnJustify = JustifyLeft
    , columnWrap    = False
    }

-- | Table widget
data Table = Table
    { tColumns     :: [Column]
    , rowSeparator :: String
    }

-- | Create a new table
tableCreate :: [Column] -> Table
tableCreate cols = Table { tColumns = cols, rowSeparator = "" }

-- | Show the table headers
tableHeaders :: TerminalDisplay -> Table -> IO ()
tableHeaders td t =
    tableAppend td t $ columnName <$> tColumns t

-- | Append a row to the table.
--
-- if the number of elements is greater than the amount of
-- column the table has been configured with, the extra
-- elements are dropped.
tableAppend :: TerminalDisplay -> Table -> [String] -> IO ()
tableAppend td (Table cols rowSep) l = do
    let disp = case compare numelems numcols of
                        LT -> zip cols (l <> replicate (fromMaybe 0 $ numcols - numelems) "")
                        _  -> zip cols l :: [(Column, String)]
    mapM_ printColRow $ intersperse Nothing $ fmap Just disp
  where
    numelems = length l
    numcols  = sizeCast Proxy (length cols) :: CountOf String
    printColRow Nothing =
        display td [T rowSep]
    printColRow (Just (c, fieldElement)) = do
        let oe = case columnJustify c of
                   JustifyLeft      -> RightT     (columnSize c) fieldElement
                   JustifyRight     -> LeftT      (columnSize c) fieldElement
                   JustifyCenter    -> CenterT    (columnSize c) fieldElement
                   JustifyJustified -> JustifiedT (columnSize c) fieldElement
        display td [oe,T "\n"]
