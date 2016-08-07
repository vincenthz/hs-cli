-- |
-- Module      : Console.Display
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
-- Displaying utilities
--
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
    , Color(..)
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

import           Control.Applicative
import           Control.Monad
import           Control.Concurrent.MVar
import           System.Console.Terminfo
import           System.IO
import           Data.List

-- | Element to output text and attributes to the display
data OutputElem =
      Bg Color
    | Fg Color
    | T  String
    | LeftT Int String
    | RightT Int String
    | NA
    deriving (Show,Eq)

-- | Terminal display state
data TerminalDisplay = TerminalDisplay (MVar Bool) Terminal

-- | Create a new display
displayInit :: IO TerminalDisplay
displayInit = do
    hSetBuffering stdout NoBuffering
    cf <- newMVar False
    TerminalDisplay cf <$> setupTermFromEnv

-- | Display
display :: TerminalDisplay -> [OutputElem] -> IO ()
display tdisp@(TerminalDisplay clearFirst term) oelems = do
    cf <- modifyMVar clearFirst $ \cf -> return (False, cf)
    when cf $ runTermOutput term (maybe mempty id clearLineFirst)
    runTermOutput term $ renderOutput tdisp oelems
  where
        clearLineFirst = getCapability term clearEOL

renderOutput :: TerminalDisplay -> [OutputElem] -> TermOutput
renderOutput (TerminalDisplay _ term) to = mconcat $ map toTermOutput to
  where
        wF = maybe (const mempty) id $ getCapability term setForegroundColor
        wB = maybe (const mempty) id $ getCapability term setBackgroundColor
        rD = maybe mempty         id $ getCapability term restoreDefaultColors

        toTermOutput (Fg c) = wF c
        toTermOutput (Bg c) = wB c
        toTermOutput (T t)  = termText t
        toTermOutput (LeftT sz t)  = termText (t ++ replicate (sz - length t) ' ')
        toTermOutput (RightT sz t)  = termText (replicate (sz - length t) ' ' ++ t)
        toTermOutput NA     = rD

-- | A simple utility that display a @msg@ in @color@
displayTextColor :: TerminalDisplay -> Color -> String -> IO ()
displayTextColor term color msg = do
    display term [Fg color, T msg]

-- | A simple utility that display a @msg@ in @color@ and newline at the end.
displayLn :: TerminalDisplay -> Color -> String -> IO ()
displayLn disp color msg = displayTextColor disp color (msg ++ "\n")

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
    let b = backend (getCapability term cursorDown)
                    (getCapability term carriageReturn)
                    (getCapability term clearEOL)
    pbar <- ProgressBar tdisp b <$> newMVar (initProgressState numberItems)

    progressStart pbar
    a <- f pbar
    displayLn tdisp White ""
    return a
  where
    backend :: Maybe (Int -> TermOutput)
            -> Maybe TermOutput
            -> Maybe TermOutput
            -> ProgressBackend
    backend _ (Just goHome) (Just clearEol) = \msg -> do
        runTermOutput term $ mconcat [clearEol, termText msg, goHome]
        modifyMVar_ cf $ return . const True
    backend _ _ _ = \msg ->
        displayLn tdisp White msg

showBar :: ProgressBar -> IO ()
showBar (ProgressBar _ backend pgsVar) = do
    pgs <- readMVar pgsVar
    let bar = getBar pgs
    backend bar
  where
    getBar (ProgressState lhs rhs maxItems current) =
            lhs `sep` bar `sep`
            (show current ++ "/" ++ show maxItems) `sep`
            rhs
      where
        sep s1 s2
            | null s1   = s2
            | null s2   = s1
            | otherwise = s1 ++ " " ++ s2

        bar
            | maxItems == current = "[" ++ replicate szMax fillingChar ++ "]"
            | otherwise           = "[" ++ replicate filled fillingChar ++ ">" ++ replicate (unfilled-1) ' ' ++ "]"

        fillingChar = '='

        unfilled, filled :: Int
        unfilled   = szMax - filled
        filled     = floor numberChar

        numberChar = fromIntegral szMax / currentProgress
        szMax      = 40

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
    let b = backend (getCapability term cursorDown)
                    (getCapability term carriageReturn)
                    (getCapability term clearEOL)
    return $ Summary b
  where
    backend :: Maybe (Int -> TermOutput)
            -> Maybe TermOutput
            -> Maybe TermOutput
            -> SummaryBackend
    backend _ (Just goHome) (Just clearEol) = \msg -> do
        runTermOutput term $ mconcat [clearEol, renderOutput tdisp msg, goHome]
        modifyMVar_ cf $ return . const True
    backend _ _ _ = \msg ->
        runTermOutput term $ mconcat [renderOutput tdisp msg]

-- | Set the summary
summarySet :: Summary -> [OutputElem] -> IO ()
summarySet (Summary backend) output = do
    backend output

-- | Justify position
data Justify = JustifyLeft | JustifyRight

-- | box a string to a specific size, choosing the justification
justify :: Justify -> Int -> String -> String
justify dir sz s
    | sz <= szS = s
    | otherwise =
        let pad = replicate (sz - szS) ' '
         in case dir of
                JustifyLeft  -> pad ++ s
                JustifyRight -> s ++ pad
  where
    szS = length s
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
    { columnSize    :: Int     -- ^ Size of the column
    , columnName    :: String  -- ^ Name of the column. used in 'tableHeaders'
    , columnJustify :: Justify -- ^ The justification to use for the cell of this column
    , columnWrap    :: Bool    -- ^ if needed to wrap, whether text disappear or use multi lines row (unimplemented)
    }

-- | Create a new column setting the right default parameters
columnNew :: Int -> String -> Column
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
    tableAppend td t $ map columnName $ tColumns t

-- | Append a row to the table.
--
-- if the number of elements is greater than the amount of
-- column the table has been configured with, the extra
-- elements are dropped.
tableAppend :: TerminalDisplay -> Table -> [String] -> IO ()
tableAppend td (Table cols rowSep) l = do
    let disp = case compare (length l) (length cols) of
                        LT -> zip cols (l ++ replicate (length cols - length l) "")
                        _  -> zip cols l
    mapM_ printColRow $ intersperse Nothing $ map Just disp
  where
    printColRow Nothing =
        display td [T rowSep]
    printColRow (Just (c, fieldElement)) = do
        let oe = case columnJustify c of
                   JustifyLeft  -> RightT (columnSize c) fieldElement
                   JustifyRight -> LeftT (columnSize c) fieldElement
        display td [oe,T "\n"]
