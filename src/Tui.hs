{-# LANGUAGE OverloadedStrings, NumericUnderscores #-}

module Tui ( tui ) where

import Shuffle

import Control.Monad.IO.Class ( liftIO )
import Control.Concurrent ( forkIO, threadDelay, ThreadId, killThread )
import Control.Monad      ( forever, void )
import Data.Time.Clock
import Data.Time.Format

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.BChan
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border
import Graphics.Vty


-- | Draws the Kostka TUI
tui :: IO ()
tui = do
  eventChan    <- Brick.BChan.newBChan 10
  initialState <- buildInitialState eventChan
  let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
  initialVty <- buildVty

  void $ customMain initialVty buildVty (Just eventChan) tuiApp initialState

-- Generates an event for the countdown until the timer starts
countdownTick :: BChan CustomEvent -> IO ()
countdownTick chan = go 3
  where go 0 = writeBChan chan $ CountDown 0
        go n = do
          writeBChan chan $ CountDown n
          threadDelay 1_000_000
          go (n-1)

-- Starts the timer and generates an event every centisecond to update the timer in the UI
timerTick :: BChan CustomEvent -> IO ()
timerTick chan = do
  start <- getCurrentTime
  forever $ do
    now <- getCurrentTime
    let diff = diffUTCTime now start
    writeBChan chan $ TimerTick $ timeFormat diff
    threadDelay 10_000

-- Formats the time diff
timeFormat :: NominalDiffTime -> String
timeFormat ts =
  let formatted = formatTime defaultTimeLocale "%m:%020ES" ts
      numDecimals = length $ takeWhile (/= '.') $ reverse formatted
  in reverse $ drop (numDecimals - 2) $ reverse formatted

data CustomEvent = CountDown Int | TimerTick String

data TuiState = TuiState
  { tuiStateShuffle     :: String
  , tuiStateMain        :: MainState
  , tuiStateEventChan   :: BChan CustomEvent
  , tuiStateTickThread  :: Maybe ThreadId
  , tuiStateTime        :: String
  }

data MainState = Passive | CountingDown Int | Running
  deriving (Eq, Show)

data ResourceName =
  ResourceName
  deriving (Show, Eq, Ord)

tuiApp :: App TuiState CustomEvent ResourceName
tuiApp =
  App
    { appDraw         = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent  = handleTuiEvent
    , appStartEvent   = pure
    , appAttrMap      = const $ attrMap mempty ourAttrMap
    }

ourAttrMap :: [(AttrName, Attr)]
ourAttrMap =
  [ ("holding", fg red )
  , ("held"   , fg green)
  ]

newShuffle :: IO String
newShuffle = show <$> genShuffle 20

buildInitialState :: BChan CustomEvent -> IO TuiState
buildInitialState chan = do
  shuffle <- newShuffle
  pure $ TuiState
    { tuiStateShuffle     = shuffle
    , tuiStateMain        = Passive
    , tuiStateEventChan = chan
    , tuiStateTickThread  = Nothing
    , tuiStateTime        = "0.00"
    }

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts =
  let border = borderWithLabel (str " Kostka Rubik's Cube Timer ")
      shuffle = hCenter $ padTop (Pad 1) $ str (tuiStateShuffle ts)
      hold    = holdWidget ts
      time    = timeWidget ts
      contents =
        shuffle <=>
        hold    <=>
        time
  in [ border $ center contents ]

-- The widget prompting the user to press space
holdWidget :: TuiState -> Widget n
holdWidget TuiState{tuiStateMain = m} =
  let skeleton = hCenter . padTop (Pad 1) . str
      holdText = "press space to start countdown"
   in case m of
        Passive        -> skeleton holdText
        CountingDown n -> withAttr "holding" . skeleton $ show n
        Running        -> str ""

-- The widget displaying the main timer
timeWidget :: TuiState -> Widget n
timeWidget TuiState{tuiStateTime = s} = hCenter $ padTop (Pad 1) $ str s

-- Handle keyboard and custom events
handleTuiEvent :: TuiState -> BrickEvent n CustomEvent -> EventM n (Next TuiState)

-- Handle the countdown event, i.e. the countdown until the timer starts
handleTuiEvent s (AppEvent (CountDown 0)) = do
  -- start a new thread for the timer and save its PID in the state
  threadId <- liftIO $ forkIO $ timerTick (tuiStateEventChan s)
  continue $ s { tuiStateMain       = Running
               , tuiStateTickThread = Just threadId
               }
handleTuiEvent s (AppEvent (CountDown n)) = continue $ s { tuiStateMain = CountingDown n }

-- Handle the timer tick to update the UI state
handleTuiEvent s (AppEvent (TimerTick t)) = continue $ s { tuiStateTime = t }

-- Handle keyboard events
handleTuiEvent s (VtyEvent vtye) =
  case vtye of
    EvKey (KChar 'q') [] -> halt s
    EvKey (KChar 's') [] -> do
      shuffle' <- liftIO newShuffle
      continue $ s { tuiStateShuffle = shuffle' }
    EvKey (KChar ' ') [] -> case tuiStateMain s of
        -- Start the countdown
        Passive -> do
          liftIO $ forkIO $ countdownTick (tuiStateEventChan s)
          continue $ s { tuiStateMain = CountingDown 3 }
        -- Stop the timer
        Running -> do
          -- kill the timer thread
          maybe (pure ()) (liftIO . killThread) (tuiStateTickThread s)
          shuffle' <- liftIO newShuffle
          continue $ s { tuiStateMain       = Passive
                       , tuiStateTickThread = Nothing
                       , tuiStateShuffle    = shuffle'
                       }
        _ -> continue s
    _                    -> continue s

