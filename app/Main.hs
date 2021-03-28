module Main where

import Shuffle ( genShuffle )
import Tui

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes


main :: IO ()
-- main = print =<< genShuffle 20
main = tui
