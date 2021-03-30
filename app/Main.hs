module Main where

import System.Environment ( getArgs )

import Tui
import Shuffle

main :: IO ()
main = do
  args <- getArgs
  if "shuffle" `elem` args
     then print =<< genShuffle 20
     else tui
