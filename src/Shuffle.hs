module Shuffle ( genShuffle ) where

import System.Random
import Data.List      ( delete )
import Data.Maybe

data Move = U | D | L | R | F | B
  deriving ( Eq, Show, Enum )

newtype Shuffle = Shuffle [Move]

opposite :: Move -> Move
opposite U = D
opposite D = U
opposite L = R
opposite R = L
opposite F = B
opposite B = F

instance Semigroup Shuffle where
  Shuffle ms <> Shuffle ns = Shuffle (ms <> ns)

nil :: Shuffle
nil = Shuffle []

showMoves :: Shuffle -> [String]
showMoves (Shuffle (m1:m2:m3:ms))
  | m1 == m2 && m1 == m3   = (show m1 <> "'") : showMoves (Shuffle ms)
showMoves (Shuffle (m1:m2:ms))
  | m1 == m2               = (show m1 <> "2") : showMoves (Shuffle ms)
showMoves (Shuffle (m:ms)) = show m : showMoves (Shuffle ms)
showMoves (Shuffle [])     = [""]

instance Show Shuffle where
  show = unwords . showMoves

randomSide :: StdGen -> Maybe Move -> (Move, StdGen)
randomSide g prev =
  let sides = [U,D,L,R,F,B]
      (ix, g') = maybe (randomR (0,5) g) (const $ randomR (0,3) g) prev
      move = maybe sides (\p -> delete (opposite p) . delete p $ sides) prev !! ix
  in (move, g')

randomNum :: StdGen -> (Int, StdGen)
randomNum g =
  let distribution = [1,1,2,3,3] -- TODO should it really be less likely to get double?
      (ix, g') = randomR (0,4) g
      num = distribution !! ix
  in (num, g')

genShuffle :: Int -> IO Shuffle
genShuffle n = do
  g <- newStdGen
  let (_,s,_) = go (n, nil, g)
  pure s

  where

    go :: (Int, Shuffle, StdGen) -> (Int, Shuffle, StdGen)
    go res@(0,_,_)            = res
    go (n, s@(Shuffle ms), g) =
      let (side, g') = randomSide g (listToMaybe ms)
          (num, g'') = randomNum g'
          moves      = Shuffle $ replicate num side
      in go (n-1, moves <> s, g'')
