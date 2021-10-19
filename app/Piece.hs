module Piece
  ( Piece (I, L, O, T, Z)
  , Colour (..)
  , cw, ccw
  , flipH, flipV
  , pieceToList
  ) where

import           Control.Arrow        ((&&&))
import           Data.Bifunctor       (bimap, first, second)
import           Data.List            (transpose)
import           Data.Maybe           (fromJust)

import           Control.Monad.Random

data Colour
  = Red   | Cyan
  | Blue  | Yellow
  | Green | Magenta
  | White
  deriving (Eq, Enum, Bounded)

instance Random Colour where
  randomR (c1, c2) = first toEnum . randomR (fromEnum c1, fromEnum c2)
  random g = first toEnum $ randomR (0, fromEnum $ maxBound @Colour) g

inv :: Colour -> Colour
inv White   = White
inv Red     = Cyan
inv Cyan    = Red
inv Blue    = Yellow
inv Yellow  = Blue
inv Green   = Magenta
inv Magenta = Green

data Piece
  = I Colour | L Colour | O Colour | T Colour | Z Colour
  | Cw Piece
  | FlipH Piece
  deriving Eq

allPieces :: Colour -> [Piece]
allPieces c = mconcat [rotated, fmap flipH rotated]
  where
    shapes = [I c, L c, O c, T c, Z c]
    rotated = concat [shapes, fmap cw shapes, fmap (cw . cw) shapes, fmap ccw shapes]

instance Random Piece where
  randomR (s1, s2) g =
    let (r, g') = random g
        getIndex a = fromJust . lookup a
        (ir, g'')
          = flip randomR g'
          $ getIndex s1 &&& getIndex s2
          $ zip (allPieces r) [0..]
     in (allPieces r !! ir, g'')
  random g =
    let (r, g') = random g
        (ir, g'') = randomR (0, 39) g'
     in (allPieces r !! ir, g'')

cw :: Piece -> Piece
cw (Cw (Cw (Cw p))) = p
cw p                = Cw p

ccw :: Piece -> Piece
ccw = cw . cw . cw

flipH :: Piece -> Piece
flipH (FlipH p) = p
flipH p         = FlipH p

flipV :: Piece -> Piece
flipV = ccw . flipH . cw

pieceToList :: Piece -> (Colour, [String])
pieceToList (Cw p) = second (fmap reverse . transpose) (pieceToList p)
pieceToList (FlipH p) = bimap inv (fmap reverse) (pieceToList p)
pieceToList (O c) =
  (c,) ["**"
       ,"**"]
pieceToList (T c) =
  (c,) ["***"
       ,"-*-"]
pieceToList (Z c) =
  (c,) ["**-"
       ,"-**"]
pieceToList (I c) =
  (c,) ["****"]
pieceToList (L c) =
  (c,) ["***"
       ,"--*"]
