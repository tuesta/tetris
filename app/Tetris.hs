module Tetris (game) where

import           Control.Category
import           Prelude                                 hiding (Left, Right,
                                                          id, (.))

import           Control.Monad.Random
import           Control.Monad.Trans.MSF.Except
import           Data.MonadicStreamFunction              hiding (left, right)

import           Action
import           Piece
import           Space

movementM
  :: Applicative m
  => (Position -> Position)
  -> (Piece -> Piece)
  -> (MobilePiece -> m MobilePiece)
  -> Space -> MobilePiece -> m MobilePiece
movementM m tp k s mp@(pos, p)
  | validAction s mp' = pure mp'
  | otherwise = k mp
  where
    mp' = (m pos, tp p)

left, right, up, rotate, mirror
  :: Applicative m => Space -> MobilePiece -> m MobilePiece
left   = movementM (first pred)  id    pure
right  = movementM (first succ)  id    pure
up     = movementM (second pred) id    pure
rotate = movementM id            cw    pure
mirror = movementM id            flipH pure

down :: Monad m => Space -> MobilePiece -> ExceptT MobilePiece m MobilePiece
down = movementM (second succ) id throwE

move :: Monad m => Space -> MobilePiece -> MSF (ExceptT MobilePiece m) TAction Space
move s0 mp0 = feedback mp0 $ arrM (fmap (\mp -> (inserts mp s0, mp)) . uncurry go)
  where
    go Left   = left s0
    go Right  = right s0
    go Up     = up s0
    go Rotate = rotate s0
    go Mirror = mirror s0
    go Down   = down s0

tetris :: (RandomGen g, Monad m) => g -> Int -> Int -> MSF (ExceptT g m) TAction Space
tetris g0 cols rows = safely $ bucle (initSpace cols rows) (random g0)
  where
    topCenter = (cols - div cols 2 - 1, 0)
    bucle s (piece, g) = do
      mp' <- try $ move s (topCenter, piece)
      let (lR, s') = (removeLines . inserts mp') s
      if isGameOver s' || (lR > 0 && lR < 4)
        then safe . throw . fst . split $ g
        else bucle s' (random g)

game :: (RandomGen g, Monad m) => Int -> Int -> g -> MSF m TAction Space
game cols rows g = catchS (tetris g cols rows) (game cols rows)
