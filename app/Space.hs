module Space
  ( Space (getSpace)
  , MobilePiece
  , Position
  , initSpace
  , inserts
  , validAction
  , removeLines
  , isGameOver
  ) where

import           Data.Bifunctor (first, second)
import           Data.Coerce    (coerce)
import           Data.Maybe     (catMaybes, isJust)

import           Data.Sequence  (Seq (..), (<|), (><))
import qualified Data.Sequence  as S

import           Piece

type Position = (Int, Int)
newtype Space = Space
  { getSpace :: Seq (Seq (Maybe Colour))
  }

type MobilePiece = (Position, Piece)

initSpace :: Int -> Int -> Space
initSpace cols rows
  = Space
  . S.replicate (rows + 1)
  . S.replicate cols
  $ Nothing

pieceInSpace :: MobilePiece -> [(Position, Colour)]
pieceInSpace ((x0, y0), p)
  = catMaybes
  . uncurry asciiToCell
  . second indexing
  $ pieceToList p
  where
    indexing = fmap (second (zip [0..])) . zip [0..]
    asciiToCell color yxs = do
      (y, xs) <- yxs
      (x, c)  <- xs
      pure $ if c == '*'
        then Just ((x + x0, y + y0), color)
        else Nothing

updateCell :: Position -> Maybe Colour -> Space -> Space
updateCell (x, y) color = coerce $ S.adjust' (S.adjust' (const color) x) y

inserts :: MobilePiece -> Space -> Space
inserts mp s = foldr (uncurry updateCell . second Just) s (pieceInSpace mp)

validAction :: Space -> MobilePiece -> Bool
validAction s mp = all isEmptyCell (pieceInSpace mp)
  where
    isEmptyCell ((x, y), _) =
      case S.lookup y (getSpace s) >>= S.lookup x of
        Just Nothing -> True
        _            -> False

removeLines :: Space -> (Int, Space)
removeLines s =
  let cols      = S.length $ S.index (getSpace s) 0
      (nL, s')  = go (getSpace s)
      emptyLine = S.replicate cols Nothing
   in (nL, coerce (S.replicate nL emptyLine >< s'))
  where
    go Empty = (0, Empty)
    go (l :<| ls)
      | all isJust l = first succ (go ls)
      | otherwise    = second (l <|) $ go ls

isGameOver :: Space -> Bool
isGameOver (Space Empty)     = False
isGameOver (Space (s :<| _)) = any isJust s
