module Render (draw) where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO)

import           Data.Sequence          (Seq)
import qualified Data.Sequence          as S
import           SDL                    (($=))
import qualified SDL

import           Piece                  (Colour (..))
import           Space

draw :: SDL.Renderer -> Space -> IO ()
draw r s = do
  SDL.clear r
  setColor r Nothing

  forWithIndex_ (getSpace s) $ \y cols ->
    forWithIndex_ cols $ \x -> drawSquare r 20 (x, y)

  setColor r Nothing
  SDL.present r

forWithIndex_ :: Applicative f => Seq a -> (Int -> a -> f b) -> f ()
forWithIndex_ t f = void (S.traverseWithIndex f t)

drawSquare :: SDL.Renderer -> Int -> (Int, Int) -> Maybe Colour -> IO ()
drawSquare r l (x, y) mc = setColor r mc >> SDL.fillRect r (Just square)
  where
    (l', x', y') = (fromIntegral l, fromIntegral x, fromIntegral y)
    square =  SDL.Rectangle
      (SDL.P (SDL.V2 (x' * l') (y' * l')))
      (SDL.V2 l' l')

setColor :: MonadIO m => SDL.Renderer -> Maybe Colour -> m ()
setColor r Nothing      = SDL.rendererDrawColor r $= SDL.V4 0 0 0 maxBound
setColor r (Just color) = case color of
  White   -> SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
  Red     -> SDL.rendererDrawColor r $= SDL.V4 maxBound 0 0 maxBound
  Green   -> SDL.rendererDrawColor r $= SDL.V4 0 maxBound 0 maxBound
  Blue    -> SDL.rendererDrawColor r $= SDL.V4 50 0 maxBound maxBound
  Yellow  -> SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound 0 maxBound
  Magenta -> SDL.rendererDrawColor r $= SDL.V4 maxBound 0 maxBound maxBound
  Cyan    -> SDL.rendererDrawColor r $= SDL.V4 0 maxBound maxBound maxBound
