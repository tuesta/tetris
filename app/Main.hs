{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Category
import           Control.Concurrent                      (threadDelay)
import           Data.Maybe                              (catMaybes)
import           Prelude                                 hiding (id, (.))

import           Control.Monad.IO.Class
import           Data.MonadicStreamFunction              (arrM)
import           Data.MonadicStreamFunction.InternalCore (MSF, unMSF)
import qualified SDL
import           System.Random

import           Action
import           Render
import           Tetris

play :: MonadIO m => Int -> MSF m Action () -> m ()
play n msf = do
  events <- SDL.pollEvents
  let as   = catMaybes (fmap getAction events)
      exit = any isQuitEvent events
  if | exit      -> pure ()
     | n <= 0    -> go (Down:as) msf >>= play 15
     | otherwise -> go as        msf >>= play (pred n)
  where
    go [] k     = liftIO (threadDelay 10000) >> pure k
    go (a:as) k = unMSF k a >>= \(_, k') -> go as k'

main :: IO ()
main = do
  SDL.initialize []
  SDL.HintRenderScaleQuality SDL.$= SDL.ScaleNearest

  w <- SDL.createWindow "Tetris" (SDL.defaultWindow {SDL.windowInitialSize = SDL.V2 300 420})
  r <- SDL.createRenderer w (-1) SDL.defaultRenderer
  g <- initStdGen

  play 0 (arrM (liftIO . draw r) . game 15 20 g)

  SDL.destroyWindow w
  SDL.quit
