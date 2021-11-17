{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Category
import           Prelude                                 hiding (id, (.))

import           Control.Monad.IO.Class
import           Control.Monad.Trans.MSF.Except
import           Data.MonadicStreamFunction
import qualified SDL
import           System.Random

import           Action
import           Render
import           Tetris

main :: IO ()
main = do
  SDL.initialize []
  SDL.HintRenderScaleQuality SDL.$= SDL.ScaleNearest

  w <- SDL.createWindow "Tetris" (SDL.defaultWindow {SDL.windowInitialSize = SDL.V2 300 420})
  r <- SDL.createRenderer w (-1) SDL.defaultRenderer
  g <- initStdGen

  reactimateExcept $
    try $ arrM (liftIO . draw r) . game 15 20 g . actions

  SDL.destroyWindow w
  SDL.quit
