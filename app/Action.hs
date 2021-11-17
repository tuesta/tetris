{-# LANGUAGE Arrows          #-}
{-# LANGUAGE PatternSynonyms #-}

module Action (actions, TAction(..)) where

import           Control.Category
import           Control.Concurrent               (threadDelay)
import           Data.Maybe                       (catMaybes)
import           Prelude                          hiding (Left, Right, id, (.))

import           Control.Monad.IO.Class
import           Control.Monad.Trans.MSF.Except
import           Data.MonadicStreamFunction
import           Data.MonadicStreamFunction.Async
import qualified SDL

data TAction = Left | Right | Up | Down | Rotate | Mirror
  deriving Show

data Action = TetrisAction TAction | Quit

pattern PressKey :: SDL.Keycode -> SDL.Event
pattern PressKey k <- SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ k _)))

getAction :: SDL.Event -> Maybe Action
getAction = \case
  PressKey SDL.KeycodeH     -> Just $ TetrisAction Left
  PressKey SDL.KeycodeL     -> Just $ TetrisAction Right
  PressKey SDL.KeycodeJ     -> Just $ TetrisAction Down
  PressKey SDL.KeycodeK     -> Just $ TetrisAction Rotate
  PressKey SDL.KeycodeUp    -> Just $ TetrisAction Up
  PressKey SDL.KeycodeM     -> Just $ TetrisAction Mirror
  PressKey SDL.KeycodeQ     -> Just Quit
  SDL.Event _ SDL.QuitEvent -> Just Quit
  _ -> Nothing

actions :: MonadIO m => MStream (ExceptT () m) TAction
actions = arrM manageQuit . concatS (proc _ -> do
  md <- gravity 60 -< ()
  es <- events     -< ()
  case md of
    Nothing -> returnA -< es
    Just  d -> returnA -< d:es)
  where
    gravity fps = feedback 1 $ proc (_, n) -> do
      constM (liftIO $ threadDelay $ 1000000 `div` fps) -< ()
      if n >= div fps 3
         then returnA -< (Just $ TetrisAction Down, 1)
         else returnA -< (Nothing, succ n)
    events = arr (catMaybes . fmap getAction) . constM SDL.pollEvents
    manageQuit Quit = throwE ()
    manageQuit (TetrisAction a) = pure a
