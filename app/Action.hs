{-# LANGUAGE PatternSynonyms #-}

module Action where

import           Prelude hiding (Left, Right, id, (.))

import qualified SDL

data Action = Left | Right | Up | Down | Rotate | Mirror
  deriving Show

pattern PressKey :: SDL.Keycode -> SDL.Event
pattern PressKey k <- SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ k _)))

getAction :: SDL.Event -> Maybe Action
getAction = \case
  PressKey SDL.KeycodeH  -> Just Left
  PressKey SDL.KeycodeL  -> Just Right
  PressKey SDL.KeycodeJ  -> Just Down
  PressKey SDL.KeycodeK  -> Just Rotate
  PressKey SDL.KeycodeUp -> Just Up
  PressKey SDL.KeycodeM  -> Just Mirror
  _                      -> Nothing

isQuitEvent :: SDL.Event -> Bool
isQuitEvent = \case
  PressKey SDL.KeycodeQ     -> True
  SDL.Event _ SDL.QuitEvent -> True
  _                         -> False
