module AppState
  ( AppState (..)
  , defaultAppState
  ) where

import Chip8
import SDL.Font as F

data AppState = AppState
  { chip8   :: Chip8
  , appfont :: F.Font
  }

defaultAppState :: F.Font -> AppState
defaultAppState f = AppState  { chip8   = testChip8
                              , appfont = f
                              }

