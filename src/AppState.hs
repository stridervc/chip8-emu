module AppState
  ( AppState (..)
  , defaultAppState
  , chip8AppState
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

chip8AppState :: F.Font -> Chip8 -> AppState
chip8AppState f c = AppState  { chip8   = c
                              , appfont = f
                              }

