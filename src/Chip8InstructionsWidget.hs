{-# LANGUAGE OverloadedStrings #-}

module Chip8InstructionsWidget
  ( c8Instructions
  ) where

import Chip8
import SDL
import SDL.Font
import Mortar
import Numeric (showHex)
import Data.Bits
import qualified Data.Text as T

c8Instructions :: Font -> Color -> Color -> Chip8 -> Drawable
c8Instructions font fg bg c8 = widget (width,Fixed) (height,Fixed) r
  where l       = label font fg bg
        pc'     = pc c8
        opcode  = getOpCode pc' c8
        w       = l $ T.pack $ showHex opcode ""
        width   = dWidth w
        height  = dHeight w
        r       = flip render w

getOpCode :: Address -> Chip8 -> Instruction
getOpCode addr c8 = opcode
  where ophi    = head $ getMemory addr 1 c8
        oplo    = head $ getMemory (addr+1) 1 c8
        opcode  = (fromIntegral ophi `shiftL` 8) .|. (fromIntegral oplo)

