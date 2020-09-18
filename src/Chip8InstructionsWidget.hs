{-# LANGUAGE OverloadedStrings #-}

module Chip8InstructionsWidget
  ( c8Instructions
  ) where

import Chip8
import Chip8Decode
import SDL
import SDL.Font
import Mortar
import Numeric (showHex)
import Data.Bits
import qualified Data.Text as T

c8Instructions :: Font -> Color -> Color -> Chip8 -> Drawable
c8Instructions font fg bg c8 = widget (width,Fixed) (height,Fixed) r
  where pc'     = pc c8
        addrs   = take 10 $ iterate (+2) pc'
        iline   = instrLine font fg bg c8
        ws      = map iline addrs
        w       = foldl1 (<=>) ws
        width   = dWidth w
        height  = dHeight w
        r       = flip render w

getOpCode :: Address -> Chip8 -> Instruction
getOpCode addr c8 = opcode
  where ophi    = head $ getMemory addr 1 c8
        oplo    = head $ getMemory (addr+1) 1 c8
        opcode  = (fromIntegral ophi `shiftL` 8) .|. (fromIntegral oplo)

instrLine :: Font -> Color -> Color -> Chip8 -> Address -> Drawable
instrLine font fg bg c8 addr = addrl <+> sep <+> opcodel <+> sep
                                <+> decode
  where l       = label font fg bg
        hexl    = (\n -> l $ T.pack $ showHex n "")
        sep     = l " : "
        addrl   = hexl addr
        opcodel = hexl opcode
        opcode  = getOpCode addr c8
        decode  = l $ decodeOpcode opcode
