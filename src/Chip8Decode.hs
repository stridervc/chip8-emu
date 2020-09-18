{-# LANGUAGE OverloadedStrings #-}

module Chip8Decode
  ( decodeOpcode
  ) where

import Chip8
import Numeric (showHex)
import Data.Bits
import qualified Data.Text as T

decodeOpcode :: Instruction -> T.Text
decodeOpcode 0x00e0   = "cls"
decodeOpcode 0x00ee   = "ret"
decodeOpcode opcode
  | hinib == 0x1      = "jmp" <+> hex nnn
  | hinib == 0x2      = "call" <+> hex nnn
  | hinib == 0x3      = "se v" <> hex x <+> hex kk
  | hinib == 0x4      = "sne v" <> hex x <+> hex kk
  | hinib == 0x5      = "se v" <> hex x <+> "v" <> hex y
  | hinib == 0x6      = "mov v" <> hex x <+> hex kk

  where hinib   = opcode `shiftR` 12
        nnn     = opcode .&. 0x0fff
        x       = fromIntegral $ (opcode .&. 0x0f00) `shiftR` 8
        y       = fromIntegral $ (opcode .&. 0x00f0) `shiftR` 4
        kk      = fromIntegral $ opcode .&. 0x00ff
        hex     = (\n -> T.pack $ showHex n "")
        (<+>)   = (\a b -> a <> " " <> b)

decodeOpcode _        = " "


