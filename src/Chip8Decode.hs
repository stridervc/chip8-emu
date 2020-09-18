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
  | hinib == 0x7      = "add v" <> hex x <+> hex kk
  | hinib == 0x8      = case lonib of
    0x0       -> "add v" <> hex x <+> "v" <> hex y
    0x1       -> "or v" <> hex x <+> "v" <> hex y
    0x2       -> "and v" <> hex x <+> "v" <> hex y
    0x3       -> "xor v" <> hex x <+> "v" <> hex y
    0x4       -> "add v" <> hex x <+> "v" <> hex y
    0x5       -> "sub v" <> hex x <+> "v" <> hex y
    0x6       -> "shr v" <> hex x <+> "v" <> hex y
    0x7       -> "subn v" <> hex x <+> "v" <> hex y
    0xe       -> "shl v" <> hex x <+> "v" <> hex y
    otherwise -> " "
  | hinib == 0x9      = "sne v" <> hex x <+> "v" <> hex y
  | hinib == 0xa      = "mov i" <+> hex nnn
  | hinib == 0xb      = "jmp v0+" <> hex nnn
  | hinib == 0xc      = "rnd v" <> hex x <+> hex kk
  | hinib == 0xd      = "drw v" <> hex x <+> "v" <> hex y <+> hex n
  | hinib == 0xe      = case lobyte of
    0x9e      -> "skp v" <> hex x
    0xa1      -> "sknp v" <> hex x
    otherwise -> " "
  | hinib == 0xf      = case lobyte of
    0x07      -> "mov v" <> hex x <+> "dt"
    0x0a      -> "mov v" <> hex x <+> "k"
    0x15      -> "mov dt v" <> hex x
    0x18      -> "mov st v" <> hex x
    0x1e      -> "add i v" <> hex x
    0x29      -> "mov f v" <> hex x
    0x33      -> "mov b v" <> hex x
    0x55      -> "mov [i] v" <> hex x
    0x65      -> "mov v" <> hex x <+> "[i]"
    otherwise -> " "
  where hinib   = opcode `shiftR` 12
        lonib   = opcode .&. 0x000f
        nnn     = opcode .&. 0x0fff
        n       = fromIntegral $ opcode .&. 0x000f
        x       = fromIntegral $ (opcode .&. 0x0f00) `shiftR` 8
        y       = fromIntegral $ (opcode .&. 0x00f0) `shiftR` 4
        kk      = fromIntegral $ opcode .&. 0x00ff
        lobyte  = kk
        hex     = (\n -> T.pack $ showHex n "")
        (<+>)   = (\a b -> a <> " " <> b)

decodeOpcode _        = " "


