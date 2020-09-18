{-# LANGUAGE OverloadedStrings #-}

module Chip8RegistersWidget
  ( c8Registers
  ) where

import Chip8
import SDL
import SDL.Font
import Mortar
import Numeric (showHex)
import qualified Data.Text as T

c8Registers :: Font -> Color -> Chip8 -> Drawable
c8Registers font color c = widget (width,Fixed) (height,Fixed) r
  where l         = label font color (V4 0 0 0 0)
        regs      = map (flip T.append " : ")
                      [ "PC", "I ", "DT", "ST", "V0", "V1", "V2", "V3"
                      , "V4","V5" ,"V6" ,"V7" ,"V8" ,"V9" ,"VA" ,"VB" 
                      ,"VC" ,"VD" ,"VE" ,"VF" ]
        regsw     = foldl1 (<=>) $ map l regs
        values16  = [ pc c, ireg c ]
        values8   = dt c : st c : map (flip getVReg c) [0..15]
        valuesw   = foldl1 (<=>) $ map (l . T.pack) $ map ($ "") (map showHex values16 ++ map showHex values8)
        w         = regsw <+> valuesw
        r         = flip render w
        width     = dWidth w
        height    = dHeight w
