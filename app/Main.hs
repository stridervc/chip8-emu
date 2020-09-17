{-# LANGUAGE OverloadedStrings #-}

module Main where

import Chip8
import AppState
import App

import SDL
import qualified SDL.Font as F
import Mortar

main :: IO ()
main = do
  initializeAll
  F.initialize

  font <- F.load "/usr/share/fonts/truetype/roboto/slab/RobotoSlab-Regular.ttf" 16

  window <- createWindow "Chip-8" defaultWindow { windowResizable = True }
  renderer <- createRenderer window (-1) defaultRenderer

  c8 <- loadRomFile "roms/test_opcode.ch8"
  appLoop (appC8 font c8) renderer

  F.free font
  F.quit
