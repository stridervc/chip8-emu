{-# LANGUAGE OverloadedStrings #-}

module Main where

import Chip8
import Chip8Instructions
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

  appLoop (app font) renderer

  F.free font
  F.quit
