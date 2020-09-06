module Main where

import Chip8
import Chip8Instructions

main :: IO ()
main = do
  let Just c = ld chip8 12 0xff

  putStrLn $ "Vc = " ++ (show $ getVReg chip8 12)
  putStrLn $ show $ vregs chip8
  putStrLn ""

  putStrLn $ "Vc = " ++ (show $ getVReg c 12)
  putStrLn $ show $ vregs c
