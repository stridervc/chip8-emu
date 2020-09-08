module Main where

import Chip8
import Chip8Instructions

main :: IO ()
main = do
  putStrLn $ "V0 = " ++ (show $ getVReg 0 chip8)
  putStrLn $ "Vf = " ++ (show $ getVReg 15 chip8)
  putStrLn $ show $ vregs chip8
  putStrLn ""

  let Just c = instr 0x6007 chip8 >>= instr 0x6102 >>= instr 0x8014
  putStrLn $ "V0 = " ++ (show $ getVReg 0 c)
  putStrLn $ "Vf = " ++ (show $ getVReg 1 c)
  putStrLn $ show $ vregs c
