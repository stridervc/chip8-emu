module Chip8Instructions
  ( cls
  , ret
  , jmp
  , call
  , se
  , sne
  , ser
  , ld
  ) where

import Chip8

-- increment program counter
-- this should always be an even numbers since opcodes
-- are 16-bit
incpc :: Chip8 -> Maybe Chip8
incpc c = return $ c { pc = pc c + 2 }

-- 00E0 clear screen
cls :: Chip8 -> Maybe Chip8
cls c = incpc $ c { screen = take (w*h) $ repeat False }
  where w = fst $ screensize c
        h = snd $ screensize c

-- 00EE return
ret :: Chip8 -> Maybe Chip8
ret c = do
  (c', a) <- pop c
  jmp c' a

-- 1nnn jump nnn
jmp :: Chip8 -> Address -> Maybe Chip8
jmp c a = return $ c { pc = a }

-- 2nnn call nnn
call :: Chip8 -> Address -> Maybe Chip8
call c a = do
  c' <- incpc c
  jmp c' a

-- 3xkk SE Vx, byte
-- skip next instruction if Vx == kk
se :: Chip8 -> Register -> Byte -> Maybe Chip8
se c r v
  | equal     = do
    c' <- incpc c
    incpc c'
  | otherwise = incpc c
  where equal = vx == v
        vx    = getVReg c r

-- 4xkk SNE Vx, byte
-- skip next instruction if Vx /= kk
sne :: Chip8 -> Register -> Byte -> Maybe Chip8
sne c r v
  | equal     = incpc c
  | otherwise = do
    c' <- incpc c
    incpc c'
  where equal = vx == v
        vx    = getVReg c r

-- 5xy0 SE Vx, Vy
-- skip next instruction if Vx == Vy
ser :: Chip8 -> Register -> Register -> Maybe Chip8
ser c a b
  | equal     = do
    c' <- incpc c
    incpc c'
  | otherwise = incpc c
  where equal = va == vb
        va    = getVReg c a
        vb    = getVReg c b

-- 6xkk LD Vx, byte
-- set register x to value kk
ld :: Chip8 -> Register -> Byte -> Maybe Chip8
ld c r v = do
  c' <- setVReg c r v
  incpc c'
