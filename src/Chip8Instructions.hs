module Chip8Instructions
  ( instrCls
  , instrRet
  , instrJmp
  , instrCall
  , instrSe
  , instrSne
  , instrSeR
  , instrLd
  ) where

import Chip8
import Data.Bits

-- increment program counter
-- this should always be an even numbers since opcodes
-- are 16-bit
incpc :: Chip8 -> Chip8
incpc c = c { pc = pc c + 2 }

-- 00E0 clear screen
instrCls :: Chip8 -> Maybe Chip8
instrCls c = return $ incpc $
  c { screen = take (w*h) $ repeat False }
  where w = fst $ screensize c
        h = snd $ screensize c

-- 00EE return
instrRet :: Chip8 -> Maybe Chip8
instrRet c = do
  (c', a) <- pop c
  instrJmp c' a

-- 1nnn jump nnn
instrJmp :: Chip8 -> Address -> Maybe Chip8
instrJmp c a = return $ c { pc = a }

-- 2nnn call nnn
instrCall :: Chip8 -> Address -> Maybe Chip8
instrCall c a = instrJmp (incpc c) a

-- 3xkk SE Vx, byte
-- skip next instruction if Vx == kk
instrSe :: Chip8 -> Register -> Byte -> Maybe Chip8
instrSe c r v
  | equal     = return $ incpc $ incpc c
  | otherwise = return $ incpc c
  where equal = vx == v
        vx    = getVReg c r

-- 4xkk SNE Vx, byte
-- skip next instruction if Vx /= kk
instrSne :: Chip8 -> Register -> Byte -> Maybe Chip8
instrSne c r v
  | equal     = return $ incpc c
  | otherwise = return $ incpc $ incpc c
  where equal = vx == v
        vx    = getVReg c r

-- 5xy0 SE Vx, Vy
-- skip next instruction if Vx == Vy
instrSeR :: Chip8 -> Register -> Register -> Maybe Chip8
instrSeR c a b
  | equal     = return $ incpc $ incpc c
  | otherwise = return $ incpc c
  where equal = va == vb
        va    = getVReg c a
        vb    = getVReg c b

-- 6xkk LD Vx, byte
-- set register x to value kk
instrLd :: Chip8 -> Register -> Byte -> Maybe Chip8
instrLd c r v = incpc <$> setVReg c r v

-- 7xkk ADD Vx, byte
-- Vx = Vx + kk
instrAdd :: Chip8 -> Register -> Byte -> Maybe Chip8
instrAdd c r v = incpc <$> setVReg c r (vx+v)
  where vx  = getVReg c r

-- 8xy0 LD Vx, Vy
-- set Vx to Vy's value
instrLdx :: Chip8 -> Register -> Register -> Maybe Chip8
instrLdx c x y = incpc <$> setVReg c x vy
  where vy  = getVReg c y

-- 8xy1 OR Vx, Vy
-- Vx = Vx OR Vy
instrOr :: Chip8 -> Register -> Register -> Maybe Chip8
instrOr c x y = incpc <$> setVReg c x (vx .|. vy)
  where vx  = getVReg c x
        vy  = getVReg c y

-- 8xy2 AND Vx, Vy
-- Vx = Vx AND Vy
instrAnd :: Chip8 -> Register -> Register -> Maybe Chip8
instrAnd c x y = incpc <$> setVReg c x (vx .&. vy)
  where vx  = getVReg c x
        vy  = getVReg c y

-- 8xy3 XOR Vx, Vy
-- Vx = Vx XOR Vy
instrXor :: Chip8 -> Register -> Register -> Maybe Chip8
instrXor c x y = incpc <$> setVReg c x (vx `xor` vy)
  where vx  = getVReg c x
        vy  = getVReg c y

-- 8xy4 ADD Vx, Vy
-- Vx = Vx + Vy
instrAddR :: Chip8 -> Register -> Register -> Maybe Chip8
instrAddR c x y = do
  c' <- setVReg c 15 vf
  incpc <$> setVReg c' x (vx+vy)
  where vx  = getVReg c x
        vy  = getVReg c y
        vf  = if (toInteger vx + toInteger vy) > 255 then 1 else 0
