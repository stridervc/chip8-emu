module Chip8Instructions
  ( instr
  ) where

import Chip8
import Data.Bits

-- apply an opcode to Chip8
instr :: Instruction -> Chip8 -> Maybe Chip8
instr 0x00e0 c  = instrCls c
instr 0x00ee c  = instrRet c
instr opcode c
  | hinib == 1    = instrJmp  nnn c
  | hinib == 2    = instrCall nnn c
  | hinib == 3    = instrSe   x kk c
  | hinib == 4    = instrSne  x kk c
  | hinib == 5    = instrSeR  x y c
  | hinib == 6    = instrLd   x kk c
  | hinib == 7    = instrAdd  x kk c
  | hinib == 9    = instrSneR x y c
  | hinib == 0xa  = instrLDI  nnn c
  | hinib == 0xb  = instrJMPA nnn c
  | hinib == 0xc  = instrRND  x kk c
  | hinib == 8    = case lonib of
    0   -> instrLdR   x y c
    1   -> instrOr    x y c
    2   -> instrAnd   x y c
    3   -> instrXor   x y c
    4   -> instrAddR  x y c
    5   -> instrSubR  x y c
    6   -> instrShr   x y c
    7   -> instrSubn  x y c
    0xe -> instrShl   x y c
  where hinib   = opcode `shiftR` 12
        lonib   = opcode .&. 0x000f
        nnn     = opcode .&. 0x0fff
        x       = fromIntegral $ (opcode .&. 0x0f00) `shiftR` 8
        y       = fromIntegral $ (opcode .&. 0x00f0) `shiftR` 4
        kk      = fromIntegral $ opcode .&. 0x00ff
instr _ _       = Nothing

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
  instrJmp a c'

-- 1nnn jump nnn
instrJmp :: Address -> Chip8 -> Maybe Chip8
instrJmp a c = return $ c { pc = a }

-- 2nnn call nnn
instrCall :: Address -> Chip8 -> Maybe Chip8
instrCall a c = incpc <$> instrJmp a c

-- 3xkk SE Vx, byte
-- skip next instruction if Vx == kk
instrSe :: Register -> Byte -> Chip8 -> Maybe Chip8
instrSe r v c
  | equal     = return $ incpc $ incpc c
  | otherwise = return $ incpc c
  where equal = vx == v
        vx    = getVReg r c

-- 4xkk SNE Vx, byte
-- skip next instruction if Vx /= kk
instrSne :: Register -> Byte -> Chip8 -> Maybe Chip8
instrSne r v c
  | equal     = return $ incpc c
  | otherwise = return $ incpc $ incpc c
  where equal = vx == v
        vx    = getVReg r c

-- 5xy0 SE Vx, Vy
-- skip next instruction if Vx == Vy
instrSeR :: Register -> Register -> Chip8 -> Maybe Chip8
instrSeR x y c
  | equal     = return $ incpc $ incpc c
  | otherwise = return $ incpc c
  where equal = vx == vy
        vx    = getVReg x c
        vy    = getVReg y c

-- 6xkk LD Vx, byte
-- set register x to value kk
instrLd :: Register -> Byte -> Chip8 -> Maybe Chip8
instrLd r v c = incpc <$> setVReg r v c

-- 7xkk ADD Vx, byte
-- Vx = Vx + kk
instrAdd :: Register -> Byte -> Chip8 -> Maybe Chip8
instrAdd r v c = incpc <$> setVReg r (vx+v) c
  where vx  = getVReg r c

-- 8xy0 LD Vx, Vy
-- set Vx to Vy's value
instrLdR :: Register -> Register -> Chip8 -> Maybe Chip8
instrLdR x y c = incpc <$> setVReg x vy c
  where vy  = getVReg y c

-- 8xy1 OR Vx, Vy
-- Vx = Vx OR Vy
instrOr :: Register -> Register -> Chip8 -> Maybe Chip8
instrOr x y c = incpc <$> setVReg x (vx .|. vy) c
  where vx  = getVReg x c
        vy  = getVReg y c

-- 8xy2 AND Vx, Vy
-- Vx = Vx AND Vy
instrAnd :: Register -> Register -> Chip8 -> Maybe Chip8
instrAnd x y c = incpc <$> setVReg x (vx .&. vy) c
  where vx  = getVReg x c
        vy  = getVReg y c

-- 8xy3 XOR Vx, Vy
-- Vx = Vx XOR Vy
instrXor :: Register -> Register -> Chip8 -> Maybe Chip8
instrXor x y c = incpc <$> setVReg x (vx `xor` vy) c
  where vx  = getVReg x c
        vy  = getVReg y c

-- 8xy4 ADD Vx, Vy
-- Vx = Vx + Vy
-- Vf set on overflow
instrAddR :: Register -> Register -> Chip8 -> Maybe Chip8
instrAddR x y c = incpc <$> setVReg x (vx+vy) c >>= setVReg 15 vf
  where vx  = getVReg x c
        vy  = getVReg y c
        vf  = if (toInteger vx + toInteger vy) > 255 then 1 else 0

-- 8xy5 SUB Vx, Vy
-- Vx = Vx - Vy
-- Vf set if not borrow (Vx > Vy)
instrSubR :: Register -> Register -> Chip8 -> Maybe Chip8
instrSubR x y c = incpc <$> setVReg x (vx-vy) c >>= setVReg 15 vf
  where vx  = getVReg x c
        vy  = getVReg y c
        vf  = if vx > vy then 1 else 0

-- 8xy6 SHR Vx {,Vy}
-- NOTE: Vy is unused according to some specs
-- if LSB Vx == 1 then Vf = 1 else 0
-- Vx = Vx SHR 1
instrShr :: Register -> Register -> Chip8 -> Maybe Chip8
instrShr x y c = incpc <$> setVReg x vx' c >>= setVReg 15 vf
  where vx  = getVReg x c
        vf  = vx .&. 0x1
        vx' = vx `shiftR` 1

-- 8xy7 SUBN Vx, Vy
-- Vx = Vy - Vx
-- if Vy > Vx then Vf = 1 else Vf = 0
instrSubn :: Register -> Register -> Chip8 -> Maybe Chip8
instrSubn x y c = incpc <$> setVReg x (vy-vx) c >>= setVReg 15 vf
  where vx  = getVReg x c
        vy  = getVReg y c
        vf  = if vy > vx then 1 else 0

-- 8xyE SHL Vx {,Vy}
-- NOTE: See note at SHR
-- if MSB Vx == 1 then Vf = 1 else Vf = 0
-- Vx = Vx SHL 1
instrShl :: Register -> Register -> Chip8 -> Maybe Chip8
instrShl x y c = incpc <$> setVReg x vx' c >>= setVReg 15 vf
  where vx  = getVReg x c
        vy  = getVReg y c
        vf  = if (vx .&. 0x80) > 0 then 1 else 0
        vx' = vx `shiftL` 1

-- 9xy0 SNE Vx, Vy
-- Skip next instruction if Vx /= Vy
instrSneR :: Register -> Register -> Chip8 -> Maybe Chip8
instrSneR x y c
  | vx == vy  = return $ incpc c
  | otherwise = return $ incpc $ incpc c
  where vx  = getVReg x c
        vy  = getVReg y c

-- Annn LD I, addr
-- Set I register = nnn
instrLDI :: Address -> Chip8 -> Maybe Chip8
instrLDI a c = return $ incpc c { ireg = a }

-- Bnnn JMP V0, addr
-- Jump to V0 + nnn
instrJMPA :: Address -> Chip8 -> Maybe Chip8
instrJMPA a c = instrJmp addr c
  where v0    = getVReg 0 c
        addr  = fromIntegral $ v0 + fromIntegral a

-- Cxkk RND Vx, byte
-- Vx = random byte AND kk
-- TODO : Generate random numbers
instrRND :: Register -> Byte -> Chip8 -> Maybe Chip8
instrRND x v c = incpc <$> setVReg x vx' c
  where vx  = getVReg x c
        vx' = fromIntegral $ vx .&. v
