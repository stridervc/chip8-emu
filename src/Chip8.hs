module Chip8
  ( Chip8 (..)
  , Byte
  , Address
  , Instruction
  , Register
  , defaultChip8
  , testChip8
  , loadRomFile
  , push
  , pop
  , getVReg
  , setVReg
  , getMemory
  , setMemory
  , getScreen
  , offsetXY
  , xorPutScreen
  , xorPutScreenByte
  , xorPutScreenBytes
  , willCollide
  , chip8tick
  ) where

import Nibble
import Data.Word
import Data.Bits
import Data.List
import Data.Foldable (foldlM)
import qualified Data.ByteString as B

type Byte         = Word8
type Address      = Word16
type Instruction  = Word16
type Register     = Nibble

data Chip8 = Chip8
  { memory      :: [Byte]       -- 4096 bytes
  , vregs       :: [Byte]       -- 16 8-bit registers, 0-F (0-16)
  , ireg        :: Address      -- 16-bit address register
  , stack       :: [Address]    -- at least 16 DoubleBytes
  , dt          :: Byte         -- Delay timer register
  , st          :: Byte         -- Sound timer register
  , keys        :: [Bool]       -- 16 keyboard buttons, 0-F
  , screensize  :: (Int, Int)   -- width, height
  , screen      :: [Bool]       -- 64x32 pixel monochrome screen
  , pc          :: Address      -- 16-bit program counter
  } deriving (Eq, Show)

-- default initialised chip-8
defaultChip8 :: Chip8
defaultChip8 = Chip8
  { memory      = chip8font ++ repeat 0
  , vregs       = take 16 $ repeat 0
  , ireg        = 0
  , stack       = []
  , dt          = 0
  , st          = 0
  , keys        = take 16 $ repeat False
  , screensize  = (64,32)
  , screen      = take (64*32) $ repeat False
  , pc          = 0x200
  }

testChip8 :: Chip8
testChip8 = defaultChip8 { screen = take (64*32) pattern }
  where pattern = cycle $ (take 64 $ cycle [True,False]) ++ (take 64 $ cycle [False,True])

loadRomAt :: Address -> [Byte] -> Chip8
loadRomAt addr rom = c { memory = mem, pc = addr }
  where mem     = take addr' oldmem ++ rom ++ repeat 0
        addr'   = fromIntegral addr
        oldmem  = memory c
        c       = defaultChip8

loadRomFile :: FilePath -> IO Chip8
loadRomFile fp = do
  rom <- B.readFile fp
  return $ loadRomAt 0x200 $ B.unpack rom

-- push an address onto the stack
push :: Address -> Chip8 -> Chip8
push a c = c { stack = a : stack c }

-- pop address from stack
pop :: Chip8 -> Maybe (Chip8, Address)
pop c
  | emptystack  = Nothing
  | otherwise   = Just (c', a)
  where emptystack  = length (stack c) == 0
        c'          = c { stack = tail $ stack c }
        a           = head $ stack c

-- get V register value
getVReg :: Register -> Chip8 -> Byte
getVReg r c = (vregs c)!!r'
  where r'  = fromEnum r

setVReg :: Register -> Byte -> Chip8 -> Maybe Chip8
setVReg r v c = return c { vregs = vregs' }
  where vregs'  = take r' regs ++ [v] ++ drop (r'+1) regs
        regs    = vregs c
        r'      = fromIntegral r

-- return n bytes memory starting at address
getMemory :: Address -> Byte -> Chip8 -> [Byte]
getMemory addr n c = take n' $ drop addr' $ memory c
  where addr' = fromIntegral addr
        n'    = fromIntegral n

setMemory :: Address -> [Byte] -> Chip8 -> Chip8
setMemory addr bs c = c { memory = mem' }
  where mem   = memory c
        mem'  = take addr' mem ++ bs ++ mem''
        mem'' = drop n $ drop addr' mem
        n     = length bs
        addr' = fromIntegral addr

getScreen :: Byte -> Byte -> Chip8 -> Bool
getScreen x y c = (screen c)!!offs
  where w     = fst $ screensize c
        offs  = offsetXY x y c

offsetXY :: Byte -> Byte -> Chip8 -> Int
offsetXY x y c = fromIntegral $ y'*w+x'
  where y'  = fromIntegral y
        x'  = fromIntegral x
        w   = fst $ screensize c

xorPutScreen :: Byte -> Byte -> Bool -> Chip8 -> Chip8
xorPutScreen x y v c = c { screen = scr'' }
  where i     = offsetXY x y c
        scr   = screen c
        scr'  = drop (i+1) scr
        scr'' = take i scr ++ [v'] ++ scr'
        old   = getScreen x y c
        v'    = old `xor` v

-- f :: Chip8 -> (x,v) -> Chip8
-- returns true if collision
xorPutScreenByte :: Byte -> Byte -> Byte -> Chip8 -> Chip8
xorPutScreenByte x y b c = c'
  where bits      = map (testBit b) [7,6..0]
        xs        = map (+x) [0..]
        xvs       = zip xs bits
        f         = (\c (x,v) -> xorPutScreen x y v c)
        c'        = foldl f c xvs

willCollide :: Byte -> Byte -> Byte -> Chip8 -> Bool
willCollide x y b c = any id $ map (\(a,b) -> a == True && b == True) oldnewbits
  where oldnewbits  = zip oldbits newbits
        oldbits     = map (\x -> getScreen x y c) xs
        xs          = map (+x) [0..7]
        newbits     = map (testBit b) [7,6..0]

-- the standard chip-8 font starting at 0x000 of memory
chip8font :: [Byte]
chip8font = [ 0xf0, 0x90, 0x90, 0x90, 0xf0  -- 0
            , 0x20, 0x60, 0x20, 0x20, 0x70  -- 1
            , 0xf0, 0x10, 0xf0, 0x80, 0xf0  -- 2
            , 0xf0, 0x10, 0xf0, 0x10, 0xf0  -- 3
            , 0x90, 0x90, 0xf0, 0x10, 0x10  -- 4
            , 0xf0, 0x80, 0xf0, 0x10, 0xf0  -- 5
            , 0xf0, 0x80, 0xf0, 0x90, 0xf0  -- 6
            , 0xf0, 0x10, 0x20, 0x40, 0x40  -- 7
            , 0xf0, 0x90, 0xf0, 0x90, 0xf0  -- 8
            , 0xf0, 0x90, 0xf0, 0x10, 0xf0  -- 9
            , 0xf0, 0x90, 0xf0, 0x90, 0x90  -- A
            , 0xe0, 0x90, 0xe0, 0x90, 0xe0  -- B
            , 0xf0, 0x80, 0x80, 0x80, 0xf0  -- C
            , 0xe0, 0x90, 0x90, 0x90, 0xe0  -- D
            , 0xf0, 0x80, 0xf0, 0x80, 0xf0  -- E
            , 0xf0, 0x80, 0xf0, 0x80, 0x80  -- F
            ]

-- execute 1 chip8 instruction
chip8tick :: Chip8 -> Maybe Chip8
chip8tick c = instr opcode c
  where opcode  = (hi `shiftL` 8) .|. lo
        hi      = fromIntegral $ head $ getMemory pc' 1 c
        lo      = fromIntegral $ head $ getMemory (pc'+1) 1 c
        pc'     = pc c

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
  | hinib == 0xd  = instrDRW  x y n c
  | hinib == 8    = case lonib of
    0     -> instrLdR   x y c
    1     -> instrOr    x y c
    2     -> instrAnd   x y c
    3     -> instrXor   x y c
    4     -> instrAddR  x y c
    5     -> instrSubR  x y c
    6     -> instrShr   x y c
    7     -> instrSubn  x y c
    0xe   -> instrShl   x y c
  | hinib == 0xE  = case lobyte of
    0x9E  -> instrSKP   x c
    0xA1  -> instrSKNP  x c
  | hinib == 0xF  = case lobyte of
    0x07  -> instrLDxDT x c
    0x0A  -> instrLDxK  x c
    0x15  -> instrLDDTx x c
    0x18  -> instrLDSTx x c
    0x1E  -> instrADDIx x c
    0x29  -> instrLDFx  x c
    0x33  -> instrLDBx  x c
    0x55  -> instrLDIx  x c
    0x65  -> instrLDxI  x c
  where hinib   = opcode `shiftR` 12
        lonib   = opcode .&. 0x000f
        nnn     = opcode .&. 0x0fff
        n       = fromIntegral $ opcode .&. 0x000f
        x       = fromIntegral $ (opcode .&. 0x0f00) `shiftR` 8
        y       = fromIntegral $ (opcode .&. 0x00f0) `shiftR` 4
        kk      = fromIntegral $ opcode .&. 0x00ff
        lobyte  = kk
instr o _       = error $ "Unknown opcode : " ++ show o

-- increment program counter
-- this should always be an even numbers since opcodes
-- are 16-bit
incpc :: Chip8 -> Chip8
incpc c = c { pc = pc c + 2 }

-- 00E0 clear screen
instrCls :: Chip8 -> Maybe Chip8
instrCls c = return $ incpc $
  c { screen = take (w*h) $ repeat False }
  where w = fromIntegral $ fst $ screensize c
        h = fromIntegral $ snd $ screensize c

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
instrCall a c = instrJmp a c'
  where c'  = push (pc'+2) c
        pc' = pc c

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
        vx' = fromIntegral $ 0x42 .&. v

-- f :: Chip8 -> (y,b) -> Chip8
-- returns true if collision
xorPutScreenBytes :: Byte -> Byte -> [Byte] -> Chip8 -> (Chip8,Bool)
xorPutScreenBytes x y bs c = (c', collision)
  where c'        = foldl f c ybs
        ys        = map (+y) [0..]
        ybs       = zip ys bs
        f         = (\c (y,b) -> xorPutScreenByte x y b c)
        collision = any id $ map (\(y,b) -> willCollide x y b c) ybs

-- Dxyn Draw Vx, Vy, n
-- Display n-byte sprite from memory location I at
-- (Vx, Vy) on screen
-- VF is set on collision
instrDRW :: Register -> Register -> Nibble -> Chip8 -> Maybe Chip8
instrDRW x y n c = incpc <$> setVReg 15 vf c'
  where vx              = getVReg x c
        vy              = getVReg y c
        i               = ireg c
        n'              = fromIntegral n
        sprite          = getMemory i n' c
        (c',collision)  = xorPutScreenBytes vx vy sprite c
        vf              = if collision then 1 else 0

-- Ex9E SKP Vx
-- Skip next instruction if key with value of Vx is pressed.
instrSKP :: Register -> Chip8 -> Maybe Chip8
instrSKP x c
  | pressed   = return $ incpc $ incpc c
  | otherwise = return $ incpc c
  where pressed = (keys c)!!vx'
        vx      = getVReg x c
        vx'     = fromIntegral vx

-- ExA1 SKP Vx
-- Skip next instruction if key with value of Vx is not pressed.
instrSKNP :: Register -> Chip8 -> Maybe Chip8
instrSKNP x c
  | pressed   = return $ incpc c
  | otherwise = return $ incpc $ incpc c
  where pressed = (keys c)!!vx'
        vx      = getVReg x c
        vx'     = fromIntegral vx

-- Fx07 LD Vx, DT
-- Vx = DT
instrLDxDT :: Register -> Chip8 -> Maybe Chip8
instrLDxDT x c = incpc <$> setVReg x (dt c) c

-- Fx0A LD Vx, K
-- Wait for keypress, store in Vx
-- wait by not incr pc, so this instruction will just get called
-- by tick until a key is pressed
instrLDxK :: Register -> Chip8 -> Maybe Chip8
instrLDxK x c
  | pressed   = incpc <$> setVReg x k c
  | otherwise = return c
  where pressed = any id $ keys c
        Just k' = elemIndex True $ keys c
        k       = toEnum k'

-- Fx15 LD DT, Vx
-- DT = Vx
instrLDDTx :: Register -> Chip8 -> Maybe Chip8
instrLDDTx x c = return $ incpc c { dt = getVReg x c }

-- Fx18 LD ST, Vx
-- ST = Vx
instrLDSTx :: Register -> Chip8 -> Maybe Chip8
instrLDSTx x c = return $ incpc c { st = getVReg x c }

-- Fx1E ADD I, Vx
-- I = I + Vx
instrADDIx :: Register -> Chip8 -> Maybe Chip8
instrADDIx x c = return $ incpc c { ireg = i' }
  where i'  = ireg c + (fromIntegral $ getVReg x c)

-- Fx29 LD F, Vx
-- I = location of font for digit Vx
instrLDFx :: Register -> Chip8 -> Maybe Chip8
instrLDFx x c = return $ incpc $ c { ireg = vx*5 }
  where vx  = fromIntegral $ getVReg x c

-- Fx33 LD B, Vx
-- Store BCD representation of Vx in memory locations I, I+1, I+2
-- Hundreds digit in I, tens in I+1, ones in I+2
instrLDBx :: Register -> Chip8 -> Maybe Chip8
instrLDBx x c = return $ incpc $ setMemory i mem c
  where vx        = getVReg x c
        hundreds  = vx `div` 100
        tens      = (vx - hundreds * 100) `div` 10
        ones      = (vx - hundreds * 100 - tens * 10)
        i         = ireg c
        mem       = [hundreds, tens, ones]

-- Fx55 LD [I], Vx
-- Store registers V0 through Vx in memory starting at I
instrLDIx :: Register -> Chip8 -> Maybe Chip8
instrLDIx x c = return $ incpc $ setMemory i values c
  where vx      = getVReg x c
        vx'     = fromIntegral vx
        values  = map (flip getVReg c) $ take vx' $ [0..]
        i       = ireg c

-- Fx65 LD Vx, [I]
-- Read registers V0 through Vx from memory starting at I
-- setreg :: Chip8 -> (r,v) -> Maybe Chip8
instrLDxI :: Register -> Chip8 -> Maybe Chip8
instrLDxI x c = incpc <$> foldlM setreg c regsvals
  where vx        = getVReg x c
        values    = getMemory i vx c
        regsvals  = zip [0..] values
        i         = ireg c
        setreg    = (\c (r,v) -> setVReg r v c)

