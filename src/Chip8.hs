module Chip8
  ( Chip8 (..)
  , Byte
  , Address
  , Instruction
  , Register
  , defaultChip8
  , testChip8
  , push
  , pop
  , getVReg
  , setVReg
  , getMemory
  , setMemory
  , getScreen
  , xorPutScreen
  , xorPutScreenByte
  , willCollide
  ) where

import Nibble
import Data.Word
import Data.Bits

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
  , screensize  :: (Byte, Byte) -- width, height
  , screen      :: [Bool]       -- 64x32 pixel monochrome screen
  , pc          :: Address      -- 16-bit program counter
  } deriving (Eq, Show)

-- default initialised chip-x
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
        offs  = fromIntegral $ y*w+x

xorPutScreen :: Byte -> Byte -> Bool -> Chip8 -> Chip8
xorPutScreen x y v c = c { screen = scr'' }
  where i     = fromIntegral $ y*w+x
        w     = fst $ screensize c
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
        xs        = map (+x) [0..7]
        xvs       = zip xs bits
        f         = (\c (x,v) -> xorPutScreen x y v c)
        c'        = foldl f c xvs

willCollide :: Byte -> Byte -> Byte -> Chip8 -> Bool
willCollide x y b c = any id $ map (\(a,b) -> a == b) oldnewbits
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
