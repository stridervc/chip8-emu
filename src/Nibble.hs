module Nibble
  ( Nibble
  ) where

import GHC.Real
import GHC.Enum

data Nibble = Nibble Int

instance Bounded Nibble where
  minBound  = 0
  maxBound  = 15

instance Num Nibble where
  (Nibble a) + (Nibble b) = Nibble (a+b)
  (Nibble a) * (Nibble b) = Nibble (a*b)
  abs                     = id
  signum (Nibble 0)       = 0
  signum _                = 1
  fromInteger x           = Nibble (fromIntegral (x `mod` 16))
  negate (Nibble a)       = Nibble (0xf - a)

instance Eq Nibble where
  (==)  = eqNibble

eqNibble :: Nibble -> Nibble -> Bool
eqNibble (Nibble a) (Nibble b) = a == b

instance Integral Nibble where
  quotRem (Nibble a) (Nibble b) = (Nibble (quot a b), Nibble (rem a b))
  toInteger (Nibble a)          = fromIntegral a

instance Real Nibble where
  toRational a  = toInteger a % 1

instance Enum Nibble where
  toEnum i
    | i >= 0 && i <= 0xf  = Nibble $ fromIntegral i
    | otherwise           = toEnumError "Nibble" i (minBound::Nibble, maxBound::Nibble)
  fromEnum (Nibble a)     = a

instance Show Nibble where
  showsPrec p x   = showsPrec p (fromIntegral x::Int)
  show (Nibble a) = show a

instance Ord Nibble where
  (Nibble a) <= (Nibble b)  = a <= b
