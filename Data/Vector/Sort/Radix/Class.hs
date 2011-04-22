{-# LANGUAGE BangPatterns, ScopedTypeVariables, MagicHash, CPP #-}
module Data.Vector.Sort.Radix.Class (Radix(..)) where

import Data.Bits
import Data.Int
import Data.Word
import Data.Primitive.Types (Prim)

import GHC.Word
import GHC.IntWord64
import GHC.Exts

#include "MachDeps.h"

class Prim a => Radix a where
  passes :: a -> Int
  radix :: Int -> a -> Word8

{-# INLINE i2w #-}
i2w :: forall i w . (Bits w, Bits i, Integral i, Bounded i, Integral w) => i -> w
i2w x = complementBit (fromIntegral x) (bitSize x - 1)

#define INT_INST(ty,wty) instance Radix ty where { \
  passes _ = passes (0 :: wty); \
  {-# INLINE radix #-}; \
  radix p i = radix p (i2w i :: wty)}

INT_INST(Int64,Word64)
INT_INST(Int32,Word32)
INT_INST(Int16,Word16)
INT_INST(Int8,Word8)
INT_INST(Int,Word)

instance Radix Word32 where
  passes _ = 4
  {-# INLINE radix #-}
  radix p w = radix p (fromIntegral w :: Word)

instance Radix Word16 where
  passes _ = 2
  {-# INLINE radix #-}
  radix p w = radix p (fromIntegral w :: Word) 

instance Radix Word8 where
  passes _ = 1
  radix _ w = w

instance Radix Word where
  passes _ = WORD_SIZE_IN_BITS `shiftR` 3
  radix (I# p#) (W# w#) = W8# (narrow8Word# (uncheckedShiftRL# w# (p# *# 8#)))

#if WORD_SIZE_IN_BITS < 64
instance Radix Word64 where
  passes _ = 8
  radix (I# p#) (W64# w#) = W8# (narrow8Word# (word64ToWord#
    (uncheckedShiftRL64# w# (p# *# 8#))))
#else
instance Radix Word64 where
  passes _ = 8
  radix p w = radix p (fromIntegral w :: Word)
#endif