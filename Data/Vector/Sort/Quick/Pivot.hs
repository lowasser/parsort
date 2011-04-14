{-# LANGUAGE ImplicitParams, BangPatterns #-}
module Data.Vector.Sort.Quick.Pivot where

import Data.Bits

import Control.Monad.Primitive

import Data.Vector.Sort.Types
import Data.Vector.Sort.Comparator

import Prelude hiding (read)

{-# INLINE pickPivot #-}
-- | Returns the index of a chosen pivot.
pickPivot :: (?cmp :: Comparator, PrimMonad m) => PMVector (PrimState m) Int -> (Int -> m a) -> m a
pickPivot xs cont0
  | n < 40 = let
      i0 = 0
      i1 = n `shiftR` 1
      i2 = n - 1
      in medOf3 i0 i1 i2 cont0
  | otherwise = let
      off = n `shiftR` 3
      i0 = 0
      i1 = off
      i2 = 2 * off
      i3 = 3 * off
      i4 = 4 * off
      i5 = 5 * off
      i6 = 6 * off
      i7 = 7 * off
      i8 = n - 1
      in medOf3 i0 i1 i2 $ \ !l ->
	  medOf3 i3 i4 i5 $ \ !m ->
	  medOf3 i6 i7 i8 $ \ !r ->
	  medOf3 l m r cont0
  where	n = lengthM xs
	{-# INLINE medOf3 #-}
	medOf3 i j k cont = do
	  a <- read xs i
	  b <- read xs j
	  c <- read xs k
	  case (a <=? b, b <=? c, a <=? c) of
	    (True, True, _)	 -> cont j
	    (True, False, True)	 -> cont k
	    (True, False, False) -> cont i
	    (False, _, True)	 -> cont i
	    (False, True, False) -> cont k
	    (False, False, _)	 -> cont j