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
  | n < 40 = medOf3 l0 m0 r0 cont0
  | otherwise = let
      off = n `shiftR` 3
      i0 = l0
      i1 = i0 + off
      i2 = i1 + off
      i3 = m0 - off
      i4 = m0
      i5 = m0 + off
      i6 = i7 - off
      i7 = i8 - off
      i8 = r0
      in medOf3 i0 i1 i2 $ \ !l ->
	  medOf3 i3 i4 i5 $ \ !m ->
	  medOf3 i6 i7 i8 $ \ !r ->
	  medOf3 l m r cont0
  where	n = lengthM xs
	l0 = 0
	m0 = n `shiftR` 1
	r0 = n - 1
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