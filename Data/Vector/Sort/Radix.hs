{-# LANGUAGE ScopedTypeVariables #-}
module Data.Vector.Sort.Radix (sort, Radix(..)) where

import Control.Monad (when)

import Data.Vector.Sort.Radix.Class
import Data.Vector.Sort.Radix.Pass
import Data.Vector.Sort.Types

import Data.Vector.Generic.Mutable

import Data.Int
import Data.Word

{-# SPECIALIZE sort ::
      PVector Int -> PVector Int,
      PVector Word -> PVector Word,
      PVector Int32 -> PVector Int32,
      PVector Word32 -> PVector Word32,
      PVector Int64 -> PVector Int64,
      PVector Word64 -> PVector Word64 #-}
sort :: Radix a => PVector a -> PVector a
sort = modify sortM

sortM :: forall s a . Radix a => PMVector s a -> ST s ()
sortM arr = do
  let !n = lengthM arr
  tmp <- basicUnsafeNew n
  let go_radix i = when (i < passes (undefined :: a)) $ do
	  radixPass i arr tmp
	  go_radix (i+1)
  go_radix 0