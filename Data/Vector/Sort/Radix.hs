{-# LANGUAGE ScopedTypeVariables #-}
module Data.Vector.Sort.Radix (sort, Radix(..)) where

import Control.Monad (when)

import Data.Vector.Sort.Radix.Class
import Data.Vector.Sort.Radix.Pass
import Data.Vector.Sort.Types

import Data.Vector.Generic (convert)

import Data.Int
import Data.Word

{-# SPECIALIZE sort ::
      PVector Int -> PVector Int,
      PVector Word -> PVector Word,
      PVector Int32 -> PVector Int32,
      PVector Word32 -> PVector Word32,
      PVector Int64 -> PVector Int64,
      PVector Word64 -> PVector Word64 #-}
sort :: forall v a . (Vector v a, Radix a) => v a -> v a
sort xs = convert (modify sortM (convert xs :: PVector a))

sortM :: forall s a . Radix a => PMVector s a -> ST s ()
sortM arr = do
  let go_radix i = when (i < passes (undefined :: a)) $ do
	  radixPass i arr
	  go_radix (i+1)
  go_radix 0