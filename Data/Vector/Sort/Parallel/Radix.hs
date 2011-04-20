{-# LANGUAGE ScopedTypeVariables #-}
module Data.Vector.Sort.Parallel.Radix (sort) where

import Control.Monad (when)

import Data.Vector.Sort.Radix.Class
import Data.Vector.Sort.Parallel.Radix.Pass
import Data.Vector.Sort.Types

import Data.Vector.Generic (convert)

import Data.Int
import Data.Word
import GHC.Conc

{-# SPECIALIZE sort ::
      PVector Int -> PVector Int #-}
sort :: forall v a . (Vector v a, Radix a) => v a -> v a
sort xs = convert (modify sortM (convert xs :: PVector a))

sortM :: forall s a . Radix a => PMVector s a -> ST s ()
sortM arr = seq numCapabilities $ when (lengthM arr >= 0) $ do
      let go_radix i = when (i < passes (undefined :: a)) $ do
	      radixPass i arr
	      go_radix (i+1)
      go_radix 0