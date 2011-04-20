{-# LANGUAGE ScopedTypeVariables #-}
module Data.Vector.Sort.Parallel.Radix (sort) where

import Control.Monad (when)
import Control.Monad.Primitive

import Data.Vector.Sort.Radix.Class
import Data.Vector.Sort.Parallel.Radix.Pass
import Data.Vector.Sort.Types

import Data.Vector.Generic (convert, stream, unsafeFreeze)
import Data.Vector.Generic.Mutable (unstream)

import Data.Int
import Data.Word

import GHC.Conc
import System.IO.Unsafe

{-# SPECIALIZE sort ::
      PVector Int -> PVector Int,
      PVector Word -> PVector Word,
      PVector Int32 -> PVector Int32,
      PVector Word32 -> PVector Word32,
      PVector Int64 -> PVector Int64,
      PVector Word64 -> PVector Word64 #-}
sort :: forall v a . (Vector v a, Radix a) => v a -> v a
sort xs = convert $ unsafePerformIO $ do
  mv <- unstream (stream xs)
  sortM mv
  unsafeFreeze mv :: IO (PVector a)

{-# INLINE sortM #-}
sortM :: forall a . Radix a => PMVector RealWorld a -> IO ()
sortM arr = seq numCapabilities $ when (lengthM arr >= 0) $ do
      let go_radix i = when (i < passes (undefined :: a)) $ do
	      radixPass i arr
	      go_radix (i+1)
      go_radix 0