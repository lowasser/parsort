{-# LANGUAGE ScopedTypeVariables #-}
module Data.Vector.Sort.Radix.Utils where

import Control.Monad.Primitive
import Data.Primitive

import Data.Vector.Generic (stream)
import Data.Vector.Generic.Mutable (unsafeAccum)
import Data.Vector.Primitive.Mutable (unsafeNew, MVector(..))
import Data.Vector.Sort.Types

import Data.Word

import Prelude hiding (read, mapM_)

{-# INLINE zeroOut #-}
zeroOut :: forall m a . (Prim a, PrimMonad m) => PMVector (PrimState m) a -> m ()
zeroOut (MVector off n mb) = memsetByteArray mb (off * siz) 0 (n * siz)
  where siz = sizeOf (undefined :: a)

{-# INLINE countUp #-}
countUp :: PVector Word8 -> ST s (PMVector s Int)
countUp buckets = do
  counts <- unsafeNew 256
  zeroOut counts
  unsafeAccum (+) counts (fmap (\ b -> (fromIntegral b, 1)) (stream buckets))
  return counts
