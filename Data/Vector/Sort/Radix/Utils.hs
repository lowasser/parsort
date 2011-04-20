{-# LANGUAGE ScopedTypeVariables #-}
module Data.Vector.Sort.Radix.Utils (countUp) where

import Control.Monad.Primitive
import Data.Primitive

import Data.Vector.Generic (stream, unsafeFreeze)
import Data.Vector.Generic.Mutable (unsafeAccum)
import Data.Vector.Primitive.Mutable (unsafeNew, MVector(..))
import Data.Vector.Sort.Types

import Prelude hiding (read, mapM_)

{-# INLINE zeroOut #-}
zeroOut :: forall m a . (Prim a, PrimMonad m) => PMVector (PrimState m) a -> m ()
zeroOut (MVector off n mb) = memsetByteArray mb (off * siz) 0 (n * siz)
  where siz = sizeOf (undefined :: a)

{-# INLINE countUp #-}
countUp :: Int -> PVector Int -> PVector Int
countUp sz buckets = unsafeInlineST $ do
  counts <- unsafeNew sz
  zeroOut counts
  unsafeAccum (+) counts (fmap (\ b -> (b, 1)) (stream buckets))
  unsafeFreeze counts