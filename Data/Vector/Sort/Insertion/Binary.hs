{-# LANGUAGE BangPatterns, ScopedTypeVariables, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Data.Vector.Sort.Insertion.Binary where

import Control.Monad
import Control.Monad.Primitive

import Data.Bits

import Data.Vector.Algorithms.Search

import Data.Vector.Generic (Vector, modify, Mutable)
import Data.Vector.Generic.Mutable
import Data.Vector.Generic.Mutable.Move

import Prelude hiding (length, take, drop, read)

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P

class (Vector v a, Movable (Mutable v) a) => BaseVector v a
instance (Vector v a, Movable (Mutable v) a) => BaseVector v a

{-# SPECIALIZE sort :: P.Vector Int -> P.Vector Int #-}
{-# INLINE sort #-}
sort :: (BaseVector v a, Ord a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: BaseVector v a => (a -> a -> Bool) -> v a -> v a
sortBy (<=) = modify (sortByM (<=))

{-# INLINE sortByM #-}
sortByM :: (Movable v a, PrimMonad m) => (a -> a -> Bool) -> v (PrimState m) a -> m ()
sortByM (<=?) xs = run (n-1) where
  !n = length xs
  binarySearch key cont = bin where
    bin l u
      | u <= l	= cont l
      | otherwise = do
	  let k = (u + l) `shiftR` 1
	  x <- unsafeRead xs k
	  if x <=? key then bin (k+1) u else bin l k
  run off = when (off > 0) $ do
    x <- unsafeRead xs (off - 1)
    let insertAt i = do
	  let moveSrc = unsafeDrop off (unsafeTake i xs)
	  let moveDst = unsafeDrop (off-1) (unsafeTake (i-1) xs)
	  unsafeMove moveDst moveSrc
	  unsafeWrite xs (i-1) x
    binarySearch x insertAt off n
    run (off-1)