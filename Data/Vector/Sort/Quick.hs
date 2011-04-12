module Data.Vector.Sort.Quick (sort, sortBy) where

import Control.Monad.Primitive

import Data.Bits

import Data.Vector.Generic (Vector, modify)
import Data.Vector.Generic.Mutable

import qualified Data.Vector.Sort.Insertion as Ins

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P

import Prelude hiding (length)

{-# SPECIALIZE sort ::
      P.Vector Int -> P.Vector Int,
      Ord a => V.Vector a -> V.Vector a #-}
sort :: (Vector v a, Ord a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: Vector v a => (a -> a -> Bool) -> v a -> v a
sortBy (<=?) = modify (quickSortM (<=?))

{-# INLINE quickSortM #-}
quickSortM :: (PrimMonad m, MVector v a) => (a -> a -> Bool) -> v (PrimState m) a -> m ()
quickSortM (<=?) = let
  qSort xs
    | n <= 20	= Ins.sortByM (<=?) xs
    | otherwise	= do
	a <- unsafeRead xs 0
	b <- unsafeRead xs (n `shiftR` 1)
	c <- unsafeRead xs (n - 1)
	let !pivot = medOf3 (<=?) a b c
	pivotIndex <- unstablePartition (<=? pivot) xs
	qSort (unsafeTake (pivotIndex - 1) xs)
	qSort (unsafeDrop pivotIndex xs)
    where n = length xs
  in qSort

medOf3 :: (a -> a -> Bool) -> a -> a -> a -> a
medOf3 (<=) a b c = case (a <= b, b <= c, a <= c) of
  (True, True, _)	-> b
  (True, False, True)	-> c
  (True, False, False)	-> a
  (False, _, True)	-> a
  (False, True, False)	-> c
  (False, False, _)	-> b