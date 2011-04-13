{-# LANGUAGE FlexibleContexts #-}
module Data.Vector.Sort.Quick where

import Control.Monad.Primitive

import Data.Bits

import Data.Vector.Generic.Mutable (unstablePartition)

import Data.Vector.Sort.Types
import qualified Data.Vector.Sort.Insertion as Ins

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P

import Prelude hiding (length, read)

{-# SPECIALIZE sort ::
      P.Vector Int -> P.Vector Int,
      (Ord a) => V.Vector a -> V.Vector a #-}
sort :: (Vector v a, Movable (Mutable v) a, Ord a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: (Vector v a, Movable (Mutable v) a) => LEq a -> v a -> v a
sortBy (<=?) = modify (sortByM (<=?))

{-# INLINE sortByM #-}
sortByM :: (PrimMonad m, Movable v a) => LEq a -> v (PrimState m) a -> m ()
sortByM (<=?) = let
  qSort xs
    | n <= 20	= Ins.sortByM (<=?) xs
    | otherwise	= do
	a <- read xs 0
	b <- read xs (n `shiftR` 1)
	c <- read xs (n - 1)
	let !pivot = medOf3 (<=?) a b c
	pivotIndex <- unstablePartition (<=? pivot) xs
	qSort (takeM pivotIndex xs)
	qSort (dropM pivotIndex xs)
    where n = lengthM xs
  in qSort

medOf3 :: LEq a -> a -> a -> a -> a
medOf3 (<=) a b c = case (a <= b, b <= c, a <= c) of
  (True, True, _)	-> b
  (True, False, True)	-> c
  (True, False, False)	-> a
  (False, _, True)	-> a
  (False, True, False)	-> c
  (False, False, _)	-> b