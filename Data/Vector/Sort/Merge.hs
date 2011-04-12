module Data.Vector.Sort.Merge (sort, sortBy) where

import Data.Bits
import Data.Vector.Generic

import Data.Vector.Sort.Merge.Stream

import qualified Data.Vector.Sort.Insertion as Ins

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P

import Prelude hiding (length)

{-# INLINE mergeVectors #-}
mergeVectors :: Vector v a => (a -> a -> Bool) -> v a -> v a -> v a
mergeVectors (<=) xs ys = unstream (mergeStreams (<=) (stream xs) (stream ys))

{-# SPECIALIZE sort ::
      P.Vector Int -> P.Vector Int,
      Ord a => V.Vector a -> V.Vector a #-}
sort :: (Vector v a, Ord a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: Vector v a => (a -> a -> Bool) -> v a -> v a
sortBy (<=?) = let
  mergeSort xs
    | n <= 20	= Ins.sortBy (<=?) xs
    | otherwise	= let
	!n' = n `shiftR` 1
	in mergeVectors (<=?) (unsafeTake n' xs) (unsafeDrop n' xs)
    where n = length xs
  in mergeSort