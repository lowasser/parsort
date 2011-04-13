{-# LANGUAGE FlexibleContexts #-}
module Data.Vector.Sort.Parallel.Merge (sort, sortBy) where

import Control.Parallel

import Data.Bits

import Data.Vector.Generic (stream, unstream)

import Data.Vector.Sort.Merge.Stream
import qualified Data.Vector.Sort.Merge as Merge

import Data.Vector.Sort.Types

import Prelude hiding (length, take, drop, null)

{-# INLINE mergeVectors #-}
mergeVectors :: Vector v a => (a -> a -> Bool) -> v a -> v a -> v a
mergeVectors (<=) xs ys = unstream (mergeStreams (<=) (stream xs) (stream ys))

{-# SPECIALIZE sort ::
      PVector Int -> PVector Int,
      Ord a => VVector a -> VVector a #-}
sort :: (Vector v a, Movable (Mutable v) a, Ord a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: (Vector v a, Movable (Mutable v) a) => (a -> a -> Bool) -> v a -> v a
sortBy (<=?) = let
  mergeSort xs
    | n <= 5000	= Merge.sortBy (<=?) xs
    | otherwise	= let
	!n' = n `shiftR` 1
	xs1' = mergeSort (take n' xs)
	xs2' = mergeSort (drop n' xs)
	in xs1' `par` xs2' `pseq` mergeVectors (<=?) xs1' xs2'
    where n = length xs
  in mergeSort
