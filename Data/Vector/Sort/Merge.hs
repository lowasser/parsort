{-# LANGUAGE FlexibleContexts #-}
module Data.Vector.Sort.Merge (sort, sortBy) where

import Data.Bits
import Data.Vector.Generic (stream, unstream)

import Data.Vector.Sort.Merge.Stream

import Data.Vector.Sort.Types
import qualified Data.Vector.Sort.Insertion as Ins

import Prelude hiding (length, take, drop)

{-# SPECIALIZE sort ::
      PVector Int -> PVector Int,
      Ord a => VVector a -> VVector a #-}
sort :: (Vector v a, Movable (Mutable v) a, Ord a) => v a -> v a
sort = sortBy (<=)

{-# INLINE mergeVectors #-}
mergeVectors :: Vector v a => LEq a -> v a -> v a -> v a
mergeVectors (<=) xs ys = unstream (mergeStreams (<=) (stream xs) (stream ys))

{-# INLINE sortBy #-}
sortBy :: (Vector v a, Movable (Mutable v) a) => LEq a -> v a -> v a
sortBy (<=?) = let
  mergeSort xs
    | n <= 20	= Ins.sortBy (<=?) xs
    | otherwise	= let
	!n' = n `shiftR` 1
	in mergeVectors (<=?) (mergeSort $ take n' xs) (mergeSort $ drop n' xs)
    where n = length xs
  in mergeSort