module Data.Vector.Sort.Parallel.Merge (sort, sortBy) where

import Control.Parallel
import Control.Monad.Primitive

import Data.Bits
import Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable as M

import Data.Vector.Sort.Merge.Stream
import qualified Data.Vector.Sort.Insertion as Ins
import qualified Data.Vector.Sort.Merge as Merge

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P

import Prelude hiding (length, take, drop, null)

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
    | n <= 5000	= Merge.sortBy (<=?) xs
    | otherwise	= let
	!n' = n `shiftR` 1
	xs1' = mergeSort (unsafeTake n' xs)
	xs2' = mergeSort (unsafeDrop n' xs)
	in xs1' `par` xs2' `pseq` mergeVectors (<=?) xs1' xs2'
    where n = length xs
  in mergeSort
