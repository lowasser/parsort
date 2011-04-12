module Data.Vector.Sort.Parallel.Merge (mergeSort) where

import Control.Parallel
import Control.Monad.Primitive

import Data.Bits
import Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable as M

import Data.Vector.Sort.Parallel.Merge.Stream
import Data.Vector.Sort.Insertion

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P

import Prelude hiding (length, take, drop, null)

{-# INLINE mergeVectors #-}
mergeVectors :: (Ord a, Vector v a) => v a -> v a -> v a
mergeVectors xs ys = inline $ unstream (mergeStreams (stream xs) (stream ys))

{-# SPECIALIZE mergeSort ::
      P.Vector Int -> P.Vector Int,
      Ord a => V.Vector a -> V.Vector a #-}
mergeSort :: (Ord a, Vector v a) => v a -> v a
mergeSort xs
  | n <= 20	= insertionSort xs
  | otherwise	= let
      !n' = n `shiftR` 1
      xs1' = mergeSort (unsafeTake n' xs)
      xs2' = mergeSort (unsafeDrop n' xs)
      in xs1' `par` xs2' `pseq` mergeVectors xs1' xs2'
    where n = length xs
