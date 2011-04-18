{-# LANGUAGE ImplicitParams #-}
module Data.Vector.Sort.Heap.Pairing (sortBy) where

import Control.Monad.ST

import Data.Vector.Sort.Common
import Data.Vector.Fusion.Stream.Monadic
import Data.Vector.Sort.Heap.PHeap

{-# INLINE sortBy #-}
sortBy :: Vector v a => LEq a -> v a -> v a
sortBy = sortPermM sortByM

mstream :: PMVector s Elem -> Stream (ST s) Elem
mstream !xs = mapM (read xs) (enumFromStepN 0 1 (lengthM xs))

sortByM :: (?cmp :: Comparator) => PMVector s Elem -> ST s ()
sortByM = sequentialSort $ \ xs -> do
  !heap <- fromStream (mstream xs)
  fill xs (stream heap)