{-# LANGUAGE ImplicitParams, BangPatterns, DoAndIfThenElse #-}
module Data.Vector.Sort.Merge (sort, sortBy, sortImpl) where

import Control.Monad.ST

import Data.Bits

import Data.Vector.Sort.Types
import Data.Vector.Sort.Comparator
import Data.Vector.Sort.Merge.Inplace
import qualified Data.Vector.Sort.Insertion as Ins

import Prelude hiding (length, take, drop, read)

{-# INLINE sort #-}
sort :: (Vector v a, Ord a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: Vector v a => LEq a -> v a -> v a
sortBy = sortPermM sortImpl

{-# INLINE sortImpl #-}
sortImpl :: (?cmp :: Comparator) => PMVector s Int -> ST s ()
sortImpl xs
  | n <= 20	= Ins.sortByM xs
  | otherwise	= do
      let !n' = n `shiftR` 1
      sortImpl (takeM n' xs)
      sortImpl (dropM n' xs)
      mergeLo n' xs
  where n = lengthM xs