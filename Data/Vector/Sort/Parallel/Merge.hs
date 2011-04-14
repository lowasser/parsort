{-# LANGUAGE ImplicitParams, BangPatterns, DoAndIfThenElse #-}
module Data.Vector.Sort.Parallel.Merge (sort, sortBy) where

import Control.Monad.Primitive

import Data.Bits

import Data.Vector.Sort.Types
import Data.Vector.Sort.Comparator
import Data.Vector.Sort.Parallel.Utils
import Data.Vector.Sort.Merge.Inplace
import qualified Data.Vector.Sort.Insertion as Ins

import Prelude hiding (length, take, drop, read)

{-# INLINE sort #-}
sort :: (Vector v a, Ord a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: Vector v a => LEq a -> v a -> v a
sortBy (<=?) xs = unsafePerformIO (sortPermIO sortImpl (<=?) xs)

sortImpl :: (?cmp :: Comparator) => PMVector RealWorld Int -> IO ()
sortImpl xs
  | n <= 20	= primToPrim $ Ins.sortByM xs
  | otherwise	= do
      let !n' = n `shiftR` 1
      doBoth (sortImpl (takeM n' xs)) (sortImpl (dropM n' xs))
      primToPrim $ mergeLo n' xs
  where n = lengthM xs