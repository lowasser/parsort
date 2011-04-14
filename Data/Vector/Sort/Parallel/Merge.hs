{-# LANGUAGE ImplicitParams, BangPatterns, DoAndIfThenElse #-}
module Data.Vector.Sort.Parallel.Merge (sort, sortBy) where

import Control.Parallel

import Data.Bits

import Data.Vector.Sort.Types
import Data.Vector.Sort.Comparator
import Data.Vector.Sort.Parallel.Utils
import Data.Vector.Sort.Merge.Stream

import qualified Data.Vector.Sort.Merge as Seq

import Data.Vector.Generic (modify, stream, unstream)

import qualified Data.Vector.Sort.Insertion as Ins

import Prelude hiding (length, take, drop, read)

{-# INLINE sort #-}
sort :: (Vector v a, Ord a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: Vector v a => LEq a -> v a -> v a
sortBy = sortPerm sortImpl

sortImpl :: (?cmp :: Comparator) => PVector Int -> PVector Int
sortImpl xs
  | n <= 1000	= modify Seq.sortImpl xs
  | otherwise	= let
      left = sortImpl (take n' xs)
      right = sortImpl (drop n' xs)
      in left `par` right `pseq` unstream (mergeStreams (<=?) (stream left) (stream right))
  where n = length xs; !n' = n `shiftR` 1