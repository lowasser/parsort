{-# LANGUAGE Rank2Types, ImplicitParams #-}
module Data.Vector.Sort.Common (
  module Data.Vector.Sort.Types,
  module Data.Vector.Sort.Comparator,
  module Data.Vector.Sort.Constants,
  parallelSort,
  sequentialSort) where

import Control.Monad.Primitive
import Control.Monad.ST

import Data.Vector.Sort.Types
import Data.Vector.Sort.Comparator
import Data.Vector.Sort.Constants

import qualified Data.Vector.Sort.Insertion.Binary as BinIns

type SequentialSort = forall s . PMVector s Elem -> ST s ()
type ParallelSort = PMVector RealWorld Elem -> IO ()

{-# INLINE parallelSort #-}
parallelSort :: (?cmp :: Comparator) =>
  SequentialSort -> ParallelSort -> ParallelSort
parallelSort seqImpl parImpl xs
  | lengthM xs <= sEQUENTIAL_SORT_THRESHOLD
	= primToPrim $ seqImpl xs
  | otherwise
  	= parImpl xs

{-# INLINE sequentialSort #-}
sequentialSort :: (?cmp :: Comparator) => SequentialSort -> SequentialSort
sequentialSort seqImpl xs
  | lengthM xs <= sMALL_SORT_THRESHOLD
      = BinIns.sortByM xs 1
  | otherwise
      = seqImpl xs