{-# LANGUAGE ImplicitParams, BangPatterns, DoAndIfThenElse #-}
module Data.Vector.Sort.Merge (sort, sortBy) where

import Control.Monad.ST

import Data.Bits
import Data.Vector.Generic (freeze)

import Data.Vector.Sort.Types
import Data.Vector.Sort.Comparator
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

mergeLo :: (?cmp :: Comparator) => Int -> PMVector s Int -> ST s ()
mergeLo k xs = do
  run1 <- freeze (takeM k xs)
  let run2 = dropM k xs
  let !n1 = length (run1 :: PVector Int); !n2 = lengthM run2
  let go !i1 !i2 !iWrite
	| i1 >= n1	= return ()
	| i2 >= n2	= copy curDest curRun1
	| otherwise	= do
	    x1 <- indexM curRun1 0
	    x2 <- read curRun2 0
	    if x1 <=? x2 then do
	      write curDest 0 x1
	      go (i1+1) i2 (iWrite+1)
	    else do
	      write curDest 0 x2
	      go i1 (i2+1) (iWrite+1)
	where curRun1 = drop i1 run1
	      curRun2 = dropM i2 run2
	      curDest = dropM iWrite xs
  go 0 0 0