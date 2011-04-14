{-# LANGUAGE ImplicitParams #-}
module Data.Vector.Sort.Insertion.Binary where

import Control.Monad

import Data.Bits

import Data.Vector.Sort.Types
import Data.Vector.Sort.Comparator
import Data.Vector.Algorithms.Optimal

import Prelude hiding (length, take, drop, read)

{-# INLINE sort #-}
sort :: (Vector v a, Ord a) => v a -> v a
sort = sortBy (<=)

{-# INLINE sortBy #-}
sortBy :: Vector v a => LEq a -> v a -> v a
sortBy = sortPermM (\ xs -> sortByM xs 1)

sortByM :: (?cmp :: Comparator) => PMVector s Int -> Int -> ST s ()
sortByM xs start = let start' = start - 1 in case lengthM xs - start' of
  0	-> return ()
  1	-> return ()
  2	-> sort2ByOffset comp xs start'
  3	-> sort3ByOffset comp xs start'
  4	-> sort4ByOffset comp xs start'
  _	-> sort4ByOffset comp xs start' >> run (start + 4) where
    !n = lengthM xs
    binarySearch key l u cont = bin l u where
      bin l u
	| u <= l	= cont l
	| otherwise = do
	    let k = l + (u - l) `shiftR` 1
	    x <- read xs k
	    if x <=? key then bin (k+1) u else bin l k
    run start = when (start < n) $ do
      x <- read xs start
      binarySearch x 0 start $ \ k -> do
	moveBy xs k (start-k) 1
	write xs k x
	run (start+1)
  where comp x y = if x <=? y then LT else GT