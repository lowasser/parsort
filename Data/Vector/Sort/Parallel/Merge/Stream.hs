{-# LANGUAGE BangPatterns #-}
module Data.Vector.Sort.Parallel.Merge.Stream where

import Data.Vector.Fusion.Stream.Monadic

data MergeState sL sR a = LR sL sR | L0 sL | R0 sR | L a sL sR | R sL a sR

{-# INLINE mergeStreams #-}
mergeStreams :: (Monad m, Ord a) => Stream m a -> Stream m a -> Stream m a
mergeStreams (Stream stepL start1 sz1) (Stream stepR start2 sz2) =
  Stream step (LR start1 start2) (sz1 + sz2)
  where	step (LR !sL !sR) = do
	  resL <- stepL sL
	  case resL of
	    Done	-> return (Skip (R0 sR))
	    Skip sL'	-> return (Skip (LR sL' sR))
	    Yield aL sL' -> return (Skip (L aL sL' sR))
	step (L !aL !sL !sR) = do
	  resR <- stepR sR
	  case resR of
	    Done	-> return (Yield aL (L0 sL))
	    Skip sR'	-> return (Skip (L aL sL sR'))
	    Yield aR sR'
	      | aL <= aR -> return (Yield aL (R sL aR sR'))
	      | otherwise -> return (Yield aR (L aL sL sR'))
	step (R !sL !aR !sR) = do
	  resL <- stepL sL
	  case resL of
	    Done	-> return (Yield aR (R0 sR))
	    Skip sL'	-> return (Skip (R sL' aR sR))
	    Yield aL sL'
	      | aL <= aR -> return (Yield aL (R sL' aR sR))
	      | otherwise -> return (Yield aR (L aL sL' sR))
	step (L0 !sL) = do
	  resL <- stepL sL
	  case resL of
	    Done	-> return Done
	    Skip sL'	-> return (Skip (L0 sL'))
	    Yield aL sL' -> return (Yield aL (L0 sL'))
	step (R0 !sR) = do
	  resR <- stepR sR
	  case resR of
	    Done	-> return Done
	    Skip sR'	-> return (Skip (R0 sR'))
	    Yield aR sR' -> return (Yield aR (R0 sR'))