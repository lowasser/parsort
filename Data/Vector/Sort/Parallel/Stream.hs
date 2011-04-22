{-# LANGUAGE BangPatterns #-}
module Data.Vector.Sort.Parallel.Stream (parVector) where

import GHC.Conc

import Data.Vector.Fusion.Stream.Monadic
import Data.Vector.Generic (stream, unstream, Vector)

{-# INLINE parStream #-}
parStream :: Monad m => Stream m a -> Stream m a
parStream (Stream step s0 n) = Stream step' s0 n where
  step' s = do
    suc <- step s
    case suc of
      Done -> return Done
      Skip s' -> return (Skip s')
      Yield a s' -> return ((a `par` ()) `seq` Yield a s')

{-# INLINE parVector #-}
parVector :: Vector v a => v a -> v a
parVector vec = unstream (parStream (stream vec))