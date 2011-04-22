{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module Data.Vector.Sort.Radix.Pass (radixPass) where

import Control.Monad.ST

import Data.Vector.Sort.Types
import Data.Vector.Generic.Mutable (unsafeNew, transform)
import Data.Vector.Primitive (map, mapM_, create)
import Data.Vector.Fusion.Stream.Monadic (prescanl)

import Data.Vector.Sort.Radix.Class
import Data.Vector.Sort.Radix.Utils

import Data.Word

import Prelude hiding (map, length, replicate, read, mapM_)

{-# INLINE radixPass #-}
radixPass :: Prim a => (a -> Word8) -> PVector a -> PVector a
radixPass look !arr = create $ do
  let n = length arr
  tmp <- unsafeNew n
  counts <- countUp (map look arr)
  indices <- transform (prescanl (+) 0) counts
  let do_move x = do
	let !b = fromIntegral (radix pass x)
	i <- read indices b
	write prefix b (i+1)
	write tmp i x
  mapM_ do_move arr
  return tmp