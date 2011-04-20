{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module Data.Vector.Sort.Radix.Pass (radixPass) where

import Control.Monad.ST

import Data.Vector.Sort.Types
import Data.Vector.Generic (stream, unsafeFreeze, mapM_, unsafeThaw)
import qualified Data.Vector.Fusion.Stream as Stream
import Data.Vector.Generic.Mutable (unstream, unsafeAccum, basicUnsafeReplicate)
import Data.Vector.Primitive (map, prescanl, Prim)

import Data.Vector.Sort.Radix.Class

import Data.Int

import Prelude hiding (map, length, replicate, read, mapM_)

type RadixPass s a = Int -> PMVector s a -> PMVector s a -> ST s ()
{-# INLINE radixPass #-}
radixPass :: forall s a . (Prim a, Radix a) => RadixPass s a
radixPass !pass !arr !tmp = do
  let n = lengthM arr
  arrF <- unsafeFreeze arr
  let rSize = size (undefined :: a)
  counts <- basicUnsafeReplicate n 0
  unsafeAccum (+) counts (Stream.map (\ x -> (radix pass x, 1)) (stream arrF))
  movePass pass arrF tmp =<< unsafeFreeze counts
  copyM arr tmp

{-# INLINE movePass #-}
movePass :: forall s a . (Prim a, Radix a) => Int -> PVector a -> PMVector s a -> PVector Int -> ST s ()
movePass !pass !arrF !tmp counts = do
  prefix <- unstream (stream (prescanl (+) 0 counts)) :: ST s (PMVector s Int)
  let do_move x = do
	let !b = radix pass x
	i <- read prefix b
	write prefix b (i+1)
	write tmp i x
  mapM_ do_move arrF
  