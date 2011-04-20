{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module Data.Vector.Sort.Radix.Pass (radixPass) where

import Control.Monad.ST

import Data.Vector.Sort.Types
import Data.Vector.Generic (unsafeFreeze, mapM_)
import Data.Vector.Generic.Mutable (unsafeNew)
import Data.Vector.Primitive (prescanl, Prim, map, unsafeThaw)

import Data.Vector.Sort.Radix.Class
import Data.Vector.Sort.Radix.Utils

import Data.Int

import Prelude hiding (map, length, replicate, read, mapM_)

type RadixPass s a = Int -> PMVector s a -> ST s ()
{-# INLINE radixPass #-}
radixPass :: forall s a . (Prim a, Radix a) => RadixPass s a
radixPass !pass !arr = do
  let n = lengthM arr
  arrF <- unsafeFreeze arr
  let rSize = size (undefined :: a)
      counts = countUp rSize (map (radix pass) arrF)
  tmp <- unsafeNew n
  movePass pass arrF tmp counts
  copyM arr tmp

{-# INLINE movePass #-}
movePass :: forall s a . (Prim a, Radix a) => Int -> PVector a -> PMVector s a -> PVector Int -> ST s ()
movePass !pass !arrF !tmp counts = do
  prefix <- unsafeThaw (prescanl (+) 0 counts) :: ST s (PMVector s Int)
  let do_move x = do
	let !b = radix pass x
	i <- read prefix b
	write prefix b (i+1)
	write tmp i x
  mapM_ do_move arrF
  