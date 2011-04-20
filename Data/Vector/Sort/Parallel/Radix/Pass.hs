{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module Data.Vector.Sort.Parallel.Radix.Pass (radixPass) where

import Control.Monad.Primitive
import GHC.Conc

import Data.Vector.Sort.Types
import Data.Vector.Sort.Radix.Class
import Data.Vector.Sort.Radix.Utils
import Data.Vector.Sort.Parallel.Stream

import Data.Vector.Generic.Mutable (unsafeNew)

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P

import Prelude hiding (read)

type RadixPass s a = Int -> PMVector s a -> ST s ()

{-# INLINE radixPass #-}
radixPass :: forall s a . Radix a => RadixPass s a
radixPass !pass !arr = do
  arrF <- P.unsafeFreeze arr
  let !n = lengthM arr
      !nCaps = numCapabilities
      !chunkSize = (n + nCaps - 1) `quot` nCaps
      chunkAt i = let k = chunkSize `min` n - i in slice i k arrF
      rSize = size (undefined :: a)
      chunkCount i = countUp rSize $ P.map (radix pass) (chunkAt i)
      chunkCounts = parVector (V.map chunkCount $ V.enumFromStepN 0 chunkSize nCaps)
      cumCounts = V.prescanl (P.zipWith (+)) (P.replicate rSize 0) chunkCounts
  tmp <- unsafeNew n
  V.zipWithM_ (\ cumCount i -> do
    counter <- P.unsafeThaw cumCount
    let !chunk = chunkAt i
	do_move x = do
	  let !b = radix pass x
	  i <- read counter b
	  write counter b (i+1)
	  write tmp i x
    P.mapM_ do_move chunk)
    cumCounts (V.map (* chunkSize) (V.enumFromN 0 nCaps))
  copyM arr tmp