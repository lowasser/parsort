{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module Data.Vector.Sort.Parallel.Radix.Pass (radixPass) where

import GHC.Conc
import Control.Concurrent
import Control.Monad.Primitive
import Control.Monad (replicateM_)

import Data.Vector.Sort.Types
import Data.Vector.Sort.Radix.Class
import Data.Vector.Sort.Radix.Utils
import Data.Vector.Sort.Parallel.Stream

import Data.Vector.Generic.Mutable (unsafeNew)

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P

import Prelude hiding (read)

type RadixPass a = Int -> PMVector RealWorld a -> IO ()

{-# INLINE radixPass #-}
radixPass :: forall a . Radix a => RadixPass a
radixPass !pass !arr = do
  arrF <- P.unsafeFreeze arr
  let !n = lengthM arr
      !nCaps = numCapabilities
      !chunkSize = (n + nCaps - 1) `quot` nCaps
      chunkAt i = let k = chunkSize `min` n - i in slice i k arrF
      rSize = size (undefined :: a)
      chunkCount i = countUp rSize $ P.map (radix pass) (chunkAt i)
      chunkCounts = parVector (V.map chunkCount $ V.enumFromStepN 0 chunkSize nCaps)
      cumCounts = V.scanl (P.zipWith (+)) (P.replicate rSize 0) chunkCounts
      totCounts = index cumCounts nCaps
      !offsets = P.prescanl (+) 0 totCounts
  tmp <- unsafeNew n
  lock <- newEmptyMVar
  let go_move !i = do
	counter <- P.unsafeThaw (P.zipWith (+) offsets (index cumCounts i))
	let do_move x = do
	      let !b = radix pass x
	      i <- read counter b
	      write counter b (i+1)
	      write tmp i x
	P.mapM_ do_move (chunkAt (i * chunkSize))
	putMVar lock ()
  P.mapM_ (forkIO . go_move) (P.enumFromN 0 nCaps)
  replicateM_ nCaps (takeMVar lock)
  copyM arr tmp