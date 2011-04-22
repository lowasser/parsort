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

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P

import Prelude hiding (read)

type RadixPass a = Int -> PMVector RealWorld a -> PMVector RealWorld a -> IO ()

{-# INLINE radixPass #-}
radixPass :: forall a . Radix a => RadixPass a
radixPass !pass !arr !tmp = do
  arrF <- P.unsafeFreeze arr
  let !n = lengthM arr
      !nCaps = numCapabilities
      !chunkSize = (n + nCaps - 1) `quot` nCaps
      chunkAt i = let k = chunkSize `min` n - i in slice i k arrF
      chunkCount i = P.create $ countUp $ P.map (radix pass) (chunkAt i)
      !chunkCounts = parVector (V.map chunkCount $ V.enumFromStepN 0 chunkSize nCaps)
      !cumCounts = V.scanl (P.zipWith (+)) (P.replicate 256 0) chunkCounts
      totCounts = index cumCounts nCaps
      !offsets = P.prescanl (+) 0 totCounts
  lock <- newEmptyMVar
  let go_move !i = do
	!counter <- P.thaw (P.zipWith (+) offsets (index cumCounts i))
	let do_move x = do
	      let !b = fromIntegral $ radix pass x
	      i <- read counter b
	      write counter b (i+1)
	      write tmp i x
	P.mapM_ do_move (chunkAt (i * chunkSize))
	putMVar lock ()
  P.mapM_ (\ i -> forkOnIO i $ go_move i) (P.enumFromN 0 nCaps)
  replicateM_ nCaps (takeMVar lock)
  copyM arr tmp
