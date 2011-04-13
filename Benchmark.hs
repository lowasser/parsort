module Benchmark where

import Control.Monad

import Criterion.Main
import System.Random.MWC

import Data.Vector.Primitive (Vector, convert)

import qualified Data.Vector.Sort.Insertion as Ins
import qualified Data.Vector.Sort.Insertion.Binary as BI
import qualified Data.Vector.Sort.Merge as Merge
import qualified Data.Vector.Sort.Parallel.Merge as MergePar
import qualified Data.Vector.Sort.Quick as Quick
import qualified Data.Vector.Sort.Parallel.Quick as QuickPar

insertion, binaryInsertion, merge, mergePar, quick,  quickPar :: Vector Int -> Benchmark
insertion xs = bench "Insertion sort" (whnf Ins.sort xs)
binaryInsertion xs = bench "Binary insertion sort" (whnf BI.sort xs)
merge xs = bench "Merge sort" (whnf Merge.sort xs)
mergePar xs = bench "Parallel merge sort" (whnf MergePar.sort xs)
quick xs = bench "Quick sort" (whnf Quick.sort xs)
quickPar xs = bench "Parallel quick sort" (whnf QuickPar.sort xs)

benchForSize :: GenIO -> Int -> IO Benchmark
benchForSize g n = do
  xs0 <- uniformVector g n
  let !xs = convert xs0 :: Vector Int
  let slowTests = [binaryInsertion, insertion]
  let fastTests = if n <= 1000 then [] else [merge, mergePar, quick, quickPar]
  return $ bgroup (show n) (map ($ xs) (slowTests ++ fastTests))

main = withSystemRandom $ \ g -> 
  defaultMain =<< mapM (benchForSize g) [10, 100, 1000, 1000000]