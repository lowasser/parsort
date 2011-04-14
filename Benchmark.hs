module Benchmark where

import Criterion.Main
import System.Random.MWC

import Data.Vector (Vector, convert, replicateM)

import qualified Data.Vector.Sort.Insertion as Ins
import qualified Data.Vector.Sort.Insertion.Binary as BI
import qualified Data.Vector.Sort.Merge as Merge
import qualified Data.Vector.Sort.Parallel.Merge as MergePar
import qualified Data.Vector.Sort.Quick as Quick
import qualified Data.Vector.Sort.Parallel.Quick as QuickPar
import qualified Data.Vector.Sort.Tim as Tim

insertion, binaryInsertion, merge, mergePar, quick,  quickPar :: Vector Int -> Benchmark
insertion xs = bench "Insertion sort" (whnf Ins.sort xs)
binaryInsertion xs = bench "Binary insertion sort" (whnf BI.sort xs)
merge xs = bench "Merge sort" (whnf Merge.sort xs)
mergePar xs = bench "Parallel merge sort" (whnf MergePar.sort xs)
quick xs = bench "Quick sort" (whnf Quick.sort xs)
quickPar xs = bench "Parallel quick sort" (whnf QuickPar.sort xs)
tim xs = bench "Timsort" (whnf Tim.sort xs)

benchForSize :: GenIO -> Int -> IO Benchmark
benchForSize g n = do
  xs0 <- uniformVector g n
  let !xs = convert xs0 :: Vector Int
  let slowTests = if n > 1000 then [] else [binaryInsertion, insertion]
  let fastTests = if n <= 20 then [] else [merge, mergePar, tim, quick]
  return $ bgroup (show n) (map ($ xs) (slowTests ++ fastTests))

main = withSystemRandom $ \ g ->
  defaultMain =<< mapM (benchForSize g) [10, 100, 1000, 100000, 1000000]