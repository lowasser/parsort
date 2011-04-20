module Benchmark where

import Criterion
import Progression.Main
import System.Random.MWC
import System.Environment
import System.Console.GetOpt
import Debug.Trace

import Data.List(span, intercalate)
import Data.Vector (Vector, convert, replicateM)

import Data.Vector.Sort.Constants
import qualified Data.Vector.Sort.Insertion as Ins
import qualified Data.Vector.Sort.Insertion.Binary as BI
import qualified Data.Vector.Sort.Merge as Merge
import qualified Data.Vector.Sort.Parallel.Merge as MergePar
import qualified Data.Vector.Sort.Quick as Quick
import qualified Data.Vector.Sort.Parallel.Quick as QuickPar
import qualified Data.Vector.Sort.Tim as Tim
import qualified Data.Vector.Sort.Heap.Binary as BinHeap

sortBench alg = whnf (alg (<=))

insertion, binaryInsertion, merge, mergePar, quick,  quickPar :: Vector Int -> Benchmark
insertion xs = bench "Insertion sort" (sortBench Ins.sortBy xs)
binaryInsertion xs = bench "Binary insertion sort" (sortBench BI.sortBy xs)
merge xs = bench "Merge sort" (sortBench Merge.sortBy xs)
mergePar xs = bench "Parallel merge sort" (sortBench MergePar.sortBy xs)
quick xs = bench "Quick sort" (sortBench Quick.sortBy xs)
quickPar xs = bench "Parallel quick sort" (sortBench QuickPar.sortBy xs)
tim xs = bench "Timsort" (sortBench Tim.sortBy xs)
binaryHeap xs = bench "Binary heap sort" (sortBench BinHeap.sortBy xs)

benchForSize :: GenIO -> Int -> IO Benchmark
benchForSize g n = do
  xs0 <- uniformVector g n
  let !xs = convert xs0 :: Vector Int
  let tests1 = if n <= sMALL_SORT_THRESHOLD then [] else [merge, tim, quick, binaryHeap]
  let tests2 = if n > 1000 then [] else [binaryInsertion, insertion]
  let tests3 = if n <= sEQUENTIAL_SORT_THRESHOLD then [] else [mergePar, quickPar]
  return $ bgroup (show n) (map ($ xs) (tests1 ++ tests2 ++ tests3))

main = withSystemRandom $ \ g -> do
  args <- getArgs
  let (ourArgs, otherArgs) = span (/= "--") args
  let sizes = process ourArgs
  let progressionArgs = (["--prefixes=" ++ intercalate "," (map show sizes)] ++ tail otherArgs)
  traceShow progressionArgs $ withArgs progressionArgs (defaultMain . bgroup "" =<< mapM (benchForSize g) sizes)

process :: [String] -> [Int]
process args = case getOpt Permute options args of
  (sizes, _, []) -> concat sizes
  (_, _, errs) -> fail (concat errs ++ usageInfo "Benchmark" options)

options :: [OptDescr [Int]]
options = [Option "n" ["n"] (ReqArg (\ str -> read ("[" ++ str ++ "]")) "benchmark size list") "benchmark sizes"]