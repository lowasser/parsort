module Tests where

import Test.QuickCheck

import Data.Vector.Primitive

import qualified Data.List as L

import qualified Data.Vector.Sort.Quick as Q
import qualified Data.Vector.Sort.Parallel.Quick as PQ
import qualified Data.Vector.Sort.Merge as M
import qualified Data.Vector.Sort.Parallel.Merge as PM
import qualified Data.Vector.Sort.Insertion as I
import qualified Data.Vector.Sort.Insertion.Binary as IB
import qualified Data.Vector.Sort.Tim as T
import qualified Data.Vector.Sort.Heap.Nary as H

implementations :: [(String, Vector Int -> Vector Int)]
implementations = [
  ("insertion", I.sortBy (<=)),
  ("insertion-binary", IB.sortBy (<=) ),
  ("quick", Q.sortBy (<=)),
  ("p-quick", PQ.sortBy (<=)),
  ("merge", M.sortBy (<=)),
  ("p-merge", PM.sortBy (<=)),
  ("tim", T.sortBy (<=)),
  ("heap", H.sortBy (<=))]

main = quickCheck (\ xs -> conjoin
    [printTestCase sortImpl $ L.sort xs == toList (theSort (fromList xs))
	| (sortImpl, theSort) <- implementations])