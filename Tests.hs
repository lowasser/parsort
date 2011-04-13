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

implementations :: [(String, Vector Int -> Vector Int)]
implementations = [
  ("insertion", I.sort),
  ("insertion-binary", IB.sort),
  ("quick", Q.sort),
  ("p-quick", PQ.sort),
  ("merge", M.sort),
  ("p-merge", PM.sort)]

main = quickCheck (\ xs -> conjoin
    [printTestCase sortImpl $ L.sort xs == toList (theSort (fromList xs))
	| (sortImpl, theSort) <- implementations])