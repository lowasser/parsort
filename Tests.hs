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
import qualified Data.Vector.Sort.Heap.Binary as H
import qualified Data.Vector.Sort.Heap.Pairing as H2
import qualified Data.Vector.Sort.Radix as R
import qualified Data.Vector.Sort.Intro as I

implementations :: [(String, Vector Int -> Vector Int)]
implementations = [
  ("insertion", I.sort),
  ("insertion-binary", IB.sort),
  ("quick", Q.sort),
  ("p-quick", PQ.sort),
  ("merge", M.sort),
  ("p-merge", PM.sort),
  ("tim", T.sort),
  ("radix", R.sort),
  ("heap", H.sort),
  ("intro", I.sort)]

main = quickCheck (\ xs -> conjoin
    [printTestCase sortImpl $ L.sort xs == toList (theSort (fromList xs))
	| (sortImpl, theSort) <- implementations])