{-# LANGUAGE ImplicitParams, MagicHash, Rank2Types, BangPatterns #-}
module Data.Vector.Sort.Comparator (
  Comparator,
  toComparator,
  mkComparator,
  runComparator,
  (<=?), (<?), (>=?), (>?),
  sortPerm, sortPermM, sortPermIO)
  where

import Control.Monad.ST

import Data.Vector.Primitive (create, enumFromN)
import Data.Vector.Generic (convert, unsafeFreeze)
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Fusion.Stream as Stream
import Data.Vector.Sort.Types

import Prelude hiding (length)
import GHC.Exts

newtype Comparator = Cmp {runCmp :: Int# -> Int# -> Bool}

{-# INLINE toComparator #-}
toComparator :: Vector v a => LEq a -> v a -> Comparator
toComparator (<=?) !xs = mkComparator (\ i j -> index xs i <=? index xs j)

mkComparator :: LEq Elem -> Comparator
mkComparator (<=?) = Cmp $ \ i# j# -> I# i# <=? I# j#

runComparator :: Comparator -> LEq Elem
runComparator cmp (I# i#) (I# j#) = runCmp cmp i# j#

(<=?), (<?), (>=?), (>?) :: (?cmp :: Comparator) => Elem -> Elem -> Bool
(<=?) = runComparator ?cmp
a <? b = not (a >=? b)
a >=? b = b <=? a
a >? b = b <? a

{-# INLINE [0] sortPerm #-}
sortPerm :: Vector v a => ((?cmp :: Comparator) => PVector Elem -> PVector Elem) -> LEq a -> v a -> v a
sortPerm sortAlg (<=?) xs =
  let !n = length xs; perm = sortAlg (enumFromN 0 n) in
	backpermute xs perm
    where ?cmp = toComparator (<=?) xs

{-# INLINE [0] sortPermM #-}
sortPermM :: Vector v a => (forall s . (?cmp :: Comparator) => PMVector s Elem -> ST s ())
  -> LEq a -> v a -> v a
sortPermM sortAlg (<=?) xs = 
  let perm = create $ do
	pv <- M.unstream $ Stream.enumFromStepN 0 1 (length xs)
	sortAlg pv
	return pv
  in backpermute xs perm
  where ?cmp = toComparator (<=?) xs

{-# INLINE [0] sortPermIO #-}
sortPermIO :: Vector v a => ((?cmp :: Comparator) => PMVector RealWorld Elem -> IO ())
  -> LEq a -> v a -> IO (v a)
sortPermIO sortAlg (<=?) xs = do
	pv <- M.unstream $ Stream.enumFromStepN 0 1 (length xs)
	sortAlg pv
	perm <- unsafeFreeze pv
	return (backpermute xs (perm :: PVector Int))
  where ?cmp = toComparator (<=?) xs

{-# RULES
      "sortPerm/Int" sortPerm = \ sortAlg (<=?) xs ->
	  let ?cmp = mkComparator (<=?) in sortAlg (convert xs);
      "sortPermM/Int" sortPermM = \ sortAlg (<=?) xs ->
	  let ?cmp = mkComparator (<=?) in modify sortAlg (convert xs :: PVector Int);
      #-}