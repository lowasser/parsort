module Data.Vector.Sort.Types where

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as P

type LEq a = a -> a -> Bool
type VVector = V.Vector
type VMVector = V.MVector
type PVector = P.Vector
type PMVector = P.MVector