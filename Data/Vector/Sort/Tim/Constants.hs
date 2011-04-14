{-# LANGUAGE BangPatterns #-}
module Data.Vector.Sort.Tim.Constants where

import Control.Exception.Base

import Data.Bits

mIN_GALLOP :: Int
mIN_GALLOP = 2

mIN_MERGE :: Int
mIN_MERGE = 3

minRunLength :: Int -> Int
minRunLength n = assert (n >= 0) $ go n 0 where
  go n !r
    | n >= mIN_MERGE	= go (n `shiftR` 1) (r .|. (n .&. 1))
    | otherwise		= n + r