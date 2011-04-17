{-# LANGUAGE MagicHash, UnboxedTuples, RecordWildCards, NamedFieldPuns, BangPatterns, CPP, MultiParamTypeClasses #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.Vector.Sort.Tim.Monad (
  Status(..),
  MergeStatus(..),
  liftST,
  MergeM,
  execMergeM,
  incrStatus, decrStatus, resetStatus,
  update1, update2, updateDest, updateGallop) where

import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad.State.Class
import GHC.Exts

data Status = Status{cursor, count, len :: !Int} deriving (Show)
data MergeStatus = MergeStatus {
      status1, status2 :: !Status, dest, gallop :: !Int} deriving (Show)
data MergeStatusP s a = MSP (State# s) !MergeStatus a

newtype MergeM s a =
  MergeM {runMergeM :: State# s -> MergeStatus -> MergeStatusP s a}

execMergeM :: MergeStatus -> MergeM s a -> ST s Int
execMergeM s0 m = primitive $ \ s# -> case runMergeM m s# s0 of
  MSP s'# MergeStatus{gallop} a -> (# s'#, gallop #)

liftST :: ST s a -> MergeM s a
liftST m = MergeM $ \ s# !stat -> case internal m s# of
    (# s'#, a #) -> MSP s'# stat a

instance Monad (MergeM s) where
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}
  return a = MergeM $ \ s# !stat -> MSP s# stat a
  m >>= k = MergeM $ \ s# !stat -> case runMergeM m s# stat of
    MSP s'# stat' a -> runMergeM (k a) s'# stat'

instance MonadState MergeStatus (MergeM s) where
  get = MergeM $ \ s# stat -> MSP s# stat stat
  put !stat = MergeM $ \ s# _ -> MSP s# stat ()

incrStatus, decrStatus :: Int -> Status -> Status
incrStatus !k Status{..} = Status{cursor = cursor + k, count = count + k, len = len - k}
decrStatus !k Status{..} = Status{cursor = cursor - k, count = count + k, len = len - k}

resetStatus :: Status -> Status
resetStatus stat = stat{count = 0}

#define UPDATE(stat,f) (\ record -> record{stat = (f) (stat record)})

update1, update2 :: (Status -> Status) -> MergeM s ()
update1 f = modify (UPDATE(status1,f))
update2 f = modify (UPDATE(status2,f))

updateDest, updateGallop :: (Int -> Int) -> MergeM s Int
updateDest f = do
  d0 <- gets dest
  modify (\ s -> s{dest = f d0})
  return d0
updateGallop f = do
  g0 <- gets gallop
  modify (\ s -> s{gallop = f g0})
  return g0