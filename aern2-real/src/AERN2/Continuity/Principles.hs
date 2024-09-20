module AERN2.Continuity.Principles
  ( maxSeqIndexUsed,
    maxIntParamUsed,
  )
where

import AERN2.Kleenean (Kleenean (..), kleenean)
import AERN2.Real (pi)
import AERN2.Real.CKleenean (CKleenean)
import AERN2.Real.Comparisons ()
import AERN2.Real.Type (CSequence (..), creal)
import GHC.Conc (TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import GHC.IO (unsafePerformIO)
import MixedTypesNumPrelude

-- |
--    Apply a predicate to a `CSequence`, assuming it returns True
--    and return the largest index in the sequence that was accessed during the computation.
--    Index 0 means that the sequence was not accessed at all,
--    1 means the first element was accessed, etc.
maxSeqIndexUsed :: (CSequence t -> CKleenean) -> CSequence t -> Integer
maxSeqIndexUsed = maxEffortUsed addCSequenceAccessMonitor

-- |
--    Apply a predicate to a convergent sequence, assuming it returns True
--    and return the largest index in the sequence that was accessed during the computation.
--    Index 0 means that the sequence was not accessed at all,
--    1 means the sequence was called only with index 0, etc.
maxIntParamUsed :: ((Integer -> t) -> CKleenean) -> (Integer -> t) -> Integer
maxIntParamUsed = maxEffortUsed addCallMonitor

maxEffortUsed :: (TVar Integer -> seq -> seq) -> (seq -> CKleenean) -> seq -> Integer
maxEffortUsed addMonitor f x =
  unsafePerformIO $ do
    maxSoFarTvar <- newTVarIO 0
    let res = f (addMonitor maxSoFarTvar x)
    let true = waitForTrue (unCSequence res)
    if isCertainlyTrue true
      then readTVarIO maxSoFarTvar
      else error "internal error in maxEffortUsed" -- this should never happen

addCallMonitor :: TVar Integer -> (Integer -> a) -> Integer -> a
addCallMonitor maxSoFarTvar x i =
  unsafePerformIO $ do
    atomically $ do
      prevMax <- readTVar maxSoFarTvar
      writeTVar maxSoFarTvar (prevMax `max` (i + 1)) -- calling x with 0 sets max to 1, etc.
    pure (x i)

addCSequenceAccessMonitor :: TVar Integer -> CSequence t -> CSequence t
addCSequenceAccessMonitor maxSoFarTvar x = CSequence (monitorSeq 1 (unCSequence x))
  where
    monitorSeq _ [] = []
    monitorSeq i (h : t) = monitorH : monitorSeq (i + 1) t
      where
        monitorH = unsafePerformIO $ do
          atomically $ writeTVar maxSoFarTvar i
          pure h

waitForTrue :: [CN Kleenean] -> CN Kleenean
waitForTrue [] = cn TrueOrFalse
waitForTrue (h : t)
  | isCertainlyTrue h = h
  | otherwise = waitForTrue t

_testNever :: Integer
_testNever = maxSeqIndexUsed (> 0) (pi - pi) -- loop forever

_test0 :: Integer
_test0 = maxSeqIndexUsed (> 0) (pi - pi + creal (1 / 1000000000)) -- 8

_test1 :: Integer
_test1 = maxIntParamUsed (\x -> CSequence (map (\n -> cn (kleenean (n > 3 * x n))) [0 ..])) (const 3) -- 11
