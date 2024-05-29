module AERN2.Continuity.Principles
  ( maxSeqIndexUsed,
    maxIntParamUsed,
  )
where

import AERN2.Kleenean (Kleenean (..))
import AERN2.Real (pi)
import AERN2.Real.CKleenean (CKleenean)
import AERN2.Real.Comparisons ()
import AERN2.Real.Type (CSequence (..), creal)
import GHC.Conc (TVar, atomically, newTVar, readTVar, writeTVar)
import GHC.IO (unsafePerformIO)
import MixedTypesNumPrelude

-- |
--    Apply a predicate to a `CSequence`, assuming it returns True
--    and return the largest index in the sequence that was accessed during the computation.
--    Index 0 means that the sequence was not accessed at all, 
--    1 means the first element was accessed, etc.
maxSeqIndexUsed :: ((CSequence t) -> CKleenean) -> (CSequence t) -> Integer
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
    maxSoFarTvar <- atomically $ newTVar 0
    let res = f (addMonitor maxSoFarTvar x)
    _ <- waitForTrue (unCSequence res)
    atomically $ readTVar maxSoFarTvar

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
    monitorSeq i (h : t) = monitorH : (monitorSeq (i + 1) t)
      where
        monitorH = unsafePerformIO $ do
          atomically $ writeTVar maxSoFarTvar i
          pure h

waitForTrue :: [CN Kleenean] -> IO (CN Kleenean)
waitForTrue [] = pure (cn TrueOrFalse)
waitForTrue (h : t)
  | isCertainlyTrue h =
      pure h
  | otherwise =
      waitForTrue t

test0 :: Integer
test0 = maxSeqIndexUsed (\x -> x > 0) (pi - pi + (creal (1 / 1000000000))) -- 8
