module AERN2.Continuity.Principles
  ( maxSeqIndexUsed,
    maxIntParamUsed
  )
where

import AERN2.Kleenean (Kleenean (..))
import AERN2.Real (pi)
import AERN2.Real.CKleenean (CKleenean)
import AERN2.Real.Comparisons ()
import AERN2.Real.Type (CSequence (..), creal)
import GHC.Conc (atomically, newTVar, readTVar, writeTVar, TVar)
import GHC.IO (unsafePerformIO)
import MixedTypesNumPrelude

-- |
--    Apply a predicate to a `CSequence`, assuming it returns True
--    and return the largest index in the sequence that was accessed during the computation.
maxSeqIndexUsed :: ((CSequence t) -> CKleenean) -> (CSequence t) -> Integer
maxSeqIndexUsed = maxEffortUsedForPredTrue addCSequenceAccessMonitor

-- |
--    Apply a predicate to a convergent sequence, assuming it returns True
--    and return the largest index in the sequence that was accessed during the computation.
maxIntParamUsed :: ((Integer -> t) -> CKleenean) -> (Integer -> t) -> Integer
maxIntParamUsed = maxEffortUsedForPredTrue addCallMonitor

maxEffortUsedForPredTrue :: (TVar Integer -> seq -> seq) -> (seq -> CKleenean) -> seq -> Integer
maxEffortUsedForPredTrue addMonitor f x =
  unsafePerformIO $ do
    maxSoFarTvar <- atomically $ newTVar (-1)
    let res = f (addMonitor maxSoFarTvar x)
    _ <- waitForTrue (unCSequence res)
    atomically $ readTVar maxSoFarTvar

addCallMonitor :: TVar Integer -> (Integer -> a) -> Integer -> a
addCallMonitor maxSoFarTvar x i =
    unsafePerformIO $ do
    atomically $ do 
        prevMax <- readTVar maxSoFarTvar
        writeTVar maxSoFarTvar (prevMax `max` i)
    pure (x i)

addCSequenceAccessMonitor :: TVar Integer -> CSequence t -> CSequence t
addCSequenceAccessMonitor maxSoFarTvar x = CSequence (monitorSeq 0 (unCSequence x))
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
test0 = maxSeqIndexUsed (\x -> x > 0) (pi - pi + (creal (1 / 1000000000))) -- 7
