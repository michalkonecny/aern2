module AERN2.BoxFunMinMax.Type where

import AERN2.MP.Ball
import MixedTypesNumPrelude
import AERN2.BoxFun.Type
import AERN2.BoxFun.Box
import AERN2.BoxFun.Optimisation (SearchBox)
import AERN2.BoxFun.TestFunctions
import AERN2.BoxFunMinMax.Optimisation

import Data.Maybe

import Debug.Trace

import Control.Concurrent
import Control.Concurrent.STM
import System.IO.Unsafe

import Control.Parallel
import Control.Parallel.Strategies

parallelOr :: (Maybe Bool, Maybe SearchBox) -> (Maybe Bool, Maybe SearchBox) -> (Maybe Bool, Maybe SearchBox)
parallelOr b1 b2 =
    result
  where
    tuple = (b1, b2) `using` (parTuple2 (dot rpar rseq) (dot rpar rseq))
    -- b1' = b1 `using` r0
    -- b2' = b2 `using` r0
    result = case (tuple) of
      (r@(Just True, _), _) -> r
      (_, r@(Just True, _)) -> r
      (r@(Nothing, _), _)   -> r
      (_, r@(Nothing, _))   -> r
      (r, _)                -> r
      -- ((o1, b1), (o2, b2)) -> (o1, b1)

parallelAnd :: (Maybe Bool, Maybe SearchBox) -> (Maybe Bool, Maybe SearchBox) -> (Maybe Bool, Maybe SearchBox)
parallelAnd b1 b2 =
  result
  where
    tuple = (b1, b2) `using` (parTuple2 (dot rpar rseq) (dot rpar rseq))
    -- b1' = b1 `using` r0
    -- b2' = b2 `using` r0
    result = case (tuple) of
      (r@(Just True, _), (Just True, _)) -> r
      (r@(Nothing, _), _)                -> r
      (_, r@(Nothing, _))                -> r
      (r@(Just False, _), _)             -> r
      (_, r@(Just False, _))             -> r
      -- ((o1, b1), (o2, b2)) -> (o1, b1)

por :: (Maybe Bool, Maybe SearchBox) -> (Maybe Bool, Maybe SearchBox) -> (Maybe Bool, Maybe SearchBox)
por b1 b2 =
  unsafePerformIO $ do
    print "creating threads"
    result1TV <- atomically $ newTVar Nothing
    result2TV <- atomically $ newTVar Nothing
    thread1 <- forkIO (atomically (seq b1 $ writeTVar result1TV (Just b1)))
    thread2 <- forkIO (atomically (seq b2 $ writeTVar result2TV (Just b2)))
    result <- atomically $ do
      maybeResult1 <- readTVar result1TV
      maybeResult2 <- readTVar result2TV
      case (maybeResult1, maybeResult2) of
        (Just r@(Just True, _), _)                     -> pure r
        (_, Just r@(Just True, _))                     -> pure r
        (Just r@(Nothing, _), Just _)                  -> pure r
        (Just _, Just r@(Nothing, _))                  -> pure r
        (Just r@(Just False, _), Just (Just False, _)) -> pure r
        _ -> retry
    print "killing thread 1 and thread 2"
    killThread thread1
    killThread thread2
    pure result
pand :: (Maybe Bool, Maybe SearchBox) -> (Maybe Bool, Maybe SearchBox) -> (Maybe Bool, Maybe SearchBox)
pand b1 b2 =
  unsafePerformIO $ do
    result1TV <- atomically $ newTVar Nothing
    result2TV <- atomically $ newTVar Nothing
    thread1 <- forkIO (atomically (seq b1 $ writeTVar result1TV (Just b1)))
    thread2 <- forkIO (atomically (seq b2 $ writeTVar result2TV (Just b2)))
    result <- atomically $ do
      maybeResult1 <- readTVar result1TV
      maybeResult2 <- readTVar result2TV
      case (maybeResult1, maybeResult2) of
        (Just r@(Just True, _), Just (Just True, _))   -> pure r
        (Just r@(Nothing, _), _)                       -> pure r
        (_, Just r@(Nothing, _))                       -> pure r
        (Just r@(Just False, _), _)                    -> pure r
        (_, Just r@(Just False, _))                    -> pure r
        _ -> retry
    killThread thread1
    killThread thread2
    pure result

-- pand1 = pand True True
-- pand2 = pand True False
-- pand3 = pand False True
-- pand4 = pand False False
-- pand5 = pand loop False
-- pand6 = pand False loop

loop = loop

por1 = por (Just True, Nothing) (Just False, Nothing)
por2 = por (Just False, Nothing) (Just True, Nothing)
por3 = por (Just True, Nothing) (Just True, Nothing)
por4 = por (Just True, Nothing) (Nothing, Nothing)
por5 = por (Nothing, Nothing) (Just True, Nothing)
por6 = por (Nothing, Nothing) (Nothing, Nothing)
por7 = por (Just False, Nothing) (Just False, Nothing)
por8 = por (Just True, Nothing) loop
por9 = por loop (Just True, Nothing)


pand1 = pand (Just True, Nothing) (Just False, Nothing)
pand2 = pand (Just False, Nothing) (Just True, Nothing)
pand3 = pand (Just True, Nothing) (Just True, Nothing)
pand4 = pand (Nothing, Nothing) (Just True, Nothing)
pand5 = pand (Just True, Nothing) (Nothing, Nothing)
pand6 = pand (Nothing, Nothing) (Nothing, Nothing)
pand7 = pand (Just False, Nothing) (Just False, Nothing)

-- All leaves in the tree must have the same domain
-- leaves should be bf_eval
data MinMaxTree = Leaf {tree_f :: BoxFun} | Min {tree_l :: MinMaxTree, tree_r :: MinMaxTree} | Max {tree_l :: MinMaxTree, tree_r :: MinMaxTree}
-- TODO: Refactor tree to take Min/Max of a list of trees

heronTree2 :: MinMaxTree
heronTree2 = Max 
            (Min heron1 heron2)
            (Min heron3 heron4)
            
heron1 :: MinMaxTree
heron1 = Max (Leaf (heron1p)) (Leaf (heron1q))

heron2 :: MinMaxTree
heron2 = Max (Leaf (heron2p)) (Leaf (heron2q))

heron3 :: MinMaxTree
heron3 = Max (Leaf (heron3p)) (Leaf (heron3q))

heron4 :: MinMaxTree
heron4 = Max (Leaf (heron4p)) (Leaf (heron4q))

checkTree :: MinMaxTree -> Box -> Accuracy -> Precision -> Rational -> (Maybe Bool, Maybe SearchBox)
checkTree (Leaf f) box ac p n = 
  trace ("Checking on domain: " ++ show (getEndpoints box)) $
  trace ("rough bound gives: " ++ show ((applyMinimumOnBox f box'))) $
  trace (show (unsafePerformIO getNumCapabilities)) $
  if applyMinimumOnBox f box' !>! n then
    (Just True, Nothing)
  else
    globalMinimumGreaterThanN f' ac p ((width box') / 10000000) (cn n)
  where
    f' = BoxFun (dimension f) (bf_eval f) box'
    box' = setPrecision p box
checkTree (Max l r) box ac p n
  | rRange !<! n =
    trace ("Right branch below " ++ show n)
    trace ("Left  range: " ++ show (endpointsAsIntervals lRange))
    trace ("Right range: " ++ show (endpointsAsIntervals rRange))
    checkTree l box ac p n

  | lRange !<! n =
    trace ("Left branch below " ++ show n)
    trace ("Left  range: " ++ show (endpointsAsIntervals lRange))
    trace ("Right range: " ++ show (endpointsAsIntervals rRange))
    checkTree r box ac p n
  | otherwise      =
    if (lRange !>! n || rRange !>! n) then
      (Just True, Nothing)
    else 
      case parallelOr lOptAboveN rOptAboveN of
        (Just True, _) -> (Just True, Nothing)
        _ -> 
          if AERN2.BoxFun.Box.width box' !>! 1 / (10000000) then -- make this threshold quite small (maybe 10^-7)
              trace ("Bisected boxes: " ++ show newBoxes)
              checkBoxes newBoxes
            else
              trace "Stopping bisections (Box too small)" $
              case checkTree l box' ac p n of
                (Just True, _)  -> (Just True, Nothing)
                _               -> checkTree r box' ac p n -- TODO: Refactor to return nothing instead of False
    where
      lRange = applyTree l box'
      rRange = applyTree r box'
      
      lOptAboveN = minimumAboveNTree l box' ac p n
      rOptAboveN = minimumAboveNTree r box' ac p n
 
      box' = setPrecision p box

      newBoxes = fullBisect box'

      checkBoxes []           = (Just True, Nothing)
      checkBoxes (b : boxes)  = case checkTree (Max l r) b ac p n of
                                  (Just True, _) -> checkBoxes boxes
                                  o              -> o

checkTree (Min l r) box ac p n = 
  parallelAnd (checkTree l box ac p n) (checkTree r box ac p n)
  -- case checkTree l box ac p n of
  --   (Just True, _) -> 
  --     case checkTree r box ac p n of
  --       (Just True, _) -> (Just True, Nothing)
  --       o -> o
  --   o -> o

size :: MinMaxTree -> Integer
size (Leaf _)  = 1
size (Min l r) = 1 + size l + size r
size (Max l r) = 1 + size l + size r

applyTree :: MinMaxTree -> Box -> CN MPBall

-- applyTree (Leaf f)  box = globalMinimumAboveN (BoxFun (dimension (fst f)) (bf_eval (fst f)) box) (bits 100) (prec 1000) (cn 0.0)
applyTree (Leaf f)  box = apply f box
applyTree (Max l r) box = max (applyTree l box) (applyTree r box)
applyTree (Min l r) box = min (applyTree l box) (applyTree r box)

-- TODO: globalMinimum and globalMaximum with low accuracy/precision to compute more accurate r and lRanges

domainTree :: MinMaxTree -> Box
domainTree (Leaf f) = domain f 
domainTree (Min l _) = domainTree l 
domainTree (Max l _) = domainTree l

minimumAboveNTree :: MinMaxTree -> Box -> Accuracy -> Precision -> Rational -> (Maybe Bool, Maybe SearchBox) -- make the cutoff a quarter of a width
minimumAboveNTree (Leaf f) box ac p n = globalMinimumGreaterThanN (BoxFun (dimension f) (bf_eval f) box) ac p ((width box) / 2) (cn n)
minimumAboveNTree (Max l r) box ac p n = 
  parallelOr (minimumAboveNTree l box ac p n) (minimumAboveNTree r box ac p n)
  -- case minimumAboveNTree l box ac p n of
  --   (Just True, _) -> (Just True, Nothing)
  --   _ ->
  --      case minimumAboveNTree r box ac p n of
  --        (Just True, _) -> (Just True, Nothing)
  --        (_, b) -> (Nothing, b)
minimumAboveNTree (Min l r) box ac p n = 
  parallelAnd (minimumAboveNTree l box ac p n) (minimumAboveNTree r box ac p n)
  -- case minimumAboveNTree l box ac p n of
  --   (Just True, _) ->
  --     case minimumAboveNTree r box ac p n of
  --       (Just True, _) -> (Just True, Nothing)
  --       o -> o
  --   o -> o
