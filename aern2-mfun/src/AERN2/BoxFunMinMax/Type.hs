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

parallelOrList :: [(Maybe Bool, Maybe SearchBox)] -> (Maybe Bool, Maybe SearchBox)
parallelOrList l =
    checkList list (Nothing, Nothing)
  where
    list = l `using` parList (dot rpar rseq)

    checkList []       c    = c
    checkList (x : xs) c    =
      case x of
        r@(Just True, _)  -> r
        -- Prefer False counterexample over Nothing counterexamples
        o@(Just False, _) -> checkList xs o 
        o@(Nothing, _)    -> 
          case c of
            (Just False, _) -> checkList xs c 
            _               -> checkList xs o

parallelAndList :: [(Maybe Bool, Maybe SearchBox)] -> (Maybe Bool, Maybe SearchBox)
parallelAndList l =
    checkList list
  where
    list = l `using` parList (dot rpar rseq)

    checkList []            = (Just True, Nothing)
    checkList (x : xs)      =
      case x of
        (Just True, _)  -> checkList xs
        o               -> o

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
data MinMaxTree = Leaf {tree_f :: BoxFun} | Min [MinMaxTree] | Max [MinMaxTree]
-- TODO: Refactor tree to take Min/Max of a list of trees

heronTree2 :: MinMaxTree
heronTree2 = Max 
            [
              Min [heron1, heron2],
              Min [heron3, heron4]
            ]
            
heron1 :: MinMaxTree
heron1 = Max [Leaf heron1p, Leaf heron1q]

heron2 :: MinMaxTree
heron2 = Max [Leaf heron2p, Leaf heron2q]

heron3 :: MinMaxTree
heron3 = Max [Leaf heron3p, Leaf heron3q]

heron4 :: MinMaxTree
heron4 = Max [Leaf heron4p, Leaf heron4q]

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
checkTree (Max []) _ _ _ _ = (Just True, Nothing)
checkTree (Max (t : ts)) box ac p n
  | tRange !<! n =
    trace ("branch below " ++ show n)
    trace ("branch range: " ++ show (endpointsAsIntervals tRange))
    checkTree (Max ts) box ac p n
  | otherwise      =
    if (tRange !>! n) then
      (Just True, Nothing)
    else 
      case minimumAboveNTree (Max (t : ts)) box' ac p n of
        (Just True, mBox) -> (Just True, mBox)
        _ -> 
          if AERN2.BoxFun.Box.width box' !>! 1 / (10000000) then -- make this threshold quite small (maybe 10^-7)
              trace ("Bisected boxes: " ++ show newBoxes)
              checkBoxes newBoxes
            else
              trace "Stopping bisections (Box too small)" $
              case minimumAboveNTree (Max (t : ts)) box' ac p n of
                (Just True, mBox) -> (Just True, mBox)
                (_, mBox)      -> (Nothing, mBox) -- Cannot guarantee that a result is False here
    where   
      tRange = applyTree t box'

      box' = setPrecision p box

      newBoxes = fullBisect box'

      checkBoxes []           = (Just True, Nothing)
      checkBoxes (b : boxes)  = case checkTree (Max (t : ts)) b ac p n of
                                  (Just True, _) -> checkBoxes boxes
                                  o              -> o

checkTree (Min l) box ac p n = minimumAboveNTree (Min l) box ac p n

size :: MinMaxTree -> Integer
size (Leaf _)  = 1
size (Min l) = 1 + sum (map size l)
size (Max l) = 1 + sum (map size l)

applyTree :: MinMaxTree -> Box -> CN MPBall

-- applyTree (Leaf f)  box = globalMinimumAboveN (BoxFun (dimension (fst f)) (bf_eval (fst f)) box) (bits 100) (prec 1000) (cn 0.0)
applyTree (Leaf f)  box = apply f box
applyTree (Max l) box   = maximum (map (\t -> applyTree t box) l)
applyTree (Min l) box   = minimum (map (\t -> applyTree t box) l)

-- TODO: globalMinimum and globalMaximum with low accuracy/precision to compute more accurate r and lRanges

domainTree :: MinMaxTree -> Box
domainTree (Leaf f) = domain f 
domainTree (Min l) = domainTree (head l) 
domainTree (Max l) = domainTree (head l)

minimumAboveNTree :: MinMaxTree -> Box -> Accuracy -> Precision -> Rational -> (Maybe Bool, Maybe SearchBox) -- make the cutoff a quarter of a width
minimumAboveNTree (Leaf f) box ac p n = globalMinimumGreaterThanN (BoxFun (dimension f) (bf_eval f) box) ac p ((width box) / 2) (cn n)
minimumAboveNTree (Max l)  box ac p n = parallelOrList (map (\t -> minimumAboveNTree t box ac p n) l)
minimumAboveNTree (Min l) box ac p n  = parallelAndList (map (\t -> minimumAboveNTree t box ac p n) l)


-- Below is simpler, won't have to drop branches in max cases
-- But dropping branches seems to be more efficient

-- minimumAboveNTree :: MinMaxTree -> Box -> Accuracy -> Precision -> Rational -> (Maybe Bool, Maybe SearchBox) -- make the cutoff a quarter of a width
-- minimumAboveNTree (Leaf f) box ac p n = 
--   if apply f box !>! n 
--     then (Just True, Nothing)
--     else globalMinimumGreaterThanN (BoxFun (dimension f) (bf_eval f) box) ac p ((width box) / 2) (cn n)
-- minimumAboveNTree (Max l)  box ac p n = 
--   parallelOrList  
--   (map 
--   (\t -> 
--     if applyTree (Max l) box !>! n 
--       then (Just True, Nothing)
--       else minimumAboveNTree t box ac p n)
--   l)
-- minimumAboveNTree (Min l) box ac p n  = 
--   parallelAndList 
--   (map 
--   (\t -> 
--     if applyTree (Min l) box !<=! n
--       then (Just False, Nothing)
--       else minimumAboveNTree t box ac p n) 
--   l)
