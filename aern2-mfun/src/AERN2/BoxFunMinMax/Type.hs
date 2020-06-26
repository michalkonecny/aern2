module AERN2.BoxFunMinMax.Type where

import AERN2.MP.Ball
import MixedTypesNumPrelude
import AERN2.BoxFun.Type
import AERN2.BoxFun.Optimisation (SearchBox)
import AERN2.BoxFunMinMax.Optimisation
import AERN2.BoxFunMinMax.VarMap
import AERN2.BoxFunMinMax.Expressions.Translators.BoxFun
import qualified AERN2.BoxFunMinMax.Expressions.Type as E


import Debug.Trace (trace)

import Control.Monad
import Control.Concurrent
import System.IO.Unsafe

import Control.Parallel.Strategies

import Data.List (take, drop, null, filter, delete)

import qualified Prelude as P

-- import System.Environment

-- import Data.List (find)


-- evalToTVar x tvar = atomically (writeTVar tvar x)

-- readSTM tvar =
--   case tvar of
--     Just r -> pure r
--     _      -> retry

-- parTree t tvar = forkIO (atomically (seq t (writeTVar tvar (Just t))))

-- orP ts tvar = 
--   do
--     result <- readTVar tvar

--     case result of
--       Just r -> pure r
--       _ -> retry
    
-- globalTVar = newTVar Nothing

-- runThreads ts =
--   parTree (ts !! 0) globalTVar

-- precondition: (length xs) <= n

checkMaxPar :: [(Maybe Bool, Maybe SearchBox)] -> (Maybe Bool, Maybe SearchBox)
checkMaxPar l =
  checkBunch (bunchList l)
  where
    checkBunch [] = (Nothing, Nothing)
    checkBunch (x : []) =
      case (unsafePerformIO (findParOrList n x)) of
        Just r@_ -> r 
        _ -> (Nothing, Nothing)
    checkBunch (x : xs) = 
      case (unsafePerformIO (findParOrList n x)) of
        Just r@(Just True, _) -> r 
        _ -> checkBunch xs

    n = unsafePerformIO getNumCapabilities
    bunchList xs =
      if null xs then [] else [Data.List.take n xs] ++ bunchList (Data.List.drop n xs)

findParOrList :: Int -> [(Maybe Bool, Maybe SearchBox)] -> IO (Maybe (Maybe Bool, Maybe SearchBox))
findParOrList n xs = do
  resultV <- newEmptyMVar 
  runningV <- newMVar n
  threads <- forM [0 .. n - 1] (\j -> forkIO (
    case (xs !! j) of
      r@(Just True, _) -> 
        void (tryPutMVar resultV (Just r))
      r -> do 
        m <- takeMVar runningV
        if (m == 1)
          then putMVar runningV (int (m-(int 1)))
          else void (tryPutMVar resultV (Just r))))
  result <- readMVar resultV
  mapM_ killThread threads
  return result

parOrList :: [(Maybe Bool, Maybe SearchBox)] -> IO (Maybe Bool, Maybe SearchBox)
parOrList l = do
  resultVs  <- mapM (\_ -> 
    trace ("creating" ++ (show n) ++ "MVars")
    newEmptyMVar) [0 .. n-1]
  threads <- mapM (\j -> 
    trace ("creating" ++ (show n) ++ "MVars")
    forkIO (seq (l!!j) (void (tryPutMVar (resultVs !! j) (Just (l!!j)))))) [0 .. n-1]

  mapM_ killThread threads
  checkResultVs resultVs
  where
    n = unsafePerformIO getNumCapabilities 

    checkResultVs :: [MVar (Maybe (Maybe Bool, Maybe SearchBox))] -> IO (Maybe Bool, Maybe SearchBox)
    checkResultVs []        = return (Just False, Nothing)
    checkResultVs (x : xs)  =
      case unsafePerformIO (tryReadMVar x) of
        Just x' ->
          case x' of
            Just r@(Just True, _) -> return r
            Just _                -> checkResultVs xs -- False or nothing, remove from list
            Nothing               -> checkResultVs (xs ++ [x])
        Nothing ->
          checkResultVs (xs ++ [x])

parAndList :: [(Maybe Bool, Maybe SearchBox)] -> IO (Maybe Bool, Maybe SearchBox)
parAndList l = do
    resultVs  <- mapM (\_ -> 
      trace ("creating" ++ (show n) ++ "MVars")
      newEmptyMVar) [0 .. n-1]
    threads <- mapM (\j -> 
      trace ("creating" ++ (show n) ++ "threads")
      forkIO (seq (l!!j) (void (tryPutMVar (resultVs !! j) (Just (l!!j)))))) [0 .. n-1]

    mapM_ killThread threads
    checkResultVs resultVs
  where
    n = length l

    checkResultVs :: [MVar (Maybe (Maybe Bool, Maybe SearchBox))] -> IO (Maybe Bool, Maybe SearchBox)
    checkResultVs []        = return (Just False, Nothing)
    checkResultVs (x : xs)  =
      case unsafePerformIO (tryReadMVar x) of
        Just x' ->
          case x' of
            Just (Just True, _)   -> checkResultVs xs
            Just r@_              -> return r
            Nothing               -> checkResultVs (xs ++ [x])
        Nothing ->
          checkResultVs (xs ++ [x])

-- parallelOrList2 :: [(Maybe Bool, Maybe SearchBox)] -> Bool
-- parallelOrList2 l =
--     list2
--   where
--     list = l `using` parList rpar

--     list2 = or $ parMap rpar checkList2 l

--     checkList2 (x)     =
--       case x of
--         r@(Just True, _)  -> True
--         -- Prefer False counterexample over Nothing counterexamples
--         _ -> False

--     checkList c []           = c
--     checkList c (x : xs)     =
--       case x of
--         r@(Just True, _)  -> r
--         -- Prefer False counterexample over Nothing counterexamples
--         o@(Just False, _) -> checkList o xs 
--         o@(Nothing, _)    -> 
--           case c of
--             (Just False, _) -> checkList c xs  
--             _               -> checkList o xs 


-- pOr :: [Bool] -> Bool
-- pOr l = or (using l (parList rpar))


parallelOrList :: [(Maybe Bool, Maybe SearchBox)] -> (Maybe Bool, Maybe SearchBox)
parallelOrList l =
    checkList (Nothing, Nothing) list
  where
    list = l `using` parBuffer (int 6) rseq

    checkList c []           = c
    checkList c (x : xs)     =
      case x of
        r@(Just True, _)  -> r
        -- Prefer False counterexample over Nothing counterexamples
        o@(Just False, _) -> checkList o xs 
        o@(Nothing, _)    -> 
          case c of
            (Just False, _) -> checkList c xs  
            _               -> checkList o xs 

parallelAndList :: [(Maybe Bool, Maybe SearchBox)] -> (Maybe Bool, Maybe SearchBox)
parallelAndList l =
    checkList list
  where
    list = l `using` parBuffer (int 6) rseq

    checkList []            = (Just True, Nothing)
    checkList (x : xs)      =
      case x of
        (Just True, _)  -> checkList xs
        o               -> o

-- All leaves in the tree must have the same domain
-- leaves should be bf_eval
data MinMaxTree = Leaf {expression :: E.E, varMap :: VarMap} | Min {list :: [MinMaxTree], varMap :: VarMap} | Max {list :: [MinMaxTree], varMap :: VarMap}
  deriving (Show, P.Eq)

checkTree :: MinMaxTree -> Accuracy -> Precision -> Rational -> (Maybe Bool, Maybe SearchBox)
checkTree (Leaf e vMap) ac p n = 
  trace ("Checking on domain: " ++ show (map snd vMap)) $
  trace ("rough bound gives: " ++ show ((applyMinimumOnBox f box'))) $
  if applyMinimumOnBox f box' !>! n then 
    (Just True, Nothing)
  else
    result
  where
    result = globalMinimumAboveN f' ac p (cn (mpBallP p (width vMap /! 10000000))) (cn (mpBallP p n))
    f = expressionToBoxFun e vMap
    f' = BoxFun (dimension f) (bf_eval f) box'
    box' = setPrecision p (domain f)
checkTree (Max ts vMap) ac p n =
    if (or roughCheck) then
      (Just True, Nothing)
    else 
      case filteredTs of
        [] -> (Just False, Nothing)
        [t] -> checkTree (updateVarMap t vMap) ac p n
        _ ->
          case minimumAboveNTree (Max filteredTs vMap) (bits 10) (prec 10) (width vMap /! 2) n of
            (Just True, mBox) -> (Just True, mBox)
            _ -> 
              if width vMap !>! 1 / 10000000 then -- make this threshold quite small (maybe 10^-7)
                  trace ("Bisected boxes: " ++ show newBoxes)
                  checkBoxes newBoxes filteredTs
                else
                  trace "Stopping bisections (Box too small)" $
                  case minimumAboveNTree (Max filteredTs vMap) ac p (width vMap /! 10000000) n of
                    (Just True, mBox) -> (Just True, mBox)
                    (_, mBox)      -> (Nothing, mBox) -- Cannot guarantee that a result is False here
    where   
      filteredTs = 
        filter 
          (\t ->
            not (applyTree (updateVarMap t vMap) !<! n)
          ) 
          ts

      roughCheck = 
        map
          (\t ->
            applyTree (updateVarMap t vMap) !>=! n
          )
        filteredTs

      newBoxes = fullBisect vMap

      checkBoxes []          _  = (Just True, Nothing)
      checkBoxes (v : vs) ts' = case checkTree (Max ts' v) ac p n of
                                  (Just True, _) -> checkBoxes vs ts'
                                  o              -> 
                                    trace ("found false at " ++ show v)
                                    o

checkTree (Min ts vMap) ac p n = parallelAndList (map (\t -> checkTree (updateVarMap t vMap) ac p n) ts)

size :: MinMaxTree -> Integer
size (Leaf _ _)  = 1
size (Min ts _) = 1 + sum (map size ts)
size (Max ts _) = 1 + sum (map size ts)

applyTree :: MinMaxTree -> CN MPBall

-- applyTree (Leaf f)  box = globalMinimumAboveN (BoxFun (dimension (fst f)) (bf_eval (fst f)) box) (bits 100) (prec 1000) (cn 0.0)
applyTree (Leaf e vMap)  = apply f (domain f) where f = expressionToBoxFun e vMap
applyTree (Max ts vMap)   = maximum (map (\t -> applyTree (updateVarMap t vMap)) ts)
applyTree (Min ts vMap)   = minimum (map (\t -> applyTree (updateVarMap t vMap)) ts)

-- TODO: globalMinimum and globalMaximum with low accuracy/precision to compute more accurate r and lRanges

minimumAboveNTree :: MinMaxTree -> Accuracy -> Precision -> Rational -> Rational -> (Maybe Bool, Maybe SearchBox) -- make the cutoff a quarter of a width
minimumAboveNTree (Leaf t vMap) ac p widthCutoff n = globalMinimumAboveN f ac p (cn (mpBallP p widthCutoff)) (cn (mpBallP p n))
  where f = expressionToBoxFun t vMap
minimumAboveNTree (Max ts vMap) ac p widthCutoff n = parallelOrList (map (\t -> minimumAboveNTree (updateVarMap t vMap) ac p widthCutoff n) ts)
minimumAboveNTree (Min ts vMap) ac p widthCutoff n = parallelAndList (map (\t -> minimumAboveNTree (updateVarMap t vMap) ac p widthCutoff n) ts)

updateVarMap :: MinMaxTree -> VarMap -> MinMaxTree
updateVarMap (Leaf t _) vMap = Leaf t vMap
updateVarMap (Min ts _) vMap = Min ts vMap
updateVarMap (Max ts _) vMap = Max ts vMap

findLeaves :: MinMaxTree -> [MinMaxTree]
findLeaves (Min ts _) = concatMap findLeaves ts
findLeaves (Max ts _) = concatMap findLeaves ts
findLeaves l@(Leaf _ _) = [l]

findUniqueLeaves :: MinMaxTree -> [MinMaxTree]
findUniqueLeaves t = filter (\t' -> t' `notElem` delete t' ts) ts
  where ts = findLeaves t

findNonUniqueLeaves :: MinMaxTree -> [MinMaxTree]
findNonUniqueLeaves t = filter (\t' -> t' `elem` delete t' ts) ts
  where ts = findLeaves t

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
