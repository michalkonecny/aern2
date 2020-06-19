module AERN2.BoxFunMinMax.Type where

import AERN2.MP.Ball
import MixedTypesNumPrelude
import AERN2.BoxFun.Type
import AERN2.BoxFun.Box
import AERN2.BoxFun.Optimisation (SearchBox)
import AERN2.BoxFun.TestFunctions
import AERN2.BoxFunMinMax.Optimisation

import Debug.Trace (trace)

import Control.Monad
import Control.Concurrent
import System.IO.Unsafe

import Control.Parallel.Strategies

import Data.List (take, drop, null)

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
data MinMaxTree = Leaf {tree_f :: BoxFun} | Min [MinMaxTree] | Max [MinMaxTree]

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
  if applyMinimumOnBox f box' !>! n then
    (Just True, Nothing)
  else
    globalMinimumAboveN f' ac p (width box' / 10000000) (cn (mpBallP p n))
  where
    f' = BoxFun (dimension f) (bf_eval f) box'
    box' = setPrecision p box
checkTree (Max ts) box ac p n =
    if (or roughCheck) then
      (Just True, Nothing)
    else 
      case filteredTs of
        [] -> (Just False, Nothing)
        [t] -> checkTree t box ac p n
        _ ->
          case minimumAboveNTree (Max filteredTs) box' (bits 10) (prec 10) (width box / 2) n of
            (Just True, mBox) -> (Just True, mBox)
            _ -> 
              if AERN2.BoxFun.Box.width box' !>! 1 / (10000000) then -- make this threshold quite small (maybe 10^-7)
                  trace ("Bisected boxes: " ++ show newBoxes)
                  checkBoxes newBoxes filteredTs
                else
                  trace "Stopping bisections (Box too small)" $
                  case minimumAboveNTree (Max filteredTs) box' ac p (width box / 10000000) n of
                    (Just True, mBox) -> (Just True, mBox)
                    (_, mBox)      -> (Nothing, mBox) -- Cannot guarantee that a result is False here
    where   
      filteredTs = 
        filter 
          (\t ->
            not (applyTree t box' !<! n)
          ) 
          ts

      roughCheck = 
        map
          (\t ->
            applyTree t box' !>=! n
          )
        filteredTs

      box' = setPrecision p box

      newBoxes = fullBisect box'

      checkBoxes []          _  = (Just True, Nothing)
      checkBoxes (b : boxes) ts' = case checkTree (Max ts') b ac p n of
                                  (Just True, _) -> checkBoxes boxes ts'
                                  o              -> 
                                    trace ("found false at " ++ show b)
                                    o

checkTree (Min l) box ac p n = parallelAndList (map (\t -> checkTree t box ac p n) l)

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

minimumAboveNTree :: MinMaxTree -> Box -> Accuracy -> Precision -> CN MPBall -> Rational -> (Maybe Bool, Maybe SearchBox) -- make the cutoff a quarter of a width
minimumAboveNTree (Leaf f) box ac p widthCutoff n = globalMinimumAboveN (BoxFun (dimension f) (bf_eval f) box) ac p widthCutoff (cn (mpBallP p n))
minimumAboveNTree (Max l)  box ac p widthCutoff n = parallelOrList (map (\t -> minimumAboveNTree t box ac p widthCutoff n) l)
minimumAboveNTree (Min l)  box ac p widthCutoff n = parallelAndList (map (\t -> minimumAboveNTree t box ac p widthCutoff n) l)

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
