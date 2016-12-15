{-|
    Module      :  AERN2.RealFun.PlotService.App
    Description :  Serving data about a real function
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Serving data about a real function
-}

module AERN2.RealFun.PlotService.App
(
  startServer, app,
  Functions, Function(..), functionUsingEval, intervalFunctionUsingEval
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

import           System.IO

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import qualified Data.Map as Map
import           Network.Wai
import           Network.Wai.MakeAssets
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Servant

import AERN2.MP.Dyadic (dyadic)
import AERN2.MP.Ball as MPBall
import AERN2.Interval as Interval
import AERN2.RealFun.Operations
import AERN2.RealFun.PlotService.API

startServer :: Functions -> Bool -> Port -> IO ()
startServer fns shouldLog port = do
  runSettings settings =<< (fmap logger (app fns))
  where
  logger
    | shouldLog = logStdoutDev
    | otherwise = id
  settings =
    setPort port $
    setBeforeMainLoop (hPutStrLn stderr
      ("listening on port " ++ show port ++ "...")) $
    defaultSettings

type WithAssets = Api :<|> Raw

withAssets :: Proxy WithAssets
withAssets = Proxy

app ::
  Functions -> IO Application
app fns =
  serve withAssets <$> server fns

server ::
  Functions -> IO (Server WithAssets)
server fns = do
  assets <- serveAssets
  samplingsStore <- mkSamplingsStore
  return $ apiServer fns samplingsStore :<|> assets

apiServer ::
  Functions -> SamplingsStore -> Server Api
apiServer fns samplingsStore =
  listSamplings samplingsStore :<|>
  postSampling samplingsStore :<|>
  getSampling samplingsStore :<|>
  listFunctionIds fns :<|>
  getFunctionDomain fns :<|>
  getFunctionValues fns samplingsStore :<|>
  getFunctionName fns :<|>
  getFunctionColour fns

{- Functions processing -}

type Functions = [Function]

data Function =
  Function
  { function_name :: FunctionName
  , function_colour :: FunctionColour
  , function_dom :: DyadicInterval
  , function_getBounds :: DyadicInterval -> Interval MPBall MPBall
  }

functionUsingEval ::
  (HasDomain fn, Domain fn ~ DyadicInterval,
  CanApply fn DyadicInterval, ApplyType fn DyadicInterval ~ MPBall)
  =>
  (FunctionName, fn) -> Function
functionUsingEval (name, fn) =
  Function
  { function_name = name
  , function_dom = getDomain fn
  , function_getBounds = \ di -> let val = apply fn di in Interval val val
  }

intervalFunctionUsingEval ::
  (HasDomain fn, Domain fn ~ DyadicInterval,
  CanApply fn DyadicInterval, ApplyType fn DyadicInterval ~ Interval MPBall MPBall)
  =>
  (FunctionName, fn) -> Function
intervalFunctionUsingEval (name, fn) =
  Function
  { function_name = name
  , function_dom = getDomain fn
  , function_getBounds = apply fn
  }

listFunctionIds :: Functions -> Handler [FunctionId]
listFunctionIds fns = return $ map int [0..n - 1]
  where
  n = integer $ length fns

getFunctionDomain ::
  Functions -> FunctionId -> Handler FunctionDomain
getFunctionDomain fns fnId =
  maybe (throwE err404) return =<< (return $ fmap getDom maybeFn)
  where
  getDom = dyadicIntervalAPI . function_dom
  maybeFn = lookupFunction fns fnId

getFunctionName ::
  Functions -> FunctionId -> Handler FunctionName
getFunctionName fns fnId =
  maybe (throwE err404) return =<< (return $ fmap function_name maybeFn)
  where
  maybeFn = lookupFunction fns fnId

getFunctionColour ::
  Functions -> FunctionId -> Handler FunctionColour
getFunctionColour fns fnId =
  maybe (throwE err404) return =<< (return $ fmap function_colour maybeFn)
  where
  maybeFn = lookupFunction fns fnId

lookupFunction :: Functions -> FunctionId -> Maybe Function
lookupFunction fns fnId
  | 0 <= fnId && fnId < length fns = Just (fns !! fnId)
  | otherwise = Nothing

getFunctionValues ::
  Functions ->
  SamplingsStore ->
  FunctionId ->
  SamplingId ->
  Handler [FunctionSegment]
getFunctionValues fns samplingsStore fnId samplingId =
  do
  maybeSampling <- liftIO $ lookupSampling samplingsStore samplingId
  useSamplingAndFn maybeSampling (lookupFunction fns fnId)
  where
  useSamplingAndFn Nothing _ = throwE err404
  useSamplingAndFn _ Nothing = throwE err404
  useSamplingAndFn (Just sampling) (Just fn) =
    case maybeIntersectedDom of
      Just intersectedDom -> return $ recursiveEval maxDepth intersectedDom
      _ -> return []
    where
    maybeIntersectedDom = dom `Interval.intersect` samplingDom
    dom = function_dom fn
    samplingDom = sampling_dom sampling
    maxStep = dyadic $ sampling_maxStep sampling
    getBounds = function_getBounds fn
    maxDepth = 20
    recursiveEval maxD di
      | boundsGoodEnough || maxD == 0 =
        [FunctionSegment
          (dyadicIntervalAPI di)
          (mpBallIntervalAPI boundsL)
          (mpBallIntervalAPI boundsR)]
      | otherwise = (recursiveEval maxD' di1) ++ (recursiveEval maxD' di2)
      where
      maxD' = maxD - 1
      (di1, di2) = Interval.split di

      (Interval diL diR) = di
      boundsL@(Interval lL rL) = getBounds (Interval diL diL)
      boundsR@(Interval lR rR) = getBounds (Interval diR diR)
      boundsGoodEnough =
        (diR-diL <= maxStep && abs (lL-lR) !<=! maxStep) && (abs (rL-rR) !<=! maxStep)

{- Samplings storage -}

newtype SamplingsStore = SamplingsStore (MVar (Map.Map SamplingId Sampling))

mkSamplingsStore :: IO SamplingsStore
mkSamplingsStore = SamplingsStore <$> newMVar Map.empty

listSamplings :: SamplingsStore -> Handler [SamplingId]
listSamplings samplingsStore = liftIO $ allSamplingIds samplingsStore

allSamplingIds :: SamplingsStore -> IO [SamplingId]
allSamplingIds (SamplingsStore mvar) =
  Map.keys <$> readMVar mvar

postSampling :: SamplingsStore -> Sampling -> Handler SamplingId
postSampling samplingsStore newSampling =
  liftIO $ insertSampling samplingsStore newSampling

insertSampling :: SamplingsStore -> Sampling -> IO SamplingId
insertSampling (SamplingsStore mvar) newSampling = modifyMVar mvar $ \ m -> do
  let newKey = case Map.keys m of
        [] -> int 0
        ks -> succ (maximum ks)
  return (Map.insert newKey newSampling m, newKey)

getSampling :: SamplingsStore -> SamplingId -> Handler Sampling
getSampling samplingsStore i =
  maybe (throwE err404) return =<< liftIO (lookupSampling samplingsStore i)

lookupSampling :: SamplingsStore -> SamplingId -> IO (Maybe Sampling)
lookupSampling (SamplingsStore mvar) i = do
  Map.lookup i <$> readMVar mvar
