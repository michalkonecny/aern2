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
  app, Functions(..)
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import qualified Data.Map as Map
import           Network.Wai
import           Network.Wai.MakeAssets
import           Servant

import AERN2.MP.Ball
import AERN2.Interval as Interval
-- import AERN2.RealFun.Operations
import AERN2.RealFun.PlotService.API

type WithAssets = Api :<|> Raw

withAssets :: Proxy WithAssets
withAssets = Proxy

app ::
  Functions fn -> IO Application
app fns =
  serve withAssets <$> server fns

server ::
  Functions fn -> IO (Server WithAssets)
server fns = do
  assets <- serveAssets
  samplingsStore <- mkSamplingsStore
  return $ apiServer fns samplingsStore :<|> assets

apiServer ::
  Functions fn -> SamplingsStore -> Server Api
apiServer fns samplingsStore =
  listSamplings samplingsStore :<|>
  postSampling samplingsStore :<|>
  getSampling samplingsStore :<|>
  listFunctionIds fns :<|>
  getFunctionDomain fns :<|>
  getFunctionValues fns samplingsStore :<|>
  getFunctionName fns

{- Functions processing -}

data Functions fn =
  Functions
  {
    functions_fns :: [(FunctionName, fn)]
    , functions_getDom :: fn -> DyadicInterval
    , functions_getBounds :: fn -> DyadicInterval -> Interval MPBall MPBall
  }

listFunctionIds :: Functions fn -> Handler [FunctionId]
listFunctionIds fns = return $ map int [0..n - 1]
  where
  n = integer $ length $ functions_fns fns

getFunctionDomain ::
  Functions fn -> FunctionId -> Handler DyadicInterval
getFunctionDomain fns fnId =
  maybe (throwE err404) return =<< (return $ fmap (getDom . snd) maybeFn)
  where
  getDom = functions_getDom fns
  maybeFn = lookupFunction fns fnId

getFunctionName ::
  Functions fn -> FunctionId -> Handler FunctionName
getFunctionName fns fnId =
  maybe (throwE err404) return =<< (return $ fmap fst maybeFn)
  where
  maybeFn = lookupFunction fns fnId

lookupFunction :: Functions fn -> FunctionId -> Maybe (FunctionName, fn)
lookupFunction fns fnId
  | 0 <= fnId && fnId < length (functions_fns fns) = Just ((functions_fns fns) !! fnId)
  | otherwise = Nothing

getFunctionValues ::
  Functions fn ->
  SamplingsStore ->
  FunctionId ->
  SamplingId ->
  Handler [(DyadicInterval, Interval MPBall MPBall)]
getFunctionValues fns samplingsStore fnId samplingId =
  do
  maybeSampling <- liftIO $ lookupSampling samplingsStore samplingId
  useSamplingAndFn maybeSampling (lookupFunction fns fnId)
  where
  useSamplingAndFn Nothing _ = throwE err404
  useSamplingAndFn _ Nothing = throwE err404
  useSamplingAndFn (Just sampling) (Just (_fnName, fn))
    | not (dom `Interval.contains` samplingDom) = throwE err404
    | otherwise = return $ recursiveEval maxDepth samplingDom
    where
    dom = functions_getDom fns fn
    samplingDom = sampling_dom sampling
    maxStep = sampling_maxStep sampling
    getBounds = functions_getBounds fns
    maxDepth = 20
    recursiveEval maxD di
      | boundsGoodEnough || maxD == 0 = [(di, bounds)]
      | otherwise = (recursiveEval maxD' diL) ++ (recursiveEval maxD' diR)
      where
      maxD' = maxD - 1
      (diL, diR) = Interval.split di
      bounds@(Interval l r) = getBounds fn di
      boundsGoodEnough =
        (radius l <= maxStep) && (radius r <= maxStep)

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
