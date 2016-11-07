{-|
    Module      :  AERN2.RealFun.PlotService.API
    Description :  Restful API for real functions
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Restful API for real functions
-}

module AERN2.RealFun.PlotService.API
(
  Api, api
  , Sampling(..), SamplingId, sampling_dom
  , FunctionName, FunctionDomain, FunctionSegment(..), FunctionId,
  mpBallIntervalAPI, dyadicIntervalAPI
)
where

import Numeric.MixedTypes
import qualified Prelude as P
-- import Text.Printf

import Data.Ratio
import Math.NumberTheory.Logarithms (integerLog2)

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant.API
import Servant.Elm
import Elm

-- import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

import AERN2.MP.Float
import AERN2.MP.Dyadic
import AERN2.MP.Ball
import AERN2.MP.ErrorBound

import AERN2.Interval

-- import AERN2.RealFun.Operations


type Api =
  "api" :>
    ("sampling" :> Get '[JSON] [SamplingId] :<|>
     "sampling" :> ReqBody '[JSON] Sampling :> Post '[JSON] SamplingId :<|>
     "sampling" :> Capture "samplingId" SamplingId :> Get '[JSON] Sampling :<|>
     "function" :> Get '[JSON] [FunctionId] :<|>
     "function" :> Capture "functionId" FunctionId :> "domain" :> Get '[JSON] FunctionDomain :<|>
     "function" :> Capture "functionId" FunctionId :>
        "valuesForSampling" :> Capture "samplingId" SamplingId :> Get '[JSON] [FunctionSegment] :<|>
     "function" :> Capture "functionId" FunctionId :> "name" :> Get '[JSON] FunctionName
    )

api :: Proxy Api
api = Proxy

type SamplingId = Int
type FunctionId = Int
type FunctionName = String
type FunctionDomain = DyadicIntervalAPI
data FunctionSegment =
  FunctionSegment
  { functionSegmentDom :: DyadicIntervalAPI
  , functionSegmentValueL :: MPBallIntervalAPI
  , functionSegmentValueR :: MPBallIntervalAPI
  }
  deriving (Show, P.Eq, Generic)

instance ElmType FunctionSegment
instance ToJSON FunctionSegment
instance FromJSON FunctionSegment

instance (ElmType l, ElmType r) => ElmType (Interval l r)
instance (ToJSON l, ToJSON r) => ToJSON (Interval l r)
instance (FromJSON l, FromJSON r) => FromJSON (Interval l r)

data DyadicIntervalAPI =
  DyadicIntervalAPI
  { dyadic_endpointL :: Dyadic
  , dyadic_endpointR :: Dyadic }
  deriving (Show, P.Eq, Generic)

dyadicIntervalAPI :: DyadicInterval -> DyadicIntervalAPI
dyadicIntervalAPI (Interval l r) = (DyadicIntervalAPI l r)

instance ElmType DyadicIntervalAPI
instance ToJSON DyadicIntervalAPI
instance FromJSON DyadicIntervalAPI
--
data MPBallIntervalAPI =
  MPBallIntervalAPI
  { mpBall_endpointL :: MPBall
  , mpBall_endpointR :: MPBall }
  deriving (Show, P.Eq, Generic)

mpBallIntervalAPI :: Interval MPBall MPBall -> MPBallIntervalAPI
mpBallIntervalAPI (Interval l r) = (MPBallIntervalAPI l r)

instance ElmType MPBall
instance ToJSON MPBall
instance FromJSON MPBall

instance ElmType MPBallIntervalAPI
instance ToJSON MPBallIntervalAPI
instance FromJSON MPBallIntervalAPI

data Sampling =
  Sampling
  {
    sampling_dom' :: DyadicIntervalAPI
    , sampling_maxStep :: ErrorBound
  }
  deriving (Show, P.Eq, Generic)

sampling_dom :: Sampling -> DyadicInterval
sampling_dom (Sampling (DyadicIntervalAPI domL domR) _) = Interval domL domR

instance ElmType Sampling
instance ToJSON Sampling
instance FromJSON Sampling

data DyadicS =
  DyadicS
  {
    dyadic_value :: Integer,
    dyadic_exp :: Int
  }
  deriving (Show, P.Eq, Generic)

instance ElmType DyadicS
instance ToJSON DyadicS
instance FromJSON DyadicS

-- the following definitions exist only to stop unused warnings:
_use_dyadic_value :: Integer
_use_dyadic_value = dyadic_value undefined
_use_dyadic_exp :: Int
_use_dyadic_exp = dyadic_exp undefined

instance FromJSON Dyadic where
  parseJSON jd = dyadicS2dyadic <$> parseJSON jd

instance ToJSON Dyadic where
  toJSON = toJSON . dyadic2dyadicS
  toEncoding = toEncoding . dyadic2dyadicS

instance ElmType Dyadic where
  toElmType = toElmType . dyadic2dyadicS

dyadicS2dyadic :: DyadicS -> Dyadic
dyadicS2dyadic (DyadicS v e)
  | e < 0 = (dyadic v) * ((dyadic 0.5)^(-e))
  | otherwise = (dyadic v) * ((dyadic 2)^e)

dyadic2dyadicS :: Dyadic -> DyadicS
dyadic2dyadicS d = DyadicS v e
  where
  v = numerator r
  e = negate $ integerLog2 $ denominator r
  r = rational d

instance FromJSON MPFloat where
  parseJSON jd = dyadic2mpFloat <$> parseJSON jd

instance ToJSON MPFloat where
  toJSON = toJSON . mpFloat2dyadic
  toEncoding = toEncoding . mpFloat2dyadic

instance ElmType MPFloat where
  toElmType = toElmType . mpFloat2dyadic

dyadic2mpFloat :: Dyadic -> MPFloat
dyadic2mpFloat = convertExactly

mpFloat2dyadic :: MPFloat -> Dyadic
mpFloat2dyadic = convertExactly

instance FromJSON ErrorBound where
  parseJSON jd = mpFloat2errorBound <$> parseJSON jd

instance ToJSON ErrorBound where
  toJSON = toJSON . errorBound2MPFloat
  toEncoding = toEncoding . errorBound2MPFloat

instance ElmType ErrorBound where
  toElmType = toElmType . errorBound2MPFloat

errorBound2MPFloat :: ErrorBound -> MPFloat
errorBound2MPFloat = convertExactly

mpFloat2errorBound :: MPFloat -> ErrorBound
mpFloat2errorBound = errorBound
