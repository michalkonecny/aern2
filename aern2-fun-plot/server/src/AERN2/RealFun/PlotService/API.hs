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
  , FunctionName, FunctionDomain, FunctionPoint, FunctionId
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
        "valuesForSampling" :> Capture "samplingId" SamplingId :> Get '[JSON] [FunctionPoint] :<|>
     "function" :> Capture "functionId" FunctionId :> "name" :> Get '[JSON] FunctionName
    )

api :: Proxy Api
api = Proxy

type SamplingId = Int
type FunctionId = Int
type FunctionName = String
type FunctionDomain = DyadicInterval'
type FunctionPoint = (DyadicInterval', MPBallInterval')

type DyadicInterval' = (Dyadic, Dyadic)
type MPBallInterval' = (MPBall, MPBall)

data Sampling =
  Sampling
  {
    sampling_dom' :: DyadicInterval'
    , sampling_maxStep :: ErrorBound
  }
  deriving (Show, P.Eq, Generic)

sampling_dom :: Sampling -> DyadicInterval
sampling_dom (Sampling (domL, domR) _) = Interval domL domR

instance (ElmType l, ElmType r) => ElmType (Interval l r)
instance (ToJSON l, ToJSON r) => ToJSON (Interval l r)
instance (FromJSON l, FromJSON r) => FromJSON (Interval l r)

instance ElmType MPBall
instance ToJSON MPBall
instance FromJSON MPBall

instance ElmType Sampling
instance ToJSON Sampling
instance FromJSON Sampling

instance ElmType DyadicS
instance ToJSON DyadicS
instance FromJSON DyadicS

data DyadicS =
  DyadicS
  {
    _dyadic_value :: Integer,
    _dyadic_exp :: Int
  }
  deriving (Show, P.Eq, Generic)

instance FromJSON Dyadic where
  parseJSON jd = dyadicS2dyadic <$> parseJSON jd

instance ToJSON Dyadic where
  toJSON = toJSON . dyadic2dyadicS
  toEncoding = toEncoding . dyadic2dyadicS

instance ElmType Dyadic where
  toElmType = toElmType . dyadic2dyadicS

dyadicS2dyadic :: DyadicS -> Dyadic
dyadicS2dyadic (DyadicS v e) = (dyadic v) * ((dyadic 0.5)^e)

dyadic2dyadicS :: Dyadic -> DyadicS
dyadic2dyadicS d = DyadicS v e
  where
  v = numerator r
  e = integerLog2 $ denominator r
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
