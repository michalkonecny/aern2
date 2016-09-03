{-|
    Module      :  AERN2.RealFun.UnaryFun.Type
    Description :  type definition and basics
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Type definitions and basics.
-}

module AERN2.RealFun.UnaryFun.Type
(
  UnaryFun(..), unaryFun
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

import Data.Typeable

-- import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

import Numeric.CatchingExceptions

-- import AERN2.MP.Dyadic
import AERN2.MP.Ball

import AERN2.Interval
import AERN2.RealFun.Operations

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
-- shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace = if shouldTrace then trace else const id
_dummy :: ()
_dummy = maybeTrace "dummy" ()

data UnaryFun =
  UnaryFun
  {
    unaryFun_Domain :: DyadicInterval
    ,
    {-| For convergent sequence of *open* balls the resulting sequence should also converge. -}
    unaryFun_Eval :: CatchingNumExceptions MPBall -> CatchingNumExceptions MPBall
  }

instance HasDomain UnaryFun where
  type Domain UnaryFun = DyadicInterval
  getDomain = unaryFun_Domain

type CanBeUnaryFun t = ConvertibleExactly t UnaryFun
unaryFun :: (CanBeUnaryFun t) => t -> UnaryFun
unaryFun = convertExactly

instance (CanBeMPBall t, Show t, Typeable t)
  =>
  ConvertibleExactly (DyadicInterval, t) UnaryFun
  where
  safeConvertExactly (dom, x) =
    case safeConvertExactly x of
      Right b -> Right $ UnaryFun dom (fmap $ const b)
      _err -> convError "unable to convert to constant function: " (dom,x)

instance HasVars UnaryFun where
  type Var UnaryFun = ()
  varFn sampleF () =
    UnaryFun dom id
    where
    dom = getDomain sampleF
