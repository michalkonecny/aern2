{-|
    Module      :  AERN2.RealFun.UnaryBallFun.Type
    Description :  type definition and basics
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Type definitions and basics.
-}

module AERN2.RealFun.UnaryBallFun.Type
(
  UnaryBallFun(..), unaryBallFun
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

import Data.Typeable

-- import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck



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

data UnaryBallFun =
  UnaryBallFun
  {
    unaryBallFun_Domain :: DyadicInterval
    ,
    {-| For convergent sequence of *open* balls the resulting sequence should also converge. -}
    unaryBallFun_Eval :: CN MPBall -> CN MPBall
  }

instance HasDomain UnaryBallFun where
  type Domain UnaryBallFun = DyadicInterval
  getDomain = unaryBallFun_Domain

type CanBeUnaryBallFun t = ConvertibleExactly t UnaryBallFun
unaryBallFun :: (CanBeUnaryBallFun t) => t -> UnaryBallFun
unaryBallFun = convertExactly

instance (CanBeMPBall t, Show t, Typeable t)
  =>
  ConvertibleExactly (DyadicInterval, t) UnaryBallFun
  where
  safeConvertExactly (dom, x) =
    case safeConvertExactly x of
      Right b -> Right $ UnaryBallFun dom (const $ cn (b :: MPBall))
      _err -> convError "unable to convert to constant function: " (dom,x)

instance HasFnConstructorInfo UnaryBallFun where
  type FnConstructorInfo UnaryBallFun = DyadicInterval
  getFnConstructorInfo = getDomain

instance HasVars UnaryBallFun where
  type Var UnaryBallFun = ()
  varFn dom () =
    UnaryBallFun dom cn
