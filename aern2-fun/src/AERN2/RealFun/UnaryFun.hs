{-|
    Module      :  AERN2.RealFun.UnaryFun
    Description :  Real functions represented by Haskell evaluators
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Real functions represented by Haskell evaluators
-}

module AERN2.RealFun.UnaryFun
(
  UnaryFun(..), unaryFun
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

import Data.Typeable

import Control.Arrow

import Control.Lens.Operators

-- import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

import Numeric.CatchingExceptions

import AERN2.MP.Dyadic
import AERN2.MP.Ball

import AERN2.QA
import AERN2.Real

import AERN2.Interval
import AERN2.RealFun.Operations

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

instance CanApply UnaryFun (CatchingNumExceptions MPBall) where
  type ApplyType UnaryFun (CatchingNumExceptions MPBall) = CatchingNumExceptions MPBall
  apply f = unaryFun_Eval f . checkInDom f

checkInDom :: UnaryFun -> CatchingNumExceptions MPBall -> CatchingNumExceptions MPBall
checkInDom f cb =
  case cb ^. numEXC_maybeValue of
    Just b | domB `contains` b -> cb
    Just b | domB ?<=? b && b ?<=? domB ->
      fmap (intersect domB) cb
    Just _ -> addCertainException cb (OutOfRange "apply UnaryFun: argument out of function domain")
    _ -> cb
  where
  domB = mpBall (unaryFun_Domain f)

instance (QAArrow to) => CanApply UnaryFun (CauchyRealA to) where
  type ApplyType UnaryFun (CauchyRealA to) = (CauchyRealA to)
  apply f =
    unaryOp "apply" (apply f) (getInitQ1FromSimple (arr id))

instance CanApply UnaryFun Integer where
  type ApplyType UnaryFun Integer = CatchingNumExceptions MPBall
  apply f = apply f . catchingNumExceptions . mpBall

instance CanApply UnaryFun Int where
  type ApplyType UnaryFun Int = CatchingNumExceptions MPBall
  apply f = apply f . catchingNumExceptions . mpBall

instance CanApply UnaryFun Dyadic where
  type ApplyType UnaryFun Dyadic = CatchingNumExceptions MPBall
  apply f = apply f . catchingNumExceptions . mpBall

instance CanApply UnaryFun DyadicInterval where
  type ApplyType UnaryFun DyadicInterval = Interval CauchyReal CauchyReal
  apply p =
    undefined -- TODO
