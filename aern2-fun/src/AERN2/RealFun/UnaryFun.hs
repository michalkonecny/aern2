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

-- import qualified Data.List as List

-- import Test.Hspec
-- import Test.QuickCheck

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
    unaryFun_Eval :: MPBall -> MPBall
  }

instance HasDomain UnaryFun where
  type Domain UnaryFun = DyadicInterval
  getDomain = unaryFun_Domain

type CanBeUnaryFun t = ConvertibleExactly t UnaryFun
unaryFun :: (CanBeUnaryFun t) => t -> UnaryFun
unaryFun = convertExactly

instance (CanBeMPBall t, Show t, Typeable t) => ConvertibleExactly (DyadicInterval, t) UnaryFun where
  safeConvertExactly (dom, x) =
    case safeConvertExactly x of
      Right b -> Right $ UnaryFun dom (const b)
      _err -> convError "unable to convert to constant function: " (dom,x)

instance HasVars UnaryFun where
  type Var UnaryFun = ()
  varFn sampleF () =
    UnaryFun dom id
    where
    dom = getDomain sampleF

instance CanApply UnaryFun MPBall where
  type ApplyType UnaryFun MPBall = MPBall
  apply f = unaryFun_Eval f . checkInDom f

checkInDom :: UnaryFun -> MPBall -> MPBall
checkInDom f b
  | domB ?<=? b && b ?<=? domB = intersect domB b
  | otherwise = error "apply UnaryFun: argument out of function domain"
  where
  domB = mpBall (unaryFun_Domain f)

instance (QAArrow to) => CanApply UnaryFun (CauchyRealA to) where
  type ApplyType UnaryFun (CauchyRealA to) = (CauchyRealA to)
  apply f =
    unaryOp "apply" (fmap (apply f)) (getInitQ1FromSimple (arr id))

instance CanApply UnaryFun Integer where
  type ApplyType UnaryFun Integer = MPBall
  apply f = apply f . mpBall

instance CanApply UnaryFun Int where
  type ApplyType UnaryFun Int = MPBall
  apply f = apply f . mpBall

instance CanApply UnaryFun Dyadic where
  type ApplyType UnaryFun Dyadic = MPBall
  apply f = apply f . mpBall

instance CanApply UnaryFun DyadicInterval where
  type ApplyType UnaryFun DyadicInterval = Interval CauchyReal CauchyReal
  apply p =
    undefined -- TODO
