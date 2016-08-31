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
  UnaryFun(..)
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

instance CanApply UnaryFun MPBall where
  type ApplyType UnaryFun MPBall = MPBall
  apply = unaryFun_Eval

instance (QAArrow to) => CanApply UnaryFun (CauchyRealA to) where
  type ApplyType UnaryFun (CauchyRealA to) = (CauchyRealA to)
  apply f =
    unaryOp "apply" (fmap (unaryFun_Eval f . checkInDom (unaryFun_Domain f))) (getInitQ1FromSimple (arr id))
    where
    checkInDom domI b
      | domB ?<=? b && b ?<=? domB = intersect domB b
      | otherwise = error "apply UnaryFun CauchyReal: argument out of function domain"
      where
      domB = mpBall domI

instance CanApply UnaryFun Integer where
  type ApplyType UnaryFun Integer = MPBall
  apply f = unaryFun_Eval f . mpBall

instance CanApply UnaryFun Int where
  type ApplyType UnaryFun Int = MPBall
  apply f = unaryFun_Eval f . mpBall

instance CanApply UnaryFun Dyadic where
  type ApplyType UnaryFun Dyadic = MPBall
  apply f = unaryFun_Eval f . mpBall

instance HasDomain UnaryFun where
  type Domain UnaryFun = DyadicInterval
  getDomain = unaryFun_Domain

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
