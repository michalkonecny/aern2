{-|
    Module      :  AERN2.RealFun.UnaryBallFun
    Description :  Real functions represented by MPBall evaluators
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Real functions represented by MPBall evaluators
-}

module AERN2.RealFun.UnaryBallFun
(
  UnaryBallFun(..), unaryFun
)
where

-- import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

import AERN2.RealFun.UnaryBallFun.Type
import AERN2.RealFun.UnaryBallFun.Evaluation ()
import AERN2.RealFun.UnaryBallFun.Integration ()
