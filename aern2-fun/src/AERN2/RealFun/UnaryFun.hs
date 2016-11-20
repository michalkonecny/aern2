{-|
    Module      :  AERN2.RealFun.UnaryFun
    Description :  Real functions represented by MPBall evaluators
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Real functions represented by MPBall evaluators
-}

module AERN2.RealFun.UnaryFun
(
  UnaryFun(..), unaryFun
)
where

-- import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

import AERN2.RealFun.UnaryFun.Type
import AERN2.RealFun.UnaryFun.Evaluation ()
import AERN2.RealFun.UnaryFun.Integration ()
