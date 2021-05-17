{-|
    Module      :  AERN2.MP
    Description :  Multiple-precision ball arithmetic
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Multiple-precision ball arithmetic
-}

module AERN2.MP
( module AERN2.Norm
, module AERN2.MP.Precision
, module AERN2.MP.Accuracy
, module AERN2.MP.ErrorBound
, module AERN2.MP.Enclosure
, MPBall(..), CanBeMPBall, mpBall, CanBeMPBallP, mpBallP
, reducePrecionIfInaccurate
, giveUpIfVeryInaccurate
)
where

-- import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

import AERN2.Norm
import AERN2.MP.Precision
import AERN2.MP.Accuracy
import AERN2.MP.ErrorBound
import AERN2.MP.Enclosure
import AERN2.MP.Ball
