{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module AERN2.TestApprox
-- ()
where

import MixedTypesNumPrelude
-- import Numeric.CollectErrors (NumErrors, CanTakeErrors(..))
-- import qualified Numeric.CollectErrors as CN

import qualified Prelude as P

-- import qualified Debug.Trace as Debug
-- import Text.Printf (printf)

-- import AERN2.MP (MPBall (ball_value), mpBallP)
-- import AERN2.MP.Float (MPFloat)
import AERN2.Real
import GHC.Num (integerLog2)
import qualified AERN2.Real.Type as Real
import AERN2.MP (MPBall, HasPrecision (getPrecision), CanTestContains (contains), IsBall (centreAsBall), updateRadius)
import AERN2.MP.ErrorBound ( errorBound )
import Numeric.CollectErrors (noValueNumErrorPotential, NumError (NumError))

sqrt_step :: _ => t -> t -> t
sqrt_step x y = (y + x/y)/2

restr_sqrt_limit :: CReal -> CReal
restr_sqrt_limit x = limit $ sqrt_approx_fast x
  where    
  sqrt_approx_fast :: _ => t -> Integer -> t
  sqrt_approx_fast x n =
    sqrt_approx x (1 + (P.fromIntegral (integerLog2 (n+1)) :: Integer))    
  sqrt_approx :: _ => t -> Integer -> t
  sqrt_approx x n =
    (iterate (sqrt_step x) (fromInteger_ 1)) !! n

restr_sqrt_incl :: CReal -> CReal
restr_sqrt_incl = Real.lift1 restr_sqrt_MPBall
  where
  restr_sqrt_MPBall :: CN MPBall -> CN MPBall
  restr_sqrt_MPBall xb = 
    waitForInclusion $ take 100 $ iterate (sqrt_step xb . widenCentre) $ xb
    where
    widenCentre = updateRadius (const eps)
    eps = errorBound $ 0.5^(integer p)
    p = getPrecision xb
    waitForInclusion (b1:b2:rest) 
      | b2 `contains` b1 = b1
      | otherwise = waitForInclusion (b2:rest)
    waitForInclusion _ = 
      noValueNumErrorPotential $ NumError "failed to converge"

