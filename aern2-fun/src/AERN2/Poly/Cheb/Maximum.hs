module AERN2.Poly.Cheb.Maximum
(
maximum,
maximumOptimised,
maximumOptimisedWithAccuracy,
minimum,
minimumOptimised,
minimumOptimisedWithAccuracy,
chPolyBounds_forChPoly
) where

import MixedTypesNumPrelude hiding (maximum, minimum)

import AERN2.MP.Ball
import AERN2.Interval
import AERN2.Normalize

import AERN2.RealFun.Operations

import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.MaximumInt

chPolyBounds_forChPoly :: ChPoly MPBall -> ChPolyBounds MPBall
chPolyBounds_forChPoly f =
    ChPolyBounds fmin fmax
    where
    fmin = minimumOptimised fE (mpBall l) (mpBall r) 5 5
    fmax = maximumOptimised fE (mpBall l) (mpBall r) 5 5
    fE = f { chPoly_bounds = error "cycle in chPolyBounds_forChPoly" }
    (Interval l r) = chPoly_dom f

instance 
  -- (CanSetPrecision c, CanNormalize (ChPoly c)) => 
  CanSetAccuracyGuide (ChPoly MPBall) where
  setAccuracyGuide acGuide p = result
    where
    result = 
      setPrecisionAtLeastAccuracy (acGuide + (degree p)) $
        p { chPoly_acGuide = acGuide, chPoly_bounds = chPolyBounds_forChPoly result }

instance CanMinimiseOverDom (ChPoly MPBall) DyadicInterval where
  type MinimumOverDomType (ChPoly MPBall) DyadicInterval = MPBall
  minimumOverDom f dom@(Interval l r) 
    | chPoly_dom f == dom =
        chPolyBounds_min $ chPoly_bounds f
    | otherwise =
        minimumOptimised f (mpBall l) (mpBall r) 5 5
    {-res
    where
    (_, Just res) = last $ iterateUntilAccurate ac withPrec
    ac = getFiniteAccuracy f
    withPrec p =
      maybeTrace (printf "ChPoly: MinimumOverDomType: withPrec: p = %s; ac = %s"
        (show p) (show $ getAccuracy resP)) $
      Just resP
      where
      resP = minimumOptimised (setPrecision p f) (mpBall l) (mpBall r) 5 5-}

instance CanMaximiseOverDom (ChPoly MPBall) DyadicInterval where
  type MaximumOverDomType (ChPoly MPBall) DyadicInterval = MPBall
  maximumOverDom f dom@(Interval l r)
    | chPoly_dom f == dom =
        chPolyBounds_max $ chPoly_bounds f
    | otherwise =
    maximumOptimised f (mpBall l) (mpBall r) 5 5
    {-res
    where
    (_, Just res) = last $ iterateUntilAccurate ac withPrec
    ac = getFiniteAccuracy f
    withPrec p =
      maybeTrace (printf "ChPoly: MaximumOverDomType: withPrec: p = %s; ac = %s"
        (show p) (show $ getAccuracy resP)) $
      Just resP
      where
      resP = maximumOptimised (setPrecision p f) (mpBall l) (mpBall r) 5 5-}
