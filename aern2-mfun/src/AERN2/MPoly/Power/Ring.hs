module AERN2.MPoly.Power.Ring where

import MixedTypesNumPrelude

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List as List
import AERN2.Util.Vector (Vector, (!))
import qualified AERN2.Util.Vector as V

import AERN2.MPoly.Type (MPoly(..))
import AERN2.MPoly.MultiIndex
import qualified AERN2.MPoly.Type as MPoly
import AERN2.Interval
import AERN2.MP.Accuracy
import AERN2.MP.Ball

import AERN2.MPoly.Power.Type

import Debug.Trace

instance (MPolyCoeff a) => CanAddAsymmetric (PowMPoly a) (PowMPoly a) where
  type AddType (PowMPoly a) (PowMPoly a) = PowMPoly a
  add p q =
    sweepAndNormalise $ addSameDim $ liftDimensions p q
    where
    addSameDim ((PowMPoly ds p0 acG0), (PowMPoly _ p1 acG1)) =
      PowMPoly ds (p0 + p1) (max acG0 acG1)

instance (MPolyCoeff a) => CanSub (PowMPoly a) (PowMPoly a) where
  type SubType (PowMPoly a) (PowMPoly a) = PowMPoly a
  sub p q =
    sweepAndNormalise $ subSameDim $ liftDimensions p q
    where
    subSameDim ((PowMPoly ds p0 acG0), (PowMPoly _ p1 acG1)) =
      PowMPoly ds (p0 - p1) (max acG0 acG1)

instance CanMulAsymmetric MPBall (PowMPoly MPBall) where
  type MulType MPBall (PowMPoly MPBall) = PowMPoly MPBall
  mul a p =
    sweepAndNormalise $ updateTerms (*a) p

instance (MPolyCoeff a) => CanMulAsymmetric (PowMPoly a) (PowMPoly a) where
  type MulType (PowMPoly a) (PowMPoly a) = PowMPoly a
  mul p q =
    sweepAndNormalise $ mulSameDim $ liftDimensions p q
    where
    mulSameDim ((PowMPoly ds p0 acG0), (PowMPoly _ p1 acG1)) =
      PowMPoly ds (prod p0 p1) (max acG0 acG1)
    prod (MPoly d ts0) (MPoly _ ts1) =
      List.foldl' (+) (MPoly d Map.empty) productPolys
      where
      monomialProducts =
        map
          (\mon -> map (mulMonomial mon) (Map.toList ts1))
          (Map.toList ts0)
      productPolys = map (MPoly.fromList) monomialProducts
    mulMonomial :: (MultiIndex, a) -> (MultiIndex, a) -> (MultiIndex, a)
    mulMonomial (i, a) (j, b) =
      (V.generate (V.length i) (\k -> i!k + j!k), a * b)

instance CanDiv (PowMPoly MPBall) Integer where
  type DivType (PowMPoly MPBall) Integer = (PowMPoly MPBall) -- TODO? introduce errors
  type DivTypeNoCN (PowMPoly MPBall) Integer = PowMPoly MPBall
  divideNoCN p n = (divide p n)
  divide p n =
    updateTerms (/! n) p
