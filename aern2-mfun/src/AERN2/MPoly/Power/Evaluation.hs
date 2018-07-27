module AERN2.MPoly.Power.Evaluation where

import MixedTypesNumPrelude

import AERN2.MP.Ball
import AERN2.MP.Dyadic
import AERN2.Box
import AERN2.MPoly.Power.Type
import AERN2.MPoly.Type as MPoly
import qualified Data.Map as Map
import Data.List as List
import AERN2.MPoly.MultiIndex

import AERN2.Interval

import AERN2.Util.Vector (Vector, (!), (!.), vlength)
import qualified AERN2.Util.Vector as V

domBound :: PowMPoly MPBall -> MPBall
domBound p = bound p (doms p)

bound :: PowMPoly MPBall -> Box Dyadic -> MPBall
bound p b =
  List.foldl' (+) (mpBall 0) (map boundMonomial $ MPoly.toList $ poly p)
  where
  iBounds :: Interval Dyadic Dyadic -> MPBall
  iBounds (Interval l r) = max (abs $ mpBall l) (abs $ mpBall r)
  boundMonomial :: (MultiIndex, MPBall) -> MPBall
  boundMonomial (i, a) =
    (abs a)*(List.foldl' (*) (mpBall 1) $ V.imap (\j k -> ((iBounds (b ! j))^!k)) i)

eval :: PowMPoly MPBall -> Vector MPBall -> MPBall
eval f x =
  Map.foldl' (+) (mpBall 0) vs
  where
  ts = (MPoly.terms . poly) f
  vs = Map.mapWithKey (\k a -> a*(evalIndex k x)) ts

evalIndex :: MultiIndex -> Vector MPBall -> MPBall
evalIndex i x =
  List.foldl' (*) (mpBall 1) [ (x !. j)^!(i !. j)  | j <- [0 .. vlength i - 1 ]]
