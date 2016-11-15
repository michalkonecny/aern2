{-|
    Module      :  AERN2.Poly.Cheb.SineCosine
    Description :  Sine and cosine for polynomials
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Sine and cosine for polynomials
-}

module AERN2.Poly.Cheb.SineCosine
-- (
-- )
where

import Numeric.MixedTypes
import qualified Prelude as P
-- import Text.Printf

import qualified Data.Map as Map

-- import Test.Hspec
-- import Test.QuickCheck

-- import AERN2.MP.ErrorBound
import AERN2.MP.Ball
-- import AERN2.MP.Dyadic

-- import AERN2.Real

-- import AERN2.Interval
-- import AERN2.RealFun.Operations
-- import AERN2.RealFun.UnaryFun

import AERN2.Poly.Basics

import AERN2.Poly.Cheb.Type
import AERN2.Poly.Cheb.Ring ()

{-|
    For a given polynomial @p@, compute all partial Taylor sums of @sin(p)@ and return
    them together with @e@, an error bound on @[-1,1]@, and a number @n@.
    The number @n@ can be used to obtain a better error bound on a domain @[-a,a]@
    for some @0 <= a < 1@.  The better error bound is @e*a^n@.
-}
sineTaylorSeries ::
  (Ring c, CanDivBy c Integer, IsInterval c c, HasNorm c)
  =>
  Degree -> NormLog -> ChPoly c -> [(ChPoly c, Rational, Integer)]
sineTaylorSeries maxDeg sweepT x =
    let
    termComponents =
        iterate addNextTerm (0,1,1,6,Map.singleton 1 x)
        where
        addNextTerm (prevI, prevN, _prevFact, currentFact, prevPowers) =
            (i, n, currentFact, nextFact, newPowers)
            where
            i = prevI + 1
            n = prevN + 2
            nextFact = currentFact*((n+1)*(n+2))
            newPowers = Map.insert n (reduce currentPower) prevPowers
            reduce = reduceDegreeAndSweep maxDeg sweepT
            currentPower
                | odd i = x * (power i) * (power i)
                | otherwise = x * (power (i-1)) * (power (i+1))
                where
                power j = lookupForce j prevPowers
    sumsAndErrors =
        makeSums (chPoly (x,0), 1) termComponents
        where
        makeSums (prevSum, sign) ((_i, n, nFact, nextFact, xPowers) : rest) =
            (newSum, 1/nextFact, n+2) : makeSums (newSum, -sign) rest
            where
            newSum = prevSum + sign*xPowN/nFact
            xPowN = lookupForce n xPowers
        makeSums _ _ = error "internal error in SineCosine.sineTaylorSeries"
    in
    sumsAndErrors

{-|
    For a given polynomial @p@, compute all partial Taylor sums of @cos(p)@ and return
    them together with @e@, an error bound on @p\in[-1,1]@, and a number @n@.
    The number @n@ can be used to obtain an error bound for @p\in[-a,a]@
    for some @0 <= a@.  The error bound is @e*a^n@.
-}
cosineTaylorSeries ::
  (Ring c, CanDivBy c Integer, IsInterval c c, HasNorm c)
  =>
  Degree -> NormLog -> ChPoly c -> [(ChPoly c, Rational, Integer)]
cosineTaylorSeries maxDeg sweepT x =
    let
    termComponents =
        iterate addNextTerm (1,2,2,24,Map.singleton 2 (x*x))
        where
        addNextTerm (prevI, prevN, _prevFact, currentFact, prevPowers) =
            (i, n, currentFact, nextFact, newPowers)
            where
            i = prevI + 1
            n = prevN + 2
            nextFact = currentFact*((n+1)*(n+2))
            newPowers = Map.insert n (reduce currentPower) prevPowers
            reduce = reduceDegreeAndSweep maxDeg sweepT
            currentPower
                | even i = (power i) * (power i)
                | otherwise = (power (i-1)) * (power (i+1))
                where
                power j = lookupForce j prevPowers
    sumsAndErrors =
        makeSums (chPoly (x, 1), -1) termComponents
        where
        makeSums (prevSum, sign) ((_i, n, nFact, nextFact, xPowers) : rest) =
            (newSum, 1/nextFact, n+2) : makeSums (newSum, -sign) rest
            where
            newSum = prevSum + sign*xPowN/nFact
            xPowN = lookupForce n xPowers
        makeSums _ _ = error "internal error in SineCosine.cosineTaylorSeries"
    in
    sumsAndErrors

lookupForce :: P.Ord k => k -> Map.Map k a -> a
lookupForce j amap =
    case Map.lookup j amap of
        Just t -> t
        Nothing -> error "internal error in SineCosine.lookupForce"
