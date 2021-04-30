{-|
    Module      :  AERN2.Real
    Description :  Exact real numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Exact real numbers represented by Cauchy sequences of MPBalls.
-}
module AERN2.Real
(
   -- * real numbers and conversions
   CReal, 
   CSequence (..), 
   creal, HasCReals, CanBeCReal,
   cseqPrecisions, cseqIndexForPrecision,
   pi,
   -- * limits
   HasLimits(..),
   -- * lazy Kleeneans
   CKleenean, select,
   -- * complex numbers and conversions
   CComplex, ccomplex, HasCComplex, CanBeCComplex,
   -- * extracting approximations
   CanExtractApproximation(..), (?), realWithAccuracy
)
where

import AERN2.Real.Type
import AERN2.Real.Comparisons ()
import AERN2.Real.CKleenean ( CKleenean, select ) 
import AERN2.Real.Field ()
import AERN2.Real.Limit
import AERN2.Real.Elementary (pi)
-- import AERN2.Real.Tests ()

-- imports used in examples below:

-- import MixedTypesNumPrelude
-- -- import qualified Prelude as P

-- import Text.Printf
-- -- import AERN2.MP.Dyadic


-- {- parallel branching -}

-- _example_pif :: CauchyReal -> CauchyRealCN
-- _example_pif r =
--   if r < 0 then -r else r -- abs via parallel if

-- _example_pif0 :: MPBall -> CN MPBall
-- _example_pif0 r =
--   if r < 0 then -r else r -- abs via parallel if

-- _nsection ::
--   Integer ->
--   (Rational -> CauchyReal) ->
--   (Rational,Rational) ->
--   CauchyRealCN
-- _nsection n f (l,r) =
--   newSeqSimple (cn $ mpBall 0) $ withAccuracy
--   where
--   withAccuracy (me,_) ac@(AccuracySG _ acG) =
--     onSegment (l,r)
--     where
--     onSegment (a,b) =
--       let ab = mpBallP p ((a+b)/!2, (b-a)/!2) in
--       if getAccuracy ab >= ac
--         then cn ab
--         else pick me (map withMidpoint midpoints)
--       where
--       midpoints = [ (i*a + (n-i)*b)/!n | i <- [1..n-1] ]
--       withMidpoint :: Rational -> Sequence (Maybe (CN MPBall))
--       withMidpoint m = newSeqSimple Nothing withAC
--         where
--         withAC (meF, _) acF
--           | fa * fm !<! 0 = Just $ onSegment (a, m)
--           | fm * fb !<! 0 = Just $ onSegment (m, b)
--           | fa * fb !>=! 0 = Just $ err
--           | otherwise = Nothing
--           where
--           fa = ((f a) ?<- meF) acF
--           fm = ((f m) ?<- meF) acF
--           fb = ((f b) ?<- meF) acF
--       err :: CN MPBall
--       err =
--         noValueNumErrorCertainCN $
--           NumError $
--             printf "n-section: function does not have opposite signs on points %s %s" (show a) (show b)
--     p = prec $ fromAccuracy acG + 8
