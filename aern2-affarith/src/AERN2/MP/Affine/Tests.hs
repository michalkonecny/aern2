-- |
--    Module      :  AERN2.MP.Affine.Tests
--    Description :  Tests for arbitrary precision affine arithmetic
--    Copyright   :  (c) Michal Konecny
--    License     :  BSD3
--
--    Maintainer  :  mikkonecny@gmail.com
--    Stability   :  experimental
--    Portability :  portable
--
--    Tests for arbitrary precision affine arithmetic
--
--    To run the tests using stack, execute:
--
--    @
--    stack test aern2-affarith --test-arguments "-a 1000"
--    @
module AERN2.MP.Affine.Tests
  ( specMPAffine,
    tMPAffine,
  )
where

import AERN2.MP (ErrorBound, MPBall (MPBall), defaultPrecision, errorBound, mpBall, mpBallP, raisePrecisionIfBelow, setPrecision)
import AERN2.MP.Dyadic (dyadic)
import AERN2.MP.Affine.Exp ()
import AERN2.MP.Affine.Field ()
import AERN2.MP.Affine.Order ()
import AERN2.MP.Affine.Ring ()
import AERN2.MP.Affine.Sqrt ()
import AERN2.MP.Affine.SinCos ()
import AERN2.MP.Affine.Type (ErrorTermId (..), MPAffine (..), MPAffineConfig (..), mpAffNormalise)
import AERN2.MP.Float (mpFloat)
import Data.Map qualified as Map
import GHC.Records
import MixedTypesNumPrelude
import Test.Hspec
import Test.QuickCheck

instance Arbitrary MPAffineConfig where
  arbitrary = do
    maxTerms <- int <$> choose (1, 5)
    let precision = integer defaultPrecision
    pure $ MPAffineConfig {precision, maxTerms}

errVars :: [ErrorTermId]
errVars = map (ErrorTermId . int) [101 .. 105]

instance Arbitrary MPAffine where
  arbitrary =
    do
      config <- arbitrary
      centre <- finiteMPFloat
      vars <- sublistOf errVars
      coeffsEB <- mapM (const smallEB) vars
      let coeffs = map mpFloat coeffsEB
      let errTerms = Map.fromList (zip vars coeffs)
      pure $ mpAffNormalise $ MPAffine {config, errTerms, centre}
    where
      smallEB =
        do
          e <- arbitrary :: Gen ErrorBound
          if mpBall e !<! 100
            then pure (0.01 * e) -- e < 1
            else smallEB
      finiteMPFloat =
        do
          x <- arbitrary
          if isFinite x
            then return x
            else finiteMPFloat

-- |
--  A runtime representative of type @MPAffine@.
--  Used for specialising polymorphic tests to concrete types.
tMPAffine :: T MPAffine
tMPAffine = T "MPAffine"

specMPAffine :: Spec
specMPAffine =
  describe "MPAffine" $ do
    describe "order" $ do
      specHasEqNotMixed tMPAffine
      specHasEq tInt tMPAffine tRational
      specCanTestZero tMPAffine
      specHasOrderNotMixed tMPAffine
      specHasOrder tInt tMPAffine tRational
    describe "min/max/abs" $ do
      specCanNegNum tMPAffine
    --   specResultIsValid1 abs "abs" tMPAffine
      specCanAbs tMPAffine
    --   specResultIsValid2 min "min" tMPAffine tMPAffine
    --   specResultIsValid2 max "max" tMPAffine tMPAffine
      specCanMinMaxNotMixed tMPAffine
      specCanMinMax tMPAffine tInteger tMPAffine
    describe "ring" $ do
      -- specResultIsValid2 add "add" tMPAffine tMPAffine
      specCanAddNotMixed tMPAffine
      specCanAddSameType tMPAffine
      specCanAdd tInt tMPAffine tRational
      specCanAdd tInteger tMPAffine tInt
      --   specResultIsValid2 sub "sub" tMPAffine tMPAffine
      specCanSubNotMixed tMPAffine
      specCanSub tMPAffine tInteger
      specCanSub tInteger tMPAffine
      specCanSub tMPAffine tInt
      specCanSub tInt tMPAffine
      --   specResultIsValid2 mul "mul" tMPAffine tMPAffine
      specCanMulNotMixed tMPAffine
      specCanMulSameType tMPAffine
      specCanMul tInt tMPAffine tRational
    -- specCanPow tMPAffine tInteger
    describe "field" $ do
      --   specResultIsValid2Pre (\_ y -> isCertainlyNonZero y) divide "divide" tMPAffine tMPAffine
      specCanDivNotMixed tMPAffine
      specCanDiv tInteger tMPAffine
      specCanDiv tMPAffine tInt
      specCanDiv tMPAffine tRational

    describe "elementary" $ do
      specCanExpReal tMPAffine
      -- specCanLogReal tMPAffine
      specCanSqrtReal tMPAffine
      specCanSinCosReal tMPAffine
      specSinCosIndependentErrors

{-|
  Affine form with the given dyadic centre and a single error term, at the default precision.
-}
affWithOneTerm :: Rational -> Rational -> Integer -> MPAffine
affWithOneTerm c r var =
  setPrecision defaultPrecision $
    MPAffine
      { config = MPAffineConfig {maxTerms = int 5, precision = integer defaultPrecision},
        centre = mpFloat (dyadic c),
        errTerms = Map.singleton (ErrorTermId (int var)) (mpFloat (dyadic r))
      }

{-|
  Evaluate an affine form at a point, given by the value (-1, 0 or 1) of each error variable,
  using at least the default precision.
-}
evalAffAt :: (ErrorTermId -> Integer) -> MPAffine -> MPBall
evalAffAt eps aff =
  foldl (+) (exact aff.centre) [exact coeff * eps var | (var, coeff) <- Map.toList aff.errTerms]
  where
    exact c = raisePrecisionIfBelow defaultPrecision (MPBall c (errorBound 0))

{-|
  Results of sin/cos on arguments with identical ranges but independent errors
  must not share error variables, even when computed by falling back on MPBall.
-}
specSinCosIndependentErrors :: Spec
specSinCosIndependentErrors =
  describe "sin/cos with independent arguments of equal range" $ do
    it "sin x - sin y contains sin(1) - sin(3/2) for x, y in 3/2 +- 1/2 (non-monotone)" $ do
      let x = affWithOneTerm 1.5 0.5 1
      let y = affWithOneTerm 1.5 0.5 2
      (mpBall (sin x - sin y) ?==? (sin (mpBallP defaultPrecision 1) - sin (mpBallP defaultPrecision 1.5))) `shouldBe` True
    it "cos x - cos y contains cos(1/2) - 1 for x, y in 0 +- 1/2 (non-monotone)" $ do
      let x = affWithOneTerm 0.0 0.5 1
      let y = affWithOneTerm 0.0 0.5 2
      (mpBall (cos x - cos y) ?==? (cos (mpBallP defaultPrecision 0.5) - 1)) `shouldBe` True
    it "sin x - sin x is exactly 0 for x in 3/2 +- 1/2 (non-monotone)" $ do
      let x = affWithOneTerm 1.5 0.5 1
      (mpBall (sin x - sin x) !==! 0) `shouldBe` True
    it "f x - f y contains f(x(1)) - f(y(0)) for f in {sin, cos}, x, y independent with equal ranges" $ do
      property $ \(x0 :: MPAffine) ->
        forAll (choose (-40, 40)) $ \(n :: Integer) ->
          let -- recentre to n/8, covering monotone and non-monotone regions
              x = x0 {centre = mpFloat (dyadic (n / 8))}
              -- same coefficients, disjoint error variables
              y = x {errTerms = Map.mapKeys (\(ErrorTermId i) -> ErrorTermId (int (i + 100))) x.errTerms}
              ok fAff fBall = mpBall (fAff x - fAff y) ?==? (fBall (evalAffAt (const 1) x) - fBall (evalAffAt (const 0) y))
           in ok sin sin && ok cos cos
