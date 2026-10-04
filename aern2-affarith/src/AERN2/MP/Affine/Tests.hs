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

import AERN2.MP (ErrorBound, MPBall (MPBall), contains, defaultPrecision, errorBound, mpBall, mpBallP, prec, raisePrecisionIfBelow, setPrecision)
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
      specRecipIndependentErrors
      specRecipInterceptErrors

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
  Results of recip on arguments with identical ranges but independent errors
  must not share error variables, even when computed by falling back on MPBall.
  This fallback is used for very small or very large arguments.
-}
specRecipIndependentErrors :: Spec
specRecipIndependentErrors =
  describe "recip with independent arguments of equal range" $ do
    it "1/x - 1/y contains 2^1000 for x, y in 2^(-1000) +- 2^(-1001)" $ do
      let x = affWithOneTerm (1 / (rational (2 ^ 1000))) (1 / (rational (2 ^ 1001))) 1
      let y = affWithOneTerm (1 / (rational (2 ^ 1000))) (1 / (rational (2 ^ 1001))) 2
      (mpBall (recip x - recip y) ?==? (mpBallP defaultPrecision (2 ^ 1000))) `shouldBe` True
    it "1/x - 1/x is exactly 0 for x in 2^(-1000) +- 2^(-1001)" $ do
      let x = affWithOneTerm (1 / (rational (2 ^ 1000))) (1 / (rational (2 ^ 1001))) 1
      (mpBall (recip x - recip x) !==! 0) `shouldBe` True
    it "1/x - 1/y contains 1/x(-1) - 1/y(0) for x, y independent in s*2^k +- m*2^(k-3)" $ do
      property $
        forAll (choose (-1500, 1500)) $ \(k :: Integer) ->
          forAll (elements [1, -1]) $ \(s :: Integer) ->
            forAll (choose (1, 7)) $ \(m :: Integer) ->
              let c = s * (rational 2) ^ k
                  r = m * (rational 2) ^ (k - 3)
                  x = affWithOneTerm c r 1
                  y = affWithOneTerm c r 2
               in mpBall (recip x - recip y) ?==? (recip (evalAffAt (const (-1)) x) - recip (evalAffAt (const 0) y))

{-|
  Cancelling the secant slope leaves only the nonlinear reciprocal error.
  Independent arguments must retain independent errors even when their ranges
  and intercept enclosures coincide.  Checking the whole range of 1/x - 1/y
  alone can miss an invalid correlation with the original input variables.
-}
specRecipInterceptErrors :: Spec
specRecipInterceptErrors =
  describe "recip nonlinear error provenance" $ do
    it "retains independent intercept errors for positive arguments" $ do
      let x = affWithOneTerm 1.5 0.5 1
      let y = affWithOneTerm 1.5 0.5 2
      -- At x = 3/2, y = 1 this expression is -1/12, not zero.
      (mpBall (recip x + x / 2 - (recip y + y / 2)) `contains` (rational (-1) / 12)) `shouldBe` True
    it "retains independent intercept errors for negative arguments" $ do
      let x = affWithOneTerm (-1.5) 0.5 1
      let y = affWithOneTerm (-1.5) 0.5 2
      (mpBall (recip x + x / 2 - (recip y + y / 2)) `contains` (rational 1 / 12)) `shouldBe` True
    it "preserves cancellation of repeated positive and negative reciprocals" $ do
      let cancels c =
            let x = affWithOneTerm c 0.5 1
             in mpBall (recip x - recip x) !==! 0
      all cancels [1.5, -1.5] `shouldBe` True
    it "encloses rounded reciprocals of exact arguments and preserves reuse" $ do
      let encloses c =
            let x = affWithOneTerm c 0.0 1
             in (mpBall (recip x) `contains` recip c) && (mpBall (recip x - recip x) !==! 0)
      all encloses [3.0, -3.0, 4.0, -4.0] `shouldBe` True
    it "encloses compensated reciprocals across precisions, scales and term limits" $ do
      property $
        forAll (elements [8, 20, 53]) $ \(p :: Integer) ->
          forAll (elements (map int [1, 2, 5])) $ \(maxTerms :: Int) ->
            forAll (choose (-20, 20)) $ \(k :: Integer) ->
              forAll (elements [1, -1]) $ \(s :: Integer) ->
                forAll (choose (4, 8)) $ \(i :: Integer) ->
                  forAll (choose (4, 8)) $ \(j :: Integer) ->
                    let magnitude = (rational 2) ^ k
                        input var = setPrecision (prec p) $
                          let aff = affWithOneTerm (s * 1.5 * magnitude) (0.5 * magnitude) var
                           in aff {config = aff.config {maxTerms}}
                        x = input 1
                        y = input 2
                        compensate t = recip t + t / (2 * magnitude * magnitude)
                        value n =
                          let q = s * n * magnitude / 4
                           in recip q + q / (2 * magnitude * magnitude)
                     in mpBall (compensate x - compensate y) `contains` (value i - value j)

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
