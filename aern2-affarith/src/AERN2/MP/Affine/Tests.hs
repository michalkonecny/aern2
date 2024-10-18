module AERN2.MP.Affine.Tests
  ( specMPAffine,
    tMPAffine,
  )
where

import AERN2.MP (ErrorBound, defaultPrecision, mpBall)
import AERN2.MP.Affine.Type (ErrorTermId (..), MPAffine (..), MPAffineConfig (..), mpAffNormalise)
import AERN2.MP.Affine.Ring ()
import AERN2.MP.Affine.Field ()
import AERN2.MP.Affine.Order ()
import AERN2.MP.Float (mpFloat)
import Data.Map qualified as Map
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
    --   specCanAbs tMPAffine
    --   specResultIsValid2 min "min" tMPAffine tMPAffine
    --   specResultIsValid2 max "max" tMPAffine tMPAffine
    --   specCanMinMaxNotMixed tMPAffine
    --   specCanMinMax tMPAffine tInteger tMPAffine
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

-- -- describe "elementary" $ do
-- --   specCanExpReal tMPAffine
-- --   specCanLogReal tMPAffine
-- --   specCanSqrtReal tMPAffine
-- --   specCanSinCosReal tMPAffine
