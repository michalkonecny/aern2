module AERN2.Frac.Type where

import Numeric.MixedTypes
import AERN2.MP
import AERN2.Normalize
import AERN2.Poly.Cheb as Cheb

import AERN2.Interval
import AERN2.RealFun.Operations

data Frac a =
  Frac {
      frac_numerator    :: ChPoly a
    , frac_denominator  :: ChPoly a
    , frac_dIM          :: a  -- upper bound on 1/Q(x) on domain
  }

instance Show (ChPoly a) => (Show (Frac a)) where
  show (Frac p q _) =
    (show p) ++ "\n / \n"++(show q)

degree :: Frac a -> Integer
degree (Frac p q _) = Cheb.degree p + Cheb.degree q

instance (CanNormalize (ChPoly a)) => CanNormalize (Frac a) where
  normalize (Frac p q m) = Frac (normalize p) (normalize q) m

instance (HasAccuracy a,  HasIntegers a, IsBall a) => HasAccuracy (Frac a) where
  getAccuracy (Frac p q _) = min (getAccuracy p) (getAccuracy q)

instance (HasPrecision a) => HasPrecision (Frac a) where
  getPrecision (Frac p q m) =
    max (getPrecision m) $ max (getPrecision p) (getPrecision q)

instance (CanSetPrecision a, CanNormalize (ChPoly a)) => CanSetPrecision (Frac a) where
  setPrecision prc (Frac p q m) =
    Frac (setPrecision prc p) (setPrecision prc q) (setPrecision prc m)

instance (HasDomain (Frac a)) where
  type Domain (Frac a) = DyadicInterval
  getDomain (Frac p _ _) = getDomain p

fracLift1 :: (HasIntegers a) => (Frac a -> b) -> ChPoly a -> b
fracLift1 f = f . fromPoly

fracLift2 :: (HasIntegers a)
  => (Frac a -> Frac a -> b) -> (ChPoly a -> ChPoly a -> b)
fracLift2 f p q = f (fromPoly p) (fromPoly q)

fromPoly :: (HasIntegers a) => ChPoly a -> Frac a
fromPoly chp@(ChPoly dom _ _) =
  Frac chp one (convertExactly 1)
  where
  one = chPoly (dom, 1)
