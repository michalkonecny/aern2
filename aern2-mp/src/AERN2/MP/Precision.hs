{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
    Module      :  AERN2.MP.Precision
    Description :  number of significant binary digits
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Precision type and its operations.  Precision expresses a limit on the size
    of an approximation, roughly corresponding to the number of significant bits for a floating-point number and its generalisations to other types.
-}
module AERN2.MP.Precision
(
     Precision, prec
     , HasPrecision(..), CanSetPrecision(..), lowerPrecisionIfAbove, raisePrecisionIfBelow, specCanSetPrecision
     , defaultPrecision, maximumPrecision, standardPrecisions, precisionTimes2
     , iterateUntilOK
     , ConvertibleWithPrecision(..), convertP
     , convertPFirst, convertPSecond
)
where

import MixedTypesNumPrelude
import qualified Prelude as P
import Text.Printf

import Control.CollectErrors (CollectErrors(..), CanBeErrors)
-- import qualified Control.CollectErrors as CE

import Data.Complex

import Data.Typeable

import Test.Hspec
import Test.QuickCheck

newtype Precision = Precision Integer
    deriving (P.Eq, P.Ord, P.Show, P.Enum, P.Num, P.Real, P.Integral, Typeable)

instance HasEqAsymmetric Precision Precision
instance HasOrderAsymmetric Precision Precision
instance CanMinMaxAsymmetric Precision Precision

instance ConvertibleExactly Precision Integer where
  safeConvertExactly (Precision p) = Right p

instance ConvertibleExactly Integer Precision where
  safeConvertExactly p
    | p < 2 = convError errmsg p
    | Precision p > maximumPrecision = convError errmsg p
      -- beware: if one removes "Precision" in the line above, it will type-check but loop
    | otherwise = Right $ Precision p
    where
    errmsg =
        "Precision must be between 2 and " ++ show maximumPrecision ++ " (given: p=" ++ show p ++ ")."

prec :: Integer -> Precision
prec = convertExactly

instance HasEqAsymmetric Precision Integer where
  equalTo p i = equalTo p (prec i)
instance HasEqAsymmetric Integer Precision where
  equalTo i p = equalTo (prec i) p
instance HasOrderAsymmetric Precision Integer where
  lessThan p i = lessThan p (prec i)
  leq p i = leq p (prec i)
instance HasOrderAsymmetric Integer Precision where
  lessThan i p = lessThan (prec i) p
  leq i p = leq (prec i) p

instance HasEqAsymmetric Precision Int where
  equalTo p i = equalTo p (prec (integer i))
instance HasEqAsymmetric Int Precision where
  equalTo i p = equalTo (prec (integer i)) p
instance HasOrderAsymmetric Precision Int where
  lessThan p i = lessThan p (prec (integer i))
  leq p i = leq p (prec (integer i))
instance HasOrderAsymmetric Int Precision where
  lessThan i p = lessThan (prec (integer i)) p
  leq i p = leq (prec (integer i)) p

instance CanAddAsymmetric Precision Precision
instance CanAddAsymmetric Integer Precision where
  type AddType Integer Precision = Precision
  add n (Precision p) = prec (n + p)
instance CanAddAsymmetric Precision Integer where
  type AddType Precision Integer = Precision
  add (Precision p) n = prec (n + p)

instance CanMulAsymmetric Precision Precision
instance CanMulAsymmetric Integer Precision where
  type MulType Integer Precision = Precision
  mul n (Precision p) = prec (n * p)
instance CanMulAsymmetric Precision Integer where
  type MulType Precision Integer = Precision
  mul (Precision p) n = prec (n * p)

class HasPrecision t where
    getPrecision :: t -> Precision

class (HasPrecision t) => CanSetPrecision t where
    setPrecision :: Precision -> t -> t

instance HasPrecision t => HasPrecision (Complex t) where
  getPrecision (a :+ i) =
    (getPrecision a) `min` (getPrecision i)

instance CanSetPrecision t => CanSetPrecision (Complex t) where
  setPrecision p (a :+ i) =
    (setPrecision p a) :+ (setPrecision p i)

instance HasPrecision t => HasPrecision (Maybe t) where
  getPrecision (Just v) = getPrecision v
  getPrecision Nothing = defaultPrecision
instance CanSetPrecision t => CanSetPrecision (Maybe t) where
  setPrecision p = fmap (setPrecision p)

instance HasPrecision Bool where
  getPrecision _ = defaultPrecision
instance CanSetPrecision Bool where
  setPrecision _ = id

instance HasPrecision t => HasPrecision (CollectErrors es t) where
  getPrecision vCE =
    case getMaybeValue vCE of
      Just v -> getPrecision v
      _ -> defaultPrecision
instance CanSetPrecision t => CanSetPrecision (CollectErrors es t) where
  setPrecision p = fmap (setPrecision p)

lowerPrecisionIfAbove :: (CanSetPrecision t) => Precision -> t -> t
lowerPrecisionIfAbove p x
  | getPrecision x > p = setPrecision p x
  | otherwise = x

raisePrecisionIfBelow :: (CanSetPrecision t) => Precision -> t -> t
raisePrecisionIfBelow p x
  | getPrecision x < p = setPrecision p x
  | otherwise = x

specCanSetPrecision ::
  (CanSetPrecision t, CanTestFinite t, Arbitrary t, Show t, Testable prop)
  =>
  (T t) -> (t -> t -> prop) -> Spec
specCanSetPrecision (T typeName :: T t) check =
  describe (printf "CanSetPrecision %s" typeName) $ do
    it "set then get" $ do
      property $ \ (x :: t) (p :: Precision) ->
        isFinite x ==>
        let xP = setPrecision p x in
          p == getPrecision xP
    it "setPrecision x ~ x" $ do
      property $ \ (x :: t) (p :: Precision) ->
        let xP = setPrecision p x in
          check xP x

maximumPrecision :: Precision
maximumPrecision = Precision 5000000

defaultPrecision :: Precision
defaultPrecision = Precision 100

standardPrecisions :: Precision -> [Precision]
standardPrecisions (Precision initPrec0) =
    map (Precision . (+ initPrec)) $ aux 0 (max 2 (initPrec `P.div` 16))
    where
    initPrec = max 2 initPrec0
    aux j j'
        | Precision j <= maximumPrecision = j : (aux j' (j+j'))
        | otherwise = []

precisionTimes2 :: Precision -> Precision
precisionTimes2 (Precision p) = Precision (2*p)

iterateUntilOK ::
    Precision ->
    (a -> Bool) ->
    (Precision -> a) ->
    [(Precision, a)]
iterateUntilOK initPrec isOK fn =
    stopWhenAccurate ps
    where
--    fnWrap p =
--        unsafePerformIO $
--            catch (return $! Just $! fn p)
--                (\e -> let _ = e :: SomeException in return Nothing)
    ps = standardPrecisions initPrec
    stopWhenAccurate [] = []
    stopWhenAccurate (p : rest)
      | isOK result = [(p, result)]
      | otherwise = (p, result) : stopWhenAccurate rest
      where
      result = fn p

class ConvertibleWithPrecision t1 t2 where
  safeConvertP :: Precision -> t1 -> ConvertResult t2

convertP :: (ConvertibleWithPrecision t1 t2) => Precision -> t1 -> t2
convertP p a =
  case safeConvertP p a of
    Right v -> v
    Left err -> error (show err)

convertPFirst ::
  (ConvertibleWithPrecision t1 t2, HasPrecision t2) =>
  (t2 -> t2 -> c) -> (t1 -> t2 -> c)
convertPFirst = convertFirstUsing (\ q b -> convertP (getPrecision b) q)

convertPSecond ::
  (ConvertibleWithPrecision t2 t1, HasPrecision t1) =>
  (t1 -> t1 -> c) -> (t1 -> t2 -> c)
convertPSecond = convertSecondUsing (\ b q -> convertP (getPrecision b) q)


instance Arbitrary Precision where
  arbitrary =
    sized $ \size -> choose (4*(size+1),10*(size+1)) >>= return . prec

$(declForTypes
  [[t| Bool |], [t| Integer |], [t| Int |], [t| Rational |], [t| Double |]]
  (\ t -> [d|

    instance (ConvertibleWithPrecision $t t, CanBeErrors es) => ConvertibleWithPrecision $t (CollectErrors es t) where
      safeConvertP p = fmap pure . safeConvertP p
  |]))
