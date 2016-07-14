{-|
    Module      :  AERN2.MP.Precision
    Description :  Floating-point precision
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Floating-point precision type and its operations
-}
module AERN2.MP.Precision
(
     HasPrecision(..), CanSetPrecision(..)
     , Precision, prec
     , defaultPrecision, maximumPrecision, standardPrecisions, precisionTimes2
     , iterateUntilOK
     , ConvertWithPrecision(..), convertP
     , convertPFirst, convertPSecond
)
where

import Numeric.MixedTypes
import qualified Prelude as P

class HasPrecision t where
    getPrecision :: t -> Precision

class CanSetPrecision t where
    setPrecision :: Precision -> t -> t

newtype Precision = Precision Integer
    deriving (P.Eq, P.Ord, P.Show, P.Enum, P.Num, P.Real, P.Integral)

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

maximumPrecision :: Precision
maximumPrecision = Precision 1000000

defaultPrecision :: Precision
defaultPrecision = Precision 100

standardPrecisions :: [Precision]
standardPrecisions =
    map Precision $ aux 8 13
    where
    aux j j'
        | Precision j <= maximumPrecision = j : (aux j' (j+j'))
        | otherwise = []

precisionTimes2 :: Precision -> Precision
precisionTimes2 (Precision p) = Precision (2*p)

iterateUntilOK ::
    (a -> Bool) ->
    (Precision -> a) ->
    [(Precision, a)]
iterateUntilOK isOK fn =
    stopWhenAccurate ps
    where
--    fnWrap p =
--        unsafePerformIO $
--            catch (return $! Just $! fn p)
--                (\e -> let _ = e :: SomeException in return Nothing)
    ps = standardPrecisions
    stopWhenAccurate [] = []
    stopWhenAccurate (p : rest)
      | isOK result = [(p, result)]
      | otherwise = (p, result) : stopWhenAccurate rest
      where
      result = fn p

class ConvertWithPrecision t1 t2 where
  safeConvertP :: Precision -> t1 -> ConvertResult t2

convertP :: (ConvertWithPrecision t1 t2) => Precision -> t1 -> t2
convertP p a =
  case safeConvertP p a of
    Right v -> v
    Left err -> error (show err)

convertPFirst ::
  (ConvertWithPrecision t1 t2, HasPrecision t2) =>
  (t2 -> t2 -> c) -> (t1 -> t2 -> c)
convertPFirst = convertFirstUsing (\ q b -> convertP (getPrecision b) q)

convertPSecond ::
  (ConvertWithPrecision t2 t1, HasPrecision t1) =>
  (t1 -> t1 -> c) -> (t1 -> t2 -> c)
convertPSecond = convertSecondUsing (\ b q -> convertP (getPrecision b) q)
