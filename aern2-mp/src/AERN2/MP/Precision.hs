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
     HasPrecision(..), CanSetPrecision(..),
     Precision, prec, prec2integer,
     defaultPrecision, maximumPrecision, standardPrecisions, precisionTimes2
    --  iterateUntilOKA, iterateUntilOK
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

prec2integer :: Precision -> Integer
prec2integer (Precision p) = p

prec :: Integer -> Precision
prec p
    | p < 2 = error errmsg
    | Precision p > maximumPrecision = error errmsg
      -- beware: if one removes "Precision" in the line above, it will type-check but loop
    | otherwise = Precision p
    where
    errmsg =
        "Precision must be between 2 and " ++ show maximumPrecision ++ " (given: p=" ++ show p ++ ")."

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

-- iterateUntilOKA ::
--     (ArrowChoice to) =>
--     (a -> Bool) ->
--     (Precision `to` a) ->
--     () `to` [(Precision, a)]
-- iterateUntilOKA isOK fnA =
--     stopWhenAccurate ps
--     where
-- --    fnWrap p =
-- --        unsafePerformIO $
-- --            catch (return $! Just $! fn p)
-- --                (\e -> let _ = e :: SomeException in return Nothing)
--     ps = standardPrecisions
--     stopWhenAccurate [] = arr $ const []
--     stopWhenAccurate (p : rest) =
--         proc () ->
--             do
--             result <- fnA -< p
--             if isOK result
--                 then returnA -< [(p, result)]
--                 else
--                     do
--                     restResults <- stopWhenAccurate rest -< ()
--                     returnA -<  (p, result) : restResults
--
-- iterateUntilOK ::
--     (a -> Bool) ->
--     (Precision -> a) ->
--     [(Precision, a)]
-- iterateUntilOK isOK fn = iterateUntilOKA isOK fn ()
