{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module AERN2.Num.Precision
(
     HasPrecision(..), Precision, prec, prec2integer, 
     defaultPrecision, maximumPrecision, standardPrecisions, precisionTimes2, 
     PrecisionPolicyMode(..), PrecisionPolicy(..), defaultPrecisionPolicy, maxPrecisionPolicy,
     ppUseCurr, ppUseMax, ppKeepExact, 
     ArrowPrecisionPolicy(..), WithPrecisionPolicy(..), arrPP, 
) 
where

import AERN2.Num.Operations
import qualified Prelude as P

import AERN2.Num.IntegerRational ()

import Control.Category
import Control.Arrow

class HasPrecision t where
    getPrecision :: t -> Precision

newtype Precision = Precision Integer
    deriving (P.Eq, P.Ord, P.Show, P.Enum, P.Num, P.Real, P.Integral)

instance (ArrowChoice to) => HasEqA to Precision Precision
instance (ArrowChoice to) => HasOrderA to Precision Precision
instance (ArrowChoice to) => CanMinMaxA to Precision Precision

prec2integer :: Precision -> Integer
prec2integer (Precision p) = p

prec :: Integer -> Precision
prec p 
    | p < 2 = error errmsg  
    | Precision p > maximumPrecision = error errmsg
    | otherwise = Precision p
    where
    errmsg =
        "Precision must be between 2 and " ++ show maximumPrecision ++ " (given: p=" ++ show p ++ ")."

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
    
{- Precision policy -}

data PrecisionPolicy =
    PrecisionPolicy
    {
        precPolicy_precision :: Precision,
        precPolicy_mode :: PrecisionPolicyMode
    }

data PrecisionPolicyMode
    = PrecisionPolicyMode_UseMax
    | PrecisionPolicyMode_UseCurrent
    | PrecisionPolicyMode_KeepExactDyadic
    
defaultPrecisionPolicy :: PrecisionPolicy
defaultPrecisionPolicy =
    PrecisionPolicy defaultPrecision PrecisionPolicyMode_UseMax

ppUseCurr :: Precision -> PrecisionPolicy
ppUseCurr p =
    PrecisionPolicy p PrecisionPolicyMode_UseCurrent

ppUseMax :: Precision -> PrecisionPolicy
ppUseMax p =
    PrecisionPolicy p PrecisionPolicyMode_UseMax

ppKeepExact :: Precision -> PrecisionPolicy
ppKeepExact p =
    PrecisionPolicy p PrecisionPolicyMode_KeepExactDyadic

maxPrecisionPolicy :: PrecisionPolicy
maxPrecisionPolicy =
    PrecisionPolicy defaultPrecision PrecisionPolicyMode_KeepExactDyadic

-- TODO: generalise "ArrowPrecisionPolicy to" to "ArrowCurrentEffort e to" 
{-| A class of Arrows that can provide current precision. -}
class (ArrowChoice to) => ArrowPrecisionPolicy to where
    getPrecisionPolicy :: () `to` PrecisionPolicy
    
arrPP :: (ArrowPrecisionPolicy to) => (PrecisionPolicy -> a -> b) ->  (a `to` b)
arrPP fn =
    proc a ->
        do
        pp <- getPrecisionPolicy -< ()
        returnA -< fn pp a

instance ArrowPrecisionPolicy (->) where
    getPrecisionPolicy _ = defaultPrecisionPolicy
    
{-| Add a current precision to an arrow. -}
newtype WithPrecisionPolicy to a b = 
    WithPrecisionPolicy { runWithPrecisionPolicy :: (PrecisionPolicy ->  a `to` b) } 

instance (ArrowChoice to) => ArrowPrecisionPolicy (WithPrecisionPolicy to) where
    getPrecisionPolicy = WithPrecisionPolicy $ \ p -> proc () -> returnA -< p

instance (Category to) => Category (WithPrecisionPolicy to) where
     id = WithPrecisionPolicy $ const id
     (WithPrecisionPolicy f) . (WithPrecisionPolicy g) = WithPrecisionPolicy $  \ p -> (f p) . (g p) 

instance (Arrow to) => Arrow (WithPrecisionPolicy to) where
    arr f = WithPrecisionPolicy $ const $ arr f
    first (WithPrecisionPolicy f) = WithPrecisionPolicy $ \ p -> first (f p) 

instance (ArrowChoice to) => ArrowChoice (WithPrecisionPolicy to) where
    left (WithPrecisionPolicy f) = WithPrecisionPolicy $ \ p -> left (f p)

instance ArrowConvert a (WithPrecisionPolicy (->)) b a (WithPrecisionPolicy (->)) b where
    arrow2arrow = id


