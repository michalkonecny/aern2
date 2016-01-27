{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

module AERN2.Num.MPFloat 
    (MPFloat, Precision, prec, prec2integer, defaultPrecision, maximumPrecision, standardPrecisions, 
     getPrecision, setPrecisionUp,
     PrecisionPolicyMode(..), PrecisionPolicy(..), defaultPrecisionPolicy, maxPrecisionPolicy, 
     ArrowPrecisionPolicy(..), WithPrecisionPolicy(..), arrPP, 
     toRational, toDoubleUp, toDoubleDown,
     zero, one, two, fromIntegerUp, fromIntegerDown, fromRationalUp, fromRationalDown,
     neg, abs, addUp, addDown, subUp, subDown, 
     distUp, distDown, avgUp, avgDown, 
     mulUp, mulDown, divUp, divDown, recipUp, recipDown,
     piUp, piDown,
     cosUp, cosDown, sinUp, sinDown, sqrtUp, sqrtDown, expUp, expDown)
where

import AERN2.Num.Operations hiding (abs,neg,toRational)
import qualified Prelude as P

import Control.Category
import Control.Arrow

import AERN2.Num.IntegerRational ()

import qualified Data.Approximate.MPFRLowLevel as MPLow


type MPFloat = MPLow.Rounded

{- Precision type -}

newtype Precision = Precision Integer
    deriving (P.Eq, P.Ord, P.Show, P.Enum, P.Num, P.Real, P.Integral)

instance (ArrowChoice to) => HasEqA to Precision Precision
instance (ArrowChoice to) => HasOrderA to Precision Precision

prec2integer :: Precision -> Integer
prec2integer (Precision p) = p

setPrecisionUp :: Precision -> MPFloat -> MPFloat
setPrecisionUp (Precision p) = MPLow.set MPLow.Up (P.fromInteger p)

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

getPrecision :: MPFloat -> Precision
getPrecision x = Precision (P.toInteger $ MPLow.getPrec x)

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



{- conversions -}

toRational :: MPFloat -> Rational
toRational = MPLow.toRationalA

toDoubleUp :: MPFloat -> Double
toDoubleUp = MPLow.toDoubleA MPLow.Up
    
toDoubleDown :: MPFloat -> Double
toDoubleDown = MPLow.toDoubleA MPLow.Down
    
instance HasEq MPFloat MPFloat where

instance HasOrder MPFloat MPFloat where

{- constants -}

zero :: MPFloat
zero = MPLow.zero
    
one :: MPFloat
one = convert 1
    
two :: MPFloat
two = convert 2
    
fromIntegerUp :: Precision -> Integer -> MPFloat
fromIntegerUp p i = fromRationalUp p (P.fromInteger i)
    
fromIntegerDown :: Precision -> Integer -> MPFloat
fromIntegerDown p i = fromRationalDown p (P.fromInteger i)
    

instance ConvertibleA (->) Integer MPFloat where
    convertA n =
        findExact $ map upDown $ drop (int 4) standardPrecisions
        where
        upDown p = (fromIntegerDown p n, fromIntegerUp p n)
        findExact [] = 
            error $ "integer too high to represent exactly: " ++ show n
        findExact ((nDown, nUp) : rest)
            | nDown == nUp = nUp
            | otherwise = findExact rest

standardPrecisions :: [Precision]
standardPrecisions =
    map Precision $ aux 8 13
    where
    aux j j' 
        | Precision j <= maximumPrecision = j : (aux j' (j+j'))
        | otherwise = []
    
fromRationalUp :: Precision -> Rational -> MPFloat
fromRationalUp (Precision p) x =
    MPLow.fromRationalA MPLow.Up (P.fromInteger p) x
    
fromRationalDown :: Precision -> Rational -> MPFloat
fromRationalDown (Precision p) x =
    MPLow.fromRationalA MPLow.Down (P.fromInteger p) x

-- | Computes an upper bound to the distance @|x - y|@ of @x@ and @y@.
distUp :: PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat
distUp pp x y = if x >= y then subUp pp x y else subUp pp y x

-- | Computes a lower bound to the distance @|x - y|@ of @x@ and @y@.
distDown :: PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat
distDown pp x y = if x >= y then subDown pp x y else subDown pp y x
    
avgUp :: PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat
avgUp pp x y = divUp pp (addUp pp x y) two

avgDown :: PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat
avgDown pp x y = divDown pp two (addDown pp x y)

{- common functions -}

neg :: MPFloat -> MPFloat
neg = unaryUp MPLow.neg

abs :: MPLow.Rounded -> MPFloat
abs x 
    | x < MPLow.zero = neg x
    | otherwise = x

addUp, addDown :: PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat
addUp = binaryUp True MPLow.add
addDown = binaryDown True MPLow.add

subUp, subDown :: PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat
subUp = binaryUp True MPLow.sub
subDown = binaryDown True MPLow.sub

mulUp, mulDown :: PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat
mulUp = binaryUp True MPLow.mul
mulDown = binaryDown True MPLow.mul

divUp,divDown :: PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat
divUp = binaryUp False MPLow.div
divDown = binaryDown False MPLow.div

recipUp :: PrecisionPolicy -> MPFloat -> MPFloat
recipUp pp x = divUp pp one x

recipDown :: PrecisionPolicy -> MPFloat -> MPFloat
recipDown pp x = divDown pp one x

{- special constants and functions -}

piUp :: Precision -> MPFloat
piUp (Precision p) =
    MPLow.pi MPLow.Up (P.fromInteger p)
    
piDown :: Precision -> MPFloat
piDown (Precision p) =
    MPLow.pi MPLow.Down (P.fromInteger p)
    
cosUp :: MPFloat -> MPFloat
cosUp = unaryUp MPLow.cos

cosDown :: MPFloat -> MPFloat
cosDown = unaryDown MPLow.cos

sinUp :: MPFloat -> MPFloat
sinUp = unaryUp MPLow.sin

sinDown :: MPFloat -> MPFloat
sinDown = unaryDown MPLow.sin

sqrtUp :: MPFloat -> MPFloat
sqrtUp = unaryUp MPLow.sqrt

sqrtDown :: MPFloat -> MPFloat
sqrtDown = unaryDown MPLow.sqrt

expUp :: MPFloat -> MPFloat
expUp = unaryUp MPLow.exp

expDown :: MPFloat -> MPFloat
expDown = unaryDown MPLow.exp

{- auxiliary functions to automatically determine result precision from operand precisions -}

unaryUp :: 
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat)
unaryUp opRP x = opRP MPLow.Up p x
    where
    p = MPLow.getPrec x

unaryDown :: 
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat) ->
    (MPFloat -> MPFloat)
unaryDown opRP x = opRP MPLow.Down p x
    where
    p = MPLow.getPrec x

binaryUp :: 
    Bool ->
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat -> MPFloat) ->
    (PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat)
binaryUp = binaryApprox True

binaryDown :: 
    Bool ->
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat -> MPFloat) ->
    (PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat)
binaryDown = binaryApprox False
    
binaryApprox :: 
    Bool -> Bool ->
    (MPLow.RoundMode -> MPLow.Precision -> MPFloat -> MPFloat -> MPFloat) ->
    (PrecisionPolicy -> MPFloat -> MPFloat -> MPFloat)
binaryApprox isUp canBeExact opRP pp x y = 
    case precPolicy_mode pp of
        PrecisionPolicyMode_UseCurrent ->
            withPrec pCurr
        PrecisionPolicyMode_KeepExactDyadic | canBeExact ->
            getExact pMax
        _ ->
            withPrec pMax
    where
    pMax = pCurr `P.max` (getPrecision x) `P.max` (getPrecision y)
    pCurr = precPolicy_precision pp
    withPrec (Precision p)
        | isUp = opRP MPLow.Up (P.fromInteger p) x y
        | otherwise = opRP MPLow.Down (P.fromInteger p) x y
    getExact (Precision p)
        | rUp P.== rDown = rUp
        | otherwise = getExact (Precision $ 2*p)
        where
        rUp = opRP MPLow.Up (P.fromInteger p) x y
        rDown = opRP MPLow.Down (P.fromInteger p) x y
        
        
    