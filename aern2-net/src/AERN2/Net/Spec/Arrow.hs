{-# LANGUAGE Arrows, TypeOperators, GeneralizedNewtypeDeriving #-}

module AERN2.Net.Spec.Arrow where

import AERN2.Real hiding (id, (.))
import Data.String (IsString(..),fromString)

--import Control.Category
import Control.Arrow
import qualified Data.Map as Map


{- mini examples -}

-- | sqrt(pi) + pi
_anet0 :: (ArrowReal to r) => () `to` r
_anet0 =
    proc _ -> do
        p <- piA -< ()
        sp <- sqrtA -< p
        psp <- addA -< (p,sp)
        returnA -< psp

-- | pi * sqrt(x) * x
_anet1 :: (ArrowReal to r) => r `to` r
_anet1 =
    proc x -> do
        p <- piA -< ()
        sx <- sqrtA -< x
        psx <- mulA -< (p,sx)
        psxx <- mulA -< (psx,x)
        returnA -< psxx

-- | sqrt(x^2+y^2+z^2)    
_anet2 :: (ArrowReal to r) => (r,r,r) `to` r
_anet2 =
    proc (x,y,z) -> do
        x2 <- mulA -< (x,x)
        y2 <- mulA -< (y,y)
        z2 <- mulA -< (z,z)
        x2y2 <- addA -< (x2,y2)
        x2y2z2 <- addA -< (x2y2, z2)
        r <- sqrtA -< x2y2z2
        returnA -< r


-- | sqrt(x^2+y^2+z^2)    
_anet3 :: (ArrowReal to r) => (Map.Map String r) `to` r
_anet3 =
    proc valMap -> do
        let (Just x) = Map.lookup "x" valMap
        let (Just y) = Map.lookup "y" valMap
        let (Just z) = Map.lookup "z" valMap
        x2 <- mulA -< (x,x)
        y2 <- mulA -< (y,y)
        z2 <- mulA -< (z,z)
        x2y2 <- addA -< (x2,y2)
        x2y2z2 <- addA -< (x2y2, z2)
        r <- sqrtA -< x2y2z2
        returnA -< r

{-| An arrow enriched with real arithmetic operations. -}
class (Arrow to) => ArrowReal to r where
    realA :: CauchyReal -> String -> (() `to` r) -- TODO: change () to (SizeLimits r)
    sqrtA :: r `to` r
    mulA :: (r,r) `to` r
    addA :: (r,r) `to` r
-- TODO: add more operations

piA :: (ArrowReal to r) => () `to` r
piA = realA pi "pi"

class (ArrowReal to r) => ArrowRealInterval to r ri where
    getEndpointsA :: ri `to` (r,r)
    fromEndpointsA :: (r,r) `to` ri
    splitIntervalA :: ri `to` (ri, ri)
    subEqIntervalA :: (ri, ri) `to` Bool 

class (ArrowRealInterval to r ri) => ArrowRealUnaryFn to r ri f where
    constUFnA :: (ri, r) `to` f
    projUFnA :: ri `to` f
    getDomainUFnA :: f `to` ri
    evalAtPointUFnA :: (f,r) `to` r
    evalOnIntervalUFnA :: (f,ri) `to` ri

newtype VarName = VarName String
    deriving (IsString, Eq, Ord, Show)

type VarMap = Map.Map VarName

class (ArrowRealInterval to r ri) => ArrowRealFn to r ri f where
    constFnA :: (VarMap ri, r) `to` f
    projFnA :: (VarMap ri, VarName) `to` f
    getDomainA :: f `to` (VarMap ri)
    evalAtPointA :: (f,VarMap r) `to` r
    evalOnIntervalA :: (f,VarMap ri) `to` ri

