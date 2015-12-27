{-# LANGUAGE Arrows, TypeOperators, GeneralizedNewtypeDeriving #-}

module AERN2.Net.Spec.Arrow where

import AERN2.Real hiding (id, (.))
import Data.String (IsString(..),fromString)

--import Control.Category
import Control.Arrow
import qualified Data.Map as Map


{- mini examples -}

-- | sqrt(pi) + pi
_anet0 :: (HasRealOps to r) => () `to` r
_anet0 =
    proc _ -> do
        p <- piA -< ()
        sp <- sqrtA -< p
        psp <- addA -< (p,sp)
        returnA -< psp

-- | pi * sqrt(x) * x
_anet1 :: (HasRealOps to r) => r `to` r
_anet1 =
    proc x -> do
        p <- piA -< ()
        sx <- sqrtA -< x
        psx <- mulA -< (p,sx)
        psxx <- mulA -< (psx,x)
        returnA -< psxx

-- | sqrt(x^2+y^2+z^2)    
_anet2 :: (HasRealOps to r) => (r,r,r) `to` r
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
_anet3 :: (HasRealOps to r) => (Map.Map String r) `to` r
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

{-| An arrow enriched with arithmetic operations. -}
class (Arrow to) => HasRealOps to r where
    piA :: to () r -- TODO: change () to (SizeLimits r)
    sqrtA :: to r r
    mulA :: to (r,r) r
    addA :: to (r,r) r
-- TODO: add more operators, allow mixed types as in AERN2.Real.Operations



