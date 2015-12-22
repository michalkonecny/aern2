{-# LANGUAGE CPP #-}
{-# LANGUAGE Arrows, EmptyDataDecls, GADTs, StandaloneDeriving, TypeOperators #-}

module AERN2.Net.Spec.Arrow where

import AERN2.Real hiding (id, (.))
--import Data.String (fromString)

import Control.Category
import Control.Arrow

{- TODO
    Provide instances of "AERN2.Real.Operations" for arbitrary arrows,
    so that networks can be defined by simple arithmetic expressions, such as:
    
    > net :: (HasRealOps to r) => r `to` r
    > net = let x = var "x" in pi * sqrt(x) * x@ 
-}

-- pi * sqrt(x) * x
_anet1 :: (HasRealOps to r) => r `to` r
_anet1 =
    proc x -> do
        p <- piR -< ()
        sx <- sqrtR -< x
        psx <- mulR -< (p,sx)
        psxx <- mulR -< (psx,x)
        returnA -< psxx

-- sqrt(x^2+y^2+z^2)    
_anet2 :: (HasRealOps to r) => (r,r,r) `to` r
_anet2 =
    proc (x,y,z) -> do
        x2 <- mulR -< (x,x)
        y2 <- mulR -< (y,y)
        z2 <- mulR -< (z,z)
        x2y2 <- addR -< (x2,y2)
        x2y2z2 <- addR -< (x2y2, z2)
        r <- sqrtR -< x2y2z2
        returnA -< r

class (Arrow a) => HasRealOps a r where
    piR :: a () r -- TODO: change () to (SizeLimits r)
    sqrtR :: a r r
    mulR :: a (r,r) r
    addR :: a (r,r) r
-- TODO: add more operators

{- Direct evaluation using CauchyReal: -}

instance HasRealOps (->) CauchyReal where
    piR = const pi
    sqrtR = sqrt
    addR = uncurry (+)
    mulR = uncurry (*)

{- Direct evaluation using CauchyReal: -}

instance HasRealOps (->) MPBall where
--    piR p = cauchyReal2ball (prec2integer p) pi -- TODO: enable when we have (SizeLimits MPBall)
    sqrtR = sqrt
    addR = uncurry (+)
    mulR = uncurry (*)

