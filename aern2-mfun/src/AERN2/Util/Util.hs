module AERN2.Util.Util where

import Control.Applicative

import MixedTypesNumPrelude

import AERN2.MP.Ball
import AERN2.MP.Dyadic

cnMPBall :: (ConvertibleExactly a MPBall) => CN a -> CN MPBall
cnMPBall = fmap mpBall

cnMPBallP :: (ConvertibleWithPrecision a MPBall) => Precision -> CN a -> CN MPBall
cnMPBallP p = fmap (mpBallP p)

intersectCN :: CN MPBall -> CN MPBall -> CN MPBall
intersectCN x y = 
    case (fst $ ensureNoCN x, fst $ ensureNoCN y) of 
        (Nothing, Nothing) -> x
        (Just a , Nothing) -> pure a
        (Nothing, Just b ) -> pure b
        (Just _ , Just _ ) ->
            setPrecision p $ (fromEndpoints l r :: CN MPBall)
            where
            p  = getPrecision x
            lx = lowerBound x :: CN MPBall
            rx = upperBound x :: CN MPBall
            ly = lowerBound y :: CN MPBall
            ry = upperBound y :: CN MPBall
            l  = max lx ly 
            r  = min rx ry

instance (IsBall (CN MPBall)) where
    type CentreType (CN MPBall) = CN Dyadic
    centre = fmap centre

instance (IsInterval (CN MPBall) (CN MPBall)) where
    fromEndpoints = liftA2 fromEndpoints
    endpoints x   = 
        (l, r)
        where
        l = (fmap fst) $ (fmap endpoints) x 
        r = (fmap snd) $ (fmap endpoints) x

upperBound :: (IsInterval i e) => i -> e
upperBound = snd . endpoints

lowerBound :: (IsInterval i e) => i -> e
lowerBound = fst . endpoints