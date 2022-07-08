module AERN2.Util.Util where

import MixedTypesNumPrelude

import AERN2.MP.Ball

upperBound :: (IsInterval i) => i -> i
upperBound = endpointRAsInterval

lowerBound :: (IsInterval i) => i -> i
lowerBound = endpointLAsInterval

intersectCN :: CN MPBall -> CN MPBall -> CN MPBall
intersectCN = intersectCNMPBall