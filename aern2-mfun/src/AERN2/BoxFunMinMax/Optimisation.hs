module AERN2.BoxFunMinMax.Optimisation where

import MixedTypesNumPrelude
import AERN2.MP.Ball
import AERN2.BoxFun.Box (Box, getEndpoints)
import AERN2.BoxFun.Optimisation
import AERN2.BoxFun.Type
import AERN2.Linear.Vector.Type as V

import Data.Maybe
import qualified Data.List as List

import qualified AERN2.PQueue as Q


import AERN2.Util.Util

import Debug.Trace (trace)

globalMinimumGreaterThanN :: BoxFun -> Accuracy -> Precision -> CN Rational -> Maybe Bool
globalMinimumGreaterThanN f ac initialPrecision n =
    case globalMin of
        Just x  -> 
            trace ("Global minimum found, minimum = " ++ show x ++ " for f at domain " ++ show (getEndpoints (domain f)))
            Just x
        Nothing -> Nothing
    where globalMin = globalMinimumAboveN f ac initialPrecision (cnMPBallP initialPrecision n)

globalMinimumWithCutoffAboveN :: BoxFun -> Accuracy -> CN MPBall -> Precision -> CN MPBall -> Maybe Bool
globalMinimumWithCutoffAboveN f ac cutoff initialPrecision n =
    if dimension f == 1 then
        let
            fl         = apply f (V.map lowerBound $ domain f)
            fr         = apply f (V.map upperBound $ domain f)
            mLocalMin   = bestLocalMinimumWithCutoffAboveN f (domain f) ac cutoff initialPrecision n
        in
            case mLocalMin of
                Just (False, _) -> Just False
                _               -> Just (fl !>! n && fr !>! n)
    else 
        let
            mLocalMin       = bestLocalMinimumWithCutoffAboveN f (domain f) ac cutoff initialPrecision n
            boundaryFuns   = boundaryRestrictions f
        in
            case mLocalMin of
                Just localMin ->
                    (&&) <$> Just (fst localMin) <*> boundaryMinima boundaryFuns (min cutoff ((upperBound (snd localMin) :: CN MPBall)))
                Nothing ->
                    boundaryMinima boundaryFuns cutoff
                where
                    boundaryMinima  []      _       =   Nothing
                    boundaryMinima  [g]     cutoff  =   case globalMinimumWithCutoffAboveN g ac cutoff initialPrecision n of
                                                            Just False  -> Just False
                                                            _           -> Just True
                    boundaryMinima  (g:gs)  cutoff  =   case globalMinimumWithCutoffAboveN g ac cutoff initialPrecision n of
                                                            Just False  -> Just False
                                                            _           -> boundaryMinima gs cutoff


globalMinimumAboveN :: BoxFun -> Accuracy -> Precision -> CN MPBall -> Maybe Bool
globalMinimumAboveN f ac initialPrecision n =
    globalMinimumWithCutoffAboveN f ac (apply f (centre boxp)) initialPrecision n
    where
    boxp = setPrecision initialPrecision (domain f)

bestLocalMinimumWithCutoffAboveN :: BoxFun -> Box -> Accuracy -> CN MPBall -> Precision -> CN MPBall -> Maybe (Bool, CN MPBall)
bestLocalMinimumWithCutoffAboveN f box ac initialCutoff initialPrecision n =
    aux initialQueue initialCutoff 0 dummyBox
    where
    boxp             = setPrecision initialPrecision box
    initialRange     = apply f boxp
    initialSearchBox = SearchBox boxp initialRange
    initialQueue     = Q.singleton initialSearchBox
    dummyBox         = SearchBox (V.fromList [cn $ mpBall $ 10^!6]) initialRange -- TODO: hack...

    aux q cutoff steps (SearchBox _lastBox rng) =  
        case Q.minView q of
            Nothing -> trace ("no local minimum.") $ Nothing
            Just (minBox, q') ->
                --trace ("value: "++ (show $ val)) $
                trace ("min box: "++ (show $ minBox)) $
                --trace ("box acc: "++ (show $ getAccuracy $ ext)) $
                --trace (show $ Box.width (extents minBox)) $
                --trace ("lower bound "++ (show $ Box.lowerBound $ val)) $
                --trace ("val' "++ (show $ val')) $
                trace ("cutoff: "++ (show $ cutoff)) $
                trace ("queue size: "++ (show $ Q.size q)) $
                --trace ("cutoff == 0? "++(show $ cutoff == (mpBall 0))) $
                --trace ("precision: "++ (show $ precision)) $
                --trace ("dist to last "++ (show $ distToLast)) $
                --trace ("accuracy: "++ (show $ getAccuracy val')) $
                --trace ("precision centre: "++ (show $ fmap (getPrecision . centre) val)) $
                if cutoff !<! n then
                    trace ("f is under " ++ show n ++ " in domain: " ++ show (AERN2.BoxFun.Box.getEndpoints ext))
                    Just (False, cutoff)
                else
                    if getAccuracy val' >= ac then
                        -- TODO: if val' >=n then Just True
                        Just (True, val')
                    else 
                        aux q'' newCutoff (steps + 1) (SearchBox ext rng)
                where
                val' = fromEndpointsAsIntervals (lowerBound val) (cutoff)
                SearchBox ext val = minBox

                (newCutoff, newBoxes) = 
                    processBox f ac cutoff minBox

                q'' = foldr (Q.insert) q' newBoxes
