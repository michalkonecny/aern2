module AERN2.BoxFunMinMax.Optimisation where

import MixedTypesNumPrelude
import AERN2.MP.Ball
import AERN2.BoxFun.Box (Box, getEndpoints, width)
import AERN2.BoxFun.Optimisation
import AERN2.BoxFun.Type
import AERN2.Linear.Vector.Type as V

import Data.Maybe
import qualified Data.List as List

import qualified AERN2.PQueue as Q

import AERN2.Util.Util

import Debug.Trace (trace)

import AERN2.BoxFun.Box
import AERN2.Linear.Vector.Type (Vector, (!))
import qualified AERN2.Linear.Vector.Type as V
import AERN2.Util.Util
import AERN2.AD.Type


-- | Modified version of AERN2.BoxFun.Type.boundaryRestrictions which
-- keeps track of reduced dimensions
boundaryRestrictionsWithInversion :: BoxFun -> [(BoxFun, Box -> Box)]
boundaryRestrictionsWithInversion (BoxFun d ev dom) =
  concat
  [
    [
      (BoxFun 
        (d - 1)
        (
        \v ->
          ev $ (vAddDimension i (setPrecision (getPrecision v) (differential 2 (upperBound (dom ! i)))) v)
        ) 
        (domWithoutDimension i)
      ,
      (vAddDimension i (upperBound (dom ! i))))
      ,
      (BoxFun 
        (d - 1)
        (
        \v ->
          ev $ (vAddDimension i (setPrecision (getPrecision v) (differential 2 (lowerBound (dom ! i)))) v)
        )
        (domWithoutDimension i)
      ,
      (vAddDimension i (lowerBound (dom ! i))))
    ]
    |
    i <- [0 .. d - 1]
  ]
  where
    domWithoutDimension i = (V.map (\j -> if j >= i then dom ! (j + 1) else dom ! j) $ V.enumFromTo 0 (d - 2))
    
    vAddDimension :: Integer -> a -> Vector a -> Vector a
    vAddDimension i value v = V.map (\j -> if j == i then value else if j < i then v ! j else v ! (j - 1)) (V.enumFromTo 0 (d - 1))

globalMinimumAboveN2 :: BoxFun -> Accuracy -> Precision -> CN MPBall -> (Maybe Bool, Maybe SearchBox)
globalMinimumAboveN2 f ac initialPrecision n =
    if dimension f == 1 then
        let
            fl         = apply f (V.map lowerBound $ domain f)
            fr         = apply f (V.map upperBound $ domain f)
        in
            if fl !<=! n then
                trace "Left boundary"
                (Just False, Just (SearchBox (V.map lowerBound (domain f)) fl))
            else
                if fr !<=! n then
                    trace "Right boundary"
                    (Just False, Just (SearchBox (V.map upperBound (domain f)) fr))
                else
                    case minimumAboveN2 f (domain f) ac initialPrecision n of
                        r@(Nothing, _) ->
                            if (fl !>! n && fr !>! n) then
                                (Just True, Nothing)
                            else
                                r
                        r -> r
    else
        case boundaryMinima boundaryFuns of
            (Just True, _)  -> minimumAboveN2 f (domain f) ac initialPrecision n
            r               -> 
                trace ("Indeterminate/False domain: " ++ show (domain f))
                r 
        where
            boundaryFuns = boundaryRestrictionsWithInversion f
            
            boundaryMinima  []      =   (Just True, Nothing)
            boundaryMinima  ((g, restoreBox) : gs)  =  
                case globalMinimumAboveN2 g ac initialPrecision n of
                    (Just True, _)  -> boundaryMinima gs
                    (mBool, mBox)   -> 
                        trace ("Indeterminate/False domain: " ++ show (domain g)) $
                        case mBox of
                            Nothing  -> (mBool, Nothing)
                            Just (SearchBox box minimum) -> (mBool, Just $ SearchBox (restoreBox box) minimum)


-- | Use the newton step to check whether or not the minimum of a given function is above n.
minimumAboveN2 :: BoxFun -> Box -> Accuracy -> Precision -> CN MPBall -> (Maybe Bool, Maybe SearchBox)
minimumAboveN2 f box ac initialPrecision n =
    aux initialQueue 0 dummyBox
    where
    boxp             = setPrecision initialPrecision box
    initialRange     = apply f boxp
    initialSearchBox = SearchBox boxp initialRange
    initialQueue     = Q.singleton initialSearchBox
    dummyBox         = SearchBox (V.fromList [cn $ mpBall $ 10^6]) initialRange -- TODO: hack...

    aux q steps (SearchBox _lastBox rng) =  
        case Q.minView q of
            Nothing -> 
                -- we know that the upper bound of f is not under n
                trace ("f is above " ++ show n) $ (Just True, Nothing)
            Just (minBox, q') ->
                trace ("value: "++ (show $ val)) $
                trace ("min box: "++ (show $ minBox)) $
                --trace ("box acc: "++ (show $ getAccuracy $ ext)) $
                --trace (show $ Box.width (extents minBox)) $
                --trace ("lower bound "++ (show $ Box.lowerBound $ val)) $
                --trace ("val' "++ (show $ val')) $
                -- trace ("cutoff: "++ (show $ cutoff)) $
                trace ("queue size: "++ (show $ Q.size q)) $
                --trace ("cutoff == 0? "++(show $ cutoff == (mpBall 0))) $
                --trace ("precision: "++ (show $ precision)) $
                --trace ("dist to last "++ (show $ distToLast)) $
                --trace ("accuracy: "++ (show $ getAccuracy val')) $
                --trace ("precision centre: "++ (show $ fmap (getPrecision . centre) val)) $
                if upperBound val !<=! n then
                    trace ("f is under " ++ show n ++ " in domain: " ++ show (AERN2.BoxFun.Box.getEndpoints ext) ++ " with val: " ++ show (upperBound val))
                    (Just False, Just minBox)
                else
                    if (lowerBound val !>! n) then
                        trace ("f is above " ++ show n ++ " in domain: " ++ show (AERN2.BoxFun.Box.getEndpoints ext) ++ " with lower bound of val: " ++ show (lowerBound val))
                        aux q' (steps + 1) (SearchBox ext rng)
                    else
                        if (boxChangeWidth !>=! cn (0.1)) then -- TODO: mpBallP parameter
                            trace "processing boxes"
                            trace (show (width (extents minBox)))
                            trace (show newBoxes)
                            aux q'' (steps + 1) (SearchBox ext rng)
                        else
                            (Nothing, Just minBox)
                where
                boxChangeWidth = abs(width (_lastBox - ext))
                SearchBox ext val = minBox

                (_, newBoxes) = 
                    processBox f ac (apply f (centre box)) minBox

                q'' = foldr (Q.insert) q' newBoxes

-- | Check that the global minimum of a given function is above n.
-- This function ensures that the edges of a BoxFun is above n,
-- and then calls minimumAboveN to ensure the rest of the BoxFun
-- is above n. 
globalMinimumAboveN :: BoxFun -> Accuracy -> Precision -> CN MPBall -> CN MPBall -> (Maybe Bool, Maybe SearchBox)
globalMinimumAboveN f ac initialPrecision widthCutoff n =
    if dimension f == 1 then
        let
            fl         = apply f (V.map lowerBound $ domain f)
            fr         = apply f (V.map upperBound $ domain f)
        in
            if fl !<=! n then
                trace "Left boundary"
                (Just False, Just (SearchBox (V.map lowerBound (domain f)) fl))
            else
                if fr !<=! n then
                    trace "Right boundary"
                    (Just False, Just (SearchBox (V.map upperBound (domain f)) fr))
                else
                    minimumAboveN f (domain f) ac initialPrecision widthCutoff n
    else 
        let
            boundaryFuns = boundaryRestrictionsWithInversion f
        in
            case minimumAboveN f (domain f) ac initialPrecision widthCutoff n of
                (Just True, _)  -> boundaryMinima boundaryFuns
                o               -> 
                    trace ("Indeterminate/False domain: " ++ show (domain f))
                    o 
            where
                boundaryMinima  []      =   (Just True, Nothing)
                boundaryMinima  ((g, restoreBox) : gs)  =  
                    case globalMinimumAboveN g ac initialPrecision widthCutoff n of
                        (Just True, _)  -> boundaryMinima gs
                        (mBool, mBox)   -> 
                            trace ("Indeterminate/False domain: " ++ show (domain g)) $
                            case mBox of
                                Nothing  -> (mBool, Nothing)
                                Just (SearchBox box minimum) -> (mBool, Just $ SearchBox (restoreBox box) minimum)

-- | Use the newton step to check whether or not the minimum of a given function is above n.
minimumAboveN :: BoxFun -> Box -> Accuracy -> Precision -> CN MPBall -> CN MPBall -> (Maybe Bool, Maybe SearchBox)
minimumAboveN f box ac initialPrecision widthCutoff n =
    aux initialQueue 0 dummyBox
    where
    boxp             = setPrecision initialPrecision box
    initialRange     = apply f boxp
    initialSearchBox = SearchBox boxp initialRange
    initialQueue     = Q.singleton initialSearchBox
    dummyBox         = SearchBox (V.fromList [cn $ mpBall $ 10^6]) initialRange -- TODO: hack...

    aux q steps (SearchBox _lastBox rng) =  
        case Q.minView q of
            Nothing -> 
                -- we know that the upper bound of f is not under n
                trace ("f is above " ++ show n) $ (Just True, Nothing)
            Just (minBox, q') ->
                trace ("value: "++ (show $ val)) $
                trace ("min box: "++ (show $ minBox)) $
                --trace ("box acc: "++ (show $ getAccuracy $ ext)) $
                --trace (show $ Box.width (extents minBox)) $
                --trace ("lower bound "++ (show $ Box.lowerBound $ val)) $
                --trace ("val' "++ (show $ val')) $
                -- trace ("cutoff: "++ (show $ cutoff)) $
                trace ("queue size: "++ (show $ Q.size q)) $
                --trace ("cutoff == 0? "++(show $ cutoff == (mpBall 0))) $
                --trace ("precision: "++ (show $ precision)) $
                --trace ("dist to last "++ (show $ distToLast)) $
                --trace ("accuracy: "++ (show $ getAccuracy val')) $
                --trace ("precision centre: "++ (show $ fmap (getPrecision . centre) val)) $
                if upperBound val !<=! n then
                    trace ("f is under " ++ show n ++ " in domain: " ++ show (AERN2.BoxFun.Box.getEndpoints ext) ++ " with val: " ++ show (upperBound val))
                    (Just False, Just minBox)
                else
                    if (lowerBound val !>! n) then
                        trace ("f is above " ++ show n ++ " in domain: " ++ show (AERN2.BoxFun.Box.getEndpoints ext) ++ " with lower bound of val: " ++ show (lowerBound val))
                        aux q' (steps + 1) (SearchBox ext rng)
                    else
                        if (width (extents minBox) !>=! widthCutoff) then -- TODO: mpBallP parameter
                            trace "processing boxes"
                            trace (show (width (extents minBox)))
                            trace (show newBoxes)
                            aux q'' (steps + 1) (SearchBox ext rng)
                        else
                            (Nothing, Just minBox)
                where
                SearchBox ext val = minBox

                (_, newBoxes) = 
                    processBox f ac (apply f (centre box)) minBox

                q'' = foldr (Q.insert) q' newBoxes
