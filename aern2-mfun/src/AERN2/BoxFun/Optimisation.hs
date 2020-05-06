module AERN2.BoxFun.Optimisation where

import qualified Prelude as Prelude
import MixedTypesNumPrelude
import AERN2.MP.Dyadic
import AERN2.MP.Ball
-- import AERN2.MP.Float
import AERN2.BoxFun.Box (Box)
import qualified AERN2.BoxFun.Box as Box
import AERN2.BoxFun.Type
-- import AERN2.AD.Differential ()
import Data.Maybe

import AERN2.Linear.Vector.Type (Vector, (!))
import AERN2.Linear.Vector.Type as V
import AERN2.Linear.Matrix.Type
import AERN2.Linear.Matrix.Inverse

import qualified Data.List as List

import qualified AERN2.PQueue as Q

import AERN2.Util.Util

import Debug.Trace (trace)

globalMinimumGreaterThanN :: BoxFun -> Accuracy -> CN Rational -> Precision -> Bool
globalMinimumGreaterThanN f ac n initialPrecision =
    trace (show x)
    x !>! n
    where x = globalMinimum f ac initialPrecision 

minFun :: BoxFun -> Accuracy -> Precision -> (Integer, CN MPBall)
minFun f ac initialPrecision = 
    bestLocalMinimum f (domain f) ac initialPrecision

data SearchBox =
    SearchBox
    {
            extents :: Box
        ,   minimum :: CN MPBall 
    } deriving (Show)

instance 
    HasPrecision SearchBox
    where
    getPrecision (SearchBox b _) = getPrecision b

instance 
    CanSetPrecision SearchBox
    where
    setPrecision p (SearchBox b m) = SearchBox (setPrecision p b) m

instance Prelude.Eq SearchBox where
    (==) (SearchBox _ _) (SearchBox _ _) =
        False -- TODO: safe?

instance Prelude.Ord SearchBox where
    (<=) (SearchBox _ min0) (SearchBox _ min1) = 
        case (fst $ ensureNoCN $ (lowerBound min0 :: CN MPBall), fst $ ensureNoCN $ (lowerBound min1 :: CN MPBall)) of
            (Nothing, Nothing) -> True
            (Nothing, Just _ ) -> True
            (Just _ , Nothing) -> False
            (Just m0, Just m1) -> 
                centre m0 - (dyadic $ radius m0) <= centre m1 - (dyadic $ radius m1) -- TODO: radius should be 0

---

globalMinimumWithCutoff :: BoxFun -> Accuracy -> CN MPBall -> Precision -> CN MPBall
globalMinimumWithCutoff f ac cutoff initialPrecision =
    if dimension f == 1 then
        let
            fl       = apply f (V.map lowerBound $ domain f)
            fr       = apply f (V.map upperBound $ domain f)
            localMin = snd $ bestLocalMinimumWithCutoff f (domain f) ac cutoff initialPrecision
        in
            min fl $ min localMin fr
    else 
        let
            localMin       = snd $ bestLocalMinimumWithCutoff f (domain f) ac cutoff initialPrecision
            boundaryFuns   = boundaryRestrictions f
            boundaryMinima = List.map (\g -> globalMinimumWithCutoff g ac (min cutoff ((upperBound localMin :: CN MPBall))) initialPrecision) boundaryFuns
        in
            List.foldl' min localMin boundaryMinima


globalMinimum :: BoxFun -> Accuracy -> Precision -> CN MPBall
globalMinimum f ac initialPrecision =
    globalMinimumWithCutoff f ac (apply f (centre boxp)) initialPrecision
    where
    boxp = setPrecision initialPrecision (domain f)

bestLocalMinimum :: BoxFun -> Box -> Accuracy -> Precision -> (Integer, CN MPBall)
bestLocalMinimum f box ac initialPrecision =
    bestLocalMinimumWithCutoff f box ac (apply f (centre boxp)) initialPrecision
    where
    boxp = setPrecision initialPrecision box

bestLocalMinimumWithCutoff :: BoxFun -> Box -> Accuracy -> CN MPBall -> Precision -> (Integer, CN MPBall)
bestLocalMinimumWithCutoff f box ac initialCutoff initialPrecision =
    aux initialQueue initialCutoff 0 dummyBox
    where
    boxp             = setPrecision initialPrecision box
    initialRange     = apply f boxp
    initialSearchBox = SearchBox boxp initialRange
    initialQueue     = Q.singleton initialSearchBox
    dummyBox         = SearchBox (V.fromList [cn $ mpBall $ 10^!6]) initialRange -- TODO: hack...

    aux q cutoff steps (SearchBox _lastBox rng) =  
        case Q.minView q of
            Nothing -> trace ("no local minimum.") $ (steps, rng)
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
                if getAccuracy val' >= ac then
                    (steps, val')
                else 
                    aux q'' newCutoff (steps + 1) (SearchBox ext rng)
                where
                val' = fromEndpointsAsIntervals (lowerBound val) (cutoff)
                SearchBox ext val = minBox

                (newCutoff, newBoxes) = 
                    processBox f ac cutoff minBox

                q'' = foldr (Q.insert) q' newBoxes

lipschitzContraction :: BoxFun -> Box -> SearchBox -> SearchBox
lipschitzContraction f g (SearchBox box m) =
    {-trace("fa: "++(show $ getAccuracy (apply f box))) $
    trace("la: "++(show $ getAccuracy $ dotProduct)) $
    trace("ba: "++(show $ getAccuracy $ box ! int 0)) $-}
    {-if (radius $ (~!) $ newRange) < (radius $ (~!) $ m) then
        trace ("Lipschitz better.")
        SearchBox box m'
    else -}
    SearchBox box m'
    where
    boxCentre      = centre box
    centreValue    = apply f boxCentre
    difference     = box - boxCentre
    dotProduct     = g * difference 
    newRange       = centreValue + dotProduct
    m'             = intersectCN m newRange

lipschitzRange :: BoxFun -> CN MPBall -> Box -> Box -> Box -> CN MPBall -> CN MPBall
lipschitzRange _f fc c g box m =
    m'
    where
    difference     = box - c
    normG          = Box.ellOneNorm g
    normDiff       = Box.inftyNorm  difference
    dotProduct     = normG * normDiff
    newRange       = fc + (fromEndpointsAsIntervals (-dotProduct) dotProduct :: CN MPBall)
    m'             = intersectCN m newRange

increasePrecision :: Precision -> Precision
increasePrecision p =
    p + (prec $ (integer p) `Prelude.div` 2)

newtonStep :: BoxFun -> Accuracy -> Vector (CN MPBall) -> Vector (CN MPBall) -> Matrix (CN MPBall) -> SearchBox -> Bool -> Maybe (Bool, SearchBox)
newtonStep f ac c dfc hInv b@(SearchBox box m) newtonSuccesful = 
    --Just $ SearchBox box' m'
    {-trace ("precision m "++(show $ (fmap getPrecision) m)) $
    trace ("precision m' "++(show $ (fmap getPrecision) m')) $
    trace ("precision box centre "++(show $ getPrecision c)) $
    trace ("precision box "++(show $ getPrecision box)) $
    trace ("precision newton box "++(show $ getPrecision newtonBox)) $
    trace ("precision box' "++(show $ getPrecision box')) $
    trace ("precision hInv "++(show $ getPrecision (entries hInv ! int 0))) $-}
    if getAccuracy m >= ac then
        Just (newtonSuccesful, b)
    --else if not hInvDefined then
    --    Just (newtonSuccesful, b)
    else if Box.intersectionCertainlyEmpty box newtonBox then
        Nothing
    else if Box.width box' !<=! (dyadic $ 0.75) * Box.width box then
        if getAccuracy m' > getAccuracy m then
            newtonStep f ac c dfc hInv (SearchBox box' m') True
        else 
            Just (True, SearchBox (setPrecision (increasePrecision $ getPrecision box') box') m')
    else 
        Just (newtonSuccesful, SearchBox box' m')
    where
    {-c           = centre box
    dfc         = gradient f c-}
    -- hInvDefined = V.foldl' (&&) (True) $ V.map (isJust . fst . ensureNoCN) (entries hInv)
    newtonBox   = c - hInv * (dfc)
    box'        = Box.nonEmptyIntersection box newtonBox
    m'          = apply f box'

processBox :: BoxFun -> Accuracy -> CN MPBall -> SearchBox -> (CN MPBall, [SearchBox])
processBox f ac cutoff box =
    if getAccuracy ext < bits 10 then 
        split f (gradient f ext) cutoff ext  
    else 
        result
    where
    ext            = extents box
    (_fb, dfb, hfb) = valueGradientHessian f ext
    c              = centre ext
    dfc            = gradient f c
    maybeHinv      = inverse hfb
    -- p              = getPrecision box
    box'           = --Just (False, box)
        case maybeHinv of 
            Nothing   -> Just (False, box)
            Just hInv -> newtonStep f ac c dfc hInv box False
    result =
        case box' of 
            Nothing -> (cutoff, [])
            Just (newtonSuccesful, bx@(SearchBox bxe m)) ->
                let
                    c' = min (upperBound $ apply f $ centre bxe :: CN MPBall) cutoff    
                in
                if newtonSuccesful then
                    if getAccuracy m >= ac then
                        (c', [bx])
                    else
                        processBox f ac c' bx
                else
                    split f dfb c' bxe

split :: BoxFun -> Vector (CN MPBall) -> CN MPBall -> Box -> (CN MPBall, [SearchBox])
split f dfb cutoff bxe = 
    let
    diff    = bxe - centre bxe
    dir i   = (fmap dyadic) $ (fmap radius) $ (dfb ! i) * (diff ! i) :: CN Dyadic
    dirs    = V.map dir $ V.enumFromTo 0 (V.length bxe - 1)
    dirsDefined = V.foldl' (&&) True $ V.map (isJust . fst . ensureNoCN) dirs
    aux k j d = 
        if k == V.length bxe then 
            j 
        else 
            let
                d' = (~!) $ dirs ! k
            in
            if d' > d then 
                aux (k + 1) k d'
            else
                aux (k + 1) j d
    splittingIndex = 
        if dirsDefined then (aux 1 0 ((~!) $ dirs ! 0)) else Box.widestDirection bxe
    (a , b)    = Box.bisect splittingIndex bxe
    (fa, dfa') = valueGradient f a
    (fb, dfb') = valueGradient f b
    ac  = centre a
    bc  = centre b
    fac  = apply f ac
    fbc  = apply f bc
    fa'  = lipschitzRange f fac ac dfa' a fa
    fb'  = lipschitzRange f fbc bc dfb' b fb
    cutoff'       = min (upperBound fac :: CN MPBall) $ min (upperBound fbc :: CN MPBall) cutoff
    leftMonotone  = V.foldl' (||) False $ V.map (!/=! 0) dfa'
    rightMonotone = V.foldl' (||) False $ V.map (!/=! 0) dfb'
    boxes = 
        case (leftMonotone || fa' !>! cutoff', rightMonotone || fb' !>! cutoff') of
            (True,  True)  -> []
            (True,  False) -> [SearchBox b fb']
            (False, True)  -> [SearchBox a fa']
            (False, False) -> [SearchBox a fa', SearchBox b fb']
    in
        (cutoff', boxes)

maxBoxFunGreaterThanN :: BoxFun -> BoxFun -> Accuracy -> CN Rational -> Precision -> Bool
maxBoxFunGreaterThanN f g ac n initialPrecision =
    (fmin !>! n || gmin !>!n) ||
        (not (Box.intersectionCertainlyEmpty fbox gbox) &&
            (Box.width fboxp !>! cutoff && Box.width gboxp !>! cutoff) &&
                let
                    newFBoxes = Box.fullBisect fboxp
                    newGBoxes = Box.fullBisect gboxp

                    bounds extents          = (lowerBound extents :: CN MPBall, upperBound extents :: CN MPBall)
                    nBetweenBounds n (l, r) = l !<=! n && n !<=! r

                    filteredNewFBoxes = List.filter (nBetweenBounds n . bounds . apply f) newFBoxes
                    filteredNewGBoxes = List.filter (nBetweenBounds n . bounds . apply f) newGBoxes

                    updateDomain z = BoxFun (dimension z) (bf_eval z)

                    checkBoxes [] _                     =   False
                    checkBoxes (fboxp' : fboxes) gboxes =   checkBoxes2 
                                                                fboxp' 
                                                                (filter (not . Box.intersectionCertainlyEmpty fboxp') gboxes) 
                                                            || checkBoxes fboxes gboxes

                    checkBoxes2 _ []                     =  False
                    checkBoxes2 fboxp' (gboxp' : gboxes) =  maxBoxFunGreaterThanN    
                                                                (updateDomain f fboxp') 
                                                                (updateDomain g gboxp') 
                                                                ac n initialPrecision 
                                                            || checkBoxes2 fboxp' gboxes
                in
                    checkBoxes filteredNewFBoxes filteredNewGBoxes
            )
    where
        fmin = globalMinimum f ac initialPrecision 
        gmin = globalMinimum g ac initialPrecision
        cutoff = 1/2^50

        fbox                 = domain f
        fboxp                = setPrecision initialPrecision fbox 

        gbox                 = domain g
        gboxp                = setPrecision initialPrecision gbox
