{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module FnReps.Polynomial.UnaryPower.Poly.EvaluationRootFinding
(
    eval,
    range,
    derivative,
    allRoots,
    isolateRoots,
    translate,
    scale,
    roots,
    rangeEstimate,
    approximateRootByBisection
)
where

import qualified Data.Map as Map
import AERN2.Num
import FnReps.Polynomial.UnaryPower.Poly.Basics

import qualified FnReps.Polynomial.UnaryPower.IntPoly.EvaluationRootFinding as IntPoly
import qualified FnReps.Polynomial.UnaryPower.IntPoly.Basics as IntPolyB

--TODO: make this more generic
eval :: Poly -> MPBall -> MPBall
eval poly@(Poly ts) x =
    evalHornerAcc (degree poly) $ (mpBall 0)
    where
    evalHornerAcc 0 sm = x*sm + terms_lookupCoeff ts 0
    evalHornerAcc k sm = evalHornerAcc (k - 1) $ x*sm + terms_lookupCoeff ts k

evalOnRational :: Poly -> Rational -> MPBall
evalOnRational poly@(Poly ts) x =
    evalOnRationalHornerAcc (degree poly) $ (integer2BallP (prec 53) 0)
    where
    evalOnRationalHornerAcc 0 sm = x*sm + terms_lookupCoeff ts 0
    evalOnRationalHornerAcc k sm = evalOnRationalHornerAcc (k - 1) $ (rational2BallP (getPrecision sm) x)*sm + terms_lookupCoeff ts k -- TODO maybe get the right precision in the beginning to avoid overhead

range :: Accuracy -> Poly -> Interval MPBall -> Interval MPBall
range ac p (Interval l r) = approxRange (toRationalDown l) (toRationalUp r) ac p

approxRange :: Rational -> Rational -> Accuracy -> Poly -> Interval MPBall
approxRange l r ac p = Interval (minValue - err) (maxValue + err)
                    where
                    (fracCoefs, err) = IntPolyB.fracListFromFPPoly $ scale (0.5*(r - l)) $ translate (0.5*(r + l)) $ p
                    p'  = IntPolyB.fromFracList $ IntPolyB.normaliseFracList fracCoefs
                    p'' = fromRationalListP (prec $ fromAccuracy ac) fracCoefs 
                    dp'  = IntPolyB.derivative $ p'
                    dp'' = IntPolyB.toFPPoly $ dp'
                    criticalPoints = map (\(Interval a b) -> approximateRootByBisection a b ac dp'') $ IntPoly.isolateRootsI dp'
                    criticalValues = [eval p'' (mpBall $ -1), eval p'' (mpBall 1)] ++ map (rangeEstimate p'') criticalPoints
                    minValue = foldl1 (\x y -> min x y) criticalValues
                    maxValue = foldl1 (\x y -> max x y) criticalValues

rangeEstimate :: Poly -> MPBall -> MPBall
rangeEstimate p b = result
                      where
                      result = (eval p b_centre) + b_errorBall * lp
                      (b_centre, b_errorBall) = getCentreAndErrorBall b
                      lp = markovBound (toRationalDown $ b_centre - b_errorBall) (toRationalUp $ b_centre + b_errorBall) p

markovBound :: Rational -> Rational -> Poly -> MPBall
markovBound l r p  = ((degree p)^2) * Map.foldl' (\x y -> x + abs(y)) (mpBall 0) ts
                     where
                     (Poly ts) = translate (-0.5*(r + l)) $ scale (0.5*(r - l)) p

derivative :: Poly -> Poly
derivative (Poly ts) = if Map.null ts' then fromList [(0,mpBall 0)] else Poly ts'
                       where
                       ts' = Map.filterWithKey (\k _ -> k >= 0) $ Map.mapKeys (\k -> k - 1) $ Map.mapWithKey (\p c -> c*p) ts

{- Root finding -}

data RootInterval = RootInterval Rational Rational Poly (Maybe Bool) Bool

instance Show RootInterval where
    show (RootInterval l r _ unique refinable) = "["++(show l)++", "++(show r)++"]. Unique root? "++(format unique)++" Refinable?"++(show refinable)
                                                 where
                                                 format x = case x of
                                                                Just True -> "True."
                                                                Just False -> "False."
                                                                Nothing -> "Don't know."

ri_isAccurate :: RootInterval -> Accuracy -> Bool
ri_isAccurate (RootInterval l r _ _ _) ac = getAccuracy (ri2ball (Interval l r) (ac + 2)) >= ac

ri_hasRoot :: RootInterval -> (Maybe Bool)
ri_hasRoot (RootInterval _ _ _ b _) = b 

split :: RootInterval -> [RootInterval]
split (RootInterval l r p _ _)  = filter (\x -> ri_hasRoot x /= Just False) [left,right]
                                     where
                                     left  = RootInterval l m p (fst rootLeft) (snd rootLeft)
                                     right = RootInterval m r p (fst rootRight) (snd rootRight)
                                     (m,rootLeft,rootRight) = midpointAndRootIndicators l r p


midpointAndRootIndicators :: Rational -> Rational -> Poly -> (Rational, (Maybe Bool, Bool), (Maybe Bool, Bool))
midpointAndRootIndicators l r p = midpointAndRootIndicatorsAcc l r p 2 1 0
                     where
                     midpointAndRootIndicatorsAcc l' r' p' n k its = 
                                                    let
                                                    m = (l' * (n - k) + k*r')/n
                                                    rl = hasSingleRoot l' m p'
                                                    rr = hasSingleRoot m r' p'
                                                    in
                                                    case evalOnRational p' m /= 0 of
                                                        Just True ->
                                                             if its > 3 then
                                                               (m,rl,rr)
                                                             else if snd rl && snd rr then
                                                               (m,rl,rr)
                                                             else
                                                                  if k + 1 < n then
                                                                    midpointAndRootIndicatorsAcc l' r' p' n (k + 1) its
                                                                  else
                                                                    midpointAndRootIndicatorsAcc l' r' p' (n + 1) 1 (its + 1)
                                                        _ -> if k + 1 < n then
                                                                    midpointAndRootIndicatorsAcc l' r' p' n (k + 1) its
                                                                  else
                                                                    midpointAndRootIndicatorsAcc l' r' p' (n + 1) 1 (its + 1) --TODO better strategy?             

--TODO this is still fairly slow when there are multiple roots
refine :: Accuracy -> RootInterval -> [RootInterval]
refine ac ri@(RootInterval _ _ _ rootUnique refinable) = if ri_isAccurate ri ac || not refinable then
                                                        [ri]
                                                    else if rootUnique == Just True then 
                                                        [approximateRootByTrisection' ac ri] 
                                                    else 
                                                        foldl (++) [] $ map (refine ac) $ split ri
                                            
roots :: Accuracy -> RootInterval -> [MPBall]
roots ac ri = map (\(RootInterval l r _ _ _) -> (ri2ball (Interval l r) (ac + 2))) (refine ac ri)                                               
     
signVariations :: Poly -> Maybe Integer
signVariations (Poly ts) = 
        snd $ Map.foldl' (\(sg,mvrs) -> \sg' -> 
                            case mvrs of
                                Nothing  -> (sg',Nothing)
                                Just vrs -> if isNothing sg' || isNothing sg then
                                                (sg',Nothing)
                                            else if fromJust sg' == 0 || fromJust sg == fromJust sg' then
                                                (sg,Just vrs)
                                            else
                                                (sg',Just $ vrs + 1)
                                                ) 
                         (sgn (snd $ Map.findMin ts), Just 0) $ Map.map (\c -> sgn c) ts
        where
        isNothing Nothing = True
        isNothing _       = False
        fromJust (Just a) = a
        fromJust _        = error " "
        sgn x = case x >= 0 of
                        Just True  -> case x == 0 of
                                        Just True  -> Just 0
                                        Just False -> Just 1
                                        Nothing -> Nothing
                        Just False -> Just $ -1
                        Nothing -> Nothing -- error $ "cannot compute sign variations: undecidable inequality " ++ (show x) ++ " >= 0, \n in the polynomial \n" ++ (show poly)

reflect :: Poly -> Poly
reflect poly@(Poly ts) = Poly ts'
                               where
                               ts' = Map.mapKeys (\p -> deg - p) ts
                               deg = degree poly 

translate :: Rational -> Poly -> Poly
translate t poly@(Poly ts) =
    translateAcc (degree poly - 1) $ (fromList [(0,terms_lookupCoeff ts (degree poly))])
    where
    translateAcc (-1) poly' = poly'
    translateAcc n poly' = let c = terms_lookupCoeff ts n in
                            translateAcc (n - 1) $ c + shiftRight 1 poly' - (rational2BallP (getPrecision c) t)*poly'

scale :: Rational -> Poly -> Poly
scale l (Poly ts) = Poly ts'
                    where
                    ts' = Map.mapWithKey (\p c -> c*l^p) ts
                                    
transform :: Rational -> Rational -> Poly -> Poly
transform l r = (translate (-1.0)) . (reflect) . (scale (r - l)) . (translate (-l)) --do transform directly?    

rootIndicator :: Rational -> Rational -> Poly -> Maybe Integer
rootIndicator l r = signVariations . transform l r 

{- The first component of the return value tells us if the function has a single simple root:
           If it is "Just True", then there is a single simple root.
           If it is "Just False", then there is no root.
           If it is "Nothing" we do not know. 
   The second component of the return value tells us if we could improve the result by making the interval smaller
            -}
hasSingleRoot :: Rational -> Rational -> Poly -> (Maybe Bool, Bool)
hasSingleRoot l r p = case rootIndicator l r p of
                        Just 0  -> (Just False, True)
                        Just 1  -> (Just True, True)
                        Just _  -> (Nothing, True)
                        Nothing -> (Nothing, False)

{-hasSomeRoot :: Rational -> Rational -> Poly -> Maybe Bool
hasSomeRoot l r p = if changesSign l r p then
                        Just True
                    else hasSingleRoot l r p

changesSign :: Rational -> Rational -> Poly -> Bool
changesSign l r p  = case (lPos,rPos) of
                        (Just True, Just False) -> True
                        (Just False, Just True) -> True
                        _                       -> False
                        where
                        lPos = (evalOnRational p l) > 0
                        rPos = (evalOnRational p r) > 0-}

{- precondition: poly is separable -}
isolateRoots :: Rational -> Rational -> Poly -> [Interval Rational]
isolateRoots l r p = ref l r
                     where
                     ref:: Rational -> Rational -> [Interval Rational]
                     ref l' r' = case hasSingleRoot l' r' p of
                                                (_, False)     -> [Interval l' r']
                                                (Just True,_)  -> [Interval l' r']
                                                (Just False,_) -> []
                                                (Nothing,_)    -> (ref l' m) ++ (ref m r')
                                                              where
                                                              m = findMidpoint l' r' p

findMidpoint :: Rational -> Rational -> Poly -> Rational
findMidpoint l r p = findMidpointAcc l r p 2 1
                     where
                     findMidpointAcc l' r' p' n k = case evalOnRational p' ((l' * (n - k) + k*r')/n) /= 0 of
                                                     Just True -> ((l' * (n - k) + k*r')/n)
                                                     _ -> if k + 1 < n then
                                                            findMidpointAcc l' r' p' n (k + 1)
                                                          else
                                                            findMidpointAcc l' r' p' (n + 1) 1 --TODO better strategy?
                                                              
approximateRootNaive :: Rational -> Rational -> Accuracy -> Bool -> Poly -> MPBall
approximateRootNaive l r a giveup p = 
                           if giveup || getAccuracy (ri2ball (Interval l r) (a + 2)) >= a then
                                ri2ball (Interval l r) a
                           else
                                approximateRootNaive l' r' a giveup' p
                                where
                                ml = (9*l + 7*r)/16
                                mr = (7*l + 9*r)/16
                                (l',r',giveup') = case hasSingleRoot l mr p of
                                                    (_, False)       -> (l,r, True)
                                                    (Just True, _)   -> (l,mr,False)
                                                    (Just False, _)  -> (ml,r,False)
                                                    (Nothing, _)     -> (l,r, True)       
       
approximateRootByTrisection :: Rational -> Rational -> Accuracy -> Poly -> MPBall
approximateRootByTrisection l r a p = case evalOnRational p l > 0 of
                                        Just False -> aux l r a p False
                                        Just True  -> aux l r a p True
                                        Nothing    -> ri2ball (Interval l r) a
                                      where
                                      aux l' r' a' p' posL =
                                       if getAccuracy (ri2ball (Interval l' r') (a + 2)) >= a then
                                            ri2ball (Interval l' r') a
                                       else case trisect l' r' posL p of
                                        Just (l'',r'',posL') -> aux l'' r'' a' p' posL'
                                        Nothing -> ri2ball (Interval l' r') a        
                                        

approximateRootByTrisection' :: Accuracy -> RootInterval -> RootInterval
approximateRootByTrisection' ac (RootInterval l r p u _) = 
                                                        case evalOnRational p l > 0 of
                                                            Just False -> aux l r ac p False
                                                            Just True  -> aux l r ac p True
                                                            Nothing    -> RootInterval l r p u False
                                                        where
                                                          aux l' r' a' p' posL =
                                                           if getAccuracy (ri2ball (Interval l' r') (ac + 2)) >= ac then
                                                                RootInterval l' r' p' u True
                                                           else case trisect l' r' posL p of
                                                            Just (l'',r'',posL') -> aux l'' r'' a' p' posL'
                                                            Nothing -> RootInterval l' r' p' u False                                         
                                        
trisect :: Rational -> Rational -> Bool -> Poly -> Maybe (Rational,Rational, Bool) -- l', r', l' positive?
trisect l r posL p = case (posML,posMR) of
                        (Just True, _) -> Just $ if not posL then (l,ml,posL) else (ml,r,True)
                        (Just False,_) -> Just $ if posL     then (l,ml,posL) else (ml,r,False)
                        (_, Just True) -> Just $ if posL     then (mr,r,True) else (l,mr, posL)
                        (_, Just False)-> Just $ if not posL then (mr,r, False) else (l, mr, posL)
                        (_,_)          -> Nothing
                     where
                     ml = (9*l + 7*r)/16
                     mr = (7*l + 9*r)/16
                     posML = evalOnRational p ml > 0
                     posMR = evalOnRational p mr > 0                                                          

approximateRootByBisection :: Rational -> Rational -> Accuracy -> Poly -> MPBall
approximateRootByBisection l r a p = case evalOnRational p l > 0 of
                                        Just False -> aux l r a p False
                                        Just True  -> aux l r a p True
                                        Nothing    -> ri2ball (Interval l r) a
                                      where
                                      aux l' r' a' p' posL =
                                       if getAccuracy (ri2ball (Interval l' r') (a + 2)) >= a then
                                            ri2ball (Interval l' r') a
                                       else let (m, posM) = (findMidpoint' l' r' p') in
                                           case (posL, posM) of
                                             (True, Just False)  -> aux l' m  a' p' True
                                             (True, Just True)   -> aux m  r' a' p' True
                                             (False, Just False) -> aux m  r' a' p' False
                                             (False, Just True)  -> aux l' m  a' p' False
                                             (_,_) -> ri2ball (Interval l' r') a

findMidpoint' :: Rational -> Rational -> Poly -> (Rational, Maybe Bool)
findMidpoint' l r p = case evalOnRational p m > 0.0 of
                        Just True  -> (m, Just True)
                        Just False -> (m, Just False)
                        Nothing    -> (m, Nothing)
                      where
                      m  = (l + r)/2                            
       
{- -}                            
allRoots :: Rational -> Rational -> Accuracy -> Poly -> [MPBall]
allRoots l r ac p = roots ac $ RootInterval l r p Nothing True

{-allRootsNaive :: Rational -> Rational -> Accuracy -> Poly -> [MPBall] -- this function is just to test the correctness of the algorithm.. do not call it on big polynomials!
allRootsNaive l r a p = map (\(Interval l' r') -> approximateRootNaive l' r' a False p) $ isolateRoots l r p

allRootsByTrisection :: Rational -> Rational -> Accuracy -> Poly -> [MPBall]
allRootsByTrisection l r a p = map (\(Interval l' r') -> approximateRootByTrisection l' r' a p) $ isolateRoots l r p   
  
allRootsByBisection :: Rational -> Rational -> Accuracy -> Poly -> [MPBall]
allRootsByBisection l r a p = map (\(Interval l' r') -> approximateRootByBisection l' r' a p) $ isolateRoots l r p  -}  
                                                                                                                                        