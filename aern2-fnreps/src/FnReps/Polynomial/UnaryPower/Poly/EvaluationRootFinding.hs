{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module FnReps.Polynomial.UnaryPower.Poly.EvaluationRootFinding
(
    eval,
    derivative,
    allRoots,
    allRootsByTrisection,
    allRootsByBisection,
    allRootsNaive,
    approximateRootByTrisection,
    approximateRootByBisection,
    approximateRootNaive
)
where

import qualified Data.Map as Map
import AERN2.Num
import FnReps.Polynomial.UnaryPower.Poly.Basics

--TODO: make this more generic
eval :: Poly -> MPBall -> MPBall
eval poly@(Poly ts) x =
    evalHornerAcc (degree poly) $ (integer2BallP (prec 53) 0)
    where
    evalHornerAcc 0 sm = x*sm + terms_lookupCoeff ts 0
    evalHornerAcc (k + 1) sm = evalHornerAcc k $ x*sm + terms_lookupCoeff ts (k + 1)
    evalHornerAcc _ _ = error ""

evalOnRational :: Poly -> Rational -> MPBall
evalOnRational poly@(Poly ts) x =
    evalOnRationalHornerAcc (degree poly) $ (integer2BallP (prec 53) 0)
    where
    evalOnRationalHornerAcc 0 sm = x*sm + terms_lookupCoeff ts 0
    evalOnRationalHornerAcc (k + 1) sm = evalOnRationalHornerAcc k $ (rational2BallP (getPrecision sm) x)*sm + terms_lookupCoeff ts (k + 1) -- TODO maybe get the right precision in the beginning to avoid overhead
    evalOnRationalHornerAcc _ _ = error ""

derivative :: Poly -> Poly
derivative (Poly ts) = Poly $ Map.filterWithKey (\k _ -> k >= 0) $ Map.mapKeys (\k -> k - 1) $ Map.mapWithKey (\p c -> c*p) ts

-- TODO: maybe make this more "usable" by replacing the output type with Maybe Integer in case of undecidable inequality     
signVariations :: Poly -> Integer
signVariations poly@(Poly ts) = 
        snd $ Map.foldl' (\(sg,vrs) -> \sg' ->  if sg' == 0 || sg == sg' then (sg,vrs) else (sg',vrs + 1)) (sgn (snd $ Map.findMin ts),0) $ Map.map (\c -> sgn c) ts
        where
        sgn x = case x >= 0 of
                        Just True  -> case x == 0 of
                                        Just True  -> 0
                                        Just False -> 1
                                        Nothing -> error $ "cannot compute sign variations: undecidable inequality " ++ (show x) ++ " >= 0, \n in the polynomial \n" ++ (show poly)
                        Just False -> -1
                        Nothing -> error $ "cannot compute sign variations: undecidable inequality " ++ (show x) ++ " >= 0, \n in the polynomial \n" ++ (show poly)

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
                                    

--TODO compute separable part to deal with multiple zeroes
transform :: Rational -> Rational -> Poly -> Poly
transform l r = (translate (-1.0)) . (reflect) . (scale (r - l)) . (translate (-l)) --do transform directly?    

rootIndicator :: Rational -> Rational -> Poly -> Integer
rootIndicator l r = signVariations . transform l r 

{- returns If this functions returns "Just True", then there is a single simple root.
           If it returns "Just False", then there is no root.
           If it returns "Nothing" we do not know.  -}
hasSingleRoot :: Rational -> Rational -> Poly -> Maybe Bool
hasSingleRoot l r p = case rootIndicator l r p of
                        0 -> Just False
                        1 -> Just True
                        _ -> Nothing

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

isolateRoots :: Rational -> Rational -> Poly -> [Interval Rational]
isolateRoots l r p = refine l r
                     where
                     refine :: Rational -> Rational -> [Interval Rational]
                     refine l' r' = case hasSingleRoot l' r' p of
                                                Just True  -> [Interval l' r']
                                                Just False -> []
                                                Nothing    -> (refine l' m) ++ (refine m r')
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
                                                              
approximateRootNaive :: Rational -> Rational -> Accuracy -> Poly -> MPBall
approximateRootNaive l r a p = if getAccuracy (ri2ball (Interval l r) (a + 2)) >= a then
                                ri2ball (Interval l r) a
                           else
                                approximateRootNaive l' r' a p
                                where
                                ml = (9*l + 7*r)/16
                                mr = (7*l + 9*r)/16
                                (l',r') = case hasSingleRoot l mr p of
                                                Just True  -> (l,mr)
                                                Just False -> (ml,r)
                                                Nothing -> error "Illegal use of approximateRootNaive: polynomial must have a unique simple root in the given interval"       
       
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

{-markovBound :: Rational -> Rational -> Poly -> MPBall
markovBound l r p  = (degree p')^2 * Map.foldl' (\x y -> x + abs(y)) (integer2Ball 0) ts
                     where
                     p'@(Poly ts) = translate (-l) $ scale (r - l) p   -}                  
       
{- -}                            
allRoots :: Rational -> Rational -> Accuracy -> Poly -> [MPBall]
allRoots = allRootsByTrisection   

allRootsNaive :: Rational -> Rational -> Accuracy -> Poly -> [MPBall] -- this function is just to test the correctness of the algorithm.. do not call it on big polynomials!
allRootsNaive l r a p = map (\(Interval l' r') -> approximateRootNaive l' r' a p) $ isolateRoots l r p

allRootsByTrisection :: Rational -> Rational -> Accuracy -> Poly -> [MPBall]
allRootsByTrisection l r a p = map (\(Interval l' r') -> approximateRootByTrisection l' r' a p) $ isolateRoots l r p   
  
allRootsByBisection :: Rational -> Rational -> Accuracy -> Poly -> [MPBall]
allRootsByBisection l r a p = map (\(Interval l' r') -> approximateRootByBisection l' r' a p) $ isolateRoots l r p  

{- auxiliary function -}

{-ri2ball :: Interval Rational -> Accuracy -> MPBall
ri2ball (Interval l r) acc =
    endpoints2Ball lB rB
    where
    lB = rational2BallP p l
    rB = rational2BallP p r
    p = prec $ fromAccuracy acc -}            
                                                                                         