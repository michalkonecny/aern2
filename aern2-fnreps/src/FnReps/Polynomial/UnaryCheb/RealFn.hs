{-# LANGUAGE UndecidableInstances #-}
module FnReps.Polynomial.UnaryCheb.RealFn 
where

import AERN2.Num
import AERN2.RealFunction

import Control.Arrow

import FnReps.Polynomial.UnaryCheb.PolyBall

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
--shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id

{- examples -}

rf_x :: RealFn
rf_x = projUnaryFnA (Interval (-1.0) (4.0))

rf_2x :: RealFn
rf_2x = rf_x + rf_x

rf_x2 :: RealFn
rf_x2 = rf_x * rf_x

rf_x_pi :: CauchyReal
rf_x_pi = evalAtOutPointUnaryFnA (rf_x, pi/4)

rf_2x_pi :: CauchyReal
rf_2x_pi = evalAtOutPointUnaryFnA (rf_2x, pi/4)

rf_x2_pi :: CauchyReal
rf_x2_pi = evalAtOutPointUnaryFnA (rf_x2, pi/4)

{- type definition -} 

data RealFn = 
    RealFn
    {
        rFn_withAccuracy :: Accuracy -> PolyBall,
        rFn_rough :: PolyBall
    }

instance Show RealFn where
    show (RealFn withAccuracy _) =
        show $ withAccuracy (bits 100)

data ApproxRealFn = ApproxRealFn Accuracy RealFn

instance HasApproximate RealFn where
    type Approximate RealFn = ApproxRealFn
    getApproximate = ApproxRealFn

instance Show ApproxRealFn where
    show (ApproxRealFn ac (RealFn withAccuracy _)) =
        show $ getApproximate ac $ withAccuracy ac
    
{- basic function operations -} 

instance
    RealUnaryFnA (->) RealFn
    where
    type UnaryFnIn RealFn = Rational
    type UnaryFnOut RealFn = CauchyReal
    getDomainUnaryFnA =
        arr $ ball_domain . rFn_rough
    constUnaryFnA (dom, value) =
        RealFn withAccuracy (withAccuracy (bits 0))
        where
        withAccuracy acc = constUnaryFnA (dom, cauchyReal2ball value acc)
    projUnaryFnA dom =
        RealFn withAccuracy (withAccuracy (bits 0))
        where
        withAccuracy =
            seqByPrecision2CauchySeq $ \p ->
                runWithPrecisionPolicy (proc () -> projUnaryFnA -< dom) (ppKeepExact p) ()
    evalOnIntervalUnaryFnA =
        error "UnaryChebSparse.RealFn evalOnIntervalUnaryFnA not implemented yet"
    evalAtInPointUnaryFnA (f, x) =
        evalAtOutPointUnaryFnA (f, convert x)
    evalAtOutPointUnaryFnA (RealFn fR f0, xR) =
        newCRA ([], Nothing, resWithAcc)
        where
        resWithAcc acc =
            keepIncreasingEffort' (evalAtOutPointUnaryFnA (f0,x0)) (zip fAccuracies fSeq0) (zip xAccuracies xSeq0)
            where
            fSeq0 = f0 : (map fR [(acc-1)..])
            fAccuracies = (bits 0) : [(acc-1)..]
            xSeq0@(x0:_) = map (cauchyReal2ball xR) [acc..]
            xAccuracies = [acc..]
            keepIncreasingEffort' resCurrent fSeq@((fA,_f):_) xSeq@((xA,x):_) =
                maybeTrace (
                    "evalAtOutPointUnaryFnA.keepIncreasingEffort:"
                    ++ "\n accuracy for f = " ++ show fA
                    ++ "\n accuracy for x = " ++ show xA
                    ++ "\n x = " ++ show x
                    ++ "\n resCurrent = " ++ show resCurrent
                    ++ "\n getAccuracy resCurrent = " ++ show (getAccuracy resCurrent)
                    ++ "\n requested accuracy = " ++ show acc
                ) $
                keepIncreasingEffort resCurrent fSeq xSeq
            keepIncreasingEffort' _ _ _ = error "keepIncreasingEffort'"
            keepIncreasingEffort resCurrent fSeq@((_,f):fRest@((_,fNext):_)) xSeq@((_,x):xRest@((_,xNext):_)) 
                | accCurrent >= acc = resCurrent
                | accNextX > accCurrent =
                    keepIncreasingEffort' resNextX fSeq xRest
                | accNextF > accCurrent =
                    keepIncreasingEffort' resNextF fRest xSeq
                | otherwise = 
                    keepIncreasingEffort' resNextXNextF fRest xRest
                where
                accCurrent = getAccuracy resCurrent
                resNextX = evalAtOutPointUnaryFnA (f,xNext)
                accNextX = getAccuracy resNextX
                resNextF = evalAtOutPointUnaryFnA (fNext,x)
                accNextF = getAccuracy resNextF
                resNextXNextF = evalAtOutPointUnaryFnA (fNext,xNext)
            keepIncreasingEffort _ _ _ =
                error "internal error in FnReps.Polynomial.UnaryCheb.RealFn RealUnaryFnA.evalAtOutPointUnaryFnA"

{- pointwise arithmetic -} 

instance CanNegA (->) RealFn where
    negA = lift1 negA 

instance CanNegSameType RealFn

{-
    TODO
    Recompute when the result is not sufficiently accurate.
    Use the same method as for CauchyReal.
-}

instance CanAddA (->) RealFn RealFn where
    addA = lift2 addA

instance CanAddThis RealFn RealFn
instance CanAddSameType RealFn

instance CanSub RealFn RealFn
instance CanSubThis RealFn RealFn
instance CanSubSameType RealFn
        
instance CanMulA (->) RealFn RealFn where
    mulA = lift2 mulA

instance CanMulBy RealFn RealFn
instance CanMulSameType RealFn

{- Mixed operations with Integer -}
    
instance CanAddA (->) RealFn Integer where
    type AddTypeA (->) RealFn Integer = RealFn
    addA (a, n) = lift1 (+n) a
    
instance CanAddA (->) Integer RealFn where
    type AddTypeA (->) Integer RealFn = RealFn
    addA (n, a) = lift1 (+n) a

instance CanAddThis RealFn Integer

instance CanSub RealFn Integer
instance CanSubThis RealFn Integer

instance CanSubA (->) Integer RealFn where
    type SubTypeA (->) Integer RealFn = RealFn
    subA (n, a) = addA (n, neg a)

instance CanMulA (->) RealFn Integer where
    type MulTypeA (->) RealFn Integer = RealFn
    mulA (a, n) = lift1 (*n) a
    
instance CanMulA (->) Integer RealFn where
    type MulTypeA (->) Integer RealFn = RealFn
    mulA (n, a) = lift1 (*n) a

instance CanMulBy RealFn Integer

instance CanDivA (->) RealFn Integer where
    type DivTypeA (->) RealFn Integer = RealFn
    divA (a, n) = lift1 (/n) a
    
instance CanDivBy RealFn Integer

{- Mixed operations with Rational -}
    
instance CanAddA (->) RealFn Rational where
    type AddTypeA (->) RealFn Rational = RealFn
    addA (a, n) = lift1 (+n) a
    
instance CanAddA (->) Rational RealFn where
    type AddTypeA (->) Rational RealFn = RealFn
    addA (n, a) = lift1 (+n) a

instance CanAddThis RealFn Rational

instance CanSub RealFn Rational
instance CanSubThis RealFn Rational

instance CanSubA (->) Rational RealFn where
    type SubTypeA (->) Rational RealFn = RealFn
    subA (n, a) = addA (n, neg a)

instance CanMulA (->) RealFn Rational where
    type MulTypeA (->) RealFn Rational = RealFn
    mulA (a, n) = lift1 (*n) a
    
instance CanMulA (->) Rational RealFn where
    type MulTypeA (->) Rational RealFn = RealFn
    mulA (n, a) = lift1 (*n) a

instance CanMulBy RealFn Rational

instance CanDivA (->) RealFn Rational where
    type DivTypeA (->) RealFn Rational = RealFn
    divA (a, n) = lift1 (/n) a
    
instance CanDivBy RealFn Rational

{- Mixed operations with MPBall -}
    
instance CanAddA (->) RealFn MPBall where
    type AddTypeA (->) RealFn MPBall = RealFn
    addA (a, n) = lift1 (+n) a
    
instance CanAddA (->) MPBall RealFn where
    type AddTypeA (->) MPBall RealFn = RealFn
    addA (n, a) = lift1 (+n) a

instance CanAddThis RealFn MPBall

instance CanSub RealFn MPBall
instance CanSubThis RealFn MPBall

instance CanSubA (->) MPBall RealFn where
    type SubTypeA (->) MPBall RealFn = RealFn
    subA (n, a) = addA (n, neg a)

instance CanMulA (->) RealFn MPBall where
    type MulTypeA (->) RealFn MPBall = RealFn
    mulA (a, n) = lift1 (*n) a
    
instance CanMulA (->) MPBall RealFn where
    type MulTypeA (->) MPBall RealFn = RealFn
    mulA (n, a) = lift1 (*n) a

instance CanMulBy RealFn MPBall

instance CanDivA (->) RealFn MPBall where
    type DivTypeA (->) RealFn MPBall = RealFn
    divA (a, n) = lift1 (/n) a
    
instance CanDivBy RealFn MPBall



lift1 :: 
    (PolyBall -> PolyBall) -> 
    (RealFn -> RealFn)
lift1 opB (RealFn withAccuracy withAccuracy0) =
    RealFn (\a -> opB $ withAccuracy a) (opB withAccuracy0) 

lift2 :: 
    ((PolyBall, PolyBall) -> PolyBall) -> 
    ((RealFn, RealFn) -> RealFn)
lift2 opB (RealFn withAccuracyA withAccuracy0A, RealFn withAccuracyB withAccuracy0B) =
    RealFn (\a -> opB (withAccuracyA a, withAccuracyB a)) (opB (withAccuracy0A, withAccuracy0B)) 
    
