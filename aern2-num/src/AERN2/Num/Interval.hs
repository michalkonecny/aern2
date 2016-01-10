{-# LANGUAGE Arrows, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
module AERN2.Num.Interval where

import AERN2.Num.Operations
import Control.Arrow

import AERN2.Num.MPBall
import AERN2.Num.CauchyReal

data Interval a = Interval a a
    deriving (Show)
    
{- Elementary Operations -}
    
instance (CanAddA to a b) =>
        CanAddA to (Interval a) (Interval b)
        where
        type AddTypeA to (Interval a) (Interval b) = Interval (AddTypeA to a b)
        addA = proc (Interval l0 r0, Interval l1 r1) ->
                       do
                       l <- addA -< (l0, l1)
                       r <- addA -< (r0, r1)
                       returnA -< Interval l r

instance (CanSubA to a b) =>
        CanSubA to (Interval a) (Interval b)
        where
        type SubTypeA to (Interval a) (Interval b) = Interval (SubTypeA to a b)
        subA = proc (Interval l0 r0, Interval l1 r1) ->
                do
                l <- subA -< (l0, r1)
                r <- subA -< (r0, l1)
                returnA -< Interval l r

instance (CanMulA to a b, CanMinMaxSameTypeA to (MulTypeA to a b)) =>
        CanMulA to (Interval a) (Interval b)
        where
        type MulTypeA to (Interval a) (Interval b) = Interval (MulTypeA to a b)
        mulA = proc (Interval l0 r0, Interval l1 r1) ->
                do
                l0l1 <- mulA -< (l0,l1)
                l0r1 <- mulA -< (l0,r1)
                r0l1 <- mulA -< (r0,l1)
                r0r1 <- mulA -< (r0,r1)
                l <- minA -< (l0l1,l0r1)
                l' <- minA -< (l, r0l1)
                l'' <- minA -< (l', r0r1)
                r <- maxA -< (l0l1,l0r1)
                r' <- maxA -< (r, r0l1)
                r'' <- maxA -< (r', r0r1)
                returnA -< Interval l'' r''
                
instance (CanMulA to a b, CanMinMaxSameTypeA to (MulTypeA to a b), CanRecipSameTypeA to b) =>
        CanDivA to (Interval a) (Interval b)
        where
        type DivTypeA to (Interval a) (Interval b) = Interval (MulTypeA to a b)
        divA  =  proc (i0, Interval l1 r1) ->
                        do --TODO support division by zero
                        l1Inv <- recipA -< l1
                        r1Inv <- recipA -< r1
                        let i1 = Interval r1Inv l1Inv
                        ret <- mulA -< (i0,i1)
                        returnA -< ret

widthA :: (Arrow to, CanSubA to a a) => Interval a `to` SubTypeA to a a 
widthA = proc(Interval l r) ->
                do
                w <- subA -< (r,l)
                returnA -< w
                
width :: (CanSub a a) => Interval a -> SubTypeA (->) a a
width = widthA

{- Selection -}

class (Arrow to) => CanSelectFromIntervalA to a where
        pickAnyA :: Interval a `to` a
        pickAnyA = proc(Interval l _) -> returnA -< l 

instance (Arrow to) => CanSelectFromIntervalA to MPBall where
        pickAnyA = arr $ \ (Interval l r) -> (l + r)/2
        

instance (Arrow to, CanAsCauchyRealA to a, CanCombineCRsA to a a,
          CanAddSameTypeA to (AsCauchyReal a), CanDivSameTypeA to (AsCauchyReal a)) 
          => CanSelectFromIntervalA to (AsCauchyReal a) where
        pickAnyA = proc(Interval l r) ->
                        do
                        sm <- addA -< (l,r)
                        mid <- divA -< (sm,2)
                        returnA -< mid

{- Limits -} 

{-instance (ArrowChoice to) => CanLimitA to (Interval MPBall) where
        type LimitTypeA to (Interval MPBall) = CauchyReal 
        limA = proc(getApprox) ->
                 do
                 newCRA -< ([], Nothing, findAccurate (makeBall getApprox))
               where
               makeBall :: (Accuracy -> Interval MPBall) -> Accuracy -> MPBall
               makeBall getApprox acc = endpoints2Ball l r
                                        where
                                        (Interval l r) = getApprox acc
               findAccurate n f acc = if getAccuracy (f n) >= acc then
                                        (f n)
                                      else
                                        findAccurate (n + 1) f acc-}
        
instance (Arrow to, CanAsCauchyRealA to a) => CanLimitA to (Interval (AsCauchyReal a)) where
        type LimitTypeA to (Interval (AsCauchyReal a)) = AsCauchyReal a
        limA = proc(getApprox) -> newCRA -< ([],Nothing, findAccurate getApprox 0)
                where
                getApproxBallA getApprox = proc (n,acc1) ->
                                do
                                let (Interval l r) = getApprox n
                                lApprox <- getAnswerCRA -< (l,acc1)
                                rApprox <- getAnswerCRA -< (r,acc1) 
                                returnA -< endpoints2Ball lApprox rApprox
                findAccurate getApprox n = proc(acc) ->
                        do
                        b <- getApproxBallA getApprox -< (n, acc+1) -- TODO more clever strategy than "acc + 1"?
                        if getAccuracy b >= acc then
                                       returnA -< b
                                       else
                                       findAccurate getApprox (n + 1) -< (acc)                      

{- MPBall plus-minus -}

instance (Arrow to) => CanPlusMinusA to MPBall MPBall where
        type PlusMinusTypeA to MPBall MPBall = Interval MPBall
        plusMinusA = proc (x, y) ->
                        do
                        absY <- absA -< y
                        l <- subA -< (x,absY)
                        r <- addA -< (x,absY)
                        returnA -< Interval l r  
                        

{- Cauchy-real plus-minus -}

instance (Arrow to, CanAbsSameTypeA to (AsCauchyReal r2), CanReadAsCauchyRealA to r1, CanAsCauchyRealA to r2,
          CanCombineCRsA to r1 r2) => CanPlusMinusA to (AsCauchyReal r1) (AsCauchyReal r2) where
        type PlusMinusTypeA to (AsCauchyReal r1) (AsCauchyReal r2) = Interval (AsCauchyReal (CombinedCRs to r1 r2))
        plusMinusA = proc (x, y) ->
                        do
                        absY <- absA -< y
                        l <- subA -< (x,absY)
                        r <- addA -< (x,absY)
                        returnA -< Interval l r

                                                       
