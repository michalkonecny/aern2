{-# LANGUAGE Arrows, FlexibleInstances, UndecidableInstances #-}
module AERN2.Num.Interval where

import AERN2.Num.Operations
import Control.Arrow

import AERN2.Num.MPBall
import AERN2.Num.CauchyReal

data Interval a = Interval a a
    deriving (Show)
    
pm :: () => a -> a -> Interval a
pm = undefined

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
                l <- minA -< (l, r0l1)
                l <- minA -< (l, r0r1)
                r <- maxA -< (l0l1,l0r1)
                r <- maxA -< (r, r0l1)
                r <- maxA -< (r, r0r1)
                returnA -< Interval l r
                
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

{- Limits -} 

instance (Arrow to) => CanLimitA to (Interval MPBall) where
        type LimitTypeA to (Interval MPBall) = CauchyReal 
        limA = undefined          

instance (Arrow to, CanAsCauchyRealA to a) => CanLimitA to (Interval (AsCauchyReal a)) where
        type LimitTypeA to (Interval (AsCauchyReal a)) = AsCauchyReal a
        limA = undefined   
        
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

instance (Arrow to, CanAbsSameTypeA to r2, CanReadAsCauchyRealA to r1, CanAsCauchyRealA to r2,
          CanCombineCRsA to r1 r2) => CanPlusMinusA to (AsCauchyReal r1) (AsCauchyReal r2) where
        type PlusMinusTypeA to (AsCauchyReal r1) (AsCauchyReal r2) = Interval (AsCauchyReal (CombinedCRs to r1 r2))
        plusMinusA = proc (x, y) ->
                        do
                        absY <- absA -< y
                        l <- subA -< (x,absY)
                        r <- addA -< (x,absY)
                        returnA -< Interval l r
                                                       