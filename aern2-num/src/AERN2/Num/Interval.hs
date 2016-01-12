{-# LANGUAGE Arrows, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
module AERN2.Num.Interval where

import AERN2.Num.Operations
import Control.Arrow

import AERN2.Num.Accuracy
import AERN2.Num.MPBall
import AERN2.Num.CauchyReal

data Interval a = Interval a a
    deriving (Show)
    
singleton :: a -> Interval a
singleton a = Interval a a

widthA :: (Arrow to, CanSubA to a a) => Interval a `to` SubTypeA to a a 
widthA = proc(Interval l r) ->
                do
                w <- subA -< (r,l)
                returnA -< w
                
width :: (CanSub a a) => Interval a -> SubTypeA (->) a a
width = widthA
    
{- Interval-Interval arithmetic operations -}

instance (CanNegA to a) =>
        CanNegA to (Interval a)
        where
        type NegTypeA to (Interval a) = Interval (NegTypeA to a)
        negA = proc (Interval l0 r0) ->
                do
                r <- negA -< l0
                l <- negA -< r0
                returnA -< Interval l r

instance (CanNegSameTypeA to a) =>
        CanNegSameTypeA to (Interval a)

instance (CanRecipA to a) =>
        CanRecipA to (Interval a)
        where
        type RecipTypeA to (Interval a) = Interval (RecipTypeA to a)
        recipA = proc (Interval l0 r0) ->
                do --TODO support division by zero
                r <- recipA -< l0
                l <- recipA -< r0
                returnA -< Interval l r

instance (CanRecipSameTypeA to a) =>
        CanRecipSameTypeA to (Interval a)

    
instance (CanAddA to a b) =>
        CanAddA to (Interval a) (Interval b)
        where
        type AddTypeA to (Interval a) (Interval b) = Interval (AddTypeA to a b)
        addA = proc (Interval l0 r0, Interval l1 r1) ->
                do
                l <- addA -< (l0, l1)
                r <- addA -< (r0, r1)
                returnA -< Interval l r

instance (CanAddThisA to a b) =>
        CanAddThisA to (Interval a) (Interval b)
instance (CanAddSameTypeA to a) =>
        CanAddSameTypeA to (Interval a)

instance (CanSubA to a b) =>
        CanSubA to (Interval a) (Interval b)
        where
        type SubTypeA to (Interval a) (Interval b) = Interval (SubTypeA to a b)
        subA = proc (Interval l0 r0, Interval l1 r1) ->
                do
                l <- subA -< (l0, r1)
                r <- subA -< (r0, l1)
                returnA -< Interval l r

instance (CanSubThisA to a b) =>
        CanSubThisA to (Interval a) (Interval b)
instance (CanSubSameTypeA to a) =>
        CanSubSameTypeA to (Interval a)

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

instance (CanMulByA to a b, CanMinMaxSameTypeA to a) =>
        CanMulByA to (Interval a) (Interval b)
instance (CanMulSameTypeA to a, CanMinMaxSameTypeA to a) =>
        CanMulSameTypeA to (Interval a)


instance (CanMulA to a b, CanMinMaxSameTypeA to (MulTypeA to a b), CanRecipSameTypeA to b) =>
        CanDivA to (Interval a) (Interval b)
        where
        type DivTypeA to (Interval a) (Interval b) = Interval (MulTypeA to a b)
        divA = proc (i0, Interval l1 r1) ->
                do --TODO support division by zero
                l1Inv <- recipA -< l1
                r1Inv <- recipA -< r1
                let i1 = Interval r1Inv l1Inv
                ret <- mulA -< (i0,i1)
                returnA -< ret

instance (CanMulByA to a b, CanMinMaxSameTypeA to a, CanRecipSameTypeA to b) =>
        CanDivByA to (Interval a) (Interval b)
instance (CanMulSameTypeA to a, CanMinMaxSameTypeA to a, CanRecipSameTypeA to a) =>
        CanDivSameTypeA to (Interval a)

{- Interval-Integer arithmetic operations -}

    
instance (CanAddA to a Integer) =>
        CanAddA to (Interval a) Integer
        where
        type AddTypeA to (Interval a) Integer = Interval (AddTypeA to a Integer)
        addA = proc (Interval l0 r0, n) ->
                do
                l <- addA -< (l0, n)
                r <- addA -< (r0, n)
                returnA -< Interval l r

instance (CanAddA to Integer b) =>
        CanAddA to Integer (Interval b)
        where
        type AddTypeA to Integer (Interval b) = Interval (AddTypeA to Integer b)
        addA = proc (n, Interval l1 r1) ->
                do
                l <- addA -< (n, l1)
                r <- addA -< (n, r1)
                returnA -< Interval l r

instance (CanAddThisA to a Integer) =>
        CanAddThisA to (Interval a) Integer

instance (ArrowChoice to, CanAddA to a Integer) =>
        CanSubA to (Interval a) Integer
instance (ArrowChoice to, CanAddThisA to a Integer) =>
        CanSubThisA to (Interval a) Integer

instance (ArrowChoice to, CanAddA to Integer a, CanNegSameTypeA to a) =>
    CanSubA to Integer (Interval a)

instance (ArrowChoice to, CanMulA to Integer a) => --TODO: could get rid of arrow choice at the expense of adding CanMinMaxA.
         CanMulA to Integer (Interval a)           --      but this is potentially less efficient, and is currently not supported
         where                                     --      by CauchyReal
         type MulTypeA to Integer (Interval a) = Interval (MulTypeA to Integer a)
         mulA = proc(n, Interval l r) ->
                do
                nl <- mulA -< (n,l)
                nr <- mulA -< (n,r)
                case n >= 0 of
                        True  -> returnA -< Interval nl nr
                        False -> returnA -< Interval nr nl
                
{- TODO: provide also mixed multiplication and division

-}

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
        
instance (CanAsCauchyRealA to a) => CanLimitA to (Interval (AsCauchyReal a)) where
    type LimitTypeA to (Interval (AsCauchyReal a)) = AsCauchyReal a
    limListA = proc(xs) -> newCRA -< ([],Nothing, fn xs)
        where
        fn xs = proc acc ->
            do
            bs <- mapA i2ball -< zip xs (repeat $ acc + 1)
            returnA -< findAccurate acc bs
        findAccurate acc (b:bs) 
            | getAccuracy b >= acc = b
            | otherwise = findAccurate acc bs      
        findAccurate _ [] = error "internal error in AERN2.Num.Interval.limListA"
    limA fnAseq = proc x -> newCRA -< ([], Nothing, getBallA x 0)
        where
        getBallA x n = 
            proc acc ->
                do
                ri <- fnAseq n -< x
                b <- i2ball -< (ri, acc + 1)
                if getAccuracy b >= acc 
                    then returnA -< b
                    else getBallA x (n+1) -< acc 

i2ball :: (CanReadAsCauchyRealA to r) => (Interval (AsCauchyReal r), Accuracy) `to` MPBall         
i2ball = proc(Interval l r,acc) -> 
    do
    lApprox <- getAnswerCRA -< (l,acc)
    rApprox <- getAnswerCRA -< (r,acc)
    returnA -< endpoints2Ball lApprox rApprox


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

                                                       
