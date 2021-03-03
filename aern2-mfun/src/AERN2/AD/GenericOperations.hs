module AERN2.AD.GenericOperations where

import MixedTypesNumPrelude
import AERN2.AD.Type

instance 
    (CanDiv a a, CanSubSameType a, CanMulSameType a, HasIntegers a, CanAddSameType a,
    CanSubSameType (DivType a a), CanSubSameType (DivTypeNoCN a a)) =>
    CanDiv (Differential a) (Differential a)
    where
    type DivTypeNoCN (Differential a) (Differential a) = Differential (DivTypeNoCN a a)
    type DivType     (Differential a) (Differential a) = Differential (DivType a a)
    divideNoCN a b =
        case min (order a) (order b) of
            2 -> 
                let
                    dtDiff    = (dxt a * x b - x a * dxt b)
                    ySqrd     = x b * x b
                    exp0      = d2x a * x b + dxt a * dx b - dxt b * dx a - x a * d2x b
                    (ta :: a) = convertExactly 2
                in
                OrderTwo  (x a /! x b) ((dx a * x b - x a * dx b)/!ySqrd) (dtDiff/!ySqrd) 
                          (exp0/!ySqrd - (ta*dx b * dtDiff) /! (ySqrd * x b))
            1 -> OrderOne  (x a /! x b) ((dx a * x b - x a * dx b)/!(x b * x b)) 
            0 -> OrderZero (x a /! x b)
    divide a b =
        case min (order a) (order b) of
            2 -> 
                let
                    dtDiff    = (dxt a * x b - x a * dxt b)
                    ySqrd     = x b * x b
                    exp0      = d2x a * x b + dxt a * dx b - dxt b * dx a - x a * d2x b
                    (ta :: a) = convertExactly 2
                in
                OrderTwo  (x a / x b) ((dx a * x b - x a * dx b)/ySqrd) (dtDiff/ySqrd) 
                          (exp0/ySqrd - (ta*dx b * dtDiff) / (ySqrd * x b))
            1 -> OrderOne  (x a / x b) ((dx a * x b - x a * dx b)/(x b * x b)) 
            0 -> OrderZero (x a / x b)

instance 
    (CanExpSameType a, CanMulSameType a, CanAddSameType a) =>
    CanExp (Differential a)
    where
    type ExpType (Differential a) = Differential a
    exp (OrderZero x)           = OrderZero (exp x)
    exp (OrderOne x dx)         = OrderOne  (exp x) (dx * exp x)
    exp (OrderTwo x dx dxt d2x) = OrderTwo  (exp x) (dx * exp x) (dxt * exp x) ((dxt * dx + d2x) * exp x)


{- TODO: fix this in AERN2.MP -}
clampedCos :: (CanSinCosSameType a, CanMinMaxSameType a, HasIntegers a) => a -> a
clampedCos (x :: a) = max (convertExactly $ -1 :: a) $ min ((convertExactly 1) :: a) (cos x)

clampedSin :: (CanSinCosSameType a, CanMinMaxSameType a, HasIntegers a) => a -> a
clampedSin (x :: a) = max (convertExactly $ -1 :: a) $ min ((convertExactly 1) :: a) (sin x)

instance 
    (CanSinCosSameType a, CanMulSameType a, CanNegSameType a, CanSubSameType a, CanAddSameType a, HasIntegers a, CanMinMaxSameType a) =>
    CanSinCos (Differential a)
    where
    type SinCosType (Differential a) = Differential a
    cos (OrderZero x)            = OrderZero (clampedCos x)
    cos (OrderOne  x dx)         = OrderOne  (clampedCos x) (-dx * clampedSin x)
    cos (OrderTwo  x dx dxt d2x) = OrderTwo  (clampedCos x) (-dx * clampedSin x) (-dxt * clampedSin x) 
                                             (-dxt * dx * clampedCos x - d2x * clampedSin x)
    
    sin (OrderZero x)            = OrderZero (clampedSin x)
    sin (OrderOne x dx)          = OrderOne  (clampedSin x) (dx * clampedCos x)
    sin (OrderTwo x dx dxt d2x)  = OrderTwo  (clampedSin x) (dx * clampedCos x) (dxt * clampedCos x)
                                             (d2x * clampedCos x - dxt * dx * clampedSin x)

instance 
    (CanAddSameType a) => 
    CanAddAsymmetric (Differential a) (Differential a)
    where
    type AddType (Differential a) (Differential a) = Differential a
    add a b =
        case min (order a) (order b) of
            2 -> OrderTwo  (x a + x b) (dx a + dx b) (dxt a + dxt b) (d2x a + d2x b)
            1 -> OrderOne  (x a + x b) (dx a + dx b)
            0 -> OrderZero (x a + x b)

instance 
    (CanMulSameType a, CanAddSameType a, HasIntegers a) =>
    CanMulAsymmetric (Differential a) (Differential a)
    where
    type MulType (Differential a) (Differential a) = Differential a
    mul a b =
        case min (order a) (order b) of
            2 -> OrderTwo  (x a * x b) (dx a * x b + x a * dx b) (x a * dxt b + x b * dxt a) 
                           (x a * d2x b + dx a * dxt b + dxt a * dx b + d2x a * x b)
            1 -> OrderOne  (x a * x b) (dx a * x b + x a * dx b)
            0 -> OrderZero (x a * x b)

instance 
    (CanSubSameType a) =>
    CanSub (Differential a) (Differential a)
    where
    type SubType (Differential a) (Differential a) = Differential a
    sub a b = 
        case min (order a) (order b) of
            2 -> OrderTwo  (x a - x b) (dx a - dx b) (dxt a - dxt b) (d2x a - d2x b)
            1 -> OrderOne  (x a - x b) (dx a - dx b)
            0 -> OrderZero (x a - x b)

instance 
    (CanNeg a) =>
    CanNeg (Differential a)
    where
    type NegType (Differential a) = Differential (NegType a)
    negate = fmap negate

instance 
    (CanSqrtSameType a, CanMulSameType a, CanNegSameType a, CanSubSameType a, CanAddSameType a, HasIntegers a, CanDivSameType a) =>
    CanSqrt (Differential a)
    where
    type SqrtType (Differential a) = Differential a
    sqrt (OrderZero x)             = OrderZero  (sqrt x)
    sqrt (OrderOne x dx)           = OrderOne   (sqrt x) (dx / (ta * sqrt x)) where (ta :: a) = convertExactly 2
    sqrt (OrderTwo x dx dxt d2x)   = OrderTwo   (sqrt x) (dx / (ta * sqrt x)) (dxt / (ta * sqrt x))
                                        ((dx / ta * (-dx / (ta * x * sqrt x))) + (d2x / (ta * sqrt x)))
                                        where (ta :: a) = convertExactly 2

-- instance
--     (CanMinMaxSameType a, HasIntegers a) =>
--     CanMinMaxAsymmetric (Differential a) (Differential a)
--     where
--     type MinMaxType (Differential a) (Differential a) = Differential a
--     min a b = 
--         case min (order a) (order b) of
--             2 -> OrderTwo  (min (x a) (x b)) (min (dx a) (dx b)) (min (dxt a) (dxt b)) 
--                            (min (d2x a) (d2x b))
--             1 -> OrderOne  (min (x a) (x b)) (min (dx a) (dx b))
--             0 -> OrderZero (min (x a) (x b))
--     max a b =
--         case min (order a) (order b) of
--             2 -> OrderTwo  (max (x a) (x b)) (max (dx a) (dx b)) (max (dxt a) (dxt b)) 
--                            (max (d2x a) (d2x b))
--             1 -> OrderOne  (max (x a) (x b)) (max (dx a) (dx b))
--             0 -> OrderZero (max (x a) (x b))

-- instance
--     (CanAbsSameType a) =>
--     CanAbs (Differential a)
--     where
--     type AbsType (Differential a) = Differential a
--     abs = fmap abs

