{-# OPTIONS_GHC -Wno-orphans #-}
module AERN2.AD.GenericOperations where

import MixedTypesNumPrelude
import AERN2.AD.Type

instance 
    (CanDiv a a, CanSubSameType a, CanMulSameType a, CanMulBy a Integer, CanAddSameType a,
    CanSubSameType (DivType a a)) 
    =>
    CanDiv (Differential a) (Differential a)
    where
    type DivType     (Differential a) (Differential a) = Differential (DivType a a)
    divide a b =
        case min (order a) (order b) of
            2 -> 
                let
                    dtDiff    = (a_dxt * b_x - a_x * b_dxt)
                    ySqrd     = b_x * b_x
                    exp0      = a_d2x * b_x + a_dxt * b_dx - b_dxt * a_dx - a_x * b_d2x
                in
                OrderTwo  (a_x / b_x) ((a_dx * b_x - a_x * b_dx)/ySqrd) (dtDiff/ySqrd) 
                          (exp0/ySqrd - (2*b_dx * dtDiff) / (ySqrd * b_x))
            1 -> OrderOne  (a_x / b_x) ((a_dx * b_x - a_x * b_dx)/(b_x * b_x)) 
            0 -> OrderZero (a_x / b_x)
            _ -> error "illegal Differential order"
        where
        a_x = diff_x a
        b_x = diff_x b
        a_dx = diff_dx a
        b_dx = diff_dx b
        a_dxt = diff_dxt a
        b_dxt = diff_dxt b
        a_d2x = diff_d2x a
        b_d2x = diff_d2x b

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
    (CanSinCosSameType a, CanMulSameType a, CanNegSameType a, CanSubSameType a, CanAddSameType a
    , HasIntegers a, CanMinMaxSameType a) 
    =>
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
            2 -> OrderTwo  (a_x + b_x) (a_dx + b_dx) (a_dxt + b_dxt) (a_d2x + b_d2x)
            1 -> OrderOne  (a_x + b_x) (a_dx + b_dx)
            0 -> OrderZero (a_x + b_x)
            _ -> error "illegal Differential order"
        where
        a_x = diff_x a
        b_x = diff_x b
        a_dx = diff_dx a
        b_dx = diff_dx b
        a_dxt = diff_dxt a
        b_dxt = diff_dxt b
        a_d2x = diff_d2x a
        b_d2x = diff_d2x b
        
instance 
    (CanMulSameType a, CanAddSameType a) =>
    CanMulAsymmetric (Differential a) (Differential a)
    where
    type MulType (Differential a) (Differential a) = Differential a
    mul a b =
        case min (order a) (order b) of
            2 -> OrderTwo  (a_x * b_x) (a_dx * b_x + a_x * b_dx) (a_x * b_dxt + b_x * a_dxt) 
                           (a_x * b_d2x + a_dx * b_dxt + a_dxt * b_dx + a_d2x * b_x)
            1 -> OrderOne  (a_x * b_x) (a_dx * b_x + a_x * b_dx)
            0 -> OrderZero (a_x * b_x)
            _ -> error "illegal Differential order"
        where
        a_x = diff_x a
        b_x = diff_x b
        a_dx = diff_dx a
        b_dx = diff_dx b
        a_dxt = diff_dxt a
        b_dxt = diff_dxt b
        a_d2x = diff_d2x a
        b_d2x = diff_d2x b

instance 
    (CanMulSameType a, CanAddSameType a, CanPowBy a a, CanSubThis a Integer, CanLogSameType a, CanDivSameType a) =>
    CanPow (Differential a) (Differential a)
    where
    type PowType        (Differential a) (Differential a) = (Differential a)
    pow a b =
        case min (order a) (order b) of
            2 -> OrderOne  (a_x ^ b_x) ((a_x ^ (b_x - 1)) * (b_x * a_dx + a_x * (log a_x) * b_dx)) --FIXME: Add real OrderTwo definition here
            -- 2 -> OrderTwo   (x a ^ x b) 
            --                 ((x a ^ (x b - (convertExactly 1 :: a))) * (x b * dx a + x a * log (x a) * dx b)) 
            --                 ((x a ^ (x b - (convertExactly 1 :: a))) * (x b * dxt a + x a * log (x a) * dxt b))
            --                 (x a)
                            
            --                 where
            --                     ta = convertExactly 2 :: a
            1 -> OrderOne  (a_x ^ b_x) ((a_x ^ (b_x - 1)) * (b_x * a_dx + a_x * (log a_x) * b_dx))
            0 -> OrderZero (a_x ^ b_x)
            _ -> undefined
        where
        a_x = diff_x a
        b_x = diff_x b
        a_dx = diff_dx a
        b_dx = diff_dx b
        -- a_dxt = diff_dxt a
        -- b_dxt = diff_dxt b
        -- a_d2x = diff_d2x a
        -- b_d2x = diff_d2x b

instance 
    (CanSubSameType a) =>
    CanSub (Differential a) (Differential a)
    where
    type SubType (Differential a) (Differential a) = Differential a
    sub a b =
        case min (order a) (order b) of
            2 -> OrderTwo  (a_x - b_x) (a_dx - b_dx) (a_dxt - b_dxt) (a_d2x - b_d2x)
            1 -> OrderOne  (a_x - b_x) (a_dx - b_dx)
            0 -> OrderZero (a_x - b_x)
            _ -> error "illegal Differential order"
        where
        a_x = diff_x a
        b_x = diff_x b
        a_dx = diff_dx a
        b_dx = diff_dx b
        a_dxt = diff_dxt a
        b_dxt = diff_dxt b
        a_d2x = diff_d2x a
        b_d2x = diff_d2x b

instance 
    (CanMulBy (Differential a) Integer) =>
    CanNeg (Differential a)
    where
    type NegType (Differential a) = Differential a
    negate x = (-1) * x

instance 
    (CanSqrtSameType a, CanMulSameType a, CanNegSameType a, CanAddSameType a, CanMulBy a Integer, CanRecipSameType a) 
    =>
    CanSqrt (Differential a)
    where
    type SqrtType (Differential a) = Differential a
    sqrt (OrderZero x)             = OrderZero (sqrt x)
    sqrt (OrderOne x dx)           = OrderOne  (sqrt x) (dx * sqrtx')
                                        where sqrtx' = recip (2 * sqrt x)
    sqrt (OrderTwo x dx dxt d2x)   = OrderTwo  (sqrt x) (dx * sqrtx') (dxt * sqrtx') 
                                               ((d2x * sqrtx') + (dx * dxt * sqrtx'')) 
                                        where 
                                        sqrtx'  = recip (2 * sqrt x)
                                        sqrtx'' = negate $ recip (4 * x * sqrt x)
                                            -- sqrtx'  == 1 / (2 * sqrt(x))
                                            -- sqrtx'' == -1 / (4 * x * sqrt(x)) == -1 / (4 * x^(3/2))

instance
    CanMinMaxSameType a =>
    CanMinMaxAsymmetric (Differential a) (Differential a)
    where
    type MinMaxType (Differential a) (Differential a) = Differential a
    min a b = OrderZero (min (diff_x a) (diff_x b))
    max a b = OrderZero (max (diff_x a) (diff_x b))


-- instance
--     (CanMinMaxSameType a, HasIntegers a) =>
--     CanMinMaxAsymmetric (Differential a) (Differential a)
--     where
--     type MinMaxType (Differential a) (Differential a) = Differential a
--     min a b = 
--         case min (order a) (order b) of
--             2 -> OrderTwo  (min (a_x) (b_x)) (min (a_dx) (b_dx)) (min (a_dxt) (b_dxt)) 
--                            (min (a_d2x) (b_d2x))
--             1 -> OrderOne  (min (a_x) (b_x)) (min (a_dx) (b_dx))
--             0 -> OrderZero (min (a_x) (b_x))
--     max a b =
--         case min (order a) (order b) of
--             2 -> OrderTwo  (max (a_x) (b_x)) (max (a_dx) (b_dx)) (max (a_dxt) (b_dxt)) 
--                            (max (a_d2x) (b_d2x))
--             1 -> OrderOne  (max (a_x) (b_x)) (max (a_dx) (b_dx))
--             0 -> OrderZero (max (a_x) (b_x))

-- instance
--     (CanAbsSameType a) =>
--     CanAbs (Differential a)
--     where
--     type AbsType (Differential a) = Differential a
--     abs = fmap abs

