module AERN2.AD.MPBallOperations where

import MixedTypesNumPrelude

import AERN2.AD.Type
import AERN2.AD.GenericOperations
import AERN2.MP.Ball

instance 
    CanAddAsymmetric (CN MPBall) (Differential (CN MPBall))
    where
    type AddType (CN MPBall) (Differential (CN MPBall)) = Differential (CN MPBall)
    add a b = add (differential (order b) a) b

instance 
    CanAddAsymmetric (Differential (CN MPBall)) (CN MPBall)
    where
    type AddType (Differential (CN MPBall)) (CN MPBall) = Differential (CN MPBall)
    add b a = add b (differential (order b) a)

instance 
    CanSub (CN MPBall) (Differential (CN MPBall))
    where
    type SubType (CN MPBall) (Differential (CN MPBall)) = Differential (CN MPBall)
    sub a b = sub (differential (order b) a) b

instance 
    CanSub (Differential (CN MPBall)) (CN MPBall) 
    where
    type SubType (Differential (CN MPBall)) (CN MPBall) = Differential (CN MPBall)
    sub b a = sub b (differential (order b) a)

instance 
    CanMulAsymmetric (CN MPBall) (Differential (CN MPBall))
    where
    type MulType (CN MPBall) (Differential (CN MPBall)) = Differential (CN MPBall)
    mul a b = mul (differential (order b) a) b

instance 
    CanMulAsymmetric (Differential (CN MPBall)) (CN MPBall)
        where
        type MulType (Differential (CN MPBall)) (CN MPBall) = Differential (CN MPBall)
        mul b a = mul b (differential (order b) a)

instance 
    CanDiv (CN MPBall) (Differential (CN MPBall))
    where
    type DivTypeNoCN (CN MPBall) (Differential (CN MPBall)) = Differential (CN MPBall)
    type DivType     (CN MPBall) (Differential (CN MPBall)) = Differential (CN MPBall)
    divideNoCN a b = divideNoCN (differential (order b) a) b
    divide     a b = divide     (differential (order b) a) b

instance 
    CanDiv (Differential (CN MPBall)) (CN MPBall) 
    where
    type DivTypeNoCN (Differential (CN MPBall)) (CN MPBall) = Differential (CN MPBall)
    type DivType     (Differential (CN MPBall)) (CN MPBall) = Differential (CN MPBall)
    divideNoCN b a = divideNoCN b (differential (order b) a)
    divide     b a = divide     b (differential (order b) a)

instance 
    CanAddAsymmetric (CN Integer) (Differential (CN MPBall))
    where
    type AddType (CN Integer) (Differential (CN MPBall)) = Differential (CN MPBall)
    add a b = add (differential (order b) ((fmap mpBall) a)) b

instance 
    CanAddAsymmetric (Differential (CN MPBall)) (CN Integer)
    where
    type AddType (Differential (CN MPBall)) (CN Integer) = Differential (CN MPBall)
    add b a = add b (differential (order b) ((fmap mpBall) a))

instance 
    CanSub (CN Integer) (Differential (CN MPBall))
    where
    type SubType (CN Integer) (Differential (CN MPBall)) = Differential (CN MPBall)
    sub a b = sub (differential (order b) ((fmap mpBall) a)) b

instance 
    CanSub (Differential (CN MPBall)) (CN Integer)
    where
    type SubType (Differential (CN MPBall)) (CN Integer) = Differential (CN MPBall)
    sub b a = sub b (differential (order b) ((fmap mpBall) a))

instance 
    CanMulAsymmetric (CN Integer) (Differential (CN MPBall))
    where
    type MulType (CN Integer) (Differential (CN MPBall)) = Differential (CN MPBall)
    mul a b = mul (differential (order b) ((fmap mpBall) a)) b

instance 
    CanMulAsymmetric (Differential (CN MPBall)) (CN Integer)
        where
        type MulType (Differential (CN MPBall)) (CN Integer) = Differential (CN MPBall)
        mul b a = mul b (differential (order b) ((fmap mpBall) a))

instance 
    CanDiv (CN Integer) (Differential (CN MPBall))
    where
    type DivTypeNoCN (CN Integer) (Differential (CN MPBall)) = Differential (CN MPBall)
    type DivType     (CN Integer) (Differential (CN MPBall)) = Differential (CN MPBall)
    divideNoCN a b = divideNoCN (differential (order b) ((fmap mpBall) a)) b
    divide     a b = divide     (differential (order b) ((fmap mpBall) a)) b
    
instance 
    CanDiv (Differential (CN MPBall)) (CN Integer)
    where
    type DivTypeNoCN (Differential (CN MPBall)) (CN Integer) = Differential (CN MPBall)
    type DivType     (Differential (CN MPBall)) (CN Integer) = Differential (CN MPBall)
    divideNoCN b a = divideNoCN b (differential (order b) ((fmap mpBall) a))
    divide     b a = divide     b (differential (order b) ((fmap mpBall) a))        


instance 
    CanPow (Differential (CN MPBall)) Integer
    where
        type PowTypeNoCN (Differential (CN MPBall)) Integer = (Differential (CN MPBall))
        type PowType     (Differential (CN MPBall)) Integer = (Differential (CN MPBall))
        powNoCN = pow
        pow (OrderZero x) n =
            OrderZero p
            where
            rawP = pow x n
            p = if even n then abs rawP else rawP -- TODO: this should go in MPBall
        pow (OrderOne x dx) n = 
            OrderOne p dp
            where
            nEven = even n
            rawP' = pow x (n - 1)
            rawP  = x * rawP'
            p  = if nEven then abs rawP else rawP  -- TODO: this should go in MPBall
            dp = n * dx * if nEven then rawP' else abs rawP'
        pow (OrderTwo x dx dxt d2x) n =
            OrderTwo p dp dpt d2p
            where
            nEven  = even n
            rawP'' = pow x (n - 2)
            rawP'  = x * rawP''
            rawP   = x * rawP'
            p    = if nEven then abs rawP else rawP -- TODO: this should go in MPBall
            xnm1 = if nEven then rawP' else abs rawP'
            dp   = n * dx  * xnm1
            dpt  = n * dxt * xnm1
            xnm2 = if nEven then abs rawP'' else rawP''
            d2p  = n*((n - 1)*dx*dxt*xnm2 + d2x*xnm1)

            
            