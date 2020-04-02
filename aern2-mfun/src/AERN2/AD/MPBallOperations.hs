module AERN2.AD.MPBallOperations where

import MixedTypesNumPrelude

import AERN2.AD.Type hiding (x,dx,dxt,d2x)
import AERN2.AD.GenericOperations ()
import AERN2.MP.Ball

{-- addition --}

instance
    CanAddAsymmetric MPBall (Differential (CN MPBall))
    where
    type AddType MPBall (Differential (CN MPBall)) = Differential (CN MPBall)
    add a b = add (differential (order b) (cn a)) b

instance 
    CanAddAsymmetric (Differential (CN MPBall)) MPBall
    where
    type AddType (Differential (CN MPBall)) MPBall = Differential (CN MPBall)
    add b a = add b (differential (order b) (cn a))

instance 
    CanAddAsymmetric Integer (Differential (CN MPBall))
    where
    type AddType Integer (Differential (CN MPBall)) = Differential (CN MPBall)
    add a b = add (differential (order b) (cn $ mpBall a)) b

instance 
    CanAddAsymmetric (Differential (CN MPBall)) Integer
    where
    type AddType (Differential (CN MPBall)) Integer = Differential (CN MPBall)
    add b a = add b (differential (order b) (cn $ mpBall a))

instance
    (CanBeMPBall a) =>
    CanAddAsymmetric (CN a) (Differential (CN MPBall))
    where
    type AddType (CN a) (Differential (CN MPBall)) = Differential (CN MPBall)
    add a b = add (differential (order b) (fmap mpBall a)) b

instance 
    (CanBeMPBall a) =>
    CanAddAsymmetric (Differential (CN MPBall)) (CN a)
    where
    type AddType (Differential (CN MPBall)) (CN a) = Differential (CN MPBall)
    add b a = add b (differential (order b) (fmap mpBall a))

{-- subtraction --}

instance 
    CanSub Integer (Differential (CN MPBall))
    where
    type SubType Integer (Differential (CN MPBall)) = Differential (CN MPBall)
    sub a b = sub (differential (order b) (cn $ mpBall a)) b

instance 
    CanSub (Differential (CN MPBall)) Integer
    where
    type SubType (Differential (CN MPBall)) Integer = Differential (CN MPBall)
    sub b a = sub b (differential (order b) (cn $ mpBall a))

instance 
    CanSub MPBall (Differential (CN MPBall))
    where
    type SubType MPBall (Differential (CN MPBall)) = Differential (CN MPBall)
    sub a b = sub (differential (order b) (cn a)) b

instance 
    CanSub (Differential (CN MPBall)) MPBall
    where
    type SubType (Differential (CN MPBall)) MPBall = Differential (CN MPBall)
    sub b a = sub b (differential (order b) (cn a))

instance 
    (CanBeMPBall a) =>
    CanSub (CN a) (Differential (CN MPBall))
    where
    type SubType (CN a) (Differential (CN MPBall)) = Differential (CN MPBall)
    sub a b = sub (differential (order b) (fmap mpBall a)) b

instance 
    (CanBeMPBall a) =>
    CanSub (Differential (CN MPBall)) (CN a) 
    where
    type SubType (Differential (CN MPBall)) (CN a) = Differential (CN MPBall)
    sub b a = sub b (differential (order b) (fmap mpBall a))

{-- multiplication --}

instance 
    CanMulAsymmetric Integer (Differential (CN MPBall))
    where
    type MulType Integer (Differential (CN MPBall)) = Differential (CN MPBall)
    mul a b = mul (differential (order b) (cn $ mpBall a)) b

instance 
    CanMulAsymmetric (Differential (CN MPBall)) Integer
        where
        type MulType (Differential (CN MPBall)) Integer = Differential (CN MPBall)
        mul b a = mul b (differential (order b) (cn $ mpBall a))

instance 
    CanMulAsymmetric MPBall (Differential (CN MPBall))
    where
    type MulType MPBall (Differential (CN MPBall)) = Differential (CN MPBall)
    mul a b = mul (differential (order b) (cn a)) b

instance 
    CanMulAsymmetric (Differential (CN MPBall)) MPBall
        where
        type MulType (Differential (CN MPBall)) MPBall = Differential (CN MPBall)
        mul b a = mul b (differential (order b) (cn a))

{-- division --}

instance 
    CanDiv Integer (Differential (CN MPBall))
    where
    type DivTypeNoCN Integer (Differential (CN MPBall)) = Differential (CN MPBall)
    type DivType     Integer (Differential (CN MPBall)) = Differential (CN MPBall)
    divideNoCN a b = divideNoCN (differential (order b) (cn $ mpBall a)) b
    divide     a b = divide     (differential (order b) (cn $ mpBall a)) b
    
instance 
    CanDiv (Differential (CN MPBall)) Integer
    where
    type DivTypeNoCN (Differential (CN MPBall)) Integer = Differential (CN MPBall)
    type DivType     (Differential (CN MPBall)) Integer = Differential (CN MPBall)
    divideNoCN b a = divideNoCN b (differential (order b) (cn $ mpBall a))
    divide     b a = divide     b (differential (order b) (cn $ mpBall a))        

instance 
    CanDiv MPBall (Differential (CN MPBall))
    where
    type DivTypeNoCN MPBall (Differential (CN MPBall)) = Differential (CN MPBall)
    type DivType     MPBall (Differential (CN MPBall)) = Differential (CN MPBall)
    divideNoCN a b = divideNoCN (differential (order b) (cn a)) b
    divide     a b = divide     (differential (order b) (cn a)) b
    
instance 
    CanDiv (Differential (CN MPBall)) MPBall
    where
    type DivTypeNoCN (Differential (CN MPBall)) MPBall = Differential (CN MPBall)
    type DivType     (Differential (CN MPBall)) MPBall = Differential (CN MPBall)
    divideNoCN b a = divideNoCN b (differential (order b) (cn a))
    divide     b a = divide     b (differential (order b) (cn a))        

instance 
    (CanBeMPBall a) =>
    CanMulAsymmetric (CN a) (Differential (CN MPBall))
    where
    type MulType (CN a) (Differential (CN MPBall)) = Differential (CN MPBall)
    mul a b = mul (differential (order b) (fmap mpBall a)) b

instance 
    (CanBeMPBall a) =>
    CanMulAsymmetric (Differential (CN MPBall)) (CN a)
        where
        type MulType (Differential (CN MPBall)) (CN a) = Differential (CN MPBall)
        mul b a = mul b (differential (order b) (fmap mpBall a))

instance 
    (CanBeMPBall a) =>
    CanDiv (CN a) (Differential (CN MPBall))
    where
    type DivTypeNoCN (CN a) (Differential (CN MPBall)) = Differential (CN MPBall)
    type DivType     (CN a) (Differential (CN MPBall)) = Differential (CN MPBall)
    divideNoCN a b = divideNoCN (differential (order b) (fmap mpBall a)) b
    divide     a b = divide     (differential (order b) (fmap mpBall a)) b

instance 
    (CanBeMPBall a) =>
    CanDiv (Differential (CN MPBall)) (CN a) 
    where
    type DivTypeNoCN (Differential (CN MPBall)) (CN a) = Differential (CN MPBall)
    type DivType     (Differential (CN MPBall)) (CN a) = Differential (CN MPBall)
    divideNoCN b a = divideNoCN b (differential (order b) (fmap mpBall a))
    divide     b a = divide     b (differential (order b) (fmap mpBall a))


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

            
            