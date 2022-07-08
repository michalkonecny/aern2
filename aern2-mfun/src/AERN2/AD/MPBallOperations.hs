{-# OPTIONS_GHC -Wno-orphans #-}
module AERN2.AD.MPBallOperations where

import MixedTypesNumPrelude

import AERN2.AD.Type
import AERN2.AD.GenericOperations ()
import AERN2.MP.Ball
import Numeric.CollectErrors.Type (noValueNumErrorCertain, NumError (NumError))

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
    type DivType     Integer (Differential (CN MPBall)) = Differential (CN MPBall)
    divide     a b = divide     (differential (order b) (cn $ mpBall a)) b
    
instance 
    CanDiv (Differential (CN MPBall)) Integer
    where
    type DivType     (Differential (CN MPBall)) Integer = Differential (CN MPBall)
    divide     b a = divide     b (differential (order b) (cn $ mpBall a))        

instance 
    CanDiv MPBall (Differential (CN MPBall))
    where
    type DivType     MPBall (Differential (CN MPBall)) = Differential (CN MPBall)
    divide     a b = divide     (differential (order b) (cn a)) b
    
instance 
    CanDiv (Differential (CN MPBall)) MPBall
    where
    type DivType     (Differential (CN MPBall)) MPBall = Differential (CN MPBall)
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
    type DivType     (CN a) (Differential (CN MPBall)) = Differential (CN MPBall)
    divide     a b = divide     (differential (order b) (fmap mpBall a)) b

instance 
    (CanBeMPBall a) =>
    CanDiv (Differential (CN MPBall)) (CN a) 
    where
    type DivType     (Differential (CN MPBall)) (CN a) = Differential (CN MPBall)
    divide     b a = divide     b (differential (order b) (fmap mpBall a))


instance 
    CanPow (Differential (CN MPBall)) Integer
    where
        type PowType     (Differential (CN MPBall)) Integer = (Differential (CN MPBall))
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

instance
    CanAbs (Differential (CN MPBall))
    where
    type AbsType (Differential (CN MPBall)) = Differential (CN MPBall)
    abs (OrderZero x)   = OrderZero $ abs x 
    abs (OrderOne x dx) = OrderOne (abs x) newDx
        where
            newDx = do 
                dx_ <- dx
                pure $ (hullMPBall dx_ (-dx_))
    abs (OrderTwo _ _ _ _) = error "Abs for differential order two undefined"

instance 
    CanDivIMod (Differential (CN MPBall)) (Differential (CN MPBall))
    where
    type DivIType (Differential (CN MPBall)) (Differential (CN MPBall)) = (Differential (CN MPBall))
    divIMod a b = (error "Integer division for Differential (CN MPBall) undefined", OrderTwo (mod ax bx) err err err)
        where   
            ax = diff_x a
            bx = diff_x b
            err = noValueNumErrorCertain $ NumError "No derivatives after modulus"
