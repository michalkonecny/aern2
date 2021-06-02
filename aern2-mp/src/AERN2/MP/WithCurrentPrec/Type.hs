{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
    Module      :  AERN2.MP.WithCurrentPrec.Type
    Description :  Type wrapper setting default precision
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Type wrapper setting default precision.

    Borrowed some tricks from https://github.com/ekmett/rounded/blob/master/src/Numeric/Rounded/Precision.hs
-}
module AERN2.MP.WithCurrentPrec.Type where

import MixedTypesNumPrelude
-- import Text.Printf

-- import Text.Printf
import Numeric.CollectErrors (NumErrors, CanTakeErrors(..))
-- import qualified Numeric.CollectErrors as CN

import Data.Proxy
import Data.Reflection
import GHC.TypeLits

import AERN2.MP

newtype WithCurrentPrec p t = WithCurrentPrec { unWithCurrentPrec :: t }
    deriving (Show)

deriving instance (CanTakeErrors NumErrors t) => (CanTakeErrors NumErrors (WithCurrentPrec p t))
deriving instance (CanTestIsIntegerType t) => (CanTestIsIntegerType (WithCurrentPrec p t))

getCurrentPrecision :: (KnownNat p) => WithCurrentPrec p t -> Precision
getCurrentPrecision (_ :: (WithCurrentPrec p t)) =
    max (prec 2) . min maximumPrecision $ prec (natVal (undefined :: Proxy p))

{-|

An existential type wrapper for convenient conversions, eg using aern2-real:

> _x :: KnownNat p => WithCurrentPrec (CN MPBall) p
> _x = undefined
>
> _r_x :: CReal
> _r_x = creal $ WithAnyPrec _x

-}
newtype WithAnyPrec t = WithAnyPrec (forall p. (KnownNat p) => WithCurrentPrec p t)

{-| 
    Run a WithCurrentPrec computation with a specific precision.
-}
runWithPrec :: Precision -> (forall p. (KnownNat p) => WithCurrentPrec p t) -> t
runWithPrec p (wfp :: (forall p. (KnownNat p) => WithCurrentPrec p t)) = 
    reifyNat (integer p) withNat
    where
    withNat :: KnownNat p => Proxy p -> t
    withNat (_ :: Proxy p) = 
        unWithCurrentPrec (wfp :: WithCurrentPrec p t)

instance (ConvertibleWithPrecision t1 t2, KnownNat p) => ConvertibleExactly t1 (WithCurrentPrec p t2) where
    safeConvertExactly v = Right r
        where
        r = WithCurrentPrec $ convertP (getCurrentPrecision r) v

-- mpBallCP :: (CanBeMPBallP t, KnownNat p) => t -> WithCurrentPrec p MPBall
-- mpBallCP = convertExactly 

cnmpBallCP :: (CanBeMPBallP t, KnownNat p) => t -> WithCurrentPrec p (CN MPBall)
cnmpBallCP = lift1 cn . convertExactly 

lift1 :: (t1 -> t2) -> (WithCurrentPrec p t1) -> (WithCurrentPrec p t2)
lift1 f (WithCurrentPrec v1) = WithCurrentPrec (f v1)

lift2 :: (p1 ~ p2) => (t1 -> t2 -> t3) -> (WithCurrentPrec p1 t1) -> (WithCurrentPrec p2 t2) -> (WithCurrentPrec p1 t3)
lift2 f (WithCurrentPrec v1) (WithCurrentPrec v2) = WithCurrentPrec (f v1 v2)

lift2P :: (p1 ~ p2) => (t1 -> t2 -> t3) -> (WithCurrentPrec p1 t1) -> (WithCurrentPrec p2 t2) -> t3
lift2P f (WithCurrentPrec v1) (WithCurrentPrec v2) = f v1 v2

lift1T :: (t1 -> t2 -> t3) -> (WithCurrentPrec p1 t1) -> t2 -> (WithCurrentPrec p1 t3)
lift1T f (WithCurrentPrec v1) v2 = WithCurrentPrec (f v1 v2)

lift1TP :: (t1 -> t2 -> t3) -> (WithCurrentPrec p1 t1) -> t2 -> t3
lift1TP f (WithCurrentPrec v1) v2 = f v1 v2

liftT1 :: (t1 -> t2 -> t3) -> t1 -> (WithCurrentPrec p2 t2) -> (WithCurrentPrec p1 t3)
liftT1 f v1 (WithCurrentPrec v2) = WithCurrentPrec (f v1 v2)

liftT1P :: (t1 -> t2 -> t3) -> t1 -> (WithCurrentPrec p2 t2) -> t3
liftT1P f v1 (WithCurrentPrec v2) = f v1 v2

