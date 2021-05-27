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

class HasCurrentPrecision p where
    getCurrentPrecision :: proxy p -> Precision

instance KnownNat n => HasCurrentPrecision n where
    getCurrentPrecision p = max (prec 2) . min maximumPrecision $ prec (natVal p)

{-|

An existential type wrapper for convenient conversions, eg using aern2-real:

> _x :: KnownNat p => WithCurrentPrec (CN MPBall) p
> _x = undefined
>
> _r_x :: CReal
> _r_x = creal $ WithAnyPrec _x

-}
newtype WithAnyPrec t = WithAnyPrec (forall p. (KnownNat p) => WithCurrentPrec t p)

-- data PrecAdd10 (p :: *)

-- instance (HasCurrentPrecision p) => HasCurrentPrecision (PrecAdd10 p) where
--     isPrecision (_ :: proxy _) = 10 + isPrecision (undefined :: proxy p)

newtype WithCurrentPrec t p = WithCurrentPrec { unWithCurrentPrec :: t }
    deriving (Show)

deriving instance (CanTakeErrors NumErrors t) => (CanTakeErrors NumErrors (WithCurrentPrec t p))

runWithPrec :: Precision -> (forall n. (KnownNat n) => WithCurrentPrec t n) -> t
runWithPrec p (wfp :: (forall n. (KnownNat n) => WithCurrentPrec t n)) = 
    reifyNat (integer p) withNat
    where
    withNat :: KnownNat n => Proxy n -> t
    withNat (_ :: Proxy n) = 
        unWithCurrentPrec (wfp :: WithCurrentPrec t n)

lift1 :: (t1 -> t2) -> (WithCurrentPrec t1 p) -> (WithCurrentPrec t2 p)
lift1 f (WithCurrentPrec v1) = WithCurrentPrec (f v1)

lift2 :: (p1 ~ p2) => (t1 -> t2 -> t3) -> (WithCurrentPrec t1 p1) -> (WithCurrentPrec t2 p2) -> (WithCurrentPrec t3 p1)
lift2 f (WithCurrentPrec v1) (WithCurrentPrec v2) = WithCurrentPrec (f v1 v2)

lift2P :: (p1 ~ p2) => (t1 -> t2 -> t3) -> (WithCurrentPrec t1 p1) -> (WithCurrentPrec t2 p2) -> t3
lift2P f (WithCurrentPrec v1) (WithCurrentPrec v2) = f v1 v2

lift1T :: (t1 -> t2 -> t3) -> (WithCurrentPrec t1 p1) -> t2 -> (WithCurrentPrec t3 p1)
lift1T f (WithCurrentPrec v1) v2 = WithCurrentPrec (f v1 v2)

lift1TP :: (t1 -> t2 -> t3) -> (WithCurrentPrec t1 p1) -> t2 -> t3
lift1TP f (WithCurrentPrec v1) v2 = f v1 v2

liftT1 :: (t1 -> t2 -> t3) -> t1 -> (WithCurrentPrec t2 p2) -> (WithCurrentPrec t3 p1)
liftT1 f v1 (WithCurrentPrec v2) = WithCurrentPrec (f v1 v2)

liftT1P :: (t1 -> t2 -> t3) -> t1 -> (WithCurrentPrec t2 p2) -> t3
liftT1P f v1 (WithCurrentPrec v2) = f v1 v2

mpBallCP :: (CanBeMPBallP t, KnownNat p) => t -> WithCurrentPrec (CN MPBall) p
mpBallCP v = r 
    where
    r = WithCurrentPrec $ cn $ mpBallP (getCurrentPrecision r) v
