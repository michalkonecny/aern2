{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, RankNTypes, EmptyDataDecls #-}
--{-# LANGUAGE PolyKinds #-}

module StrategyClassesMockup2 where

import AERN2.Num

import Control.Arrow


exampleRealGeneric1 :: RealGeneric (GPair GR GR) GR
exampleRealGeneric1 = 
    RealGeneric (anyStrategy addA)

exampleRealGeneric1EvalReal_OrdFun :: (CauchyReal, CauchyReal) -> CauchyReal
exampleRealGeneric1EvalReal_OrdFun =
    exampleRealGeneric1 `withEvalStrategyReal` EvalReal_OrdFun

exampleRealGeneric1EvalReal_FixedPrec :: Precision -> (MPBall, MPBall) -> MPBall
exampleRealGeneric1EvalReal_FixedPrec p =
    runWithPrecisionPolicy (exampleRealGeneric1 `withEvalStrategyReal` EvalReal_FixedPrecision) pp
    where
    pp = defaultPrecisionPolicy { precPolicy_precision = p }

exampleRealGeneric2 :: RealGeneric GR (GPair GR GR)
exampleRealGeneric2 = 
    RealGeneric (anyStrategy $ proc x -> do nx <- negA -< x; returnA -< (x,nx))

exampleRealGeneric2EvalReal_OrdFun :: CauchyReal -> (CauchyReal, CauchyReal)
exampleRealGeneric2EvalReal_OrdFun =
    exampleRealGeneric2 `withEvalStrategyReal` EvalReal_OrdFun

exampleRealGeneric2EvalReal_FixedPrec :: Precision -> MPBall -> (MPBall, MPBall)
exampleRealGeneric2EvalReal_FixedPrec p =
    runWithPrecisionPolicy (exampleRealGeneric2 `withEvalStrategyReal` EvalReal_FixedPrecision) pp
    where
    pp = defaultPrecisionPolicy { precPolicy_precision = p }

exampleRealGeneric3 :: RealGeneric GR (GList GR)
exampleRealGeneric3 = 
    RealGeneric (anyStrategy $ proc x -> do nx <- negA -< x; returnA -< [x,nx])

exampleRealGeneric3EvalReal_OrdFun :: CauchyReal -> [CauchyReal]
exampleRealGeneric3EvalReal_OrdFun =
    exampleRealGeneric3 `withEvalStrategyReal` EvalReal_OrdFun


{-| strategy for evaluating arrow-generic real expressions  -}
class
    (ArrowReal (ES_to s) (ES_r s))
    => 
    EvalStrategyReal s 
    where
    type ES_to s :: * -> * -> *
    type ES_r s -- ^ Real number type
    type ES_kl s -- ^ Kleenean type (eg Maybe Boolean)
    withEvalStrategyReal :: RealGeneric i o -> s -> (ES_to s) (GType2ESType (i s)) (GType2ESType (o s))
    withEvalStrategyReal = withEvalStrategyReal_  

-- example instances
data EvalReal_OrdFun = EvalReal_OrdFun
data EvalReal_FixedPrecision = EvalReal_FixedPrecision
data EvalReal_IncreasePrecision = EvalReal_IncreasePrecision

instance EvalStrategyReal EvalReal_OrdFun where
    type ES_to EvalReal_OrdFun = (->)
    type ES_r EvalReal_OrdFun = CauchyReal
    type ES_kl EvalReal_OrdFun = Accuracy -> Maybe Bool

instance EvalStrategyReal EvalReal_FixedPrecision where
    type ES_to EvalReal_FixedPrecision = WithPrecisionPolicy (->)
    type ES_r EvalReal_FixedPrecision = MPBall
    type ES_kl EvalReal_FixedPrecision = Maybe Bool

instance EvalStrategyReal EvalReal_IncreasePrecision where
    type ES_to EvalReal_IncreasePrecision = (->)
    type ES_r EvalReal_IncreasePrecision = CauchyReal
    type ES_kl EvalReal_IncreasePrecision = Accuracy -> Maybe Bool
    withEvalStrategyReal fnG _ input =
        undefined -- TODO
--        fromPrecisionSequence (\p -> runWithPrecisionPolicy fnMB (ppUseCurr p) (encloseWithPrecision p input)) 
--        where
--        fnMB = fnG `withEvalStrategyReal` EvalReal_FixedPrecision 

class CanEncloseWithPrecision r b where
    encloseWithPrecision :: Precision -> r -> b

class CanMakeFromPrecisionSequence r b where
    fromPrecisionSequence :: (Precision -> b) -> r

{-| Strategy-generic expression -}
data RealGeneric i o = RealGeneric
    { 
        withEvalStrategyReal_ :: 
            (forall s. EvalStrategyReal s => s -> (ES_to s) (GType2ESType (i s)) (GType2ESType (o s))) 
    }

{-| This trivial synonym helps to make expressions that return one real number more readable, eg: 
    @RealGeneric (anyStrategy addA)@
-}
anyStrategy :: a -> b -> a
anyStrategy = const

data GR s
data GKL s

data GNil s
data GPair (t1 :: * -> *) (t2 :: * -> *) s
data GTriple (t1 :: * -> *) (t2 :: * -> *) (t3 :: * -> *) s
data GList (t :: * -> *) s
data GVarMap (t :: * -> *) s

type family GType2ESType s

type instance GType2ESType (GR s) = ES_r s
type instance GType2ESType (GKL s) = ES_kl s
type instance GType2ESType (GNil s) = ()
type instance GType2ESType (GPair t1 t2 s) = (GType2ESType (t1 s), GType2ESType (t2 s))
type instance GType2ESType (GTriple t1 t2 t3 s) = (GType2ESType (t1 s), GType2ESType (t2 s), GType2ESType (t3 s))
type instance GType2ESType (GList t s) = [GType2ESType (t s)]
type instance GType2ESType (GVarMap t s) = VarMap (GType2ESType (t s))
