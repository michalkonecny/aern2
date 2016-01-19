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

exampleRealGeneric1EvalWithOrdFuncReal :: (CauchyReal, CauchyReal) -> CauchyReal
exampleRealGeneric1EvalWithOrdFuncReal =
    withEvalStrategyReal exampleRealGeneric1 EvalWithOrdFuncReal

exampleRealGeneric2 :: RealGeneric GR (GPair GR GR)
exampleRealGeneric2 = 
    RealGeneric (anyStrategy $ proc x -> do nx <- negA -< x; returnA -< (x,nx))

exampleRealGeneric2EvalWithOrdFuncReal :: CauchyReal -> (CauchyReal, CauchyReal)
exampleRealGeneric2EvalWithOrdFuncReal =
    withEvalStrategyReal exampleRealGeneric2 EvalWithOrdFuncReal

exampleRealGeneric3 :: RealGeneric GR (GList GR)
exampleRealGeneric3 = 
    RealGeneric (anyStrategy $ proc x -> do nx <- negA -< x; returnA -< [x,nx])

exampleRealGeneric3EvalWithOrdFuncReal :: CauchyReal -> [CauchyReal]
exampleRealGeneric3EvalWithOrdFuncReal =
    withEvalStrategyReal exampleRealGeneric3 EvalWithOrdFuncReal


{-| strategy for evaluating arrow-generic real expressions  -}
class
    (ArrowReal (ES_to s) (ES_r s))
    => 
    EvalStrategyReal s 
    where
    type ES_to s :: * -> * -> *
    type ES_r s -- ^ Real number type
    type ES_kl s -- ^ Kleenean type (eg Maybe Boolean) 

-- example instance
data EvalWithOrdFuncReal = EvalWithOrdFuncReal

instance EvalStrategyReal EvalWithOrdFuncReal where
    type ES_to EvalWithOrdFuncReal = (->)
    type ES_r EvalWithOrdFuncReal = CauchyReal
    type ES_kl EvalWithOrdFuncReal = Accuracy -> Maybe Bool

{-| Strategy-generic expression -}
data RealGeneric i o = RealGeneric
    { 
        withEvalStrategyReal :: 
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
