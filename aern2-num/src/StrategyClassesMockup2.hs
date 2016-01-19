{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances, FunctionalDependencies, RankNTypes #-}
--{-# LANGUAGE PolyKinds #-}

module StrategyClassesMockup2 where

import AERN2.Num

import Control.Arrow

{-| strategy for evaluating arrow-generic real expressions  -}
class
    (ArrowReal (ES_to s) (ES_r s))
    => 
    EvalStrategyReal s 
    where
    type ES_to s :: * -> * -> *
    type ES_r s -- ^ Real number type
    type ES_b s -- ^ Kleenean type (eg Maybe Boolean) 

-- example instance
data EvalWithOrdFuncReal = EvalWithOrdFuncReal

instance EvalStrategyReal EvalWithOrdFuncReal where
    type ES_to EvalWithOrdFuncReal = (->)
    type ES_r EvalWithOrdFuncReal = CauchyReal
    type ES_b EvalWithOrdFuncReal = Accuracy -> Maybe Bool

{-| Strategy-generic expression -}
data RealGenericFixed = RealGenericFixed
    { 
        withEvalStrategyRealFixed :: 
            (forall s. EvalStrategyReal s => s -> (ES_to s) (ES_r s) (ES_r s)) 
    }

exampleRealGenericFixed1 :: RealGenericFixed
exampleRealGenericFixed1 =
    RealGenericFixed (const negA)

data RealGeneric i o = RealGeneric
    { 
        withEvalStrategyReal :: 
            (forall s. EvalStrategyReal s => s -> (ES_to s) (Unwrapped (i s)) (o s)) 
    }

class Wrapper w where
    type Unwrapped w
    wrap :: w -> (Unwrapped w) -> w
    unwrap :: w -> (Unwrapped w)
    

{-| This trivial synonym helps to make expressions more readable, eg: 
    @RealGeneric (anyStrategy $ arr R . addA)@
-}
getR ::
          Arrow to =>
          a `to` (ES_r s) -> b -> a `to` (R s)
getR fnA = const $ arr R . fnA

data R s = R { unR :: ES_r s }
data RR s = RR { unRR :: (ES_r s, ES_r s) }
data RL s = RL { unRL :: [ES_r s] }

instance Wrapper (R s) where
    type Unwrapped (R s) = ES_r s
    wrap _ = R
    unwrap = unR

instance Wrapper (RR s) where
    type Unwrapped (RR s) = (ES_r s, ES_r s)
    wrap _ = RR
    unwrap = unRR

exampleRealGeneric1 :: RealGeneric RR R
exampleRealGeneric1 = RealGeneric (getR addA)


exampleRealGeneric1EvalWithOrdFuncReal :: (CauchyReal, CauchyReal) -> CauchyReal
exampleRealGeneric1EvalWithOrdFuncReal =
    unwrap . withEvalStrategyReal exampleRealGeneric1 EvalWithOrdFuncReal

