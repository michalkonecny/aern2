{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, RankNTypes #-}
--{-# LANGUAGE PolyKinds #-}

module StrategyClassesMockup2 where

import AERN2.Num

import Control.Arrow


exampleRealGeneric1 :: RealGeneric (Pair R R) R
exampleRealGeneric1 = 
    RealGeneric (anyStrategy addA)

exampleRealGeneric1EvalWithOrdFuncReal :: (CauchyReal, CauchyReal) -> CauchyReal
exampleRealGeneric1EvalWithOrdFuncReal =
    withEvalStrategyReal exampleRealGeneric1 EvalWithOrdFuncReal

exampleRealGeneric2 :: RealGeneric R (Pair R R)
exampleRealGeneric2 = 
    RealGeneric (anyStrategy $ proc x -> do nx <- negA -< x; returnA -< (x,nx))

exampleRealGeneric2EvalWithOrdFuncReal :: CauchyReal -> (CauchyReal, CauchyReal)
exampleRealGeneric2EvalWithOrdFuncReal =
    withEvalStrategyReal exampleRealGeneric2 EvalWithOrdFuncReal

exampleRealGeneric3 :: RealGeneric R (List R)
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
    type ES_b s -- ^ Kleenean type (eg Maybe Boolean) 

-- example instance
data EvalWithOrdFuncReal = EvalWithOrdFuncReal

instance EvalStrategyReal EvalWithOrdFuncReal where
    type ES_to EvalWithOrdFuncReal = (->)
    type ES_r EvalWithOrdFuncReal = CauchyReal
    type ES_b EvalWithOrdFuncReal = Accuracy -> Maybe Bool

{-| Strategy-generic expression -}
data RealGeneric i o = RealGeneric
    { 
        withEvalStrategyReal :: 
            (forall s. EvalStrategyReal s => s -> (ES_to s) (Unwrapped (i s)) (Unwrapped (o s))) 
    }

class Wrapper w where
    type Unwrapped w
    wrap :: w {-^ dummy parameter, only for type inference -} -> (Unwrapped w) -> w
    unwrap :: w -> (Unwrapped w)
    

{-| This trivial synonym helps to make expressions that return one real number more readable, eg: 
    @RealGeneric (anyStrategy addA)@
-}
anyStrategy :: a -> s -> a
anyStrategy = const

data R s = R { unR :: ES_r s }

data Pair t1 t2 s = Pair { unPair :: (t1 s, t2 s) }
data List t s = List { unList :: [t s] }

instance Wrapper (R s) where
    type Unwrapped (R s) = ES_r s
    wrap _ = R
    unwrap = unR

instance (Wrapper (t1 s), Wrapper (t2 s)) => Wrapper (Pair t1 t2 s) where
    type Unwrapped (Pair t1 t2 s) = (Unwrapped (t1 s), Unwrapped (t2 s))
    wrap (Pair (sample1, sample2)) (a,b) = Pair (wrap sample1 a, wrap sample2 b)
    unwrap (Pair (a,b)) = (unwrap a, unwrap b)

instance (Wrapper (t s)) => Wrapper (List t s) where
    type Unwrapped (List t s) = [Unwrapped (t s)]
    wrap (List (sample:_)) items = List $ map (wrap sample) items
    wrap (List []) _ = error "instance Wrapper (List t s): wrap called with an empty sample list" 
    unwrap (List items) = map unwrap items

