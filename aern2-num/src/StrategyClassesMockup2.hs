{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, EmptyDataDecls, UndecidableInstances, ExistentialQuantification, DefaultSignatures #-}

module StrategyClassesMockup2 where

import AERN2.Num

import Control.Arrow

import qualified Data.Map as Map

{---- examples of use ----}

exampleRealGeneric1 :: RealGeneric (GPair GR GR) GR
exampleRealGeneric1 = 
    RealGeneric (anyStrategy addA)

exampleRealGeneric1EvalReal_CauchyReal :: (CauchyReal, CauchyReal) -> CauchyReal
exampleRealGeneric1EvalReal_CauchyReal =
    exampleRealGeneric1 `withEvalStrategyReal` EvalReal_CauchyReal

exampleRealGeneric1EvalReal_FixedPrec :: Precision -> (MPBall, MPBall) -> MPBall
exampleRealGeneric1EvalReal_FixedPrec p =
    runWithPrecisionPolicy (exampleRealGeneric1 `withEvalStrategyReal` EvalReal_BallFixedPrec) (ppUseCurr p)

exampleRealGeneric1EvalReal_BallIncreasePrec :: (CauchyReal, CauchyReal) -> CauchyReal
exampleRealGeneric1EvalReal_BallIncreasePrec =
    exampleRealGeneric1 `withEvalStrategyReal` EvalReal_BallIncreasePrec

exampleRealGeneric2 :: RealGeneric GR (GPair GR GR)
exampleRealGeneric2 = 
    RealGeneric (anyStrategy $ proc x -> do nx <- negA -< x; returnA -< (x,nx))

exampleRealGeneric2EvalReal_CauchyReal :: CauchyReal -> (CauchyReal, CauchyReal)
exampleRealGeneric2EvalReal_CauchyReal =
    exampleRealGeneric2 `withEvalStrategyReal` EvalReal_CauchyReal

exampleRealGeneric2EvalReal_FixedPrec :: Precision -> MPBall -> (MPBall, MPBall)
exampleRealGeneric2EvalReal_FixedPrec p =
    runWithPrecisionPolicy (exampleRealGeneric2 `withEvalStrategyReal` EvalReal_BallFixedPrec) (ppUseCurr p)

exampleRealGeneric2EvalReal_BallIncreasePrec :: CauchyReal -> (CauchyReal, CauchyReal)
exampleRealGeneric2EvalReal_BallIncreasePrec =
    exampleRealGeneric2 `withEvalStrategyReal` EvalReal_BallIncreasePrec

exampleRealGenericNested :: RealGeneric GR GR
exampleRealGenericNested = 
    RealGeneric $ \ s ->
        case getSubStrategy s GR GR "neg" of
            SubEvalStrategyReal negStrategy ->
                proc x -> 
                    do
--                    nx <- realGenericA "neg" myNeg -< x
                    nx <- arrow2arrow (withEvalStrategyReal myNeg negStrategy) -< x
                    let _ = [x,nx]
                    addA -< (1,nx)
    where        
    myNeg :: RealGeneric GR GR
    myNeg = RealGeneric (anyStrategy negA)

exampleRealGenericNested_CauchyReal :: CauchyReal -> CauchyReal
exampleRealGenericNested_CauchyReal =
    exampleRealGenericNested `withEvalStrategyReal` EvalReal_CauchyReal

exampleRealGenericNested_BallIncreasePrec :: CauchyReal -> CauchyReal
exampleRealGenericNested_BallIncreasePrec =
    exampleRealGenericNested `withEvalStrategyReal` EvalReal_BallIncreasePrec

{- TODO:

exampleRealGenericNested_CauchyReal_BallIncreasePrec  :: CauchyReal -> CauchyReal
exampleRealGenericNested_CauchyReal_BallIncreasePrec =
    exampleRealGenericNested `withEvalStrategyReal` 
        (EvalReal_Nested EvalReal_CauchyReal ["neg", EvalReal_BallIncreasePrec])
-}

{-| Strategy-generic expression -}
data RealGeneric i o = RealGeneric
    { 
        withEvalStrategyReal_ :: 
            (forall s. (EvalStrategyReal s i o) => s -> (ES_to s) (GType2ESType (i s)) (GType2ESType (o s))) 
    }

{-| This trivial synonym helps to make expressions that return one real number more readable, eg: 
    @RealGeneric (anyStrategy addA) :: RealGeneric (GPair GR GR) GR@
-}
anyStrategy :: a -> b -> a
anyStrategy = const

data GR s = GR
data GKL s = GKL

data GNil s = GNIL
data GPair (t1 :: * -> *) (t2 :: * -> *) s = GPair (t1 s) (t2 s)
data GTriple (t1 :: * -> *) (t2 :: * -> *) (t3 :: * -> *) s = GTriple (t1 s) (t2 s) (t3 s)
data GList (t :: * -> *) s = GList (t s)
data GVarMap (t :: * -> *) s = GVarMap (t s)


type family GType2ESType s

type instance GType2ESType (GR s) = ES_r s
type instance GType2ESType (GKL s) = ES_kl s
type instance GType2ESType (GNil s) = ()
type instance GType2ESType (GPair t1 t2 s) = (GType2ESType (t1 s), GType2ESType (t2 s))
type instance GType2ESType (GTriple t1 t2 t3 s) = (GType2ESType (t1 s), GType2ESType (t2 s), GType2ESType (t3 s))
type instance GType2ESType (GList t s) = [GType2ESType (t s)]
type instance GType2ESType (GVarMap t s) = VarMap (GType2ESType (t s))


{-| strategy for evaluating arrow-generic real expressions  -}
class
    (ArrowReal (ES_to s) (ES_r s),
     HaveASubStrategy s)
    => 
    EvalStrategyReal s i o
    where
    type ES_to s :: * -> * -> *
    type ES_r s -- ^ Real number type
    type ES_kl s -- ^ Kleenean type (eg Maybe Boolean)
    withEvalStrategyReal :: RealGeneric i o -> s -> (ES_to s) (GType2ESType (i s)) (GType2ESType (o s))
    withEvalStrategyReal = withEvalStrategyReal_
    
class
    HaveASubStrategy s
    where
    getSubStrategy ::
        (EvalStrategyReal s i o)
        => 
        s -> (i s) -> (o s) -> 
        String -> 
        SubEvalStrategyReal s i o
    default getSubStrategy ::
        (EvalStrategyReal s i o, 
         ArrowConvert
            (GType2ESType (i s)) (ES_to s) (GType2ESType (o s)) 
            (GType2ESType (i s)) (ES_to s) (GType2ESType (o s)))
        => 
        s -> (i s) -> (o s) -> 
        String -> 
        SubEvalStrategyReal s i o
    getSubStrategy s _ _ _ = SubEvalStrategyReal s

data SubEvalStrategyReal s i o =
    forall sN. 
        (EvalStrategyReal sN i o, 
         ArrowConvert
            (GType2ESType (i sN)) (ES_to sN) (GType2ESType (o sN)) 
            (GType2ESType (i s )) (ES_to s ) (GType2ESType (o s )) 
        ) 
        => 
        SubEvalStrategyReal sN

{----- specific strategies -----}

data EvalReal_CauchyReal = EvalReal_CauchyReal
instance HaveASubStrategy EvalReal_CauchyReal
instance EvalStrategyReal EvalReal_CauchyReal i o where
    type ES_to EvalReal_CauchyReal = (->)
    type ES_r EvalReal_CauchyReal = CauchyReal
    type ES_kl EvalReal_CauchyReal = Accuracy -> Maybe Bool

data EvalReal_BallFixedPrec = EvalReal_BallFixedPrec
instance HaveASubStrategy EvalReal_BallFixedPrec
instance EvalStrategyReal EvalReal_BallFixedPrec i o where
    type ES_to EvalReal_BallFixedPrec = WithPrecisionPolicy (->)
    type ES_r EvalReal_BallFixedPrec = MPBall
    type ES_kl EvalReal_BallFixedPrec = Maybe Bool

--data EvalReal_Nested s1 i2 o2 =
--    EvalReal_Nested s1 String (SomeEvalStrategyReal i2 o2)
--instance 
--    (EvalStrategyReal s1 i1 o1) 
--    =>
--    EvalStrategyReal (EvalReal_Nested s1 i2 o2) i1 o1
--    where
--    type ES_to (EvalReal_Nested s1 i2 o2) = WithNestedArrow (ES_to s1)
--    type ES_r (EvalReal_Nested s1 i2 o2) = ES_r s1  
--    type ES_kl (EvalReal_Nested s1 i2 o2) = ES_kl s1  
--
--data WithNestedArrow to a b




data EvalReal_BallIncreasePrec = EvalReal_BallIncreasePrec
instance
    HaveASubStrategy EvalReal_BallIncreasePrec
instance
    (CanEncloseWithPrecision
        (GType2ESType (i EvalReal_BallIncreasePrec))
        (GType2ESType (i EvalReal_BallFixedPrec))
    ,
     CanMakeFromPrecisionSequence
        (GType2ESType (o EvalReal_BallIncreasePrec))
        (GType2ESType (o EvalReal_BallFixedPrec))
    )
    => 
    EvalStrategyReal EvalReal_BallIncreasePrec i o 
    where
    type ES_to EvalReal_BallIncreasePrec = (->)
    type ES_r EvalReal_BallIncreasePrec = CauchyReal
    type ES_kl EvalReal_BallIncreasePrec = Accuracy -> Maybe Bool
    withEvalStrategyReal fnG _ input =
        fromPrecisionSequence (\p -> runWithPrecisionPolicy fnMB (ppUseCurr p) (encloseWithPrecision p input)) 
        where
        fnMB = fnG `withEvalStrategyReal` EvalReal_BallFixedPrec

class CanEncloseWithPrecision r b where
    encloseWithPrecision :: Precision -> r -> b

instance CanEncloseWithPrecision CauchyReal MPBall where
    encloseWithPrecision p r = cauchyReal2ball r (bits $ prec2integer p) 

instance CanEncloseWithPrecision () () where
    encloseWithPrecision _ _ = () 

instance 
    (CanEncloseWithPrecision r b)
    =>
    CanEncloseWithPrecision [r] [b]
    where
    encloseWithPrecision p = map $ encloseWithPrecision p

instance 
    (CanEncloseWithPrecision r b)
    =>
    CanEncloseWithPrecision (VarMap r) (VarMap b)
    where
    encloseWithPrecision p = Map.map $ encloseWithPrecision p

instance 
    (CanEncloseWithPrecision r1 b1, CanEncloseWithPrecision r2 b2)
    =>
    CanEncloseWithPrecision (r1,r2) (b1,b2)
    where
    encloseWithPrecision p (r1,r2) =
        (encloseWithPrecision p r1, encloseWithPrecision p r2)

instance 
    (CanEncloseWithPrecision r1 b1, CanEncloseWithPrecision r2 b2, CanEncloseWithPrecision r3 b3)
    =>
    CanEncloseWithPrecision (r1,r2,r3) (b1,b2,b3)
    where
    encloseWithPrecision p (r1,r2,r3) =
        (encloseWithPrecision p r1, encloseWithPrecision p r2, encloseWithPrecision p r3)

class CanMakeFromPrecisionSequence r b where
    fromPrecisionSequence :: (Precision -> b) -> r

instance CanMakeFromPrecisionSequence CauchyReal MPBall where
    fromPrecisionSequence = seqByPrecision2Cauchy Nothing

instance CanMakeFromPrecisionSequence () () where
    fromPrecisionSequence _ = () 

instance 
    (CanMakeFromPrecisionSequence r1 b1, CanMakeFromPrecisionSequence r2 b2)
    =>
    CanMakeFromPrecisionSequence (r1,r2) (b1,b2)
    where
    fromPrecisionSequence sq =
        (fromPrecisionSequence (fst . sq), 
         fromPrecisionSequence (snd . sq))

instance 
    (CanMakeFromPrecisionSequence r1 b1, CanMakeFromPrecisionSequence r2 b2, CanMakeFromPrecisionSequence r3 b3)
    =>
    CanMakeFromPrecisionSequence (r1,r2,r3) (b1,b2,b3)
    where
    fromPrecisionSequence sq =
        (fromPrecisionSequence ((\(a,_,_) -> a) . sq), 
         fromPrecisionSequence ((\(_,b,_) -> b) . sq),
         fromPrecisionSequence ((\(_,_,c) -> c) . sq))


{- 
instance 
    (CanMakeFromPrecisionSequence r b)
    =>
    CanMakeFromPrecisionSequence [r] [b]
    where
    fromPrecisionSequence seq = 
        undefined

instance 
    (CanMakeFromPrecisionSequence r b)
    =>
    CanMakeFromPrecisionSequence (VarMap r) (VarMap b)
    where
    fromPrecisionSequence seq = 
        undefined
-}

