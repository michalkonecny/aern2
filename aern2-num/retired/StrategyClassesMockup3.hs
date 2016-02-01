{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, EmptyDataDecls, UndecidableInstances, ExistentialQuantification #-}

module  where

import AERN2.Num

import Control.Arrow

import qualified Data.Map as Map

{---- examples of use ----}


{----  ----}


data GR s = GR

data GNil s = GNIL
data GPair (t1 :: * -> *) (t2 :: * -> *) s = GPair (t1 s) (t2 s)
data GTriple (t1 :: * -> *) (t2 :: * -> *) (t3 :: * -> *) s = GTriple (t1 s) (t2 s) (t3 s)
data GList (t :: * -> *) s = GList (t s)
data GVarMap (t :: * -> *) s = GVarMap (t s)

type family GType2ESType s

type instance GType2ESType (GR s) = ES_r s
type instance GType2ESType (GNil s) = ()
type instance GType2ESType (GPair t1 t2 s) = (GType2ESType (t1 s), GType2ESType (t2 s))
type instance GType2ESType (GTriple t1 t2 t3 s) = (GType2ESType (t1 s), GType2ESType (t2 s), GType2ESType (t3 s))
type instance GType2ESType (GList t s) = [GType2ESType (t s)]
type instance GType2ESType (GVarMap t s) = VarMap (GType2ESType (t s))


{-| strategy for evaluating arrow-generic real expressions  -}
class
    (ArrowReal (ES_to s) (ES_r s))
    => 
    EvalStrategyReal s i o
    where
    type ES_to s :: * -> * -> *
    type ES_r s -- ^ Real number type
    sample_i :: i s
    sample_i = error "sample_i"
    sample_o :: o s
    sample_o = error "sample_o"
    withEvalStrategyReal ::
        s ->
        (i s) ->
        (o s) ->
        ((ES_to s) (GType2ESType (i s)) (GType2ESType (o s))) ->
        ((ES_to s) (GType2ESType (i s)) (GType2ESType (o s)))
    withEvalStrategyReal _ _ _ a = a



ex1_arrowGeneric ::
    (ArrowReal to r) =>
    (r,r) `to` r
ex1_arrowGeneric = addA

ex1_withStrategy ::
    (EvalStrategyReal s (GPair GR GR) GR,
     EqCompareTypeA (ES_to s) (ES_r s) (ES_r s)
     ~ OrderCompareTypeA (ES_to s) (ES_r s) (ES_r s)) =>
    s -> ES_to s (ES_r s, ES_r s) (ES_r s)
    -- the above type is auto-derived!
ex1_withStrategy s =
    withEvalStrategyReal s (GPair GR GR) GR ex1_arrowGeneric

ex1_CR :: (CauchyReal, CauchyReal) -> CauchyReal
ex1_CR = ex1_withStrategy EvalReal_CauchyReal

ex1_MB :: (MPBall, MPBall) -> MPBall
ex1_MB = runWithPrecisionPolicy (ex1_withStrategy EvalReal_BallFixedPrec) (ppUseCurr (prec 100))

ex1_iMB :: (CauchyReal, CauchyReal) -> CauchyReal
ex1_iMB = ex1_withStrategy EvalReal_BallIncreasePrec

--data SomeEvalStrategyReal i o =
--    forall s. (EvalStrategyReal s i o) => SomeEvalStrategyReal s

{----- specific strategies -----}

data EvalReal_CauchyReal = EvalReal_CauchyReal
instance EvalStrategyReal EvalReal_CauchyReal i o where
    type ES_to EvalReal_CauchyReal = (->)
    type ES_r EvalReal_CauchyReal = CauchyReal

data EvalReal_BallFixedPrec = EvalReal_BallFixedPrec
instance EvalStrategyReal EvalReal_BallFixedPrec i o where
    type ES_to EvalReal_BallFixedPrec = WithPrecisionPolicy (->)
    type ES_r EvalReal_BallFixedPrec = MPBall

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
    withEvalStrategyReal _ is os fnA input = 
        fromPrecisionSequence (\p -> runWithPrecisionPolicy fnMB (ppUseCurr p) (encloseWithPrecision p input)) 
        where
        fnMB = withEvalStrategyReal EvalReal_BallFixedPrec is os fnA 

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

