{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  AERN2.Sequence.Ring
    Description :  ring operations on sequences
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Ring operations on convergent sequences
-}
module AERN2.Sequence.Ring
(
  mulGetInitAC
)
where

import MixedTypesNumPrelude hiding (id)
-- import qualified Prelude as P

import Control.Category (id)
import Control.Arrow

import Control.CollectErrors

import AERN2.MP.Ball
import AERN2.MP.Dyadic

import AERN2.QA.Protocol
import AERN2.AccuracySG
import AERN2.Sequence.Type
import AERN2.Sequence.Helpers

{- addition -}

instance
  (QAArrow to, CanAddAsymmetric a b, SuitableForSeq a, SuitableForSeq b, SuitableForSeq (AddType a b))
  =>
  CanAddAsymmetric (SequenceA to a) (SequenceA to b)
  where
  type AddType (SequenceA to a) (SequenceA to b) = SequenceA to (AddType a b)
  add = binaryOp "+" add (getInitQ1Q2FromSimple $ proc q -> returnA -< (q,q))

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|

  instance
    (QAArrow to, CanAddAsymmetric a $t, SuitableForSeq a, SuitableForSeq (AddType a $t))
    =>
    CanAddAsymmetric (SequenceA to a) $t
    where
    type AddType (SequenceA to a) $t = SequenceA to (AddType a $t)
    add = binaryOpWithPureArg "+" add (getInitQ1TFromSimple id)

  instance
    (QAArrow to, CanAddAsymmetric $t b, SuitableForSeq b, SuitableForSeq (AddType $t b))
    =>
    CanAddAsymmetric $t (SequenceA to b)
    where
    type AddType $t (SequenceA to b) = SequenceA to (AddType $t b)
    add = flip $ binaryOpWithPureArg "+" (flip add) (getInitQ1TFromSimple id)

  |]))

instance
  (CanAddAsymmetric a MPBall, SuitableForSeq a
  , CanSetPrecision (AddType a MPBall))
  =>
  CanAddAsymmetric (Sequence a) MPBall
  where
  type AddType (Sequence a) MPBall = AddType a MPBall
  add = binaryWithEncl add

instance
  (CanAddAsymmetric MPBall b, SuitableForSeq b
  , CanSetPrecision (AddType MPBall b))
  =>
  CanAddAsymmetric MPBall (Sequence b)
  where
  type AddType MPBall (Sequence b) = AddType MPBall b
  add = flip $ binaryWithEncl (flip add)

instance
  (CanAddAsymmetric (SequenceA to a) b
  , CanEnsureCE es b
  , CanEnsureCE es (AddType (SequenceA to a) b)
  , CanBeErrors es)
  =>
  CanAddAsymmetric (SequenceA to a) (CollectErrors es  b)
  where
  type AddType (SequenceA to a) (CollectErrors es  b) =
    EnsureCE es (AddType (SequenceA to a) b)
  add = lift2TLCE add

instance
  (CanAddAsymmetric a (SequenceA to b)
  , CanEnsureCE es a
  , CanEnsureCE es (AddType a (SequenceA to b))
  , CanBeErrors es)
  =>
  CanAddAsymmetric (CollectErrors es a) (SequenceA to b)
  where
  type AddType (CollectErrors es  a) (SequenceA to b) =
    EnsureCE es (AddType a (SequenceA to b))
  add = lift2TCE add


{- subtraction -}

instance
  (QAArrow to, CanSub a b, SuitableForSeq a, SuitableForSeq b, SuitableForSeq (SubType a b))
  =>
  CanSub (SequenceA to a) (SequenceA to b)
  where
  type SubType (SequenceA to a) (SequenceA to b) = SequenceA to (SubType a b)
  sub = binaryOp "-" sub (getInitQ1Q2FromSimple $ proc q -> returnA -< (q,q))


$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|

  instance
    (QAArrow to, CanSub a $t, SuitableForSeq a, SuitableForSeq (SubType a $t))
    =>
    CanSub (SequenceA to a) $t
    where
    type SubType (SequenceA to a) $t = SequenceA to (SubType a $t)
    sub = binaryOpWithPureArg "-" sub (getInitQ1TFromSimple id)

  instance
    (QAArrow to, CanSub $t b, SuitableForSeq b, SuitableForSeq (SubType $t b))
    =>
    CanSub $t (SequenceA to b)
    where
    type SubType $t (SequenceA to b) = SequenceA to (SubType $t b)
    sub = flip $ binaryOpWithPureArg "-" (flip sub) (getInitQ1TFromSimple id)

  |]))

instance
  (CanSub a MPBall, SuitableForSeq a, CanSetPrecision (SubType a MPBall))
  =>
  CanSub (Sequence a) MPBall
  where
  type SubType (Sequence a) MPBall = SubType a MPBall
  sub = binaryWithEncl sub

instance
  (CanSub MPBall b, SuitableForSeq b, CanSetPrecision (SubType MPBall b))
  =>
  CanSub MPBall (Sequence b)
  where
  type SubType MPBall (Sequence b) = SubType MPBall b
  sub = flip $ binaryWithEncl (flip sub)

instance
  (CanSub (SequenceA to a) b
  , CanEnsureCE es b
  , CanEnsureCE es (SubType (SequenceA to a) b)
  , CanBeErrors es)
  =>
  CanSub (SequenceA to a) (CollectErrors es  b)
  where
  type SubType (SequenceA to a) (CollectErrors es  b) =
    EnsureCE es (SubType (SequenceA to a) b)
  sub = lift2TLCE sub

instance
  (CanSub a (SequenceA to b)
  , CanEnsureCE es a
  , CanEnsureCE es (SubType a (SequenceA to b))
  , CanBeErrors es)
  =>
  CanSub (CollectErrors es a) (SequenceA to b)
  where
  type SubType (CollectErrors es  a) (SequenceA to b) =
    EnsureCE es (SubType a (SequenceA to b))
  sub = lift2TCE sub


{- multiplication -}

instance
  (QAArrow to, CanMulAsymmetric a b, HasNorm (EnsureNoCN a), HasNorm (EnsureNoCN b)
  , SuitableForSeq a, SuitableForSeq b, SuitableForSeq (MulType a b))
  =>
  CanMulAsymmetric (SequenceA to a) (SequenceA to b)
  where
  type MulType (SequenceA to a) (SequenceA to b) = SequenceA to (MulType a b)
  mul =
    binaryOp "*" mul getInitQ1Q2
    where
    getInitQ1Q2 me a1 a2 =
      proc q ->
        do
        b1 <- seqWithAccuracy a1 me -< q
        let jInit2 = mulGetInitAC b1 q
        -- favouring 2*x over x*2 in a Num instance
        b2 <- seqWithAccuracy a2 me -< jInit2
        let jInit1 = mulGetInitAC b2 q
        returnA -< ((jInit1, Just b1), (jInit2, Just b2))

mulGetInitAC ::
  (HasNorm (EnsureNoCN other), CanEnsureCN other)
  =>
  other -> AccuracySG -> AccuracySG
mulGetInitAC other acSG =
  case ensureNoCN other of
    (Just otherNoCN, _) ->
      case getNormLog otherNoCN of
        NormBits otherNL -> max acSG0 (acSG + otherNL)
        NormZero -> acSG0
    _ -> acSG

instance
  (CanMulAsymmetric a MPBall, SuitableForSeq a
  , CanSetPrecision (MulType a MPBall))
  =>
  CanMulAsymmetric (Sequence a) MPBall
  where
  type MulType (Sequence a) MPBall = MulType a MPBall
  mul = binaryWithEnclTranslateAC (\_ -> mulGetInitAC) mul

instance
  (CanMulAsymmetric MPBall b, SuitableForSeq b
  , CanSetPrecision (MulType MPBall b))
  =>
  CanMulAsymmetric MPBall (Sequence b)
  where
  type MulType MPBall (Sequence b) = MulType MPBall b
  mul = flip $ binaryWithEnclTranslateAC (\ _ -> mulGetInitAC) (flip mul)

instance
  (CanMulAsymmetric (SequenceA to a) b
  , CanEnsureCE es b
  , CanEnsureCE es (MulType (SequenceA to a) b)
  , CanBeErrors es)
  =>
  CanMulAsymmetric (SequenceA to a) (CollectErrors es  b)
  where
  type MulType (SequenceA to a) (CollectErrors es  b) =
    EnsureCE es (MulType (SequenceA to a) b)
  mul = lift2TLCE mul

instance
  (CanMulAsymmetric a (SequenceA to b)
  , CanEnsureCE es a
  , CanEnsureCE es (MulType a (SequenceA to b))
  , CanBeErrors es)
  =>
  CanMulAsymmetric (CollectErrors es a) (SequenceA to b)
  where
  type MulType (CollectErrors es  a) (SequenceA to b) =
    EnsureCE es (MulType a (SequenceA to b))
  mul = lift2TCE mul


mulGetInitQ1T ::
  (Arrow to, HasNorm (EnsureNoCN other), CanEnsureCN other)
  =>
  Maybe (QAId to) {-^ my id -} -> SequenceA to t -> other -> AccuracySG `to` (AccuracySG, Maybe t)
mulGetInitQ1T _me _seq other =
  arr $ \q -> (mulGetInitAC other q, Nothing)

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |], [t| Dyadic |]]
  (\ t -> [d|

  instance
    (QAArrow to, CanMulAsymmetric a $t, SuitableForSeq a, SuitableForSeq (MulType a $t))
    =>
    CanMulAsymmetric (SequenceA to a) $t
    where
    type MulType (SequenceA to a) $t = SequenceA to (MulType a $t)
    mul = binaryOpWithPureArg "*" mul mulGetInitQ1T

  instance
    (QAArrow to, CanMulAsymmetric $t b, SuitableForSeq b, SuitableForSeq (MulType $t b))
    =>
    CanMulAsymmetric $t (SequenceA to b)
    where
    type MulType $t (SequenceA to b) = SequenceA to (MulType $t b)
    mul = flip $ binaryOpWithPureArg "*" (flip mul) mulGetInitQ1T

  |]))
