{-|
    Module      :  AERN2.WithGlobalParam.Branching
    Description :  branching operations
    Copyright   :  (c) Michal Konecny, Eike Neumann
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Branching operations for WithGlobalParam objects.
-}
module AERN2.WithGlobalParam.Branching
(
  -- pickNonZeroWGParamA
)
where

import MixedTypesNumPrelude hiding (id)
-- import qualified Prelude as P

import Control.Arrow

import AERN2.QA.Protocol
import AERN2.WithGlobalParam.Type
-- import AERN2.WithGlobalParam.Comparison ()

-- {- non-zero picking -}
--
-- {-|
--   Given a list @[(a1,b1),(a2,b2),...]@ and assuming that
--   at least one of @a1,a2,...@ is non-zero, pick one of them
--   and return the corresponding pair @(ai,bi)@.
--
--   If none of @a1,a2,...@ is zero, either throw an exception
--   or loop forever.
--  -}
-- pickNonZeroWGParamA ::
--   (QAArrow to, CanPickNonZero a)
--   =>
--   Maybe (QAId to) ->
--   [(WithGlobalParamA to prm a, s)] `to` Maybe (WithGlobalParamA to prm a, s)
-- pickNonZeroWGParamA src =
--     proc seqsAndS -> do
--       balls <- wgprmQueryA src -< (map fst seqsAndS, prm)
--       let maybeNonZero = pickNonZero $ zip balls seqsAndS
--       case maybeNonZero of
--         Just (_,result) -> returnA -< Just result
--         _ -> startFromAccuracy (ac + 1) -< seqsAndS
--
-- instance (CanPickNonZero a) => CanPickNonZero (WithGlobalParam prm a) where
--   pickNonZero = pickNonZeroWGParamA Nothing

{-| lifted if-then-else -}
instance
  (QAArrow to, HasIfThenElse b t
  , SuitableForWGParam prm b, SuitableForWGParam prm t, SuitableForWGParam prm (IfThenElseType b t))
  =>
  HasIfThenElse (WithGlobalParamA to prm b) (WithGlobalParamA to prm t)
  where
  type IfThenElseType (WithGlobalParamA to prm b) (WithGlobalParamA to prm t) = (WithGlobalParamA to prm (IfThenElseType b t))
  ifThenElse (b::WithGlobalParamA to prm b) (e1::WithGlobalParamA to prm t) e2 =
    newWGParam samplePrm sampleT "pif" [AnyProtocolQA b, AnyProtocolQA e1, AnyProtocolQA e2] makeQ
    where
    sampleT = undefined :: (IfThenElseType b t)
    samplePrm = undefined :: Maybe prm
    makeQ (me,_src) =
      proc prm ->
        do
        bP <- wgprmQueryA me -< (b, prm)
        e1P <- wgprmQueryA me -< (e1, prm)
        e2P <- wgprmQueryA me -< (e2, prm)
        returnA -< if bP then e1P else e2P
