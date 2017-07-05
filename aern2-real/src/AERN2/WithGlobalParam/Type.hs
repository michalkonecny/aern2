{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- #define DEBUG
{-|
    Module      :  AERN2.WithGlobalParam.Type
    Description :  Values that depend on a globale state
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    The type of values that depends on an immutable gloabal parameter,
    such as FP precision.
-}
module AERN2.WithGlobalParam.Type
(
  -- * The protocol and type of fast converging sequences
  WithGlobalParamP(..), pWGParam
  , SuitableForWGParam
  , wgprmName, wgprmId, wgprmSources, wgprmRename
  , wgprmQuery, wgprmQueryA, wgprmListQueryA
  , WithGlobalParamA, WithGlobalParam
  , newWGParam, newWGParamSimple
  , fmapWGParam
)
where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO (\ (_ :: String) -> return ())
#endif

import MixedTypesNumPrelude
-- import qualified Prelude as P

import Control.Arrow
import Control.Monad (join)

import Text.Printf

import Control.CollectErrors

-- import AERN2.MP
-- import AERN2.MP.Dyadic

import AERN2.QA.Protocol
import AERN2.QA.Strategy.CachedUnsafe ()

{- QA protocol -}

data WithGlobalParamP prm a =
  WithGlobalParamP { withGlobalState_s :: Maybe prm, withGlobalState_a :: a} deriving (Show)

pWGParam :: Maybe prm -> a -> WithGlobalParamP prm a
pWGParam prm a = WithGlobalParamP prm a

instance (Show a, Show prm) => QAProtocol (WithGlobalParamP prm a) where
  type Q (WithGlobalParamP prm a) = prm
  type A (WithGlobalParamP prm a) = a

type SuitableForWGParam prm a = (Show a, Show prm, HasOrderCertainly prm prm)

instance
  SuitableForWGParam prm a
  =>
  QAProtocolCacheable (WithGlobalParamP prm a)
  where
  type QACache (WithGlobalParamP prm a) = Maybe (a, prm)
  newQACache _ = Nothing
  lookupQACache _ cache prm =
    case cache of
      Just (b, prmC) | prm !<=! prmC -> (Just b, Just (logMsg b))
      Just (b, _) -> (Nothing, Just (logMsg b))
      Nothing -> (Nothing, Just ("cache empty"))
    where
    logMsg _b = printf "query: %s; cache: %s" (show prm) (show cache)
  updateQACache _ prm b _ = Just (b, prm)

instance Functor (WithGlobalParamP prm) where
  fmap f (WithGlobalParamP prm a) = WithGlobalParamP prm (f a)

{- Objects -}

type WithGlobalParamA to prm a = QA to (WithGlobalParamP prm a)
type WithGlobalParam prm a = WithGlobalParamA (->) prm a

fmapWGParam ::
  (Arrow to) =>
  (a -> b) -> (WithGlobalParamA to prm a) -> (WithGlobalParamA to prm b)
fmapWGParam f = mapQAsameQ (fmap f) f

wgprmName :: WithGlobalParamA to prm a -> String
wgprmName = qaName
--
wgprmRename :: (String -> String) -> WithGlobalParamA to prm a -> WithGlobalParamA to prm a
wgprmRename = qaRename

wgprmId :: WithGlobalParamA to prm a -> Maybe (QAId to)
wgprmId = qaId

wgprmSources :: WithGlobalParamA to prm a -> [QAId to]
wgprmSources = qaSources

{-| Get an approximation of the limit with at least the specified accuracy.
   (A specialisation of 'qaMakeQuery' for values with global state.) -}
wgprmQuery :: (QAArrow to) => WithGlobalParamA to prm a -> Maybe (QAId to) -> prm `to` a
wgprmQuery = (?<-)

wgprmQueryA :: (QAArrow to) => (Maybe (QAId to)) -> (WithGlobalParamA to prm a, prm) `to` a
wgprmQueryA = qaMakeQueryA

wgprmListQueryA :: (QAArrow to) => (Maybe (QAId to)) -> ([WithGlobalParamA to prm a], prm) `to` [a]
wgprmListQueryA = qaMakeQueryOnManyA

{- constructions -}

newWGParam ::
  (QAArrow to, SuitableForWGParam prm a)
  =>
  Maybe prm -> a -> String -> [AnyProtocolQA to] -> ((Maybe (QAId to), Maybe (QAId to)) -> prm `to` a) -> WithGlobalParamA to prm a
newWGParam samplePrm sampleA name sources makeQ =
  newQA name sources (pWGParam samplePrm sampleA) samplePrm makeQ

newWGParamSimple ::
  (QAArrow to, SuitableForWGParam prm a)
  =>
  Maybe prm -> a -> ((Maybe (QAId to), Maybe (QAId to)) -> prm `to` a) -> WithGlobalParamA to prm a
newWGParamSimple samplePrm sampleA = newWGParam samplePrm sampleA "simple" []

{- CollectErrors instances -}

instance
  (SuitableForCE es, CanEnsureCE es a)
  =>
  CanEnsureCE es (WithGlobalParamP prm a)
  where
  type EnsureCE es (WithGlobalParamP prm a) = WithGlobalParamP prm (EnsureCE es a)
  type EnsureNoCE es (WithGlobalParamP prm a) = WithGlobalParamP prm (EnsureNoCE es a)

  ensureCE sample_es = fmap (ensureCE sample_es)
  deEnsureCE sample_es (WithGlobalParamP prm a) = fmap (WithGlobalParamP prm) (deEnsureCE sample_es a)
  ensureNoCE sample_es (WithGlobalParamP prm a) =  fmap (WithGlobalParamP prm) (ensureNoCE sample_es a)

  noValueECE sample_vCE es =
    WithGlobalParamP (join $ fmap withGlobalState_s sample_vCE)
      (noValueECE (fmap withGlobalState_a sample_vCE) es)

  prependErrorsECE sample_vCE es (WithGlobalParamP prm aCE) =
    (WithGlobalParamP prm (prependErrorsECE (fmap withGlobalState_a sample_vCE) es aCE))

instance
  (Arrow to, SuitableForCE es, CanEnsureCE es a)
  =>
  CanEnsureCE es (WithGlobalParamA to prm a)
  where
  type EnsureCE es (WithGlobalParamA to prm a) = WithGlobalParamA to prm (EnsureCE es a)
  type EnsureNoCE es (WithGlobalParamA to prm a) = WithGlobalParamA to prm (EnsureNoCE es a)

  ensureCE sample_es = fmapWGParam (ensureCE sample_es)
  deEnsureCE sample_es = Right . fmapWGParam (removeEither . deEnsureCE sample_es)
    where
    removeEither (Right a) = a
    removeEither (Left es) = error $ "WithGlobalParam deEnsureCE: " ++ show es
  ensureNoCE sample_es = Right . fmapWGParam (removeEither . ensureNoCE sample_es)
    where
    removeEither (Right a) = a
    removeEither (Left es) = error $ "WithGlobalParam ensureNoCE: " ++ show es

  noValueECE _sample_vCE _es =
    error "noValueECE not implemented for WithGlobalParam yet"

  prependErrorsECE (_sample_vCE :: Maybe (WithGlobalParamA to prm a)) es =
    fmapWGParam (prependErrorsECE (Nothing :: Maybe a) es)

-- The following has to be made specific to specific prm and a types
-- so that the dependency on the parameter can be expressed
--
-- $(declForTypes
--   [[t| Integer |], [t| Int |], [t| Dyadic |]]
--   (\ t -> [d|
--
--     instance
--       (QAArrow to, ConvertibleExactly $t a, CanSetPrecision a, SuitableForWGParam prm a)
--       =>
--       ConvertibleExactly $t (WithGlobalParamA to prm a)
--       where
--       safeConvertExactly x =
--         Right $ newWGParam Nothing a (show x) [] (\_src -> arr $ \_prm -> a)
--         where
--         a = convertExactly x
--
--   |]))
