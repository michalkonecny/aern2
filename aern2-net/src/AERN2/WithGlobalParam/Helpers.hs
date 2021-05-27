{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|
    Module      :  AERN2.WithGlobalParam.Helpers
    Description :  helper functions for operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Helper functions for defining operations over WithGlobalParam objects.
-}
module AERN2.WithGlobalParam.Helpers
(
  -- Operations returning WithGlobalParam
  unaryOp, binaryOp, binaryOpWithPureArg
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

import AERN2.QA.Protocol
import AERN2.WithGlobalParam.Type

{- generic implementations of operations of different arity -}

unaryOp ::
  (QAArrow to, SuitableForWGParam prm a, SuitableForWGParam prm b)
  =>
  String ->
  (a -> b) ->
  WithGlobalParamA to prm a -> WithGlobalParamA to prm b
unaryOp name op aWGPrm =
  newWGParam samplePrm (op sampleA) name [AnyProtocolQA aWGPrm] makeQ
  where
  WithGlobalParamP samplePrm sampleA = qaProtocol aWGPrm
  makeQ (me, _src) =
    proc ac ->
      do
      a <- wgprmQuery aWGPrm me -< ac
      returnA -< op a

binaryOpWithPureArg ::
  (QAArrow to, SuitableForWGParam prm a, SuitableForWGParam prm c)
  =>
  String -> (a -> t -> c) -> WithGlobalParamA to prm a -> t -> WithGlobalParamA to prm c
binaryOpWithPureArg name op aWGPrm b =
  newWGParam samplePrm (op sampleA b) name [AnyProtocolQA aWGPrm] makeQ
  where
  WithGlobalParamP samplePrm sampleA = qaProtocol aWGPrm
  makeQ (me, _src) =
    proc ac ->
      do
      a <- wgprmQuery aWGPrm me -< ac
      returnA -< op a b

binaryOp ::
  (QAArrow to, SuitableForWGParam prm a, SuitableForWGParam prm b, SuitableForWGParam prm c)
  =>
  String -> (a -> b -> c) -> WithGlobalParamA to prm a -> WithGlobalParamA to prm b -> WithGlobalParamA to prm c
binaryOp name op aWGPrm bWGPrm =
  newWGParam samplePrm (op sampleA sampleB) name [AnyProtocolQA aWGPrm, AnyProtocolQA bWGPrm] makeQ
  where
  WithGlobalParamP samplePrm sampleA = qaProtocol aWGPrm
  WithGlobalParamP _ sampleB = qaProtocol bWGPrm
  makeQ (me, _src) =
    proc ac ->
      do
      a <- wgprmQuery aWGPrm me -< ac
      b <- wgprmQuery bWGPrm me -< ac
      returnA -< op a b
