{-|
    Module      :  AERN2.WithGlobalParam
    Description :  adding a global parameter to a type
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable


-}
module AERN2.WithGlobalParam
(
  -- * The protocol and type of objects depending on a global parameter
  WithGlobalParamP(..), pWGParam
  , SuitableForWGParam
  , wgprmName, wgprmId, wgprmSources, wgprmRename
  , wgprmQuery, (?), wgprmQueryA, wgprmListQueryA
  , WithGlobalParamA, WithGlobalParam
  , newWGParam, newWGParamSimple
  , fmapWGParam
  -- * auxiliary functions for making new operations
  , unaryOp, binaryOp, binaryOpWithPureArg
)
where

-- import MixedTypesNumPrelude
-- import qualified Prelude as P

-- import Control.Arrow

import AERN2.QA.Protocol
import AERN2.WithGlobalParam.Type
import AERN2.WithGlobalParam.Helpers
import AERN2.WithGlobalParam.Comparison ()
import AERN2.WithGlobalParam.Branching ()
import AERN2.WithGlobalParam.Ring ()
import AERN2.WithGlobalParam.Field ()
import AERN2.WithGlobalParam.Elementary ()
