{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|
    Module      :  AERN2.QA.Strategy.Parallel
    Description :  QA net parallel evaluation
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    QA net parallel evaluation
-}
module AERN2.QA.Strategy.Parallel
(
  -- QAParA, QANetInfo(..), executeQAParA
)
where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#else
#define maybeTrace (\ (_ :: String) t -> t)
#endif

import Numeric.MixedTypes
import qualified Prelude as P
-- import Text.Printf

import Control.Arrow

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.State

-- import Data.Maybe
-- import Data.List
-- import qualified Data.Map as Map


import AERN2.QA.Protocol
import AERN2.QA.Strategy.Cached

-- TODO
