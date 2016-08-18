{-|
    Module      :  AERN2.Effort
    Description :  Effort indicator to direct enclosure computation
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Effort indicator to direct enclosure computation
-}
module AERN2.Effort
(
  WithEffort, WithEffortP(..)
  , Effort(..), EffortItem(..)
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P

-- import Control.Arrow
-- import Data.Maybe
import qualified Data.Map as Map

-- import Control.Monad.Trans.State

import AERN2.QA

-- import Debug.Trace (trace)
--
-- shouldTrace :: Bool
-- shouldTrace = False
-- -- shouldTrace = True
--
-- maybeTrace :: String -> a -> a
-- maybeTrace
--     | shouldTrace = trace
--     | otherwise = const id
--
-- _dummy :: ()
-- _dummy = maybeTrace "dummy" ()

type WithEffort t = QA (->) (WithEffortP t)

newtype WithEffortP t = WithEffortP t deriving (Show)

instance (Show t) => QAProtocol (WithEffortP t) where
  type Q (WithEffortP t) = Effort
  type A (WithEffortP t) = t

instance (Show t) => QAProtocolCacheable (WithEffortP t) where
  type QACache (WithEffortP t) = () -- dummy cache
  newQACache _ = ()
  lookupQACache _ _ _ = Nothing
  updateQACache _ _ _ _ = ()

newtype Effort = Effort (Map.Map String EffortItem)
  deriving (Show)
data EffortItem
  = EffortNum { effortNum :: Integer }
  -- | EffortFun { effortFun :: Integer -> Integer }
  -- | EffortFun { effortFunSampleArg :: Effort, effortFun :: Effort -> Integer }
  | EffortSub { effortSub :: Effort }
  deriving (Show)
