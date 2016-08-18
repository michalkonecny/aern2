{-# LANGUAGE TemplateHaskell, Rank2Types #-}
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
  , Effort, EffortItem(..)
  , subs, subE
  , empty, normalise,
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P

import Control.Lens

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

type Effort = Map.Map String EffortItem
data EffortItem
  = EffortNum { _effortNum :: Integer }
  -- | EffortFun { effortFun :: Integer -> Integer }
  -- | EffortFun { effortFunSampleArg :: Effort, effortFun :: Effort -> Integer }
  | EffortSub { _effortSub :: Effort }
  deriving (Show)

makePrisms ''EffortItem

empty :: Effort
empty = Map.empty

normalise :: Effort -> Effort
normalise e =
  Map.filter nonEmpty $
    e & subs %~ normalise
  where
  nonEmpty (EffortSub eSub) = not $ Map.null eSub
  nonEmpty _ = True

subs :: Traversal' Effort Effort
subs = each . _EffortSub

subE :: String -> Prism' Effort Effort
subE subname = prism' makeSub extractSub
  where
    makeSub e =
      -- at subname ?~ EffortSub e $ empty
      Map.singleton subname (EffortSub e)
    extractSub e =
      case e ^. at subname of
        Just (EffortSub eSub) -> Just eSub
        _ -> Nothing

{- TODO: expand and move the following to separate modules -}

instance (CanAddAsymmetric t1 t2) => CanAddAsymmetric (WithEffort t1) (WithEffort t2) where
  type AddType (WithEffort t1) (WithEffort t2) = WithEffort (AddType t1 t2)
  add x y =
    QA name Nothing p sampleEff makeQ
    where
      name = "+" -- "(" ++ (qaName x) ++ "+" ++ (qaName y) ++ ")"
      p = WithEffortP (sampleX + sampleY)
      WithEffortP sampleX = qaProtocol x
      WithEffortP sampleY = qaProtocol y
      (sampleEff, projectXEff, projectYEff) =
        combineSampleEfforts xSampleEff ySampleEff
      xSampleEff = qaSampleQ x
      ySampleEff = qaSampleQ y
      makeQ eff = xB + yB
        where
        xB = qaMakeQuery x $ projectXEff eff
        yB = qaMakeQuery y $ projectYEff eff

combineSampleEfforts e1 e2
  = undefined
  -- | bothHavePrec =
  --     (Map.singleton "prec" prec1, id, id)
  --     (Map.fromList [("prec", prec1)], e1WithoutPrec, e2WithoutPrec])
  -- (e, proj1, proj2)
  -- where
  --   e = e1
