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
  , subs, subE, precE
  , normaliseTopLevel, unifyPrecTopLevel
  , combineSampleEfforts
  -- , empty
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P

import Control.Lens

-- import Data.Maybe
import qualified Data.Map as Map

-- import Control.Monad.Trans.State

import AERN2.QA.Protocol

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
  lookupQACache _ _ _ = (Nothing, Nothing)
  updateQACache _ _ _ _ = ()

type Effort = Map.Map String EffortItem
data EffortItem
  = EffortNum { _effortNum :: Integer }
  -- | EffortFun { effortFun :: Integer -> Integer }
  -- | EffortFun { effortFunSampleArg :: Effort, effortFun :: Effort -> Integer }
  | EffortSub { _effortSub :: Effort }
  deriving (Show)

makePrisms ''EffortItem

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

precE :: Traversal' Effort Integer
precE = at "prec" . _Just . _EffortNum

empty :: Effort
empty = Map.empty

-- normalise :: Effort -> Effort
-- normalise e =
--   normaliseTopLevel $ e & subs %~ normalise


normaliseTopLevel :: Effort -> Effort
normaliseTopLevel e =
  Map.filter nonEmpty e
  where
  nonEmpty (EffortSub eSub) = not $ Map.null eSub
  nonEmpty _ = True

-- unifyPrec :: Effort -> Effort
-- unifyPrec e =
--   unifyPrecTopLevel $
--     e & subs %~ unifyPrec

unifyPrecTopLevel :: Effort -> Effort
unifyPrecTopLevel e =
  normaliseTopLevel $
    (e & subs %~ removePrec) & precE .~ p
  where
    removePrec = at "prec" .~ Nothing
    p = maximum $ (e ^.. subs . precE) ++ (e ^.. precE)

combineSampleEfforts ::
  Effort -> Effort ->
  (Effort, Effort -> Effort, Effort -> Effort)
combineSampleEfforts eL eR =
  (sampleEff, projectL, projectR)
  where
  sampleEff =
    unifyPrecTopLevel $
      empty & subE "L" .~ eL & subE "R" .~ eR
  projectL = project "L"
  projectR = project "R"
  project subname eff =
    case (eff ^? precE, eff ^? subE subname) of
      (Just p, Just e) -> e & precE .~ p
      (_, Just e) -> e
      _ -> eff

{- TODO: expand and move the following to separate modules -}

instance
  (CanAddAsymmetric t1 t2, Show (AddType t1 t2))
  =>
  CanAddAsymmetric (WithEffort t1) (WithEffort t2) where
  type AddType (WithEffort t1) (WithEffort t2) = WithEffort (AddType t1 t2)
  add x y =
    newQA name [] p sampleEff makeQ
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
