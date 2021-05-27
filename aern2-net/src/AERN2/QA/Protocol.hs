{-# LANGUAGE ExistentialQuantification #-}
{-|
    Module      :  AERN2.QA.Protocol
    Description :  Cacheable question-answer protocols
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Cacheable question-answer protocols
-}
module AERN2.QA.Protocol
(
  -- * QA protocols and objects
  QAProtocol(..), QAProtocolCacheable(..)
  , QA(..), QAPromiseA, (?..)
  , qaRename
  , mapQA, mapQAsameQ
  -- * QAArrows
  , AnyProtocolQA(..)
  , QAArrow(..), defaultNewQA, QARegOption(..)
  , qaMakeQuery, qaMakeQueryA, qaMakeQueriesA, qaMakeQueryOnManyA
  , (?<-), (?)
  , (-:-), (-:-|), (-:-||)
  , (-?<-), (-?-), (-?..<-), (-?..-), (-???<-), (-<?<->-)
  , qaMake2Queries, (??<-)
  , qaMake3Queries
)
where

import MixedTypesNumPrelude
import qualified Prelude as P
-- import Text.Printf

import Control.Arrow
import AERN2.Utils.Arrows

-- import Data.Maybe
import Data.List

import Control.CollectErrors

{-| A QA protocol at this level is simply a pair of types. -}
class (Show p, Show (Q p), Show (A p)) => QAProtocol p where
  type Q p -- a type of queries
  type A p -- a type of answers

{-| A QA protocol with a caching method. -}
class (QAProtocol p, HasOrderCertainly (Q p) (Q p)) => QAProtocolCacheable p where
  type QACache p
  newQACache :: p -> QACache p
  lookupQACache :: p -> QACache p -> Q p -> (Maybe (A p), Maybe String) -- ^ the String is a log message
  updateQACache :: p -> Q p -> A p -> QACache p -> QACache p

instance (QAProtocol p, CanBeErrors es) => (QAProtocol (CollectErrors es p)) where
  type Q (CollectErrors es p) = Q p
  type A (CollectErrors es p) = CollectErrors es (A p)

{-| An object we can ask queries about.  Queries can be asked in some Arrow @to@. -}
data QA to p = QA__
  {
    qaName :: String,
    qaId :: Maybe (QAId to),
    qaSources :: [QAId to],
    qaProtocol :: p,
    qaSampleQ :: Maybe (Q p),
    qaMakeQueryGetPromise ::
      (Maybe (QAId to), Maybe (QAId to)) -- this node id, source of query
      ->
      (Q p) `to` (QAPromiseA to (A p))
  }

type QAPromiseA to a = () `to` a

{-| An infix synonym of 'qaMakeQuery'. -}
(?..) :: QA to p -> (Q p) `to` (QAPromiseA to (A p))
(?..) qa = qaMakeQueryGetPromise qa (Nothing, Nothing)

infix 1 ?..

qaRename :: (String -> String) -> QA to p -> QA to p
qaRename f qa = qa {  qaName = f (qaName qa)  }

mapQA ::
  (Arrow to) =>
  (p1 -> p2) ->
  (Q p1 -> Q p2) ->
  (Q p2 -> Q p1) ->
  (A p1 -> A p2) ->
  QA to p1 -> QA to p2
mapQA
    translateP translateQ translateBackQ translateA
    (QA__ name qaid sources p sampleQ makeQ) =
  QA__ name qaid sources (translateP p) (fmap translateQ sampleQ) $
    \ source -> (arr $ ((arr translateA) <<<) ) <<< makeQ source <<< arr translateBackQ

mapQAsameQ ::
  (Arrow to, Q p1 ~ Q p2) =>
  (p1 -> p2) ->
  (A p1 -> A p2) ->
  QA to p1 -> QA to p2
mapQAsameQ translateP = mapQA translateP id id

data AnyProtocolQA to =
  forall p. (QAProtocolCacheable p) => AnyProtocolQA (QA to p)

anyPqaId :: AnyProtocolQA to -> (Maybe (QAId to))
anyPqaId (AnyProtocolQA qa) = qaId qa

anyPqaSources :: AnyProtocolQA to -> [QAId to]
anyPqaSources (AnyProtocolQA qa) = qaSources qa

data QARegOption =
  QARegPreferParallel | QARegPreferSerial
  deriving (P.Eq)

{-|
  A class of Arrows suitable for use in QA objects.
-}
class (ArrowChoice to, P.Eq (QAId to)) => QAArrow to where
  type QAId to
  {-|
    Register a QA object, which leads to a change in its
    query processing mechanism so that, eg, answers can be cached
    or computations assigned to different threads/processes.

    The "sources" component of the incoming QA object can be
    used to record the dependency graph among QA objects.
    After registration, the QA object should have its list
    of dependencies **empty**
    as the registration has recorded them elsewhere.
  -}
  qaRegister :: (QAProtocolCacheable p) => [QARegOption] -> (QA to p) `to` (QA to p)
  {-|
    Create a qa object.  The object is not "registered" automatically.
    Invoking this function does not lead to any `to'-arrow computation.
    The function is an operation of 'QAArrow' so that for some arrows,
    the question-answer mechanism can be automatically altered.
    In particular, this is used to make all objects in the (->) arrow
    automatically (unsafely) caching their answers.
    For most arrows, the default implementation is sufficient.
  -}
  newQA :: (QAProtocolCacheable p) =>
    String -> [AnyProtocolQA to] -> p -> Maybe (Q p) -> ((Maybe (QAId to), Maybe (QAId to)) -> (Q p) `to` (A p)) -> QA to p
  newQA = defaultNewQA
  qaFulfilPromiseA :: (QAPromiseA to a) `to` a
  qaMakeQueryGetPromiseA :: Maybe (QAId to) -> (QA to p, Q p) `to` (QAPromiseA to (A p))

defaultNewQA ::
  (QAArrow to, QAProtocolCacheable p) =>
  String -> [AnyProtocolQA to] -> p -> Maybe (Q p) ->
  ((Maybe (QAId to), Maybe (QAId to)) -> (Q p) `to` (A p)) -> QA to p
defaultNewQA name sources p sampleQ makeQ =
  QA__ name Nothing (nub $ concat $ map getSourceIds sources) p sampleQ makeQPromise
  where
  getSourceIds source =
    case anyPqaId source of
      Just id1 -> [id1]
      Nothing -> anyPqaSources source
  makeQPromise me_src =
    proc acSG ->
      returnA -< promise acSG
    where
    promise acSG =
      proc () ->
        do
        a <- makeQ me_src -< acSG
        returnA -< a

qaMakeQuery :: (QAArrow to) => (QA to p) -> (Maybe (QAId to)) -> (Q p) `to` (A p)
  -- ^ composition of qaMakeQueryGetPromise and the execution of the promise
qaMakeQuery qa src = (qaMakeQueryGetPromise qa (me, src)) >>> qaFulfilPromiseA
  where
  me = case qaId qa of Nothing -> src; me2 -> me2

qaMakeQueryA :: (QAArrow to) => Maybe (QAId to) -> (QA to p, Q p) `to` (A p)
qaMakeQueryA src = qaMakeQueryGetPromiseA src >>> qaFulfilPromiseA

qaMakeQueriesA :: (QAArrow to) => Maybe (QAId to) -> [(QA to p, Q p)] `to` [A p]
qaMakeQueriesA src = (mapA (qaMakeQueryGetPromiseA src)) >>> (mapA qaFulfilPromiseA)

qaMakeQueryOnManyA :: (QAArrow to) => Maybe (QAId to) -> ([QA to p], Q p) `to` [A p]
qaMakeQueryOnManyA src =
  proc (qas, q) -> qaMakeQueriesA src -< map (flip (,) q) qas

{-| An infix synonym of 'qaMakeQuery' -}
(?<-) :: (QAArrow to) => QA to p -> Maybe (QAId to) -> (Q p) `to` (A p)
(?<-) = qaMakeQuery

{-| An infix synonym of 'qaMakeQuery' with no source -}
(?) :: (QAArrow to) => QA to p -> (Q p) `to` (A p)
(?) = \qa -> qaMakeQuery qa Nothing

infix 1 ?, ?<-

{-| An infix synonym of 'qaRegister' -}
(-:-) :: (QAArrow to, QAProtocolCacheable p) => (QA to p) `to` (QA to p)
(-:-) = qaRegister []

{-| An infix synonym of 'qaRegister' -}
(-:-||) :: (QAArrow to, QAProtocolCacheable p) => (QA to p) `to` (QA to p)
(-:-||) = qaRegister [QARegPreferParallel]

{-| An infix synonym of 'qaRegister' -}
(-:-|) :: (QAArrow to, QAProtocolCacheable p) => (QA to p) `to` (QA to p)
(-:-|) = qaRegister [QARegPreferSerial]

{-| An infix synonym of 'qaMakeQueryGetPromiseA' -}
(-?..<-) :: (QAArrow to) => Maybe (QAId to) -> (QA to p, Q p) `to` (QAPromiseA to (A p))
(-?..<-) = qaMakeQueryGetPromiseA

{-| An infix synonym of 'qaMakeQueryGetPromiseA' with no source -}
(-?..-) :: (QAArrow to) => (QA to p, Q p) `to` (QAPromiseA to (A p))
(-?..-) = qaMakeQueryGetPromiseA Nothing

{-| An infix synonym of 'qaMakeQueryA' -}
(-?<-) :: (QAArrow to) => Maybe (QAId to) -> (QA to p, Q p) `to` (A p)
(-?<-) = qaMakeQueryA

{-| An infix synonym of 'qaMakeQueryA' with no source -}
(-?-) :: (QAArrow to) => (QA to p, Q p) `to` (A p)
(-?-) = qaMakeQueryA Nothing

{-| An infix synonym of 'qaMakeQueryOnManyA' -}
(-<?<->-) :: (QAArrow to) => Maybe (QAId to) -> ([QA to p], Q p) `to` [A p]
(-<?<->-) = qaMakeQueryOnManyA

{-| An infix synonym of 'qaMakeQueriesA' -}
(-???<-) :: (QAArrow to) => Maybe (QAId to) -> [(QA to p, Q p)] `to` [A p]
(-???<-) = qaMakeQueriesA

infix 0 -?<-, -?..<-, -???<-, -<?<->-
infix 0 -:-, -:-|, -:-||

{-| An infix synonym of 'qaMake2Queries'. -}
(??<-) :: (QAArrow to) => (QA to p1, QA to p2) -> Maybe (QAId to) -> (Q p1, Q p2) `to` (A p1, A p2)
(??<-) = qaMake2Queries

infix 0 ??<-

{-| Run two queries in an interleaving manner, enabling parallelism. -}
qaMake2Queries :: (QAArrow to) => (QA to p1, QA to p2) -> Maybe (QAId to) -> (Q p1, Q p2) `to` (A p1, A p2)
qaMake2Queries (qa1, qa2) src =
  proc (q1,q2) ->
    do
    ap1 <- (-?..<-) src -< (qa1, q1)
    ap2 <- (-?..<-) src -< (qa2, q2)
    a1 <- qaFulfilPromiseA -< ap1
    a2 <- qaFulfilPromiseA -< ap2
    returnA -< (a1,a2)

{-| Run two queries in an interleaving manner, enabling parallelism. -}
qaMake3Queries ::
  (QAArrow to) =>
  (QA to p1, QA to p2, QA to p3) -> Maybe (QAId to) -> (Q p1, Q p2, Q p3) `to` (A p1, A p2, A p3)
qaMake3Queries (qa1, qa2, qa3) src =
  proc (q1,q2,q3) ->
    do
    ap1 <- (-?..<-) src -< (qa1, q1)
    ap2 <- (-?..<-) src -< (qa2, q2)
    ap3 <- (-?..<-) src -< (qa3, q3)
    a1 <- qaFulfilPromiseA -< ap1
    a2 <- qaFulfilPromiseA -< ap2
    a3 <- qaFulfilPromiseA -< ap3
    returnA -< (a1,a2,a3)

{- arrow conversions -}

instance
  (CanSwitchArrow to1 to2, QAArrow to1, QAArrow to2, QAProtocolCacheable p)
  =>
  ConvertibleExactly (QA to1 p) (QA to2 p)
  where
  safeConvertExactly qa =
    Right $ defaultNewQA (qaName qa) [] (qaProtocol qa) (qaSampleQ qa) (\ _src -> switchArrow (qaMakeQuery qa Nothing))
