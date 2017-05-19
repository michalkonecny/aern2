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
  , QA(..), QAPromiseA, (?..), AnyProtocolQA(..)
  , addUnsafeMemoisation
  -- * QAArrows
  , QAArrow(..), qaMakeQuery, qaMakeQueryA, qaMakeQueriesA, qaMakeQueryOnManyA
  , (?)
  , (-:-), (-?-), (-???-), (-<?>-)
  , qaMake2Queries, (??)
  -- * arrow utilities
  , mapA, CanSwitchArrow(..)
)
where

import Numeric.MixedTypes
import qualified Prelude as P
-- import Text.Printf

import System.IO.Unsafe (unsafePerformIO)

import Control.Arrow
import Control.Concurrent.MVar

-- import Data.Maybe
import Data.List

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

{-| An object we can ask queries about.  Queries can be asked in some Arrow @to@. -}
data QA to p = QA__
  {
    qaName :: String,
    qaId :: Maybe (QAId to),
    qaSources :: [QAId to],
    qaProtocol :: p,
    qaSampleQ :: Q p,
    qaMakeQueryGetPromise :: (Q p) `to` (QAPromiseA to (A p))
  }

type QAPromiseA to a = () `to` a

{-| An infix synonym of 'qaMakeQuery'. -}
(?..) :: QA to p -> (Q p) `to` (QAPromiseA to (A p))
(?..) = qaMakeQueryGetPromise

infix 1 ?..

data AnyProtocolQA to =
  forall p. (QAProtocolCacheable p) => AnyProtocolQA (QA to p)

anyPqaId :: AnyProtocolQA to -> (Maybe (QAId to))
anyPqaId (AnyProtocolQA qa) = qaId qa

anyPqaSources :: AnyProtocolQA to -> [QAId to]
anyPqaSources (AnyProtocolQA qa) = qaSources qa

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
  qaRegister :: (QAProtocolCacheable p) => (QA to p) `to` (QA to p)
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
    String -> [AnyProtocolQA to] -> p -> Q p -> (Q p) `to` (A p) -> QA to p
  newQA = defaultNewQA
  qaFulfilPromiseA :: (QAPromiseA to a) `to` a
  qaMakeQueryGetPromiseA :: (QA to p, Q p) `to` (QAPromiseA to (A p))

defaultNewQA ::
  (QAArrow to, QAProtocolCacheable p) =>
  String -> [AnyProtocolQA to] -> p -> Q p -> (Q p) `to` (A p) -> QA to p
defaultNewQA name sources p sampleQ makeQ =
  QA__ name Nothing (nub $ concat $ map getSourceIds sources) p sampleQ makeQPromise
  where
  getSourceIds source =
    case anyPqaId source of
      Just id1 -> [id1]
      Nothing -> anyPqaSources source
  makeQPromise =
    proc acSG ->
      returnA -< promise acSG
  promise acSG =
    proc () ->
      do
      a <- makeQ -< acSG
      returnA -< a

qaMakeQuery :: (QAArrow to) => (QA to p) -> (Q p) `to` (A p) -- ^ composition of qaMakeQueryGetPromise and the execution of the promise
qaMakeQuery qa = (qaMakeQueryGetPromise qa) >>> qaFulfilPromiseA

qaMakeQueryA :: (QAArrow to) => (QA to p, Q p) `to` (A p)
qaMakeQueryA = qaMakeQueryGetPromiseA >>> qaFulfilPromiseA

qaMakeQueriesA :: (QAArrow to) => [(QA to p, Q p)] `to` [A p]
qaMakeQueriesA = (mapA qaMakeQueryGetPromiseA) >>> (mapA qaFulfilPromiseA)

qaMakeQueryOnManyA :: (QAArrow to) => ([QA to p], Q p) `to` [A p]
qaMakeQueryOnManyA =
  proc (qas, q) -> qaMakeQueriesA -< map (flip (,) q) qas

{-| An infix synonym of 'qaMakeQuery'. -}
(?) :: (QAArrow to) => QA to p -> (Q p) `to` (A p)
(?) = qaMakeQuery

infix 1 ?

{-| An infix synonym of 'qaRegister'. -}
(-:-) :: (QAArrow to, QAProtocolCacheable p) => (QA to p) `to` (QA to p)
(-:-) = qaRegister

{-| An infix synonym of 'qaMakeQueryA'. -}
(-?-) :: (QAArrow to) => (QA to p, Q p) `to` (A p)
(-?-) = qaMakeQueryA

{-| An infix synonym of 'qaMakeQueryOnManyA'. -}
(-<?>-) :: (QAArrow to) => ([QA to p], Q p) `to` [A p]
(-<?>-) = qaMakeQueryOnManyA

{-| An infix synonym of 'qaMakeQueriesA'. -}
(-???-) :: (QAArrow to) => [(QA to p, Q p)] `to` [A p]
(-???-) = qaMakeQueriesA

infix 0 -?-, -???-, -<?>-
infix 0 -:-

{-| An infix synonym of 'qaMake2Queries'. -}
(??) :: (QAArrow to) => (QA to p1, QA to p2) -> (Q p1, Q p2) `to` (A p1, A p2)
(??) = qaMake2Queries

infix 1 ??

{-| Run two queries in an interleaving manner, enabling parallelism. -}
qaMake2Queries :: (QAArrow to) => (QA to p1, QA to p2) -> (Q p1, Q p2) `to` (A p1, A p2)
qaMake2Queries (qa1, qa2) =
  proc (q1,q2) ->
    do
    ap1 <- qaMakeQueryGetPromiseA -< (qa1, q1)
    ap2 <- qaMakeQueryGetPromiseA -< (qa2, q2)
    a1 <- qaFulfilPromiseA -< ap1
    a2 <- qaFulfilPromiseA -< ap2
    returnA -< (a1,a2)

-- (//..) :: a -> b -> (a,b)
-- a //..b = (a,b)

{- Arrow swiching mechanism and application to QA arrow conversion -}

class CanSwitchArrow to1 to2 where
  switchArrow :: (a `to1` b) -> (a `to2` b)
  -- switchArrow2 :: (a `to1` (b `to1` c)) -> (a `to2` (b `to2` c))

instance (Arrow to) => CanSwitchArrow (->) to where
  switchArrow = arr
  -- switchArrow2 = arr . (arr .)

instance
  (CanSwitchArrow to1 to2, QAArrow to1, QAArrow to2, QAProtocolCacheable p)
  =>
  ConvertibleExactly (QA to1 p) (QA to2 p)
  where
  safeConvertExactly qa =
    Right $ defaultNewQA (qaName qa) [] (qaProtocol qa) (qaSampleQ qa) (switchArrow $ qaMakeQuery qa)


{-| Apply an arrow morphism on all elements of a list -}
mapA :: (ArrowChoice to) => (t1 `to` t2) -> ([t1] `to` [t2])
mapA fA =
  proc list -> do
    case list of
      [] -> returnA -< []
      (x : xs) -> do
        y <- fA -< x
        ys <-mapA fA -< xs
        returnA -< y : ys

{- Trivial QAArrow instance -}

{-|
  Normal Haskell functions are a trivial QAArrow instance
  where registration has no effect.
-}
instance QAArrow (->) where
  type QAId (->) = ()
  qaRegister = id
  newQA name sources p sampleQ makeQ =
    addUnsafeMemoisation $
      defaultNewQA name sources p sampleQ makeQ
  qaMakeQueryGetPromiseA = uncurry qaMakeQueryGetPromise
  qaFulfilPromiseA promise = promise ()

{-|
  Add caching to pure (->) QA objects via unsafe memoization, inspired by
  https://hackage.haskell.org/package/ireal-0.2.3/docs/src/Data-Number-IReal-UnsafeMemo.html#unsafeMemo,
  which, in turn, is inspired by Lennart Augustsson's uglymemo.
-}
addUnsafeMemoisation :: (QAProtocolCacheable p) => QA (->) p -> QA (->) p
addUnsafeMemoisation qa = qa { qaMakeQueryGetPromise = unsafeMemo }
  where
  unsafeMemo = (unsafePerformIO .) . unsafePerformIO memoIO
  p = qaProtocol qa
  -- name = qaName qa
  memoIO =
    do
    -- putStrLn $ "memoIO starting for " ++ name
    cacheVar <- newMVar $ newQACache p
    return $ useMVar cacheVar
    where
    useMVar cacheVar q () =
      do
      -- putStrLn $ "memoIO: q = " ++ (show q)
      cache <- readMVar cacheVar
      -- putStrLn $ "memoIO: got cache"
      case lookupQACache p cache q of
        (Just a, _logMsg) ->
          do
          -- putStrLn $ printf "memoIO %s: using cache: ? %s -> ! %s" name (show q) (show a)
          return a
        _ ->
          do
          let a = qaMakeQueryGetPromise qa q ()
          modifyMVar_ cacheVar (const (return (updateQACache p q a cache)))
          -- putStrLn $ printf "memoIO  %s: updated cache: ? %s -> ! %s" name (show q) (show a)
          return a
