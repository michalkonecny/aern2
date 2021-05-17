{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|
    Module      :  AERN2.QA.Strategy.Cached.Arrow
    Description :  QA net evaluation with answer caching
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    QA net evaluation with answer caching
-}
module AERN2.QA.Strategy.Cached.Arrow
(
  module AERN2.QA.NetLog
  , module AERN2.QA.Strategy.Cached.NetState
  , QACachedA
  , executeQACachedA, executeQAUncachedA
)
where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#else
#define maybeTrace (\ (_ :: String) t -> t)
#endif

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

import Control.Arrow

import Control.Monad.Trans.State

import AERN2.QA.Protocol
import AERN2.QA.NetLog
import AERN2.QA.Strategy.Cached.NetState

type QACachedA = Kleisli QACachedM

data QACachedM a =
  QACachedM { unQACachedM :: (State (QANetState QACachedM) a) }

instance Functor QACachedM where
  fmap f (QACachedM ma) = QACachedM (fmap f ma)
instance Applicative QACachedM where
  pure a = QACachedM (pure a)
  (QACachedM f) <*> (QACachedM a) = QACachedM (f <*> a)
instance Monad QACachedM where
  (QACachedM ma) >>= f =
    QACachedM $ ma >>= (unQACachedM . f)

instance QAArrow QACachedA where
  type QAId QACachedA = ValueId
  qaRegister _ = Kleisli qaRegisterM
    where
    qaRegisterM (QA__ name Nothing sourceIds p sampleQ q2paA) =
      do
      valueId <- newId
      return $ QA__ name (Just valueId) [] p sampleQ (\me_src -> (Kleisli $ makeQCached me_src valueId))
      where
      sq2pa me_src = q2pa
        where
        (Kleisli q2pa) = q2paA me_src
      newId =
        maybeTrace ("newId: " ++ show name) $
        QACachedM $
          do
          ns <- get
          let (i, ns') = insertNode p name sourceIds sq2pa ns
          put ns'
          return i
      makeQCached (_me,src) valueId q =
        maybeTrace ("makeQCached: q = " ++ show q) $
        QACachedM $
          do
          ns <- get
          let ns' = logQuery ns src valueId (show q)
          put ns'
          aAndCachePromise <- unQACachedM $ getAnswerPromise ns' p src valueId q
          return $ Kleisli (aPromise aAndCachePromise)
        where
        aPromise aAndCachePromise () =
          QACachedM $
            do
            (a, usedCache, cache') <- unQACachedM $ aAndCachePromise ()
            ns2 <- get
            let ns2' = logAnswerUpdateCache ns2 p src valueId (show a, usedCache, cache')
            put ns2'
            return $
                maybeTrace ("makeQCached: a = " ++ show a)
                    a
    qaRegisterM _ =
      error "internal error in AERN2.QA.Strategy.Cached: qaRegister called with an existing id"

  qaFulfilPromiseA = Kleisli qaFulfilPromiseM
    where
    qaFulfilPromiseM promiseA =
      runKleisli promiseA ()
  qaMakeQueryGetPromiseA src = Kleisli qaMakeQueryGetPromiseM
    where
    qaMakeQueryGetPromiseM (qa, q) =
      runKleisli (qaMakeQueryGetPromise qa (me, src)) q
      where
      me = case qaId qa of Nothing -> src; me2 -> me2

executeQACachedA :: (QACachedA () a) -> (QANetLog, a)
executeQACachedA code =
  (net_log ns, result)
  where
  (result, ns) = (runState $ unQACachedM $ runKleisli code ()) (initQANetState True)

executeQAUncachedA :: (QACachedA () a) -> (QANetLog, a)
executeQAUncachedA code =
  (net_log ns, result)
  where
  (result, ns) = (runState $ unQACachedM $ runKleisli code ()) (initQANetState False)
