{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|
    Module      :  AERN2.QA.Strategy.CachedUnsafe
    Description :  QA net plain evaluation with unsafe IO caching
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    QA net plain evaluation with unsafe IO caching
-}
module AERN2.QA.Strategy.CachedUnsafe
(
  qaUnsafeCachingMV
)
where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#define maybeTraceIO putStrLn
#else
#define maybeTrace (\ (_ :: String) t -> t)
#define maybeTraceIO  (\ (_ :: String)-> return ())
#endif

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

import System.IO.Unsafe (unsafePerformIO)

import Control.Concurrent.MVar

import AERN2.QA.Protocol

{-|
  Normal Haskell functions are a trivial QAArrow instance
  where registration has no effect.
-}
instance QAArrow (->) where
  type QAId (->) = ()
  qaRegister _ = id
  newQA name sources p sampleQ makeQ =
    addUnsafeMemoisation $
      defaultNewQA name sources p sampleQ makeQ
  qaMakeQueryGetPromiseA src (qa,q) = qaMakeQueryGetPromise qa src q
  qaFulfilPromiseA promise = promise ()

{-| A global variable controlling whether unsafe caching is used in QA objects in the (->) arrow -}
qaUnsafeCachingMV :: MVar Bool
qaUnsafeCachingMV = unsafePerformIO (newMVar True)

{-|
  Add caching to pure (->) QA objects via unsafe memoization, inspired by
  https://hackage.haskell.org/package/ireal-0.2.3/docs/src/Data-Number-IReal-UnsafeMemo.html#unsafeMemo,
  which, in turn, is inspired by Lennart Augustsson's uglymemo.
-}
addUnsafeMemoisation :: (QAProtocolCacheable p) => QA (->) p -> QA (->) p
addUnsafeMemoisation qa = qa { qaMakeQueryGetPromise = \ _src -> unsafeMemo }
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
      shouldCache <- readMVar qaUnsafeCachingMV
      if not shouldCache then return $ qaMakeQueryGetPromise qa Nothing q ()
        else
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
              let a = qaMakeQueryGetPromise qa Nothing q ()
              modifyMVar_ cacheVar (const (return (updateQACache p q a cache)))
              -- putStrLn $ printf "memoIO  %s: updated cache: ? %s -> ! %s" name (show q) (show a)
              cache' <- readMVar cacheVar
              case lookupQACache p cache' q of
                (Just a', _) -> return a'
                -- this arranges that any size reductions specified in lookupQACache are applied even when the cache was not used
                _ -> return a
