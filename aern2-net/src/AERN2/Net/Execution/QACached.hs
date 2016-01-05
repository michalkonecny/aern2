{-# LANGUAGE Arrows, StandaloneDeriving, ExistentialQuantification, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts #-}
module AERN2.Net.Execution.QACached 
(
    module AERN2.Net.Execution.QACached.Basics,
    module AERN2.Net.Execution.QACached.CauchyReal,
    module AERN2.Net.Execution.QACached
)
where

import AERN2.Num

import AERN2.Net.Execution.QACached.Basics
import AERN2.Net.Execution.QACached.CauchyReal 

import AERN2.Net.Spec.Arrow
import Control.Arrow (runKleisli)
import qualified Data.Map as Map

_anet0cachedCauchy :: Integer -> MPBall
_anet0cachedCauchy p =
    executeQACachedM $
        do
        (QACached_CauchyReal rId) <- runKleisli (_anet0 :: QACachedA () QACached_CauchyReal) ()
        a <- getAnswer QAP_CauchyReal rId (bits p)
        return a

_anet3cachedCauchy :: (Rational, Rational, Rational) -> Integer -> MPBall
_anet3cachedCauchy (x,y,z) p =
    executeQACachedM $
        do
        channels <- mapM mkInput $ [("x",x), ("y",y), ("z",z)]
        let envCh = Map.fromList channels
        (QACached_CauchyReal rId) <- runKleisli _anet3 envCh
        a <- getAnswer QAP_CauchyReal rId (bits p)
        return a
        where
        mkInput (name, value) =
            do
            ch <- constCRCachedM name value
            return (name, ch)

