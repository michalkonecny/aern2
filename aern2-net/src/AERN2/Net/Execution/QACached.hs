{-# LANGUAGE Arrows, StandaloneDeriving, ExistentialQuantification, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts, TypeOperators #-}
module AERN2.Net.Execution.QACached 
(
    module AERN2.Net.Execution.QACached.Basics,
    module AERN2.Net.Execution.QACached.CauchyReal,
    module AERN2.Net.Execution.QACached.Complex,
    module AERN2.Net.Execution.QACached
)
where

import AERN2.Num

import AERN2.Net.Execution.QACached.Basics
import AERN2.Net.Execution.QACached.CauchyReal
import AERN2.Net.Execution.QACached.Complex

import AERN2.Net.Spec.Arrow
import Control.Arrow
import qualified Data.Map as Map

_anet0cachedCauchy :: Integer -> MPBall
_anet0cachedCauchy p =
    executeQACachedA $
        proc () ->
            do
            r <- _anet0  -< ()
            let (AsCauchyReal ur) = r :: QACached_CauchyReal
            getAnswerCRA -< (ur, bits p)

_anet3cachedCauchy :: (Rational, Rational, Rational) -> Integer -> MPBall
_anet3cachedCauchy (x,y,z) p =
    executeQACachedA $
        proc () ->
            do
            channels <- mapA mkInput -< [("x",x), ("y",y), ("z",z)]
            let envCh = Map.fromList channels
            r <- _anet3 -< envCh
            let (AsCauchyReal ur) = r :: QACached_CauchyReal
            getAnswerCRA -< (ur, bits p)
        where
        mkInput = proc (name, value) ->
            do
            ch <- newCRA -< (Just name, proc ac -> returnA -< (cauchyReal2ball (cauchyReal value) ac))
            returnA -< (name, AsCauchyReal ch)

