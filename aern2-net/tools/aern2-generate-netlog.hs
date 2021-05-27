{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  Main (file aern2-real-benchOp)
    Description :  Generate Elm bindings for NetLog JSON
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Generate Elm bindings for NetLog JSON
-}
module Main where

import MixedTypesNumPrelude
-- import Prelude

-- import Text.Printf

import Elm.Derive
import Elm.Module

import Data.Proxy

import AERN2.QA.NetLog

deriveElmDef defaultOptions ''ValueId
deriveElmDef defaultOptions ''QANetLogItem

main :: IO ()
main =
    do
    putStr $ makeElmModule "QANetLog"
              [ DefineElm (Proxy :: Proxy QANetLogItem),
                DefineElm (Proxy :: Proxy ValueId)
              ]
