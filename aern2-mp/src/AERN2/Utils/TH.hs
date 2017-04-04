{-# LANGUAGE TemplateHaskell #-}
{-|
    Module      :  AERN2.Utils.TH
    Description :  Template Haskell utilities
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Template Haskell utilities
-}

module AERN2.Utils.TH
(
  declForTypes
)
where

import Numeric.MixedTypes
-- import qualified Prelude as P

import Language.Haskell.TH

{- template haskell to generate repetitive instances -}

{-|

A toy example of use:

@
class HasTT t where
  type TT t
  getTT :: t -> TT t

$(declForTypes
  [[t| Integer |], [t| Int |], [t| Rational |]]
  (\ t -> [d|
    instance HasTT $t where
      type TT $t = ()
      getTT _ = ()
  |]))
@

-}
declForTypes :: [Q Type] -> (Q Type -> Q [Dec]) -> Q [Dec]
declForTypes types makeDecl =
  do
  decsList <- sequence $ map makeDecl types
  return $ concat decsList
