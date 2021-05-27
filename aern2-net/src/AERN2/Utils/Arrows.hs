{-|
    Module      :  AERN2.Utils.Arrows
    Description :  Miscellaneous arrow-generic functions
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Miscellaneous arrow-generic functions
-}
module AERN2.Utils.Arrows
(
  mapA, mapWithIndexA
  , CanSwitchArrow(..)
)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Text.Printf

import Control.Arrow

{- Arrow swiching mechanism and application to QA arrow conversion -}

class CanSwitchArrow to1 to2 where
  switchArrow :: (a `to1` b) -> (a `to2` b)
  -- switchArrow2 :: (a `to1` (b `to1` c)) -> (a `to2` (b `to2` c))

instance (Arrow to) => CanSwitchArrow (->) to where
  switchArrow = arr
  -- switchArrow2 = arr . (arr .)

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

{-| Apply an arrow morphism on all elements of a list -}
mapWithIndexA :: (ArrowChoice to) => (Integer -> t1 `to` t2) -> ([t1] `to` [t2])
mapWithIndexA fA = aux 0
  where
  aux i =
    proc list -> do
      case list of
        [] -> returnA -< []
        (x : xs) -> do
          y <- fA i -< x
          ys <- aux (i+1) -< xs
          returnA -< y : ys
