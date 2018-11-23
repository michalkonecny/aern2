{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-|
    Module      :  AERN2.MP.Float.BoundsCEDU
    Description :  Auxiliary structure for bounds on result
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Auxiliary structure for bounds on result
-}
module AERN2.MP.Float.BoundsCEDU
(
    BoundsCEDU(..)
)
where

data BoundsCEDU a =
  BoundsCEDU 
  {
    ceduCentre :: a
  , ceduErr :: a
  , ceduDown :: a
  , ceduUp :: a
  }