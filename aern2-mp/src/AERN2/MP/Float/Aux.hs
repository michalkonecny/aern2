{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-|
    Module      :  AERN2.MP.Float.Aux
    Description :  Auxiliary structures
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Auxiliary structures for bounds on result and printing.
-}
module AERN2.MP.Float.Aux
(
    BoundsCEDU(..)
    , ceduDownUp
    , ceduCentreErr
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

ceduDownUp :: BoundsCEDU a -> (a,a)
ceduDownUp cedu = (ceduDown cedu, ceduUp cedu)

ceduCentreErr :: BoundsCEDU a -> (a,a)
ceduCentreErr cedu = (ceduCentre cedu, ceduErr cedu)
