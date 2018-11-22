{-# LANGUAGE CPP #-}
{-|
    Module      :  AERN2.MP.Float.Operators
    Description :  Infix operators for up/down-rounded floating-point numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Infix operators for up/down-rounded floating-point numbers
-}

module AERN2.MP.Float.Operators where

#ifdef USE_CDAR

import AERN2.MP.Float.UseCDAR.Type
import AERN2.MP.Float.UseCDAR.Arithmetic

#else

import AERN2.MP.Float.UseRounded.Type
import AERN2.MP.Float.UseRounded.Arithmetic

#endif

infixl 6  +^, -^, +., -.
infixl 7  *^, *., /^, /.

(+^) :: MPFloat -> MPFloat -> MPFloat
(+^) = addUp
(-^) :: MPFloat -> MPFloat -> MPFloat
(-^) = subUp
(*^) :: MPFloat -> MPFloat -> MPFloat
(*^) = mulUp
(/^) :: MPFloat -> MPFloat -> MPFloat
(/^) = divUp

(+.) :: MPFloat -> MPFloat -> MPFloat
(+.) = addDown
(-.) :: MPFloat -> MPFloat -> MPFloat
(-.) = subDown
(*.) :: MPFloat -> MPFloat -> MPFloat
(*.) = mulDown
(/.) :: MPFloat -> MPFloat -> MPFloat
(/.) = divDown
