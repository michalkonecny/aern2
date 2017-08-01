{-|
    Module      :  AERN2.MP.UseMPFR.Float.Operators
    Description :  Infix operators for up/down-rounded floating-point numbers
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Infix operators for up/down-rounded floating-point numbers
-}

module AERN2.MP.UseMPFR.Float.Operators where

import AERN2.MP.UseMPFR.Float.Type
import AERN2.MP.UseMPFR.Float.Arithmetic

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
