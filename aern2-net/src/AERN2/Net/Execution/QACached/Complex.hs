{-# LANGUAGE FlexibleInstances #-}
module AERN2.Net.Execution.QACached.Complex where

import AERN2.Num

import AERN2.Net.Spec.Arrow
import AERN2.Net.Execution.QACached.Basics 
import AERN2.Net.Execution.QACached.CauchyReal

type QACached_Complex = Complex QACached_CauchyReal

instance RealA QACachedA (Complex QACached_CauchyReal)
instance RealExprA QACachedA (Complex QACached_CauchyReal)
instance RealPredA QACachedA (Complex QACached_CauchyReal)
instance ComplexA QACachedA (Complex QACached_CauchyReal)
