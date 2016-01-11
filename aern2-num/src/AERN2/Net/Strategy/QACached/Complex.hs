{-# LANGUAGE FlexibleInstances #-}
module AERN2.Net.Strategy.QACached.Complex where

import AERN2.Num

--import AERN2.Net.Strategy.QACached.Basics 
import AERN2.Net.Strategy.QACached.CauchyReal

type QACached_Complex = Complex QACached_CauchyReal

--instance RealA QACachedA (Complex QACached_CauchyReal)
--instance ComplexA QACachedA (Complex QACached_CauchyReal)
