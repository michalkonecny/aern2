module AERN2.Net.Execution.QACached.Complex where

import AERN2.Num
import Data.String (IsString(..),fromString)

import AERN2.Net.Execution.QACached.Basics 
import AERN2.Net.Execution.QACached.CauchyReal 

type QACached_Complex = (QACached_CauchyReal, QACached_CauchyReal)
