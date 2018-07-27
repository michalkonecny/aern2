module AERN2.Box where

import MixedTypesNumPrelude
import AERN2.Util.Vector (Vector, (!), (!.))
import qualified AERN2.Util.Vector as V
import AERN2.Interval
import AERN2.MP.Dyadic

type Box a = Vector (Interval a a)
type DyadicBox = Box Dyadic

dim :: Box a -> Integer
dim = V.vlength

box :: [(a,a)] -> Box a
box is =
  V.fromList [Interval a b | (a,b) <- is]

remove :: Integer -> Box a -> Box a
remove i b =
  V.generate
    (int $ dim b - 1)
    (\j -> if j < i then b ! j else b !. (j + 1))

unitCube :: (HasIntegers a) => Integer -> Box a
unitCube n =
  box [(convertExactly $ -1, convertExactly 1) | _i <- [1 .. n]]
