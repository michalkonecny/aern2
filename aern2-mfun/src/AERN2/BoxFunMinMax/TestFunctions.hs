module AERN2.BoxFunMinMax.TestFunctions where

import MixedTypesNumPrelude
import AERN2.MP.Ball
import qualified Data.List as List

import AERN2.AD.Differential hiding (x)
-- import AERN2.Util.Util
import AERN2.BoxFun.Type
import AERN2.BoxFun.TestFunctions
import AERN2.BoxFunMinMax.Type
import AERN2.Linear.Vector.Type ((!), Vector)
import qualified AERN2.Linear.Vector.Type as V

import Data.Functor

treeAbs :: (Vector (Differential (CN MPBall)) -> Differential (CN MPBall)) -> (CN MPBall) -> Integer -> Vector (CN MPBall) -> MinMaxTree
treeAbs f x fDimension fDomain =
    Min
        (Max
            (Leaf (fun x1))
            (Leaf (fun f1)))
        (Max
            (Leaf (fun x2))
            (Leaf (fun f2)))
    where
        fun :: (Vector (Differential (CN MPBall)) -> Differential (CN MPBall)) -> BoxFun
        fun g = 
            BoxFun 
            fDimension 
            g
            fDomain

        x1 = negate <$> f
        f1 = f <&> (+x)
        x2 = f
        f2 = (negate <$> f) <&> (+x)

heronTreeL :: MinMaxTree
heronTreeL =
    treeAbs     
    (\v ->
        let
            x = v!0
            y = v!1
        in
            sqrt x - y

    ) 
    (- (mpBallP (prec 1000) 1/2)^(2^(i-1)) - mpBallP (prec 1000) 6 * eps * (i-1)) 
    2 
    (fromListDomain [(0.5, 2.0), (0.8, 1.8)])
    where
        eps = 1/2^23
        i = 2

heronTreeR :: MinMaxTree
heronTreeR =
    treeAbs     
    (\v ->
        let
            x = v!0
            y = v!1
        in
            sqrt x - (y+x/y)/2

    ) 
    ((mpBallP (prec 1000) 1/2)^(2^i) + mpBallP (prec 1000) 6 * eps * (i-1)) 
    2 
    (fromListDomain [(0.5, 2.0), (0.8, 1.8)])
    where
        eps = 1/2^23
        i = 2

heronTree :: MinMaxTree
heronTree =
    Max
        heronTreeL
        heronTreeR
