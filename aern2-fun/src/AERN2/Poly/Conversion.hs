module AERN2.Poly.Conversion
where

import AERN2.Poly.Basics
import qualified AERN2.Poly.Power.Type as Power
--import qualified AERN2.Poly.Cheb.Type  as Cheb
import Numeric.MixedTypes
import qualified Data.Map as Map
import Math.NumberTheory.Logarithms (integerLog2)
import AERN2.MP.Ball

cheb2Power :: Poly MPBall -> Power.PowPoly MPBall -- Poly encodes polynomial in Chebyshev basis on [-1,1]
cheb2Power (Poly ts) =
    Power.PowPoly $ Poly $ Map.filterWithKey (\k _ -> k <= terms_degree ts) nts
    where
    (Power.PowPoly (Poly nts)) =
      (aux 0 0)
        * (PolyVec (Power.fromIntegerList [(0,1)]) (Power.fromIntegerList [(1,1)] :: Power.PowPoly MPBall))
    d = integer (integerLog2 $ terms_degree ts + 1)  + 1
    dm1 = d - 1
    aux j i =
        if j == dm1 then
          PolyCoVec
            (Power.fromList [(0, terms_lookupCoeff ts $ 2*i)])
            (Power.fromList [(0, terms_lookupCoeff ts $ 2*i + 1)])
        else
          aux (j + 1) (2*i) + (aux (j + 1) (2*i + 1)) * (recPowers !! (dm1 - j))

recPowers :: [PolyMat] -- TODO: slow?
recPowers =
    mat : accu mat
    where
    accu m = let m2 = m*m in m2 : accu m2
    mat =
        PolyMat
            (Power.fromIntegerList [(0,0)])
            (Power.fromIntegerList [(0,1)])
            (Power.fromIntegerList [(0,-1)])
            (Power.fromIntegerList [(1,2)])

data PolyMat =
  PolyMat (Power.PowPoly MPBall) (Power.PowPoly MPBall)
          (Power.PowPoly MPBall) (Power.PowPoly MPBall)

data PolyCoVec = PolyCoVec (Power.PowPoly MPBall) (Power.PowPoly MPBall)

data PolyVec = PolyVec (Power.PowPoly MPBall) (Power.PowPoly MPBall)

instance CanMulAsymmetric PolyMat PolyMat where
    type MulType PolyMat PolyMat = PolyMat
    mul (PolyMat a b c d) (PolyMat a' b' c' d') =
      PolyMat (a*a' + b*c') (a*b' + b*d') (c*a' + d*c') (c*b' + d*d')

instance CanMulAsymmetric PolyCoVec PolyMat where
    type MulType PolyCoVec PolyMat = PolyCoVec
    mul (PolyCoVec x y) (PolyMat a b c d) =
       PolyCoVec (x*a + y*c) (x*b + y*d)

instance CanMulAsymmetric PolyCoVec PolyVec where
    type MulType PolyCoVec PolyVec = Power.PowPoly MPBall
    mul (PolyCoVec x y) (PolyVec a b) = x*a + y*b

instance CanAddAsymmetric PolyCoVec PolyCoVec where
    type AddType PolyCoVec PolyCoVec = PolyCoVec
    add (PolyCoVec a b) (PolyCoVec c d) = PolyCoVec (a + c) (b + d)
