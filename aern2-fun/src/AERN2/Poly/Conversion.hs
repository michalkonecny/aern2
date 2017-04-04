module AERN2.Poly.Conversion
where

import Numeric.MixedTypes
import AERN2.Poly.Basics
import AERN2.Poly.Power.Type
import AERN2.Poly.Power (PowPoly)
import qualified AERN2.Poly.Power as Power hiding (PowPoly)
--import qualified AERN2.Poly.Cheb.Type  as Cheb
import qualified Data.Map as Map
import Math.NumberTheory.Logarithms (integerLog2)
import AERN2.MP.Ball
import AERN2.MP.Dyadic

cheb2PowerExact :: Poly MPBall -> PowPoly MPBall
cheb2PowerExact f@(Poly ts) =
  updateRadius (+ r) $
  PowPoly $ Poly $ terms_map mpBall powTs
  where
  --p  = getPrecision f
  fc = Poly $ terms_map (centre) ts
  r  = Map.foldl' (+) (errorBound 0) $ Map.map radius ts
  PowPoly (Poly powTs) = cheb2Power fc

cheb2Power ::
  (CanMulSameType (PowPoly a), HasIntegers a, CanAddSameType a)
  => Poly a -> PowPoly a -- Poly encodes polynomial in Chebyshev basis on [-1,1]
cheb2Power (Poly ts :: Poly a) =
    PowPoly $ Poly $ Map.filterWithKey (\k _ -> k <= terms_degree ts) nts
    where
    (PowPoly (Poly nts)) =
      (aux 0 0)
        * (PolyVec (Power.fromIntegerList [(0,1)]) (Power.fromIntegerList [(1,1)] :: PowPoly a))
    d = integer (integerLog2 $ terms_degree ts + 1)  + 1
    dm1 = d - 1
    aux j i =
        if j == dm1 then
          PolyCoVec
            (Power.fromList [(0, terms_lookupCoeff ts $ 2*i)])
            (Power.fromList [(0, terms_lookupCoeff ts $ 2*i + 1)])
        else
          aux (j + 1) (2*i) + (aux (j + 1) (2*i + 1)) * (recPowers !! (dm1 - j) :: PolyMat a)

recPowers ::
  (CanMulSameType (PowPoly a), HasIntegers a, CanAddSameType a)
  => [(PolyMat a)] -- TODO: slow?
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

data PolyMat a =
  PolyMat (PowPoly a) (PowPoly a)
          (PowPoly a) (PowPoly a)

data PolyCoVec a = PolyCoVec (PowPoly a) (PowPoly a)

data PolyVec a = PolyVec (PowPoly a) (PowPoly a)

instance
    (CanMulSameType (PowPoly a), CanAddSameType a) =>
    CanMulAsymmetric (PolyMat a) (PolyMat a) where
    type MulType (PolyMat a) (PolyMat a) = (PolyMat a)
    mul (PolyMat a b c d) (PolyMat a' b' c' d') =
      PolyMat (a*a' + b*c') (a*b' + b*d') (c*a' + d*c') (c*b' + d*d')

instance
    (CanMulSameType (PowPoly a), CanAddSameType a) =>
    CanMulAsymmetric (PolyCoVec a) (PolyMat a) where
    type MulType (PolyCoVec a) (PolyMat a) = (PolyCoVec a)
    mul (PolyCoVec x y) (PolyMat a b c d) =
       PolyCoVec (x*a + y*c) (x*b + y*d)

instance
    (CanMulSameType (PowPoly a), CanAddSameType a) =>
    CanMulAsymmetric (PolyCoVec a) (PolyVec a) where
    type MulType (PolyCoVec a) (PolyVec a) = PowPoly a
    mul (PolyCoVec x y) (PolyVec a b) = x*a + y*b

instance
    (CanAddSameType a) =>
    CanAddAsymmetric (PolyCoVec a) (PolyCoVec a) where
    type AddType (PolyCoVec a) (PolyCoVec a) = (PolyCoVec a)
    add (PolyCoVec a b) (PolyCoVec c d) = PolyCoVec (a + c) (b + d)
