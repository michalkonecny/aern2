module FnReps.Polynomial.UnaryCheb.Poly.Cheb2Power(
cheb2Power,
cheb2IntPower
) where

import qualified FnReps.Polynomial.UnaryPower.Poly.Basics as Power
import qualified FnReps.Polynomial.UnaryPower.IntPoly.Basics as IntPower
import qualified FnReps.Polynomial.UnaryCheb.Poly.Basics as Cheb
import qualified Data.Map as Map
import AERN2.Num
import Math.NumberTheory.Logarithms (integerLog2)

import Data.Ratio

instance Show Cheb.Poly where
    show = show . cheb2Power 


cheb2Power :: Cheb.Poly -> Power.Poly
cheb2Power (Cheb.Poly ts) = Power.Poly $ Map.filterWithKey (\k _ -> k <= Cheb.terms_degree ts) nts
                            where
                            (Power.Poly nts) = (aux 0 0) * (PolyVec (Power.fromIntegerListP (prec 53) [(0,1)]) (Power.fromIntegerListP (prec 53) [(1,1)])) 
                            d = integer (integerLog2 $ Cheb.terms_degree ts + 1)  + 1
                            dm1 = d - 1
                            aux j i = if j == dm1 then
                                        PolyCoVec (Power.fromList [(0,Cheb.terms_lookupCoeff ts $ 2*i)]) (Power.fromList [(0,Cheb.terms_lookupCoeff ts $ 2*i + 1)])
                                      else
                                        aux (j + 1) (2*i) + (aux (j + 1) (2*i + 1)) * recPowers !! (int $ dm1 - j)
                            
recPowers :: [PolyMat]
recPowers = mat : accu mat
            where
            accu m = let p = m*m in p : (accu $ p)
            mat = PolyMat (Power.fromIntegerListP (prec 53) [(0,0)]) (Power.fromIntegerListP (prec 53) [(0,1)]) (Power.fromIntegerListP (prec 53) [(0,-1)]) (Power.fromIntegerListP (prec 53) [(1,2)])

-- this assumes that all coefficients except the constant one are exact.
cheb2IntPower :: Cheb.Poly -> (IntPower.IntPoly, MPBall)
cheb2IntPower (Cheb.Poly ts) = (IntPower.IntPoly $ Map.filterWithKey (\k x -> k <= Cheb.terms_degree ts && x /= 0) nts, err)
                               where
                               (_, err) = getCentreAndErrorBall $ Cheb.terms_lookupCoeff ts 0
                               --IntPower.IntPoly tsInt = IntPower.fromFracList $ IntPower.normaliseFracList $ Map.toList $ Map.map (\x -> ballCentreRational x) ts
                               tsFrac = Map.map (ballCentreRational) ts
                               lcmd = Map.foldl' (\x y -> lcm x (denominator y)) 1 tsFrac
                               tsInt = Map.map (\x -> numerator $ lcmd*x) tsFrac
                               (IntPower.IntPoly nts) = (aux 0 0) * (IntPolyVec (IntPower.fromList [(0,1)]) (IntPower.fromList [(1,1)])) 
                               d = integer (integerLog2 $ Cheb.terms_degree ts + 1)  + 1
                               dm1 = d - 1
                               aux j i = if j == dm1 then
                                            IntPolyCoVec (IntPower.fromList [(0,IntPower.terms_lookupCoeff tsInt $ 2*i)]) (IntPower.fromList [(0,IntPower.terms_lookupCoeff tsInt $ 2*i + 1)])
                                         else
                                            aux (j + 1) (2*i) + (aux (j + 1) (2*i + 1)) * recPowersInt !! (int $ dm1 - j)

recPowersInt :: [IntPolyMat]
recPowersInt = mat : accu mat
            where
            accu m = let p = m*m in p : (accu $ p)
            mat = IntPolyMat (IntPower.fromList [(0,0)]) (IntPower.fromList [(0,1)]) (IntPower.fromList [(0,-1)]) (IntPower.fromList [(1,2)])


data PolyMat = PolyMat Power.Poly Power.Poly Power.Poly Power.Poly deriving Show
data IntPolyMat = IntPolyMat IntPower.IntPoly IntPower.IntPoly IntPower.IntPoly IntPower.IntPoly deriving Show

data PolyCoVec = PolyCoVec Power.Poly Power.Poly deriving Show
data IntPolyCoVec = IntPolyCoVec IntPower.IntPoly IntPower.IntPoly deriving Show

data PolyVec = PolyVec Power.Poly Power.Poly
data IntPolyVec = IntPolyVec IntPower.IntPoly IntPower.IntPoly

instance CanMulA (->) PolyMat PolyMat where
 type MulTypeA (->) PolyMat PolyMat = PolyMat
 mulA ((PolyMat a b c d), (PolyMat a' b' c' d')) = PolyMat (a*a' + b*c') (a*b' + b*d') (c*a' + d*c') (c*b' + d*d')
 
instance CanMulA (->) IntPolyMat IntPolyMat where
 type MulTypeA (->) IntPolyMat IntPolyMat = IntPolyMat
 mulA ((IntPolyMat a b c d), (IntPolyMat a' b' c' d')) = IntPolyMat (a*a' + b*c') (a*b' + b*d') (c*a' + d*c') (c*b' + d*d')

instance CanMulA (->) PolyCoVec PolyMat where
 type MulTypeA (->) PolyCoVec PolyMat = PolyCoVec
 mulA ((PolyCoVec x y), (PolyMat a b c d)) = PolyCoVec (x*a + y*c) (x*b + y*d)

instance CanMulA (->) IntPolyCoVec IntPolyMat where
 type MulTypeA (->) IntPolyCoVec IntPolyMat = IntPolyCoVec
 mulA ((IntPolyCoVec x y), (IntPolyMat a b c d)) = IntPolyCoVec (x*a + y*c) (x*b + y*d)

instance CanMulA (->) PolyCoVec PolyVec where
  type MulTypeA (->) PolyCoVec PolyVec = Power.Poly
  mulA ((PolyCoVec x y), (PolyVec a b)) = x*a + y*b
  
instance CanMulA (->) IntPolyCoVec IntPolyVec where
  type MulTypeA (->) IntPolyCoVec IntPolyVec = IntPower.IntPoly
  mulA ((IntPolyCoVec x y), (IntPolyVec a b)) = x*a + y*b
  
instance CanAddA (->) PolyCoVec PolyCoVec where
    type AddTypeA (->) PolyCoVec PolyCoVec = PolyCoVec
    addA (PolyCoVec a b, PolyCoVec c d) = PolyCoVec (a + c) (b + d)
    
instance CanAddA (->) IntPolyCoVec IntPolyCoVec where
    type AddTypeA (->) IntPolyCoVec IntPolyCoVec = IntPolyCoVec
    addA (IntPolyCoVec a b, IntPolyCoVec c d) = IntPolyCoVec (a + c) (b + d)    