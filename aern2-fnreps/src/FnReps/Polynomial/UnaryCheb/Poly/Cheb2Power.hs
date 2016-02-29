module FnReps.Polynomial.UnaryCheb.Poly.Cheb2Power(
cheb2Power
) where

import qualified FnReps.Polynomial.UnaryPower.Poly.Basics as Power
import qualified FnReps.Polynomial.UnaryCheb.Poly.Basics as Cheb
import AERN2.Num
import Math.NumberTheory.Logarithms (integerLog2)

instance Show Cheb.Poly where
    show = show . cheb2Power 


cheb2Power :: Cheb.Poly -> Power.Poly
cheb2Power (Cheb.Poly ts) = (aux 0 0) * (PolyVec (Power.fromIntegerListP (prec 53) [(0,1)]) (Power.fromIntegerListP (prec 53) [(1,1)]))
                            where
                            d = integer (integerLog2 $ Cheb.terms_degree ts) + 1
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

data PolyMat = PolyMat Power.Poly Power.Poly Power.Poly Power.Poly deriving Show

data PolyCoVec = PolyCoVec Power.Poly Power.Poly deriving Show

data PolyVec = PolyVec Power.Poly Power.Poly

instance CanMulA (->) PolyMat PolyMat where
 type MulTypeA (->) PolyMat PolyMat = PolyMat
 mulA ((PolyMat a b c d), (PolyMat a' b' c' d')) = PolyMat (a*a' + b*c') (a*b' + b*d') (c*a' + d*c') (c*b' + d*d')

instance CanMulA (->) PolyCoVec PolyMat where
 type MulTypeA (->) PolyCoVec PolyMat = PolyCoVec
 mulA ((PolyCoVec x y), (PolyMat a b c d)) = PolyCoVec (x*a + y*c) (x*b + y*d)

instance CanMulA (->) PolyCoVec PolyVec where
  type MulTypeA (->) PolyCoVec PolyVec = Power.Poly
  mulA ((PolyCoVec x y), (PolyVec a b)) = x*a + y*b
  
instance CanAddA (->) PolyCoVec PolyCoVec where
    type AddTypeA (->) PolyCoVec PolyCoVec = PolyCoVec
    addA (PolyCoVec a b, PolyCoVec c d) = PolyCoVec (a + c) (b + d)