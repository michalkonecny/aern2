module FnReps.Polynomial.UnaryCheb.Poly.Newman
(
newmanAbs,
newmanSqrt,
_test1,
_test2,
_test3,
_test4,

--test:

newmanSansError,
_test5,
_test6
)
where

import AERN2.Num
import qualified FnReps.Polynomial.UnaryCheb.Poly as Cheb
import qualified FnReps.Polynomial.UnaryPower.Poly as Pow
import qualified FnReps.Polynomial.UnaryCheb.Poly.Basics as ChebB
import Data.List as List
import qualified Data.Map as Map
import qualified Prelude as Prelude


import FnReps.Polynomial.UnaryCheb.Poly.DCTMultiplication (lift2_DCT)

-- Newman's approximation to |x| on [-1,1] and sqrt(x) on [0,1].

-- xi_n^k = exp(k/sqrt(n))
xi :: Integer -> Integer -> CauchyReal
xi n k = exp $ - k/(sqrt(n))

psi :: Integer -> Pow.Poly
psi n = foldl' (*) (Pow.fromList [(1,mpBall 1), (0,mpBall 1)]) [Pow.fromList [(1,mpBall 1), (0, cauchyReal2ball (xi n k) $ bits $ 100*n + 53)]  | k <- [1 .. n - 1]]

psim :: Integer -> Pow.Poly
psim n = foldl' (*) (Pow.fromList [(1,mpBall $ -1), (0,mpBall 1)]) [Pow.fromList [(1,mpBall $ -1), (0, cauchyReal2ball (xi n k) $ bits $ 100*n + 53)]  | k <- [1 .. n - 1]]

newmanNumerator :: Integer -> Pow.Poly
newmanNumerator n = Pow.Poly $ Map.map (\x -> 2*x) $ Map.mapKeys (\k -> k + 1) $ Map.filterWithKey (\k _ -> odd k) ts
                     where
                     Pow.Poly ts = psi (n*n)

newmanDenominator :: Integer -> Pow.Poly
newmanDenominator n = Pow.Poly $ Map.map (\x -> 2*x) $ Map.filterWithKey (\k _ -> even k) ts
                     where
                     Pow.Poly ts = psi (n*n)
           
newmanAbs :: Integer -> Cheb.Poly
newmanAbs n = r
            where
            p@(Cheb.Poly ts)  = Pow.power2Cheb $ newmanNumerator (n + 1)
            q@(Cheb.Poly ts') = Pow.power2Cheb $ newmanDenominator (n + 1)
            d = ChebB.terms_degree ts + ChebB.terms_degree ts'
            Cheb.Poly rEts = lift2_DCT (const $ const $ d) (/) p q
            rClean@(Cheb.Poly rCts) = Cheb.Poly $ Map.map (\x -> ballCentre x) rEts
            r = Cheb.Poly $ Map.insertWith (\y x -> let Interval l r = x +- y in endpoints2Ball l r) 0 (err) $ rCts
            err = max a b
            x = ChebB.fromList [(0,mpBall 0),(1,mpBall 1)]
            Interval _ a = abs $ Cheb.range (bits 100) (rClean - x) $ Interval (mpBall $ 0) (mpBall 1)
            Interval _ b = abs $ Cheb.range (bits 100) (rClean + x) $ Interval (mpBall $ -1) (mpBall 0)

newmanSansError :: Integer -> Cheb.Poly 
newmanSansError n = r 
                    where
                    p@(Cheb.Poly ts)  = Pow.power2Cheb $ newmanNumerator (n + 1)
                    q@(Cheb.Poly ts') = Pow.power2Cheb $ newmanDenominator (n + 1)
                    d = ChebB.terms_degree ts + ChebB.terms_degree ts'
                    Cheb.Poly rEts = lift2_DCT (const $ const $ d) (/) p q
                    rClean@(Cheb.Poly rCts) = Cheb.Poly $ Map.map (\x -> ballCentre x) rEts
                    r = rClean
                    
newmanSqrt :: Integer -> Cheb.Poly
newmanSqrt n = Pow.power2Cheb sqrtApprox
               where
               Pow.Poly ts = Cheb.cheb2Power $ newmanAbs n
               sqrtApprox = Pow.Poly $ Map.mapKeys (\k -> k `Prelude.div` 2) $ Map.filterWithKey (\k _ -> even k) ts
               

_test1 n = (ChebB.terms_degree ts, err)
           where    
           fn@(Cheb.Poly ts) = newmanAbs n
           (_,err) = getCentreAndErrorBall (Cheb.evalDirectOnBall fn (mpBall 0))  
           
_test2 n = mapM print [(k, _test1 k) | k <- [1.. n]]      

_test3 n = (ChebB.terms_degree ts, err)
           where    
           fn@(Cheb.Poly ts) = newmanSqrt n
           (_,err) = getCentreAndErrorBall (Cheb.evalDirectOnBall fn (mpBall 0))  
           
_test4 n = mapM print [(k, _test3 k) | k <- [1.. n]]      
 
_test5 n = (ChebB.terms_degree ts, err)
           where    
           fn@(Cheb.Poly ts) = newmanSansError n
           (_,err) = getCentreAndErrorBall (Cheb.evalDirectOnBall fn (mpBall 0))  
           
_test6 n = mapM print [(k, _test5 k) | k <- [1.. n]]      
 
                       