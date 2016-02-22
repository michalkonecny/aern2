module FnReps.Polynomial.UnaryPower.Poly
(
module FnReps.Polynomial.UnaryPower.Poly.Basics,
module FnReps.Polynomial.UnaryPower.Poly.EvaluationRootFinding,
truncatedSin,
truncatedCos,
sinCos
)
where

import FnReps.Polynomial.UnaryPower.Poly.Basics
import FnReps.Polynomial.UnaryPower.Poly.EvaluationRootFinding
import qualified Data.List as List

truncatedSin :: Integer -> Poly
truncatedSin n = fromList [ (2*k + 1, rational2BallP (prec 530) $ (-1)^k/(fac (2*k + 1)))  | k <- [0..n] ]  
                 where
                 fac k = List.foldl' (*) 1 [1..k] 
                 
truncatedCos :: Integer -> Poly
truncatedCos n = derivative $ truncatedSin n

sinCos :: Integer -> MPBall -> MPBall -> Poly
sinCos n a b = a*(truncatedSin n) + b*(truncatedCos n)    



    

