module FnReps.Polynomial.UnaryPowerDense
(
module FnReps.Polynomial.UnaryPowerDense.Basics,
module FnReps.Polynomial.UnaryPowerDense.EvaluationRootFinding,
truncatedSin,
truncatedCos,
sinCos
)
where

import FnReps.Polynomial.UnaryPowerDense.Basics
import FnReps.Polynomial.UnaryPowerDense.EvaluationRootFinding
import qualified Data.List as List

truncatedSin :: Integer -> UnaryPowerDense
truncatedSin n = fromList [ (2*k + 1, rational2BallP (prec 530) $ (-1)^k/(fac (2*k + 1)))  | k <- [0..n] ]  
                 where
                 fac k = List.foldl' (*) 1 [1..k] 
                 
truncatedCos :: Integer -> UnaryPowerDense
truncatedCos n = derivative $ truncatedSin n

sinCos :: Integer -> MPBall -> MPBall -> UnaryPowerDense
sinCos n a b = a*(truncatedSin n) + b*(truncatedCos n)    



    

