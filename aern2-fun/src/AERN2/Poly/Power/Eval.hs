module AERN2.Poly.Power.Eval
where

import MixedTypesNumPrelude
import AERN2.Poly.Power.Type
import AERN2.Poly.Basics
import AERN2.MP.Ball
import AERN2.MP.Dyadic
import qualified Data.Map as Map

evalDirect :: (CanAddAsymmetric b c, b ~ AddType b c ,
               Ring b, HasIntegers b, HasIntegers c)
               => PowPoly c -> b -> b
evalDirect (PowPoly (Poly ts)) (x :: b) =
  evalHornerAcc (terms_degree ts) (convertExactly 0)
  where
  evalHornerAcc :: Integer -> b -> b
  evalHornerAcc 0 sm = x*sm + terms_lookupCoeff ts 0
  evalHornerAcc k sm = evalHornerAcc (k - 1) $ x*sm + terms_lookupCoeff ts k

evalMBI :: PowPoly MPBall -> MPBall -> MPBall
evalMBI f =
  evalLip f (markovBoundI f)

evalDI :: PowPoly MPBall -> MPBall -> MPBall
evalDI f =
  evalDf f (derivative f)

evalDf :: PowPoly MPBall -> PowPoly MPBall -> MPBall -> MPBall
evalDf f f' x =
  evalLip f (abs $ evalDirect f' x) x

evalDIn :: PowPoly MPBall -> MPBall -> Integer -> MPBall
evalDIn f x n =
  if n == 0 then
    evalDirect f x
  else
    evalLip f (abs $ evalDIn (derivative f) x (n - 1)) x

evalLip :: PowPoly MPBall -> MPBall -> MPBall -> MPBall
evalLip f lip x =
  (evalDirect f $ centreAsBall x) + (hullMPBall (-err) err)
  where
  err = lip*(dyadic $ ball_error x)*0.5

markovBoundI :: PowPoly MPBall -> MPBall
markovBoundI f@(PowPoly (Poly ts)) =
  ((degree f)^!2) * Map.foldl' (\s y -> s + abs y) (mpBall 0) ts
