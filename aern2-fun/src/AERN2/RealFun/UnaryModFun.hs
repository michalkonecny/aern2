{-# LANGUAGE CPP #-}
#define DEBUG
{-|
    Module      :  AERN2.RealFun.UnaryModFun
    Description :  Real functions by evaluator on dyadic points and a modulus of continuity
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Real functions by evaluator on dyadic points and a modulus of continuity
-}

module AERN2.RealFun.UnaryModFun
-- (
--   UnaryModFun(..), unaryModFun,
--   inverseNonDecreasingFnMaxBelow
-- )
where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#else
#define maybeTrace (\ (_ :: String) t -> t)
#endif

import Numeric.MixedTypes
-- import qualified Prelude as P
-- import Text.Printf

-- import Control.Applicative

import Numeric.CatchingExceptions

import AERN2.MP
-- import qualified AERN2.MP.Ball as MPBall
import AERN2.MP.Dyadic

import AERN2.Real
import AERN2.Interval

import AERN2.RealFun.Operations

import AERN2.RealFun.UnaryBallFun

{-|

  Here we represent a continuous function \(f\) using its evaluation function on dyadics
  and a "local" modulus of continuity \(\omega\) in the following sense:
  .
  For any dyadic \(d_1,d_2\in b\) with \(|d_1-d_2|\leq 2^{ -\omega(b)(i)}\) we have
  \(|f(d_1)-f(d_2)| \leq 2^{ -i }\).

  For any \(b\), the modulus of continuity has to be a non-decreasing function:
  \[ i\leq j \implies \omega(b)(i)\leq\omega(b)(j) \]
  and \(omega(b)(i)\) has to converge to \(\infty\) with increasing \(i\).
-}
data UnaryModFun =
  UnaryModFun
  {
    _modfun_domain :: DyadicInterval
  , _modfun_eval :: CatchingNumExceptions Dyadic -> CatchingNumExceptions CauchyReal {-^ \\(d \mapsto f(d)\\) -}
  , _modfun_modulus :: MPBall -> Integer -> Integer {-^ \\(\omega\\) -}
  }

instance HasDomain UnaryModFun where
  type Domain UnaryModFun = DyadicInterval
  getDomain f = _modfun_domain f

instance ConvertibleExactly (DyadicInterval, Integer) UnaryModFun where
  safeConvertExactly (dom,n) =
    Right $ UnaryModFun dom  (const $ pure (real n)) (constFnModulus)

constFnModulus :: MPBall -> Integer -> Integer
constFnModulus _b i = i

unaryModFun :: (ConvertibleExactly t UnaryModFun) => t -> UnaryModFun
unaryModFun = convertExactly

instance HasVars UnaryModFun where
  type Var UnaryModFun = ()
  varFn sampleFn () =
    UnaryModFun dom (fmap real) (const id)
    where
    dom = getDomain sampleFn

instance ConvertibleExactly UnaryModFun UnaryBallFun where
  safeConvertExactly = Right . modFun2BallFun

modFun2BallFun :: UnaryModFun -> UnaryBallFun
modFun2BallFun (UnaryModFun dom eval modulus) =
    UnaryBallFun dom (b2bE =<<)
    where
    b2bE b =
      maybeTrace ("UnaryModFun b2bE: "
        ++ "b = "  ++ show b
        ++ ", domAC = "  ++ show domAC
        ++ ", rangeAC = "  ++ show rangeAC
        ++ ", tolerance = "  ++ show tolerance
        ++ ", fbCE = "  ++ show fbCE
      ) $
      do
      fbC <- fbCE
      pure $ updateRadius (+ tolerance) $ qaMakeQuery fbC (bits $ rangeAC + 2)
      where
      fbCE = eval (catchingNumExceptions (centre b))

      domAC = fromAccuracy $ getFiniteAccuracy b
      rangeAC = inverseNonDecreasingFnMaxBelow (modulus b) domAC
      tolerance = errorBound $ 0.5^(rangeAC)

{-|
  For a monotone integer function \(f\), and an integer \(n\) which is neither
  below or above the range of \(f\), return the largest \(i\) such that
  \(f(i) \leq n\).
-}
inverseNonDecreasingFnMaxBelow ::
  (Integer -> Integer)  {-^ \(f\) -} ->
  (Integer -> Integer)
inverseNonDecreasingFnMaxBelow f n
  | fn <= n = searchMax $ boundsUp (n, fn)
  | otherwise = searchMax $ boundsDown (n, fn)
  where
  fn = f n
  boundsUp (i, fi)
    | n < fii = ((i,fi), (ii, fii))
    | otherwise = boundsUp (ii, fii)
    where
    ii = increase i
    fii = f ii
  boundsDown (ii, fii)
    | fi <= n = ((i,fi), (ii, fii))
    | otherwise = boundsDown (i, fi)
    where
    i = decrease ii
    fi = f i
  decrease = negate . increase . negate
  increase i
    | i == 0 = 1
    | i < 0 = -((-i) `div` 2)
    | otherwise = i * 2
  searchMax ((i,fi),(j,fj))
    | m == i || m == j = i
    | fm <= n = searchMax ((m,fm), (j,fj))
    | otherwise = searchMax ((i,fi), (m,fm))
    where
    m = (i + j) `div` 2
    fm = f m


_modFun2BallFun_split :: UnaryModFun -> UnaryBallFun
_modFun2BallFun_split (UnaryModFun dom eval modulus) =
    UnaryBallFun dom (b2bE =<<)
    where
    b2bE b =
      maybeTrace ("UnaryModFun b2bE: "
        ++ "b = "  ++ show b
        ++ ", ac = "  ++ show ac
        ++ ", distEval = "  ++ show distEval
        ++ ", n = "  ++ show n
      ) $
      do -- using the CatchingNumExceptions monad
      minVal <- minValE
      maxVal <- maxValE
      pure $ withExtremesOnPoints minVal maxVal
      where
      ac = getFiniteAccuracy b
      distEval = 0.5^(modulus b (fromAccuracy ac))
      l,r :: MPBall
      (l,r) = endpoints b
      (_, n) = integerBounds $ (r-l)/(2*distEval)
        -- we need to split b into at least n segments and evaluate on each
        -- segment to get a reliable estimate of the range of f on b
        -- using the modulus of continuity
      pts -- points to evaluate f on
        | ptsOK pts1 = pts1
        | otherwise = pts2
        where
        pts1 = ptsWithN n
        pts2 = ptsWithN (n + 1)
        ptsOK [] = False
        ptsOK ps =
          and $
            [(head ps) - l !<=! distEval, r - (last ps) !<=! distEval]
            ++
            zipWith (\p p' -> p' - p !<=! 2*distEval) ps (tail ps)
        ptsWithN n2 =
          map centre [ (l*(i-0.5) + r*(n2-(i-0.5))) / n2 | i <- [1..n2]]
          -- centres of n2 equal-sized segments of b=[l,r]
      values = sequence $ map (eval . catchingNumExceptions) pts
      maxValE = fmap maximum values
      minValE = fmap minimum values
      withExtremesOnPoints :: CauchyReal -> CauchyReal -> MPBall
      withExtremesOnPoints minVal maxVal =
        updateRadius (+ (errorBound $ 0.5^(fromAccuracy ac))) rangeOnPoints
        where
        rangeOnPoints = fromEndpoints (qaMakeQuery minVal ac) (qaMakeQuery maxVal ac)

instance CanApply UnaryModFun MPBall where
  type ApplyType UnaryModFun MPBall = MPBall
  apply = apply . unaryBallFun

instance CanApply UnaryModFun DyadicInterval where
  type ApplyType UnaryModFun DyadicInterval = RealInterval
  apply = apply . unaryBallFun

instance CanMaximiseOverDom UnaryModFun DyadicInterval where
  type MaximumOverDomType UnaryModFun DyadicInterval = CauchyReal
  maximumOverDom = maximumOverDom . unaryBallFun

instance CanMinimiseOverDom UnaryModFun DyadicInterval where
  type MinimumOverDomType UnaryModFun DyadicInterval = CauchyReal
  minimumOverDom = minimumOverDom . unaryBallFun

instance CanIntegrateOverDom UnaryModFun DyadicInterval where
  type IntegralOverDomType UnaryModFun DyadicInterval = CauchyReal
  integrateOverDom = integrateOverDom . unaryBallFun

{- selected field ops -}

instance CanAddAsymmetric UnaryModFun UnaryModFun where
  add (UnaryModFun dom1 eval1 modulus1) (UnaryModFun dom2 eval2 modulus2)
    | dom1 == dom2 =
      UnaryModFun dom1 (\d -> eval1 d + eval2 d) modulus'
    | otherwise =
      error "UnaryModFun: add: incompatible domains"
    where
    modulus' b i = max (modulus1 b (i+1)) (modulus2 b (i+1))

instance CanAddAsymmetric Integer UnaryModFun where
  type AddType Integer UnaryModFun = UnaryModFun
  add n (UnaryModFun dom eval modulus) =
    UnaryModFun dom (\d -> n + (eval d)) modulus

instance CanAddAsymmetric UnaryModFun Integer where
  type AddType UnaryModFun Integer = UnaryModFun
  add f n = add n f

instance CanMulAsymmetric UnaryModFun UnaryModFun where
  mul f1@(UnaryModFun dom1 eval1 modulus1) f2@(UnaryModFun dom2 eval2 modulus2)
    | dom1 == dom2 =
      UnaryModFun dom1 (\d -> eval1 d * eval2 d) modulus'
    | otherwise =
      error "UnaryModFun: mul: incompatible domains"
    where
    modulus' b i =
      {-
        (x1+e1)*(x2+e2) = x1*x2 + (x1*e2+x2*e1+e1*e2)

        We need i1 and i2 such that whenever |e1|<=0.5^i1 and |e2|<=0.5^i2
        then (x1*e2+x2*e1+e1*e2) <= 0.5^i.

        First we strengthen this to:
        |x1*e2| <= 0.5^(i+2)
        |x2*e1| <= 0.5^(i+2)
        |e1*e2| <= 0.5^(i+2)

        Then we translate & strengthen this to:
        |e2| <= 0.5^(i+(lognorm x1)+2)
        |e1| <= 0.5^(i+(lognorm x2)+2)
        |e1| <= 0.5^((i+2)/2)
        |e2| <= 0.5^((i+2)/2)

        which is equivalent to:
        |e1| <= 0.5^(max (i+(lognorm x2)+2) ((i+2)/2))
        |e2| <= 0.5^(max (i+(lognorm x1)+2) ((i+2)/2))

      -}
      max m1 m2
      where
      i22 = (i+2) `div` 2
      x1 = apply (unaryBallFun f1) b
      x2 = apply (unaryBallFun f2) b
      m1 =
        case getNormLog x2 of
          NormZero -> modulus1 b i22
          NormBits nx2 -> modulus1 b (max (i + nx2 + 2) i22)
      m2 =
        case getNormLog x1 of
          NormZero -> modulus2 b i22
          NormBits nx1 -> modulus2 b (max (i + nx1 + 2) i22)

instance CanMulAsymmetric Integer UnaryModFun where
  type MulType Integer UnaryModFun = UnaryModFun
  mul n (UnaryModFun dom eval modulus) =
    UnaryModFun dom (\d -> n * (eval d)) modulus'
    where
    modulus' b =
      case (getNormLog (abs n)) of
        NormZero -> constFnModulus b
        NormBits nn -> \ i -> modulus b (i + nn)

instance CanMulAsymmetric UnaryModFun Integer where
  type MulType UnaryModFun Integer = UnaryModFun
  mul f n = mul n f

instance CanDiv UnaryModFun UnaryModFun where
  divide f1 f2 = f1 * recipF f2
    where
    recipF f@(UnaryModFun dom eval modulus) =
      UnaryModFun dom eval' modulus'
      where
      eval' d =
        do
        fd <- eval d
        pure $ 1 / fd -- cannot detect div by 0 for CauchyReals...
      modulus' b0 i =
        {-
          To compute the modulus, we need to know the size of (f b)
          and (f b) has to be zero-free.

          When x = f b contains 0 due to dependency errors, we split b and compute
          the modulus as the maximum of moduli over a partition of b.
        -}
        splitting maxDepth b0
        where
        maxDepth = 20
        splitting md b
          | md == 0 =
              error "UnaryModFun: division modulus: division by zero"
          | abs fb !>! 0 =
              modulus b $ j fb
          | otherwise =
              max (splitting (md-1) b1) (splitting (md-1) b2)
          where
          fb = apply f b
          (b1I,b2I) = split $ dyadicInterval b
          b1 = mpBall b1I
          b2 = mpBall b2I
        j x =
          {-
            Assume |x| > 0.

            We study the propagation of error during division:

            1/(x+e) - 1/x = -e/(x*(x+e))

            We need j such that whenever |e|<=0.5^j
            then |e/(x*(x+e))| <= 0.5^i and x+e /= 0.

            Assuming x /= 0, this is equivalent to:
            |e| < |x|
            AND
            |e| <= (0.5^i)*|x(x+e)|

            The latter is equivalent to:

            |e| <= (0.5^i)*(x^2+xe)

            thanks to |e| < |x|.

            We stengthen it to:

            |e| <= (0.5^i)*(x^2-x|e|)

            which is equivalent to:

            |e| <= ((0.5^i)*x^2)/(0.5^i|x|+1)

            which we strengthen to:

            |e| <= 0.5^(i-2*((lognorm x) - 1) - max(0,(i-(lognorm x))))

            using the inequality x>0 => log(1+x) >= max(0,log(x)).

            We have proved that
            j = max (1-(lognorm x)) (i-2*((lognorm x) - 1) - max(0,(i-(lognorm x))))
            has the desired property.
          -}
          case getNormLog x of
            NormZero -> error "UnaryModFun: division modulus: internal error"
            NormBits nx ->
              max (1-nx) -- |e| < |x|
                (i-2*(nx - 1) - (max 0 (i-nx)))


instance CanDiv Integer UnaryModFun where
  type DivType Integer UnaryModFun = UnaryModFun
  divide n f = divide (unaryModFun (getDomain f, n)) f

instance CanSinCos UnaryModFun where
  sin (UnaryModFun dom eval modulus) = UnaryModFun dom (sin . eval) modulus
  cos (UnaryModFun dom eval modulus) = UnaryModFun dom (cos . eval) modulus

example_ModFun :: UnaryModFun
example_ModFun =
  x*x
  -- sin(10*x)+cos(20*x)
  -- 1/((10*x*x)+1)
  -- 1/(x+2)
  where
  x = x_modfun

x_modfun :: UnaryModFun
x_modfun = varFn (unaryModFun (unaryIntervalDom, 0)) ()

mtest1 :: Integer -> MPBall
mtest1 b =
  qaMakeQuery m (bits b)
  where
  m =
    -- integrateOverDom example_ModFun $
    -- minimumOverDom example_ModFun $
    maximumOverDom example_ModFun $
      dyadicInterval (-1.0,1.0)

unaryIntervalDom :: DyadicInterval
unaryIntervalDom = dyadicInterval (-1,1)
