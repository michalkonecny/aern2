{-# LANGUAGE CPP #-}
-- #define DEBUG
{-|
    Module      :  AERN2.Poly.Cheb.Tests
    Description :  Tests for Chebyshev-basis polynomials
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Tests for Chebyshev-basis polynomials

    To run the tests using stack, execute:

    @
    stack test aern2-fun --test-arguments "-a 100 -m ChPoly"
    @
-}

module AERN2.PPoly.Tests
  -- (
  --   specChPoly, tPPoly
  -- , ChPolyConstruction(..)
  -- , chPolyFromOps
  -- , chPolyFromOpsWithDeg
  -- , arbitraryWithMinOpsDom
  -- , arbitraryWithDegDom
  -- , makeFnPositive
  -- , makeFnSmallRange
  -- , makeFnPositiveSmallRange
  -- )
where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#else
#define maybeTrace (\ (_ :: String) t -> t)
#endif

import MixedTypesNumPrelude
-- import qualified Prelude as P
-- import Data.Ratio
import Text.Printf

-- import qualified Data.Set as Set

import Test.Hspec
import Test.QuickCheck
-- import qualified Test.Hspec.SmallCheck as SC

--

import AERN2.MP
import AERN2.MP.Dyadic
import AERN2.MP.Ball.Tests

import AERN2.Interval

import AERN2.RealFun.Operations
import AERN2.RealFun.Tests

-- import AERN2.RealFun.SineCosine (sineWithAccuracyGuide)

-- import AERN2.Poly.Basics

import qualified AERN2.Poly.Cheb as ChPoly

import AERN2.PPoly.Type
import AERN2.PPoly.Eval ()
import AERN2.PPoly.Division
import AERN2.PPoly.Maximum (minimumOptimisedWithAccuracy, maximumOptimisedWithAccuracy)

data PPolyConstruction =
  PPolyConstruction
  { ppConstr_acGuide :: Accuracy
  , ppConstr_dom :: DyadicInterval
  , ppConstr_i0 :: FnIndex
  , ppConstr_opIndices :: [(OpIndex, [FnIndex])]
  }
  deriving (Show)


pPolyFromOps :: PPolyConstruction -> PPoly
pPolyFromOps (PPolyConstruction acGuide dom i0 opIndices) =
  applyOps opIndices (centreAsBall $ fns !! i0)
  where
  fns = map snd $ basicFunctions (dom, acGuide)
  applyOps [] fn = fn
  applyOps ((opIndex, operandIndices):rest) fn =
    applyOps rest newFn
    where
    (_arity, opList) = operations !! opIndex
    operands = map (fns !!) operandIndices
    newFn = centreAsBall $ liftCheb2PPoly (reduceSizeUsingAccuracyGuide acGuide) $ opList (fn : operands)

pPolyFromOpsWithDeg :: Integer -> PPolyConstruction -> (PPoly, PPolyConstruction)
pPolyFromOpsWithDeg deg (PPolyConstruction acGuide dom i0 opIndices) =
  applyOps [] opIndices (centreAsBall $ fns !! i0)
  where
  fns = map snd $ basicFunctions (dom, acGuide)
  applyOps usedOpIndices [] fn =
    (fn, PPolyConstruction acGuide dom i0 (reverse usedOpIndices))
  applyOps usedOpIndices ((opIndex, operandIndices):rest) fn
    | maximum (map (ChPoly.degree . centre . snd) (ppoly_pieces fn)) >= deg =
      (fn, PPolyConstruction acGuide dom i0 (reverse usedOpIndices))
    | otherwise =
      applyOps ((opIndex, operandIndices):usedOpIndices) rest newFn
    where
    (_arity, opList) = operations !! opIndex
    operands = map (fns !!) operandIndices
    newFn = centreAsBall $ liftCheb2PPoly (reduceSizeUsingAccuracyGuide acGuide) $ opList (fn : operands)

type OpIndex = Integer
type Arity = Integer

operations :: [(Arity, [PPoly] -> PPoly)]
operations =
  [op2 (+), op2 (-), op2 (*), op1 recipShift, (1, addBreak)]
  where
  op1 op = (1, \[e] -> op e)
  op2 op = (2, \[e1,e2] -> op e1 e2)
  acGuide = bits 10
  recipShift p = inverseWithAccuracy acGuide (p - lb + 1)
    where
    lb :: MPBall
    (lb, _) =
        endpoints $
          -- minimumOverDom p (getDomain p)
          minimumOptimisedWithAccuracy p (mpBall l) (mpBall r) 5 5 acGuide
          where
          (Interval l r) = getDomain p
  addBreak [p] =
    -- Force a break point in the partition by adding a piecewise constant 0:
    p + (linearPolygonI [(dyadic $ -1,mpBall 0),(x,mpBall 0),(dyadic 1,mpBall 0)] dom acGuide)
    where
    dom = getDomain p
    Interval rl ru = applyApprox p dom
    rlA = abs rl
    ruA = abs ru
    x
      | rlA == 0 || ruA == 0 = dyadic 0
      | otherwise = centre $ (ruA - rlA) /! (mpBall $ rlA + ruA)
    -- x is an approximate average of dom endpoints, weighted by the range endpoints.
    -- This definitoin is deliberately rather arbitrary to achieve a high variation.
  addBreak _ = error "addBreak used with wrong arity"

type FnIndex = Integer
type Frequency = Integer

basicFunctions :: (DyadicInterval, Accuracy) -> [(Frequency, PPoly)]
basicFunctions domAcc = [(10,x), (1, c 0.5), (1, c 2), (1, c 100), (1, c (0.5^!20))]
  where
  x = fromPoly $ varFn domAcc ()
  c :: (CanBeDyadic t) => t -> PPoly
  c n = fromPoly $ constFn domAcc (dyadic n)

instance HasDomain PPolyConstruction where
  type Domain PPolyConstruction = DyadicInterval
  getDomain = ppConstr_dom

instance
  -- (Arbitrary c, IsBall c, Show c) => Arbitrary (ChPolyConstruction c)
  Arbitrary PPolyConstruction
  where
  arbitrary =
    arbitraryWithDom =<< arbitraryNonEmptySmallInterval
    --arbitraryWithDom =<< return (dyadicInterval (-1.0,1.0))

instance
  -- (Arbitrary c, IsBall c, Show c) => ArbitraryWithDom (ChPolyConstruction c)
  ArbitraryWithDom (PPolyConstruction)
  where
  arbitraryWithDom = arbitraryWithMinOpsDom 0

arbitraryWithMinOpsDom :: Integer -> DyadicInterval -> Gen PPolyConstruction
arbitraryWithMinOpsDom minOps dom =
  sized withSize
  where
  withSize size =
    do
    numOfOps <- growingElements [minOps..(minOps+10+size)]
    ops <- vectorOf (int numOfOps) (elements opIndicesArities)
    fn0 <- elementsWeighted fnIndices
    opIndices <- mapM addOperands ops
    return $ PPolyConstruction acGuide dom fn0 opIndices
    where
    opIndicesArities = zip [0..] $ map fst operations
    fnIndices = map (\(i,(n,_)) -> (n,i)) $ zip [0..] $ basicFunctions (dom, acGuide)
    elementsWeighted es = frequency $ map (\(n,e) -> (int n, return e)) es
    acGuide = bits $ 10 + size
    addOperands (i, arity) =
      do
      operandIndices <- mapM getOperandIndex [2..arity]
      return (i, operandIndices)
      where
      getOperandIndex _ = elementsWeighted fnIndices


arbitraryWithDegDom :: Integer -> DyadicInterval -> Gen (PPoly, PPolyConstruction)
arbitraryWithDegDom deg dom =
  sized withSize
  where
  withSize size =
    do
    ops <- infiniteListOf (elements opIndicesArities)
    fn0 <- elementsWeighted fnIndices
    opIndices <- mapM addOperands ops
    return $ pPolyFromOpsWithDeg deg $ PPolyConstruction acGuide dom fn0 opIndices
    where
    opIndicesArities = zip [0..] $ map fst operations
    fnIndices = map (\(i,(n,_)) -> (n,i)) $ zip [0..] $ basicFunctions (dom, acGuide)
    elementsWeighted es = frequency $ map (\(n,e) -> (int n, return e)) es
    acGuide = bits $ 100 + size
    addOperands (i, arity) =
      do
      operandIndices <- mapM getOperandIndex [2..arity]
      return (i, operandIndices)
      where
      getOperandIndex _ = elementsWeighted fnIndices

instance
  Arbitrary (FnAndDescr PPoly)
  where
  arbitrary =
    do
    constr <- arbitrary
    return $ FnAndDescr (pPolyFromOps constr) (show constr)

instance
  ArbitraryWithDom (FnAndDescr PPoly)
  where
  arbitraryWithDom dom =
    do
    constr <- arbitraryWithDom dom
    return $ FnAndDescr (pPolyFromOps constr) (show constr)

instance Arbitrary PPoly where
  arbitrary =
    do
    (FnAndDescr f _) <- arbitrary
    return f

{-|
  A runtime representative of type @ChPoly MPBall@.
  Used for specialising polymorphic tests to concrete types.
-}
tPPoly :: T PPoly
tPPoly = T "PPoly"

anyFn :: FnAndDescr PPoly -> FnAndDescr PPoly
anyFn = id

makeFnPositive :: FnAndDescr PPoly -> FnAndDescr PPoly
makeFnPositive (FnAndDescr p pDescr) =
  FnAndDescr res $ "makeFnPositive (" ++ pDescr ++ ")"
  where
  res
    | lb !>! 0 = p
    | otherwise = centreAsBall $ p - lb + 1
  Interval l r = getDomain p
  lb :: MPBall
  (lb, _) = endpoints $ minimumOptimisedWithAccuracy p (mpBall l) (mpBall r) 5 5 (bits 0)

makeFnSmallRange :: Integer -> FnAndDescr PPoly -> FnAndDescr PPoly
makeFnSmallRange limit (FnAndDescr p pDescr) =
  maybeTrace (printf "makeFnSmallRange: p = %s" (show p)) $
  maybeTrace (printf "makeFnSmallRange: p construction = %s" pDescr) $
  maybeTrace (printf "makeFnSmallRange: radius p = %s" (show (radius p))) $
  maybeTrace (printf "makeFnSmallRange: lb = %s" (show lb)) $
  maybeTrace (printf "makeFnSmallRange: ub = %s" (show ub)) $
  FnAndDescr res $ "makeFnSmallRange " ++ show limit ++  " (" ++ pDescr ++ ")"
  where
  res
    | b !<! limit = p
    | otherwise = centreAsBall $ (limit * p /! b)
  b = ub `max` (-lb)
  lb, ub :: MPBall
  -- (lb, _) = endpoints $ minimumOverDom p (getDomain p)
  -- (_, ub) = endpoints $ maximumOverDom p (getDomain p)
  (lb, _) = endpoints $ minimumOptimisedWithAccuracy p (mpBall l) (mpBall r) 5 5 (bits 0)
  (_, ub) = endpoints $ maximumOptimisedWithAccuracy p (mpBall l) (mpBall r) 5 5 (bits 0)
  Interval l r = getDomain p

makeFnPositiveSmallRange :: Integer -> FnAndDescr PPoly -> FnAndDescr PPoly
makeFnPositiveSmallRange limit (FnAndDescr p pDescr) =
  FnAndDescr res $ "makeFnPositiveSmallRange " ++ show limit ++  " (" ++ pDescr ++ ")"
  where
  res
    | 1 !<=! lb && ub !<! limit = p
    | b !<! limit = p - lb + 1
    | otherwise = centreAsBall $ (1 - lb + (limit * p /! b))
  b = ub `max` (-lb)
  lb, ub :: MPBall
  -- (lb, _) = endpoints $ minimumOverDom p (getDomain p)
  -- (_, ub) = endpoints $ maximumOverDom p (getDomain p)
  (lb, _) = endpoints $ minimumOptimisedWithAccuracy p (mpBall l) (mpBall r) 5 5 (bits 0)
  (_, ub) = endpoints $ maximumOptimisedWithAccuracy p (mpBall l) (mpBall r) 5 5 (bits 0)
  Interval l r = getDomain p


-- precondAnyT :: t -> Bool
-- precondAnyT _t = True
--
-- precondNonZeroT :: (HasEqCertainly t Integer) => t -> Bool
-- precondNonZeroT t = t !/=! 0
--
-- precondSmallT :: (HasOrderCertainly t Integer) => t -> Bool
-- precondSmallT t = -1000 !<=! t && t !<=! 1000

specChPoly :: Spec
specChPoly =
  describe ("ChPoly") $ do
    -- describe "evaluation" $ do
    --   specEvalConstFn tMPBall tPPoly tMPBall
    --   specEvalUnaryVarFn tPPoly tMPBall
    describe "ring" $ do
      specFnPointwiseOp2 tPPoly tMPBall "+" (+) (+) anyFn anyFn
      specFnPointwiseOp2 tPPoly tMPBall "-" (-) (-) anyFn anyFn
      specFnPointwiseOp2 tPPoly tMPBall "*" (*) (*) anyFn anyFn
    describe "field" $ do
      specFnPointwiseOp1 tPPoly tMPBall "1/" (inverseWithAccuracy (bits 0)) (1/!) (makeFnPositiveSmallRange 100)
    -- describe "size reduction" $ do
    --   specFnPointwiseOp1 tPPoly tMPBall "reduce size (bits=10)" (reduceSizeUsingAccuracyGuide (bits 10)) id anyFn
    --   specFnPointwiseOp1 tPPoly tMPBall "reduce size (bits=0)" (reduceSizeUsingAccuracyGuide (bits 0)) id anyFn
      -- specCanReduceSizeUsingAccuracyGuide tPPoly
    -- describe "range" $ do
    --   specCanMaximiseOverDom tPPoly tMPBall
    -- describe "trigonometric" $ do
    --   specFnPointwiseOp1 tPPoly tMPBall "sine" (sineWithAccuracyGuide (bits 10)) (sin) (makeFnSmallRange 10)

generate :: IO ()
generate =
  do
  fns <- (sample' arbitrary :: IO [PPolyConstruction])
  mapM_ putStrLn $ concat $ map (\fnC -> [show fnC, show (pPolyFromOps fnC)]) fns

test1 =
  pPolyFromOps $
  PPolyConstruction
    {ppConstr_acGuide = bits 18, ppConstr_dom = Interval (dyadic (-1)) (dyadic 1),
     ppConstr_i0 = 0, -- x
     ppConstr_opIndices = [(2,[0]),(4,[])]} -- addBreak(x^2)

{- recent issues:
-}

{- a template for probing bugs:

generate :: IO ()
generate =
  do
  fns <- (sample' arbitrary :: IO [PPolyConstruction])
  mapM_ putStrLn $ concat $ map (\fnC -> [show fnC, show (pPolyFromOps fnC)]) fns


dom1 = Interval (dyadic 100663296) (dyadic 100663299)

p1D =
  PPolyConstruction {ppConstr_acGuide = bits 12, ppConstr_dom = dom1,
    ppConstr_i0 = 0, ppConstr_opIndices = [(0,[3]),(0,[0])]}

p1 = chPolyFromOps p1D
p1FD = FnAndDescr p1 (show p1D)

p2D =
  PPolyConstruction {ppConstr_acGuide = bits 30, ppConstr_dom = dom12,
    ppConstr_i0 = 4,
    ppConstr_opIndices = [(2,[0]),(0,[0]),(0,[0]),(0,[0]),(0,[0]),(2,[0]),(2,[0]),(2,[0]),(0,[0]),(0,[0]),(2,[0]),(1,[0]),(1,[0]),(2,[1]),(0,[0]),(1,[0]),(0,[0]),(2,[4]),(0,[0]),(2,[0]),(2,[0])]}

p2 = chPolyFromOps p2D
p2FD = FnAndDescr p2 (show p2D)

FnAndDescr p2sm _ = makeFnPositiveSmallRange 100 p2FD

p1Divp2sm = chebDivideDCT (bits 0) p1 p2sm

-- pt = (mpBall 1104) + (mpBall )

-}
