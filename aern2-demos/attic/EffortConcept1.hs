module EffortConcept1 where

import MixedTypesNumPrelude hiding (id, (.))
-- import qualified Prelude as P
-- import Text.Printf

-- import Control.Arrow

import Control.Category

-- import qualified Data.List as List

-- import AERN2.QA.Protocol
import AERN2.MP
-- import AERN2.Real

-- import Debug.Trace
--------------------------------
-- based on Eike's ideas:

class HasUEffort e where
  getUEffort :: e -> UEffort

class CanSetUEffort e where
  setUEffort :: UEffort -> e -> e

type UEffort = Integer

instance HasUEffort MPBall where
  getUEffort = integer . getPrecision

instance CanSetUEffort MPBall where
  setUEffort e = setPrecision (prec e)

-- fnF :: effort -> enclA -> enclB
-- fnF = undefined
--
-- fnG :: UEffort -> effort
-- fnG = undefined
--
-- fn :: (HasUEffort enclA) => enclA -> enclB
-- fn = mergeFG fnF fnG
--
-- mergeFG ::
--   (HasUEffort enclA) =>
--   (effort -> enclA -> enclB) -> (UEffort -> effort) -> (enclA -> enclB)
-- mergeFG f g x =
--   f (g $ getUEffort x) x

{-
  ***********************************************
  a version with stateful iteration
  ***********************************************
-}

data Effort1 = Effort1
data State1 = State1 (Effort1, [Integer])

fn1SF :: Effort1 -> (enclA,State1) -> (enclB,State1)
fn1SF = undefined

fn1SG :: UEffort -> Effort1
fn1SG = undefined

fn1S :: (HasUEffort enclA, enclB~MPBall) => (enclA,State1) -> (enclB,State1)
fn1S = mergeSFG fn1SF fn1SG

data Effort2 = Effort2
data State2 = State2

fn2SF :: effort -> (enclA,State2) -> (enclB,State2)
fn2SF = undefined

fn2SG :: UEffort -> effort
fn2SG = undefined

fn2S :: (enclA~MPBall) => (enclA,State2) -> (enclB,State2)
fn2S = mergeSFG fn2SF fn2SG

fnS :: (HasUEffort a) => (a, (State1,State2)) -> (b, (State1,State2))
fnS =  fn2S `composeS` fn1S


composeS :: ((b,s2) -> (c,s2)) ->  ((a,s1) -> (b,s1)) ->  ((a,(s1,s2)) -> (c,(s1,s2)))
composeS f2 f1 = f12
  where
  f12 (x,(s1,s2)) =
    let (y,s1') = f1 (x,s1) in
    let (z,s2') = f2 (y,s2) in
    (z, (s1',s2'))

-- note that composeS is not associative, thus one cannot turn it into a category in Haskell

mergeSFG ::
  (HasUEffort enclA) =>
  (effort -> (enclA, s) -> (enclB, s)) -> (UEffort -> effort) -> ((enclA, s) -> (enclB, s))
mergeSFG f g (x,s) =
  f (g $ getUEffort x) (x,s)

evalS :: s -> ((a,s) -> (b,s)) -> (Accuracy -> a) -> (Accuracy -> b)
evalS _initS _fnS = undefined


{-
  ***********************************************
  a version with a stateful iteration which hides the state in a new function returned by the function
  ***********************************************
-}

data FnC a b = FnC (a -> (b, FnC a b))

mergeCFG ::
  (HasUEffort enclA) =>
  (FnC (effort, enclA) enclB) -> (UEffort -> effort) -> FnC enclA enclB
mergeCFG (FnC f) g = FnC ff
  where
  ff x =
    case f (g (getUEffort x), x) of
      (y, fC') ->
        (y, mergeCFG fC' g)

myexpCF :: FnC ((Integer, Precision), MPBall) MPBall
myexpCF = FnC (f [1])
  where
  f precompFactorials ((acGuide,p), x) =
    (res, FnC (f $ take (length terms) factorials))
    -- (res, FnC (f [1])) -- switch-off caching of factorials
    where
    xP = setPrecision p x

    pf = precompFactorials
    factorials = pf ++ (continueFactorials (integer $ length pf) (last pf))
      where
      continueFactorials n prev = next : continueFactorials (n+1) next
        where next = n*prev

    terms = takeUntilTooSmall $ zipWith (/!) [ xP^!i | i <- [0..] ] factorials
    res = sum terms

    takeUntilTooSmall [] = undefined
    takeUntilTooSmall (t:ts)
      | getAccuracy eB > acGuide = [eB]
      | otherwise = t : (takeUntilTooSmall ts)
      where
      eB = 3 * (fromEndpoints t (-t) :: MPBall) -- assuming x <= 1


myexpCG :: UEffort -> (Integer, Precision)
myexpCG e = (e, prec (10*e))

myexpC :: FnC MPBall MPBall
myexpC = mergeCFG myexpCF myexpCG

fn1CF :: FnC (Effort1, enclA) enclB
fn1CF = undefined

fn1CG :: UEffort -> Effort1
fn1CG = undefined

fn1C :: (HasUEffort enclA, enclB~MPBall) => FnC enclA enclB
fn1C = mergeCFG fn1CF fn1CG

fn2CF :: FnC (Effort2, enclA) enclB
fn2CF = undefined

fn2CG :: UEffort -> Effort2
fn2CG = undefined

fn2C :: (HasUEffort enclA) => FnC enclA enclB
fn2C = mergeCFG fn2CF fn2CG

composeC :: FnC b c -> FnC a b -> FnC a c
composeC (FnC f2) (FnC f1) = FnC f12
  where
  f12 a =
    case f1 a of
      (b, f1C') ->
        case f2 b of
          (c, f2C') ->
            (c, composeC f2C' f1C')

instance Category FnC where
  id = FnC (\ a -> (a, id))
  (.) = composeC

fnC :: (HasUEffort enclA) => FnC enclA enclB
fnC = fn2C . fn1C

evalC :: (CanSetUEffort a, HasAccuracy b) => FnC a b -> (Accuracy -> a) -> (Accuracy -> b)
evalC fC0 a = aux fC0
  where
  aux (FnC f) ac =
    let
      aB = a ac
      e = fromAccuracy ac
    in
    case f (setUEffort e aB) of
      (b, fnC')
        | getAccuracy b >= ac -> b
        | otherwise -> aux fnC' (ac + 1)
