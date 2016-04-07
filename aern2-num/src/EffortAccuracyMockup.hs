{-# LANGUAGE ImplicitParams #-}

{-|

An experiment to validate and refine an approach to passing effort to enclosure functions.

The main points of this approach are:
* We never rely on values to provide some aspects of effort (eg precision).
* There is only one effort type and each inexact enclosure function takes this kind of effort as a parameter.
* We always use the implicit parameter ?effort to pass effort to enclosure functions.

There is the following caveat when using the default parameter ?effort.  
In a context where ?effort is defined and has the value e1, and we write f = g where
g has the type constraint (?effort :: Effort), f will *not* inherit
this constraint.  Instead, f will have the value of ?effort fixed to e1.
This means that when we write let ?effort = e2 in ... f ..., f will ignore
the value e2 and use the value e1 for ?effort.
Nevertheless, when we write f a = g a, the constraint will be inherited.
This means that when we write let ?effort = e2 in ... f ..., f will use
the value e2 for ?effort.

-}
module EffortAccuracyMockup where

import Prelude
import Data.String (fromString)

import qualified Data.Map as Map

testBall :: IO ()
testBall =
    do
    let ?effort = effort 
    print $ (MPBall 3 0.1) `mulBall` (MPBall 4 0.1)
    where
    effort =
        effortSingleton "precision" 100

testCR :: IO ()
testCR =
    do
    putStrLn $ showCR $ (cr 2.0) `mulCR` (cr 0.125)

testSqrt :: IO ()
testSqrt =
    do
    let ?effort = effort
    putStrLn $ show $ sqrtBall (MPBall 1.44 0)
    where
    effort =
        effortFromList 
            [("initialPrecision", 100),
             ("sufficientAccuracy", 20)]

{- approximation level -}

type Accuracy = Integer
type Precision = Integer
type MPNum = Double -- eek
type Effort = Map.Map String Effort'
data Effort' = Effort Integer Effort

effortSingleton :: String -> Integer -> Effort
effortSingleton name value = 
    Map.singleton name (Effort value Map.empty)

effortFromList :: [(String, Integer)] -> Effort
effortFromList keyValues =
    Map.fromList $ map addEffort keyValues
    where
    addEffort (k,v) = (k, Effort v Map.empty)

(+:) :: (?effort :: Effort) => MPNum -> MPNum -> MPNum
(-:) :: (?effort :: Effort) => MPNum -> MPNum -> MPNum
(*:) :: (?effort :: Effort) => MPNum -> MPNum -> MPNum
(/:) :: (?effort :: Effort) => MPNum -> MPNum -> MPNum

infixl 7  *:, /:  
infixl 6  +:, -:

a +: b =
    seq _p $ a + b
    where
    _p = getTopLevelEffortComponent "precision"
a -: b =
    seq _p $ a - b
    where
    _p = getTopLevelEffortComponent "precision"
a *: b =
    seq _p $ a * b
    where
    _p = getTopLevelEffortComponent "precision"
a /: b =
    seq _p $ a / b
    where
    _p = getTopLevelEffortComponent "precision"

getTopLevelEffortComponent :: (?effort :: Effort) => String -> Integer
getTopLevelEffortComponent name =
    case Map.lookup name ?effort of
        Just (Effort r _) -> r
        _ -> error $ "effort structure is missing the component " ++ name

{- enclosure level -}

data MPBall = MPBall { centre :: MPNum, radius :: Double }
    deriving Show

mpBall :: Double -> MPBall
mpBall d = MPBall d 0

getAccuracy :: MPBall -> Accuracy
getAccuracy (MPBall _ r) = ceiling (-(log r))


negBall :: MPBall -> MPBall
negBall (MPBall c r) = MPBall (-c) r

{-
    MPBall @?effort@:
    * "precision": @MPNum@ precision 
-}

addBall :: (?effort :: Effort) => MPBall -> MPBall -> MPBall
addBall (MPBall ac ar) (MPBall bc br) =
    -- TODO: use the effort
    MPBall (ac+:bc) (ar+:br)
    where
    -- FIXME: add a bound on the rounding error in ac+bc

mulBall :: (?effort :: Effort) => MPBall -> MPBall -> MPBall
mulBall (MPBall ac ar) (MPBall bc br) =
    -- TODO: use the effort
    MPBall (ac*:bc) (ar*br +: (abs ac)*:br + (abs bc)*:ar)
    where
    -- FIXME: add a bound on the rounding error in ac*bc

divBall :: (?effort :: Effort) => MPBall -> MPBall -> MPBall
divBall (MPBall ac ar) (MPBall bc br) 
    | denom <= 0 = error "div by 0"
    | otherwise = 
        -- TODO: use the effort
        MPBall divCentre
            ((ar +: (abs divCentre) *: br) /: denom)
    where
    denom = (abs bc) +: (-br)
    divCentre = ac /: bc
    -- FIXME: add a bound on the rounding error in ac/bc

ballHull :: (?effort :: Effort) => MPBall -> MPBall -> MPBall
ballHull (MPBall ac ar) (MPBall bc br) =
    -- TODO: use the effort
    -- TODO: optimise when one contains the other
    MPBall c rad
    where
    l = (ac - ar) `min` (bc - br)
    r = (ac + ar) `max` (bc + br)
    c = (l + r)/2
    rad = (r - l)/2

ballCentre :: MPBall -> MPBall
ballCentre (MPBall c _) = MPBall c 0

sqrtBall :: (?effort :: Effort) => MPBall -> MPBall
sqrtBall x =
    terminate $ drop 1 $ iterate newton $ ballHull x (mpBall 1)
    where
    sufficientAccuracy =
        getTopLevelEffortComponent "sufficientAccuracy"
    initialPrecision =
        getTopLevelEffortComponent "initialPrecision"
    arithEffort = effortSingleton "precision" initialPrecision
    terminate enclosures =
        search enclosuresWithAccuracies
        where
        search ((enclosure,acc,accNext):rest)
            | acc >= sufficientAccuracy = enclosure -- TODO: terminate also when there is no improvement
            | otherwise = search rest
        enclosuresWithAccuracies = zip3 enclosures accuracies (drop 1 accuracies)
        accuracies = map getAccuracy enclosures
    newton xi =
        -- TODO: increase precision with each iteration
        let ?effort = arithEffort in
        ((ballCentre xi)/!(mpBall 2)) +! (x/!((mpBall 2)*!xi))
        where
--        (+!) :: (?effort :: Effort) => MPBall -> MPBall -> MPBall
        (+!) a = addBall a
--        (*!) :: (?effort :: Effort) => MPBall -> MPBall -> MPBall
        (*!) a = mulBall a
--        (/!) :: (?effort :: Effort) => MPBall -> MPBall -> MPBall
        (/!) a = divBall a


{- effective level -}

type CR = Accuracy -> MPBall

showCR :: CR -> String
showCR x = show (x 10)

mulCR :: CR -> CR -> CR
mulCR a b acc =
    -- TODO: use accuracy more efficiently
    aux acc
    where
    aux acc2 
        | getAccuracy result < acc =
            aux (acc2 + 1)
        | otherwise =
            result
            where
            result = 
                (a acc2) `mulBall` (b acc2)
                where 
                ?effort = effortSingleton "precision" p
            p = max 10 acc2
    
cr :: Double -> CR
cr d acc = 
    MPBall d (0.35^acc)
