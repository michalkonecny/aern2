module AERN2.Linear.Matrix.Type where

import qualified Prelude as P
import MixedTypesNumPrelude
import qualified Numeric.CollectErrors as CN
import AERN2.Linear.Vector.Type (Vector, (!))
import qualified AERN2.Linear.Vector.Type as V
import qualified Data.List as List
-- import Data.Maybe

-- import Debug.Trace

import AERN2.MP.Ball
-- import AERN2.MP.Float
-- import AERN2.MP.Dyadic

data (Matrix a) = 
    Matrix
    {
        width     :: Integer,
        entries   :: Vector a
    } deriving (Show)

height :: Matrix a -> Integer
height (Matrix w e) = 
    (V.length e) `P.div` w

get :: Matrix a -> Integer -> Integer -> a
get m i j =
    entries m ! (i * (width m) + j)

identity :: (HasIntegers a) => Integer -> Integer -> Matrix a
identity m n = 
    diag m n (convertExactly 1)

diag :: (HasIntegers a) => Integer -> Integer -> a -> Matrix a
diag m n x = 
    create m n (\i j -> if i == j then x else (convertExactly 0))

rows :: Matrix a -> [Vector a]
rows m@(Matrix w e) = 
    [V.slice (i*w) w e| i <- [0 .. height m - 1]]

columns :: Matrix a -> Vector (Vector a)
columns m = 
    V.map (\j -> V.map (\i -> get m i j) $ V.enumFromTo 0 (height m - 1)) $ V.enumFromTo 0 (width m - 1)

create :: Integer -> Integer -> (Integer -> Integer -> a) -> Matrix a
create m n f =
    Matrix n $ V.map (\x -> f (i x) (j x)) $ V.enumFromTo 0 (m*n - 1)
    where
    j x = x `mod` n
    i x = (x - j x) `P.div` n

imap :: (Integer -> Integer -> a -> a) -> Matrix a -> Matrix a
imap f (Matrix w ents) =
    Matrix w (V.imap g ents)
    where
    j x = x `mod` w
    i x = (x - j x) `P.div` w
    g k x = f (i k) (j k) x

instance CanIntersectAsymmetric (Matrix (CN MPBall)) (Matrix (CN MPBall)) where
    type IntersectionType (Matrix (CN MPBall)) (Matrix (CN MPBall)) = Matrix (CN MPBall)
    intersect (Matrix w0 v0) (Matrix _w1 v1) =
        Matrix w0 $ V.zipWith intersect v0 v1

inftyNorm :: (CanAddSameType a, CanSubSameType a, CanAbsSameType a, HasIntegers a, CanMinMaxSameType a) => Matrix a -> a
inftyNorm (m :: Matrix a) = 
    -- TODO: could be optimised.
    List.foldl' max (convertExactly 0 :: a)
    [
        V.foldl' (+) (convertExactly 0 :: a) $ V.map abs r 
        |
        r <- rows m
    ]

instance Functor Matrix where
    fmap h m = 
        Matrix (width m) (V.map h (entries m))

instance 
    (CanAddSameType a, CanMulSameType a, HasIntegers a) =>
    CanMulAsymmetric (Matrix a) (Matrix a)
    where
    type MulType (Matrix a) (Matrix a) = Matrix a
    mul m0 m1 = 
        create (height m0) (width m1) (aux 0 (convertExactly 0))
        where
        aux k sm i j = 
            if k == width m0 then 
                sm
            else 
                aux (k + 1) (sm + (get m0 i k) * (get m1 k j)) i j


instance 
    (CanAddSameType a) =>
    CanAddAsymmetric (Matrix a) (Matrix a)
    where
    type AddType (Matrix a) (Matrix a) = Matrix a
    add (Matrix w e) (Matrix _ e') =
        Matrix w (e + e')

instance 
    (CanSubSameType a) =>
    CanSub (Matrix a) (Matrix a)
    where
    type SubType (Matrix a) (Matrix a) = Matrix a
    sub (Matrix w e) (Matrix _ e') =
        Matrix w (e - e')
    

instance 
    (CanAddSameType a, CanMulSameType a, HasIntegers a) =>
    CanMulAsymmetric (Matrix a) (Vector a)
    where
    type MulType (Matrix a) (Vector a) = Vector a
    mul m@(Matrix _w _e) v =
        V.fromList [r * v| r <- rows m]

instance 
    (HasAccuracy a, HasPrecision a) => HasAccuracy (Matrix a)
    where
    getAccuracy m = 
        V.foldl' max NoInformation $ V.map getAccuracy (entries m)

instance 
    (HasPrecision a) => HasPrecision (Matrix a)
    where
    getPrecision m = 
        V.foldl' max (prec 2) $ V.map getPrecision (entries m)

instance 
    (CN.CanTestErrorsPresent a) => CN.CanTestErrorsPresent (Matrix a)
    where
    hasError m = V.foldl' (||) False $ V.map (CN.hasError) (entries m)
