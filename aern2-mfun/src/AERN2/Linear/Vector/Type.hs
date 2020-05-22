module AERN2.Linear.Vector.Type where

import Control.Monad.ST
import Data.STRef
import MixedTypesNumPrelude hiding (length)
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as M
import AERN2.MP.Precision
import AERN2.MP.Ball

type (Vector a) = V.Vector a

(+++) :: Vector a -> Vector a -> Vector a
(+++) = (V.++)

drop :: Int -> Vector a -> Vector a 
drop = V.drop

take :: Int -> Vector a -> Vector a 
take = V.take

empty :: Vector a
empty = V.empty

singleton :: a -> Vector a
singleton = V.singleton

cons :: a -> Vector a -> Vector a
cons = V.cons

fromList :: [a] -> Vector a
fromList = V.fromList

map :: (a -> b) -> Vector a -> Vector b
map = V.map

imap :: (Integer -> a -> b) -> Vector a -> Vector b
imap h = V.imap (\i x -> h (integer i) x)

enumFromTo :: Enum a => a -> a -> Vector a
enumFromTo = V.enumFromTo

slice :: Integer -> Integer -> Vector a -> Vector a
slice i j = V.slice (int i) (int j)

foldl' :: (b -> a -> b) -> b -> Vector a -> b
foldl' = V.foldl'

zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith = V.zipWith

(!) :: Vector a -> Integer -> a
(!) v i = (V.!) v (int i)

length :: Vector a -> Integer
length = integer . V.length

intLength :: Vector a -> Int 
intLength = V.length

inftyNorm :: (HasIntegers a, CanMinMaxSameType a) => Vector a -> a
inftyNorm (v :: Vector a) =
    V.foldl' max (convertExactly 0 :: a) v

instance 
    (HasAccuracy a, HasPrecision a) => HasAccuracy (Vector a)
    where
    getAccuracy v = 
        V.foldl' max NoInformation $ V.map getAccuracy v

instance 
    (HasPrecision a) => HasPrecision (Vector a)
    where
    getPrecision v = 
        if V.null v then 
            (prec 2) 
        else 
            getPrecision $ v ! 0 -- TODO: safe? Alternative: V.foldl' max (prec 2) $ V.map getPrecision v 

instance 
    (CanSetPrecision a) => CanSetPrecision (Vector a)
    where
    setPrecision p = V.map (setPrecision p)

instance 
    (CanAddSameType a) =>
    CanAddAsymmetric (Vector a) (Vector a)
    where
    type AddType (Vector a) (Vector a) = Vector a
    add v w =
        runST $
        do
        mv <- M.new (intLength v)
        aux mv 0
        V.freeze mv
        where
        lth = length v
        aux :: (V.MVector s a) -> Integer -> (ST s ())
        aux mv k = 
            if k == lth then
                return ()
            else
                do                
                M.write mv (int k) (v ! k + w ! k)
                aux mv (k + 1)

instance 
    (CanSubSameType a) =>
    CanSub (Vector a) (Vector a)
    where
    type SubType (Vector a) (Vector a) = Vector a
    sub v w =
        runST $
        do
        mv <- M.new (intLength v)
        aux mv 0
        V.freeze mv
        where
        lth = length v
        aux :: (V.MVector s a) -> Integer -> (ST s ())
        aux mv k = 
            if k == lth then
                return ()
            else
                do                
                M.write mv (int k) (v ! k - w ! k)
                aux mv (k + 1)


instance 
    (CanAddSameType a, CanMulSameType a, HasIntegers a) =>
    CanMulAsymmetric (Vector a) (Vector a) 
    where
    type MulType (Vector a) (Vector a) = a
    mul v w =
        runST $
        do
        sum <- newSTRef (convertExactly 0)
        aux sum 0
        readSTRef sum
        where
        lth = length v
        aux :: (STRef s a) -> Integer -> (ST s ())
        aux sum k = 
            if k == lth then
                return ()
            else
                do                
                modifySTRef sum (\x -> x + (v ! k) * (w ! k))
                aux sum (k + 1)

instance 
    CanMulAsymmetric (CN MPBall) (Vector (CN MPBall)) where
    type MulType (CN MPBall) (Vector (CN MPBall)) = Vector (CN MPBall)
    mul x v = V.map (\y -> x * y) v