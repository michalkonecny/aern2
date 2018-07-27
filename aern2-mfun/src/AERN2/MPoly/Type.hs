module AERN2.MPoly.Type where

import MixedTypesNumPrelude

import Data.Map (Map)
import qualified Data.Map as Map
import AERN2.Util.Vector (Vector, (!), vlength)
import qualified AERN2.Util.Vector as V
import AERN2.MP.Accuracy
import AERN2.MP.Ball
import AERN2.MPoly.MultiIndex

data MPoly a =
  MPoly
  {
       dim     :: Integer
    ,  terms   :: Map MultiIndex a
  }

coef :: (HasIntegers a) => MPoly a -> MultiIndex -> a
coef (p :: MPoly a) i =
  case Map.lookup i (terms p) of
    Nothing -> (convertExactly 0 :: a)
    Just x  -> x

fromListWith :: (a -> a -> a) -> [(MultiIndex, a)] -> MPoly a
fromListWith f cs =
    MPoly d ts
    where
    d  = (vlength . fst . head) cs
    ts = Map.fromListWith f cs

fromList :: [(MultiIndex, a)] -> MPoly a
fromList cs =
  MPoly d ts
  where
  d  = (vlength . fst . head) cs
  ts = Map.fromList cs

toList :: MPoly a -> [(MultiIndex, a)]
toList (MPoly _d ts) =
  Map.toList ts

instance (HasPrecision a) => HasPrecision (MPoly a) where
  getPrecision (MPoly _d ts) =
    Map.foldl' (\a b -> a `max` (getPrecision b)) (prec 2) ts

instance (CanSetPrecision a) => CanSetPrecision (MPoly a) where
  setPrecision prc (MPoly d ts) =
    MPoly d (Map.map (setPrecision prc) ts)

{-groupTerms :: MPoly a -> Integer -> MPoly a
groupTerms p@(MPoly d ds ts) k =
  if dim p == 0 then
    error "MPoly groupTerms: polynomial dimension must be positive"
  else
    MPoly (d - 1) ds' ts'
  where
  ds' = V.tail ds
  ts' = Map.generate-}

instance (CanAddSameType a) => CanAddAsymmetric (MPoly a) (MPoly a) where
  type AddType (MPoly a) (MPoly a) = MPoly a
  add p q =
    MPoly d (Map.unionWith (+) ts0 ts1)
    where
    (MPoly d ts0) = p `increaseDimensionBy` (max 0 (dim q - dim p))
    (MPoly _ ts1) = q `increaseDimensionBy` (max 0 (dim p - dim q))

instance (CanSubSameType a) => CanSub (MPoly a) (MPoly a) where
  type SubType (MPoly a) (MPoly a) = MPoly a
  sub p q =
    MPoly d (Map.unionWith (-) ts0 ts1)
    where
    (MPoly d ts0) = p `increaseDimensionBy` (max 0 (dim q - dim p))
    (MPoly _ ts1) = q `increaseDimensionBy` (max 0 (dim p - dim q))


increaseDimensionBy :: MPoly a -> Integer -> MPoly a
increaseDimensionBy p@(MPoly d ts) n
  | n == 0    = p
  | n <  0    = error "MPoly increaseDimensionBy: negative increase provided."
  | otherwise =
      MPoly (d + n) (Map.mapKeys (\i -> newIndex i) ts)
      where
      newIndex i =
        V.generate (int $ d + n) (\k -> if k < d then (i ! k) else 0)
