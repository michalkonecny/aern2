{-|
    Module      :  AERN2.PQueue
    Description :  IntPSQ hiding keys and values
    Copyright   :  2016 (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    IntPSQ with simplified API, hiding the keys
-}

module AERN2.PQueue
(
  -- * Type
  PQueue
  -- * Query
  , null
  , size
  -- * Construction
  , singleton
  , empty
  -- * Insertion
  , insert
  -- * Deletion
  , minView
)
where

import Prelude hiding (null)
-- import qualified Prelude as P

import qualified Data.IntPSQ as Q

data PQueue p = PQueue { pq_queue :: Q.IntPSQ p (), pq_nextKey :: Int }

null :: PQueue p -> Bool
null = Q.null . pq_queue

size :: PQueue p -> Int
size = Q.size . pq_queue

singleton :: (Ord p) => p -> PQueue p
singleton p = PQueue (Q.singleton 0 p ()) 1

empty :: (Ord p) => PQueue p
empty = PQueue Q.empty 0

insert :: (Ord p) => p -> PQueue p -> PQueue p
insert p q
  | nextKey >= 0 =
    PQueue (Q.insert nextKey p () (pq_queue q)) (nextKey + 1)
  | otherwise =
    error "PQueue insert: key overflow"
  where
  nextKey = pq_nextKey q

minView :: (Ord p) => PQueue p -> Maybe (p, PQueue p)
minView q =
  case Q.minView (pq_queue q) of
    Just (_,p,_,qq) -> Just (p, q { pq_queue = qq })
    Nothing -> Nothing
