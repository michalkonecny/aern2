{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- |
--    Adaptation of routines from Linear.Matrix, package "linear" by E. Kmett.
--
--    This version of the functions should fail gracefully
--    when using interval/ball operations and there is insufficient information
--    to obtain the result.
module AERN2.Linear.AdaptedLinear where

-- import Numeric.CollectErrors (NumErrors, CanTakeErrors(..))
-- import qualified Numeric.CollectErrors as CN

-- import qualified Debug.Trace as Debug
-- import Text.Printf (printf)

import Control.Lens
import qualified Data.Foldable as Foldable
import qualified Linear as L
import MixedTypesNumPrelude
import qualified Prelude as P

lu ::
  ( CanAddSubMulBy a a,
    P.Num a,
    CanDivSameType a,
    Foldable m,
    Traversable m,
    Applicative m,
    L.Additive m,
    Ixed (m a),
    Ixed (m (m a)),
    i ~ Index (m a),
    i ~ Index (m (m a)),
    HasEq i i,
    EqCompareType i i ~ Bool,
    P.Integral i,
    CanAddThis Integer i,
    a ~ IxValue (m a),
    m a ~ IxValue (m (m a)),
    P.Num (m a)
  ) =>
  m (m a) ->
  (m (m a), m (m a))
lu a =
  let n = fI (length a)
      fI :: (P.Integral a, P.Num b) => a -> b
      fI = P.fromIntegral
      initU = L.identity
      initL = L.zero
      buildLVal !i !j !l !u =
        let go !k !s
              | k P.== j = s
              | otherwise =
                go
                  (fI $ k + 1)
                  ( s
                      + ( (l ^?! ix i ^?! ix k)
                            * (u ^?! ix k ^?! ix j)
                        )
                  )
            s' = go (P.fromInteger 0) (P.fromInteger 0)
         in l & (ix i . ix j) .~ ((a ^?! ix i ^?! ix j) P.- s')
      buildL !i !j !l !u
        | i P.== n = l
        | otherwise = buildL (fI $ i + 1) j (buildLVal i j l u) u
      buildUVal !i !j !l !u =
        let go !k !s
              | k == j = s
              | otherwise =
                go
                  (fI $ k + 1)
                  ( s
                      P.+ ( (l ^?! ix j ^?! ix k)
                              P.* (u ^?! ix k ^?! ix i)
                          )
                  )
            s' = go (P.fromInteger 0) (P.fromInteger 0)
         in u & (ix j . ix i)
              .~ ( ((a ^?! ix j ^?! ix i) - s')
                     / (l ^?! ix j ^?! ix j)
                 )
      buildU !i !j !l !u
        | i == n = u
        | otherwise = buildU (fI $ i + 1) j l (buildUVal i j l u)
      buildLU !j !l !u
        | j == n = (l, u)
        | otherwise =
          let l' = buildL j j l u
              u' = buildU j j l' u
           in buildLU (fI $ j + 1) l' u'
   in buildLU (P.fromInteger 0) initL initU

luDet ::
  ( CanAddSubMulBy a a,
    P.Num a,
    CanDivSameType a,
    Foldable m,
    Traversable m,
    Applicative m,
    L.Additive m,
    L.Trace m,
    Ixed (m a),
    Ixed (m (m a)),
    i ~ Index (m a),
    i ~ Index (m (m a)),
    HasEq i i,
    EqCompareType i i ~ Bool,
    P.Integral i,
    CanAddThis Integer i,
    a ~ IxValue (m a),
    m a ~ IxValue (m (m a)),
    P.Num (m a)
  ) =>
  m (m a) ->
  a
luDet (a :: m (m a)) =
  let (l, u) = lu a
      detl = Foldable.foldl (*) (P.fromInteger 1 :: a) (L.diagonal l)
      detu = Foldable.foldl (*) (P.fromInteger 1 :: a) (L.diagonal u)
   in detl * detu

luSolve ::
  ( CanAddSubMulBy a a,
    P.Num a,
    CanDivSameType a,
    Foldable m,
    Traversable m,
    Applicative m,
    L.Additive m,
    Ixed (m a),
    Ixed (m (m a)),
    i ~ Index (m a),
    i ~ Index (m (m a)),
    HasEq i i,
    EqCompareType i i ~ Bool,
    HasOrder i Integer,
    OrderCompareType i Integer ~ Bool,
    P.Integral i,
    CanAddThis Integer i,
    CanSub i Integer,  SubType i Integer ~ Integer,
    a ~ IxValue (m a),
    m a ~ IxValue (m (m a)),
    P.Num (m a)
  ) =>
  m (m a) ->
  m a ->
  m a
luSolve a b =
  let (l, u) = lu a
   in backwardSub u (forwardSub l b)

forwardSub ::
  ( P.Num a,
    CanAddSubMulDivBy a a,
    Foldable m,
    L.Additive m,
    Ixed (m a),
    Ixed (m (m a)),
    i ~ Index (m a),
    i ~ Index (m (m a)),
    HasEq i i,
    EqCompareType i i ~ Bool,
    -- , Ord i
    P.Integral i,
    CanAddThis Integer i,
    a ~ IxValue (m a),
    m a ~ IxValue (m (m a))
  ) =>
  m (m a) ->
  m a ->
  m a
forwardSub (a :: m (m a)) b =
  let n = fromIntegral (length b) :: Index (m a)
      fI :: (P.Integral a1, P.Num b) => a1 -> b
      fI = P.fromIntegral
      initX = L.zero
      coeff !i !j !s !x
        | j == i = (s :: a)
        | otherwise = coeff i (fI $ j + 1) (s + ((a ^?! ix i ^?! ix j) * (x ^?! ix j))) x
      go !i !x
        | i == n = x
        | otherwise =
          go
            (fI $ i + 1)
            ( x & ix i
                .~ ( ((b ^?! ix i) - coeff i (P.fromInteger 0) (P.fromInteger 0) x)
                       / (a ^?! ix i ^?! ix i)
                   )
            )
   in go (P.fromInteger 0) initX

backwardSub ::
  ( P.Num a,
    CanAddSubMulDivBy a a,
    Foldable m,
    L.Additive m,
    Ixed (m a),
    Ixed (m (m a)),
    i ~ Index (m a),
    i ~ Index (m (m a)),
    HasEq i i,
    EqCompareType i i ~ Bool,
    HasOrder i Integer,
    OrderCompareType i Integer ~ Bool,
    P.Integral i,
    CanAddThis Integer i, 
    CanSub i Integer,  SubType i Integer ~ Integer,
    a ~ IxValue (m a),
    m a ~ IxValue (m (m a))
  ) =>
  m (m a) ->
  m a ->
  m a
backwardSub (a :: m (m a)) b =
    let n = fromIntegral (length b) :: Index (m a)
        fI :: (P.Integral a1, P.Num b) => a1 -> b
        fI = P.fromIntegral
        initX = L.zero
        coeff !i !j !s !x
            | j == n = (s :: a)
            | otherwise = coeff i
                                (fI $ j+1)
                                (s + ((a ^?! ix i ^?! ix j) * (x ^?! ix j)))
                                x
        go !i !x
            | i < 0 = x
            | otherwise = go (fI $ i-1)
                             (x & ix i .~ ( ((b ^?! ix i) - coeff i (fI $ i+1) (P.fromInteger 0) x)
                                          / (a ^?! ix i ^?! ix i)
                                          ))
    in go (fI $ n-1) initX