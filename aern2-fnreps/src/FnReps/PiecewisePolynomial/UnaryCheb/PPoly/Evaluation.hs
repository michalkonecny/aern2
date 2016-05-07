module FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Evaluation 
(
eval,
range,
range'
)
where

import AERN2.Num
import FnReps.PiecewisePolynomial.UnaryCheb.PPoly.Basics
import qualified FnReps.Polynomial.UnaryCheb.Poly as Poly

import Control.Monad

import Data.List as List

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id

toRationalUp' :: MPBall -> Rational
toRationalUp' x = if (x == 0) == Just True then 0.0 else toRationalUp x

toRationalDown' :: MPBall -> Rational
toRationalDown' x = if (x == 0) == Just True then 0.0 else toRationalDown x

eval :: PPoly -> MPBall -> MPBall
eval (PPoly pieces _) x = 
   maybeTrace
   (
   "eval\n"++
   "argument: " ++ show x ++ "\n" ++
   "pieces: " ++ show (filter (\(i,_) -> i `intersects` x) pieces) ++ "\n" ++
   "pieces intervals: " ++ show (map fst (filter (\(i,_) -> i `intersects` x) pieces)) ++ "\n" ++
   "values: " ++ show (map (\p -> Poly.evalLipschitzOnBall p x) $ polys)++"\n"++
   "union: " ++ show (foldl1 union $ map (\p -> Poly.evalLipschitzOnBall p x) $ polys)++"\n"
   ) $
   foldl1 union $ map (\p -> Poly.evalLipschitzOnBall p x) $ polys
   where
   polys = map snd $ filter (\(i,_) -> i `intersects` x) pieces
   union x y = let (l,r) = ball2endpoints x; (l', r') = ball2endpoints y in
                endpoints2Ball (min l l') (max r r')
           
range :: Accuracy -> PPoly -> Interval MPBall -> Interval MPBall
range ac (PPoly pieces _) (Interval l r) = 
     maybeTrace
     (
       "range\n"++
       "argument: " ++ show (Interval l r) ++ "\n" ++
       "pieces: " ++ show (map (\j -> let Just x = intersection i j in x) $ filter (\j -> not $ isNothing $ intersection i j) $ map fst pieces) ++ "\n" ++
       "values: " ++ show (map (\(Just x) -> x) $ filter (not.isNothing) $ map rangePiece pieces)++"\n"++
       "union: " ++ show (map (\(Just x) -> x) $ filter (not.isNothing) $ map rangePiece pieces)++"\n"
     ) $
     foldl1 union $ map (\(Just x) -> x) $ filter (not.isNothing) $ map rangePiece pieces
     where
     isNothing x = case x of 
                    Nothing -> True
                    Just _  -> False
     i = Interval (toRationalDown' l) (toRationalUp' r)
     union (Interval a b) (Interval c d) = Interval (min a c) (max b d)
     rangePiece (j, p) = case intersection i j of
                            Nothing -> Nothing
                            Just (Interval a' b') -> Just $ Poly.range ac p $ Interval (mpBall a') (mpBall b')

-- range computation which explicitly considers the overlap. This is needed when using range computation for error estimates.
range' :: Accuracy -> PPoly -> Interval MPBall -> Interval MPBall
range' ac (PPoly pieces overlap) (Interval l r) = 
     maybeTrace
     (
     "range'"++ "\n" ++
     "argument: "++ show (Interval l r) ++ "\n" ++
     "pieces: " ++ show (filter (not.isNothing) $ map ((liftM addOverlap).intersectionPiece) pieces) ++ "\n" ++
     "pieces intervals: " ++ show (filter (not.isNothing) $ map ((liftM fst).(liftM addOverlap).intersectionPiece) pieces) ++ "\n" ++
     "values: " ++ show (map (\(Just x) -> x) $ filter (not.isNothing) $ map (liftM (\(j,p) -> (j,(rangePiece (j,p)))).(liftM addOverlap).intersectionPiece) pieces) ++ "\n"++
     "union: " ++ show (foldl1 union $ map (\(Just x) -> x) $ filter (not.isNothing) $ map ((liftM rangePiece).(liftM addOverlap).intersectionPiece) pieces) 
     )
     $
     foldl1 union $ map (\(Just x) -> x) $ filter (not.isNothing) $ map ((liftM rangePiece).(liftM addOverlap).intersectionPiece) pieces
     where
     isNothing x = case x of 
                    Nothing -> True
                    Just _  -> False
     i = Interval (toRationalDown' l) (toRationalUp' r)
     union (Interval a b) (Interval c d) = Interval (min a c) (max b d)
     addOverlap (Interval a b, p) = (Interval (min (toRationalDown r) (max (toRationalUp l) (a - overlap))) 
                                              (max (toRationalUp l) (min (toRationalDown r) (b + overlap))), p)
     intersectionPiece (j, p) = case intersection i j of
                                    Nothing -> Nothing
                                    Just (Interval a' b') -> if a' == b' then
                                                                Nothing
                                                             else
                                                                Just (Interval a' b', p)
     rangePiece (Interval a' b', p) = Poly.range ac p $ Interval (mpBall a') (mpBall b')
                                         

intersects :: Interval Rational -> MPBall -> Bool
intersects i0 b = let l' = toRationalDown' b; r' = toRationalUp' b 
                  in
                   intervalsIntersect i0 (Interval l' r')
                              
intervalsIntersect :: Interval Rational -> Interval Rational -> Bool
intervalsIntersect (Interval l r) (Interval l' r') = 
      (l  <= l' && r' <= r)
   || (l  <= l' && l' <= r)
   || (l  <= r' && r' <= r)
   || (l' <= l  && r  <= r')                                  
                              
intersection :: Interval Rational -> Interval Rational -> Maybe (Interval Rational)
intersection x@(Interval l r) y@(Interval l' r') = 
  if not $ intervalsIntersect x y then
        Nothing
       else
        Just $ Interval a b
   where
   sorted = sort [l,r,l',r']
   a = sorted!!!1
   b = sorted!!!2                               