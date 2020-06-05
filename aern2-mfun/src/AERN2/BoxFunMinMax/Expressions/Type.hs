module AERN2.BoxFunMinMax.Expressions.Type where

import MixedTypesNumPrelude
import Data.Maybe

import Debug.Trace

-- TODO: Implement symbolic expressions

-- data E = Add E E | Sub E E | Mul E E | Div E E | Sqrt E | Abs E | Var String | Lit Rational | Min E E | Max E E | Negate E

-- minMaxTransformer :: E -> E
-- minMaxTransformer (Add e1 e2) = minMaxTransformer e1

-- The right E does not contain any Min/Max/Abs
-- This is done by dropping one branch for Min/Max
-- For abs, we can rewrite as Max(x,-x), and apply previous rule
-- The left E is a list of expressions where all expressions must be >= 0
--  If this is true, then the right E is equivalent to the original E
-- minMaxAbsEliminator :: E -> [([E],E)]
-- minMaxAbsEliminator (Min e1 e2) = ([Sub e2 e1], e1) : ([Sub e1 e2], e2) : minMaxAbsEliminator e1 : minMaxAbsEliminator e2
-- minMaxAbsEliminator (Max e1 e2) = ([Sub e1 e2], e1) : ([Sub e2 e1], e2) : minMaxAbsEliminator e1 : minMaxAbsEliminator e2
-- minMaxAbsEliminator (Abs e)     = minMaxAbsEliminator (Max (e, Negate e))

-- say we have Min (e1, e2)
-- function would return [([Sub e2 e1], e1), ([Sub e1 e2], e2)]
-- This is only the case if e1 e2 are without Abs/Min/Max 

-- Next step, E -> MinMaxTree

-- In particular, we can take an E value and put all the abs on the top

-- If we abandon symbolic expressions...
-- We want abs on trees
-- We need to implement mathematical operations on trees
-- TODO: Implement addition on trees
--  - Easy case, one tree is a leaf. Add result of leaf to all leaves in the other tree
--  - Another easy case, tree is the same shape. Simply add matching leaves with eachother
--  - Other cases will need thinking
--  -  Probably can just add a dummy Min/Max node and then recurse


-- BoxFunMinMax will be
-- MinMaxTree, dimension, and domain

data E = Add E E | Sub E E | Mul E E | Div E E | Sqrt E | Negate E | Var String | Lit Rational | Min E E | Max E E |  Abs E
  deriving Show

-- The right E does not contain any Min/Max/Abs
-- This is done by dropping one branch for Min/Max
-- For abs, we can rewrite as Max(x,-x), and apply previous rule
-- The left E is a list of expressions where, if all expressions 
-- are >= 0, the right E is equivalent to the original E
minMaxAbsEliminator :: E -> [([E],E)]
minMaxAbsEliminator (Min e1 e2)   = [([Sub e2 e1], e1), ([Sub e1 e2], e2)]
minMaxAbsEliminator (Max e1 e2)   = [([Sub e1 e2], e1), ([Sub e2 e1], e2)]
minMaxAbsEliminator (Abs e)       = minMaxAbsEliminator (Max e (Negate e))
minMaxAbsEliminator e             = [([],e)] -- Vacuously true

expressionsGEZero :: [E] -> Maybe Bool
expressionsGEZero []                              = Just True
expressionsGEZero ((Lit e) : es)                  = Just (e >= 0) && expressionsGEZero es
expressionsGEZero (e : es)                        = case computeE e of
                                                      Nothing -> Nothing
                                                      Just e' ->
                                                        expressionsGEZero [e'] && expressionsGEZero es

chooseEFromEliminatorList :: [([E],E)] -> E
chooseEFromEliminatorList x = 
  let
    l = (map (\es -> (expressionsGEZero (fst es), snd es :: E))) x
    
    findE :: [(Maybe Bool,E)] -> E
    findE []       = Var "nothing" --TODO: Nothing
    findE (b : bs) = if fromMaybe False (fst b) then snd b else findE bs
  in
    findE l



-- Add (Min (Lit 1) (Lit 2)) (Max (Min (Lit 4) (Lit 8)) (Lit 7))

-- Break E down to Just Lit or Just Var. If we have an invalid operation, return Nothing
computeE :: E -> Maybe E -- TODO: Handle Var
computeE (Lit e)                 = Just $ Lit e
computeE (Var e)                 = Just $ Var e

computeE (Add (Lit e1) (Lit e2)) = Just $ Lit (e1 + e2)
computeE (Add e1 e2)             = case computeE e1 of
                                    Nothing -> Nothing
                                    Just e1' ->
                                      case computeE e2 of
                                        Nothing -> Nothing
                                        Just e2' ->
                                          computeE $ Add e1' e2'

computeE (Sub e1 e2)             = computeE $ Add e1 (Negate e2)

computeE (Mul (Lit e1) (Lit e2)) = Just $ Lit (e1 * e2)
computeE (Mul e1 e2)             = case computeE e1 of
                                    Nothing -> Nothing
                                    Just e1' ->
                                      case computeE e2 of
                                        Nothing -> Nothing
                                        Just e2' ->
                                          computeE $ Mul e1' e2'

computeE (Div (Lit e1) (Lit e2)) = if e2 == 0 then Nothing else Just (Lit (e1 /! e2))
computeE (Div e1 e2)             = case computeE e1 of
                                    Nothing -> Nothing
                                    Just e1' ->
                                      case computeE e2 of
                                        Nothing -> Nothing
                                        Just e2' ->
                                          computeE $ Div e1' e2'

computeE (Min e1 e2)             = computeE $ chooseEFromEliminatorList (minMaxAbsEliminator (Min e1 e2))
computeE (Max e1 e2)             = computeE $ chooseEFromEliminatorList (minMaxAbsEliminator (Max e1 e2))
computeE (Abs e)                 = computeE $ chooseEFromEliminatorList (minMaxAbsEliminator (Abs e))

computeE (Sqrt e)                = case computeE e of
                                    Nothing -> Nothing
                                    Just _ -> undefined -- TODO: Finish

computeE (Negate (Lit e))        = Just $ Lit (negate e)
computeE (Negate e)              = case computeE e of
                                    Nothing -> Nothing
                                    Just e' -> computeE (Negate (e'))



-- computeE :: E -> Integer
-- computeE (Add (Lit e1) (Lit e2)) = e1+e2
-- computeE (Add (Lit e1) e2)       =
--   case e2 of
--     Min m1 m2 ->
--       trace "1"
--       computeE (Add (Min (Lit e1) (Lit e1)) (Min m1 m2))
--     Max m1 m2 ->
--       trace "2"
--       computeE (Add (Max (Lit e1) (Lit e1)) (Max m1 m2))
--     _ ->
--       trace "3"
--       computeE (Add (Lit e1) (Lit (computeE e2)))
-- computeE (Add e1 (Lit e2))       = computeE (Add (Lit e2) e1)
-- computeE (Add e1 e2)             = 
--   case e1 of
--     Min m1 m2 ->
--       trace "4"
--       computeE (Add (chooseEFromEliminatorList (minMaxAbsEliminator (Min m1 m2))) e2)
--     Max m1 m2 ->
--       trace "5"
--       computeE (Add (chooseEFromEliminatorList (minMaxAbsEliminator (Max m1 m2))) e2)
--     Abs e ->
--       trace "6"
--       computeE (Add (chooseEFromEliminatorList (minMaxAbsEliminator (Abs e))) e2)
--     _ -> case e2 of
--       Min m1 m2 ->
--         trace "7"
--         computeE (Add e1 (chooseEFromEliminatorList (minMaxAbsEliminator (Min m1 m2))))
--       Max m1 m2 ->
--         trace "8"
--         computeE (Add e1 (chooseEFromEliminatorList (minMaxAbsEliminator (Max m1 m2))))
--       Abs e ->
--         trace "9"
--         computeE (Add e1 (chooseEFromEliminatorList (minMaxAbsEliminator (Abs e))))
--       _ ->
--         trace "No" $
--         -1000

  

  
  -- let x = map (\l2 -> map (\l1 -> (fst l1 ++ fst l2, Add (snd l1) (snd l2))) (minMaxAbsEliminator e1)) (minMaxAbsEliminator e2) in
  -- trace (show x) $
  -- trace (show (map (map (\es -> (expressionsGEZero (fst es), snd es :: E))) x)) $
  -- concat x


-- minMaxAbsEliminator2 :: E -> [([E],E)]
-- minMaxAbsEliminator2 (Min e1(Just True,Add (Lit 1) (Lit 7)) e2) = ([Sub e2 e1], e1) : ([Sub e1 e2], e2) : minMaxAbsEliminator e1 : minMaxAbsEliminator e2
-- minMaxAbsEliminator2 (Max e1 e2) = ([Sub e1 e2], e1) : ([Sub e2 e1], e2) : minMaxAbsEliminator e1 : minMaxAbsEliminator e2
-- minMaxAbsEliminator2 (Abs e)     = minMaxAbsEliminator $ Max e (Negate e)


-- say we have Min (e1, e2)
-- function would return [([Sub e2 e1], e1), ([Sub e1 e2], e2)]
-- This is only the case if e1 e2 are without Abs/Min/Max 

-- Next step, E -> MinMaxTree

-- In particular, we can take an E value and put all the abs on the top
