module AERN2.BoxFunMinMax.Expressions.Parsers.Lisp.DataTypes
( Frame
, Environment(..)
, Expression(..)
, addBinding
, lookupValue
, extendEnvironment
, pairToList
) where
import qualified Data.Map as Map
import qualified Data.List as List
import Prelude

-- A frame contains mappings from variable names to Lisp values.
type Frame = Map.Map String Expression

-- An Environment is a frame coupled with a parent environment.
data Environment = EmptyEnvironment
                 | Environment Frame Environment

-- The Expression data type defines the elements of the abstract syntax tree
-- and the runtime types manipulated by the Lisp system.
data Expression = Null
                | Number Rational
                | Boolean Bool
                | Variable String
                | Pair Expression Expression
                | Exception String
                | Lambda [Expression] Expression
                | PrimitiveProcedure ([Expression] -> Expression)
                | Application Expression [Expression]
                | Definition Expression Expression
                | If Expression Expression Expression
                | Cond [(Expression, Expression)]

instance Show Expression where
  show = showExpression

instance Eq Expression where
  x == y = eqExpression x y

eqExpression :: Expression -> Expression -> Bool
eqExpression (Pair x1 x2) (Pair y1 y2) = eqExpression x1 y1 && eqExpression x2 y2
eqExpression (Lambda x1s x2) (Lambda y1s y2) = and (List.zipWith eqExpression x1s y1s) && eqExpression x2 y2
eqExpression (Application x1 y1s) (Application x2 y2s) = eqExpression x1 x2 && and (List.zipWith eqExpression y1s y2s)
eqExpression (Definition x1 y1) (Definition x2 y2) = eqExpression x1 y1 && eqExpression x2 y2
eqExpression (If x1 y1 z1) (If x2 y2 z2) = eqExpression x1 y1 && eqExpression x2 y2 && eqExpression z1 z2
eqExpression Null Null = True
eqExpression Null _ = False
eqExpression _ Null = False
eqExpression (Number x) (Number y) = x == y
eqExpression (Boolean x) (Boolean y) = x == y
eqExpression (Variable x) (Variable y) = x == y
eqExpression (Exception x) (Exception y) = x == y
eqExpression _ _ = False
-- A function that recursively converts a Lisp Expression to a
-- String representation.
showExpression :: Expression -> String
showExpression (Null) = "null"
showExpression (Number number) = show number
showExpression (Boolean bool)
  | bool == True = "#t"
  | otherwise    = "#f"
showExpression (Variable variable) = variable
showExpression (Exception message) = "#Exception: " ++ "'" ++ message ++ "'"
showExpression pair@(Pair first second)
  | isList pair = "(" ++ (showPairList pair) ++ ")"
  | otherwise = "(" ++ (show first) ++ " . " ++ (show second) ++ ")"
showExpression (Lambda parameters body) = "#CompoundProcedure"
showExpression (PrimitiveProcedure _) = "#PrimitiveProcedure"
showExpression (Application operator operands) = "#Application " ++ show operator ++ " " ++ show operands
showExpression (Definition variable value) = "#Definition " ++ show variable ++ " " ++ show value
showExpression _ = "#Unknown"

showPairList :: Expression -> String
showPairList Null = ""
showPairList (Pair first (Null)) = (show first)
showPairList (Pair first second) = (show first) ++ " " ++ (showPairList second)

-- Helper functions for environment manipulation.
addBinding :: Environment -> String -> Expression -> Environment
addBinding EmptyEnvironment _ _ = EmptyEnvironment
addBinding (Environment frame parent) name value = Environment newFrame parent
  where newFrame = Map.insert name value frame

lookupValue :: Environment -> String -> Expression
lookupValue EmptyEnvironment variable = Exception ("Binding for " ++ variable ++ " not found.")
lookupValue (Environment frame parent) variable =
  case value of
    Just result -> result
    Nothing     -> lookupValue parent variable
  where value = Map.lookup variable frame

extendEnvironment :: Environment -> [Expression] -> [Expression] -> Environment
extendEnvironment environment parameters arguments =
  let params = map show parameters
  in Environment (Map.fromList (zip params arguments)) environment

-- Helper functions for pair manipulation.
pairToList :: Expression -> [Expression]
pairToList Null = []
pairToList (Pair first rest) = first : pairToList rest

isList :: Expression -> Bool
isList Null = True
isList (Pair _ second) = isList second
isList _ = False

