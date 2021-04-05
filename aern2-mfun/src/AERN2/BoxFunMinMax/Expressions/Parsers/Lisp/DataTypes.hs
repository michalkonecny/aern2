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
import Prelude

-- A frame contains mappings from variable names to Lisp values.
type Frame = Map.Map String Expression

-- An Environment is a frame coupled with a parent environment.
data Environment = EmptyEnvironment
                 | Environment Frame Environment

-- The Expression data type defines the elements of the abstract syntax tree
-- and the runtime types manipulated by the Lisp system.
data Expression = Null
                | Number Double
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
showExpression (Application operator operands) = "#Application"
showExpression (Definition variable value) = "#Definition"
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

