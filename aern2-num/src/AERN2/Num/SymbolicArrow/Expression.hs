{-# LANGUAGE Arrows, GeneralizedNewtypeDeriving, OverloadedStrings, TypeOperators, FlexibleContexts, TypeSynonymInstances, FlexibleInstances, Rank2Types #-}
module AERN2.Num.SymbolicArrow.Expression where

import Control.Arrow
import qualified Data.Map as Map

import AERN2.Num.Operations
import AERN2.Num.CauchyReal

data RealExpr' expr
    = Var VarName
    | RFunct (Maybe String) (forall to r. (FieldA to r, HasCauchyRealsA to r) => [r] `to` r) [expr]

data RealPred' pred
    = RRel (Maybe String) (forall to r b. (FieldA to r, EqCompareTypeA to r r ~ b, OrderCompareTypeA to r r ~ b) => [r] `to` b) [RealExpr]
    | BFunct (Maybe String) (forall to b. (BoolA to b) => [b] `to` b) [pred]

data RealExpr = RealExpr (RealExpr' RealExpr)
data RealPred = RealPred (RealPred' RealPred)


newtype VarName = VarName String
    deriving (IsString, Eq, Ord, Show)

type VarMap = Map.Map VarName

var :: String -> RealExpr
var name = RealExpr (Var (VarName name))

integer2expr :: Integer -> RealExpr
integer2expr x = RealExpr (RFunct Nothing (proc [] -> convertA -< x) [])

rational2expr :: Rational -> RealExpr
rational2expr x = RealExpr (RFunct Nothing (proc [] -> convertA -< x) [])

cauchyReal2expr :: CauchyReal -> RealExpr
cauchyReal2expr x = RealExpr (RFunct Nothing (proc [] -> convertA -< x) [])

integer2exprNamed :: String -> Integer -> RealExpr
integer2exprNamed name x = RealExpr (RFunct (Just name) (proc [] -> convertA -< x) [])

rational2exprNamed :: String -> Rational -> RealExpr
rational2exprNamed name x = RealExpr (RFunct (Just name) (proc [] -> convertA -< x) [])

cauchyReal2exprNamed :: String -> CauchyReal -> RealExpr
cauchyReal2exprNamed name x = RealExpr (RFunct (Just name) (proc [] -> convertA -< x) [])

bool2pred :: Bool -> RealPred
bool2pred x = RealPred (BFunct Nothing (proc [] -> convertA -< x) [])

bool2predNamed :: String -> Bool -> RealPred
bool2predNamed name x = RealPred (BFunct (Just name) (proc [] -> convertA -< x) [])

unaryFn :: (Arrow to) => (a `to` a) -> ([a] `to` a)
unaryFn fn = proc [x] -> fn -< x

binaryFn :: (Arrow to) => ((a,a) `to` a) -> ([a] `to` a)
binaryFn pairFn = proc [x,y] -> pairFn -< (x,y)
        
binaryRel :: (Arrow to) => ((a,a) `to` b) -> ([a] `to` b)
binaryRel pairFn = proc [x,y] -> pairFn -< (x,y)
        

{- Conversion to an arrow-generic function -}

realExpr2arrow :: (FieldA to r, HasCauchyRealsA to r) => RealExpr -> ((VarMap r) `to` r)
realExpr2arrow (RealExpr expr) =
    case expr of
        Var name -> 
            proc varMap -> 
                returnA -< 
                    case Map.lookup name varMap of 
                        Just value -> value; 
                        _ -> error ("AERN2.Num.SymbolicArrow: " ++ show name ++ " not found")
        RFunct _maybeName fnA args ->
            proc varMap ->
                do
                argValues <- sequenceA argArrows -< varMap
                fnA -< argValues
            where
            argArrows = map realExpr2arrow args        

realPred2arrow :: 
    (FieldA to r, HasCauchyRealsA to r, 
     EqCompareTypeA to r r ~ b, OrderCompareTypeA to r r ~ b, BoolA to b) 
    => 
    RealPred -> ((VarMap r) `to` b)
realPred2arrow (RealPred predicate) =
    case predicate of
        RRel _maybeName fnA args ->
            proc varMap ->
                do
                argValues <- sequenceA argArrows -< varMap
                fnA -< argValues 
            where
            argArrows = map realExpr2arrow args        
        BFunct _maybeName fnA args ->
            proc varMap ->
                do
                argValues <- sequenceA argArrows -< varMap
                fnA -< argValues
            where
            argArrows = map realPred2arrow args        
    
{- Operation instances to conveniently build expressions -}

instance BoolA (->) RealPred

instance ConvertibleA (->) Bool RealPred where
    convertA = bool2pred

instance CanNegA (->) RealPred where
    negA e = RealPred (BFunct (Just "not") (unaryFn negA) [e])

instance CanNegSameTypeA (->) RealPred

instance CanAndOrA (->) RealPred RealPred where
    and2A (e1,e2) = RealPred (BFunct (Just "&&") (binaryFn and2A) [e1,e2])
    or2A (e1,e2) = RealPred (BFunct (Just "&&") (binaryFn or2A) [e1,e2])

instance CanAndOrSameTypeA (->) RealPred

instance RingA (->) RealExpr
instance FieldA (->) RealExpr

instance ConvertibleA (->) Integer RealExpr where
    convertA = integer2expr

instance ConvertibleA (->) Rational RealExpr where
    convertA = rational2expr

instance HasEqA (->) RealExpr RealExpr where
    type EqCompareTypeA (->) RealExpr RealExpr = RealPred
    equalToA (e1,e2) = RealPred (RRel (Just "==") (binaryRel equalToA) [e1,e2]) 

instance HasOrderA (->) RealExpr RealExpr where
    type OrderCompareTypeA (->) RealExpr RealExpr = RealPred
    lessThanA (e1,e2) = RealPred (RRel (Just "<") (binaryRel lessThanA) [e1,e2]) 
    leqA (e1,e2) = RealPred (RRel (Just "<=") (binaryRel leqA) [e1,e2]) 

instance CanNegA (->) RealExpr where
    negA e = RealExpr (RFunct (Just "neg") (unaryFn negA) [e])

instance CanNegSameTypeA (->) RealExpr

instance CanRecipA (->) RealExpr where
    recipA e = RealExpr (RFunct (Just "recip") (unaryFn recipA) [e])

instance CanRecipSameTypeA (->) RealExpr

instance CanAddA (->) RealExpr RealExpr where
    addA (e1,e2) = RealExpr (RFunct (Just "+") (binaryFn addA) [e1,e2])

instance CanAddThisA (->) RealExpr RealExpr
instance CanAddSameTypeA (->) RealExpr

instance CanSubA (->) RealExpr RealExpr where
    subA (e1,e2) = RealExpr (RFunct (Just "-") (binaryFn subA) [e1,e2])

instance CanSubThisA (->) RealExpr RealExpr
instance CanSubSameTypeA (->) RealExpr

instance CanMulA (->) RealExpr RealExpr where
    mulA (e1,e2) = RealExpr (RFunct (Just "*") (binaryFn mulA) [e1,e2])

instance CanMulByA (->) RealExpr RealExpr
instance CanMulSameTypeA (->) RealExpr

instance CanDivA (->) RealExpr RealExpr where
    divA (e1,e2) = RealExpr (RFunct (Just "/") (binaryFn divA) [e1,e2])

instance CanDivByA (->) RealExpr RealExpr
instance CanDivSameTypeA (->) RealExpr


{- TODO
instance CanAddMulDivScalarA (->) RealExpr Integer
instance CanAddMulDivScalarA (->) RealExpr Rational
-}