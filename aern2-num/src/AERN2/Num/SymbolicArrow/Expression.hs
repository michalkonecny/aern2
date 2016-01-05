{-# LANGUAGE Arrows, GeneralizedNewtypeDeriving, OverloadedStrings, TypeOperators, FlexibleContexts, TypeSynonymInstances, FlexibleInstances, Rank2Types #-}
module AERN2.Num.SymbolicArrow.Expression 
(RealExprA, RealPredA, RealExpr, RealPred, realExpr2arrow, realPred2arrow, var)
where

import Control.Arrow
import qualified Data.Map as Map

import AERN2.Num.Operations
import AERN2.Num.CauchyReal

data RealExpr' expr
    = Var VarName
    | RInt (Maybe String) Integer
    | RRat (Maybe String) Rational
    | RFunct (Maybe String) (forall to r. (RealExprA to r) => [r] `to` r) [expr]

data RealPred' pred
    = RRel (Maybe String) (forall to r. (RealPredA to r) => [r] `to` (EqCompareTypeA to r r)) [RealExpr]
    | BFunct (Maybe String) (forall to b. (BoolA to b) => [b] `to` b) [pred]

data RealExpr = RealExpr (RealExpr' RealExpr)
data RealPred = RealPred (RealPred' RealPred)

class
    (FieldA to r, HasCauchyRealsA to r, 
     CanAddMulDivScalarA to r Integer, 
     CanAddMulDivScalarA to r Rational)
    => 
    RealExprA to r 

class
    (RealExprA to r,
     OrderCompareTypeA to r r ~ EqCompareTypeA to r r)
    =>
    RealPredA to r

instance RealExprA (->) CauchyReal
instance RealPredA (->) CauchyReal

{- TODO: use this inside RealExpr nodes:

data RIR r = RIR_Real r | RIR_Int Integer | RIR_Rat Rational

rir2r :: (HasIntegersA to r, HasRationalsA to r) => RIR r `to` r
rir2r =
    proc x ->
        case x of
            RIR_Real r -> returnA -< r
            RIR_Int i -> convertA -< i  
            RIR_Rat q -> convertA -< q  
-}

newtype VarName = VarName String
    deriving (IsString, Eq, Ord, Show)

type VarMap = Map.Map VarName

var :: String -> RealExpr
var name = RealExpr (Var (VarName name))

integer2expr :: Integer -> RealExpr
integer2expr x = RealExpr (RInt Nothing x)

rational2expr :: Rational -> RealExpr
rational2expr x = RealExpr (RRat Nothing x)

cauchyReal2expr :: CauchyReal -> RealExpr
cauchyReal2expr x = RealExpr (RFunct Nothing (proc [] -> convertA -< x) [])

--integer2exprNamed :: String -> Integer -> RealExpr
--integer2exprNamed name x = RealExpr (RInt (Just name) x)
--
--rational2exprNamed :: String -> Rational -> RealExpr
--rational2exprNamed name x = RealExpr (RRat (Just name) x)
--
--cauchyReal2exprNamed :: String -> CauchyReal -> RealExpr
--cauchyReal2exprNamed name x = RealExpr (RFunct (Just name) (proc [] -> convertA -< x) [])

bool2pred :: Bool -> RealPred
bool2pred x = RealPred (BFunct Nothing (proc [] -> convertA -< x) [])

--bool2predNamed :: String -> Bool -> RealPred
--bool2predNamed name x = RealPred (BFunct (Just name) (proc [] -> convertA -< x) [])

unaryFn :: (Arrow to) => (a `to` a) -> ([a] `to` a)
unaryFn fn = proc [x] -> fn -< x

binaryFn :: (Arrow to) => ((a,a) `to` a) -> ([a] `to` a)
binaryFn pairFn = proc [x,y] -> pairFn -< (x,y)
        
binaryRel :: (Arrow to) => ((a,a) `to` b) -> ([a] `to` b)
binaryRel pairFn = proc [x,y] -> pairFn -< (x,y)
        

{- Conversion to an arrow-generic function -}


realExpr2arrow :: (RealExprA to r) => RealExpr -> ((VarMap r) `to` r)
realExpr2arrow (RealExpr expr) =
    case expr of
        Var name -> 
            proc varMap -> 
                returnA -< 
                    case Map.lookup name varMap of 
                        Just value -> value; 
                        _ -> error ("AERN2.Num.SymbolicArrow: " ++ show name ++ " not found")
        -- TODO: use RIR to avoid the conversion 
        RInt _maybeName n ->    
            proc _varMap -> convertA -< n
        RRat _maybeName n ->
            proc _varMap -> convertA -< n
        RFunct _maybeName fnA args ->
            proc varMap ->
                do
                argValues <- sequenceA argArrows -< varMap
                fnA -< argValues
            where
            argArrows = map realExpr2arrow args        

realPred2arrow :: 
    (RealPredA to r) => RealPred -> ((VarMap r) `to` (EqCompareTypeA to r r))
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

{- predicates -}

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

instance HasEqA (->) RealExpr RealExpr where
    type EqCompareTypeA (->) RealExpr RealExpr = RealPred
    equalToA (e1,e2) = RealPred (RRel (Just "==") (binaryRel equalToA) [e1,e2]) 

instance HasOrderA (->) RealExpr RealExpr where
    type OrderCompareTypeA (->) RealExpr RealExpr = RealPred
    lessThanA (e1,e2) = RealPred (RRel (Just "<") (binaryRel lessThanA) [e1,e2]) 
    leqA (e1,e2) = RealPred (RRel (Just "<=") (binaryRel leqA) [e1,e2]) 

instance HasEqA (->) RealExpr Integer where
    type EqCompareTypeA (->) RealExpr Integer = RealPred
    equalToA (e1,e2) = equalToA (e1, integer2expr e2) 

instance HasEqA (->) Integer RealExpr where
    type EqCompareTypeA (->) Integer RealExpr = RealPred
    equalToA (e1,e2) = equalToA (integer2expr e1, e2) 

instance HasOrderA (->) RealExpr Integer where
    type OrderCompareTypeA (->) RealExpr Integer = RealPred
    lessThanA (e1,e2) = lessThanA (e1, integer2expr e2) 
    leqA (e1,e2) = leqA (e1, integer2expr e2) 

instance HasOrderA (->) Integer RealExpr where
    type OrderCompareTypeA (->) Integer RealExpr = RealPred
    lessThanA (e1,e2) = lessThanA (integer2expr e1, e2) 
    leqA (e1,e2) = leqA (integer2expr e1, e2) 

instance HasEqA (->) RealExpr Rational where
    type EqCompareTypeA (->) RealExpr Rational = RealPred
    equalToA (e1,e2) = equalToA (e1, rational2expr e2) 

instance HasEqA (->) Rational RealExpr where
    type EqCompareTypeA (->) Rational RealExpr = RealPred
    equalToA (e1,e2) = equalToA (rational2expr e1, e2) 

instance HasOrderA (->) RealExpr Rational where
    type OrderCompareTypeA (->) RealExpr Rational = RealPred
    lessThanA (e1,e2) = lessThanA (e1, rational2expr e2) 
    leqA (e1,e2) = leqA (e1, rational2expr e2) 

instance HasOrderA (->) Rational RealExpr where
    type OrderCompareTypeA (->) Rational RealExpr = RealPred
    lessThanA (e1,e2) = lessThanA (rational2expr e1, e2) 
    leqA (e1,e2) = leqA (rational2expr e1, e2) 


{- numeric operations -}

instance RingA (->) RealExpr
instance FieldA (->) RealExpr

instance ConvertibleA (->) Integer RealExpr where
    convertA = integer2expr

instance ConvertibleA (->) Rational RealExpr where
    convertA = rational2expr

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

instance CanAddMulScalarA (->) RealExpr Integer
instance CanAddMulDivScalarA (->) RealExpr Integer

instance CanAddA (->) RealExpr Integer where
    type AddTypeA (->) RealExpr Integer = RealExpr
    addA (e1,e2) = RealExpr (RFunct (Just "+") (binaryFn addA) [e1,integer2expr e2])

instance CanAddA (->) Integer RealExpr where
    type AddTypeA (->) Integer RealExpr = RealExpr
    addA (e1,e2) = RealExpr (RFunct (Just "+") (binaryFn addA) [integer2expr e1,e2])

instance CanAddThisA (->) RealExpr Integer

instance CanSubA (->) RealExpr Integer where
    type SubTypeA (->) RealExpr Integer = RealExpr
    subA (e1,e2) = RealExpr (RFunct (Just "-") (binaryFn subA) [e1,integer2expr e2])

instance CanSubA (->) Integer RealExpr where
    type SubTypeA (->) Integer RealExpr = RealExpr
    subA (e1,e2) = RealExpr (RFunct (Just "-") (binaryFn subA) [integer2expr e1,e2])

instance CanSubThisA (->) RealExpr Integer

instance CanMulA (->) RealExpr Integer where
    type MulTypeA (->) RealExpr Integer = RealExpr
    mulA (e1,e2) = RealExpr (RFunct (Just "*") (binaryFn mulA) [e1,integer2expr e2])

instance CanMulA (->) Integer RealExpr where
    type MulTypeA (->) Integer RealExpr = RealExpr
    mulA (e1,e2) = RealExpr (RFunct (Just "*") (binaryFn mulA) [integer2expr e1,e2])

instance CanMulByA (->) RealExpr Integer

instance CanDivA (->) RealExpr Integer where
    type DivTypeA (->) RealExpr Integer = RealExpr
    divA (e1,e2) = RealExpr (RFunct (Just "/") (binaryFn divA) [e1,integer2expr e2])

instance CanDivA (->) Integer RealExpr where
    type DivTypeA (->) Integer RealExpr = RealExpr
    divA (e1,e2) = RealExpr (RFunct (Just "/") (binaryFn divA) [integer2expr e1,e2])

instance CanDivByA (->) RealExpr Integer

instance CanAddMulScalarA (->) RealExpr Rational
instance CanAddMulDivScalarA (->) RealExpr Rational

instance CanAddA (->) RealExpr Rational where
    type AddTypeA (->) RealExpr Rational = RealExpr
    addA (e1,e2) = RealExpr (RFunct (Just "+") (binaryFn addA) [e1,rational2expr e2])

instance CanAddA (->) Rational RealExpr where
    type AddTypeA (->) Rational RealExpr = RealExpr
    addA (e1,e2) = RealExpr (RFunct (Just "+") (binaryFn addA) [rational2expr e1,e2])

instance CanAddThisA (->) RealExpr Rational

instance CanSubA (->) RealExpr Rational where
    type SubTypeA (->) RealExpr Rational = RealExpr
    subA (e1,e2) = RealExpr (RFunct (Just "-") (binaryFn subA) [e1,rational2expr e2])

instance CanSubA (->) Rational RealExpr where
    type SubTypeA (->) Rational RealExpr = RealExpr
    subA (e1,e2) = RealExpr (RFunct (Just "-") (binaryFn subA) [rational2expr e1,e2])

instance CanSubThisA (->) RealExpr Rational

instance CanMulA (->) RealExpr Rational where
    type MulTypeA (->) RealExpr Rational = RealExpr
    mulA (e1,e2) = RealExpr (RFunct (Just "*") (binaryFn mulA) [e1,rational2expr e2])

instance CanMulA (->) Rational RealExpr where
    type MulTypeA (->) Rational RealExpr = RealExpr
    mulA (e1,e2) = RealExpr (RFunct (Just "*") (binaryFn mulA) [rational2expr e1,e2])

instance CanMulByA (->) RealExpr Rational

instance CanDivA (->) RealExpr Rational where
    type DivTypeA (->) RealExpr Rational = RealExpr
    divA (e1,e2) = RealExpr (RFunct (Just "/") (binaryFn divA) [e1,rational2expr e2])

instance CanDivA (->) Rational RealExpr where
    type DivTypeA (->) Rational RealExpr = RealExpr
    divA (e1,e2) = RealExpr (RFunct (Just "/") (binaryFn divA) [rational2expr e1,e2])

instance CanDivByA (->) RealExpr Rational

instance CanAddMulScalarA (->) RealExpr CauchyReal
instance CanAddMulDivScalarA (->) RealExpr CauchyReal

instance CanAddA (->) RealExpr CauchyReal where
    type AddTypeA (->) RealExpr CauchyReal = RealExpr
    addA (e1,e2) = RealExpr (RFunct (Just "+") (binaryFn addA) [e1,cauchyReal2expr e2])

instance CanAddA (->) CauchyReal RealExpr where
    type AddTypeA (->) CauchyReal RealExpr = RealExpr
    addA (e1,e2) = RealExpr (RFunct (Just "+") (binaryFn addA) [cauchyReal2expr e1,e2])

instance CanAddThisA (->) RealExpr CauchyReal

instance CanSubA (->) RealExpr CauchyReal where
    type SubTypeA (->) RealExpr CauchyReal = RealExpr
    subA (e1,e2) = RealExpr (RFunct (Just "-") (binaryFn subA) [e1,cauchyReal2expr e2])

instance CanSubA (->) CauchyReal RealExpr where
    type SubTypeA (->) CauchyReal RealExpr = RealExpr
    subA (e1,e2) = RealExpr (RFunct (Just "-") (binaryFn subA) [cauchyReal2expr e1,e2])

instance CanSubThisA (->) RealExpr CauchyReal

instance CanMulA (->) RealExpr CauchyReal where
    type MulTypeA (->) RealExpr CauchyReal = RealExpr
    mulA (e1,e2) = RealExpr (RFunct (Just "*") (binaryFn mulA) [e1,cauchyReal2expr e2])

instance CanMulA (->) CauchyReal RealExpr where
    type MulTypeA (->) CauchyReal RealExpr = RealExpr
    mulA (e1,e2) = RealExpr (RFunct (Just "*") (binaryFn mulA) [cauchyReal2expr e1,e2])

instance CanMulByA (->) RealExpr CauchyReal

instance CanDivA (->) RealExpr CauchyReal where
    type DivTypeA (->) RealExpr CauchyReal = RealExpr
    divA (e1,e2) = RealExpr (RFunct (Just "/") (binaryFn divA) [e1,cauchyReal2expr e2])

instance CanDivA (->) CauchyReal RealExpr where
    type DivTypeA (->) CauchyReal RealExpr = RealExpr
    divA (e1,e2) = RealExpr (RFunct (Just "/") (binaryFn divA) [cauchyReal2expr e1,e2])

instance CanDivByA (->) RealExpr CauchyReal
