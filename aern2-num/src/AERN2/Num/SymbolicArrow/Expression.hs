{-# LANGUAGE OverloadedStrings, FlexibleContexts, FlexibleInstances, Rank2Types, UndecidableInstances, ConstraintKinds #-}
module AERN2.Num.SymbolicArrow.Expression 
(ArrowReal, RealExprA, RealExpr, RealPred, realExpr2arrow, realPred2arrow, var)
where

import AERN2.Num.Operations
import qualified Prelude

import Control.Arrow
import qualified Data.Map as Map
import AERN2.Num.CauchyReal
import AERN2.Num.MPBall

data RealExpr' expr
    = Var VarName 
    | RInt (Maybe String) Integer
    | RRat (Maybe String) Rational
    | RFunct (Maybe String) (forall to r. (RealExprA to r) => [RIR r] `to` (RIR r)) [expr]


data RealPred' pred
    = RRel (Maybe String) (forall to r. (RealExprA to r) => [r] `to` (EqCompareTypeA to r r)) [RealExpr]
-- TODO: use RIR also in relations 
    | BFunct (Maybe String) (forall to b. (BoolA to b) => [b] `to` b) [pred]

data RealExpr = RealExpr (RealExpr' RealExpr)
data RealPred = RealPred (RealPred' RealPred)

type RealExprA =  ArrowReal

instance 
    (CanCombineCRwithA to r r, CanCombineCRwithA to r CauchyReal_) => 
    RealExprA to (AsCauchyReal r)

instance RealExprA (->) MPBall

instance ConvertibleA (->) CauchyReal MPBall where
    convertA =
        error "conversion from CauchyReal to MPBall not implemented"

var :: String -> RealExpr
var name = RealExpr (Var (VarName name))

integer2expr :: Integer -> RealExpr
integer2expr x = RealExpr (RInt Nothing x)

rational2expr :: Rational -> RealExpr
rational2expr x = RealExpr (RRat Nothing x)

cauchyReal2expr :: CauchyReal -> RealExpr
cauchyReal2expr x = 
    RealExpr $ 
        RFunct 
            Nothing 
            (proc [] -> do { xR <- convertA -< x; returnA -< RIR_Real xR }) 
            []

bool2pred :: Bool -> RealPred
bool2pred x = RealPred (BFunct Nothing (proc [] -> convertA -< x) [])

unaryFn :: (Arrow to) => (a `to` a) -> ([a] `to` a)
unaryFn fn = proc [x] -> fn -< x

binaryFn :: (Arrow to) => ((a,a) `to` a) -> ([a] `to` a)
binaryFn pairFn = proc [x,y] -> pairFn -< (x,y)
        
binaryRel :: (Arrow to) => ((a,a) `to` b) -> ([a] `to` b)
binaryRel pairFn = proc [x,y] -> pairFn -< (x,y)
        

{- Conversion to an arrow-generic function -}

realExpr2arrow :: (RealExprA to r) => RealExpr -> ((VarMap r) `to` r)
realExpr2arrow e = 
    realExpr2arrowRIR e >>> rir2r

realExpr2arrowRIR :: (RealExprA to r) => RealExpr -> ((VarMap r) `to` RIR r)
realExpr2arrowRIR (RealExpr expr) =
    case expr of
        Var name -> 
            proc varMap -> 
                returnA -< 
                    case Map.lookup name varMap of 
                        Just value -> RIR_Real value; 
                        _ -> error ("AERN2.Num.SymbolicArrow: " ++ show name ++ " not found")
        RInt _maybeName n ->    
            proc _varMap -> returnA -< RIR_Int n
        RRat _maybeName n ->
            proc _varMap -> returnA -< RIR_Rat n
        RFunct _maybeName fnA args ->
            proc varMap ->
                do
                argValues <- mergeInputsA argArrows -< varMap
                fnA -< argValues
            where
            argArrows = map realExpr2arrowRIR args        

data RIR r = RIR_Real r | RIR_Int Integer | RIR_Rat Rational

rir2r :: (HasIntegersA to r, HasRationalsA to r) => RIR r `to` r
rir2r =
    proc x ->
        case x of
            RIR_Real r -> returnA -< r
            RIR_Int i -> convertA -< i  
            RIR_Rat q -> convertA -< q  

negRIR :: 
    (RealExprA to r) =>
    [RIR r] `to` (RIR r)
negRIR =
    proc [ar] ->
        case ar of
            (RIR_Real a) -> do r <- negA -< a; returnA -< RIR_Real r
            (RIR_Int a) -> do r <- negA -< a; returnA -< RIR_Int r
            (RIR_Rat a) -> do r <- negA -< a; returnA -< RIR_Rat r

recipRIR :: 
    (RealExprA to r) =>
    [RIR r] `to` (RIR r)
recipRIR =
    proc [ar] ->
        case ar of
            (RIR_Real a) -> do r <- recipA -< a; returnA -< RIR_Real r
            (RIR_Int a) -> do r <- recipA -< a; returnA -< RIR_Rat r
            (RIR_Rat a) -> do r <- recipA -< a; returnA -< RIR_Rat r

powRIR :: 
    (RealExprA to r) =>
    Integer ->
    [RIR r] `to` (RIR r)
powRIR n =
    proc [ar] ->
        case ar of
            (RIR_Real a) -> do r <- powA -< (a,n); returnA -< RIR_Real r
            (RIR_Int a) -> do r <- powA -< (a,n); returnA -< RIR_Int r
            (RIR_Rat a) -> do r <- powA -< (a,n); returnA -< RIR_Rat r

unaryToR_RIR :: 
    (RealExprA to r) =>
    (r `to` r) ->
    [RIR r] `to` (RIR r)
unaryToR_RIR opR =
    proc [ar] ->
        case ar of
            (RIR_Real a) -> do r <- opR -< a; returnA -< RIR_Real r
            (RIR_Int a) -> do r <- (opR <<< convertA) -< a; returnA -< RIR_Real r
            (RIR_Rat a) -> do r <- (opR <<< convertA) -< a; returnA -< RIR_Real r

addRIR :: 
    (RealExprA to r) =>    
    [RIR r] `to` (RIR r)
addRIR =
    proc [ar,br] ->
        case (ar,br) of 
            (RIR_Int a, RIR_Int b) -> do r <- addA -< (a,b); returnA -< RIR_Int r 
            (RIR_Int a, RIR_Rat b) -> do r <- addA -< (a,b); returnA -< RIR_Rat r 
            (RIR_Rat a, RIR_Int b) -> do r <- addA -< (a,b); returnA -< RIR_Rat r 
            (RIR_Rat a, RIR_Rat b) -> do r <- addA -< (a,b); returnA -< RIR_Rat r 
            (RIR_Real a, RIR_Int b) -> do r <- addA -< (a,b); returnA -< RIR_Real r 
            (RIR_Real a, RIR_Rat b) -> do r <- addA -< (a,b); returnA -< RIR_Real r 
            (RIR_Int a, RIR_Real b) -> do r <- addA -< (a,b); returnA -< RIR_Real r 
            (RIR_Rat a, RIR_Real b) -> do r <- addA -< (a,b); returnA -< RIR_Real r 
            (RIR_Real a, RIR_Real b) -> do r <- addA -< (a,b); returnA -< RIR_Real r 

subRIR :: 
    (RealExprA to r) =>    
    [RIR r] `to` (RIR r)
subRIR =
    proc [ar,br] ->
        case (ar,br) of 
            (RIR_Int a, RIR_Int b) -> do r <- subA -< (a,b); returnA -< RIR_Int r 
            (RIR_Int a, RIR_Rat b) -> do r <- subA -< (a,b); returnA -< RIR_Rat r 
            (RIR_Rat a, RIR_Int b) -> do r <- subA -< (a,b); returnA -< RIR_Rat r 
            (RIR_Rat a, RIR_Rat b) -> do r <- subA -< (a,b); returnA -< RIR_Rat r 
            (RIR_Real a, RIR_Int b) -> do r <- subA -< (a,b); returnA -< RIR_Real r 
            (RIR_Real a, RIR_Rat b) -> do r <- subA -< (a,b); returnA -< RIR_Real r 
            (RIR_Int a, RIR_Real b) -> do r <- subA -< (a,b); returnA -< RIR_Real r 
            (RIR_Rat a, RIR_Real b) -> do r <- subA -< (a,b); returnA -< RIR_Real r 
            (RIR_Real a, RIR_Real b) -> do r <- subA -< (a,b); returnA -< RIR_Real r 

mulRIR :: 
    (RealExprA to r) =>    
    [RIR r] `to` (RIR r)
mulRIR =
    proc [ar,br] ->
        case (ar,br) of 
            (RIR_Int a, RIR_Int b) -> do r <- mulA -< (a,b); returnA -< RIR_Int r 
            (RIR_Int a, RIR_Rat b) -> do r <- mulA -< (a,b); returnA -< RIR_Rat r 
            (RIR_Rat a, RIR_Int b) -> do r <- mulA -< (a,b); returnA -< RIR_Rat r 
            (RIR_Rat a, RIR_Rat b) -> do r <- mulA -< (a,b); returnA -< RIR_Rat r 
            (RIR_Real a, RIR_Int b) -> do r <- mulA -< (a,b); returnA -< RIR_Real r 
            (RIR_Real a, RIR_Rat b) -> do r <- mulA -< (a,b); returnA -< RIR_Real r 
            (RIR_Int a, RIR_Real b) -> do r <- mulA -< (a,b); returnA -< RIR_Real r 
            (RIR_Rat a, RIR_Real b) -> do r <- mulA -< (a,b); returnA -< RIR_Real r 
            (RIR_Real a, RIR_Real b) -> do r <- mulA -< (a,b); returnA -< RIR_Real r 

divRIR :: 
    (RealExprA to r) =>    
    [RIR r] `to` (RIR r)
divRIR =
    proc [ar,br] ->
        case (ar,br) of 
            (RIR_Int a, RIR_Int b) -> do r <- divA -< (a,b); returnA -< RIR_Rat r 
            (RIR_Int a, RIR_Rat b) -> do r <- divA -< (a,b); returnA -< RIR_Rat r 
            (RIR_Rat a, RIR_Int b) -> do r <- divA -< (a,b); returnA -< RIR_Rat r 
            (RIR_Rat a, RIR_Rat b) -> do r <- divA -< (a,b); returnA -< RIR_Rat r 
            (RIR_Real a, RIR_Int b) -> do r <- divA -< (a,b); returnA -< RIR_Real r 
            (RIR_Real a, RIR_Rat b) -> do r <- divA -< (a,b); returnA -< RIR_Real r 
            (RIR_Int a, RIR_Real b) -> do r <- divA -< (a,b); returnA -< RIR_Real r 
            (RIR_Rat a, RIR_Real b) -> do r <- divA -< (a,b); returnA -< RIR_Real r 
            (RIR_Real a, RIR_Real b) -> do r <- divA -< (a,b); returnA -< RIR_Real r 

realPred2arrow :: 
    (RealExprA to r) => RealPred -> ((VarMap r) `to` (EqCompareTypeA to r r))
realPred2arrow (RealPred predicate) =
    case predicate of
        RRel _maybeName fnA args ->
            proc varMap ->
                do
                argValues <- mergeInputsA argArrows -< varMap
                fnA -< argValues 
            where
            argArrows = map realExpr2arrow args        
        BFunct _maybeName fnA args ->
            proc varMap ->
                do
                argValues <- mergeInputsA argArrows -< varMap
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


-- providing this only because Template Haskell translates (-x) to (Prelude.negate x)  
instance Num RealExpr where
    fromInteger = convert
    negate = negate
    (+) = (+)
    (*) = (*)
    abs = error "RealExpr Prelude.abs not implemented"
    signum = error "RealExpr Prelude.signum not implemented"

instance RingA (->) RealExpr
instance FieldA (->) RealExpr

instance ConvertibleA (->) Integer RealExpr where
    convertA = integer2expr

instance ConvertibleA (->) Rational RealExpr where
    convertA = rational2expr

instance CanNegA (->) RealExpr where
    negA e = RealExpr (RFunct (Just "neg") negRIR [e])

instance CanNegSameTypeA (->) RealExpr

instance CanRecipA (->) RealExpr where
    recipA e = RealExpr (RFunct (Just "recip") recipRIR [e])

instance CanRecipSameTypeA (->) RealExpr

instance CanAddA (->) RealExpr RealExpr where
    addA (e1,e2) = RealExpr (RFunct (Just "+") addRIR [e1,e2])

instance CanAddThisA (->) RealExpr RealExpr
instance CanAddSameTypeA (->) RealExpr

instance CanSubA (->) RealExpr RealExpr where
    subA (e1,e2) = RealExpr (RFunct (Just "-") subRIR [e1,e2])

instance CanSubThisA (->) RealExpr RealExpr
instance CanSubSameTypeA (->) RealExpr

instance CanMulA (->) RealExpr RealExpr where
    mulA (e1,e2) = RealExpr (RFunct (Just "*") mulRIR [e1,e2])

instance CanMulByA (->) RealExpr RealExpr
instance CanMulSameTypeA (->) RealExpr

instance CanPowA (->) RealExpr Integer where
    powA (e1,n) = RealExpr (RFunct (Just "^") (powRIR n) [e1])
instance CanPowByA (->) RealExpr Integer

instance CanDivA (->) RealExpr RealExpr where
    divA (e1,e2) = RealExpr (RFunct (Just "/") divRIR [e1,e2])

instance CanDivByA (->) RealExpr RealExpr
instance CanDivSameTypeA (->) RealExpr

instance CanAddMulScalarA (->) RealExpr Integer
instance CanAddMulDivScalarA (->) RealExpr Integer

instance CanAddA (->) RealExpr Integer where
    type AddTypeA (->) RealExpr Integer = RealExpr
    addA (e1,e2) = RealExpr (RFunct (Just "+") addRIR [e1, integer2expr e2])


instance CanAddA (->) Integer RealExpr where
    type AddTypeA (->) Integer RealExpr = RealExpr
    addA (e1,e2) = RealExpr (RFunct (Just "+") addRIR [integer2expr e1,e2])

instance CanAddThisA (->) RealExpr Integer

instance CanSubA (->) RealExpr Integer where
    type SubTypeA (->) RealExpr Integer = RealExpr
    subA (e1,e2) = RealExpr (RFunct (Just "-") subRIR [e1,integer2expr e2])

instance CanSubA (->) Integer RealExpr where
    type SubTypeA (->) Integer RealExpr = RealExpr
    subA (e1,e2) = RealExpr (RFunct (Just "-") subRIR [integer2expr e1,e2])

instance CanSubThisA (->) RealExpr Integer

instance CanMulA (->) RealExpr Integer where
    type MulTypeA (->) RealExpr Integer = RealExpr
    mulA (e1,e2) = RealExpr (RFunct (Just "*") mulRIR [e1,integer2expr e2])

instance CanMulA (->) Integer RealExpr where
    type MulTypeA (->) Integer RealExpr = RealExpr
    mulA (e1,e2) = RealExpr (RFunct (Just "*") mulRIR [integer2expr e1,e2])

instance CanMulByA (->) RealExpr Integer

instance CanDivA (->) RealExpr Integer where
    type DivTypeA (->) RealExpr Integer = RealExpr
    divA (e1,e2) = RealExpr (RFunct (Just "/") divRIR [e1,integer2expr e2])

instance CanDivA (->) Integer RealExpr where
    type DivTypeA (->) Integer RealExpr = RealExpr
    divA (e1,e2) = RealExpr (RFunct (Just "/") divRIR [integer2expr e1,e2])

instance CanDivByA (->) RealExpr Integer

instance CanAddMulScalarA (->) RealExpr Rational
instance CanAddMulDivScalarA (->) RealExpr Rational

instance CanAddA (->) RealExpr Rational where
    type AddTypeA (->) RealExpr Rational = RealExpr
    addA (e1,e2) = RealExpr (RFunct (Just "+") addRIR [e1,rational2expr e2])

instance CanAddA (->) Rational RealExpr where
    type AddTypeA (->) Rational RealExpr = RealExpr
    addA (e1,e2) = RealExpr (RFunct (Just "+") addRIR [rational2expr e1,e2])

instance CanAddThisA (->) RealExpr Rational

instance CanSubA (->) RealExpr Rational where
    type SubTypeA (->) RealExpr Rational = RealExpr
    subA (e1,e2) = RealExpr (RFunct (Just "-") subRIR [e1,rational2expr e2])

instance CanSubA (->) Rational RealExpr where
    type SubTypeA (->) Rational RealExpr = RealExpr
    subA (e1,e2) = RealExpr (RFunct (Just "-") subRIR [rational2expr e1,e2])

instance CanSubThisA (->) RealExpr Rational

instance CanMulA (->) RealExpr Rational where
    type MulTypeA (->) RealExpr Rational = RealExpr
    mulA (e1,e2) = RealExpr (RFunct (Just "*") mulRIR [e1,rational2expr e2])

instance CanMulA (->) Rational RealExpr where
    type MulTypeA (->) Rational RealExpr = RealExpr
    mulA (e1,e2) = RealExpr (RFunct (Just "*") mulRIR [rational2expr e1,e2])

instance CanMulByA (->) RealExpr Rational

instance CanDivA (->) RealExpr Rational where
    type DivTypeA (->) RealExpr Rational = RealExpr
    divA (e1,e2) = RealExpr (RFunct (Just "/") divRIR [e1,rational2expr e2])

instance CanDivA (->) Rational RealExpr where
    type DivTypeA (->) Rational RealExpr = RealExpr
    divA (e1,e2) = RealExpr (RFunct (Just "/") divRIR [rational2expr e1,e2])

instance CanDivByA (->) RealExpr Rational

instance CanAddMulScalarA (->) RealExpr CauchyReal
instance CanAddMulDivScalarA (->) RealExpr CauchyReal

instance CanAddA (->) RealExpr CauchyReal where
    type AddTypeA (->) RealExpr CauchyReal = RealExpr
    addA (e1,e2) = RealExpr (RFunct (Just "+") addRIR [e1,cauchyReal2expr e2])

instance CanAddA (->) CauchyReal RealExpr where
    type AddTypeA (->) CauchyReal RealExpr = RealExpr
    addA (e1,e2) = RealExpr (RFunct (Just "+") addRIR [cauchyReal2expr e1,e2])

instance CanAddThisA (->) RealExpr CauchyReal

instance CanSubA (->) RealExpr CauchyReal where
    type SubTypeA (->) RealExpr CauchyReal = RealExpr
    subA (e1,e2) = RealExpr (RFunct (Just "-") subRIR [e1,cauchyReal2expr e2])

instance CanSubA (->) CauchyReal RealExpr where
    type SubTypeA (->) CauchyReal RealExpr = RealExpr
    subA (e1,e2) = RealExpr (RFunct (Just "-") subRIR [cauchyReal2expr e1,e2])

instance CanSubThisA (->) RealExpr CauchyReal

instance CanMulA (->) RealExpr CauchyReal where
    type MulTypeA (->) RealExpr CauchyReal = RealExpr
    mulA (e1,e2) = RealExpr (RFunct (Just "*") mulRIR [e1,cauchyReal2expr e2])

instance CanMulA (->) CauchyReal RealExpr where
    type MulTypeA (->) CauchyReal RealExpr = RealExpr
    mulA (e1,e2) = RealExpr (RFunct (Just "*") mulRIR [cauchyReal2expr e1,e2])

instance CanMulByA (->) RealExpr CauchyReal

instance CanDivA (->) RealExpr CauchyReal where
    type DivTypeA (->) RealExpr CauchyReal = RealExpr
    divA (e1,e2) = RealExpr (RFunct (Just "/") divRIR [e1,cauchyReal2expr e2])

instance CanDivA (->) CauchyReal RealExpr where
    type DivTypeA (->) CauchyReal RealExpr = RealExpr
    divA (e1,e2) = RealExpr (RFunct (Just "/") divRIR [cauchyReal2expr e1,e2])

instance CanDivByA (->) RealExpr CauchyReal


instance CanSqrtA (->) RealExpr where
    sqrtA e1 = RealExpr (RFunct (Just "sqrt") (unaryToR_RIR sqrtA) [e1])

instance CanSqrtSameType RealExpr

instance CanExpA (->) RealExpr where
    expA e1 = RealExpr (RFunct (Just "exp") (unaryToR_RIR expA) [e1])

instance CanExpSameType RealExpr

instance CanSineCosineA (->) RealExpr where
    sinA e1 = RealExpr (RFunct (Just "sin") (unaryToR_RIR sinA) [e1])
    cosA e1 = RealExpr (RFunct (Just "cos") (unaryToR_RIR cosA) [e1])

instance CanSineCosineSameType RealExpr
