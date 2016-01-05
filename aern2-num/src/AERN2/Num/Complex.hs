{-# LANGUAGE Arrows, TypeSynonymInstances, FlexibleInstances, TypeOperators, ConstraintKinds, FlexibleContexts, UndecidableInstances, TemplateHaskell #-}
module AERN2.Num.Complex 
(
    Complex(..), complexI,
    complexCR2balls,
    showComplexCR
    ,
    HasComplexA, HasComplex,
    CanBeComplexA, complexA, complexNamedA, complexListA, complexListNamedA, CanBeComplex, complex, complexList
)
where

import AERN2.Num.Operations
import AERN2.Num.CauchyReal
import AERN2.Num.MPBall
import AERN2.Num.Accuracy
import AERN2.Num.SymbolicArrow

import Control.Arrow

data Complex r = r :+ r 

infixr 5 :+

complexI :: (HasIntegers r) => Complex r
complexI = (convert 0) :+ (convert 1)

complexCR2balls :: (Complex CauchyReal) -> Accuracy -> (MPBall, MPBall)
complexCR2balls (r :+ i) a = 
    (cauchyReal2ball r a, cauchyReal2ball i a)

showComplexCR :: Accuracy -> (Complex CauchyReal) -> String
showComplexCR a (r :+ i) = 
    "(" ++ show (cauchyReal2ball r a) ++ ":+" ++ show (cauchyReal2ball i a) ++ ")"

instance
    (ConvertibleA to r1 r2)
    => 
    ConvertibleA to (Complex r1) (Complex r2) 
    where
    convertA =
        proc (r :+ i) ->
            do
            r' <- convertA -< r  
            i' <- convertA -< i
            returnA -< (r' :+ i')

type HasComplexA r to = ConvertibleA to (Complex r)
type HasComplex r = HasComplexA r (->)

type CanBeComplexA r to a = ConvertibleA to a (Complex r)
complexA :: (CanBeComplexA r to a) => a `to` (Complex r)
complexA = convertA
complexNamedA :: (CanBeComplexA r to a) => String -> a `to` (Complex r)
complexNamedA = convertNamedA
complexListA :: (CanBeComplexA r to a) => [a] `to` [(Complex r)]
complexListA = convertListA
complexListNamedA :: (CanBeComplexA r to a) => String -> [a] `to` [(Complex r)]
complexListNamedA = convertListNamedA
type CanBeComplex r a = CanBeComplexA r (->) a
complex :: (CanBeComplex r a) => a -> (Complex r)
complex = convert
complexList :: (CanBeComplex r a) => [a] -> [(Complex r)]
complexList = convertList

-- | HasIntegers (Complex r), CanBeComplex Integer
instance (HasIntegersA to r) => ConvertibleA to Integer (Complex r) where
    convertA = toComplexA

-- | HasRationals (Complex r), CanBeComplex Rational
instance (HasRationalsA to r, HasIntegersA to r) => ConvertibleA to Rational (Complex r) where
    convertA = toComplexA

-- | HasCauchyReals (Complex r), CanBeComplex CauchyReal
instance (HasCauchyRealsA to r, HasIntegersA to r) => ConvertibleA to CauchyReal (Complex r) where
    convertA = toComplexA

toComplexA ::
    (ConvertibleA to a r, HasIntegersA to r)
    =>
    a `to` Complex r
toComplexA =
    proc x ->
        do
        x' <- convertA -< x  
        z <- convertA -< 0
        returnA -< (x' :+ z)

{- Comparison of complex numbers -}

instance (RealPredA to r) => HasEqA to (Complex r) (Complex r) where
    type EqCompareTypeA to (Complex r) (Complex r) = EqCompareTypeA to r r 
    equalToA =
        binaryRel $(predA [| let [r1,i1,r2,i2] = vars in r1 == r2 && i1 == i2|])
        
instance
    (RealPredA to r) =>
    HasOrderA to (Complex r) (Complex r) 
    where
    type OrderCompareTypeA to (Complex r) (Complex r) = OrderCompareTypeA to r r 
    lessThanA =
        binaryRel $(predA [|let [r1,i1,r2,i2] = vars in (r1 <= r2 && i1 <= i2) && (r1 /= r2 || i1 /= i2)|])
    leqA =
        binaryRel $(predA [|let [r1,i1,r2,i2] = vars in (r1 <= r2 && i1 <= i2)|])

instance 
    (RealPredA to r) =>
    HasEqA to (Complex r) Integer 
    where
    type EqCompareTypeA to (Complex r) Integer = EqCompareTypeA to (Complex r) (Complex r)
    equalToA = convertSecondA equalToA

instance 
    (RealPredA to r) =>
    HasEqA to Integer (Complex r) where
    type EqCompareTypeA to Integer (Complex r) = EqCompareTypeA to (Complex r) (Complex r)
    equalToA = convertFirstA equalToA

instance
    (RealPredA to r) =>
    HasOrderA to (Complex r) Integer 
    where
    type OrderCompareTypeA to (Complex r) Integer = OrderCompareTypeA to (Complex r) (Complex r)
    lessThanA = convertSecondA lessThanA
    leqA = convertSecondA leqA

instance
    (RealPredA to r) =>
    HasOrderA to Integer (Complex r) 
    where
    type OrderCompareTypeA to Integer (Complex r) = OrderCompareTypeA to (Complex r) (Complex r)
    lessThanA = convertFirstA lessThanA
    leqA = convertFirstA leqA

instance 
    (RealPredA to r) =>
    HasEqA to (Complex r) Rational 
    where
    type EqCompareTypeA to (Complex r) Rational = EqCompareTypeA to (Complex r) (Complex r)
    equalToA = convertSecondA equalToA

instance 
    (RealPredA to r) =>
    HasEqA to Rational (Complex r) where
    type EqCompareTypeA to Rational (Complex r) = EqCompareTypeA to (Complex r) (Complex r)
    equalToA = convertFirstA equalToA

instance
    (RealPredA to r) =>
    HasOrderA to (Complex r) Rational 
    where
    type OrderCompareTypeA to (Complex r) Rational = OrderCompareTypeA to (Complex r) (Complex r)
    lessThanA = convertSecondA lessThanA
    leqA = convertSecondA leqA

instance
    (RealPredA to r) =>
    HasOrderA to Rational (Complex r) 
    where
    type OrderCompareTypeA to Rational (Complex r) = OrderCompareTypeA to (Complex r) (Complex r)
    lessThanA = convertFirstA lessThanA
    leqA = convertFirstA leqA

instance 
    (RealPredA to r) =>
    HasEqA to (Complex r) CauchyReal 
    where
    type EqCompareTypeA to (Complex r) CauchyReal = EqCompareTypeA to (Complex r) (Complex r)
    equalToA = convertSecondA equalToA

instance 
    (RealPredA to r) =>
    HasEqA to CauchyReal (Complex r) where
    type EqCompareTypeA to CauchyReal (Complex r) = EqCompareTypeA to (Complex r) (Complex r)
    equalToA = convertFirstA equalToA

instance
    (RealPredA to r) =>
    HasOrderA to (Complex r) CauchyReal 
    where
    type OrderCompareTypeA to (Complex r) CauchyReal = OrderCompareTypeA to (Complex r) (Complex r)
    lessThanA = convertSecondA lessThanA
    leqA = convertSecondA leqA

instance
    (RealPredA to r) =>
    HasOrderA to CauchyReal (Complex r) 
    where
    type OrderCompareTypeA to CauchyReal (Complex r) = OrderCompareTypeA to (Complex r) (Complex r)
    lessThanA = convertFirstA lessThanA
    leqA = convertFirstA leqA


{- Operations among (Complex r) numbers -}

instance (RealExprA to r) => CanNegA to (Complex r) where
    type NegTypeA to (Complex r) = Complex r
    negA =
        unaryOp ($(exprA[|let [r,_i]=vars in neg r|]),  
                 $(exprA[|let [_r,i]=vars in neg i|]))
        -- TODO add support for tuples of expresions in exprA

instance (RealExprA to r) => CanNegSameTypeA to (Complex r)

instance (CanSqrtSameTypeA to r, RealExprA to r) => CanAbsA to (Complex r) where
    type AbsTypeA to (Complex r) = r
    absA =
        proc (r :+ i) ->
             $(exprA[| let [r,i]=vars in sqrt (r*r + i*i) |]) -< (r,i)

instance (RealExprA to r) => CanRecipA to (Complex r) where
    recipA = proc x -> divA -< (1,x)

instance (RealExprA to r) => CanRecipSameTypeA to (Complex r)

instance (RealExprA to r) => CanAddA to (Complex r) (Complex r) where
    addA =
        binaryOp ($(exprA[|let [r1,_i1,r2,_i2]=vars in r1 + r2 |]), 
                  $(exprA[|let [_r1,i1,_r2,i2]=vars in i1 + i2 |])) 

instance (RealExprA to r) => CanAddThisA to (Complex r) (Complex r)
instance (RealExprA to r) => CanAddSameTypeA to (Complex r)

instance (RealExprA to r) => (CanSubA to (Complex r) (Complex r))  
instance (RealExprA to r) => CanSubThisA to (Complex r) (Complex r)
instance (RealExprA to r) => CanSubSameTypeA to (Complex r)

instance (RealExprA to r) => CanMulA to (Complex r) (Complex r) where
    mulA =
        binaryOp ($(exprA[|let [r1,i1,r2,i2]=vars in r1 * r2 - i1 * i2 |]), 
                  $(exprA[|let [r1,i1,r2,i2]=vars in r1 * i2 + r2 * i1|])) 

instance (RealExprA to r) => CanMulByA to (Complex r) (Complex r)
instance (RealExprA to r) => CanMulSameTypeA to (Complex r)

instance (RealExprA to r) => CanDivA to (Complex r) (Complex r) where
    divA =
        binaryOp ($(exprA[|let [r1,i1,r2,i2]=vars in (r1 * r2 + i1 * i2)/(r2*r2 + i2 * i2) |]), 
                  $(exprA[|let [r1,i1,r2,i2]=vars in (r2 * i1 - r1 * i2)/(r2*r2 + i2 * i2) |])) 
        
instance (RealExprA to r) => CanDivByA to (Complex r) (Complex r)

instance (RealExprA to r) => CanDivSameTypeA to (Complex r)

instance (RealPredA to r) => RingA to (Complex r)
instance (RealPredA to r) => FieldA to (Complex r)

{- (Complex r)-Integer operations -}

instance (RealExprA to r) => CanAddA to Integer (Complex r) where
    type AddTypeA to Integer (Complex r) = (Complex r)
    addA = convertFirstRealOnlyA addA 


instance (RealExprA to r) => CanSubA to Integer (Complex r)


instance (RealExprA to r) => CanAddA to (Complex r) Integer where
    type AddTypeA to (Complex r) Integer = (Complex r)
    addA = convertSecondRealOnlyA addA 


instance (RealExprA to r) => CanAddThisA to (Complex r) Integer

instance (RealExprA to r) => CanSubA to (Complex r) Integer where
    type SubTypeA to (Complex r) Integer = (Complex r)
    subA = convertSecondRealOnlyA subA 


instance (RealExprA to r) => CanSubThisA to (Complex r) Integer

instance (RealExprA to r) => CanMulA to Integer (Complex r) where
    type MulTypeA to Integer (Complex r) = (Complex r)
    mulA = convertFirstA mulA 

instance (RealExprA to r) => CanMulA to (Complex r) Integer where
    type MulTypeA to (Complex r) Integer = (Complex r)
    mulA = convertSecondA mulA 

instance (RealExprA to r) => CanMulByA to (Complex r) Integer

instance (RealExprA to r) => CanDivA to Integer (Complex r) where
    type DivTypeA to Integer (Complex r) = (Complex r)
    divA = convertFirstA divA

instance (RealExprA to r) => CanDivA to (Complex r) Integer where
    type DivTypeA to (Complex r) Integer = (Complex r)
    divA = convertSecondA divA -- (r :+ i, a) = r / a :+ i / a 

instance (RealExprA to r) => CanDivByA to (Complex r) Integer


{- (Complex r)-Rational operations -}

instance (RealExprA to r) => CanAddA to Rational (Complex r) where
    type AddTypeA to Rational (Complex r) = (Complex r)
    addA = convertFirstRealOnlyA addA 


instance (RealExprA to r) => CanSubA to Rational (Complex r)


instance (RealExprA to r) => CanAddA to (Complex r) Rational where
    type AddTypeA to (Complex r) Rational = (Complex r)
    addA = convertSecondRealOnlyA addA 


instance (RealExprA to r) => CanAddThisA to (Complex r) Rational

instance (RealExprA to r) => CanSubA to (Complex r) Rational where
    type SubTypeA to (Complex r) Rational = (Complex r)
    subA = convertSecondRealOnlyA subA 


instance (RealExprA to r) => CanSubThisA to (Complex r) Rational

instance (RealExprA to r) => CanMulA to Rational (Complex r) where
    type MulTypeA to Rational (Complex r) = (Complex r)
    mulA = convertFirstA mulA 

instance (RealExprA to r) => CanMulA to (Complex r) Rational where
    type MulTypeA to (Complex r) Rational = (Complex r)
    mulA = convertSecondA mulA 

instance (RealExprA to r) => CanMulByA to (Complex r) Rational

instance (RealExprA to r) => CanDivA to Rational (Complex r) where
    type DivTypeA to Rational (Complex r) = (Complex r)
    divA = convertFirstA divA

instance (RealExprA to r) => CanDivA to (Complex r) Rational where
    type DivTypeA to (Complex r) Rational = (Complex r)
    divA = convertSecondA divA -- (r :+ i, a) = r / a :+ i / a 

instance (RealExprA to r) => CanDivByA to (Complex r) Rational


{- (Complex r)-CauchyReal operations -}

instance (RealExprA to r) => CanAddA to CauchyReal (Complex r) where
    type AddTypeA to CauchyReal (Complex r) = (Complex r)
    addA = convertFirstRealOnlyA addA 


instance (RealExprA to r) => CanSubA to CauchyReal (Complex r)


instance (RealExprA to r) => CanAddA to (Complex r) CauchyReal where
    type AddTypeA to (Complex r) CauchyReal = (Complex r)
    addA = convertSecondRealOnlyA addA 


instance (RealExprA to r) => CanAddThisA to (Complex r) CauchyReal

instance (RealExprA to r) => CanSubA to (Complex r) CauchyReal where
    type SubTypeA to (Complex r) CauchyReal = (Complex r)
    subA = convertSecondRealOnlyA subA 


instance (RealExprA to r) => CanSubThisA to (Complex r) CauchyReal

instance (RealExprA to r) => CanMulA to CauchyReal (Complex r) where
    type MulTypeA to CauchyReal (Complex r) = (Complex r)
    mulA = convertFirstA mulA 

instance (RealExprA to r) => CanMulA to (Complex r) CauchyReal where
    type MulTypeA to (Complex r) CauchyReal = (Complex r)
    mulA = convertSecondA mulA 

instance (RealExprA to r) => CanMulByA to (Complex r) CauchyReal

instance (RealExprA to r) => CanDivA to CauchyReal (Complex r) where
    type DivTypeA to CauchyReal (Complex r) = (Complex r)
    divA = convertFirstA divA

instance (RealExprA to r) => CanDivA to (Complex r) CauchyReal where
    type DivTypeA to (Complex r) CauchyReal = (Complex r)
    divA = convertSecondA divA -- (r :+ i, a) = r / a :+ i / a 

instance (RealExprA to r) => CanDivByA to (Complex r) CauchyReal

{- Selected complex functions -}


instance (RealExprA to r) => CanSqrtA to (Complex r) where
    sqrtA = error "Complex sqrt not implemented yet"

instance (RealExprA to r) => CanSqrtSameTypeA to (Complex r)

instance (RealExprA to r) => CanExpA to (Complex r) where
    expA =
        unaryOp ($(exprA[|let [r1,i1]=vars in (exp r1) * (cos i1)|]),
                 $(exprA[|let [r1,i1]=vars in (exp r1) * (sin i1)|]))
--     (r :+ i) =
--        (exp r :+ convert 0) * (cos i :+ sin i)

instance (RealExprA to r) => CanExpSameTypeA to (Complex r)
    
instance (RealExprA to r) => CanSineCosineA to (Complex r) where
    sinA = error "(Complex r) sin not implemented yet"
    cosA = error "(Complex r) cos not implemented yet"

instance (RealExprA to r) => CanSineCosineSameTypeA to (Complex r)

{- auxiliary function -}

convertSecondA ::
    (Arrow to, ConvertibleA to a r) =>
    ((r,r) `to` b) -> ((r,a) `to` b) 
convertSecondA opA =
    proc (x,yI) ->
        do
        y <- convertA -< yI
        opA -< (x,y)

convertFirstA ::
    (Arrow to, ConvertibleA to a r) =>
    ((r,r) `to` b) -> ((a,r) `to` b) 
convertFirstA opA =
    proc (xI,y) ->
        do
        x <- convertA -< xI
        opA -< (x,y)


convertSecondRealOnlyA ::
    (Arrow to, ConvertibleA to a r) =>
    ((r,r) `to` r) -> ((Complex r,a) `to` Complex r) 
convertSecondRealOnlyA opA =
    proc (r :+ i,yI) ->
        do
        y <- convertA -< yI
        r' <- opA -< (r,y)
        returnA -< (r' :+ i)

convertFirstRealOnlyA ::
    (Arrow to, ConvertibleA to a r) =>
    ((r,r) `to` r) -> ((a,Complex r) `to` Complex r) 
convertFirstRealOnlyA opA =
    proc (xI,r :+ i) ->
        do
        x <- convertA -< xI
        r' <- opA -< (x,r)
        returnA -< (r' :+ i)

--flipA ::
--    (Arrow to) =>
--    ((a,b) `to` c) -> ((b,a) `to` c) 
--flipA opA =
--    proc (x,y) -> opA -< (y,x) 

binaryRel ::
    (Arrow to) => 
    ((r,r,r,r) `to` a) -> ((Complex r, Complex r) `to` a)
binaryRel tupleA =
    proc (r1 :+ i1, r2 :+ i2) -> tupleA -< (r1,i1,r2,i2)

binaryOp ::
    (Arrow to) => 
    ((r,r,r,r) `to` r, (r,r,r,r) `to` r) -> ((Complex r, Complex r) `to` (Complex r))
binaryOp (realA, imagA) =
    proc (r1 :+ i1, r2 :+ i2) ->
        do
        r <- realA  -< (r1,i1,r2,i2)
        i <- imagA  -< (r1,i1,r2,i2)
        returnA -< (r :+ i)

unaryOp ::
    (Arrow to) => 
    ((r,r) `to` r, (r,r) `to` r) -> ((Complex r) `to` (Complex r))
unaryOp (realA, imagA) =
    proc (r1 :+ i1) ->
        do
        r <- realA  -< (r1,i1)
        i <- imagA  -< (r1,i1)
        returnA -< (r :+ i)

