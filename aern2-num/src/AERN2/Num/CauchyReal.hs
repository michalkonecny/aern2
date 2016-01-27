{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, ConstraintKinds #-}
module AERN2.Num.CauchyReal
(
    ArrowReal,
    CauchyReal_, CauchyReal,  
    showCauchyReal,
    mapCauchyRealUnsafe,
    cauchyReal2ball,
    cauchyRealName,
    HasCauchyRealsA, HasCauchyReals,
    CanBeCauchyRealA, cauchyRealA, cauchyRealNamedA, cauchyRealsA, cauchyRealsNamedA, CanBeCauchyReal, cauchyReal, cauchyReals,
    integer2CauchyReal, rational2CauchyReal,
    convergent2CauchyReal,
    pi,
    module AERN2.Num.CauchyRealA
)
where

import AERN2.Num.Operations
import qualified Prelude

import Control.Arrow
import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)

import AERN2.Num.Accuracy
import AERN2.Num.MPBall
import AERN2.Num.CauchyRealA
import AERN2.Num.IntegerRational ()

--import Debug.Trace (trace)
--
--shouldTrace :: Bool
--shouldTrace = False
----shouldTrace = True
--
--maybeTrace :: String -> a -> a
--maybeTrace 
--    | shouldTrace = trace
--    | otherwise = const id


class
    (FieldA to r,
     HasCauchyRealsA to r,
     CanSqrtSameTypeA to r, CanExpSameTypeA to r, CanSineCosineSameTypeA to r, 
     CanAddMulDivScalarA to r Integer, 
     CanAddMulDivScalarA to r Rational,
     CanAddMulDivScalarA to r CauchyReal,
     CanSubA to Integer r,  SubTypeA to Integer r ~ r,
     CanSubA to Rational r,  SubTypeA to Rational r ~ r, 
     CanSubA to CauchyReal r,  SubTypeA to CauchyReal r ~ r,
     CanDivA to Integer r,  DivTypeA to Integer r ~ r,
     CanDivA to Rational r,  DivTypeA to Rational r ~ r, 
     CanDivA to CauchyReal r,  DivTypeA to CauchyReal r ~ r,
     OrderCompareTypeA to r r ~ EqCompareTypeA to r r
     )
    => 
    ArrowReal to r 

instance 
--    (ArrowPrecisionPolicy to) => 
    ArrowReal (WithPrecisionPolicy (->)) MPBall

type CauchyReal = AsCauchyReal CauchyReal_

instance Show CauchyReal where
        show x = show $ cauchyReal2ball x (bits 53)

data CauchyReal_ = 
    CauchyReal_ { cr_name :: Maybe String, cr_seq :: Accuracy -> MPBall } 

instance 
    (CanCombineCRwithA to r r, CanCombineCRwithA to r CauchyReal_) => 
    ArrowReal to (AsCauchyReal r)


{-|
    Construct a CauchyReal_ with unsafe memoization, inspired by
    https://hackage.haskell.org/package/ireal-0.2.3/docs/src/Data-Number-IReal-UnsafeMemo.html#unsafeMemo,
    which, in turn, is inspired by Lennart Augustsson's uglymemo.
    
    For the sake of efficiency, cr_ should be used in place of the CauchyReal_ constructor.
-}
cr_ :: Maybe String -> (Accuracy -> MPBall) -> CauchyReal_ 
cr_ name sq = CauchyReal_ name unsafeMemo
    where
    unsafeMemo = unsafePerformIO . unsafePerformIO memoIO
    memoIO =
        do
        cacheVar <- newMVar Nothing
--        putStrLn "new cr_"
        return $ useMVar cacheVar
        where
        useMVar cacheVar ac =
            do
            maybeCache <- readMVar cacheVar
            case maybeCache of
                Just (acC, bC) | acC >= ac ->
                    do
--                    putStrLn $ "cr_: using cache: ac = " ++ show ac ++ "; acC = " ++ show acC ++ "; bC = " ++ show bC ++ "; prec = " ++ show (getPrecision $ setPrecisionMatchAccuracy ac bC)
                    return $ setPrecisionMatchAccuracy ac bC
                _ -> 
                    do
                    modifyMVar_ cacheVar (const (return (Just (getAccuracy b, b))))
--                    putStrLn $ "cr_: amending cache: ac = " ++ show ac ++ "; b = " ++ show b
                    return b
                    where
                    b = sq ac
            

cauchyReal2ball :: CauchyReal -> Accuracy -> MPBall
cauchyReal2ball = cr_seq . unAsCauchyReal

cauchyRealName :: CauchyReal -> Maybe String
cauchyRealName = cr_name . unAsCauchyReal

showCauchyReal :: Accuracy -> CauchyReal -> String
showCauchyReal a r = show (cauchyReal2ball r a)

mapCauchyRealUnsafe :: (Accuracy -> MPBall -> MPBall) -> CauchyReal -> CauchyReal
mapCauchyRealUnsafe f (AsCauchyReal (CauchyReal_ name sq)) = 
    AsCauchyReal (CauchyReal_ name (\ ac -> f ac (sq ac))) 

convergent2CauchyReal :: 
    Maybe String -> [MPBall] -> CauchyReal
convergent2CauchyReal name convergentSeq =
    AsCauchyReal $ cr_ name sq
    where
    sq i =
        findAccurate convergentSeq
        where
        findAccurate [] =
            error "convergent2CauchyReal: the sequence either converges too slowly or it does not converge"
        findAccurate (b : rest)
            | getAccuracy b >= i = b
            | otherwise = findAccurate rest

seqByPrecision2Cauchy :: 
    Maybe String -> (Precision -> MPBall) -> CauchyReal
seqByPrecision2Cauchy name seqByPrecision =
    AsCauchyReal $ cr_ name
        (seqByPrecision2CauchySeq seqByPrecision)


instance CanAsCauchyRealA (->) CauchyReal_
instance CanAsCauchyRealA (WithPrecisionPolicy (->)) CauchyReal_

instance CanCreateAsCauchyRealA (->) CauchyReal_ where
    newCRA (_, name, ac2b) = AsCauchyReal $ cr_ name ac2b'
        where
        ac2b' ac = setPrecisionMatchAccuracy ac $ ac2b ac
              
instance CanCreateAsCauchyRealA (WithPrecisionPolicy (->)) CauchyReal_ where
    newCRA = 
        proc (_, name, ac2b) ->
            do
            pp <- getPrecisionPolicy -< () 
            returnA -< AsCauchyReal $ cr_ name (amend $ runWithPrecisionPolicy ac2b pp)
        where
        amend ac2b ac = setPrecisionMatchAccuracy ac $ ac2b ac
              
instance (ArrowChoice to) => CanReadAsCauchyRealA to CauchyReal_ where
    getNameCRA = arr $ cr_name . unAsCauchyReal 
    getAnswerCRA = arr getAnswerCR

getAnswerCR :: (CauchyReal, Accuracy) -> MPBall
getAnswerCR (AsCauchyReal r,ac) = cr_seq r ac
    

instance (Arrow to) => SupportsSenderIdA to CauchyReal_ where
    type SenderId to CauchyReal_ = ()
instance (Arrow to) => HasSenderIdA to CauchyReal_ where
    getSenderIdA = arr $ const ()


instance CanCombineCRsA (->) CauchyReal_ CauchyReal_ where
    type CombinedCRs (->) CauchyReal_ CauchyReal_ = CauchyReal_
    getSourcesOfCombinedCRs = const []

instance CanCombineCRwithA (->) CauchyReal_ CauchyReal_

{- conversions -}

type HasCauchyRealsA to = ConvertibleA to CauchyReal
type HasCauchyReals = HasCauchyRealsA (->)

type CanBeCauchyRealA to a = ConvertibleA to a CauchyReal
cauchyRealA :: (CanBeCauchyRealA to a) => a `to` CauchyReal
cauchyRealA = convertA
cauchyRealNamedA :: (CanBeCauchyRealA to a) => String -> a `to` CauchyReal
cauchyRealNamedA = convertNamedA
cauchyRealsA :: (CanBeCauchyRealA to a) => [a] `to` [CauchyReal]
cauchyRealsA = convertListA
cauchyRealsNamedA :: (CanBeCauchyRealA to a) => String -> [a] `to` [CauchyReal]
cauchyRealsNamedA = convertListNamedA
type CanBeCauchyReal a = CanBeCauchyRealA (->) a
cauchyReal :: (CanBeCauchyReal a) => a -> CauchyReal
cauchyReal = convert
cauchyReals :: (CanBeCauchyReal a) => [a] -> [CauchyReal]
cauchyReals = convertList

integer2CauchyReal :: Integer -> CauchyReal
integer2CauchyReal = convert

rational2CauchyReal :: Rational -> CauchyReal
rational2CauchyReal = convert

instance ConvertibleA (->) CauchyReal MPBall where
    convertA =
        error "conversion from CauchyReal to MPBall is allowed only in the WithPrecisionPolicy (->) arrow."

instance (CanAsCauchyRealA (WithPrecisionPolicy to) r, ArrowChoice to) => 
    ConvertibleA (WithPrecisionPolicy to) (AsCauchyReal r) MPBall where
    convertA =
        proc x ->
            do
            pp <- getPrecisionPolicy -< ()
            getAnswerCRA -< (x, bits $ prec2integer $ precPolicy_precision pp)

{- CauchyReal-producing operations -}

pi :: CauchyReal
pi = piA ()

instance (Arrow to ) => CanSqrtA to Integer where
    type SqrtTypeA to Integer = CauchyReal
    sqrtA = proc x ->
        returnA -< seqByPrecision2Cauchy (Just $ "sqrt " ++ show x) $ 
                        \p -> sqrt (integer2BallP p x)      

instance (Arrow to ) => CanExpA to Integer where
    type ExpTypeA to Integer = CauchyReal
    expA = proc x -> 
        returnA -< seqByPrecision2Cauchy (Just $ "exp " ++ show x) $ 
                        \p -> exp (integer2BallP p x)

instance (Arrow to) => CanSineCosineA to Integer where
    type SineCosineTypeA to Integer = CauchyReal
    sinA = proc x -> 
        returnA -< seqByPrecision2Cauchy (Just $ "sin " ++ show x) $ 
                        \p -> sin (integer2BallP p x)
    cosA = proc x -> 
        returnA -< seqByPrecision2Cauchy (Just $ "cos " ++ show x) $ 
                        \p -> cos (integer2BallP p x)

instance (Arrow to ) => CanSqrtA to Rational where
    type SqrtTypeA to Rational = CauchyReal
    sqrtA = proc x ->
        returnA -< seqByPrecision2Cauchy (Just $ "sqrt " ++ show x) $ 
                        \p -> sqrt (rational2BallP p x)      

instance (Arrow to ) => CanExpA to Rational where
    type ExpTypeA to Rational = CauchyReal
    expA = proc x -> 
        returnA -< seqByPrecision2Cauchy (Just $ "exp " ++ show x) $ 
                        \p -> exp (rational2BallP p x)

instance (Arrow to) => CanSineCosineA to Rational where
    type SineCosineTypeA to Rational = CauchyReal
    sinA = proc x -> 
        returnA -< seqByPrecision2Cauchy (Just $ "sin " ++ show x) $ 
                        \p -> sin (rational2BallP p x)
    cosA = proc x -> 
        returnA -< seqByPrecision2Cauchy (Just $ "cos " ++ show x) $ 
                        \p -> cos (rational2BallP p x)

{- Instances of Prelude numerical classes provided for convenient use outside AERN2 
   and also because Template Haskell translates (-x) to (Prelude.negate x) -}  
instance Num CauchyReal where
    fromInteger = convert
    negate = negate
    (+) = (+)
    (*) = (*)
    abs = abs
    signum = error "Prelude.signum not implemented for CauchyReal"

instance Eq CauchyReal where
    (==) = (==)

instance Ord CauchyReal where
    compare r1 r2 
        | r1 < r2 = LT
        | r1 > r2 = GT
        | r1 == r2 = EQ
        | otherwise = error "AERN2.Num.CauchyReal: compare: impossible case"
        
instance Fractional CauchyReal where
    fromRational = convert
    recip = recip
    (/) = (/)

instance Floating CauchyReal where
    pi = pi
    sqrt = sqrt
    exp = exp
    sin = sin
    cos = cos
    log = error "CauchyReal: log not implemented yet"
    atan = error "CauchyReal: atan not implemented yet"
    atanh = error "CauchyReal: atanh not implemented yet"
    asin = error "CauchyReal: asin not implemented yet"
    acos = error "CauchyReal: acos not implemented yet"
    sinh = error "CauchyReal: sinh not implemented yet"
    cosh = error "CauchyReal: cosh not implemented yet"
    asinh = error "CauchyReal: asinh not implemented yet"
    acosh = error "CauchyReal: acosh not implemented yet"
    
        