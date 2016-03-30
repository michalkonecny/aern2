{-# LANGUAGE CPP, FlexibleInstances, FlexibleContexts #-}
module AERN2.Net.Strategy.Direct 
(
    ufnB2B_x, ufnB2B_10x2p1, ufnB2B_1o10x2p1, ufnB2B_sinx,
    Interval(..), 
    onRationalInterval, rati2MPBall, 
    UnaryFnMPBall(..), UnaryFnCR(..)
)
where

import AERN2.Num
import qualified Prelude as P

import AERN2.RealFunction

import qualified Data.PQueue.Max as Q -- used in a range algorithm

--import qualified Data.Map as Map

import Debug.Trace (trace)

shouldTrace :: Bool
shouldTrace = False
--shouldTrace = True

maybeTrace :: String -> a -> a
maybeTrace 
    | shouldTrace = trace
    | otherwise = const id

{- mini examples -}

ufnB2B_x :: UnaryFnMPBall
ufnB2B_x = projUnaryFnA (Interval (-1.0) 2.0)

ufnB2B_1o10x2p1 :: UnaryFnMPBall
ufnB2B_1o10x2p1= 
    UnaryFnMPBall (Interval (-1.0) 2.0) $
        \ b -> 1/((10*b*b)+1)

ufnB2B_10x2p1 :: UnaryFnMPBall
ufnB2B_10x2p1= 
    UnaryFnMPBall (Interval (-1.0) 2.0) $
        \ b -> ((10*b*b)+1)

ufnB2B_sinx :: UnaryFnMPBall
ufnB2B_sinx= 
    UnaryFnMPBall (Interval (-1.0) 10.0) $
        \ b -> (sin b)

-- rangeOnIntervalUnaryFnA (ufnB2B_sinx, Interval (-1.0) 10.0)

{- TODO The following function types should move to aern2-function, when it is created -}

data UnaryFnMPBall =
    UnaryFnMPBall
    {
        ufnB2B_dom :: Interval Rational, 
        ufnB2B_eval :: MPBall -> MPBall
    }

instance RealUnaryFnA (->) UnaryFnMPBall where
    type UnaryFnDomPoint UnaryFnMPBall = Rational
    type UnaryFnPoint UnaryFnMPBall = CauchyReal
    constUnaryFnA (dom, r) = UnaryFnMPBall dom (\b -> cauchyReal2ball r (getFiniteAccuracy b))
    projUnaryFnA dom = UnaryFnMPBall dom id
    getDomainUnaryFnA = ufnB2B_dom
    evalAtPointUnaryFnA (UnaryFnMPBall _dom f, r) = 
        convergent2CauchyReal Nothing $ 
            map f $
                map (cauchyReal2ball r) (map bits [1..])
    evalAtDomPointUnaryFnA (UnaryFnMPBall _dom f, r) = 
        convergent2CauchyReal Nothing $ 
            map f $ map (flip rational2BallP r) standardPrecisions
    rangeOnIntervalUnaryFnA (UnaryFnMPBall _dom f, ri) =
        Interval l r
        where
        l = convergent2CauchyReal Nothing minSequence
        r = convergent2CauchyReal Nothing maxSequence
        maxSequence = search fi friL $ Q.singleton $ MaxSearchSegment ri friL friR
            where
            (friL, friR) = ball2endpoints fri
            fri = fi ri
            fi = onRationalInterval f -- . rati2MPBall
        minSequence = map negate $ search fi friL $ Q.singleton $ MaxSearchSegment ri friL friR
            where
            (friL, friR) = ball2endpoints fri
            fri = fi ri
            fi = negate . onRationalInterval f -- . rati2MPBall
        search fi prevL prevQueue =
            maybeTrace 
            (
                "rangeOnIntervalUnaryFnA: search:" 
                ++ "\n  seg = " ++ show seg
                ++ "\n  normLog(seg) = " ++ show (getNormLog (width seg))
                ++ "\n  nextL = " ++ show nextL
                ++ "\n  segValR = " ++ show segValR
                ++ "\n  currentBall = " ++ show currentBall
                ++ "\n  accuracy(currentBall) = " ++ show (getAccuracy currentBall)
            ) $
            currentBall : 
                search fi nextL nextQueue12
            where
            currentBall = endpoints2Ball nextL segValR
            (MaxSearchSegment seg segValL segValR, rest) = Q.deleteFindMax prevQueue
            nextL = segValL `max` prevL
            (seg1, seg2) = splitInterval seg
            (seg1ValL, seg1ValR) = ball2endpoints $ fi seg1
            (seg2ValL, seg2ValR) = ball2endpoints $ fi seg2
            seg1NoMax = (seg1ValR <= nextL) == Just True 
            seg2NoMax = (seg2ValR <= nextL) == Just True
            nextQueue1 =
                if seg1NoMax then rest else Q.insert seg1E rest
            nextQueue12 =
                if seg2NoMax then nextQueue1 else Q.insert seg2E nextQueue1
            seg1E = MaxSearchSegment seg1 seg1ValL seg1ValR
            seg2E = MaxSearchSegment seg2 seg2ValL seg2ValR

data MaxSearchSegment =
    MaxSearchSegment 
    {
        _maxSearchSegment_seg :: Interval Rational,
        _maxSearchSegment_lowerBnd :: MPBall, -- should be exact
        _maxSearchSegment_upperBnd :: MPBall -- should be exact
    }

instance Eq MaxSearchSegment where
    (MaxSearchSegment (Interval l1 r1) _ _) == (MaxSearchSegment (Interval l2 r2) _ _) =
        l1 == l2 && r1 == r2
instance Ord MaxSearchSegment where
    compare (MaxSearchSegment _ _ u1) (MaxSearchSegment _ _ u2) =
        case (u1 < u2, u1 > u2) of
            (Just True, _) -> P.LT
            (_, Just True) -> P.GT
            _ -> P.EQ

splitInterval ::
    (CanDivBy a Integer, CanAddSameTypeA (->) a) => 
    Interval a -> (Interval a, Interval a)
splitInterval (Interval l r) =
    (Interval l m, Interval m r)
    where
    m = (l + r) / 2 
    
instance CanIntegrateUnaryFnA (->) UnaryFnMPBall where
    integrateUnaryFnA (UnaryFnMPBall _dom f, aCR, bCR) =
        newCRA ([], Nothing, withAccuracy a b)
        where
        -- TODO: remove the assumption that a,b are "small" rationals
        a = toRationalUp $ cauchyReal2ball aCR (bits 100)
        b = toRationalUp $ cauchyReal2ball bCR (bits 100)
        withAccuracy l r ac 
            | getAccuracy value >= ac =
                maybeTrace 
                ("integrateUnaryFnA:"
                 ++ "\n l = " ++ show (mpBall l)
                 ++ "\n r = " ++ show (mpBall r)
                 ++ "\n ac = " ++ show ac
                 ++ "\n getAccuracy value = " ++ show (getAccuracy value)
                )  
                value 
            | otherwise = 
                (withAccuracy l m (ac+1))
                +
                (withAccuracy m r (ac+1))
            where
            m = (l+r)/2
            value = (f lr)*(r-l)
            lr = endpoints2Ball lB rB
            lB = z + l
            rB = z + r
            z = setPrecisionMatchAccuracy (ac + 100) $ mpBall 0

data UnaryFnCR =
    UnaryFnCR
    {
        ufnCR2CR_dom :: Interval Rational, 
        ufnCR2CR_eval :: CauchyReal -> CauchyReal
    } 


instance RealUnaryFnA (->) UnaryFnCR
    where
    type UnaryFnDomPoint UnaryFnCR = Rational
    type UnaryFnPoint UnaryFnCR = CauchyReal
    constUnaryFnA (dom, r) = UnaryFnCR dom (const r)
    projUnaryFnA dom = UnaryFnCR dom id
    getDomainUnaryFnA = ufnCR2CR_dom
    evalAtPointUnaryFnA (UnaryFnCR _dom f, r) = f r 
    evalAtDomPointUnaryFnA (UnaryFnCR _dom f, r) = f (cauchyReal r) 
    rangeOnIntervalUnaryFnA (UnaryFnCR _dom _f, _ri) = 
        error "rangeOnIntervalUnaryFnA not implemented for UnaryFnCR"


{- utilities -}

onRationalInterval :: (MPBall -> MPBall) -> (Interval Rational -> MPBall)
onRationalInterval f (Interval l r) =
    maybeTrace
    (
        "onRationalInterval:"
        ++ "\n nl = " ++ show nl
        ++ "\n precisions = " ++ show (take (int 10) precisions)
        ++ "\n result accuracy = " ++ show (getAccuracy result)
    ) $
    result
    where
    result = untilLittleImprovement resultsWithIncreasingPrecision 
    resultsWithIncreasingPrecision = map fp precisions
    fp p = f b
        where
        b = endpoints2Ball lMP rMP
        lMP = rational2BallP p l
        rMP = rational2BallP p r
    precisions = 
        drop (int 1) $ -- ignore the initial precision
        map prec precisions'
    precisions' = -- Fibonacci series starting with initPrec, initPrec+10, 2*initPrec + 10, ...
        initPrec : (initPrec+10) : zipWith (+) precisions' (drop (int 1) precisions')
    initPrec = 
        case nl of 
            NormBits i -> (max 10 (-i))
            NormZero -> error "onRationalInterval does not work for a singleton interval"
    nl = getNormLog (r - l)
    untilLittleImprovement results =
        maybeTrace ("untilLittleImprovement: improvements = " ++ show (take (int 10) improvements)) $
        pickFirstResultWithLowImprovement $ zip improvements results
        where
        pickFirstResultWithLowImprovement ((improvementPrec, res) : rest)
            | improvementPrec == NormZero = res
            | otherwise = pickFirstResultWithLowImprovement rest
        pickFirstResultWithLowImprovement _ = error "internal error in onRationalInterval"
        radii = map ballRadius results
        improvements = zipWith measureImprovement radii (drop (int 1) radii)
        measureImprovement r1 r2 =
            getNormLog $ max (mpBall 0) $ r1 - r2

rati2MPBall :: Interval Rational -> MPBall
rati2MPBall _il@(Interval l r) =
    maybeTrace
    (
        "rati2MPBall: " ++ show _il ++ ": nl = " ++ show nl ++ "prec(l) = " ++ show (getPrecision lMP)
    ) $
    endpoints2Ball lMP rMP
    where
    lMP = q2MP l
    rMP = q2MP r
    q2MP q =
        case nl of
            NormBits i -> rational2BallP (prec (max 10 (10 - i))) q
            NormZero -> error "rati2MPBall does not work for a singleton interval"
    nl = getNormLog (r - l)

_mpBall2rati :: MPBall -> Interval Rational
_mpBall2rati b =
    Interval l r
    where
    l = toRationalDown b
    r = toRationalUp b

_cri2MPBall :: Interval CauchyReal -> MPBall
_cri2MPBall (Interval l r) =
--    maybeTrace
--    (
--        "cri2MPBall: Interval " ++ show lMP ++ " " ++ show rMP
--    ) $
    endpoints2Ball lMP rMP
    where
    lMP = cauchyReal2ball l a
    rMP = cauchyReal2ball r a
    a =
        case nl of
            NormBits i -> bits (max 10 (1000 - i))
            NormZero -> error "cri2MPBall does not work for a singleton interval"
    nl = getNormLog ((cauchyReal2ball r a0) - (cauchyReal2ball l a0))
    a0 = bits 10

_mpBall2cri :: MPBall -> Interval CauchyReal
_mpBall2cri b =
--    maybeTrace
--    (
--        "mpBall2cri: b = " ++ show b
--    ) $
    Interval l r
    where
    l = convergent2CauchyReal Nothing $ repeat lMP
    r = convergent2CauchyReal Nothing $ repeat rMP
    (lMP, rMP) = ball2endpoints b

