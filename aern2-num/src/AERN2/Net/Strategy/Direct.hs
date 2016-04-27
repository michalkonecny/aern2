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

import Control.Applicative

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
        \ b -> catchingExceptions $ 1/((10*b*b)+1)

ufnB2B_10x2p1 :: UnaryFnMPBall
ufnB2B_10x2p1= 
    UnaryFnMPBall (Interval (-1.0) 2.0) $
        \ b -> catchingExceptions ((10*b*b)+1)

ufnB2B_sinx :: UnaryFnMPBall
ufnB2B_sinx= 
    UnaryFnMPBall (Interval (-1.0) 10.0) $
        \ b -> catchingExceptions (sin b)

-- rangeOnIntervalUnaryFnA (ufnB2B_sinx, Interval (-1.0) 10.0)

{- TODO The following function types should move to aern2-function, when it is created -}

data UnaryFnMPBall =
    UnaryFnMPBall
    {
        ufnB2B_dom :: Interval Rational, 
        ufnB2B_eval :: MPBall -> CatchingExceptions MPBall
    }

instance RealUnaryFnA (->) UnaryFnMPBall where
    type UnaryFnDomPoint UnaryFnMPBall = Rational
    type UnaryFnPoint UnaryFnMPBall = CauchyReal
    constUnaryFnA (dom, r) = UnaryFnMPBall dom (\b -> catchingExceptions $ cauchyReal2ball r (getFiniteAccuracy b))
    projUnaryFnA dom = UnaryFnMPBall dom catchingExceptions
    getDomainUnaryFnA = ufnB2B_dom
    evalAtPointUnaryFnA (UnaryFnMPBall _dom f, r) = 
        convergent2CauchyReal Nothing $
            filterNoException 20 True $
                map f $
                    map (cauchyReal2ball r) (map bits [1..])
    evalAtDomPointUnaryFnA (UnaryFnMPBall _dom f, r) = 
        convergent2CauchyReal Nothing $ 
            filterNoException 20 True $
                map f $ map (flip rational2BallP r) standardPrecisions
    rangeOnIntervalUnaryFnA (UnaryFnMPBall _dom f, ri) =
        Interval l r
        where
        l = convergent2CauchyReal Nothing $ filterNoException 100 True minSequence
        r = convergent2CauchyReal Nothing $ filterNoException 100 True maxSequence
        maxSequence = search fi friL $ Q.singleton $ MaxSearchSegment ri friL friR
            where
            (friL, friR) = gunzip $ fmap ball2endpoints fri
            fri = fi ri
            fi = onRationalInterval f -- . rati2MPBall
        minSequence = map negate $ search fi friL $ Q.singleton $ MaxSearchSegment ri friL friR
            where
            (friL, friR) = gunzip $ fmap ball2endpoints fri
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
            -- unpack the current segment and a pre-computed enclosure of the function on this segment:
            (MaxSearchSegment seg segValL segValR, rest) = Q.deleteFindMax prevQueue
            -- get an enclosure of the function's maximum based on previous segments and the current segment:
            nextL 
                | hasError prevL = segValL
                | otherwise = liftA2 max segValL prevL
            currentBall = liftA2 endpoints2Ball nextL segValR
            
            -- split the current segment and pre-compute
            (seg1, seg2) = splitInterval seg
            (seg1ValL, seg1ValR) = fiEE seg1
            (seg2ValL, seg2ValR) = fiEE seg2
            seg1NoMax = (seg1ValR <= nextL) == Just (Just True) 
            seg2NoMax = (seg2ValR <= nextL) == Just (Just True)
            nextQueue1 =
                if seg1NoMax then rest else Q.insert seg1E rest
            nextQueue12 =
                if seg2NoMax then nextQueue1 else Q.insert seg2E nextQueue1
            seg1E = MaxSearchSegment seg1 seg1ValL seg1ValR
            seg2E = MaxSearchSegment seg2 seg2ValL seg2ValR
            
            fiEE s = 
                gunzip $ fmap ball2endpoints $ fi s

data MaxSearchSegment =
    MaxSearchSegment 
    {
        _maxSearchSegment_seg :: Interval Rational,
        _maxSearchSegment_lowerBnd :: CatchingExceptions MPBall, -- should be exact
        _maxSearchSegment_upperBnd :: CatchingExceptions MPBall -- should be exact
    }

instance Eq MaxSearchSegment where
    (MaxSearchSegment (Interval l1 r1) _ _) == (MaxSearchSegment (Interval l2 r2) _ _) =
        l1 == l2 && r1 == r2
instance Ord MaxSearchSegment where
    compare (MaxSearchSegment _ _ u1) (MaxSearchSegment _ _ u2) =
        case (u1 < u2, u1 > u2) of
            (Just (Just True), _) -> P.LT
            (_, Just (Just True)) -> P.GT
            _ 
                | hasError u1 && hasError u2 -> P.EQ
                | hasError u1 -> P.GT
                | hasError u2 -> P.LT
                | otherwise -> P.EQ

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
        withAccuracy l r ac =
            ifExceptionDie "integrateUnaryFnA for an UnaryFnMPBall" $
                integr l r ac 
        integr l r ac 
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
                (integr l m (ac+1))
                +
                (integr m r (ac+1))
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

onRationalInterval :: (MPBall -> CatchingExceptions MPBall) -> (Interval Rational -> CatchingExceptions MPBall)
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
    untilLittleImprovement resultsE =
        case results of
            [] -> head resultsE
            _ -> 
                maybeTrace ("untilLittleImprovement: improvements = " ++ show (take (int 10) improvements)) $
                catchingExceptions $ pickFirstResultWithLowImprovement $ zip improvements results
        where
        results = filterNoException 20 False resultsE
        pickFirstResultWithLowImprovement [(_,res)] = res
        pickFirstResultWithLowImprovement ((improvementPrec, res) : rest)
            | improvementPrec == NormZero = res
            | otherwise = pickFirstResultWithLowImprovement rest
        pickFirstResultWithLowImprovement _ = error "internal error in onRationalInterval"
        radii = map ballRadius results
        improvements = zipWith measureImprovement radii (drop (int 1) radii)
        measureImprovement r1 r2 = getNormLog $ max (mpBall 0) $ r1 - r2

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

