module FnReps.Fun where

import AERN2.Num

import qualified Data.PQueue.Max as Q -- used in a range algorithm
import Control.Applicative (liftA2)

import AERN2.Net (UnaryFnMPBall(..), MaxSearchSegment(..), onRationalInterval, splitInterval)

integrateDFun :: UnaryFnMPBall -> UnaryFnMPBall -> Accuracy -> MPBall
integrateDFun fn@(UnaryFnMPBall _dom f) _dfn@(UnaryFnMPBall _ df) acG =
    withAccuracy lG rG acG
    where
    Interval lG rG = ufnB2B_dom fn
    withAccuracy l r ac =
        ifExceptionDie "integrateUnaryFnA for an UnaryFnMPBall" $
            integr l r ac 
    integr l r ac 
        | getAccuracy value >= ac =
            value 
        | otherwise = 
            (integr l m (ac+1))
            +
            (integr m r (ac+1))
        where
        m = (l+r)/2
        mB = z + m
        value = (f mB)*(r-l)+errB
        errB = ((deriv - deriv)/2)*(((r-l)/2)^2)/2
        deriv = df lr
        lr = endpoints2Ball lB rB
        lB = z + l
        rB = z + r
        z = setPrecisionMatchAccuracy (ac + 100) $ mpBall 0
    
maxDFun :: UnaryFnMPBall -> UnaryFnMPBall -> Accuracy -> MPBall
maxDFun _fn@(UnaryFnMPBall _dom f) _dfn@(UnaryFnMPBall _ df) ac =
    head $ filter (\b -> getAccuracy b >= ac) $ filterNoException 100 True maxSequence
    where
    maxSequence = search fi friL $ Q.singleton $ MaxSearchSegment ri friL friR
        where
        (friL, friR) = gunzip $ fmap ball2endpoints fri
        fri = fi ri
        ri = Interval (-1.0) 1.0
        fi i@(Interval l r) =
--            liftA2 intersectBalls resViaD resDirect
            resViaD
            where
--            resDirect =
--                onRationalInterval f i
            resViaD = 
                fm + (err * (catchingExceptions $ endpoints2Ball (-o) o))
            o = mpBall 1
            fm = case catchingExceptions_maybeValue err of
                    Just errV -> f (rational2BallP (getPrecision errV) m)
                    _ -> err
            m = (l + r)/2
            err = (dfi i) * (r-l)/2
        dfi = onRationalInterval df
    search fi prevL prevQueue =
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
