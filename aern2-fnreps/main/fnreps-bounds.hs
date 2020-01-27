{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
#define DEBUG
-- #define LIMIT
module Main where

#ifdef DEBUG
import Debug.Trace (trace)
#define maybeTrace trace
#else
#define maybeTrace (\ (_ :: String) t -> t)
#endif

import MixedTypesNumPrelude
-- import qualified Prelude as P
import Text.Printf

import qualified Data.Map as Map

-- import Control.Applicative (liftA2)

import System.Environment

import AERN2.MP
-- import qualified AERN2.MP.Ball as MPBall

import AERN2.Real

import AERN2.Interval

import AERN2.RealFun.Operations
import AERN2.RealFun.UnaryBallFun
import AERN2.RealFun.UnaryBallDFun
import AERN2.RealFun.UnaryModFun
-- import AERN2.Poly.Basics

import qualified AERN2.PPoly as PPoly
import AERN2.PPoly (PPoly)

import AERN2.Poly.Cheb (ChPoly)

import qualified AERN2.Frac as Frac
import AERN2.Frac (Frac)

import qualified AERN2.Local as Local
import qualified AERN2.Local.Poly as LPoly
import qualified AERN2.Local.PPoly as LPPoly
import qualified AERN2.Local.Frac as LFrac

import Demo ()

type FracMB = Frac MPBall

type LPolyMB = LPoly.LocalPoly MPBall
type LPPolyMB = LPPoly.LocalPPoly
type LFracMB = LFrac.LocalFrac MPBall

type RF = ChPoly MPBall

main :: IO ()
main =
    do
    args <- getArgs
    (computationDescription, result) <- processArgs args
    putStrLn $ computationDescription
    putStrLn $ "result = " ++ show result
    -- putStrLn $ "accuracy: " ++ show (getAccuracy result)
    -- putStrLn $ "precision = " ++ show (getPrecision result)

processArgs ::
    [String] ->
    IO (String, MPBall)
processArgs (operationCode : functionCode : representationCode : effortArgs) =
    return (computationDescription, result)
    where
    computationDescription =
        "computing " ++ operationCode ++ "  " ++ fnDescription

    (result, fnDescription) =
      case (representationCode, operationCode) of
        -- ("fun", "max") ->  eval functions maxModFun id x_MF
        -- ("ball", "max") -> eval functions maxBallFun id x_BF
        -- ("dball", "max") -> eval functions maxDBallFun id x_DBF
        ("poly", "bounds") -> eval functions boundsPB (x_PB (bits 60))
        -- ("ppoly", "max") -> eval functions maxPP PPoly.fromPoly (x_PB accuracy)
        -- ("frac", "max") -> eval functions maxFR Frac.fromPoly (x_PB accuracy)
        -- ("lpoly", "max") -> eval functions maxLP id x_LP
        -- ("lppoly", "max") -> eval functions maxLPP LPPoly.fromPoly x_LP
        -- ("lfrac", "max") -> eval functions maxLF LFrac.fromPoly x_LP
        _ -> error $ "unknown (representationCode, operationCode): " ++ show (representationCode, operationCode)
    eval ::
      Map.Map String (String, DyadicInterval, (RF -> RF)) ->
      (RF -> DyadicInterval -> MPBall) ->
      (DyadicInterval -> RF) ->
      (MPBall, String)
    eval fns op xForDom  =
      case Map.lookup functionCode fns of
        Just (fnDescription2, fnDom, fn_x) ->
          (op (fn_x (xForDom fnDom)) fnDom, fnDescription2)
        _ -> error $ "unknown function: " ++ functionCode

    -- accuracy = bits $ (read accuracyS :: Int)
    -- [accuracyS] = effortArgs

    boundsPB :: ChPoly MPBall -> DyadicInterval -> MPBall
    boundsPB f fDom = 
        snd $ endpoints $ max (abs fMin) (abs fMax)
        where
        fMax = maximumOverDom f fDom
        fMin = minimumOverDom f fDom

    -- maxLP :: LPolyMB -> Accuracy -> MPBall
    -- maxLP lf ac = Local.maximum lf (mpBall (-1)) (mpBall 1) ac

    -- maxLPP :: LPPolyMB -> Accuracy -> MPBall
    -- maxLPP lf ac = Local.maximum lf (mpBall (-1)) (mpBall 1) ac

    -- maxLF :: LFracMB -> Accuracy -> MPBall
    -- maxLF lf ac = Local.maximum lf (mpBall (-1)) (mpBall 1) ac

    -- maxPP :: PPoly -> Accuracy -> MPBall
    -- maxPP f _ = f `maximumOverDomPP` (getDomain f)
    --   where
    --   maximumOverDomPP f2 (Interval l r) =
    --     PPoly.maximum f2 lB rB
    --     where
    --     lB = setPrecision prc $ mpBall l
    --     rB = setPrecision prc $ mpBall r
    --     prc = getPrecision f2

    -- maxFR :: FracMB -> Accuracy -> MPBall
    -- maxFR f _ = f `maximumOverDomFR` (getDomain f)
    --   where
    --   maximumOverDomFR f2 (Interval l r) =
    --     Frac.maximumOptimisedWithAccuracy accuracy (setPrc f2) lB rB 5 5
    --     where
    --     lB = setPrc $ mpBall l
    --     rB = setPrc $ mpBall r
    --     setPrc :: (CanSetPrecision a) => a -> a
    --     setPrc =
    --       setPrecisionAtLeastAccuracy (accuracy) . setPrecision prc
    --     prc = getPrecision f2

    -- maxBallFun :: UnaryBallFun -> Accuracy -> MPBall
    -- maxBallFun fn ac =
    --     m ? AccuracySG ac ac
    --     where
    --     m = fn `maximumOverDom` getDomain fn

    -- maxModFun :: UnaryModFun -> Accuracy -> MPBall
    -- maxModFun fn ac =
    --     m ? AccuracySG ac ac
    --     where
    --     m = fn `maximumOverDom` getDomain fn

    -- maxDBallFun :: UnaryBallDFun -> Accuracy -> MPBall
    -- maxDBallFun (UnaryBallDFun [f, f']) ac =
    --     m ? AccuracySG ac ac
    --     where
    --     m = fn `maximumOverDom` getDomain f
    --     fn = UnaryBallDFun [f,f']
    -- maxDBallFun _ _ = error "maxDBallFun: invalid UnaryBallDFun"
processArgs _ =
    error "expecting arguments: <operationCode> <functionCode> <representationCode>s"

-- x_MF :: UnaryModFun
-- x_MF = varFn unaryIntervalDom ()

-- x_BF :: UnaryBallFun
-- x_BF = varFn unaryIntervalDom ()

-- x_DBF :: UnaryBallDFun
-- x_DBF = varFn unaryIntervalDom ()

x_PB :: Accuracy -> DyadicInterval -> ChPoly MPBall
x_PB acG dom =
  setAccuracyGuide acG $ varFn (dom, bits 10) ()

-- x_LP :: LPoly.LocalPoly MPBall
-- x_LP = LPoly.variable

functions ::
  Map.Map String (String, DyadicInterval, RF -> RF)
functions =
    Map.fromList
    [
      ("sine-T", 
        (sine_name "T (total error)", 
         sine_dom, sine_totalerr_x))
      ,
      ("sine-M", 
        (sine_name "M (model error)", 
         sine_dom, sine_modelerr_x))
      ,
      ("sine-R", 
        (sine_name "R (rounding error)", 
         sine_dom, sine_rounderr_x))
      ,
      -- ("heron-init-T", 
      --   (heron_init_name "T (total error)", 
      --    heron_x_dom, heron_init_totalerr_x))
      -- ,
      -- ("heron-init-M", 
      --   (heron_init_name "M (model error)", 
      --    heron_x_dom, heron_init_modelerr_x))
      -- ,
      ("heron-init-R", 
        (heron_init_name "R (rounding error)", 
         heron_x_dom, heron_init_rounderr_x))
    ]


-- type Signature1 f r =
--   ( HasAccuracy f, HasAccuracyGuide f
--   , CanSetAccuracyGuide f
--   , CanSinCosSameType f
--   , CanSqrtCNSameType f
--   , CanAddSameType f
--   , CanAddThis f Integer
--   , CanAddThis f CauchyReal
--   , CanAddThis f MPBall
--   , CanMulBy f Integer
--   , CanMulBy f CauchyReal
--   , CanMulBy f MPBall
--   , CanMulSameType f
--   , CanDivCNBy f Integer
--   , CanPowCNBy f Integer
--   , CanSub Integer f, SubType Integer f ~ f
--   , CanSub f f, SubType f f ~ f
--   , f ~ r
--   )

-- type Signature2 f =
--   ( HasAccuracy f, HasAccuracyGuide f
--   , CanSetAccuracyGuide f
--   , CanMulSameType f
--   , CanMinMaxSameType f
--   , CanDivCNSameType f, CanRecipCNSameType f)

-----------------------------------
-----------------------------------

-- constants related to single-precision floating-point format
single_prec :: Integer
single_prec = 23

single_minExp :: Integer
single_minExp = 126

---------------------------------------------------------------------
-- UNIVARIATE GLOBAL OPTIMISATION
-- |sin x - (x - x^3/6 + x^5/120 - x^7/5040)| <= eps

sine_name :: String -> String
sine_name label = 
  "sine deg-7 single-precision Taylor series " ++ label

sine_totalerr_x :: RF -> RF
sine_totalerr_x x = 
  sin x - (sinT7fp single_prec single_minExp x)

sine_modelerr_x :: RF -> RF
sine_modelerr_x x = 
  sin x - (sinT7horner x)

sine_rounderr_x :: RF -> RF
sine_rounderr_x x = 
  (sinT7fp single_prec single_minExp x) - (sinT7horner x)

sinT7horner :: 
  (CanAddSubMulBy t t, CanPowCNBy t Integer
  , CanSub Integer t, SubType Integer t ~ t
  , CanDivCNBy t Integer) => 
  t -> t
sinT7horner x = 
  x * (1 - x^!2/!6 * (1 - x^!2/!20*(1 - x^!2/!42)))

sinT7fp :: 
  (CanAddSubMulBy t t, CanPowCNBy t Integer
  , CanMulBy t MPBall
  , CanAddThis t MPBall
  , CanSub Integer t, SubType Integer t ~ t
  , CanDivCNBy t Integer) => 
  Integer -> Integer -> t -> t
sinT7fp fpPrec fpMinExp x = 
  x *. (1 -. x2/!.6 *. (1 -. x2/!.20*.(1 -. x2/!.42)))
  where
  x2 = (x^!2)*onePMe
  a -. b = (a-b+zeroPMd)*onePMe
  a *. b = (a*b+zeroPMd)*onePMe
  a /!. b = ((a/!b)+zeroPMd)*onePMe
  infixl 6 -.
  infixl 7 *.
  infixl 7 /!.
  onePMe = mpBall (1, 0.5^!fpPrec)
  zeroPMd = mpBall (0, 0.5^!fpMinExp)

sine_dom :: DyadicInterval
sine_dom = dyadicInterval (0.0, 0.75)

-- |sqrt x - (x+1)/2| <= 1/(2^(2^1)) + 6*eps

heron_init_name :: String -> String
heron_init_name label = 
  "heron first iteration single-precision rounding " ++ label

-- heron_init_totalerr_x :: RF -> RF
-- heron_init_totalerr_x x = 
--   (sqrt x) - (heron_init_y1_fp p x) + (real $ 0.25 + 6*eps) -- >= 0
--   where
--   p = single_prec
--   eps = 0.5^!p

-- heron_init_modelerr_x :: RF -> RF
-- heron_init_modelerr_x x = 
--   (sqrt x) - (heron_init_y1 x) + (real $ 0.25) -- >= 0

heron_init_rounderr_x :: RF -> RF
heron_init_rounderr_x x = 
  (heron_init_y1 x) - (heron_init_y1_fp single_prec x) -- >= 6*eps
  where
  p = single_prec
  eps = 0.5^!p

heron_init_y1 :: 
  (CanAddThis t Integer, CanDivCNBy t Integer) => 
  t -> t
heron_init_y1 x = (1 + x)/!2

heron_init_y1_fp :: 
  (CanMulBy t MPBall
  , CanAddThis t Integer
  , CanDivCNBy t Integer) => 
  Integer -> t -> t
heron_init_y1_fp fpPrec x = 
  (1 +^ x)/!^2
  where
  a +^ b = (a+b)*onePMe
  a /!^ b = ((a/!b))*onePMe
  infixl 6 +^
  infixl 7 /!^
  onePMe = mpBall (1, 0.5^!fpPrec)

heron_x_dom :: DyadicInterval
heron_x_dom = dyadicInterval (0.5, 2.0)


