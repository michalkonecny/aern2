{-# LANGUAGE CPP #-}
{-|
    Module      :  Main (file aern2-fun-serialise)
    Description :  generate and serialise function pairs
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Main where

import Numeric.MixedTypes
-- import qualified Prelude as P

import Text.Printf

import System.Environment

-- import Data.String (fromString)
-- import qualified Data.ByteString.Lazy as ByteString
-- import Data.ByteString.Lazy.Char8 (unpack)
-- import qualified Codec.Compression.GZip as GZip

-- import Test.QuickCheck

import AERN2.Utils.Bench

import AERN2.MP
-- import AERN2.Real

import AERN2.Interval

import AERN2.RealFun.Tests (FnAndDescr(..))

#ifdef CHPOLY
import qualified AERN2.Poly.Cheb as Fn
import AERN2.Poly.Cheb.Tests

type Fn = Fn.ChPolyMB
#endif

#ifdef PPOLY
import qualified AERN2.PPoly as Fn
import AERN2.PPoly.Tests

type Fn = Fn.PPoly
#endif

main :: IO ()
main =
  do
  args <- getArgs
  let (deg, count) = processArgs args
  serialisePolys deg count

processArgs :: [String] -> (Integer, Integer)
processArgs [degS, countS] =
  (read degS, read countS)
processArgs _ =
  error "expecting arguments: <degree> <count>"

serialisePolys :: Integer -> Integer -> IO ()
serialisePolys deg count =
  writeFile fileName $
    --  unpack $ GZip.compressWith GZip.defaultCompressParams { GZip.compressLevel = GZip.bestCompression } $ fromString $
      unlines $ map Fn.serialise polys
  where
  fileName = serialiseFileName deg count
  polys =
    concat $ map (\(a,b) -> [a, b]) $
      take (int count) $ valuePairsWithDeg deg

serialiseFileName :: Integer -> Integer -> String
serialiseFileName deg count =
  printf "ChPolyPair-deg%03d-%dx.hss" deg count

valuePairsWithDeg :: Integer -> [(Fn, Fn)]
valuePairsWithDeg deg =
  map makeFn2PositiveSmallRange $
    map reduceDegrees $
      valuePairsWithMinDeg deg
  where
  reduceDegrees = mapBoth (centreAsBall . Fn.reduceDegree deg)

mapBoth :: (t1 -> t2) -> (t1,t1) -> (t2,t2)
mapBoth f (a,b) = (f a, f b)

valuePairsWithMinDeg :: Integer -> [(Fn, Fn)]
valuePairsWithMinDeg deg =
  listFromGen $
    do
    (p1,_) <- arbitraryWithDegDom deg dom
    (p2,_) <- arbitraryWithDegDom deg dom
    return (p1, p2)
  where
  dom = dyadicInterval (0.0,1.0)


makeFn2PositiveSmallRange :: (Fn, Fn) -> (Fn, Fn)
makeFn2PositiveSmallRange = mapSecondFD (makeFnPositiveSmallRange 10)

mapSecondFD :: (FnAndDescr f1 -> FnAndDescr f2) -> (t, f1) -> (t, f2)
mapSecondFD f (a,b) = (a, fb)
  where
  FnAndDescr fb _ = f (FnAndDescr b "")
