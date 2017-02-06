{-|
    Module      :  Main (file aern2-fun-chPoly-serialise)
    Description :  generate and serialise ChPoly pairs
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

import qualified AERN2.Poly.Cheb as ChPoly
import AERN2.Poly.Cheb (ChPolyMB)
import AERN2.Poly.Cheb.Tests

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
      unlines $ map ChPoly.serialiseChPoly polys
  where
  fileName = serialiseFileName deg count
  polys = concat $ map (\(a,b) -> [a,b]) $ take (int count) $ valuePairsWithDeg deg

serialiseFileName :: Integer -> Integer -> String
serialiseFileName deg count =
  printf "ChPolyPair-deg%03d-%dx.hss" deg count

valuePairsWithDeg :: Integer -> [(ChPolyMB, ChPolyMB)]
valuePairsWithDeg deg =
  map reduceDegrees $
    valuePairsWithMinDeg deg
  where
  reduceDegrees = mapBoth (centreAsBall . ChPoly.reduceDegree deg)

mapBoth :: (t1 -> t2) -> (t1,t1) -> (t2,t2)
mapBoth f (a,b) = (f a, f b)

valuePairsWithMinDeg :: Integer -> [(ChPolyMB, ChPolyMB)]
valuePairsWithMinDeg deg =
  listFromGen $
    do
    (p1,_) <- arbitraryWithDegDom deg dom
    (p2,_) <- arbitraryWithDegDom deg dom
    return (p1, p2)
  where
  dom = dyadicInterval (0.0,1.0)
