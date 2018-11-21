{-|
    Module      :  AERN2.Utils.Bench
    Description :  utilities for benchmarks
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module AERN2.Utils.Bench
(
    listFromGen
)
where

-- import Test.QuickCheck
import Test.QuickCheck.Random (mkQCGen)
import Test.QuickCheck.Gen (Gen(..))

import MixedTypesNumPrelude
-- import qualified Prelude as P

listFromGen :: Gen a -> [a]
listFromGen gen =
    list
    where
    list =
        concat $ map genSome [1..]
        where
        genSome size =
            unGen (sequence $ replicate 10 gen) qcGen (int size)
    qcGen = mkQCGen (int 148548830)
