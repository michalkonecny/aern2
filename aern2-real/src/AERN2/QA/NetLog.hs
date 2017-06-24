{-# LANGUAGE DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving #-}
{-|
    Module      :  AERN2.QA.NetLog
    Description :  QA network log data structure
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    QA network log data structure
-}
module AERN2.QA.NetLog where

import MixedTypesNumPrelude
import qualified Prelude as P

import Text.Printf

import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Aeson as J (ToJSON, encode)

type QANetLog = [QANetLogItem]

data QANetLogItem
    = QANetLogCreate
        ValueId -- new value
        [ValueId] -- dependent values (sources of queries)
        String -- name
    | QANetLogQuery
        (Maybe ValueId) -- the source of the query
        ValueId -- the value being queried
        String -- description of query
    | QANetLogAnswer
        (Maybe ValueId) -- the destination of the answer
        ValueId -- the value being described
        String -- information about the use of cache
        String -- description of answer
    deriving (Generic, ToJSON)

instance Show QANetLogItem where
  show (QANetLogCreate valId sources name) =
    printf "new (%s) %s <- %s"
      (show valId) name (show sources)
  show (QANetLogQuery mSrcId valId queryS) =
    printf "(%s)<-(%s): ? %s"
      (show valId) (showSrc mSrcId) queryS
  show (QANetLogAnswer mSrcId valId cacheInfoS answerS) =
    printf "(%s)->(%s): ! %s (%s)"
      (show valId) (showSrc mSrcId) answerS cacheInfoS

showSrc (Just srcId) = show srcId
showSrc Nothing = ""

newtype ValueId = ValueId Integer
    deriving (Show, P.Eq, P.Ord, Generic, ToJSON)

instance Enum ValueId where
  toEnum = ValueId . toEnum
  fromEnum (ValueId n) = fromEnum n

printQANetLogThenResult :: (Show a) =>(QANetLog, a) -> IO ()
printQANetLogThenResult (lg, result) =
    do
    printQALog lg
    putStrLn $ show result

printQALog :: QANetLog -> IO ()
printQALog = putStrLn . formatQALog 0

formatQALog :: Integer -> QANetLog -> String
formatQALog = aux
    where
    aux _ [] = ""
    aux level (item : rest) =
        (indent ++ show item ++ "\n") ++
        (aux level' rest)
        where
        indent = replicate (int levelNow) ' '
        (levelNow, level') =
            case item of
                QANetLogQuery _ _ _ -> (level + 1, level + 1)
                QANetLogAnswer _ _ _ _ -> (level, level - 1)
                _ -> (level, level)

formatQALogJSON :: QANetLog -> String
formatQALogJSON = BS.unpack . J.encode

printQALogJSON :: QANetLog -> IO ()
printQALogJSON =
  BS.putStrLn . J.encode
