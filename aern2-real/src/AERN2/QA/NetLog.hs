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
module AERN2.QA.NetLog
(ValueId(..), QANetLogItem(..), QANetLog
, formatQALog, printQALog, printQANetLogThenResult
, formatQALogJSON, printQALogJSON, writeNetLogJSON)
where

import MixedTypesNumPrelude
import qualified Prelude as P

import Text.Printf

import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Aeson as J
import Data.Aeson.Types as JT

type QANetLog = [QANetLogItem]

data QANetLogItem
    = QANetLogCreate
      {
        qaLogCreate_newId :: ValueId
      , qaLogCreate_sources :: [ValueId]
      , qaLogCreate_name :: String
      }
    | QANetLogQuery
      {
        qaLogQuery_client :: (Maybe ValueId)
      , qaLogQuery_provider :: ValueId
      , qaLogQuery_description :: String
      }
    | QANetLogAnswer
      {
        qaLogAnswer_client :: (Maybe ValueId)
      , qaLogAnswer_provider :: ValueId
      , qaLogAnswer_cacheUseDescription :: String
      , qaLogAnswer_description :: String
      }
    deriving (Generic)

instance ToJSON QANetLogItem where
  toJSON = J.genericToJSON customOptions
    where
    customOptions = J.defaultOptions
        { JT.sumEncoding = JT.ObjectWithSingleField }


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

showSrc :: (Show a) => Maybe a -> String
showSrc (Just srcId) = show srcId
showSrc Nothing = ""

data ValueId = ValueId Integer
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
        indent = replicate levelNow ' '
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

writeNetLogJSON :: QANetLog -> IO ()
writeNetLogJSON netlog =
  writeFile "netlog.js" $
    "netlog='" ++  (filter goodChar $ formatQALogJSON netlog) ++ "'"
  where
  goodChar 'Â' = False
  goodChar _ = True
