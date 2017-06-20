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

type QANetLog = [QANetLogItem]

data QANetLogItem
    = QANetLogCreate
        ValueId -- new value
        [ValueId] -- dependent values
        String -- name
    | QANetLogQuery
        ValueId -- the value being queried
        String -- description of query
    | QANetLogAnswer
        ValueId -- the value being described
        String -- information about the use of cache
        String -- description of answer

instance Show QANetLogItem where
  show (QANetLogCreate valId sources name) =
    "new (" ++ (show valId) ++ ") " ++ name ++ " <- " ++ show sources
  show (QANetLogQuery valId queryS) =
    "(" ++ (show valId) ++ "): ? " ++ queryS
  show (QANetLogAnswer valId cacheInfoS answerS) =
    "(" ++ (show valId) ++ "): ! " ++ answerS ++ " (" ++ cacheInfoS ++ ")"

newtype ValueId = ValueId Integer
    deriving (Show, P.Eq, P.Ord, Enum)

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
                QANetLogQuery _ _ -> (level + 1, level + 1)
                QANetLogAnswer _ _ _ -> (level, level - 1)
                _ -> (level, level)
