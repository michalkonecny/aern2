module QANetLog exposing (..)

import Json.Decode
import Json.Decode exposing ((:=))
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict
import Set


type QANetLogItem  =
    QANetLogCreate {qaLogCreate_newId: ValueId, qaLogCreate_sources: (List ValueId), qaLogCreate_name: String}
    | QANetLogQuery {qaLogQuery_client: (Maybe ValueId), qaLogQuery_provider: ValueId, qaLogQuery_description: String}
    | QANetLogAnswer {qaLogAnswer_client: (Maybe ValueId), qaLogAnswer_provider: ValueId, qaLogAnswer_cacheUseDescription: String, qaLogAnswer_description: String}

jsonDecQANetLogItem : Json.Decode.Decoder ( QANetLogItem )
jsonDecQANetLogItem =
    let jsonDecDictQANetLogItem = Dict.fromList
            [ ("QANetLogCreate", Json.Decode.map QANetLogCreate (   ("qaLogCreate_newId" := jsonDecValueId) `Json.Decode.andThen` \pqaLogCreate_newId ->    ("qaLogCreate_sources" := Json.Decode.list (jsonDecValueId)) `Json.Decode.andThen` \pqaLogCreate_sources ->    ("qaLogCreate_name" := Json.Decode.string) `Json.Decode.andThen` \pqaLogCreate_name ->    Json.Decode.succeed {qaLogCreate_newId = pqaLogCreate_newId, qaLogCreate_sources = pqaLogCreate_sources, qaLogCreate_name = pqaLogCreate_name}))
            , ("QANetLogQuery", Json.Decode.map QANetLogQuery (   (Json.Decode.maybe ("qaLogQuery_client" := jsonDecValueId)) `Json.Decode.andThen` \pqaLogQuery_client ->    ("qaLogQuery_provider" := jsonDecValueId) `Json.Decode.andThen` \pqaLogQuery_provider ->    ("qaLogQuery_description" := Json.Decode.string) `Json.Decode.andThen` \pqaLogQuery_description ->    Json.Decode.succeed {qaLogQuery_client = pqaLogQuery_client, qaLogQuery_provider = pqaLogQuery_provider, qaLogQuery_description = pqaLogQuery_description}))
            , ("QANetLogAnswer", Json.Decode.map QANetLogAnswer (   (Json.Decode.maybe ("qaLogAnswer_client" := jsonDecValueId)) `Json.Decode.andThen` \pqaLogAnswer_client ->    ("qaLogAnswer_provider" := jsonDecValueId) `Json.Decode.andThen` \pqaLogAnswer_provider ->    ("qaLogAnswer_cacheUseDescription" := Json.Decode.string) `Json.Decode.andThen` \pqaLogAnswer_cacheUseDescription ->    ("qaLogAnswer_description" := Json.Decode.string) `Json.Decode.andThen` \pqaLogAnswer_description ->    Json.Decode.succeed {qaLogAnswer_client = pqaLogAnswer_client, qaLogAnswer_provider = pqaLogAnswer_provider, qaLogAnswer_cacheUseDescription = pqaLogAnswer_cacheUseDescription, qaLogAnswer_description = pqaLogAnswer_description}))
            ]
    in  decodeSumObjectWithSingleField  "QANetLogItem" jsonDecDictQANetLogItem

jsonEncQANetLogItem : QANetLogItem -> Value
jsonEncQANetLogItem  val =
    let keyval v = case v of
                    QANetLogCreate vs -> ("QANetLogCreate", encodeObject [("qaLogCreate_newId", jsonEncValueId vs.qaLogCreate_newId), ("qaLogCreate_sources", (Json.Encode.list << List.map jsonEncValueId) vs.qaLogCreate_sources), ("qaLogCreate_name", Json.Encode.string vs.qaLogCreate_name)])
                    QANetLogQuery vs -> ("QANetLogQuery", encodeObject [("qaLogQuery_client", (maybeEncode (jsonEncValueId)) vs.qaLogQuery_client), ("qaLogQuery_provider", jsonEncValueId vs.qaLogQuery_provider), ("qaLogQuery_description", Json.Encode.string vs.qaLogQuery_description)])
                    QANetLogAnswer vs -> ("QANetLogAnswer", encodeObject [("qaLogAnswer_client", (maybeEncode (jsonEncValueId)) vs.qaLogAnswer_client), ("qaLogAnswer_provider", jsonEncValueId vs.qaLogAnswer_provider), ("qaLogAnswer_cacheUseDescription", Json.Encode.string vs.qaLogAnswer_cacheUseDescription), ("qaLogAnswer_description", Json.Encode.string vs.qaLogAnswer_description)])
    in encodeSumObjectWithSingleField keyval val



type ValueId  =
    ValueId Int

jsonDecValueId : Json.Decode.Decoder ( ValueId )
jsonDecValueId =
    Json.Decode.map ValueId (Json.Decode.int)


jsonEncValueId : ValueId -> Value
jsonEncValueId (ValueId v1) =
    Json.Encode.int v1
