module Api exposing (..)

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
import String
import Task


getApiSampling : Task.Task Http.Error (List Int)
getApiSampling =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "api"
          ++ "/" ++ "sampling"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list Json.Decode.int)
      (Http.send Http.defaultSettings request)

type alias Sampling =
  { sampling_dom' : Interval
  , sampling_maxStep : DyadicS
  }

type alias Interval =
  { endpointL : DyadicS
  , endpointR : DyadicS
  }

type alias DyadicS =
  { dyadic_value : Int
  , dyadic_exp : Int
  }

encodeSampling : Sampling -> Json.Encode.Value
encodeSampling x =
  Json.Encode.object
    [ ( "sampling_dom'", encodeInterval x.sampling_dom' )
    , ( "sampling_maxStep", encodeDyadicS x.sampling_maxStep )
    ]

encodeInterval : Interval -> Json.Encode.Value
encodeInterval x =
  Json.Encode.object
    [ ( "endpointL", encodeDyadicS x.endpointL )
    , ( "endpointR", encodeDyadicS x.endpointR )
    ]

encodeDyadicS : DyadicS -> Json.Encode.Value
encodeDyadicS x =
  Json.Encode.object
    [ ( "dyadic_value", Json.Encode.int x.dyadic_value )
    , ( "dyadic_exp", Json.Encode.int x.dyadic_exp )
    ]

postApiSampling : Sampling -> Task.Task Http.Error (Int)
postApiSampling body =
  let
    request =
      { verb =
          "POST"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "api"
          ++ "/" ++ "sampling"
      , body =
          Http.string (Json.Encode.encode 0 (encodeSampling body))
      }
  in
    Http.fromJson
      Json.Decode.int
      (Http.send Http.defaultSettings request)

decodeSampling : Json.Decode.Decoder Sampling
decodeSampling =
  Json.Decode.succeed Sampling
    |: ("sampling_dom'" := decodeInterval)
    |: ("sampling_maxStep" := decodeDyadicS)

decodeInterval : Json.Decode.Decoder Interval
decodeInterval =
  Json.Decode.succeed Interval
    |: ("endpointL" := decodeDyadicS)
    |: ("endpointR" := decodeDyadicS)

decodeDyadicS : Json.Decode.Decoder DyadicS
decodeDyadicS =
  Json.Decode.succeed DyadicS
    |: ("dyadic_value" := Json.Decode.int)
    |: ("dyadic_exp" := Json.Decode.int)

getApiSamplingBySamplingId : Int -> Task.Task Http.Error (Sampling)
getApiSamplingBySamplingId samplingId =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "api"
          ++ "/" ++ "sampling"
          ++ "/" ++ (samplingId |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeSampling
      (Http.send Http.defaultSettings request)

getApiFunction : Task.Task Http.Error (List Int)
getApiFunction =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "api"
          ++ "/" ++ "function"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list Json.Decode.int)
      (Http.send Http.defaultSettings request)

type alias DyadicIntervalAPI =
  { dyadic_endpointL : DyadicS
  , dyadic_endpointR : DyadicS
  }

decodeDyadicIntervalAPI : Json.Decode.Decoder DyadicIntervalAPI
decodeDyadicIntervalAPI =
  Json.Decode.succeed DyadicIntervalAPI
    |: ("dyadic_endpointL" := decodeDyadicS)
    |: ("dyadic_endpointR" := decodeDyadicS)

getApiFunctionByFunctionIdDomain : Int -> Task.Task Http.Error (DyadicIntervalAPI)
getApiFunctionByFunctionIdDomain functionId =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "api"
          ++ "/" ++ "function"
          ++ "/" ++ (functionId |> toString |> Http.uriEncode)
          ++ "/" ++ "domain"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeDyadicIntervalAPI
      (Http.send Http.defaultSettings request)

type alias FunctionPoint =
  { functionPointDom : DyadicIntervalAPI
  , functionPointValue : MPBallIntervalAPI
  }

type alias MPBallIntervalAPI =
  { mpBall_endpointL : MPBall
  , mpBall_endpointR : MPBall
  }

type alias MPBall =
  { ball_value : DyadicS
  , ball_error : DyadicS
  }

decodeFunctionPoint : Json.Decode.Decoder FunctionPoint
decodeFunctionPoint =
  Json.Decode.succeed FunctionPoint
    |: ("functionPointDom" := decodeDyadicIntervalAPI)
    |: ("functionPointValue" := decodeMPBallIntervalAPI)

decodeMPBallIntervalAPI : Json.Decode.Decoder MPBallIntervalAPI
decodeMPBallIntervalAPI =
  Json.Decode.succeed MPBallIntervalAPI
    |: ("mpBall_endpointL" := decodeMPBall)
    |: ("mpBall_endpointR" := decodeMPBall)

decodeMPBall : Json.Decode.Decoder MPBall
decodeMPBall =
  Json.Decode.succeed MPBall
    |: ("ball_value" := decodeDyadicS)
    |: ("ball_error" := decodeDyadicS)

getApiFunctionByFunctionIdValuesForSamplingBySamplingId : Int -> Int -> Task.Task Http.Error (List (FunctionPoint))
getApiFunctionByFunctionIdValuesForSamplingBySamplingId functionId samplingId =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "api"
          ++ "/" ++ "function"
          ++ "/" ++ (functionId |> toString |> Http.uriEncode)
          ++ "/" ++ "valuesForSampling"
          ++ "/" ++ (samplingId |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeFunctionPoint)
      (Http.send Http.defaultSettings request)

getApiFunctionByFunctionIdName : Int -> Task.Task Http.Error (String)
getApiFunctionByFunctionIdName functionId =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "api"
          ++ "/" ++ "function"
          ++ "/" ++ (functionId |> toString |> Http.uriEncode)
          ++ "/" ++ "name"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      Json.Decode.string
      (Http.send Http.defaultSettings request)