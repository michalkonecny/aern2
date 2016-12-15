module Api exposing (..)

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
import String
import Task


getAern2PlotSampling : Task.Task Http.Error (List Int)
getAern2PlotSampling =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "aern2Plot"
          ++ "/" ++ "sampling"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list Json.Decode.int)
      (Http.send Http.defaultSettings request)

type alias Sampling =
  { sampling_dom' : DyadicIntervalAPI
  , sampling_maxStep : DyadicS
  }

type alias DyadicIntervalAPI =
  { dyadic_endpointL : DyadicS
  , dyadic_endpointR : DyadicS
  }

type alias DyadicS =
  { dyadic_value : Int
  , dyadic_exp : Int
  }

encodeSampling : Sampling -> Json.Encode.Value
encodeSampling x =
  Json.Encode.object
    [ ( "sampling_dom'", encodeDyadicIntervalAPI x.sampling_dom' )
    , ( "sampling_maxStep", encodeDyadicS x.sampling_maxStep )
    ]

encodeDyadicIntervalAPI : DyadicIntervalAPI -> Json.Encode.Value
encodeDyadicIntervalAPI x =
  Json.Encode.object
    [ ( "dyadic_endpointL", encodeDyadicS x.dyadic_endpointL )
    , ( "dyadic_endpointR", encodeDyadicS x.dyadic_endpointR )
    ]

encodeDyadicS : DyadicS -> Json.Encode.Value
encodeDyadicS x =
  Json.Encode.object
    [ ( "dyadic_value", Json.Encode.int x.dyadic_value )
    , ( "dyadic_exp", Json.Encode.int x.dyadic_exp )
    ]

postAern2PlotSampling : Sampling -> Task.Task Http.Error (Int)
postAern2PlotSampling body =
  let
    request =
      { verb =
          "POST"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "aern2Plot"
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
    |: ("sampling_dom'" := decodeDyadicIntervalAPI)
    |: ("sampling_maxStep" := decodeDyadicS)

decodeDyadicIntervalAPI : Json.Decode.Decoder DyadicIntervalAPI
decodeDyadicIntervalAPI =
  Json.Decode.succeed DyadicIntervalAPI
    |: ("dyadic_endpointL" := decodeDyadicS)
    |: ("dyadic_endpointR" := decodeDyadicS)

decodeDyadicS : Json.Decode.Decoder DyadicS
decodeDyadicS =
  Json.Decode.succeed DyadicS
    |: ("dyadic_value" := Json.Decode.int)
    |: ("dyadic_exp" := Json.Decode.int)

getAern2PlotSamplingBySamplingId : Int -> Task.Task Http.Error (Sampling)
getAern2PlotSamplingBySamplingId samplingId =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "aern2Plot"
          ++ "/" ++ "sampling"
          ++ "/" ++ (samplingId |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeSampling
      (Http.send Http.defaultSettings request)

getAern2PlotFunction : Task.Task Http.Error (List Int)
getAern2PlotFunction =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "aern2Plot"
          ++ "/" ++ "function"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list Json.Decode.int)
      (Http.send Http.defaultSettings request)

getAern2PlotFunctionByFunctionIdDomain : Int -> Task.Task Http.Error (DyadicIntervalAPI)
getAern2PlotFunctionByFunctionIdDomain functionId =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "aern2Plot"
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

type alias FunctionSegment =
  { functionSegmentDom : DyadicIntervalAPI
  , functionSegmentValueL : MPBallIntervalAPI
  , functionSegmentValueR : MPBallIntervalAPI
  }

type alias MPBallIntervalAPI =
  { mpBall_endpointL : MPBall
  , mpBall_endpointR : MPBall
  }

type alias MPBall =
  { ball_value : DyadicS
  , ball_error : DyadicS
  }

decodeFunctionSegment : Json.Decode.Decoder FunctionSegment
decodeFunctionSegment =
  Json.Decode.succeed FunctionSegment
    |: ("functionSegmentDom" := decodeDyadicIntervalAPI)
    |: ("functionSegmentValueL" := decodeMPBallIntervalAPI)
    |: ("functionSegmentValueR" := decodeMPBallIntervalAPI)

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

getAern2PlotFunctionByFunctionIdValuesForSamplingBySamplingId : Int -> Int -> Task.Task Http.Error (List (FunctionSegment))
getAern2PlotFunctionByFunctionIdValuesForSamplingBySamplingId functionId samplingId =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "aern2Plot"
          ++ "/" ++ "function"
          ++ "/" ++ (functionId |> toString |> Http.uriEncode)
          ++ "/" ++ "valuesForSampling"
          ++ "/" ++ (samplingId |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeFunctionSegment)
      (Http.send Http.defaultSettings request)

getAern2PlotFunctionByFunctionIdName : Int -> Task.Task Http.Error (String)
getAern2PlotFunctionByFunctionIdName functionId =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "aern2Plot"
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

type alias FunctionColour =
  { functionColourR : Int
  , functionColourG : Int
  , functionColourB : Int
  }

decodeFunctionColour : Json.Decode.Decoder FunctionColour
decodeFunctionColour =
  Json.Decode.succeed FunctionColour
    |: ("functionColourR" := Json.Decode.int)
    |: ("functionColourG" := Json.Decode.int)
    |: ("functionColourB" := Json.Decode.int)

getAern2PlotFunctionByFunctionIdColour : Int -> Task.Task Http.Error (FunctionColour)
getAern2PlotFunctionByFunctionIdColour functionId =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "aern2Plot"
          ++ "/" ++ "function"
          ++ "/" ++ (functionId |> toString |> Http.uriEncode)
          ++ "/" ++ "colour"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeFunctionColour
      (Http.send Http.defaultSettings request)