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
  { sampling_dom : ( DyadicS, DyadicS )
  , sampling_maxStep : DyadicS
  }

type alias DyadicS =
  { _dyadic_value : Int
  , _dyadic_exp : Int
  }

encodeSampling : Sampling -> Json.Encode.Value
encodeSampling x =
  Json.Encode.object
    [ ( "sampling_dom", Exts.Json.Encode.tuple2 encodeDyadicS encodeDyadicS x.sampling_dom )
    , ( "sampling_maxStep", encodeDyadicS x.sampling_maxStep )
    ]

encodeDyadicS : DyadicS -> Json.Encode.Value
encodeDyadicS x =
  Json.Encode.object
    [ ( "_dyadic_value", Json.Encode.int x._dyadic_value )
    , ( "_dyadic_exp", Json.Encode.int x._dyadic_exp )
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
    |: ("sampling_dom" := Json.Decode.tuple2 (,) decodeDyadicS decodeDyadicS)
    |: ("sampling_maxStep" := decodeDyadicS)

decodeDyadicS : Json.Decode.Decoder DyadicS
decodeDyadicS =
  Json.Decode.succeed DyadicS
    |: ("_dyadic_value" := Json.Decode.int)
    |: ("_dyadic_exp" := Json.Decode.int)

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

getApiFunctionByFunctionIdDomain : Int -> Task.Task Http.Error (<Tuple2 (DataType "DyadicS" (Record "DyadicS" (Product (Selector "_dyadic_value" (Field (Primitive "Int"))) (Selector "_dyadic_exp" (Field (Primitive "Int")))))) (DataType "DyadicS" (Record "DyadicS" (Product (Selector "_dyadic_value" (Field (Primitive "Int"))) (Selector "_dyadic_exp" (Field (Primitive "Int"))))))>)
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
      <Tuple2 (DataType "DyadicS" (Record "DyadicS" (Product (Selector "_dyadic_value" (Field (Primitive "Int"))) (Selector "_dyadic_exp" (Field (Primitive "Int")))))) (DataType "DyadicS" (Record "DyadicS" (Product (Selector "_dyadic_value" (Field (Primitive "Int"))) (Selector "_dyadic_exp" (Field (Primitive "Int"))))))>
      (Http.send Http.defaultSettings request)

getApiFunctionByFunctionIdValuesForSamplingBySamplingId : Int -> Int -> Task.Task Http.Error (List (<Tuple2 (Tuple2 (DataType "DyadicS" (Record "DyadicS" (Product (Selector "_dyadic_value" (Field (Primitive "Int"))) (Selector "_dyadic_exp" (Field (Primitive "Int")))))) (DataType "DyadicS" (Record "DyadicS" (Product (Selector "_dyadic_value" (Field (Primitive "Int"))) (Selector "_dyadic_exp" (Field (Primitive "Int"))))))) (Tuple2 (DataType "MPBall" (Record "MPBall" (Product (Selector "ball_value" (Field (DataType "DyadicS" (Record "DyadicS" (Product (Selector "_dyadic_value" (Field (Primitive "Int"))) (Selector "_dyadic_exp" (Field (Primitive "Int")))))))) (Selector "ball_error" (Field (DataType "DyadicS" (Record "DyadicS" (Product (Selector "_dyadic_value" (Field (Primitive "Int"))) (Selector "_dyadic_exp" (Field (Primitive "Int"))))))))))) (DataType "MPBall" (Record "MPBall" (Product (Selector "ball_value" (Field (DataType "DyadicS" (Record "DyadicS" (Product (Selector "_dyadic_value" (Field (Primitive "Int"))) (Selector "_dyadic_exp" (Field (Primitive "Int")))))))) (Selector "ball_error" (Field (DataType "DyadicS" (Record "DyadicS" (Product (Selector "_dyadic_value" (Field (Primitive "Int"))) (Selector "_dyadic_exp" (Field (Primitive "Int"))))))))))))>))
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
      (Json.Decode.list <Tuple2 (Tuple2 (DataType "DyadicS" (Record "DyadicS" (Product (Selector "_dyadic_value" (Field (Primitive "Int"))) (Selector "_dyadic_exp" (Field (Primitive "Int")))))) (DataType "DyadicS" (Record "DyadicS" (Product (Selector "_dyadic_value" (Field (Primitive "Int"))) (Selector "_dyadic_exp" (Field (Primitive "Int"))))))) (Tuple2 (DataType "MPBall" (Record "MPBall" (Product (Selector "ball_value" (Field (DataType "DyadicS" (Record "DyadicS" (Product (Selector "_dyadic_value" (Field (Primitive "Int"))) (Selector "_dyadic_exp" (Field (Primitive "Int")))))))) (Selector "ball_error" (Field (DataType "DyadicS" (Record "DyadicS" (Product (Selector "_dyadic_value" (Field (Primitive "Int"))) (Selector "_dyadic_exp" (Field (Primitive "Int"))))))))))) (DataType "MPBall" (Record "MPBall" (Product (Selector "ball_value" (Field (DataType "DyadicS" (Record "DyadicS" (Product (Selector "_dyadic_value" (Field (Primitive "Int"))) (Selector "_dyadic_exp" (Field (Primitive "Int")))))))) (Selector "ball_error" (Field (DataType "DyadicS" (Record "DyadicS" (Product (Selector "_dyadic_value" (Field (Primitive "Int"))) (Selector "_dyadic_exp" (Field (Primitive "Int"))))))))))))>)
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