module Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


getAern2PlotSampling : Http.Request (List (Int))
getAern2PlotSampling =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "aern2Plot"
                , "sampling"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list int)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postAern2PlotSampling : Sampling -> Http.Request (Int)
postAern2PlotSampling body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "aern2Plot"
                , "sampling"
                ]
        , body =
            Http.jsonBody (encodeSampling body)
        , expect =
            Http.expectJson int
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getAern2PlotSamplingBySamplingId : Int -> Http.Request (Sampling)
getAern2PlotSamplingBySamplingId capture_samplingId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "aern2Plot"
                , "sampling"
                , capture_samplingId |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeSampling
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getAern2PlotFunction : Http.Request (List (Int))
getAern2PlotFunction =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "aern2Plot"
                , "function"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list int)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getAern2PlotFunctionByFunctionIdDomain : Int -> Http.Request (DyadicIntervalAPI)
getAern2PlotFunctionByFunctionIdDomain capture_functionId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "aern2Plot"
                , "function"
                , capture_functionId |> toString |> Http.encodeUri
                , "domain"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeDyadicIntervalAPI
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getAern2PlotFunctionByFunctionIdValuesForSamplingBySamplingId : Int -> Int -> Http.Request (List (FunctionSegment))
getAern2PlotFunctionByFunctionIdValuesForSamplingBySamplingId capture_functionId capture_samplingId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "aern2Plot"
                , "function"
                , capture_functionId |> toString |> Http.encodeUri
                , "valuesForSampling"
                , capture_samplingId |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeFunctionSegment)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getAern2PlotFunctionByFunctionIdName : Int -> Http.Request (String)
getAern2PlotFunctionByFunctionIdName capture_functionId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "aern2Plot"
                , "function"
                , capture_functionId |> toString |> Http.encodeUri
                , "name"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson string
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getAern2PlotFunctionByFunctionIdColour : Int -> Http.Request (FunctionColour)
getAern2PlotFunctionByFunctionIdColour capture_functionId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "aern2Plot"
                , "function"
                , capture_functionId |> toString |> Http.encodeUri
                , "colour"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeFunctionColour
        , timeout =
            Nothing
        , withCredentials =
            False
        }