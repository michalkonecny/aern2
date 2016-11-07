module Main exposing (..)

import Set exposing (Set)
import Dict exposing (Dict)
import List

import Html exposing (..)
import Html.App exposing (program)
-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Text

import Http
import Task exposing (Task, andThen)

import Api exposing (..)
import DInterval exposing (divDi, mulDi, dToFloat)

main : Program Never
main =
    program
        { init = (initState, fetchFunctions initState)
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }

-- MODEL

type alias SamplingId = Int
type alias FunctionId = Int
type alias FunctionName = String
type alias FunctionDetails =
  { name : FunctionName
  , domain : DyadicIntervalAPI
  , points : List FunctionPoint
  }

type alias State =
    { functionIds : List FunctionId
    , functionDetails : Dict FunctionId FunctionDetails
    , functionPoints : Dict FunctionId (List FunctionPointF)
    , plotArea : Maybe PlotArea
    , plotCanvasSize : { w : Pixels, h : Pixels }
    , plotResolution : Pixels
    , error : Maybe String
    }

type alias FunctionPointF =
  { domL : Float
  , domR : Float
  , valL : (Float, Float)
  , valR : (Float, Float)
  }

functionPointF : FunctionPoint -> FunctionPointF
functionPointF pt =
  { domL = dToFloat pt.functionPointDom.dyadic_endpointL
  , domR = dToFloat pt.functionPointDom.dyadic_endpointR
  , valL = mpBallToFloatFloat pt.functionPointValue.mpBall_endpointL
  , valR = mpBallToFloatFloat pt.functionPointValue.mpBall_endpointR
  }

mpBallToFloatFloat : MPBall -> (Float, Float)
mpBallToFloatFloat b =
  let
    c = dToFloat b.ball_value
    e = dToFloat b.ball_error
  in (c - e, c + e)

type alias PlotArea =
  { domain : DyadicIntervalAPI
  , domL : Float
  , domR : Float
  , rangeL : Float
  , rangeR : Float
  }

type alias Pixels = Int


initState : State
initState =
  { functionIds = []
  , functionDetails = Dict.empty
  , functionPoints = Dict.empty
  , plotArea = Nothing
  , plotCanvasSize = { w = 800, h = 800 }
  , plotResolution = 100 -- TODO: change to 2 pixels
  , error = Nothing
  }

-- INITIALISATION

fetchFunctions s =
  toServer Functions (getFunctions s)

getFunctions s =
  getApiFunction
  `andThen`
  (\fnIds -> Task.map (\fnDetails -> (fnIds, fnDetails)) (getFunctionsDetails fnIds))
  `andThen`
  getFunctionsPoints s

getFunctionsDetails : List FunctionId -> Task Http.Error (Dict FunctionId FunctionDetails)
getFunctionsDetails fnIds =
  Task.map Dict.fromList (Task.sequence (List.map getFunctionDetails fnIds))

getFunctionDetails : FunctionId -> Task Http.Error (FunctionId, FunctionDetails)
getFunctionDetails fnId =
  Task.map (\fnDetails -> (fnId, fnDetails)) <|
    Task.map2
      (\ name domain -> { name = name, domain = domain, points = [] })
        (getApiFunctionByFunctionIdName fnId)
        (getApiFunctionByFunctionIdDomain fnId)

getFunctionsPoints : State -> (List FunctionId, Dict FunctionId FunctionDetails)
  -> Task Http.Error State
getFunctionsPoints s (fnIds, fnDetails) =
  let
    domains = List.map (.domain) (Dict.values fnDetails)
    plotDomain = DInterval.unions domains
    maxStep = ((DInterval.width plotDomain) `mulDi` s.plotResolution) `divDi` s.plotCanvasSize.w
    sampling = { sampling_dom' = plotDomain, sampling_maxStep = maxStep }
    plotArea =
      { domain = plotDomain
      , domL = dToFloat plotDomain.dyadic_endpointL
      , domR = dToFloat plotDomain.dyadic_endpointR
      , rangeL = -1.0 -- TODO
      , rangeR = 1.0
      }
    makeState fnPoints =
      { initState |
        functionIds = fnIds
      , functionDetails = fnDetails
      , functionPoints = Dict.map (\ _ -> List.map functionPointF) fnPoints
      , plotArea = Just plotArea }
  in
  Task.map makeState <|
    postApiSampling sampling
    `andThen`
    getFunctionsPointsUsingSampling fnIds

getFunctionsPointsUsingSampling : (List FunctionId) -> SamplingId -> Task Http.Error (Dict FunctionId (List FunctionPoint))
getFunctionsPointsUsingSampling fnIds samplingId =
  let
    getFunctionPoints fnId =
      Task.map (\pts -> (fnId, pts)) <|
        getApiFunctionByFunctionIdValuesForSamplingBySamplingId fnId samplingId
  in
  Task.map Dict.fromList (Task.sequence (List.map getFunctionPoints fnIds))

toServer : (a -> FromServer) -> Task Http.Error a -> Cmd Msg
toServer tag task =
  Task.perform (Error << toString) (FromServer << tag) task

-- UPDATE

type Msg
    = FromServer FromServer
    | FromUI FromUI
    | Error String

type FromServer
    = Functions State
    -- | NewItem Item
    -- | Delete ItemId

type FromUI
    = NoAction

update : Msg -> State -> ( State, Cmd Msg )
update msg s =
  case msg of
    FromServer fromServer ->
      case fromServer of
        Functions s' -> s' ! []
    FromUI fromUI ->
      case fromUI of
        NoAction -> s ! []
    Error msg ->
        { s | error = Just msg } ! []

-- VIEW

view : State -> Html Msg
view s =
  let
    width = s.plotCanvasSize.w
    height = s.plotCanvasSize.h
  in
    toHtml <|
    container width height middle <|
    collage width height <|
      [ rect (toFloat width) (toFloat height)
          |> filled bgrColour
      ] ++
      (List.concat <| List.map (drawFn s) <| Dict.toList s.functionPoints)

drawFn s (fnId, points) =
  List.concat <| List.map (drawPoint s) points

drawPoint s point =
  case s.plotArea of
    Nothing -> []
    Just plotArea ->
      let
        w = toFloat s.plotCanvasSize.w
        h = toFloat s.plotCanvasSize.h
        pAdL = plotArea.domL
        pAdR = plotArea.domR
        pArL = plotArea.rangeL
        pArR = plotArea.rangeR
        toCoordX x = (w/2) * (2*x - pAdL - pAdR) / (pAdR - pAdL)
        toCoordY y = (h/2) * (2*y - pArL - pArR) / (pArR - pArL)
        domL = toCoordX point.domL
        domR = toCoordX point.domR
        domM = (domL + domR) / 2
        domW = domR - domL
        vLL = toCoordY <| fst point.valL
        vLR = toCoordY <| snd point.valL
        vLc = (vLL + vLR)/2
        vLd = vLR - vLL
        vRL = toCoordY <| fst point.valR
        vRR = toCoordY <| snd point.valR
        vRc = (vRL + vRR)/2
        vRd = vRR - vRL
      in
      [
        rect domW vLd |> filled enclosureFillColour |> move (domM, vLc)
      , rect domW vLd |> outlined enclosureOutlineStyle |> move (domM, vLc)
      , rect domW vRd |> filled enclosureFillColour |> move (domM, vRc)
      , rect domW vRd |> outlined enclosureOutlineStyle |> move (domM, vRc)
      ]

bgrColour =
  -- white
  rgb 200 255 200

enclosureFillColour =
  rgb 200 200 255

enclosureOutlineStyle : LineStyle
enclosureOutlineStyle =
  { defaultLine |
    color = rgb 10 10 255
  }
