module Main exposing (..)

import Set exposing (Set)
import Dict exposing (Dict)
import List
import String
import Tuple2

import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes as A exposing (..)
import Html.Events exposing (..)
import Window

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Text

import Http
import Task exposing (Task, andThen)
import Process exposing (sleep)
import Time
import Platform.Cmd as Cmd
-- import AnimationFrame

import Api exposing (..)
import DInterval exposing (divDi, mulDi, dToFloat)

main : Program Never
main =
    program
        { init = initState ! initCmds
        , update = update
        , subscriptions =
              (\_ -> Window.resizes Resize)
        , view = view
        }

initCmds =
  [simulateResize]

simulateResize =
  Task.perform (\_ -> NoAction) Resize Window.size


-- MODEL

type alias State =
    { functionsIds : List FunctionId
    , functionsDetails : FunctionsDetails
    , functionsSegments : FunctionsSegments
    , serialCommandNext : Maybe (Cmd Msg) -- server commands to be sent serially
    , serialCommandActive : Bool
      -- invariant: serialCommandActive == False ==> serialCommandNext == Nothing
    , plotArea : Maybe PlotArea
    , plotCanvasSize : { width : Pixels, height : Pixels }
    , plotResolution : Pixels
    , windowSize : Maybe Window.Size
    , error : Maybe String
    }

initState : State
initState =
  { functionsIds = []
  , functionsDetails = Dict.empty
  , functionsSegments = Dict.empty
  , serialCommandNext = Nothing
  , serialCommandActive = False
  , plotArea = Nothing
  , plotCanvasSize = { width = 800, height = 800 }
  , plotResolution = 5
  , windowSize = Nothing
  , error = Nothing
  }

type alias Pixels = Int

type alias SamplingId = Int
type alias FunctionId = Int
type alias FunctionsIds = List FunctionId
type alias FunctionName = String
type alias FunctionDomain = DyadicIntervalAPI
type alias FunctionDetails =
  { name : FunctionName
  , colour : FunctionColour
  , domain : FunctionDomain
  }
type alias FunctionsDetails = Dict FunctionId FunctionDetails
type alias FunctionsSegments = Dict FunctionId (List FunctionSegmentF)

type alias FunctionSegmentF =
  { domL : Float
  , domR : Float
  , enclL : ((Float, Float), (Float, Float))
  , enclR : ((Float, Float), (Float, Float))
  }

functionSegmentF : FunctionSegment -> FunctionSegmentF
functionSegmentF pt =
  { domL = dToFloat pt.functionSegmentDom.dyadic_endpointL
  , domR = dToFloat pt.functionSegmentDom.dyadic_endpointR
  , enclL =
    (mpBallToFloatFloat pt.functionSegmentValueL.mpBall_endpointL,
     mpBallToFloatFloat pt.functionSegmentValueL.mpBall_endpointR)
  , enclR =
    (mpBallToFloatFloat pt.functionSegmentValueR.mpBall_endpointL,
     mpBallToFloatFloat pt.functionSegmentValueR.mpBall_endpointR)
  }

mpBallToFloatFloat : MPBall -> (Float, Float)
mpBallToFloatFloat b =
  let
    c = dToFloat b.ball_value
    e = dToFloat b.ball_error
  in (c - e, c + e)

type alias PlotArea =
  { domain : FunctionDomain
  , domL : Float
  , domR : Float
  , rangeL : Float
  , rangeR : Float
  }

initPlotArea : State -> FunctionDomain -> PlotArea
initPlotArea s plotDomain =
  { domain = plotDomain
  , domL = dToFloat plotDomain.dyadic_endpointL
  , domR = dToFloat plotDomain.dyadic_endpointR
  , rangeL = -0.0 -- TODO: derive from canvas ratio and plotDomain, centre on 0
  , rangeR = 1.1
  }

unionOfFunctionsDomains : State -> FunctionDomain
unionOfFunctionsDomains s =
  let
    fnDetails = s.functionsDetails
    domains = List.map (.domain) (Dict.values fnDetails)
    in
    DInterval.unions domains

-- FETCHING FUNCTION DATA FROM SERVER

-- request metadate about the list of functions to be plotted:
getFunctions =
  getAern2PlotFunction
  `andThen`
  (\fnIds ->
    getFunctionsDetails fnIds)

getFunctionsDetails : List FunctionId -> Task Http.Error (FunctionsIds, FunctionsDetails)
getFunctionsDetails fnIds =
  Task.map (\ l -> (fnIds, Dict.fromList l)) <|
    Task.sequence <| List.map getFunctionDetails fnIds

getFunctionDetails : FunctionId -> Task Http.Error (FunctionId, FunctionDetails)
getFunctionDetails fnId =
  Task.map (\fnDetails -> (fnId, fnDetails)) <|
    Task.map3
      (\ name colour domain -> { name = name, colour = colour, domain = domain })
        (getAern2PlotFunctionByFunctionIdName fnId)
        (getAern2PlotFunctionByFunctionIdColour fnId)
        (getAern2PlotFunctionByFunctionIdDomain fnId)

-- request segments to plot:

createNewSampling : State -> FunctionDomain -> Task Http.Error SamplingId
createNewSampling s plotDomain =
  let
    maxStep =
      ((DInterval.width plotDomain) `mulDi` s.plotResolution)
        `divDi` (s.plotCanvasSize.width)
    sampling = { sampling_dom' = plotDomain, sampling_maxStep = maxStep }
  in
  postAern2PlotSampling sampling

getFunctionsSegments : State -> FunctionDomain -> Task Http.Error (FunctionDomain, FunctionsSegments)
getFunctionsSegments s plotDomain =
  createNewSampling s plotDomain
  `andThen`
  (\ samplingId ->
    Task.map (\segs -> (plotDomain, segs)) <| getFunctionsSegmentsUsingSampling s samplingId)

getFunctionsSegmentsUsingSampling : State -> SamplingId -> Task Http.Error FunctionsSegments
getFunctionsSegmentsUsingSampling s samplingId =
  let
    fnIds = s.functionsIds
    getSegs fnId =
      Task.map (\segs -> (fnId, List.map functionSegmentF segs)) <|
        getAern2PlotFunctionByFunctionIdValuesForSamplingBySamplingId fnId samplingId
  in
  Task.map Dict.fromList <|
    Task.sequence <| List.map getSegs fnIds
    -- TODO: send the above list of requests in parallel

-- UPDATE

type Msg
    = NoAction
    | Functions (FunctionsIds, FunctionsDetails)
    | FunctionsSegments (FunctionDomain, FunctionsSegments)
    | Resize Window.Size
    | ResampleIfNoChange State
    | Error String

toServer : (a -> Msg) -> Task Http.Error a -> Cmd Msg
toServer tag task =
  Task.perform (Error << toString) (tag) task

viaUI tag task =
  Task.perform (\ _ -> NoAction) (tag) task

update : Msg -> State -> ( State, Cmd Msg )
update msg s =
  case msg of
    NoAction -> s ! []
    Functions (fnIds, fnDetails) ->
      let
        plotDomain =
          case s.plotArea of
            Just plotArea -> plotArea.domain
            _ -> unionOfFunctionsDomains s
        sWithFns =
          {s |
            functionsIds = fnIds
          , functionsDetails = fnDetails
          , plotArea = Just (initPlotArea s plotDomain)}
        cmdGetSegments =
          getFunctionsSegments sWithFns plotDomain |> toServer FunctionsSegments
      in
      { sWithFns | serialCommandNext = Nothing } ! [cmdGetSegments]
        -- serialCommandActive already True due to "Functions"
    FunctionsSegments (plotDomain, fnSegments) ->
      let
        s2 = { s | functionsSegments = fnSegments }
      in
      case s.serialCommandNext of
        Just cmd ->
          { s2 | serialCommandNext = Nothing } ! [cmd]
        -- serialCommandActive already True due to "FunctionsSegments"
        _ ->
          { s2 | serialCommandActive = False } ! []
    Resize size ->
      let
        s2 = { s | windowSize = Just size,  plotCanvasSize = size }
      in
      case s.windowSize of
        Nothing -> -- initial simulated resize
          { s2 | serialCommandActive = True } ! [toServer Functions getFunctions]
        Just oldSize ->
          if size == oldSize then s ! []
          else
            s2 ! [viaUI ResampleIfNoChange (sleep Time.second `andThen` (\() -> Task.succeed s2))]
    ResampleIfNoChange s2 ->
      let
        plotChanged =
          s.plotArea /= s2.plotArea
          || s.plotCanvasSize /= s2.plotCanvasSize
          || s.plotResolution /= s2.plotResolution
          || s.functionsDetails /= s2.functionsDetails
      in
      if plotChanged then s ! []
      else
        case s.plotArea of
          Nothing -> s ! []
          Just plotArea ->
            let
              cmdGetSegments =
                getFunctionsSegments s plotArea.domain |> toServer FunctionsSegments
            in
            if s.serialCommandActive then
              { s | serialCommandNext = Just cmdGetSegments } ! []
            else
              { s | serialCommandActive = True } ! [ cmdGetSegments ]
    Error msg ->
      { s | error = Just msg } ! []

-- VIEW

view : State -> Html Msg
view s =
  let
    { width, height } = s.plotCanvasSize
  in
    div [] <|
    [
    --   Html.text (toString s.functionSegments)
    -- ,
      -- slider { makeMsg = FromUI << ZoomLevel, minValue = 50, maxValue = 150, state = s }
      -- ,
      toHtml <|
      container width height middle <|
      collage width height <|
        [ rect (toFloat width) (toFloat height)
            |> filled bgrColour
        ] ++
        (List.concat <| List.map (drawFn s) <| Dict.toList s.functionsSegments)
    ]

-- slider { makeMsg, minValue, maxValue, state } =
--   div []
--     [ Html.text <| toString minValue
--     , input
--       [ type' "range"
--       , A.min <| toString minValue
--       , A.max <| toString maxValue
--       , value <| toString state.zoomLevel
--       , onInput (makeMsg << Result.withDefault 0 << String.toInt)
--       ] []
--     , Html.text <| toString maxValue
--     ]

drawFn s (fnId, segments) =
  let
    colour =
      case Dict.get fnId s.functionsDetails of
        Just d -> d.colour
        _ -> defaultPlotColour
  in
  List.concat <| List.map (drawSegment s colour) segments

drawSegment s colour segm =
  case s.plotArea of
    Nothing -> []
    Just plotArea ->
      let
        w = toFloat s.plotCanvasSize.width
        h = toFloat s.plotCanvasSize.height
        pAdL = plotArea.domL
        pAdR = plotArea.domR
        pArL = plotArea.rangeL
        pArR = plotArea.rangeR
        -- zoomRatio = 100 / (toFloat s.zoomLevel)
        toCoordX x = (w/2) * (2*x - pAdL - pAdR) / (pAdR - pAdL)
        toCoordY y = (h/2) * (2*y - pArL - pArR) / (pArR - pArL)
        domL = toCoordX segm.domL
        domR = toCoordX segm.domR
        domM = (domL + domR) / 2
        domW = domR - domL
        ((eLDD, eLDU), (eLUD, eLUU)) = Tuple2.mapBoth (Tuple2.mapBoth toCoordY) segm.enclL
        ((eRDD, eRDU), (eRUD, eRUU)) = Tuple2.mapBoth (Tuple2.mapBoth toCoordY) segm.enclR
        parallelogramU =
          polygon [(domL, eLUD), (domL,eLUU), (domR,eRUU), (domR,eRUD)]
        parallelogramUD =
          polygon [(domL, eLUU), (domL,eLDD), (domR,eRDD), (domR,eRUU)]
        parallelogramD =
          polygon [(domL, eLDD), (domL,eLDU), (domR,eRDU), (domR,eRDD)]
      in
      [
        parallelogramUD |> filled (enclosureFillColour colour)
      , parallelogramD |> outlined (enclosureOutlineStyle colour)
      , parallelogramU |> outlined (enclosureOutlineStyle colour)
      ]

bgrColour =
  -- white
  rgb 200 255 220

enclosureFillColour colour =
  let
    r = tr colour.functionColourR
    g = tr colour.functionColourG
    b = tr colour.functionColourB
    tr c = (3*255 + c) // 4
  in
  rgb r g b

enclosureOutlineStyle : FunctionColour -> LineStyle
enclosureOutlineStyle colour =
  let
    r = colour.functionColourR
    g = colour.functionColourG
    b = colour.functionColourB
  in
  { defaultLine |
    color = rgb r g b
  }

defaultPlotColour = FunctionColour 10 10 255
