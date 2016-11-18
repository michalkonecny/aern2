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
              (\_ -> Window.resizes (FromUI << Resize))
        , view = view
        }

-- MODEL

type alias SamplingId = Int
type alias FunctionId = Int
type alias FunctionName = String
type alias FunctionDetails =
  { name : FunctionName
  , domain : DyadicIntervalAPI
  , points : List FunctionSegment
  }

type alias State =
    { functionIds : List FunctionId
    , functionDetails : Dict FunctionId FunctionDetails
    , functionSegments : Dict FunctionId (List FunctionSegmentF)
    , plotArea : Maybe PlotArea
    , plotCanvasSize : { width : Pixels, height : Pixels }
    , plotResolution : Pixels
    , samplingId : Maybe SamplingId -- derived from the 3 above
    , zoomLevel : Percent
    , windowSize : Maybe Window.Size
    , error : Maybe String
    }

initState : State
initState =
  { functionIds = []
  , functionDetails = Dict.empty
  , functionSegments = Dict.empty
  , plotArea = Nothing
  , plotCanvasSize = { width = 800, height = 800 }
  , plotResolution = 5
  , samplingId = Nothing
  , zoomLevel = 100
  , windowSize = Nothing
  , error = Nothing
  }

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
  { domain : DyadicIntervalAPI
  , domL : Float
  , domR : Float
  , rangeL : Float
  , rangeR : Float
  }

type alias Pixels = Int
type alias Percent = Int

initCmds =
  [simulateResize]

simulateResize =
  Task.perform (\_ -> FromUI NoAction) (FromUI << Resize) Window.size

-- FETCHING FUNCTION DATA FROM SERVER

fetchFunctions s =
  toServer Functions (getFunctions s)

getFunctions s =
  getAern2PlotFunction
  `andThen`
  (\fnIds ->
    getFunctionsDetails fnIds
    `andThen`
    (\fnDetails ->
      getFunctionsSegmentsWholeDomain
        { s | functionIds = fnIds, functionDetails = fnDetails }))

getFunctionsDetails : List FunctionId -> Task Http.Error (Dict FunctionId FunctionDetails)
getFunctionsDetails fnIds =
  Task.map Dict.fromList (Task.sequence (List.map getFunctionDetails fnIds))

getFunctionDetails : FunctionId -> Task Http.Error (FunctionId, FunctionDetails)
getFunctionDetails fnId =
  Task.map (\fnDetails -> (fnId, fnDetails)) <|
    Task.map2
      (\ name domain -> { name = name, domain = domain, points = [] })
        (getAern2PlotFunctionByFunctionIdName fnId)
        (getAern2PlotFunctionByFunctionIdDomain fnId)

getFunctionsSegmentsWholeDomain : State -> Task Http.Error State
getFunctionsSegmentsWholeDomain s =
  let
    fnDetails = s.functionDetails
    domains = List.map (.domain) (Dict.values fnDetails)
    plotDomain = DInterval.unions domains
  in
  getFunctionsSegmentsNewPlotArea s plotDomain

getFunctionsSegmentsNewPlotArea : State -> DyadicIntervalAPI -> Task Http.Error State
getFunctionsSegmentsNewPlotArea s plotDomain =
  let
    maxStep =
      ((DInterval.width plotDomain) `mulDi` (s.plotResolution * s.zoomLevel))
        `divDi` ((s.plotCanvasSize.width * 100) )
    sampling = { sampling_dom' = plotDomain, sampling_maxStep = maxStep }
    plotArea =
      { domain = plotDomain
      , domL = dToFloat plotDomain.dyadic_endpointL
      , domR = dToFloat plotDomain.dyadic_endpointR
      , rangeL = -0.0 -- TODO
      , rangeR = 1.0
      }
    sWithPlotArea = { s | plotArea = Just plotArea }
    fnIds = s.functionIds
  in
  postAern2PlotSampling sampling
  `andThen`
  (\ samplingId ->
    getFunctionsSegmentsUsingSampling { sWithPlotArea | samplingId = Just samplingId } fnIds)

getFunctionsSegmentsUsingSampling : State -> (List FunctionId) -> Task Http.Error State
getFunctionsSegmentsUsingSampling s fnIds =
  case s.samplingId of
    Nothing -> Task.succeed s
    Just samplingId ->
      let
        getFunctionSegments fnId =
          Task.map (\segs -> (fnId, List.map functionSegmentF segs)) <|
            getAern2PlotFunctionByFunctionIdValuesForSamplingBySamplingId fnId samplingId
      in
      Task.map (\list -> { s | functionSegments = Dict.fromList list } ) <|
        Task.sequence <| List.map getFunctionSegments fnIds

-- UPDATE

type Msg
    = FromServer FromServer
    | FromUI FromUI
    | Error String

type FromServer
    = Functions State
    -- | NewItem Item
    -- | Delete ItemId

toServer : (a -> FromServer) -> Task Http.Error a -> Cmd Msg
toServer tag task =
  Task.perform (Error << toString) (FromServer << tag) task

type FromUI
  = NoAction
  | Resize Window.Size
  | ResizeResampleIfNoChange Window.Size
  -- | ZoomLevel Percent
  -- | ZoomLevelResampleIfNoChange Percent

viaUI tag task =
  Task.perform (\ _ -> FromUI NoAction) (FromUI << tag) task

update : Msg -> State -> ( State, Cmd Msg )
update msg s =
  case msg of
    FromServer fromServer ->
      case fromServer of
        Functions s' -> { s' | zoomLevel = s.zoomLevel } ! []
    FromUI fromUI ->
      case fromUI of
        Resize size ->
          case s.windowSize of
            Nothing -> -- initial simulated resize
              s ! [fetchFunctions { initState | windowSize = Just size,  plotCanvasSize = size  }]
            Just _ ->
              let
                s2 = { s | windowSize = Just size,  plotCanvasSize = size }
              in
              s2 ! [viaUI ResizeResampleIfNoChange (sleep Time.second `andThen` (\() -> Task.succeed size))]
        ResizeResampleIfNoChange size ->
          if s.windowSize /= Just size then s ! []
          else
            case s.plotArea of
              Just plotArea ->
                s ! [toServer Functions (getFunctionsSegmentsNewPlotArea s plotArea.domain)]
              Nothing -> s ! []
        -- ZoomLevel percent ->
        --   let s' = { s | zoomLevel = percent } in
        --   s' ! [viaUI ZoomLevelResampleIfNoChange (sleep Time.second `andThen` (\() -> Task.succeed percent))]
        -- ZoomLevelResampleIfNoChange percent ->
        --   if s.zoomLevel /= percent then s ! []
        --   else
        --     case s.plotArea of
        --       Just plotArea ->
        --         s ! [toServer Functions (getFunctionsSegmentsNewPlotArea s plotArea.domain)]
        --       Nothing -> s ! []
        NoAction -> s ! []
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
        (List.concat <| List.map (drawFn s) <| Dict.toList s.functionSegments)
    ]

slider { makeMsg, minValue, maxValue, state } =
  div []
    [ Html.text <| toString minValue
    , input
      [ type' "range"
      , A.min <| toString minValue
      , A.max <| toString maxValue
      , value <| toString state.zoomLevel
      , onInput (makeMsg << Result.withDefault 0 << String.toInt)
      ] []
    , Html.text <| toString maxValue
    ]

drawFn s (fnId, segments) =
  List.concat <| List.map (drawSegment s) segments

drawSegment s segm =
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
        zoomRatio = 100 / (toFloat s.zoomLevel)
        toCoordX x = zoomRatio * (w/2) * (2*x - pAdL - pAdR) / (pAdR - pAdL)
        toCoordY y = zoomRatio * (h/2) * (2*y - pArL - pArR) / (pArR - pArL)
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
        parallelogramUD |> filled enclosureFillColour
      , parallelogramD |> outlined enclosureOutlineStyle
      , parallelogramU |> outlined enclosureOutlineStyle
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
