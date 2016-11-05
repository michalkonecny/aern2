module Main exposing (..)

import Set exposing (Set)
import Dict exposing (Dict)
import List

import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Http
import Task exposing (Task, andThen)

import Api exposing (..)
import DInterval exposing (divDi, mulDi)

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
    , functionPoints : Dict FunctionId (List FunctionPoint)
    , plotDomain : Maybe DyadicIntervalAPI
    , plotArea : { w : Pixels, h : Pixels }
    , plotResolution : Pixels
    , error : Maybe String
    }

type alias Pixels = Int

initState : State
initState =
  { functionIds = []
  , functionDetails = Dict.empty
  , functionPoints = Dict.empty
  , plotDomain = Nothing
  , plotArea = { w = 800, h = 800 }
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
    maxStep = ((DInterval.width plotDomain) `mulDi` s.plotResolution) `divDi` s.plotArea.w
    sampling = { sampling_dom' = plotDomain, sampling_maxStep = maxStep }
    makeState fnPoints =
      { initState |
        functionIds = fnIds
      , functionDetails = fnDetails
      , functionPoints = fnPoints
      , plotDomain = Just plotDomain }
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
  div []
    <| [text (toString s)]
