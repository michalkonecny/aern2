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
import DInterval

main : Program Never
main =
    program
        { init = (initState, fetchFunctionIds)
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }

fetchFunctionIds =
  toServer Functions getFunctions

getFunctions =
  getApiFunction
  `andThen`
  (\fnIds -> Task.map (\dict -> (fnIds, dict)) (getFunctionsDetails fnIds))

getFunctionsDetails : List FunctionId -> Task Http.Error (Dict FunctionId FunctionDetails)
getFunctionsDetails fnIds =
  Task.map Dict.fromList (Task.sequence (List.map getFunctionDetails fnIds))

getFunctionDetails : FunctionId -> Task Http.Error (FunctionId, FunctionDetails)
getFunctionDetails fnId =
  Task.map2
    (\ name domain -> (fnId, { name = name, domain = domain }))
      (getApiFunctionByFunctionIdName fnId)
      (getApiFunctionByFunctionIdDomain fnId)

toServer : (a -> FromServer) -> Task Http.Error a -> Cmd Msg
toServer tag task =
  Task.perform (Error << toString) (FromServer << tag) task

-- MODEL

type alias FunctionId = Int
type alias FunctionName = String
type alias FunctionDetails =
  { name : FunctionName
  , domain : DyadicIntervalAPI
  }

type alias State =
    { functionIds : List FunctionId
    , functionDetails : Dict FunctionId FunctionDetails
    , plotDomain : Maybe DyadicIntervalAPI
    , error : Maybe String
    }

initState : State
initState =
  { functionIds = []
  , functionDetails = Dict.empty
  , plotDomain = Nothing
  , error = Nothing
  }

-- UPDATE

type Msg
    = FromServer FromServer
    | FromUI FromUI
    | Error String

type FromServer
    = Functions (List FunctionId, Dict FunctionId FunctionDetails)
    -- | NewItem Item
    -- | Delete ItemId

type FromUI
    = NoAction

update : Msg -> State -> ( State, Cmd Msg )
update msg s =
  case msg of
    FromServer fromServer ->
      case fromServer of
        Functions (fnIds, functionDetails) ->
          functionsInitState s fnIds functionDetails ! []
    FromUI fromUI ->
      case fromUI of
        NoAction -> s ! []
    Error msg ->
        { s | error = Just msg } ! []

functionsInitState s fnIds functionDetails =
  let
    domains = List.map (.domain) (Dict.values functionDetails)
    plotDomain = DInterval.unions domains
  in
  { s | functionIds = fnIds, functionDetails = functionDetails, plotDomain = Just plotDomain }

-- VIEW

view : State -> Html Msg
view s =
  div []
    <| [text (toString s)]
