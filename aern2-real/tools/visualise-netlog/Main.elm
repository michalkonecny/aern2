module Main exposing (..)

import Http
import Platform.Sub as Sub exposing (..)
import Task

import Json.Decode as Decode exposing (Decoder, (:=))

import Html exposing (..)
import Html.App exposing (program)
-- import Html.Attributes as A exposing (..)
-- import Html.Events exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Text

import List
import Dict

import QANetLog exposing (..)

main : Program Never
main =
    program
        { init = initState ! initCmds
        , update = update
        , subscriptions =
            (\s -> Sub.none)
              -- (\_ -> Window.resizes Resize)
        , view = view
        }

type alias QANetLog = List QANetLogItem

type alias State =
  {
    err : Maybe String
  , log : Maybe QANetLog
  , plotCanvasSize : { width : Pixels, height : Pixels }
  , nodes : Dict.Dict Int NodeInfo
  , eventsDone : List QANetLogItem
  , eventsTodo : List QANetLogItem
  }

type alias NodeInfo =
  {
    name : String
  , sources : List ValueId
  , pos : Maybe (Float, Float)
  }

type alias Pixels = Int

initState : State
initState =
  {
    err = Nothing
  , log = Nothing
  , plotCanvasSize = { width = 800, height = 600 }
  , nodes = Dict.empty
  , eventsDone = []
  , eventsTodo = []
  }

initCmds : List (Cmd Msg)
initCmds = [getLog]

jsonDecQANetLog : Decoder (List QANetLogItem)
jsonDecQANetLog = Decode.list jsonDecQANetLogItem

jsonDecNodePositions : Decoder (List NodePos)
jsonDecNodePositions = Decode.list jsonDecNodePos

jsonDecNodePos : Decoder NodePos
jsonDecNodePos =
  Decode.tuple3 (,,) Decode.int Decode.float Decode.float

type alias NodePos = (Int, Float, Float)
type alias NodePositions = List NodePos

logUrl : String
logUrl = "./netlog.json"

posUrl : String
posUrl = "./nodepos.json"

getLog : Cmd Msg
getLog =
  Task.map2
    SetLog
    (Http.get jsonDecQANetLog logUrl)
    (Http.get jsonDecNodePositions posUrl)
  |> Task.perform (\e -> Err (toString e)) (\t -> t)

-- simulateResize =
--   Task.perform (\_ -> NoAction) Resize Window.size

type Msg
    = NoAction
    | Err String
    | SetLog QANetLog NodePositions
    | NextEvent

update : Msg -> State -> ( State, Cmd Msg )
update msg s =
  case msg of
    NoAction -> s ! []
    Err e -> { s | err = Just e } ! []
    SetLog log nodepositions ->
      { s |
        log = Just log
      , nodes = getNodeInfo log nodepositions
      , eventsTodo = List.filter (not << isQANetLogCreate) log
      , eventsDone = []
      } ! []
    NextEvent ->
      case s.eventsTodo of
        (nextEvent :: rest) ->
          { s |
            eventsTodo = rest
          , eventsDone = nextEvent :: s.eventsDone
          } ! []
        _ -> s ! []

getNodeInfo : QANetLog -> NodePositions -> Dict.Dict Int NodeInfo
getNodeInfo log nodepositions =
  let
    nodeposDict =
      nodepositions |>
      List.map (\(i,x,y) -> (i,(x,y))) |>
      Dict.fromList
    getCreate logItem =
      case logItem of
        QANetLogCreate c -> [c]
        _ -> []
    makeNodeInfo c =
      let
        cId = fromValueId c.qaLogCreate_newId
        pos =
          case Dict.get cId nodeposDict of
            Just pos -> Just pos
            _ -> Nothing
      in
      { name = c.qaLogCreate_name, sources = c.qaLogCreate_sources, pos =  pos }
  in
    List.map getCreate log |>
    List.concat |>
    List.map (\c -> (fromValueId <| c.qaLogCreate_newId, makeNodeInfo c)) |>
    Dict.fromList

isQANetLogCreate : QANetLogItem -> Bool
isQANetLogCreate logItem =
  case logItem of
    QANetLogCreate c -> True
    _ -> False

fromValueId : ValueId -> Int
fromValueId (ValueId n) = n

view : State -> Html Msg
view s =
  let
    { width, height } = s.plotCanvasSize
  in
    div [] <|
      [
        case s.err of
          Just e -> Html.text (toString  e)
          _ -> Html.text ""
      ]
      -- ++ (List.map (\n -> Html.p [] [Html.text (toString n)]) (Dict.toList s.nodes))
      ++
      [
        toHtml <|
        container width height middle <|
        collage width height <|
          [ rect (toFloat width) (toFloat height)
              |> filled bgrColour
          ]
          ++
          (List.concat <| List.map (drawNode s) <| Dict.toList s.nodes)
      ]
      ++ case s.eventsTodo of
          nextEvent :: _ -> [Html.text (toString nextEvent)]
          _ -> []

bgrColour : Color
bgrColour =
  -- white
  rgb 200 255 220

drawNode : State -> (Int, NodeInfo) -> List Form
drawNode s (nodeId, info) =
  case info.pos of
    Nothing -> []
    Just (x,y) ->
      let
        { width, height } = s.plotCanvasSize
        w = toFloat width
        h = toFloat height
        nw = nodeW*w
        nh = nodeH*h
        pos = ((x-0.5)*w, (y-0.5)*h)
        srcN = List.length info.sources
        nodeBox = rect nw nh
        nameBox = rect (nw) (0.3*nh)
        nameMove = move (0.0, 0.35*nh)
      in
      [
        group
        [
          nodeBox |> filled white
        ,
          nameBox |> filled (rgb 200 220 255) |> nameMove
        ,
          nameBox |> outlined (solid black) |> nameMove
        ,
          nodeBox |> outlined (solid black)
        ,
          Collage.text (Text.fromString (info.name))
          |> scale (h/500.0)
          |> move (0.0, nh*0.4)
        ]
        |> move pos
      ]
      ++ (List.concat <| List.map (drawLink s pos srcN) (List.map2 (,) [0..(srcN-1)] info.sources))

drawLink : State -> (Float, Float) -> Int -> (Int, ValueId) -> List Form
drawLink s (destX, destY) srcN (srcK, srcId) =
  case Dict.get (fromValueId srcId) s.nodes of
    Nothing -> []
    Just info ->
      case info.pos of
        Nothing -> []
        Just (x,y) ->
          let
            { width, height } = s.plotCanvasSize
            w = toFloat width
            h = toFloat height
            nw = nodeW*w
            nh = nodeH*h
            (srcX, srcY) = ((x-0.5)*w, (y-0.5)*h)
            destYK =
              if srcN <= 1 then destY
                else destY + (0.4*nh)*(0.5-((toFloat srcK)/((toFloat srcN)-1)))
          in
          [
            path [(srcX+nw*0.5,srcY), (srcX+nw*0.55,srcY), (destX-nw*0.55,destYK), (destX-nw*0.5,destYK)]
            |> traced (solid blue)
          ]


nodeW : Float
nodeW = 0.16

nodeH : Float
nodeH = 0.1
