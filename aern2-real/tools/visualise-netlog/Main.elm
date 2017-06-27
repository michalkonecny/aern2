module Main exposing (..)

import Http
import Platform.Sub as Sub exposing (..)
import Task

-- import Mouse
import Keyboard
import Window

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
import Char

import QANetLog exposing (..)

main : Program Never
main =
    program
        { init = initState ! initCmds
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

subscriptions : State -> Sub Msg
subscriptions s =
  let
    keyEvent code =
      case Char.fromCode code of
        '[' -> PrevEvent
        ']' -> NextEvent
        _ -> NoAction
  in
  Sub.batch
    [
      -- Mouse.clicks (\pos -> NextEvent)
      Keyboard.presses keyEvent
    , Window.resizes Resize
    ]

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
initCmds = [getLog, simulateResize]

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

simulateResize : Cmd Msg
simulateResize =
  Task.perform (\_ -> NoAction) Resize Window.size

type Msg
    = NoAction
    | Resize Window.Size
    | Err String
    | SetLog QANetLog NodePositions
    | NextEvent
    | PrevEvent

update : Msg -> State -> ( State, Cmd Msg )
update msg s =
  case msg of
    NoAction -> s ! []
    Resize size ->
      { s | plotCanvasSize = size } ! []
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
    PrevEvent ->
      case s.eventsDone of
        (lastEvent :: rest) ->
          { s |
            eventsDone = rest
          , eventsTodo = lastEvent :: s.eventsTodo
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
    pendingQueries = filterPendingQueries <| List.reverse s.eventsDone
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
          ++ drawPendingQueries s pendingQueries
          ++ drawCachedAnswers s s.eventsDone
          ++ case s.eventsTodo of
              nextEvent :: _ -> drawEvent s nextEvent
              _ -> []
      ]
      -- ++ case s.eventsTodo of
      --     nextEvent :: _ -> [Html.text (toString nextEvent)]
      --     _ -> []
      -- ++ [Html.text (toString pendingQueries)]

nodeW : Float
nodeW = 0.16

nodeH : Float
nodeH = 0.1

bgrColour : Color
bgrColour =
  -- white
  rgb 200 255 220

drawNode : State -> (Int, NodeInfo) -> List Form
drawNode s (nodeIx, _) =
  case getNodeInfoPos s (ValueId nodeIx) of
    Nothing -> []
    Just (info, pos) ->
      let
        { width, height } = s.plotCanvasSize
        w = toFloat width
        h = toFloat height
        nw = nodeW*w
        nh = nodeH*h
        srcN = List.length info.sources
        nodeBox = rect nw nh
        nameBox = rect (nw) (0.3*nh)
        nameMove = move (0.0, 0.35*nh)
      in
      [
        group
        [
          nodeBox |> filled white
        , nameBox |> filled (rgb 200 220 255) |> nameMove
        , nameBox |> outlined (solid black) |> nameMove
        , nodeBox |> outlined (solid black)
        ,
          Collage.text (Text.fromString (info.name))
          |> scale (h/500.0)
          |> move (0.0, nh*0.4)
        ]
        |> move pos
      ]
      ++ (List.concat <| List.map (drawLink s blue (ValueId nodeIx)) info.sources)

drawLink : State -> Color -> ValueId -> ValueId -> List Form
drawLink s c destId srcId =
  --  (destX, destY) srcN (srcK, srcId)
  case (getNodeInfoPos s srcId, getNodeInfoPos s destId) of
    (Just (srcInfo, (srcX,srcY)), Just (destInfo, (destX,destY))) ->
      let
        { width, height } = s.plotCanvasSize
        w = toFloat width
        h = toFloat height
        nw = nodeW*w
        nh = nodeH*h
        srcN = List.length destInfo.sources
        msrcK =
          Maybe.oneOf <|
          List.map2
            (\k sId -> if sId == srcId then Just k else Nothing)
            [0..(srcN-1)]
            destInfo.sources
        srcK =
          case msrcK of
            Just k -> k
            _ -> 0
        destYK =
          if srcN <= 1 then destY
            else destY + (0.4*nh)*(0.5-((toFloat srcK)/((toFloat srcN)-1)))
      in
      [
        path [(srcX+nw*0.5,srcY), (srcX+nw*0.55,srcY), (destX-nw*0.55,destYK), (destX-nw*0.5,destYK)]
        |> traced (solid c)
      ]
    _ -> []

getNodeInfoPos : State -> ValueId -> Maybe (NodeInfo, (Float,Float))
getNodeInfoPos s vId =
  case Dict.get (fromValueId vId) s.nodes of
    Nothing -> Nothing
    Just info ->
      case info.pos of
        Nothing -> Nothing
        Just (x,y) ->
          let
            { width, height } = s.plotCanvasSize
            w = toFloat width
            h = toFloat height
          in
          Just (info, ((x-0.5)*w, (y-0.5)*h))

drawEvent : State -> QANetLogItem -> List Form
drawEvent s event =
  let
    { width, height } = s.plotCanvasSize
    w = toFloat width
    h = toFloat height
    nw = nodeW*w
    nh = nodeH*h
    drawQuery q =
      case getNodeInfoPos s q.qaLogQuery_provider of
        Nothing -> []
        Just (info, pos) ->
          [
            Collage.text (Text.fromString ("? " ++ q.qaLogQuery_description) |> Text.color red)
            |> scale (h/500.0)
            |> move (0.0, nh*0.1)
            |> move pos
          ]
          ++
          case q.qaLogQuery_client of
            Nothing -> []
            Just clientId ->
              drawLink s red clientId q.qaLogQuery_provider
    drawAnswer a =
      case getNodeInfoPos s a.qaLogAnswer_provider of
        Nothing -> []
        Just (info, pos) ->
          [
            Collage.text (Text.fromString ("! " ++ a.qaLogAnswer_description) |> Text.color red)
            |> scale (h/500.0)
            |> move (0.0, -nh*0.3)
            |> move pos
          ]
          ++
          case a.qaLogAnswer_client of
            Nothing -> []
            Just clientId ->
              drawLink s red clientId a.qaLogAnswer_provider
  in
  case event of
    QANetLogCreate _ -> []
    QANetLogQuery q -> drawQuery q
    QANetLogAnswer a -> drawAnswer a

drawPendingQueries s pendingQueries =
  let
    { width, height } = s.plotCanvasSize
    w = toFloat width
    h = toFloat height
    nw = nodeW*w
    nh = nodeH*h
    drawQuery q =
      case getNodeInfoPos s q.qaLogQuery_provider of
        Nothing -> []
        Just (info, pos) ->
          [
            Collage.text (Text.fromString ("? " ++ q.qaLogQuery_description))
            |> scale (h/500.0)
            |> move (0.0, nh*0.1)
            |> move pos
          ]
  in
  List.concat <| List.map drawQuery pendingQueries

drawCachedAnswers s events =
  let
    { width, height } = s.plotCanvasSize
    w = toFloat width
    h = toFloat height
    nw = nodeW*w
    nh = nodeH*h
    drawAnswer event =
      case event of
        QANetLogAnswer a ->
          case getNodeInfoPos s a.qaLogAnswer_provider of
            Nothing -> []
            Just (info, pos) ->
              [
                Collage.text (Text.fromString ("! " ++ a.qaLogAnswer_description))
                |> scale (h/500.0)
                |> move (0.0, -nh*0.3)
                |> move pos
              ]
        _ -> []
  in
  List.concat <| List.map drawAnswer events


filterPendingQueries =
  let
    aux prevQueries events =
      case events of
        [] ->
          Dict.values prevQueries
        (QANetLogQuery q :: rest) ->
          aux (Dict.insert (fromValueId q.qaLogQuery_provider) q prevQueries) rest
        (QANetLogAnswer a :: rest) ->
          aux (Dict.remove (fromValueId a.qaLogAnswer_provider) prevQueries) rest
        (_ :: rest) -> aux prevQueries rest
  in
  aux Dict.empty
