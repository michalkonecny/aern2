module Main where

import Control.Monad(void,when)
import Control.Lens

import System.Environment
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Backend.Diagrams(renderableToFile, FileOptions(..))

import ChartScaledLogAxis (scaledLogAxis)

--import Debug.Trace (trace)

main :: IO ()
main =
    do
    args <- getArgs
    let (mode, plotQuantity, inFileName, outFolder) = checkArgs args
    contents <- readFile inFileName
    let chartsData = parseBenchResults mode inFileName contents
    void $ mapM (renderChart mode plotQuantity outFolder) chartsData

data Mode = FnOpReprs | Ops
  deriving (Show, Read)

data PlotQuantity = MaxMem | ExecTime
  deriving (Show, Read)

checkArgs :: [String] -> (Mode, PlotQuantity, String, String)
checkArgs [modeS, plotQuantityS, inFileName, outFolder] =
  (read modeS, read plotQuantityS, inFileName, outFolder)
checkArgs _ =
  error "usage: aern2-bench-chart <mode(Ops|FnOpReprs)> <plotQuantity(MaxMem|ExecTime)> <csvFileName> <outFolder>"

{-|
    A dummy sample result:
@
    [
        ("sinesine+cos-Integrate-Dummy",
            [ ("Fun", [(10::Int,0.1::Double),(20,1.5),(30,100.5)]),
             ("Poly", [(10,0.1),(20,2.5),(30,10.5)])]),
        ("fracSin-Max-Dummy",
            [ ("Fun", [(10,0.1),(20,1.5),(30,100.5)]),
             ("Poly", [(10,0.1),(20,2.5),(30,10.5)])])
    ]
@
-}
parseBenchResults :: Mode -> String -> String -> [(String, [(String, [(Double, Double, Double)])])]
parseBenchResults FnOpReprs _inFileName csvContent =
    over (mapped._2.mapped._2) Set.toAscList $ -- convert the inner Sets to ascending lists (using Lens)
        over (mapped._2) Map.toList $ -- convert the inner Maps to lists
            Map.toList fnOp_To_Repr_To_Points -- convert the outer Map to list
    where
    fnOp_To_Repr_To_Points = mergeByFnOp_FnRepr records
    records = indexRecordsByKeysAndHeader ["Fn","Op","FnRepr"] $ parseCSV csvContent
parseBenchResults Ops inFileName csvContent =
    over (mapped._2.mapped._2) Set.toAscList $ -- convert the inner Sets to ascending lists (using Lens)
        over (mapped._2) Map.toList $ -- convert the inner Maps to lists
            Map.toList fnOp_To_Repr_To_Points -- convert the outer Map to list
    where
    fnOp_To_Repr_To_Points = Map.singleton inFileNameNoCSV $ mergeByOp records
    inFileNameNoCSV = take (length inFileName-4) inFileName
    records = indexRecordsByKeysAndHeader ["Op", "Count"] $ parseCSV csvContent

mergeByOp ::
    [([String], Map.Map String String)] -> Map.Map String (Set.Set (Double,Double,Double))
mergeByOp records =
    foldl insertRecord Map.empty records
    where
    insertRecord preMap ([opName, countS], fields) =
        Map.insertWith Set.union
            (opName ++ countS) (Set.singleton $ getPoint fields)
            preMap
    insertRecord _ _ = error "internal error in mergeByOp"

getPoint :: Map.Map String String -> (Double,Double,Double)
getPoint fields = (realToFrac (min 100 bits),utime+stime,maxram)
  where
  utime = read (lookupValue fields "UTime(s)") :: Double
  stime = read (lookupValue fields "STime(s)") :: Double
  maxram = read (lookupValue fields "Mem(kB)") :: Double
  bits :: Int
  bits
    | accS == "exact" = read (lookupValue fields "Parameters")
    | otherwise = read accS
    where
    accS = lookupValue fields "Accuracy(bits)"


mergeByFnOp_FnRepr ::
    [([String], Map.Map String String)] -> Map.Map String (Map.Map String (Set.Set (Double,Double,Double)))
mergeByFnOp_FnRepr records =
    foldl insertRecord Map.empty records
    where
    insertRecord preMap ([fnName,opName,reprName], fields) =
        Map.insertWith
            (Map.unionWith Set.union)
            chartTitle (Map.singleton reprName (Set.singleton $ getPoint fields))
            preMap
        where
        chartTitle = fnName ++ "-" ++ opName
    insertRecord _ _ = error "internal error in mergeByFnOp_FnRepr"

renderChart ::
    Mode -> PlotQuantity -> String -> (String, [(String, [(Double, Double, Double)])]) -> IO ()
renderChart mode plotQuantity outFolder (title, plotData) =
    void $
        renderableToFile fileOpts filePathTime $
            fillBackground def $
                gridToRenderable $ layoutToGrid (chartLayout mode)
    where
    filePathTime =
      case plotQuantity of
        MaxMem -> outFolder ++ "/" ++ title ++ "-space.svg"
        ExecTime -> outFolder ++ "/" ++ title ++ "-time.svg"
    chartLayout FnOpReprs =
        execEC $
        do
        layout_y_axis . laxis_generate .= scaledLogAxis def (minV,maxV)
        -- layout_y_axis . laxis_generate .= scaledAxis def (minV,maxV)
        layout_y_axis . laxis_title .= plotQuantityTitle
        layout_x_axis . laxis_generate .= scaledAxis def (minAC, maxAC)
        layout_x_axis . laxis_title .= "Accuracy (bits)"
        layout_x_axis . laxis_style . axis_label_gap .= 1
        mapM layoutPlotData $ zip [0..] plotData
    chartLayout Ops =
        execEC $
        do
        layout_y_axis . laxis_title .= plotQuantityTitle
        layout_x_axis . laxis_title .= "Requested Accuracy (bits)"
        layout_x_axis . laxis_style . axis_label_gap .= 1
        mapM layoutPlotData $ zip [0..] plotData
    (minAC, maxAC) = (0,100) :: (Double, Double)
    (minV,maxV) =
      case plotQuantity of
        MaxMem -> (1,1000000)
        ExecTime -> (0.01,1000)
    plotQuantityTitle =
      case plotQuantity of MaxMem -> "Space (kB)"; ExecTime -> "Time (s)"
    layoutPlotData (benchNum, (benchName, benchPoints)) =
        plotPoints name
          [(ac, case plotQuantity of MaxMem -> maxmem; ExecTime -> time)
            | (ac,time,maxmem) <- benchPoints]
        where
        (name, colour, shape) =
          case mode of
            FnOpReprs -> (reprShow benchName, reprColor benchName, reprShape benchName)
            Ops -> (benchName, stdColors !! benchNum, stdShapes !! benchNum)
        plotPoints ltitle values =
            do
            plot $ pointsLayout ltitle values
            plot $ linesLayout values
        linesLayout values =
            liftEC $
                do
                plot_lines_values .= [values]
                plot_lines_style . line_color .= opaque colour
        pointsLayout ltitle values =
            liftEC $
                do
                plot_points_values .= values
                plot_points_title .= ltitle
                plot_points_style . point_color .= opaque colour
                plot_points_style . point_shape .= shape
                plot_points_style . point_radius .= 3
                -- Show borders for unfilled shapes:
                when (not (isFilled shape)) $ do
                    plot_points_style . point_border_color .= opaque colour
                    plot_points_style . point_border_width .= 1
        isFilled :: PointShape -> Bool
        isFilled PointShapeCircle = True
        isFilled PointShapePolygon{} = True
        isFilled _ = False

fileOpts :: FileOptions
fileOpts = def { _fo_size = (300,300) }

reprShow :: String -> String
reprShow "dball" = "DBall"
reprShow "ball" = "Ball"
reprShow "fun" = "Fun"
reprShow "poly" = "Poly"
reprShow "ppoly" = "PPoly"
reprShow "frac" = "Frac"
reprShow "lpoly" = "LPoly"
reprShow "lppoly" = "LPPoly"
reprShow "lfrac" = "LFrac"
reprShow reprName = error $ "unknown representation " ++ reprName

reprColor :: String -> Colour Double
reprColor "dball" = powderblue
reprColor "ball" = darkblue
reprColor "fun" = grey
reprColor "poly" = orangered
reprColor "ppoly" = mediumvioletred
reprColor "frac" = darkgreen
reprColor "lpoly" = orangered
reprColor "lppoly" = mediumvioletred
reprColor "lfrac" = darkgreen
reprColor reprName = error $ "unknown representation " ++ reprName

reprShape :: String -> PointShape
reprShape "dball" = PointShapeEllipse 0.7 1.0
reprShape "ball" = PointShapeCircle
reprShape "fun" = PointShapeCross
reprShape "poly" = PointShapeCross
reprShape "ppoly" = PointShapeCircle
reprShape "frac" = PointShapeCircle
reprShape "lpoly" = PointShapeStar
reprShape "lppoly" = PointShapeStar
reprShape "lfrac" = PointShapeStar
reprShape reprName = error $ "unknown representation " ++ reprName

opShow :: String -> String
opShow s = s
-- opShow opName = error $ "unknown operation " ++ opName

stdShapes :: [PointShape]
stdShapes =
  cycle
  [
    PointShapeEllipse 0.7 1.0,
    PointShapeCircle,
    PointShapeCross,
    PointShapeStar
  ]

lookupValue ::
    Map.Map String String ->
    String ->
    String
lookupValue nameToValueMap name =
    case Map.lookup name nameToValueMap of
        Nothing -> error $ "field " ++ show name ++ " missing in record: " ++ show nameToValueMap
        Just value -> value

indexRecordsByKeysAndHeader ::
    [String] ->
    [[String]] ->
    [([String], Map.Map String String)]
indexRecordsByKeysAndHeader keys (header : records) =
    map getKeysAndMap records
    where
    getKeysAndMap record =
        (getKeys record, getMap record)
    getKeys record =
        map getKey keyIndices
        where
        getKey keyIx = record !! keyIx
    keyIndices =
        map keyIndex keys
    keyIndex key =
        case List.elemIndex key header of
            Nothing -> error $ "key " ++ show key ++ " not found in the header " ++ show header
            Just ix2 -> ix2
    getMap record =
        Map.fromList $ zip header record
indexRecordsByKeysAndHeader _ _ = error "internal error in indexRecordsByKeysAndHeader"

parseCSV :: String -> [[String]]
parseCSV contents =
    records
    where
    records =
        map parseLine $ lines contents
    parseLine line_ =
        state1 0 [] "" line_
        where
        -- expecting new field or end of line; initial state
        state1 _pos revPrevItems revPrevOutput [] =
            reverse $ reverse revPrevOutput : revPrevItems
        state1 _pos revPrevItems revPrevOutput "\x0D" = -- DOS end of line
            reverse $ reverse revPrevOutput : revPrevItems
        state1 pos revPrevItems revPrevOutput (',' : cs) =
            state1 (pos + 1) (reverse revPrevOutput : revPrevItems) "" cs
        state1 pos revPrevItems revPrevOutput ('"' : cs) =
            state3 (pos + 1) revPrevItems revPrevOutput cs
        state1 pos revPrevItems revPrevOutput (c : cs) =
            state2 (pos + 1) revPrevItems (c : revPrevOutput) cs

        -- reading a field with no double quotes
        state2 _pos revPrevItems revPrevOutput [] =
            reverse $ reverse revPrevOutput : revPrevItems
        state2 _pos revPrevItems revPrevOutput "\x0D" = -- DOS end of line
            reverse $ reverse revPrevOutput : revPrevItems
        state2 pos revPrevItems revPrevOutput (',' : cs) =
            state1 (pos + 1) (reverse revPrevOutput : revPrevItems) "" cs
        state2 pos revPrevItems revPrevOutput (c : cs) =
            state2 (pos + 1) revPrevItems (c : revPrevOutput) cs

        -- reading a field in double quotes
        state3 pos _revPrevItems _revPrevOutput [] =
            parseerror pos
        state3 pos revPrevItems revPrevOutput ('"' : cs) =
            state4 (pos + 1) revPrevItems revPrevOutput cs
        state3 pos revPrevItems revPrevOutput (c : cs) =
            state3 (pos + 1) revPrevItems (c : revPrevOutput) cs

        -- reading a field in double quotes and just found a double quote
        -- that could be the closing one or an inner one
        state4 _pos revPrevItems revPrevOutput [] =
            reverse $ reverse revPrevOutput : revPrevItems
        state4 _pos revPrevItems revPrevOutput "\x0D" = -- DOS end of line
            reverse $ reverse revPrevOutput : revPrevItems
        state4 pos revPrevItems revPrevOutput (',' : cs) =
            state1 (pos + 1) (reverse revPrevOutput : revPrevItems) "" cs
        state4 pos revPrevItems revPrevOutput (c : cs) =
            state3 (pos + 1) revPrevItems (c : revPrevOutput) cs

        parseerror pos =
            error $
                "parse error in CVS file at pos:\n"
                ++ take pos line_ ++ "\n"
                ++ replicate pos ' ' ++ drop pos line_

stdColors :: [Colour Double]
stdColors =
  [
    -- aliceblue,
    -- -- antiquewhite,
    -- aqua,
    -- aquamarine,
    -- -- azure,
    -- beige,
    -- bisque,
    -- -- black,
    -- blanchedalmond,
    -- blue,
    -- -- blueviolet,
    -- brown,
    -- burlywood,
    -- cadetblue,
    -- chartreuse,
    -- -- chocolate,
    -- coral,
    -- -- cornflowerblue,
    -- -- cornsilk,
    -- crimson,
    -- cyan,
    darkblue,
    darkcyan,
    darkgoldenrod,
    darkgray,
    darkgreen,
    darkgrey,
    darkkhaki,
    darkmagenta,
    darkolivegreen,
    darkorange,
    darkorchid,
    darkred,
    darksalmon,
    darkseagreen,
    darkslateblue,
    darkslategray,
    darkslategrey,
    darkturquoise,
    darkviolet,
    deeppink,
    deepskyblue,
    dimgray,
    dimgrey,
    dodgerblue,
    firebrick,
    floralwhite,
    forestgreen,
    fuchsia,
    gainsboro,
    ghostwhite,
    gold,
    goldenrod,
    gray,
    grey,
    green,
    greenyellow,
    honeydew,
    hotpink,
    indianred,
    indigo,
    ivory,
    khaki,
    lavender,
    lavenderblush,
    lawngreen,
    lemonchiffon,
    -- lightblue,
    -- lightcoral,
    -- lightcyan,
    -- lightgoldenrodyellow,
    -- lightgray,
    -- lightgreen,
    -- lightgrey,
    -- lightpink,
    -- lightsalmon,
    -- lightseagreen,
    -- lightskyblue,
    -- lightslategray,
    -- lightslategrey,
    -- lightsteelblue,
    -- lightyellow,
    lime,
    limegreen,
    linen,
    magenta,
    maroon,
    mediumaquamarine,
    mediumblue,
    mediumorchid,
    mediumpurple,
    mediumseagreen,
    mediumslateblue,
    mediumspringgreen,
    mediumturquoise,
    mediumvioletred,
    midnightblue,
    mintcream,
    mistyrose,
    moccasin,
    navajowhite,
    navy,
    oldlace,
    olive,
    olivedrab,
    orange,
    orangered,
    orchid,
    palegoldenrod,
    palegreen,
    paleturquoise,
    palevioletred,
    papayawhip,
    peachpuff,
    peru,
    pink,
    plum,
    powderblue,
    purple,
    red,
    rosybrown,
    royalblue,
    saddlebrown,
    salmon,
    sandybrown,
    seagreen,
    seashell,
    sienna,
    -- silver,
    skyblue,
    slateblue,
    slategray,
    slategrey,
    snow,
    springgreen,
    steelblue,
    teal,
    thistle,
    tomato,
    turquoise,
    violet,
    wheat,
    -- white,
    -- whitesmoke,
    -- yellow,
    yellowgreen
  ]
