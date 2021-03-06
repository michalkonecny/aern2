module Main (main) where

import Control.Monad(void,when)
import Control.Lens

import System.Environment
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

-- import Data.Convertible

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Backend.Diagrams(renderableToFile, FileOptions(..))

import ChartTweaks (scaledLogAxis, scaledAxisExtraXSteps)

--import Debug.Trace (trace)

main :: IO ()
main =
    do
    args <- getArgs
    let (mode, inFileName, outFolder) = checkArgs args
    print mode
    contents <- readFile inFileName
    let chartsData =  
            -- translateLogToLin mode $ 
            parseBenchResults mode inFileName contents
    void $ mapM (renderChart mode outFolder) chartsData

data ChartId = CSVName | FnOp
  deriving (Show, Read)

data LineId = FnRepr | OpCount | Method
  deriving (Show, Read)

data AxisContent = BenchN | Accuracy | MaxMem | ExecTime
  deriving (Show, Read)

-- data AxisMode = LogFromTo Double Double | LinFromTo Double Double
-- data AxisMode = Log | Lin | LogFromTo Double Double | LinFromTo Double Double
data AxisMode = LogFromTo Double Double | LinFromTo Double Double
  deriving (Show, Read)

type Mode = (ChartId, LineId, (AxisContent, AxisMode), (AxisContent, AxisMode))

lineKeys :: LineId -> [String]
lineKeys OpCount = ["Op", "Count"]
lineKeys Method = ["Method"]
lineKeys FnRepr = ["FnRepr"]

checkArgs :: [String] -> (Mode, String, String)
checkArgs [chartIdS, lineIdS, xContS, xModeS, yContS, yModeS, inFileName, outFolder] =
  ((read chartIdS, read lineIdS, (read xContS, read xModeS), (read yContS, read yModeS)), inFileName, outFolder)
checkArgs _ =
  error "usage: aern2-bench-chart <chartsBy(CSVName|FnOp)> <linesBy(OpCount|Method|FnRepr)> <xAxis(Accuracy|BenchN)> <Lin|\"LinFromTo n m\"|Log|\"LogFromTo n m\"> <yAxis(MaxMem|ExecTime)> <Lin|\"LinFromTo n m\"|Log|\"LogFromTo n m\"> <csvFileName> <outFolder>"

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
parseBenchResults :: Mode -> String -> String -> [(String, [(String, [(Double, Double)])])]
parseBenchResults mode@(FnOp, lineId, _, _) _inFileName csvContent =
    over (mapped._2.mapped._2) Set.toAscList $ -- convert the inner Sets to ascending lists (using Lens)
        over (mapped._2) Map.toList $ -- convert the inner Maps to lists
            Map.toList fnOp_To_Repr_To_Points -- convert the outer Map to list
    where
    fnOp_To_Repr_To_Points = mergeByFnOp_FnRepr mode records
    records = indexRecordsByKeysAndHeader (["Fn", "Op"] ++ lineKeys lineId) $ parseCSV csvContent
parseBenchResults mode@(CSVName, lineId, _, _) inFileName csvContent =
    over (mapped._2.mapped._2) Set.toAscList $ -- convert the inner Sets to ascending lists (using Lens)
        over (mapped._2) Map.toList $ -- convert the inner Maps to lists
            Map.toList fnOp_To_Repr_To_Points -- convert the outer Map to list
    where
    fnOp_To_Repr_To_Points = Map.singleton inFileNameNoCSV $ mergeByKeys mode records
    inFileNameNoCSV = take (length inFileName-4) inFileName
    records = indexRecordsByKeysAndHeader (lineKeys lineId) $ parseCSV csvContent

-- translateLogToLin :: Mode -> [(String, [(String, [(Double, Double)])])] -> [(String, [(String, [(Double, Double)])])]
-- translateLogToLin mode dat = over (mapped._2.mapped._2.mapped) translateCoords dat
--     where
--     xMode = mode ^. _3 . _2
--     yMode = mode ^. _4 . _2
--     xShouldTranslate = getShouldTranslate xMode
--     yShouldTranslate = getShouldTranslate yMode
--     -- mode2 = mode & _3 . _2 .~ xMode2 & _4 . _2 .~ yMode2
--     -- getShouldTranslate Log = True
--     getShouldTranslate (LogFromTo _ _) = True
--     getShouldTranslate _ = False
--     translateCoords (x,y) = 
--         (if xShouldTranslate then logBase 10 x else x,
--          if yShouldTranslate then logBase 10 y else y)

mergeByFnOp_FnRepr ::
    Mode -> [([String], Map.Map String String)] -> Map.Map String (Map.Map String (Set.Set (Double,Double)))
mergeByFnOp_FnRepr mode records =
    foldl insertRecord Map.empty records
    where
    insertRecord preMap (fnName : opName : lKeys, fields) =
        Map.insertWith
            (Map.unionWith Set.union)
            chartTitle (Map.singleton (concat lKeys) (Set.singleton $ getPoint mode fields))
            preMap
        where
        chartTitle = fnName ++ "-" ++ opName
    insertRecord _ _ = error "internal error in mergeByFnOp_FnRepr"

mergeByKeys ::
    Mode -> [([String], Map.Map String String)] -> Map.Map String (Set.Set (Double,Double))
mergeByKeys mode records =
    foldl insertRecord Map.empty records
    where
    insertRecord preMap (keys, fields) =
        Map.insertWith Set.union
            (concat keys) (Set.singleton $ getPoint mode fields)
            preMap
    -- insertRecord _ _ = error "internal error in mergeByKeys"

getPoint :: Mode -> Map.Map String String -> (Double,Double)
getPoint (_, _, xAxis, yAxis) fields = (pt xAxis, pt yAxis)
  where
  pt (BenchN, _) = benchParams
  pt (Accuracy, _) = realToFrac (min 100 bits)
  pt (MaxMem, _) = maxram
  pt (ExecTime, _) = utime+stime
  benchParams = read (lookupValue fields "BenchParams") :: Double
  utime = read (lookupValue fields "UTime(s)") :: Double
  stime = read (lookupValue fields "STime(s)") :: Double
  maxram = read (lookupValue fields "Mem(kB)") :: Double
  bits :: Int
  bits
    | accS == "exact" = read (lookupValue fields "Parameters")
    | otherwise = read accS
    where
    accS = lookupValue fields "Accuracy(bits)"


renderChart ::
    Mode -> String -> (String, [(String, [(Double, Double)])]) -> IO ()
renderChart (_, lineId, xAxis@(_xCont, _xMode), yAxis@(yCont, _yMode)) outFolder (title, plotData) =
    void $
        renderableToFile fileOpts (filePath yCont) $
            fillBackground def $
                gridToRenderable $ layoutToGrid chartLayout
    where
    filePath MaxMem = outFolder ++ "/" ++ title ++ "-space.svg"
    filePath ExecTime = outFolder ++ "/" ++ title ++ "-time.svg"
    filePath Accuracy = outFolder ++ "/" ++ title ++ "-bits.svg"
    filePath BenchN = outFolder ++ "/" ++ title ++ "-n.svg"

    chartLayout =
        execEC $
        do
        layout_legend . _Just . legend_orientation .= LORows (legend_cols lineId)

        layout_y_axis . laxis_generate .= axisForModeCont yAxis
        -- layout_y_axis . laxis_title .= axisTitle yCont yMode

        layout_x_axis . laxis_generate .= axisForModeCont xAxis
        -- layout_x_axis . laxis_title .= axisTitle xCont xMode
        -- layout_x_axis . laxis_title_style . font_size .= 10

        layout_x_axis . laxis_style . axis_label_gap .= 0
        mapM layoutPlotData $ zip [0..] plotData

    legend_cols FnRepr = 5
    -- legend_cols Method = 2
    legend_cols _ = 3

    -- axisTitle MaxMem =  addLog "Space (kB)"
    -- axisTitle ExecTime = addLog "Time (s)"
    -- axisTitle Accuracy = addLog "Accuracy (bits)"
    -- axisTitle BenchN = addLog "n"
    -- -- addLog s Log = "Log10 " ++ s
    -- -- addLog s (LogFromTo _ _) = "Log10 " ++ s
    -- addLog s _ = s

    -- axisForModeCont (_, Lin) = autoScaledAxis def
    -- axisForModeCont (_, Log) = autoScaledLogAxis def
    axisForModeCont (Accuracy, LinFromTo n m) = scaledAxisExtraXSteps def (n, m) [24,53]
    axisForModeCont (_, LinFromTo n m) = scaledAxis def (n, m)
    axisForModeCont (_, LogFromTo n m) = 
        -- scaledAxisPow10 def (logBase 10 n, logBase 10 m) 
        scaledLogAxis (LogAxisParams labelf) (n, m)
        where
        labelf = map show

    -- lowLog :: Int -> Double
    -- lowLog limit
    --   | limit > 1000 = 1
    --   | otherwise = 0.01

    -- lowLin :: Int -> Double
    -- lowLin _ = 0


    layoutPlotData (lineNum, (lineName, linePoints)) =
        plotPoints name linePoints
        where
        (name, colour, shape) =
          case lineId of
            FnRepr -> (reprShow lineName, reprColor lineName, reprShape lineName)
            _ -> (lineName, stdColors !! lineNum, stdShapes !! lineNum)
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
fileOpts = def { _fo_size = (400,300) }
-- fileOpts = def { _fo_size = (330,200) }

reprShow :: String -> String
reprShow "dball" = "DBFun"
reprShow "ball" = "BFun"
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

-- opShow :: String -> String
-- opShow s = s
-- -- opShow opName = error $ "unknown operation " ++ opName

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
