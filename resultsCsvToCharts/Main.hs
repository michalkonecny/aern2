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

--import Debug.Trace (trace)

main :: IO ()
main = 
    do
    args <- getArgs
    let (inFileName, outFolder) = checkArgs args
    contents <- readFile inFileName
    let chartsData = parseFnOpReprBitsTime contents
    void $ mapM (renderChart outFolder) chartsData
    
checkArgs :: [String] -> (String, String)
checkArgs [inFileName, outFolder] = (inFileName, outFolder)
checkArgs _ = error "usage: <program name> <inFileName> <outFolder>"

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
parseFnOpReprBitsTime :: String -> [(String, [(String, [(Int, Double)])])]
parseFnOpReprBitsTime csvContent =
    over (mapped._2.mapped._2) Set.toAscList $ -- convert the inner Sets to ascending lists (using Lens)
        over (mapped._2) Map.toList $ -- convert the inner Maps to lists
            Map.toList fnOp_To_Repr_To_Points -- convert the outer Map to list
    where
    fnOp_To_Repr_To_Points = mergeByFnOp_FnRepr records
    records = indexRecordsByKeysAndHeader ["Fn","Op","FnRepr"] $ parseCSV csvContent

mergeByFnOp_FnRepr :: 
    [([String], Map.Map String String)] -> Map.Map String (Map.Map String (Set.Set (Int,Double)))
mergeByFnOp_FnRepr records =
    foldl insertRecord Map.empty records
    where
    insertRecord preMap ([fnName,opName,reprName], fields) = 
        Map.insertWith 
            (Map.unionWith Set.union) 
            chartTitle (Map.singleton reprName (Set.singleton point)) 
            preMap
        where
        chartTitle = fnName ++ "-" ++ opName
        point = (bits,utime+stime)
        bits = read (lookupValue fields "Accuracy(bits)")
        utime = read (lookupValue fields "UTime(s)") :: Double
        stime = read (lookupValue fields "STime(s)") :: Double
    insertRecord _ _ = error "internal error in mergeByFnOp_FnRepr"

renderChart :: 
    (PlotValue y, PlotValue x, Show y, RealFloat y) 
    =>
    String
    -> (String, [(String, [(x, y)])]) -> 
    IO ()
renderChart outFolder (title, reprsData) =
    void $
        renderableToFile fileOpts filePath $ 
            fillBackground def $ 
                gridToRenderable $ layoutToGrid chartLayout
    where
    filePath = outFolder ++ "/" ++ title ++ ".svg"
    chartLayout =
        execEC $ 
        do
        layout_y_axis . laxis_generate .= autoScaledLogAxis def
        layout_y_axis . laxis_title .= "Time (s)"
        layout_x_axis . laxis_title .= "Accuracy (bits)"
        layout_x_axis . laxis_style . axis_label_gap .= 1
        mapM layoutReprData reprsData
    layoutReprData (reprName, reprData) =
        plotPointsLine (reprShow reprName) [(ac, time) | (ac,time) <- reprData]
        where
        plotPointsLine ltitle values =
            do
            plot $ pointsLayout ltitle values
            plot $ linesLayout values
        linesLayout values =
            liftEC $
                do
                plot_lines_values .= [values]
                plot_lines_style . line_color .= opaque (reprColor reprName)
        pointsLayout ltitle values =
            liftEC $ 
                do
                plot_points_values .= values
                plot_points_title .= ltitle
                plot_points_style . point_color .= opaque (reprColor reprName)
                plot_points_style . point_shape .= (reprShape reprName)
                plot_points_style . point_radius .= 3
                -- Show borders for unfilled shapes:
                when (not (isFilled (reprShape reprName))) $ do
                    plot_points_style . point_border_color .= opaque (reprColor reprName)
                    plot_points_style . point_border_width .= 1
        isFilled :: PointShape -> Bool
        isFilled PointShapeCircle = True
        isFilled PointShapePolygon{} = True
        isFilled _ = False
        
fileOpts :: FileOptions
fileOpts = def { _fo_size = (300,200) }
 
reprColor :: String -> Colour Double
reprColor "dfun" = powderblue
reprColor "fun" = darkblue
reprColor "poly" = orangered
reprColor "ppoly" = mediumvioletred
reprColor reprName = error $ "unknown representation " ++ reprName

reprShow :: String -> String
reprShow "dfun" = "DFun"
reprShow "fun" = "Fun"
reprShow "poly" = "Poly"
reprShow "ppoly" = "PPoly"
reprShow reprName = error $ "unknown representation " ++ reprName

reprShape :: String -> PointShape
reprShape "dfun" = PointShapeEllipse 0.7 1.0
reprShape "fun" = PointShapeCircle
reprShape "poly" = PointShapeCross
reprShape "ppoly" = PointShapeStar
reprShape reprName = error $ "unknown representation " ++ reprName

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
