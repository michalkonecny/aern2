
import Prelude

import Data.List (intercalate)
import Servant.Elm

import AERN2.RealFun.PlotService.API (api)

main :: IO ()
main = do
  let code = intercalate "\n\n" $
        "module Api exposing (..)" :
        defElmImports :
        generateElmForAPI api
  writeFile "client/Api.elm" code
