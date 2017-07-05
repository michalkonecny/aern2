
import Prelude

import Data.List (intercalate)
import Servant.Elm

import qualified Data.Text as T

import AERN2.RealFun.PlotService.API (api)

main :: IO ()
main = do
  let code = intercalate "\n\n" $
        "module Api exposing (..)" :
        T.unpack defElmImports :
        map T.unpack (generateElmForAPI api)
  writeFile "client/Api.elm" code
