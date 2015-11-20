module Main where

import Control.Lens.Basic (view)
import Data.Monoid ((<>))


-- Uses the Strict Record syntax.
type Person =
  {! 
    name :: String, 
    birthday :: {! year :: Int, month :: Int, day :: Int }, 
    country :: Country
  }

-- Uses the Lazy Record syntax.
type Country =
  {~
    name :: String,
    language :: String
  }

person :: Person
person =
  {! 
    name = "Yuri Alekseyevich Gagarin", 
    birthday = {! year = 1934, month = 3, day = 9 },
    country = {~ name = "Soviet Union", language = "Russian" }
  }

-- Uses the Field-label Lens syntax.
main = do
  putStrLn $ "Name: " <> show (view @name person)
  putStrLn $ "Country: " <> show (view (@country . @name) person)



