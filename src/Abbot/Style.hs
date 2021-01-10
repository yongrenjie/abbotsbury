module Abbot.Style
  ( module Abbot.Style,
  ) where

import           Data.Maybe                     ( fromJust )
import           Data.Colour                    ( Colour )
import qualified Data.Colour.Names             as CNames
import           System.Console.ANSI

setColor :: String -> String -> String
setColor colourName text = mconcat
  [ setSGRCode [SetRGBColor Foreground colour]
  , text
  , setSGRCode [Reset]
  ]
  where
    colour = fromJust $ CNames.readColourName colourName

setBold :: String -> String
setBold text = mconcat
  [ setSGRCode [SetConsoleIntensity BoldIntensity]
  , text
  , setSGRCode [Reset]
  ]

setItalic :: String -> String
setItalic text = mconcat
  [ setSGRCode [SetItalicized True]
  , text
  , setSGRCode [Reset]
  ]
