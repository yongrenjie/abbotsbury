module Abbot.Style
  ( module Abbot.Style,
  ) where

import           Data.Maybe                     ( fromJust )
import qualified Data.Colour.Names             as CNames
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           System.Console.ANSI     hiding ( setSGRCode )
import qualified System.Console.ANSI           as ANSI

-- | Wrapper around ansi-terminal's setSGRCode which returns String values.
setSGRCode :: [SGR] -> Text
setSGRCode = T.pack . ANSI.setSGRCode

-- | Change the foreground colour.
setColor :: String  -- The name of the colour. Only CSS colours are accepted.
         -> Text    -- The text to transform.
         -> Text    -- The coloured text.
setColor colourName text = mconcat
  [ setSGRCode [SetRGBColor Foreground colour]
  , text
  , setSGRCode [Reset]
  ]
  where
    colour = fromJust $ CNames.readColourName colourName

-- | Makes text bold.
setBold :: Text -> Text
setBold text = mconcat
  [ setSGRCode [SetConsoleIntensity BoldIntensity]
  , text
  , setSGRCode [Reset]
  ]

-- | Makes text italicised.
setItalic :: Text -> Text
setItalic text = mconcat
  [ setSGRCode [SetItalicized True]
  , text
  , setSGRCode [Reset]
  ]
