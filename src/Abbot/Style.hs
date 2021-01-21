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

resetCode :: Text
resetCode = setSGRCode [Reset]

-- | Change the foreground colour.
setColor
  :: String  -- The name of the colour. Only CSS colours are accepted.
  -> Text    -- The text to transform.
  -> Text    -- The coloured text.
setColor colourName =
  let colour        = fromJust $ CNames.readColourName colourName
      colourCode    = setSGRCode [SetRGBColor Foreground colour]
  in  styleText colourCode

-- | Makes text bold.
setBold :: Text -> Text
setBold = styleText (setSGRCode [SetConsoleIntensity BoldIntensity])

-- | Makes text italicised.
setItalic :: Text -> Text
setItalic = styleText (setSGRCode [SetItalicized True])

-- | Style a text.
styleText :: Text -> Text -> Text
styleText code text =
  let endsWithReset = T.takeEnd 4 text == resetCode
  in  if endsWithReset then code <> text else code <> text <> resetCode
