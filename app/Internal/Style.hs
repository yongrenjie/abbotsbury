module Internal.Style
  ( module Internal.Style
  ) where

import qualified Data.Colour.Names             as CNames
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           System.Console.ANSI     hiding ( setSGRCode )
import qualified System.Console.ANSI           as ANSI

-- | Wrapper around ansi-terminal's setSGRCode which returns String values.
-- We need the "\STX" here because the ANSI escape sequence that setSGRCode
-- produces looks like "\ESC[0m" (that's the reset code). A typical terminal
-- would understand the 'm' to signify the end of the escape sequence. However,
-- haskeline's internal stringToGraphemes function doesn't recognise the 'm' as
-- the end of a control sequence: it only recognises "\STX" as the end of one.
-- This leads to subtle bugs with styles not being correctly applied depending
-- on the terminal width.
-- See also https://github.com/judah/haskeline/wiki/ControlSequencesInPrompt.
setSGRCode :: [SGR] -> Text
setSGRCode = T.pack . (++ "\STX") . ANSI.setSGRCode

resetCode :: Text
resetCode = setSGRCode [Reset]

-- | Change the foreground colour. Only CSS colours are accepted. These can be viewed at
-- https://www.w3.org/TR/SVG11/types.html#ColorKeywords
setColor
  :: String
  -> -- The name of the colour.
     Text
  -> -- The text to transform.
     Text -- The coloured text.
setColor colourName =
  let colour     = fromJust $ CNames.readColourName colourName
      colourCode = setSGRCode [SetRGBColor Foreground colour]
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

-- | Make an error message.
makeError :: Text -> Text
makeError errMsg = setColor "red" "error: " <> setColor "coral" errMsg
