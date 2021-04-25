module Abbot.Cite.Format where

import           Abbot.Cite.Rules
import           Data.Text                      ( Text )



-- | surroundWith x1 x2 y surrounds the Text y with the specified ends.
--
-- surroundWith x1 x2 y = x1 <> y <> x2
surroundWith :: Text -> Text -> Text -> Text
surroundWith x1 x2 y = x1 <> y <> x2


-- | surroundBothWith x y surrounds the Text y on both sides with x.
--
-- surroundBothWith x y = x <> y <> x
surroundBothWith :: Text -> Text -> Text
surroundBothWith x y = x <> y <> x


formatBold :: Format -> Text -> Text
formatBold fmt = case fmt of
  PlainText    -> id
  Markdown     -> surroundBothWith "**"
  Restructured -> surroundBothWith "**"
  HTML         -> surroundWith "<b>" "</b>"


formatItalic :: Format -> Text -> Text
formatItalic fmt = case fmt of
  PlainText    -> id
  Markdown     -> surroundBothWith "*"
  Restructured -> surroundBothWith "*"
  HTML         -> surroundWith "<i>" "</i>"


formatLink :: Format -> Text -> Text -> Text
formatLink fmt url displayText = case fmt of
  PlainText    -> displayText
  Markdown     -> "[" <> displayText <> "](" <> url <> ")"
  Restructured -> "`" <> displayText <> " <" <> url <> ">`_"
  HTML         -> "<a href=\">" <> url <> "\"" <> displayText <> "</a>"
