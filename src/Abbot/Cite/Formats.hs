module Abbot.Cite.Formats
  where


import           Abbot.Cite.Internal
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.URI                       ( render )


-- | surroundWith2 x1 x2 y surrounds the Text y with the specified ends.
--
-- surroundWith2 x1 x2 y = x1 <> y <> x2
surroundWith2 :: Text -> Text -> Text -> Text
surroundWith2 x1 x2 y = x1 <> y <> x2


-- | surroundWith x y surrounds the Text y on both sides with x.
--
-- surroundWith x y = x <> y <> x
surroundWith :: Text -> Text -> Text
surroundWith x y = x <> y <> x


-- | surroundWithTag surrounds a Text with a HTML tag.
surroundWithTag :: Text -> [(Text, Maybe Text)] -> Text -> Text
surroundWithTag tagName tagArgs x = openingTag <> x <> closingTag
  where
    openingTag = "<" <> tagName <> (T.concat $ map makeArgs tagArgs) <> ">"
    closingTag = "</" <> tagName <> ">"
    makeArgs :: (Text, Maybe Text) -> Text
    makeArgs (key, Just val) = " " <> key <> "=\"" <> val <> "\""
    makeArgs (key, Nothing)  = " " <> key


-- | Plain text, i.e. no formatting whatsoever.
textFormat :: Format
textFormat = Format { plainFormatter  = id
                    , boldFormatter   = id
                    , italicFormatter = id
                    , linkFormatter   = const id
                    }


-- | Markdown formatter.
mdFormat :: Format
mdFormat = Format
  { plainFormatter  = id
  , boldFormatter   = surroundWith "**"
  , italicFormatter = surroundWith "*"
  , linkFormatter   = \url disp -> "[" <> disp <> "](" <> render url <> ")"
  }


-- | RestructuredText formatter.
rstFormat :: Format
rstFormat = Format
  { plainFormatter  = id
  , boldFormatter   = surroundWith "**"
  , italicFormatter = surroundWith "*"
  , linkFormatter   = \url disp -> "`" <> disp <> " <" <> render url <> ">`_"
  }


-- | HTML formatter.
htmlFormat :: Format
htmlFormat = Format
  { plainFormatter  = id
  , boldFormatter   = surroundWithTag "b" []
  , italicFormatter = surroundWithTag "i" []
  , linkFormatter   = \url -> surroundWithTag "a" [("href", Just (render url))]
  }
