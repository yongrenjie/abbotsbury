module Abbotsbury.Cite.Formats.Internal
  ( surroundWith2
  , surroundWith
  , surroundWithTag
  , Text.URI.render
  ) where


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
  makeArgs (key, Nothing ) = " " <> key
