module Abbot.Format.Author where


import           Abbot.LatexEscapes
import           Abbot.Reference

import           Data.Char                      ( isSpace )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Lens.Micro.Platform


-- | Methods of formatting author names.
data AuthorFormatting = ListCmd          -- For the list command.
                      | FamilyInitials   -- ACS style.
                      | InitialsFamily   -- ACIE style.
                      | BibLaTeX         -- For .bib files.
                      deriving (Ord, Eq, Show, Enum, Bounded)



-- | Formats an Author according to the specified AuthorFormatting mode.
formatAuthor :: AuthorFormatting -> Author -> Text
formatAuthor fmt auth =
  let fam = auth ^. family
  in
    case auth ^. given of
      Nothing  -> fam
      Just gvn -> case fmt of
        ListCmd ->
          T.pack (map T.head (T.split (\c -> isSpace c || c == '-') gvn))
            <> " "
            <> fam
        FamilyInitials -> fam <> ", " <> makeInitials gvn
        InitialsFamily -> makeInitials gvn <> " " <> fam
        BibLaTeX -> latexify (fam <> ", " <> gvn)



-- | Extracts the initials from a given name.
-- 
-- >>> makeInitials "Jonathan Ren Jie"
-- "J. R. J."
-- >>> makeInitials "Jean-Baptiste Simon"
-- "J.-B. S."
makeInitials :: Text -> Text
makeInitials name0 = name4
  where
    name1 = map (T.split (== '-')) (T.split isSpace name0)  -- [["Jean", "Baptiste"], ["Simon"]]
    name2 = (fmap . fmap) ((`T.cons` ".") . T.head) name1   -- [["J.", "B."], ["S."]]
    name3 = map (T.intercalate "-") name2                   -- ["J.-B.", "S."]
    name4 = T.intercalate " " name3                         -- "J.-B. S."
