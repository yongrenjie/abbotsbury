module Abbot.Cite.Helpers.Author where


import           Abbot.LatexEscapes
import           Abbot.Work
import           Abbot.Cite.Internal
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Lens.Micro.Platform


-- Note that these AuthorStyles are merely helper functions. We can't and shouldn't hard code them
-- into a set of Rules or Style, because it won't be extensible by other people. The same is true of
-- JInfoStyles.


-- | Methods of formatting author names.
data AuthorStyle = FamilyInitials   -- ACS style.
                 | InitialsFamily   -- ACIE style.
                 | BibLaTeX         -- For .bib files.
                 deriving (Ord, Eq, Show, Enum, Bounded)


-- | Formats an Author according to the specified AuthorFormat mode.
formatAuthor :: AuthorStyle -> Author -> CitationPart
formatAuthor fmt auth =
  let fam = auth ^. family
      makeInitials = joinInitialsWith " " "-" "." . getInitials
  in
    CText $ case auth ^. given of
      Nothing  -> fam
      Just gvn -> case fmt of
        FamilyInitials -> fam <> ", " <> makeInitials gvn
        InitialsFamily -> makeInitials gvn <> " " <> fam
        BibLaTeX -> latexify (fam <> ", " <> gvn)


-- | Extracts the initials from a given name. The elements of the outermost list are separated by
-- spaces; the elements of each inner list are separated by hyphens.
-- 
-- >>> getInitials "Jonathan Ren Jie"
-- [['J'], ['R'], ['J']]
-- >>> getInitials "Jean-Baptiste Simon"
-- [['J', 'B'], ['S']]
getInitials :: Text -> [[Char]]
getInitials = (fmap . fmap) T.head     -- extract first character
            . map (T.split (== '-'))   -- form inner lists by splitting on hyphens
            . T.words                  -- form outer lists by splitting on spaces


joinInitialsWith :: Text -> Text -> Text -> [[Char]] -> Text
joinInitialsWith spaceReplace hyphenReplace dotReplace
 = T.intercalate spaceReplace
 . map (T.intercalate hyphenReplace)
 . (fmap . fmap) (`T.cons` dotReplace)
