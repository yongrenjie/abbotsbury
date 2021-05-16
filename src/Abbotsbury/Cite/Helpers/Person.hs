module Abbotsbury.Cite.Helpers.Person where

import           Abbotsbury.Cite.Internal
import           Abbotsbury.LatexEscapes
import           Abbotsbury.Work
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

-- Note that these PersonStyles are merely helper functions. We can't and shouldn't hard code them
-- into a Style, because it won't be extensible by other people.

-- | Methods of formatting author names.
data PersonStyle
  = FamilyInitials -- ACS style.
  | InitialsFamily -- ACIE style.
  | BibLaTeX -- For .bib files.
  deriving (Ord, Eq, Show, Enum, Bounded)

-- | Formats an Person according to the specified PersonFormat mode, but wraps it inside a
-- CText to make it a proper CitationPart.
formatPersonAsCPart :: PersonStyle -> Person -> CitationPart
formatPersonAsCPart fmt auth = CText (formatPerson fmt auth)

-- | Formats an Person according to the specified PersonFormat mode, but output in Text instead of
-- as a CitationPart.
formatPerson :: PersonStyle -> Person -> Text
formatPerson fmt auth =
  let fam          = _family auth
      makeInitials = joinInitialsWith " " "-" "." . getInitials
  in  case _given auth of
        Nothing  -> fam
        Just gvn -> case fmt of
          FamilyInitials -> fam <> ", " <> makeInitials gvn
          InitialsFamily -> makeInitials gvn <> " " <> fam
          BibLaTeX       -> latexify (fam <> ", " <> gvn)

-- | Extracts the initials from a given name. The elements of the outermost list are separated by
-- spaces; the elements of each inner list are separated by hyphens.
--
-- >>> getInitials "Jonathan Ren Jie"
-- [['J'], ['R'], ['J']]
-- >>> getInitials "Jean-Baptiste Simon"
-- [['J', 'B'], ['S']]
getInitials :: Text -> [[Char]]
getInitials =
  (fmap . fmap) T.head -- extract first character
    . map (T.split (== '-')) -- form inner lists by splitting on hyphens
    . T.words -- form outer lists by splitting on spaces

joinInitialsWith :: Text -> Text -> Text -> [[Char]] -> Text
joinInitialsWith spaceReplace hyphenReplace dotReplace =
  T.intercalate spaceReplace . map (T.intercalate hyphenReplace) . (fmap . fmap)
    (`T.cons` dotReplace)
