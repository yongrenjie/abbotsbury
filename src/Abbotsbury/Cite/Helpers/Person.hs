module Abbotsbury.Cite.Helpers.Person where

import           Abbotsbury.Cite.Internal
import           Abbotsbury.LatexEscapes
import           Abbotsbury.Work
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Lens.Micro

-- Note that these PersonStyles are merely helper functions. We can't and shouldn't hard code them
-- into a Style, because it won't be extensible by other people.

-- | Methods of formatting author names.
data PersonStyle
  = FamilyInitials -- ACS style.
  | InitialsFamily -- ACIE style.
  | BibLaTeX -- For .bib files.
  deriving (Ord, Eq, Show, Enum, Bounded)

-- | Creates Text for a person's name according to the specified 'PersonStyle',
-- and then wraps it inside a plain CitationPart.
formatPersonAsCPart :: PersonStyle -> Person -> CitationPart
formatPersonAsCPart fmt auth = CText (formatPerson fmt auth)

-- | Creates Text for a person's name according to the specified 'PersonStyle'.
formatPerson :: PersonStyle -> Person -> Text
formatPerson fmt person = t
 where
  makeInitials = joinInitialsWith " " "-" "." . getInitials
  t            = case fmt of
    FamilyInitials ->
      T.intercalate ", "
        . catMaybes
        $ [ Just (person ^. family)
          , makeInitials <$> person ^. given
          , person ^. suffix
          ]
    InitialsFamily ->
      T.unwords
        . catMaybes
        $ [ makeInitials <$> person ^. given
          , Just (person ^. family)
          , person ^. suffix
          ]
    BibLaTeX ->
      latexReplaceEscapes
        . latexReplaceSpaces
        . T.intercalate ", "
        . catMaybes
        $ [Just (person ^. family), person ^. suffix, person ^. given]

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
