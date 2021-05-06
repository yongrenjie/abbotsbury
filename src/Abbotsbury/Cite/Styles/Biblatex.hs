-- | Defines the BibLaTeX "citation style". In practice, this actually gives an entry for a .bib
-- file (it isn't a true citation style per se).
module Abbotsbury.Cite.Styles.Biblatex where


import           Abbotsbury.LatexEscapes        ( latexify )
import           Abbotsbury.Cite.Helpers.Author
import           Abbotsbury.Cite.Internal
import           Abbotsbury.Work
import           Data.Char                      ( isLetter
                                                , isUpper
                                                )
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                     ( catMaybes )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Lens.Micro.Platform


bibStyle :: Style
bibStyle = Style { articleConstructor = articleConstructorBib }


-- | In practice, we do most of the work as Text, before converting it to CitationPart.
articleConstructorBib :: Work -> [CitationPart]
articleConstructorBib work = [CText (latexify t)]
 where
  t :: Text
  t =
    T.intercalate "\n" -- T.unlines adds one extra \n at the very end which I don't want.
      $  [headerL, doiL, authorL, journalL, titleL, yearL]
      ++ catMaybes [volumeM, issueM, pagesM]
      ++ ["}"]
  headerL, doiL, authorL, journalL, titleL, yearL :: Text
  volumeM, issueM, pagesM :: Maybe Text  -- These fields may be empty.
  headerL = "@article{" <> identifier <> ","
   where
    identifier =
      -- TODO convert special characters to ASCII before using isLetter
      T.filter isLetter (work ^. (authors . ix 0 . family))
        <> T.filter isUpper (work ^. journalShort)
        <> (T.pack . show) (work ^. year)
  doiL    = makeBibField "doi" (work ^. doi)
  authorL = makeBibField
    "author"
    (T.intercalate
      " and "
      (fmap (formatAuthor BibLaTeX) (NE.toList $ work ^. authors))
    )
  journalL = makeBibField "journal" (work ^. journalShort)
  titleL   = makeBibField "title" (work ^. title)
  yearL    = makeBibField "year" (T.pack . show $ work ^. year)
  volumeM  = makeMaybeBibField "volume" (work ^. volume)
  issueM   = makeMaybeBibField "issue" (work ^. issue)
  pagesM   = makeMaybeBibField "pages" (work ^. pages)


-- | Shortcut to make a BibLaTeX key-value pair. If the value is not guaranteed to be present, then
-- use makeMaybeBibField.
makeBibField
  :: Text -- ^ The key.
  -> Text -- ^ The value.
  -> Text -- ^ The line to be printed to the bib file.
makeBibField key val = "    " <> key <> " = {" <> val <> "},"


-- | Shortcut to make Just a BibLaTex key-value pair, but only if the value is nonempty.
makeMaybeBibField
  :: Text -- ^ The key.
  -> Text -- ^ The (possibly empty) value.
  -> Maybe Text -- ^ Just the line to be printed, or Nothing if the value was empty.
makeMaybeBibField key val =
  if T.null val then Nothing else Just (makeBibField key val)
