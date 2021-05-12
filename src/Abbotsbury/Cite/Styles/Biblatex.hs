{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Defines the BibLaTeX "citation style". In practice, this actually gives an entry for a .bib
-- file (it isn't a true citation style per se).
module Abbotsbury.Cite.Styles.Biblatex where

import           Abbotsbury.Cite.Helpers.Author
import           Abbotsbury.Cite.Internal
import           Abbotsbury.LatexEscapes        ( latexify )
import           Abbotsbury.Work
import           Control.Applicative            ( (<|>) )
import           Data.Char                      ( isAscii
                                                , isDigit
                                                , isUpper
                                                )
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Normalize            ( NormalizationMode(..)
                                                , normalize
                                                )
import           Lens.Micro

bibStyle :: Style
bibStyle = Style { articleConstructor = articleConstructorBib }

-- | In practice, we do most of the work as Text, before converting it to CitationPart.
articleConstructorBib :: Work -> [CitationPart]
articleConstructorBib work = [CText (latexify t)]
 where
  t :: Text
  t =
    T.intercalate "\n"
      $ -- T.unlines adds one extra \n at the very end which I don't want.
         [headerL, doiL, authorL, journalL, titleL, yearL]
      ++ catMaybes [volumeM, issueM, pagesM]
      ++ ["}"]
  headerL, doiL, authorL, journalL, titleL, yearL :: Text
  volumeM, issueM, pagesM :: Maybe Text -- These fields may be empty.
  headerL = "@article{" <> identifier <> ","
   where
    identifier =
      T.filter isAscii (normalize NFD (work ^. (authors . ix 0 . family)))
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
  issueM =
    makeMaybeBibFieldWith (T.all isDigit) "number" (work ^. issue)
      <|> makeMaybeBibField "issue" (work ^. issue)
  pagesM = makeMaybeBibField "pages" (work ^. pages)
    <|> makeMaybeBibField "pages" (work ^. articleNumber)

-- | Shortcut to make a BibLaTeX key-value pair. If the value is not guaranteed to be present, then
-- use makeMaybeBibField.
makeBibField
  ::
  -- | The key.
     Text
  ->
  -- | The value.
     Text
  ->
  -- | The line to be printed to the bib file.
     Text
makeBibField key val = "    " <> key <> " = {" <> val <> "},"

-- | Makes a BibLaTeX field if a certain predicate is satisfied. Generalised form of
-- makeMaybeBibField.
makeMaybeBibFieldWith
  ::
  -- | The predicate. The field is created if this predicate returns True when applied to the value.
     (Text -> Bool)
  ->
  -- | The key.
     Text
  ->
  -- | The value (which the predicate is tested against).
     Text
  ->
  -- | Just the line to be printed, or Nothing if predicate fails.
     Maybe Text
makeMaybeBibFieldWith pred key val =
  if pred val then Just (makeBibField key val) else Nothing

-- | Shortcut to make Just a BibLaTex key-value pair, but only if the value is nonempty.
makeMaybeBibField
  ::
  -- | The key.
     Text
  ->
  -- | The (possibly empty) value.
     Text
  ->
  -- | Just the line to be printed, or Nothing if the value was empty.
     Maybe Text
makeMaybeBibField = makeMaybeBibFieldWith (not . T.null)
